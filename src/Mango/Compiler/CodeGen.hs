{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Mango.Compiler.CodeGen (
    -- Code Generation
    CompiledPackage (..),
    CompiledModule (..),
    CompiledFunction (..),
    CompiledInstruction (..),
    ByteCode (..),
    compile,
    ) where

import Control.Applicative
import Control.Monad
import Crypto.Hash (Digest, hashlazy)
import Crypto.Hash.Algorithms (SHA256)
import Data.Bits
import Data.Bool
import Data.ByteArray (convert)
import Data.ByteString.Builder
import Data.ByteString.Lazy (ByteString)
import Data.Eq
import Data.Function
import Data.Functor.Identity
import Data.Int
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.String
import Data.Tuple
import Data.Word
import Mango.Compiler.Error
import Mango.Compiler.Symbols
import Mango.Compiler.Syntax
import Mango.Compiler.Verifier
import Prelude (Bounded (..), Integral (..), Num (..), Integer,  fromIntegral)
import Text.Show

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.Vector as V

--------------------------------------------------------------------------------

data CompiledPackage
    = CompiledPackage [CompiledModule]

data CompiledModule
    = CompiledModule !ModuleSymbol !Int64 !Word8 !Word8 !Word8 !Word8 [CompiledInstruction] ![B.ByteString] [CompiledFunction] ByteString B.ByteString

data CompiledFunction
    = CompiledFunction !FunctionSymbol !Int64 !Word8 !Word8 !Word8 [CompiledInstruction]

data CompiledInstruction
    = CompiledInstruction !Int64 !ByteCode

data ByteCode
    = ByteCode !Builder !Int64 !Word8

instance Monoid ByteCode where
    mempty = ByteCode mempty 0 0x00
    mappend (ByteCode b1 l1 f1) (ByteCode b2 l2 f2) = ByteCode (mappend b1 b2) (l1 + l2) (f1 .|. f2)

--------------------------------------------------------------------------------

compile :: (Monad m) => VerifiedCompilation -> CompilerT Diagnostic m CompiledPackage
compile =
    mapCompilerT (return . runIdentity) . compileCompilation

--------------------------------------------------------------------------------

type Compiler = CompilerT Diagnostic Identity

reportIf :: Bool -> String -> Location -> Compiler ()
reportIf condition message location =
    if condition then report (GenericError location message) else return ()

--------------------------------------------------------------------------------

buildModule :: CompiledModule -> Builder
buildModule (CompiledModule _ _ version features moduleCount importCount entryPoint imports functions _ _) =
    word8 version <> word8 features <> word8 moduleCount <> word8 importCount <> buildInstructions entryPoint <> mconcat (L.map byteString imports) <> buildFunctions functions

buildFunctions :: [CompiledFunction] -> Builder
buildFunctions functions =
    mconcat $ L.map (\(CompiledFunction _ _ argCount locCount maxStack instructions) -> word8 argCount <> word8 locCount <> word8 maxStack <> buildInstructions instructions) functions

buildInstructions :: [CompiledInstruction] -> Builder
buildInstructions instructions =
    mconcat $ L.map (\(CompiledInstruction _ (ByteCode builder _ _)) -> builder) instructions

--------------------------------------------------------------------------------

compileCompilation :: VerifiedCompilation -> Compiler CompiledPackage
compileCompilation (VerifiedCompilation modules) = do
    modules'  <- parallelC $ L.map (compileModule Nothing)         modules
    modules'' <- parallelC $ L.map (compileModule (Just modules')) modules
    return (CompiledPackage modules'')

compileModule :: Maybe [CompiledModule] -> VerifiedModule -> Compiler CompiledModule
compileModule context (VerifiedModule symbol functions imports) = do
    let version     = 1
        moduleCount = 1 + L.length (moduleDependencies symbol)
        importCount = L.length imports
    reportIf (moduleCount > fromIntegral (maxBound :: Word8)) "The module has too many dependencies" (symbolLocation symbol)
    reportIf (importCount > fromIntegral (maxBound :: Word8)) "The module has too many imports"      (symbolLocation symbol)
    mapM_ (\t -> reportIf (typeLayout_size (typeLayout t) > fromIntegral (maxBound :: Word16)) "The size of the structure if too large" (symbolLocation t)) (moduleSymbol_types symbol)
    entryPoint                       <- compileEntryPoint context symbol
    ((length, features), functions') <- mapAccumM (compileFunction context) (fromIntegral (8 + 12 * L.length imports), 0x00) functions
    let compiled    = CompiledModule symbol length version features (fromIntegral moduleCount) (fromIntegral importCount) entryPoint imports' functions' image fingerprint
        imports'    = L.map (moduleFingerprint context) imports
        image       = toLazyByteString (buildModule compiled)
        fingerprint = B.take 12 (convert (hashlazy image :: Digest SHA256))
    reportIf (BL.length image > fromIntegral (maxBound :: Word16)) "The compiled module is too large" (symbolLocation symbol)
    return compiled

compileEntryPoint :: Maybe [CompiledModule] -> ModuleSymbol -> Compiler [CompiledInstruction]
compileEntryPoint context module_ =
    fmap snd $ mapAccumM compileByteCode (4, 0x00, False) instructions
    where
        instructions = case moduleSymbol_entryPoint module_ of
            Nothing -> [nop, nop, nop, halt]
            Just f  -> [call_s (functionOffset context f), halt]

compileFunction :: Maybe [CompiledModule] -> (Int64, Word8) -> VerifiedFunction -> Compiler ((Int64, Word8), CompiledFunction)
compileFunction context (offset, features) (VerifiedFunction symbol instructions) = do
    let argCount = parameterSlotCount (functionSymbol_parameters symbol)
        locCount = localsSlotCount (functionSymbol_locals symbol)
        maxStack = maxStackSlotCount (V.toList instructions)
    reportIf (argCount            > fromIntegral (maxBound :: Word8)) "The total size of arguments is too large" (symbolLocation symbol)
    reportIf (locCount            > fromIntegral (maxBound :: Word8)) "The total size of local variables is too large" (symbolLocation symbol)
    reportIf (argCount + locCount > fromIntegral (maxBound :: Word8)) "The total size of arguments and local variables is too large" (symbolLocation symbol)
    reportIf (maxStack            > fromIntegral (maxBound :: Word8)) "The maximum size of the evaluation stack is too large" (symbolLocation symbol)
    ((offset', features', _), instructions') <- mapAccumM (compileInstruction context symbol) (offset + 3, features, False) (V.toList instructions)
    return ((offset', features'), CompiledFunction symbol offset (fromIntegral argCount) (fromIntegral locCount) (fromIntegral maxStack) instructions')
compileFunction _ (offset, features) (VerifiedExternFunction symbol ordinal) = do
    let adjustment = parameterSlotCount (functionSymbol_parameters symbol) - slotCount [functionSymbol_returnType symbol]
    reportIf (ordinal    < fromIntegral (minBound :: Word16) || ordinal    > fromIntegral (maxBound :: Word16)) "The ordinal is out of range" (symbolLocation symbol)
    reportIf (adjustment < fromIntegral (minBound :: Int8)   || adjustment > fromIntegral (maxBound :: Int8))   "The total size of arguments or the return value is too large" (symbolLocation symbol)
    ((offset', features', _), instructions') <- mapAccumM compileByteCode (offset + 3, features, False) [syscall (fromIntegral adjustment) (fromIntegral ordinal), ret]
    return ((offset', features'), CompiledFunction symbol offset 0 0 0 instructions')

compileByteCode :: (Int64, Word8, Bool) -> ByteCode -> Compiler ((Int64, Word8, Bool), CompiledInstruction)
compileByteCode (offset, features, seenDeadCode) bytecode@(ByteCode _ length features') =
    return ((offset + length, features .|. features', seenDeadCode), CompiledInstruction offset bytecode)

compileInstruction :: Maybe [CompiledModule] -> FunctionSymbol -> (Int64, Word8, Bool) -> VerifiedInstruction -> Compiler ((Int64, Word8, Bool), CompiledInstruction)
compileInstruction context symbol (offset, features, seenDeadCode) instruction =
    go instruction >>= compileByteCode (offset, features, case instruction of DeadInstruction _ _ -> True; _ -> False)
    where
        go :: VerifiedInstruction -> Compiler ByteCode
        go (DeadInstruction           j _                      ) = reportIf (not seenDeadCode) "Unreachable code detected" j >> pure mempty

        go (NopInstruction            _ _                      ) = pure nop
        go (BreakInstruction          _ _                      ) = pure break

        go (PopInstruction            j _ t                    ) = select5   j t   pop_i32         pop_i64         pop_f32         pop_f64         pop_ref
        go (DupInstruction            j _ t                    ) = select5   j t   dup_i32         dup_i64         dup_f32         dup_f64         dup_ref

        go (NewobjInstruction         j _ _                    ) = reportIf True "Not implemented yet" j >> return (newobj 0)
        go (NewarrInstruction         _ _ t                    ) = pure (newarr (fromIntegral (typeLayout_size (typeLayout t))))
        go (LdlenInstruction          _ _                      ) = pure ldlen

        go (CallInstruction           j _ f                    ) = if containingModule f == containingModule symbol then pure (call_s o) else call <$> moduleImport j symbol f <*> pure o where o = functionOffset context f
        go (CalliInstruction          _ _ _                    ) = pure calli
        go (SyscallInstruction        j _ f                    ) = if containingModule f == containingModule symbol then pure (call_s o) else call <$> moduleImport j symbol f <*> pure o where o = functionOffset context f
        go (RetInstruction            _ _ VoidTypeSymbol       ) = pure ret
        go (RetInstruction            j _ t                    ) = select5   j t   ret_i32         ret_i64         ret_f32         ret_f64         ret_ref

        go (BrInstruction             j _ l                    ) = branch context symbol offset j l (pure mempty                                                                            ) br
        go (BrfalseInstruction        j _ l _                  ) = branch context symbol offset j l (pure mempty                                                                            ) brfalse
        go (BrtrueInstruction         j _ l _                  ) = branch context symbol offset j l (pure mempty                                                                            ) brtrue
        go (BeqInstruction            j _ l t                  ) = branch context symbol offset j l (select5   j t   ceq_i32         ceq_i64         ceq_f32         ceq_f64         ceq_i32) brtrue
        go (BgeInstruction            j _ l t                  ) = branch context symbol offset j l (select4   j t   cge_i32         cge_i64         cge_f32         cge_f64                ) brtrue
        go (BgeUnInstruction          j _ l t                  ) = branch context symbol offset j l (select4   j t   cge_i32_un      cge_i64_un      cge_f32_un      cge_f64_un             ) brtrue
        go (BgtInstruction            j _ l t                  ) = branch context symbol offset j l (select4   j t   cgt_i32         cgt_i64         cgt_f32         cgt_f64                ) brtrue
        go (BgtUnInstruction          j _ l t                  ) = branch context symbol offset j l (select4   j t   cgt_i32_un      cgt_i64_un      cgt_f32_un      cgt_f64_un             ) brtrue
        go (BleInstruction            j _ l t                  ) = branch context symbol offset j l (select4   j t   cle_i32         cle_i64         cle_f32         cle_f64                ) brtrue
        go (BleUnInstruction          j _ l t                  ) = branch context symbol offset j l (select4   j t   cle_i32_un      cle_i64_un      cle_f32_un      cle_f64_un             ) brtrue
        go (BltInstruction            j _ l t                  ) = branch context symbol offset j l (select4   j t   clt_i32         clt_i64         clt_f32         clt_f64                ) brtrue
        go (BltUnInstruction          j _ l t                  ) = branch context symbol offset j l (select4   j t   clt_i32_un      clt_i64_un      clt_f32_un      clt_f64_un             ) brtrue
        go (BneUnInstruction          j _ l t                  ) = branch context symbol offset j l (select5   j t   ceq_i32         ceq_i64         ceq_f32         ceq_f64         ceq_i32) brfalse

        go (BrSInstruction            j _ l                    ) = branch context symbol offset j l (pure mempty                                                                            ) br_s
        go (BrfalseSInstruction       j _ l _                  ) = branch context symbol offset j l (pure mempty                                                                            ) brfalse_s
        go (BrtrueSInstruction        j _ l _                  ) = branch context symbol offset j l (pure mempty                                                                            ) brtrue_s
        go (BeqSInstruction           j _ l t                  ) = branch context symbol offset j l (select5   j t   ceq_i32         ceq_i64         ceq_f32         ceq_f64         ceq_i32) brtrue_s
        go (BgeSInstruction           j _ l t                  ) = branch context symbol offset j l (select4   j t   cge_i32         cge_i64         cge_f32         cge_f64                ) brtrue_s
        go (BgeUnSInstruction         j _ l t                  ) = branch context symbol offset j l (select4   j t   cge_i32_un      cge_i64_un      cge_f32_un      cge_f64_un             ) brtrue_s
        go (BgtSInstruction           j _ l t                  ) = branch context symbol offset j l (select4   j t   cgt_i32         cgt_i64         cgt_f32         cgt_f64                ) brtrue_s
        go (BgtUnSInstruction         j _ l t                  ) = branch context symbol offset j l (select4   j t   cgt_i32_un      cgt_i64_un      cgt_f32_un      cgt_f64_un             ) brtrue_s
        go (BleSInstruction           j _ l t                  ) = branch context symbol offset j l (select4   j t   cle_i32         cle_i64         cle_f32         cle_f64                ) brtrue_s
        go (BleUnSInstruction         j _ l t                  ) = branch context symbol offset j l (select4   j t   cle_i32_un      cle_i64_un      cle_f32_un      cle_f64_un             ) brtrue_s
        go (BltSInstruction           j _ l t                  ) = branch context symbol offset j l (select4   j t   clt_i32         clt_i64         clt_f32         clt_f64                ) brtrue_s
        go (BltUnSInstruction         j _ l t                  ) = branch context symbol offset j l (select4   j t   clt_i32_un      clt_i64_un      clt_f32_un      clt_f64_un             ) brtrue_s
        go (BneUnSInstruction         j _ l t                  ) = branch context symbol offset j l (select5   j t   ceq_i32         ceq_i64         ceq_f32         ceq_f64         ceq_i32) brfalse_s

        go (LdcInstruction            j _ Int32TypeSymbol value) = ldc_i32 <$> intConstant j value
        go (LdcInstruction            j _ Int64TypeSymbol value) = ldc_i64 <$> intConstant j value
        go (LdcInstruction            j _ _               _    ) = reportIf True "Unsupported constant type" j >> stop
        go (LdftnInstruction          j _ f                    ) = if containingModule f == containingModule symbol then pure (ldftn 0xFF o) else ldftn <$> moduleImport j symbol f <*> pure o where o = functionOffset context f
        go (LdnullInstruction         _ _                      ) = pure ldnull

        go (LdargInstruction          j s p                    ) = select11  j t   ldarg_i8 ldarg_i16 ldarg_i32 ldarg_i64 ldarg_u8 ldarg_u16 ldarg_u32 ldarg_u64 ldarg_f32 ldarg_f64 ldarg_ref <*> stackSlot j (parameterStackSlot s p) where t = parameterSymbol_parameterType p
        go (LdargaInstruction         j s p                    ) = ldarga                                                                                                                      <$> stackSlot j (parameterStackSlot s p)
        go (StargInstruction          j s p                    ) = select11  j t   starg_i8 starg_i16 starg_i32 starg_i64 starg_u8 starg_u16 starg_u32 starg_u64 starg_f32 starg_f64 starg_ref <*> stackSlot j (parameterStackSlot s p) where t = parameterSymbol_parameterType p

        go (LdlocInstruction          j s l                    ) = select11  j t   ldloc_i8 ldloc_i16 ldloc_i32 ldloc_i64 ldloc_u8 ldloc_u16 ldloc_u32 ldloc_u64 ldloc_f32 ldloc_f64 ldloc_ref <*> stackSlot j (localStackSlot s l) where t = localSymbol_localType l
        go (LdlocaInstruction         j s l                    ) = ldloca                                                                                                                      <$> stackSlot j (localStackSlot s l)
        go (StlocInstruction          j s l                    ) = select11  j t   stloc_i8 stloc_i16 stloc_i32 stloc_i64 stloc_u8 stloc_u16 stloc_u32 stloc_u64 stloc_f32 stloc_f64 stloc_ref <*> stackSlot j (localStackSlot s l) where t = localSymbol_localType l

        go (LdindInstruction          j _ t                    ) = select11  j t   ldind_i8 ldind_i16 ldind_i32 ldind_i64 ldind_u8 ldind_u16 ldind_u32 ldind_u64 ldind_f32 ldind_f64 ldind_ref
        go (StindInstruction          j _ t                    ) = select11  j t   stind_i8 stind_i16 stind_i32 stind_i64 stind_u8 stind_u16 stind_u32 stind_u64 stind_f32 stind_f64 stind_ref

        go (LdfldInstruction          j _ f                    ) = select11  j t   ldfld_i8 ldfld_i16 ldfld_i32 ldfld_i64 ldfld_u8 ldfld_u16 ldfld_u32 ldfld_u64 ldfld_f32 ldfld_f64 ldfld_ref <*> pure (fromIntegral (fieldSymbol_offset f)) where t = fieldSymbol_fieldType f
        go (LdfldaInstruction         _ _ f                    ) = ldflda                                                                                                                      <$> pure (fromIntegral (fieldSymbol_offset f))
        go (StfldInstruction          j _ f                    ) = select11  j t   stfld_i8 stfld_i16 stfld_i32 stfld_i64 stfld_u8 stfld_u16 stfld_u32 stfld_u64 stfld_f32 stfld_f64 stfld_ref <*> pure (fromIntegral (fieldSymbol_offset f)) where t = fieldSymbol_fieldType f

        go (LdelemInstruction         j _ t                    ) = select11  j t   ldelem_i8 ldelem_i16 ldelem_i32 ldelem_i64 ldelem_u8 ldelem_u16 ldelem_u32 ldelem_u64 ldelem_f32 ldelem_f64 ldelem_ref
        go (LdelemaInstruction        _ _ t                    ) = pure (ldelema (fromIntegral (typeLayout_size (typeLayout t))))
        go (StelemInstruction         j _ t                    ) = select11  j t   stelem_i8 stelem_i16 stelem_i32 stelem_i64 stelem_u8 stelem_u16 stelem_u32 stelem_u64 stelem_f32 stelem_f64 stelem_ref

        go (AddInstruction            j _ t                    ) = select4   j t   add_i32         add_i64         add_f32         add_f64
        go (DivInstruction            j _ t                    ) = select4   j t   div_i32         div_i64         div_f32         div_f64
        go (DivUnInstruction          j _ t                    ) = select4   j t   div_i32_un      div_i64_un      div_f32         div_f64
        go (MulInstruction            j _ t                    ) = select4   j t   mul_i32         mul_i64         mul_f32         mul_f64
        go (RemInstruction            j _ t                    ) = select4   j t   rem_i32         rem_i64         rem_f32         rem_f64
        go (RemUnInstruction          j _ t                    ) = select4   j t   rem_i32_un      rem_i64_un      rem_f32         rem_f64
        go (SubInstruction            j _ t                    ) = select4   j t   sub_i32         sub_i64         sub_f32         sub_f64

        go (NegInstruction            j _ t                    ) = select4   j t   neg_i32         neg_i64         neg_f32         neg_f64

        go (AndInstruction            j _ t                    ) = select2   j t   and_i32         and_i64
        go (OrInstruction             j _ t                    ) = select2   j t   or_i32          or_i64
        go (XorInstruction            j _ t                    ) = select2   j t   xor_i32         xor_i64

        go (ShlInstruction            j _ t                    ) = select2   j t   shl_i32         shl_i64
        go (ShrInstruction            j _ t                    ) = select2   j t   shr_i32         shr_i64
        go (ShrUnInstruction          j _ t                    ) = select2   j t   shr_i32_un      shr_i64_un

        go (NotInstruction            j _ t                    ) = select2   j t   not_i32         not_i64

        go (CeqInstruction            j _ t                    ) = select5   j t   ceq_i32         ceq_i64         ceq_f32         ceq_f64         ceq_i32
        go (CgtInstruction            j _ t                    ) = select4   j t   cgt_i32         cgt_i64         cgt_f32         cgt_f64
        go (CgtUnInstruction          j _ t                    ) = select4   j t   cgt_i32_un      cgt_i64_un      cgt_f32_un      cgt_f64_un
        go (CltInstruction            j _ t                    ) = select4   j t   clt_i32         clt_i64         clt_f32         clt_f64
        go (CltUnInstruction          j _ t                    ) = select4   j t   clt_i32_un      clt_i64_un      clt_f32_un      clt_f64_un

        go (ConvInstruction           j _ Int8TypeSymbol    t  ) = select4   j t   conv_i8_i32     conv_i8_i64     conv_i8_f32     conv_i8_f64
        go (ConvInstruction           j _ Int16TypeSymbol   t  ) = select4   j t   conv_i16_i32    conv_i16_i64    conv_i16_f32    conv_i16_f64
        go (ConvInstruction           j _ Int32TypeSymbol   t  ) = select4   j t   nop             conv_i32_i64    conv_i32_f32    conv_i32_f64
        go (ConvInstruction           j _ Int64TypeSymbol   t  ) = select4   j t   conv_i64_i32    nop             conv_i64_f32    conv_i64_f64
        go (ConvInstruction           j _ UInt8TypeSymbol   t  ) = select4   j t   conv_u8_i32     conv_u8_i64     conv_u8_f32     conv_u8_f64
        go (ConvInstruction           j _ UInt16TypeSymbol  t  ) = select4   j t   conv_u16_i32    conv_u16_i64    conv_u16_f32    conv_u16_f64
        go (ConvInstruction           j _ UInt32TypeSymbol  t  ) = select4   j t   nop             conv_u32_i64    conv_u32_f32    conv_u32_f64
        go (ConvInstruction           j _ UInt64TypeSymbol  t  ) = select4   j t   conv_u64_i32    nop             conv_u64_f32    conv_u64_f64
        go (ConvInstruction           j _ Float32TypeSymbol t  ) = select4   j t   conv_f32_i32    conv_f32_i64    nop             conv_f32_f64
        go (ConvInstruction           j _ Float64TypeSymbol t  ) = select4   j t   conv_f64_i32    conv_f64_i64    conv_f64_f32    nop
        go (ConvInstruction           j _ _                 _  ) = reportIf True "Unsupported conversion" j >> pure nop

        go (ConvUnInstruction         j _ Float32TypeSymbol t  ) = select4   j t   conv_f32_i32_un conv_f32_i64_un nop             conv_f32_f64
        go (ConvUnInstruction         j _ Float64TypeSymbol t  ) = select4   j t   conv_f64_i32_un conv_f64_i64_un conv_f64_f32    nop
        go (ConvUnInstruction         j _ _                 _  ) = reportIf True "Unsupported conversion" j >> pure nop

--------------------------------------------------------------------------------

slotCount :: [TypeSymbol] -> Int
slotCount = L.foldr (\t a -> quot (typeLayout_size (typeLayout t) + 3) 4 + a) 0

parameterSlotCount :: [ParameterSymbol] -> Int
parameterSlotCount = slotCount . L.map parameterSymbol_parameterType

localsSlotCount :: [LocalSymbol] -> Int
localsSlotCount = slotCount . L.map localSymbol_localType

maxStackSlotCount :: [VerifiedInstruction] -> Int
maxStackSlotCount = L.foldr max 0 . L.map slotCount . L.map (\i -> let VerifiedState s = verifiedInstruction_state i in s)

parameterStackSlot :: VerifiedState -> ParameterSymbol -> Int
parameterStackSlot (VerifiedState stack) parameter =
    slotCount stack +
    slotCount (L.map localSymbol_localType (functionSymbol_locals (containingFunction parameter))) + 
    slotCount (L.map parameterSymbol_parameterType (L.takeWhile (/=parameter) (L.reverse (functionSymbol_parameters (containingFunction parameter)))))

localStackSlot :: VerifiedState -> LocalSymbol -> Int
localStackSlot (VerifiedState stack) local =
    slotCount stack +
    slotCount (L.map localSymbol_localType (L.takeWhile (/=local) (L.reverse (functionSymbol_locals (containingFunction local)))))

--------------------------------------------------------------------------------

moduleFingerprint :: Maybe [CompiledModule] -> ModuleSymbol -> B.ByteString
moduleFingerprint Nothing        _       = B.replicate 12 0
moduleFingerprint (Just modules) module_ = fingerprint
    where
        Just (CompiledModule _ _ _ _ _ _ _ _ _ _ fingerprint) = L.find (\(CompiledModule symbol _ _ _ _ _ _ _ _ _ _) -> symbol == module_) modules

functionOffset :: Maybe [CompiledModule] -> FunctionSymbol -> Word16
functionOffset Nothing _ = 0
functionOffset (Just modules) function = fromIntegral offset
    where
        Just (CompiledModule _ _ _ _ _ _ _ _ functions _ _) = L.find (\(CompiledModule symbol _ _ _ _ _ _ _ _ _ _) -> symbol == containingModule function) modules
        Just (CompiledFunction _ offset _ _ _ _) = L.find (\(CompiledFunction symbol _ _ _ _ _) -> symbol == function) functions

moduleImport :: Location -> FunctionSymbol -> FunctionSymbol -> Compiler Word8
moduleImport location caller callee =
    case L.elemIndex (containingModule callee) (moduleSymbol_imports (containingModule caller)) of
        Nothing    -> reportIf True ("The module '" <> symbolName (containingModule callee) <> "' is not imported") location >> return 0
        Just index -> return (fromIntegral index)

branch :: forall a. (Integral a, Bounded a) => Maybe [CompiledModule] -> FunctionSymbol -> Int64 -> Location -> LabelSymbol -> Compiler ByteCode -> (a -> ByteCode) -> Compiler ByteCode
branch Nothing _ _ _ _ f g = do
    x <- f
    return (x <> g 0)
branch (Just modules) function offset location label f g = do
    x@(ByteCode _ length  _) <- f
    let Just (CompiledModule _ _ _ _ _ _ _ _ functions _ _) = L.find (\(CompiledModule symbol _ _ _ _ _ _ _ _ _ _) -> symbol == containingModule function) modules
        Just (CompiledFunction _ _ _ _ _ instructions) = L.find (\(CompiledFunction symbol _ _ _ _ _) -> symbol == function) functions
        CompiledInstruction offset' _ = instructions L.!! labelSymbol_index label
        ByteCode _ length' _ = g 0
        delta = offset' - (offset + length + length')
    reportIf (delta < fromIntegral (minBound :: a) || delta > fromIntegral (maxBound :: a)) "The branch target is too far away" location
    return (x <> g (fromIntegral delta))

intConstant :: forall a. (Integral a, Bounded a) => Location -> Integer -> Compiler a
intConstant location value = do
    reportIf (value < fromIntegral (minBound :: a) || value > fromIntegral (maxBound :: a)) "The value is outside the range of the constant type" location
    return (fromIntegral value)

stackSlot :: Location -> Int -> Compiler Word8
stackSlot location slot = do
    reportIf (slot < fromIntegral (minBound :: Word8) || slot > fromIntegral (maxBound :: Word8)) "The size of the evaluation stack is too large" location
    return (fromIntegral slot)

--------------------------------------------------------------------------------

select2 :: Location -> TypeSymbol -> a -> a -> Compiler a
select2 _ Int32TypeSymbol i32 _ = return i32
select2 _ Int64TypeSymbol _ i64 = return i64
select2 l t               _ _   = reportIf True ("Instruction supports i32 and i64, but not '" <> show t <> "'") l >> stop

select4 :: Location -> TypeSymbol -> a -> a -> a -> a -> Compiler a
select4 _ Int32TypeSymbol   i32 _ _ _ = return i32
select4 _ Int64TypeSymbol   _ i64 _ _ = return i64
select4 _ Float32TypeSymbol _ _ f32 _ = return f32
select4 _ Float64TypeSymbol _ _ _ f64 = return f64
select4 l t                 _ _ _ _   = reportIf True ("Instruction supports i32, i64, f32, and f64, but not '" <> show t <> "'") l >> stop

select5 :: Location -> TypeSymbol -> a -> a -> a -> a -> a -> Compiler a
select5 _ Int32TypeSymbol        i32 _ _ _ _ = return i32
select5 _ Int64TypeSymbol        _ i64 _ _ _ = return i64
select5 _ Float32TypeSymbol      _ _ f32 _ _ = return f32
select5 _ Float64TypeSymbol      _ _ _ f64 _ = return f64
select5 _ ReferenceTypeSymbol {} _ _ _ _ ref = return ref
select5 l t                      _ _ _ _ _   = reportIf True ("Instruction supports i32, i64, f32, f64, and references, but not '" <> show t <> "'") l >> stop

select11 :: Location -> TypeSymbol -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Compiler a
select11 _ BoolTypeSymbol         _ _ _ _ u8  _ _ _ _ _ _ = return u8
select11 _ Int8TypeSymbol         i8  _ _ _ _ _ _ _ _ _ _ = return i8
select11 _ Int16TypeSymbol        _ i16 _ _ _ _ _ _ _ _ _ = return i16
select11 _ Int32TypeSymbol        _ _ i32 _ _ _ _ _ _ _ _ = return i32
select11 _ Int64TypeSymbol        _ _ _ i64 _ _ _ _ _ _ _ = return i64
select11 _ UInt8TypeSymbol        _ _ _ _ u8  _ _ _ _ _ _ = return u8
select11 _ UInt16TypeSymbol       _ _ _ _ _ u16 _ _ _ _ _ = return u16
select11 _ UInt32TypeSymbol       _ _ _ _ _ _ u32 _ _ _ _ = return u32
select11 _ UInt64TypeSymbol       _ _ _ _ _ _ _ u64 _ _ _ = return u64
select11 _ Float32TypeSymbol      _ _ _ _ _ _ _ _ f32 _ _ = return f32
select11 _ Float64TypeSymbol      _ _ _ _ _ _ _ _ _ f64 _ = return f64
select11 _ ReferenceTypeSymbol {} _ _ _ _ _ _ _ _ _ _ ref = return ref
select11 l t                      _ _ _ _ _ _ _ _ _ _ _   = reportIf True ("Instruction supports bool, i8, i16, i32, i64, u8, u16, u32, u64, f32, f64, and references, but not '" <> show t <> "'") l >> stop

--------------------------------------------------------------------------------

nop             = ByteCode (word8 0x00) 1 0x00
break           = ByteCode (word8 0x01) 1 0x00
halt            = ByteCode (word8 0x02) 1 0x00

pop_i32         = ByteCode (word8 0x04) 1 0x00
pop_i64         = ByteCode (word8 0x05) 1 0x00
pop_f32         = ByteCode (word8 0x04) 1 0x00
pop_f64         = ByteCode (word8 0x05) 1 0x00
pop_ref         = ByteCode (word8 0x04) 1 0x00

dup_i32         = ByteCode (word8 0x06) 1 0x00
dup_i64         = ByteCode (word8 0x07) 1 0x00
dup_f32         = ByteCode (word8 0x06) 1 0x00
dup_f64         = ByteCode (word8 0x07) 1 0x00
dup_ref         = ByteCode (word8 0x06) 1 0x00

newobj        s = ByteCode (word8 0x60 <> word16LE s) 3 0x80

newarr        s = ByteCode (word8 0x61 <> word16LE s) 3 0x80
ldlen           = ByteCode (word8 0x04) 1 0x00

call        i o = ByteCode (word8 0x1D <> word8 i <> word16LE o) 4 0x00
call_s        o = ByteCode (word8 0x1C <> word16LE o) 3 0x00
calli           = ByteCode (word8 0x1B) 1 0x00
syscall     a o = ByteCode (word8 0x1E <> int8 a <> word16LE o) 4 0x00

ret             = ByteCode (word8 0x18) 1 0x00
ret_i32         = ByteCode (word8 0x19) 1 0x00
ret_i64         = ByteCode (word8 0x1A) 1 0x00
ret_f32         = ByteCode (word8 0x19) 1 0x00
ret_f64         = ByteCode (word8 0x1A) 1 0x00
ret_ref         = ByteCode (word8 0x19) 1 0x00

br            t = ByteCode (word8 0x23 <> int16LE t) 3 0x00
brfalse       t = ByteCode (word8 0x24 <> int16LE t) 3 0x00
brtrue        t = ByteCode (word8 0x25 <> int16LE t) 3 0x00

br_s          t = ByteCode (word8 0x20 <> int8 t) 2 0x00
brfalse_s     t = ByteCode (word8 0x21 <> int8 t) 2 0x00
brtrue_s      t = ByteCode (word8 0x22 <> int8 t) 2 0x00

ldc_i32_m1      = ByteCode (word8 0x28) 1 0x00
ldc_i32_0       = ByteCode (word8 0x29) 1 0x00
ldc_i32_1       = ByteCode (word8 0x2A) 1 0x00
ldc_i32_2       = ByteCode (word8 0x2B) 1 0x00
ldc_i32_3       = ByteCode (word8 0x2C) 1 0x00
ldc_i32_4       = ByteCode (word8 0x2D) 1 0x00
ldc_i32_5       = ByteCode (word8 0x2E) 1 0x00
ldc_i32_6       = ByteCode (word8 0x2F) 1 0x00
ldc_i32_7       = ByteCode (word8 0x30) 1 0x00
ldc_i32_8       = ByteCode (word8 0x31) 1 0x00

ldc_i32_s     c = ByteCode (word8 0x32 <> int8 c) 2 0x00

ldc_i32       c = ByteCode (word8 0x33 <> int32LE  c) 5 0x00
ldc_u32       c = ByteCode (word8 0x33 <> word32LE c) 5 0x00
ldc_i64       c = ByteCode (word8 0x34 <> int64LE  c) 9 0x00
ldc_u64       c = ByteCode (word8 0x34 <> word64LE c) 9 0x00
ldc_f32       c = ByteCode (word8 0x33 <> floatLE  c) 5 0x00
ldc_f64       c = ByteCode (word8 0x34 <> doubleLE c) 9 0x00

ldftn       i o = ByteCode (word8 0x35 <> word8 i <> word16LE o) 4 0x00

ldnull          = ByteCode (word8 0x29) 1 0x00

ldarg_i8      s = ByteCode (word8 0x0C <> word8 s) 2 0x00
ldarg_u8      s = ByteCode (word8 0x0D <> word8 s) 2 0x00
ldarg_i16     s = ByteCode (word8 0x0E <> word8 s) 2 0x00
ldarg_u16     s = ByteCode (word8 0x0F <> word8 s) 2 0x00
ldarg_i32     s = ByteCode (word8 0x10 <> word8 s) 2 0x00
ldarg_u32     s = ByteCode (word8 0x10 <> word8 s) 2 0x00
ldarg_i64     s = ByteCode (word8 0x11 <> word8 s) 2 0x00
ldarg_u64     s = ByteCode (word8 0x11 <> word8 s) 2 0x00
ldarg_f32     s = ByteCode (word8 0x10 <> word8 s) 2 0x00
ldarg_f64     s = ByteCode (word8 0x11 <> word8 s) 2 0x00
ldarg_ref     s = ByteCode (word8 0x10 <> word8 s) 2 0x00

ldarga        s = ByteCode (word8 0x12 <> word8 s) 2 0x00

starg_i8      s = ByteCode (word8 0x13 <> word8 s) 2 0x00
starg_u8      s = ByteCode (word8 0x13 <> word8 s) 2 0x00
starg_i16     s = ByteCode (word8 0x13 <> word8 s) 2 0x00
starg_u16     s = ByteCode (word8 0x13 <> word8 s) 2 0x00
starg_i32     s = ByteCode (word8 0x13 <> word8 s) 2 0x00
starg_u32     s = ByteCode (word8 0x13 <> word8 s) 2 0x00
starg_i64     s = ByteCode (word8 0x14 <> word8 s) 2 0x00
starg_u64     s = ByteCode (word8 0x14 <> word8 s) 2 0x00
starg_f32     s = ByteCode (word8 0x13 <> word8 s) 2 0x00
starg_f64     s = ByteCode (word8 0x14 <> word8 s) 2 0x00
starg_ref     s = ByteCode (word8 0x13 <> word8 s) 2 0x00

ldloc_i8      s = ByteCode (word8 0x0C <> word8 s) 2 0x00
ldloc_u8      s = ByteCode (word8 0x0D <> word8 s) 2 0x00
ldloc_i16     s = ByteCode (word8 0x0E <> word8 s) 2 0x00
ldloc_u16     s = ByteCode (word8 0x0F <> word8 s) 2 0x00
ldloc_i32     s = ByteCode (word8 0x10 <> word8 s) 2 0x00
ldloc_u32     s = ByteCode (word8 0x10 <> word8 s) 2 0x00
ldloc_i64     s = ByteCode (word8 0x11 <> word8 s) 2 0x00
ldloc_u64     s = ByteCode (word8 0x11 <> word8 s) 2 0x00
ldloc_f32     s = ByteCode (word8 0x10 <> word8 s) 2 0x00
ldloc_f64     s = ByteCode (word8 0x11 <> word8 s) 2 0x00
ldloc_ref     s = ByteCode (word8 0x10 <> word8 s) 2 0x00

ldloca        s = ByteCode (word8 0x12 <> word8 s) 2 0x00

stloc_i8      s = ByteCode (word8 0x13 <> word8 s) 2 0x00
stloc_u8      s = ByteCode (word8 0x13 <> word8 s) 2 0x00
stloc_i16     s = ByteCode (word8 0x13 <> word8 s) 2 0x00
stloc_u16     s = ByteCode (word8 0x13 <> word8 s) 2 0x00
stloc_i32     s = ByteCode (word8 0x13 <> word8 s) 2 0x00
stloc_u32     s = ByteCode (word8 0x13 <> word8 s) 2 0x00
stloc_i64     s = ByteCode (word8 0x14 <> word8 s) 2 0x00
stloc_u64     s = ByteCode (word8 0x14 <> word8 s) 2 0x00
stloc_f32     s = ByteCode (word8 0x13 <> word8 s) 2 0x00
stloc_f64     s = ByteCode (word8 0x14 <> word8 s) 2 0x00
stloc_ref     s = ByteCode (word8 0x13 <> word8 s) 2 0x00

ldind_i8        = ByteCode (word8 0x68 <> word16LE 0) 3 0x80
ldind_u8        = ByteCode (word8 0x69 <> word16LE 0) 3 0x80
ldind_i16       = ByteCode (word8 0x6A <> word16LE 0) 3 0x80
ldind_u16       = ByteCode (word8 0x6B <> word16LE 0) 3 0x80
ldind_i32       = ByteCode (word8 0x6C <> word16LE 0) 3 0x80
ldind_u32       = ByteCode (word8 0x6C <> word16LE 0) 3 0x80
ldind_i64       = ByteCode (word8 0x6D <> word16LE 0) 3 0x80
ldind_u64       = ByteCode (word8 0x6D <> word16LE 0) 3 0x80
ldind_f32       = ByteCode (word8 0x6C <> word16LE 0) 3 0x80
ldind_f64       = ByteCode (word8 0x6D <> word16LE 0) 3 0x80
ldind_ref       = ByteCode (word8 0x6C <> word16LE 0) 3 0x80

stind_i8        = ByteCode (word8 0x6F <> word16LE 0) 3 0x80
stind_u8        = ByteCode (word8 0x6F <> word16LE 0) 3 0x80
stind_i16       = ByteCode (word8 0x70 <> word16LE 0) 3 0x80
stind_u16       = ByteCode (word8 0x70 <> word16LE 0) 3 0x80
stind_i32       = ByteCode (word8 0x71 <> word16LE 0) 3 0x80
stind_u32       = ByteCode (word8 0x71 <> word16LE 0) 3 0x80
stind_i64       = ByteCode (word8 0x72 <> word16LE 0) 3 0x80
stind_u64       = ByteCode (word8 0x72 <> word16LE 0) 3 0x80
stind_f32       = ByteCode (word8 0x71 <> word16LE 0) 3 0x80
stind_f64       = ByteCode (word8 0x72 <> word16LE 0) 3 0x80
stind_ref       = ByteCode (word8 0x71 <> word16LE 0) 3 0x80

ldfld_i8      o = ByteCode (word8 0x68 <> word16LE o) 3 0x80
ldfld_u8      o = ByteCode (word8 0x69 <> word16LE o) 3 0x80
ldfld_i16     o = ByteCode (word8 0x6A <> word16LE o) 3 0x80
ldfld_u16     o = ByteCode (word8 0x6B <> word16LE o) 3 0x80
ldfld_i32     o = ByteCode (word8 0x6C <> word16LE o) 3 0x80
ldfld_u32     o = ByteCode (word8 0x6C <> word16LE o) 3 0x80
ldfld_i64     o = ByteCode (word8 0x6D <> word16LE o) 3 0x80
ldfld_u64     o = ByteCode (word8 0x6D <> word16LE o) 3 0x80
ldfld_f32     o = ByteCode (word8 0x6C <> word16LE o) 3 0x80
ldfld_f64     o = ByteCode (word8 0x6D <> word16LE o) 3 0x80
ldfld_ref     o = ByteCode (word8 0x6C <> word16LE o) 3 0x80

ldflda        o = ByteCode (word8 0x6E <> word16LE o) 3 0x80

stfld_i8      o = ByteCode (word8 0x6F <> word16LE o) 3 0x80
stfld_u8      o = ByteCode (word8 0x6F <> word16LE o) 3 0x80
stfld_i16     o = ByteCode (word8 0x70 <> word16LE o) 3 0x80
stfld_u16     o = ByteCode (word8 0x70 <> word16LE o) 3 0x80
stfld_i32     o = ByteCode (word8 0x71 <> word16LE o) 3 0x80
stfld_u32     o = ByteCode (word8 0x71 <> word16LE o) 3 0x80
stfld_i64     o = ByteCode (word8 0x72 <> word16LE o) 3 0x80
stfld_u64     o = ByteCode (word8 0x72 <> word16LE o) 3 0x80
stfld_f32     o = ByteCode (word8 0x71 <> word16LE o) 3 0x80
stfld_f64     o = ByteCode (word8 0x72 <> word16LE o) 3 0x80
stfld_ref     o = ByteCode (word8 0x71 <> word16LE o) 3 0x80

ldelem_i8       = ByteCode (word8 0x7E) 1 0x80
ldelem_u8       = ByteCode (word8 0x7F) 1 0x80
ldelem_i16      = ByteCode (word8 0x80) 1 0x80
ldelem_u16      = ByteCode (word8 0x81) 1 0x80
ldelem_i32      = ByteCode (word8 0x82) 1 0x80
ldelem_u32      = ByteCode (word8 0x82) 1 0x80
ldelem_i64      = ByteCode (word8 0x83) 1 0x80
ldelem_u64      = ByteCode (word8 0x83) 1 0x80
ldelem_f32      = ByteCode (word8 0x82) 1 0x80
ldelem_f64      = ByteCode (word8 0x83) 1 0x80
ldelem_ref      = ByteCode (word8 0x82) 1 0x80

ldelema       s = ByteCode (word8 0x84 <> word16LE s) 3 0x80

stelem_i8       = ByteCode (word8 0x85) 1 0x80
stelem_u8       = ByteCode (word8 0x85) 1 0x80
stelem_i16      = ByteCode (word8 0x86) 1 0x80
stelem_u16      = ByteCode (word8 0x86) 1 0x80
stelem_i32      = ByteCode (word8 0x87) 1 0x80
stelem_u32      = ByteCode (word8 0x87) 1 0x80
stelem_i64      = ByteCode (word8 0x88) 1 0x80
stelem_u64      = ByteCode (word8 0x88) 1 0x80
stelem_f32      = ByteCode (word8 0x87) 1 0x80
stelem_f64      = ByteCode (word8 0x88) 1 0x80
stelem_ref      = ByteCode (word8 0x87) 1 0x80

add_i32         = ByteCode (word8 0x40) 1 0x00
div_i32         = ByteCode (word8 0x43) 1 0x00
div_i32_un      = ByteCode (word8 0x44) 1 0x00
mul_i32         = ByteCode (word8 0x42) 1 0x00
rem_i32         = ByteCode (word8 0x45) 1 0x00
rem_i32_un      = ByteCode (word8 0x46) 1 0x00
sub_i32         = ByteCode (word8 0x41) 1 0x00

neg_i32         = ByteCode (word8 0x47) 1 0x00

and_i32         = ByteCode (word8 0x4B) 1 0x00
or_i32          = ByteCode (word8 0x4C) 1 0x00
xor_i32         = ByteCode (word8 0x4D) 1 0x00

shl_i32         = ByteCode (word8 0x48) 1 0x00
shr_i32         = ByteCode (word8 0x49) 1 0x00
shr_i32_un      = ByteCode (word8 0x4A) 1 0x00

not_i32         = ByteCode (word8 0x4E) 1 0x00

ceq_i32         = ByteCode (word8 0x4F) 1 0x00
cge_i32         = ByteCode (word8 0x53) 1 0x00
cge_i32_un      = ByteCode (word8 0x54) 1 0x00
cgt_i32         = ByteCode (word8 0x51) 1 0x00
cgt_i32_un      = ByteCode (word8 0x52) 1 0x00
cle_i32         = ByteCode (word8 0x57) 1 0x00
cle_i32_un      = ByteCode (word8 0x58) 1 0x00
clt_i32         = ByteCode (word8 0x55) 1 0x00
clt_i32_un      = ByteCode (word8 0x56) 1 0x00
cne_i32         = ByteCode (word8 0x50) 1 0x00

conv_i8_i32     = ByteCode (word8 0x59) 1 0x00
conv_u8_i32     = ByteCode (word8 0x5A) 1 0x00
conv_i16_i32    = ByteCode (word8 0x5B) 1 0x00
conv_u16_i32    = ByteCode (word8 0x5C) 1 0x00

add_i64         = ByteCode (word8 0x90) 1 0x10
div_i64         = ByteCode (word8 0x93) 1 0x10
div_i64_un      = ByteCode (word8 0x94) 1 0x10
mul_i64         = ByteCode (word8 0x92) 1 0x10
rem_i64         = ByteCode (word8 0x95) 1 0x10
rem_i64_un      = ByteCode (word8 0x96) 1 0x10
sub_i64         = ByteCode (word8 0x91) 1 0x10

neg_i64         = ByteCode (word8 0x97) 1 0x10

and_i64         = ByteCode (word8 0x9B) 1 0x10
or_i64          = ByteCode (word8 0x9C) 1 0x10
xor_i64         = ByteCode (word8 0x9D) 1 0x10

shl_i64         = ByteCode (word8 0x98) 1 0x10
shr_i64         = ByteCode (word8 0x99) 1 0x10
shr_i64_un      = ByteCode (word8 0x9A) 1 0x10

not_i64         = ByteCode (word8 0x9E) 1 0x10

ceq_i64         = ByteCode (word8 0x9F) 1 0x10
cge_i64         = ByteCode (word8 0xA3) 1 0x10
cge_i64_un      = ByteCode (word8 0xA4) 1 0x10
cgt_i64         = ByteCode (word8 0xA1) 1 0x10
cgt_i64_un      = ByteCode (word8 0xA2) 1 0x10
cle_i64         = ByteCode (word8 0xA7) 1 0x10
cle_i64_un      = ByteCode (word8 0xA8) 1 0x10
clt_i64         = ByteCode (word8 0xA5) 1 0x10
clt_i64_un      = ByteCode (word8 0xA6) 1 0x10
cne_i64         = ByteCode (word8 0xA0) 1 0x10

conv_i8_i64     = ByteCode (word8 0xA9) 1 0x10
conv_u8_i64     = ByteCode (word8 0xAA) 1 0x10
conv_i16_i64    = ByteCode (word8 0xAB) 1 0x10
conv_u16_i64    = ByteCode (word8 0xAC) 1 0x10
conv_i32_i64    = ByteCode (word8 0xAD) 1 0x10
conv_u32_i64    = ByteCode (word8 0xAE) 1 0x10

conv_i64_i32    = ByteCode (word8 0xAF) 1 0x10
conv_u64_i32    = ByteCode (word8 0xB0) 1 0x10
conv_i64_f32    = ByteCode (word8 0xB1) 1 0x10
conv_u64_f32    = ByteCode (word8 0xB2) 1 0x10
conv_i64_f64    = ByteCode (word8 0xB3) 1 0x10
conv_u64_f64    = ByteCode (word8 0xB4) 1 0x10

add_f32         = ByteCode (word8 0xC0) 1 0x20
div_f32         = ByteCode (word8 0xC3) 1 0x20
mul_f32         = ByteCode (word8 0xC2) 1 0x20
rem_f32         = ByteCode (word8 0xC4) 1 0x20
sub_f32         = ByteCode (word8 0xC1) 1 0x20

neg_f32         = ByteCode (word8 0xC5) 1 0x20

ceq_f32         = ByteCode (word8 0xC6) 1 0x20
ceq_f32_un      = ByteCode (word8 0xC7) 1 0x20
cge_f32         = ByteCode (word8 0xCC) 1 0x20
cge_f32_un      = ByteCode (word8 0xCD) 1 0x20
cgt_f32         = ByteCode (word8 0xCA) 1 0x20
cgt_f32_un      = ByteCode (word8 0xCB) 1 0x20
cle_f32         = ByteCode (word8 0xD0) 1 0x20
cle_f32_un      = ByteCode (word8 0xD1) 1 0x20
clt_f32         = ByteCode (word8 0xCE) 1 0x20
clt_f32_un      = ByteCode (word8 0xCF) 1 0x20
cne_f32         = ByteCode (word8 0xC8) 1 0x20
cne_f32_un      = ByteCode (word8 0xC9) 1 0x20

conv_i8_f32     = ByteCode (word8 0xD2) 1 0x20
conv_u8_f32     = ByteCode (word8 0xD3) 1 0x20
conv_i16_f32    = ByteCode (word8 0xD4) 1 0x20
conv_u16_f32    = ByteCode (word8 0xD5) 1 0x20
conv_i32_f32    = ByteCode (word8 0xD6) 1 0x20
conv_u32_f32    = ByteCode (word8 0xD7) 1 0x20

conv_f32_i32    = ByteCode (word8 0xD8) 1 0x20
conv_f32_i32_un = ByteCode (word8 0xD9) 1 0x20
conv_f32_i64    = ByteCode (word8 0xDA) 1 0x20
conv_f32_i64_un = ByteCode (word8 0xDB) 1 0x20
conv_f32_f64    = ByteCode (word8 0xDC) 1 0x20

add_f64         = ByteCode (word8 0xE0) 1 0x40
div_f64         = ByteCode (word8 0xE3) 1 0x40
mul_f64         = ByteCode (word8 0xE2) 1 0x40
rem_f64         = ByteCode (word8 0xE4) 1 0x40
sub_f64         = ByteCode (word8 0xE1) 1 0x40

neg_f64         = ByteCode (word8 0xE5) 1 0x40

ceq_f64         = ByteCode (word8 0xE6) 1 0x40
ceq_f64_un      = ByteCode (word8 0xE7) 1 0x40
cge_f64         = ByteCode (word8 0xEC) 1 0x40
cge_f64_un      = ByteCode (word8 0xED) 1 0x40
cgt_f64         = ByteCode (word8 0xEA) 1 0x40
cgt_f64_un      = ByteCode (word8 0xEB) 1 0x40
cle_f64         = ByteCode (word8 0xF0) 1 0x40
cle_f64_un      = ByteCode (word8 0xF1) 1 0x40
clt_f64         = ByteCode (word8 0xEE) 1 0x40
clt_f64_un      = ByteCode (word8 0xEF) 1 0x40
cne_f64         = ByteCode (word8 0xE8) 1 0x40
cne_f64_un      = ByteCode (word8 0xE9) 1 0x40

conv_i8_f64     = ByteCode (word8 0xF2) 1 0x40
conv_u8_f64     = ByteCode (word8 0xF3) 1 0x40
conv_i16_f64    = ByteCode (word8 0xF4) 1 0x40
conv_u16_f64    = ByteCode (word8 0xF5) 1 0x40
conv_i32_f64    = ByteCode (word8 0xF6) 1 0x40
conv_u32_f64    = ByteCode (word8 0xF7) 1 0x40

conv_f64_i32    = ByteCode (word8 0xF8) 1 0x40
conv_f64_i32_un = ByteCode (word8 0xF9) 1 0x40
conv_f64_i64    = ByteCode (word8 0xFA) 1 0x40
conv_f64_i64_un = ByteCode (word8 0xFB) 1 0x40
conv_f64_f32    = ByteCode (word8 0xFC) 1 0x40

--------------------------------------------------------------------------------
