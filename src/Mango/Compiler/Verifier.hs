{-# LANGUAGE NoImplicitPrelude #-}

module Mango.Compiler.Verifier (
    -- Verification
    VerifiedCompilation (..),
    VerifiedModule (..),
    VerifiedFunction (..),
    VerifiedInstruction (..),
    VerifiedState (..),
    verify
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State (StateT, evalStateT, get, lift, put)
import Data.Bool
import Data.Either (Either (..))
import Data.Eq
import Data.Function
import Data.Functor.Identity
import Data.Int (Int)
import Data.Maybe (Maybe (..), fromJust, maybe)
import Data.Ord
import Data.Semigroup
import Data.String (String)
import Data.Vector (Vector, (!), (//))
import Mango.Compiler.Binder
import Mango.Compiler.Error
import Mango.Compiler.Symbols
import Mango.Compiler.Syntax
import Prelude (Num (..), Integer, fromIntegral, undefined)
import Text.Show

import qualified Data.ByteString as B
import qualified Data.List as L
import qualified Data.Vector as V

--------------------------------------------------------------------------------

data VerifiedCompilation
    = VerifiedCompilation [VerifiedModule]

data VerifiedModule
    = VerifiedModule !ModuleSymbol [VerifiedFunction] [ModuleSymbol]

data VerifiedFunction
    = VerifiedFunction       { verifiedFunction_symbol :: !FunctionSymbol, verifiedFunction_instructions  :: Vector VerifiedInstruction }
    | VerifiedExternFunction { verifiedFunction_symbol :: !FunctionSymbol, verifiedExternFunction_ordinal :: !Integer }

data VerifiedInstruction
    = DeadInstruction     { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState                                                                                                 }

    | NopInstruction      { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState                                                                                                 }
    | BreakInstruction    { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState                                                                                                 }

    | PopInstruction      { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_type      :: !TypeSymbol                                                   }
    | DupInstruction      { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_type      :: !TypeSymbol                                                   }

    | NewobjInstruction   { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_function  :: !FunctionSymbol                                               }
    | NewarrInstruction   { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_type      :: !TypeSymbol                                                   }
    | LdlenInstruction    { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState                                                                                                 }

    | CallInstruction     { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_function  :: !FunctionSymbol                                               }
    | CalliInstruction    { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_type      :: !TypeSymbol                                                   }
    | SyscallInstruction  { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_function  :: !FunctionSymbol                                               }
    | RetInstruction      { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_type      :: !TypeSymbol                                                   }

    | BrInstruction       { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_label     :: !LabelSymbol                                                  }
    | BrfalseInstruction  { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_label     :: !LabelSymbol,     verifiedInstruction_type     :: !TypeSymbol }
    | BrtrueInstruction   { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_label     :: !LabelSymbol,     verifiedInstruction_type     :: !TypeSymbol }

    | BeqInstruction      { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_label     :: !LabelSymbol,     verifiedInstruction_type     :: !TypeSymbol }
    | BgeInstruction      { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_label     :: !LabelSymbol,     verifiedInstruction_type     :: !TypeSymbol }
    | BgeUnInstruction    { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_label     :: !LabelSymbol,     verifiedInstruction_type     :: !TypeSymbol }
    | BgtInstruction      { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_label     :: !LabelSymbol,     verifiedInstruction_type     :: !TypeSymbol }
    | BgtUnInstruction    { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_label     :: !LabelSymbol,     verifiedInstruction_type     :: !TypeSymbol }
    | BleInstruction      { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_label     :: !LabelSymbol,     verifiedInstruction_type     :: !TypeSymbol }
    | BleUnInstruction    { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_label     :: !LabelSymbol,     verifiedInstruction_type     :: !TypeSymbol }
    | BltInstruction      { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_label     :: !LabelSymbol,     verifiedInstruction_type     :: !TypeSymbol }
    | BltUnSInstruction   { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_label     :: !LabelSymbol,     verifiedInstruction_type     :: !TypeSymbol }
    | BneUnInstruction    { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_label     :: !LabelSymbol,     verifiedInstruction_type     :: !TypeSymbol }

    | BrSInstruction      { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_label     :: !LabelSymbol                                                  }
    | BrfalseSInstruction { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_label     :: !LabelSymbol,     verifiedInstruction_type     :: !TypeSymbol }
    | BrtrueSInstruction  { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_label     :: !LabelSymbol,     verifiedInstruction_type     :: !TypeSymbol }

    | BeqSInstruction     { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_label     :: !LabelSymbol,     verifiedInstruction_type     :: !TypeSymbol }
    | BgeSInstruction     { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_label     :: !LabelSymbol,     verifiedInstruction_type     :: !TypeSymbol }
    | BgeUnSInstruction   { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_label     :: !LabelSymbol,     verifiedInstruction_type     :: !TypeSymbol }
    | BgtSInstruction     { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_label     :: !LabelSymbol,     verifiedInstruction_type     :: !TypeSymbol }
    | BgtUnSInstruction   { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_label     :: !LabelSymbol,     verifiedInstruction_type     :: !TypeSymbol }
    | BleSInstruction     { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_label     :: !LabelSymbol,     verifiedInstruction_type     :: !TypeSymbol }
    | BleUnSInstruction   { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_label     :: !LabelSymbol,     verifiedInstruction_type     :: !TypeSymbol }
    | BltSInstruction     { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_label     :: !LabelSymbol,     verifiedInstruction_type     :: !TypeSymbol }
    | BltUnInstruction    { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_label     :: !LabelSymbol,     verifiedInstruction_type     :: !TypeSymbol }
    | BneUnSInstruction   { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_label     :: !LabelSymbol,     verifiedInstruction_type     :: !TypeSymbol }

    | LdcInstruction      { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_type      :: !TypeSymbol,      verifiedInstruction_value    :: !Integer    }
    | LdftnInstruction    { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_function  :: !FunctionSymbol                                               }
    | LdnullInstruction   { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState                                                                                                 }

    | LdargInstruction    { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_parameter :: !ParameterSymbol                                              }
    | LdargaInstruction   { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_parameter :: !ParameterSymbol                                              }
    | StargInstruction    { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_parameter :: !ParameterSymbol                                              }

    | LdlocInstruction    { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_local     :: !LocalSymbol                                                  }
    | LdlocaInstruction   { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_local     :: !LocalSymbol                                                  }
    | StlocInstruction    { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_local     :: !LocalSymbol                                                  }

    | LdindInstruction    { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_type      :: !TypeSymbol                                                   }
    | StindInstruction    { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_type      :: !TypeSymbol                                                   }

    | LdfldInstruction    { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_field     :: !FieldSymbol                                                  }
    | LdfldaInstruction   { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_field     :: !FieldSymbol                                                  }
    | StfldInstruction    { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_field     :: !FieldSymbol                                                  }

    | LdelemInstruction   { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_type      :: !TypeSymbol                                                   }
    | LdelemaInstruction  { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_type      :: !TypeSymbol                                                   }
    | StelemInstruction   { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_type      :: !TypeSymbol                                                   }

    | AddInstruction      { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_type      :: !TypeSymbol                                                   }
    | DivInstruction      { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_type      :: !TypeSymbol                                                   }
    | DivUnInstruction    { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_type      :: !TypeSymbol                                                   }
    | MulInstruction      { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_type      :: !TypeSymbol                                                   }
    | RemInstruction      { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_type      :: !TypeSymbol                                                   }
    | RemUnInstruction    { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_type      :: !TypeSymbol                                                   }
    | SubInstruction      { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_type      :: !TypeSymbol                                                   }

    | NegInstruction      { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_type      :: !TypeSymbol                                                   }

    | AndInstruction      { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_type      :: !TypeSymbol                                                   }
    | OrInstruction       { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_type      :: !TypeSymbol                                                   }
    | XorInstruction      { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_type      :: !TypeSymbol                                                   }

    | ShlInstruction      { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_type      :: !TypeSymbol                                                   }
    | ShrInstruction      { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_type      :: !TypeSymbol                                                   }
    | ShrUnInstruction    { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_type      :: !TypeSymbol                                                   }

    | NotInstruction      { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_type      :: !TypeSymbol                                                   }

    | CeqInstruction      { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_type      :: !TypeSymbol                                                   }
    | CgtInstruction      { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_type      :: !TypeSymbol                                                   }
    | CgtUnInstruction    { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_type      :: !TypeSymbol                                                   }
    | CltInstruction      { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_type      :: !TypeSymbol                                                   }
    | CltUnInstruction    { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_type      :: !TypeSymbol                                                   }

    | ConvInstruction     { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_toType    :: !TypeSymbol,      verifiedInstruction_fromType :: !TypeSymbol }
    | ConvUnInstruction   { verifiedInstruction_location :: !Location, verifiedInstruction_state :: !VerifiedState, verifiedInstruction_toType    :: !TypeSymbol,      verifiedInstruction_fromType :: !TypeSymbol }

data VerifiedState
    = VerifiedState ![TypeSymbol]

--------------------------------------------------------------------------------

verify :: (Monad m) => Compilation -> CompilerT Diagnostic m VerifiedCompilation
verify = mapCompilerT (return . runIdentity) . verifyCompilation

--------------------------------------------------------------------------------

type Compiler = CompilerT Diagnostic Identity

type Verifier = StateT VerifierState Compiler

data VerifierState = VerifierState !(Vector (Maybe VerifiedState)) ![Int] !FunctionSymbol !Location

reportAtPosAndStop :: String -> Verifier a
reportAtPosAndStop message = do
    VerifierState _ _ _ location <- get
    lift (report (GenericError location message) >> stop)

--------------------------------------------------------------------------------

verifyModuleReference :: ModuleSymbol -> Compiler ModuleSymbol
verifyModuleReference    s@ErrorModuleSymbol    {} = reportMany (moduleSymbol_diagnostics s) >> return s
verifyModuleReference    s                         = return s

verifyTypeReference :: TypeSymbol -> Compiler TypeSymbol
verifyTypeReference      s@ErrorTypeSymbol      {} = reportMany (structuredTypeSymbol_diagnostics s) >> return s
verifyTypeReference      s@ArrayTypeSymbol      {} = verifyTypeReference (arrayTypeSymbol_elementType s) >> return s
verifyTypeReference      s@FunctionTypeSymbol   {} = verifyTypeReference (functionTypeSymbol_returnType s) >> mapM_ verifyTypeReference (functionTypeSymbol_parameterTypes s) >> return s
verifyTypeReference      s@ReferenceTypeSymbol  {} = verifyTypeReference (referenceTypeSymbol_referencedType s) >> return s
verifyTypeReference      s@SpanTypeSymbol       {} = verifyTypeReference (spanTypeSymbol_elementType s) >> return s
verifyTypeReference      s                         = return s

verifyFieldReference :: FieldSymbol -> Compiler FieldSymbol
verifyFieldReference     s@ErrorFieldSymbol     {} = reportMany (fieldSymbol_diagnostics s) >> return s
verifyFieldReference     s                         = return s

verifyFunctionReference :: FunctionSymbol -> Compiler FunctionSymbol
verifyFunctionReference  s@ErrorFunctionSymbol  {} = reportMany (functionSymbol_diagnostics s) >> return s
verifyFunctionReference  s                         = return s

verifyParameterReference :: ParameterSymbol -> Compiler ParameterSymbol
verifyParameterReference s@ErrorParameterSymbol {} = reportMany (parameterSymbol_diagnostics s) >> return s
verifyParameterReference s                         = return s

verifyLocalReference :: LocalSymbol -> Compiler LocalSymbol
verifyLocalReference     s@ErrorLocalSymbol     {} = reportMany (localSymbol_diagnostics s) >> return s
verifyLocalReference     s                         = return s

verifyLabelReference :: LabelSymbol -> Compiler LabelSymbol
verifyLabelReference     s@ErrorLabelSymbol     {} = reportMany (labelSymbol_diagnostics s) >> return s
verifyLabelReference     s                         = return s

--------------------------------------------------------------------------------

verifyCompilation :: Compilation -> Compiler VerifiedCompilation
verifyCompilation compilation = do
    let semanticModel = createSemanticModel compilation
    verifiedModules <- parallelC (L.concatMap (verifySyntaxTree semanticModel) (compilation_syntaxTrees compilation))
    return (VerifiedCompilation verifiedModules)

verifySyntaxTree :: SemanticModel -> SyntaxTree -> [Compiler VerifiedModule]
verifySyntaxTree semanticModel syntaxTree =
    L.map (verifyDeclaredModule semanticModel) (compilationUnitSyntax_modules (syntaxTree_root syntaxTree))

verifyDeclaredModule :: SemanticModel -> ModuleDeclarationSyntax -> Compiler VerifiedModule
verifyDeclaredModule semanticModel moduleDeclaration = do
    let moduleSymbol = declaredModule semanticModel moduleDeclaration
    importedModules   <- verifyImports moduleSymbol
    _                 <- sequentialC $ L.map (verifyDeclaredType     moduleSymbol) [syntax | syntax@TypeDeclarationSyntax     {} <- moduleDeclarationSyntax_members moduleDeclaration]
    verifiedFunctions <- sequentialC $ L.map (verifyDeclaredFunction moduleSymbol) [syntax | syntax@FunctionDeclarationSyntax {} <- moduleDeclarationSyntax_members moduleDeclaration]
    return (VerifiedModule moduleSymbol verifiedFunctions importedModules)

verifyImports :: ModuleSymbol -> Compiler [ModuleSymbol]
verifyImports moduleSymbol = do
    if L.elem moduleSymbol (moduleDependencies moduleSymbol) then
        report (GenericError (symbolLocation moduleSymbol) "The module directly or indirectly imports itself")
    else
        return ()
    mapM verifyModuleReference (moduleSymbol_imports moduleSymbol)

verifyDeclaredType :: ModuleSymbol -> ModuleMemberSyntax -> Compiler ()
verifyDeclaredType container typeDeclaration = do
    let typeSymbol = declaredType container typeDeclaration
    reportMany (structuredTypeSymbol_diagnostics typeSymbol)
    mapM_ verifyDeclaredField (structuredTypeSymbol_fields typeSymbol)
    return ()

verifyDeclaredField :: FieldSymbol -> Compiler ()
verifyDeclaredField fieldSymbol = do
    reportMany (fieldSymbol_diagnostics fieldSymbol)
    _ <- verifyTypeReference (fieldSymbol_fieldType fieldSymbol)
    return ()

verifyDeclaredFunction :: ModuleSymbol -> ModuleMemberSyntax -> Compiler VerifiedFunction
verifyDeclaredFunction container functionDeclaration = do
    let functionSymbol = declaredFunction container functionDeclaration
    reportMany (functionSymbol_diagnostics functionSymbol)
    _ <- verifyTypeReference (functionSymbol_returnType functionSymbol)
    mapM_ verifyDeclaredParameter (functionSymbol_parameters functionSymbol)
    mapM_ verifyDeclaredLocal (functionSymbol_locals functionSymbol)
    case functionDeclarationSyntax_body functionDeclaration of
        Left (FunctionBodySyntax _ instructions) -> verifyDeclaredFunction' functionSymbol instructions
        Right _                                  -> return (VerifiedExternFunction functionSymbol (fromJust (functionSymbol_ordinal functionSymbol)))

verifyDeclaredParameter :: ParameterSymbol -> Compiler ()
verifyDeclaredParameter parameterSymbol = do
    reportMany (parameterSymbol_diagnostics parameterSymbol)
    _ <- verifyTypeReference (parameterSymbol_parameterType parameterSymbol)
    return ()

verifyDeclaredLocal :: LocalSymbol -> Compiler ()
verifyDeclaredLocal localSymbol = do
    reportMany (localSymbol_diagnostics localSymbol)
    _ <- verifyTypeReference (localSymbol_localType localSymbol)
    return ()

verifyDeclaredFunction' :: FunctionSymbol -> Vector InstructionSyntax -> Compiler VerifiedFunction
verifyDeclaredFunction' functionSymbol instructions = do
    state <- evalStateT verifier initialState
    return (VerifiedFunction functionSymbol (makeVerifiedInstructions functionSymbol state instructions))
    where
        initialState = VerifierState (V.replicate (V.length instructions) Nothing)
                                     []
                                     functionSymbol
                                     (symbolLocation functionSymbol)

        verifier = do
            branchTo 0 []
            verifyNextInstruction instructions
            VerifierState state _ _ _ <- get
            return state

verifyNextInstruction :: Vector InstructionSyntax -> Verifier ()
verifyNextInstruction instructions = do
    VerifierState state queue function pos <- get
    case queue of
        []           -> return ()
        index:queue' -> do
            put $ VerifierState state queue' function pos
            verifyInstructionAt instructions index
            verifyNextInstruction instructions

verifyInstructionAt :: Vector InstructionSyntax -> Int -> Verifier ()
verifyInstructionAt instructions index = do
    VerifierState state queue function _ <- get
    let instruction = leaf (instructions!index)
    let Just (VerifiedState stack) = state!index
    put $ VerifierState state queue function (syntaxLocation instruction)
    verifyInstruction function index instruction stack
    where
        leaf :: InstructionSyntax -> InstructionSyntax
        leaf (LabeledInstructionSyntax _ labeledInstruction) = leaf labeledInstruction
        leaf instruction = instruction

verifyInstruction :: (Symbol a) => a -> Int -> InstructionSyntax -> [TypeSymbol] -> Verifier ()
verifyInstruction context index instruction stack =
    v instruction
    where
        v :: InstructionSyntax -> Verifier ()
        v (LabeledInstructionSyntax _ labeledInstruction) = v labeledInstruction

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        --
        --  Basic
        --

        v NopInstructionSyntax {} = do
            branchTo (index + 1) stack

        v BreakInstructionSyntax {} = do
            branchTo (index + 1) stack

        --
        --  Stack
        --

        v PopInstructionSyntax {} = do
            (_, stack') <- pop1 stack
            branchTo (index + 1) stack'

        v DupInstructionSyntax {} = do
            (value, stack') <- pop1 stack
            branchTo (index + 1) (value:value:stack')

        --
        --  Heap
        --

        v NewobjInstructionSyntax {} =
            reportAtPosAndStop "The 'newobj' instruction is not implemented yet"

        v NewarrInstructionSyntax {} = do
            elementType' <- lift . verifyTypeReference $ bindType context instruction
            (length', stack') <- pop1 stack
            expectInt32 length' stack
            branchTo (index + 1) (intermediateType (SpanTypeSymbol elementType'):stack')

        v LdlenInstructionSyntax {} = do
            (value, stack') <- pop1 stack
            _ <- expectSpan value stack
            branchTo (index + 1) (intermediateType Int32TypeSymbol:stack')

        --
        --  Functions
        --

        v CallInstructionSyntax {} = do
            function <- lift . verifyFunctionReference $ bindFunction context instruction
            verify_Call_Calli_Syscall (functionSymbol_returnType function) (L.map parameterSymbol_parameterType (functionSymbol_parameters function)) stack

        v CalliInstructionSyntax {} = do
            indirectFunctionType' <- lift . verifyTypeReference $ bindType context instruction
            case indirectFunctionType' of
                FunctionTypeSymbol { functionTypeSymbol_returnType = returnType, functionTypeSymbol_parameterTypes = parameterTypes } -> do
                    (value, stack') <- pop1 stack
                    requireAssignableTo value indirectFunctionType'
                    verify_Call_Calli_Syscall returnType parameterTypes stack'
                _ -> reportAtPosAndStop "The type argument of the instruction must be a function type"

        v SyscallInstructionSyntax {} = do
            function <- lift . verifyFunctionReference $ bindFunction context instruction
            verify_Call_Calli_Syscall (functionSymbol_returnType function) (L.map parameterSymbol_parameterType (functionSymbol_parameters function)) stack

        v RetInstructionSyntax {} = do
            VerifierState _ _ function _ <- get
            case functionSymbol_returnType function of
                VoidTypeSymbol -> do
                    requireStackEmpty stack ("The evaluation stack must be empty\n\nCurrent Stack:\n" <> prettyStack stack)
                returnType -> do
                    (value, stack') <- pop1 stack
                    requireAssignableTo value returnType
                    requireStackEmpty stack' ("The evaluation stack must be empty except for the value to be returned\n\nCurrent Stack:\n" <> prettyStack stack)
                    checkLifetime returnType

        --
        --  Branches
        --

        v BrInstructionSyntax       {} = verify_Br
        v BrSInstructionSyntax      {} = verify_Br

        v BrfalseInstructionSyntax  {} = verify_Brfalse_Brtrue
        v BrfalseSInstructionSyntax {} = verify_Brfalse_Brtrue
        v BrtrueInstructionSyntax   {} = verify_Brfalse_Brtrue
        v BrtrueSInstructionSyntax  {} = verify_Brfalse_Brtrue

        v BeqInstructionSyntax      {} = verify_Beq_BneUn
        v BeqSInstructionSyntax     {} = verify_Beq_BneUn
        v BneUnInstructionSyntax    {} = verify_Beq_BneUn
        v BneUnSInstructionSyntax   {} = verify_Beq_BneUn

        v BgeInstructionSyntax      {} = verify_Bge_BgeUn_Bgt_BgtUn_Ble_BleUn_Blt_BltUn
        v BgeSInstructionSyntax     {} = verify_Bge_BgeUn_Bgt_BgtUn_Ble_BleUn_Blt_BltUn
        v BgeUnInstructionSyntax    {} = verify_Bge_BgeUn_Bgt_BgtUn_Ble_BleUn_Blt_BltUn
        v BgeUnSInstructionSyntax   {} = verify_Bge_BgeUn_Bgt_BgtUn_Ble_BleUn_Blt_BltUn
        v BgtInstructionSyntax      {} = verify_Bge_BgeUn_Bgt_BgtUn_Ble_BleUn_Blt_BltUn
        v BgtSInstructionSyntax     {} = verify_Bge_BgeUn_Bgt_BgtUn_Ble_BleUn_Blt_BltUn
        v BgtUnInstructionSyntax    {} = verify_Bge_BgeUn_Bgt_BgtUn_Ble_BleUn_Blt_BltUn
        v BgtUnSInstructionSyntax   {} = verify_Bge_BgeUn_Bgt_BgtUn_Ble_BleUn_Blt_BltUn
        v BleInstructionSyntax      {} = verify_Bge_BgeUn_Bgt_BgtUn_Ble_BleUn_Blt_BltUn
        v BleSInstructionSyntax     {} = verify_Bge_BgeUn_Bgt_BgtUn_Ble_BleUn_Blt_BltUn
        v BleUnInstructionSyntax    {} = verify_Bge_BgeUn_Bgt_BgtUn_Ble_BleUn_Blt_BltUn
        v BleUnSInstructionSyntax   {} = verify_Bge_BgeUn_Bgt_BgtUn_Ble_BleUn_Blt_BltUn
        v BltInstructionSyntax      {} = verify_Bge_BgeUn_Bgt_BgtUn_Ble_BleUn_Blt_BltUn
        v BltSInstructionSyntax     {} = verify_Bge_BgeUn_Bgt_BgtUn_Ble_BleUn_Blt_BltUn
        v BltUnInstructionSyntax    {} = verify_Bge_BgeUn_Bgt_BgtUn_Ble_BleUn_Blt_BltUn
        v BltUnSInstructionSyntax   {} = verify_Bge_BgeUn_Bgt_BgtUn_Ble_BleUn_Blt_BltUn

        --
        --  Constants
        --

        v LdcInstructionSyntax {} = do
            constantType' <- lift . verifyTypeReference $ bindType context instruction
            case constantType' of
                Int32TypeSymbol   -> branchTo (index + 1) (intermediateType constantType':stack)
                Int64TypeSymbol   -> branchTo (index + 1) (intermediateType constantType':stack)
                Float32TypeSymbol -> reportAtPosAndStop "The 'ldc' instruction with an f32 type argument is not implemented yet"
                Float64TypeSymbol -> reportAtPosAndStop "The 'ldc' instruction with an f64 type argument is not implemented yet"
                _                 -> reportAtPosAndStop "The type argument of the instruction must be i32, i64, f32, or f64"

        v LdftnInstructionSyntax {} = do
            function <- lift . verifyFunctionReference $ bindFunction context instruction
            branchTo (index + 1) (intermediateType (functionType function):stack)

        v LdnullInstructionSyntax {} = do
            branchTo (index + 1) (NullTypeSymbol:stack)

        --
        --  Arguments
        --

        v LdargInstructionSyntax {} = do
            parameter <- lift . stopOnError . verifyParameterReference $ bindParameter context instruction
            let parameterType = parameterSymbol_parameterType parameter
            branchTo (index + 1) (intermediateType parameterType:stack)

        v LdargaInstructionSyntax {} = do
            parameter <- lift . stopOnError . verifyParameterReference $ bindParameter context instruction
            let parameterType = parameterSymbol_parameterType parameter
            branchTo (index + 1) (intermediateType (ReferenceTypeSymbol parameterType):stack)

        v StargInstructionSyntax {} = do
            parameter <- lift . stopOnError . verifyParameterReference $ bindParameter context instruction
            let parameterType = parameterSymbol_parameterType parameter
            (value, stack') <- pop1 stack
            requireAssignableTo value parameterType
            branchTo (index + 1) stack'

        --
        --  Locals
        --

        v LdlocInstructionSyntax {} = do
            local <- lift . stopOnError . verifyLocalReference $ bindLocal context instruction
            let localType = localSymbol_localType local
            branchTo (index + 1) (intermediateType localType:stack)

        v LdlocaInstructionSyntax {} = do
            local <- lift . stopOnError . verifyLocalReference $ bindLocal context instruction
            let localType = localSymbol_localType local
            branchTo (index + 1) (intermediateType (ReferenceTypeSymbol localType):stack)

        v StlocInstructionSyntax {} = do
            local <- lift . stopOnError . verifyLocalReference $ bindLocal context instruction
            let localType = localSymbol_localType local
            (value, stack') <- pop1 stack
            requireAssignableTo value localType
            branchTo (index + 1) stack'

        --
        --  References
        --

        v LdindInstructionSyntax {} = do
            referencedType' <- lift . verifyTypeReference $ bindType context instruction
            (address, stack') <- pop1 stack
            sourceType <- expectReference address stack
            requireAssignableTo sourceType referencedType'
            branchTo (index + 1) (intermediateType referencedType':stack')

        v StindInstructionSyntax {} = do
            referencedType' <- lift . verifyTypeReference $ bindType context instruction
            (value, address, stack') <- pop2 stack
            destinationType <- expectReference address stack
            requireAssignableTo value referencedType'
            requireAssignableTo referencedType' destinationType
            checkLifetime referencedType'
            branchTo (index + 1) stack'

        --
        --  Fields
        --

        v LdfldInstructionSyntax {} = do
            field <- lift . verifyFieldReference $ bindField context instruction
            let fieldType = fieldSymbol_fieldType field
            (object, stack') <- pop1 stack
            sourceType <- expectReference object stack
            requireAssignableTo sourceType (containingType field)
            branchTo (index + 1) (intermediateType fieldType:stack')

        v LdfldaInstructionSyntax {} = do
            field <- lift . verifyFieldReference $ bindField context instruction
            let fieldType = fieldSymbol_fieldType field
            (object, stack') <- pop1 stack
            sourceType <- expectReference object stack
            requireAssignableTo sourceType (containingType field)
            branchTo (index + 1) (intermediateType (ReferenceTypeSymbol fieldType):stack')

        v StfldInstructionSyntax {} = do
            field <- lift . verifyFieldReference $ bindField context instruction
            let fieldType = fieldSymbol_fieldType field
            (value, object, stack') <- pop2 stack
            destinationType <- expectReference object stack
            requireAssignableTo (containingType field) destinationType
            requireAssignableTo value fieldType
            checkLifetime fieldType
            branchTo (index + 1) stack'

        --
        --  Elements
        --

        v LdelemInstructionSyntax {} = do
            elementType' <- lift . verifyTypeReference $ bindType context instruction
            (index', array, stack') <- pop2 stack
            sourceType <- expectSpan array stack
            requireAssignableTo sourceType elementType'
            expectInt32 index' stack
            branchTo (index + 1) (intermediateType elementType':stack')

        v LdelemaInstructionSyntax {} = do
            elementType' <- lift . verifyTypeReference $ bindType context instruction
            (index', array, stack') <- pop2 stack
            sourceType <- expectSpan array stack
            requireAssignableTo sourceType elementType'
            expectInt32 index' stack
            branchTo (index + 1) (intermediateType (ReferenceTypeSymbol elementType'):stack')

        v StelemInstructionSyntax {} = do
            elementType' <- lift . verifyTypeReference $ bindType context instruction
            (value, index', array, stack') <- pop3 stack
            destinationType <- expectSpan array stack
            requireAssignableTo elementType' destinationType
            expectInt32 index' stack
            requireAssignableTo value elementType'
            checkLifetime elementType'
            branchTo (index + 1) stack'

        --
        --  Arithmetic
        --

        v AddInstructionSyntax   {} = verify_Add_Div_DivUn_Mul_Rem_RemUn_Sub
        v DivInstructionSyntax   {} = verify_Add_Div_DivUn_Mul_Rem_RemUn_Sub
        v DivUnInstructionSyntax {} = verify_Add_Div_DivUn_Mul_Rem_RemUn_Sub
        v MulInstructionSyntax   {} = verify_Add_Div_DivUn_Mul_Rem_RemUn_Sub
        v RemInstructionSyntax   {} = verify_Add_Div_DivUn_Mul_Rem_RemUn_Sub
        v RemUnInstructionSyntax {} = verify_Add_Div_DivUn_Mul_Rem_RemUn_Sub
        v SubInstructionSyntax   {} = verify_Add_Div_DivUn_Mul_Rem_RemUn_Sub

        v NegInstructionSyntax {} = do
            (value, stack') <- pop1 stack
            expectNumeric value stack
            branchTo (index + 1) (value:stack')

        --
        --  Bitwise
        --

        v AndInstructionSyntax   {} = verify_And_Or_Xor
        v OrInstructionSyntax    {} = verify_And_Or_Xor
        v XorInstructionSyntax   {} = verify_And_Or_Xor

        v ShlInstructionSyntax   {} = verify_Shl_Shr_ShrUn
        v ShrInstructionSyntax   {} = verify_Shl_Shr_ShrUn
        v ShrUnInstructionSyntax {} = verify_Shl_Shr_ShrUn

        v NotInstructionSyntax {} = do
            (value, stack') <- pop1 stack
            expectInt value stack
            branchTo (index + 1) (value:stack')

        --
        --  Comparison
        --

        v CeqInstructionSyntax   {} = verify_Ceq

        v CgtInstructionSyntax   {} = verify_Cgt_CgtUn_Clt_CltUn
        v CgtUnInstructionSyntax {} = verify_Cgt_CgtUn_Clt_CltUn
        v CltInstructionSyntax   {} = verify_Cgt_CgtUn_Clt_CltUn
        v CltUnInstructionSyntax {} = verify_Cgt_CgtUn_Clt_CltUn

        --
        --  Conversion
        --

        v ConvInstructionSyntax   {} = verify_Conv_ConvUn
        v ConvUnInstructionSyntax {} = verify_Conv_ConvUn

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        verify_Call_Calli_Syscall :: TypeSymbol -> [TypeSymbol] -> [TypeSymbol] -> Verifier ()
        verify_Call_Calli_Syscall returnType parameterTypes stack' = do
            let parameterTypes' = L.reverse parameterTypes
            case stripPrefix parameterTypes' stack' of
                Nothing ->
                    reportAtPosAndStop $ "The values on the stack do not match the expected argument types\n\nExpected:\n" <> (prettyStack parameterTypes') <> "\n\nCurrent Stack:\n" <> (prettyStack' (L.length parameterTypes') stack')
                Just stack'' ->
                    if returnType == VoidTypeSymbol then
                        branchTo (index + 1) stack''
                    else
                        branchTo (index + 1) (intermediateType returnType:stack'')
            where
                stripPrefix :: [TypeSymbol] -> [TypeSymbol] -> Maybe [TypeSymbol]
                stripPrefix [] ys = Just ys
                stripPrefix (x:xs) (y:ys)
                    | verifierAssignableTo y x = stripPrefix xs ys
                stripPrefix _ _ = Nothing

        verify_Br :: Verifier ()
        verify_Br = do
            target <- lift . stopOnError . verifyLabelReference $ bindLabel context instruction
            branchTo (labelSymbol_index target) stack

        verify_Brfalse_Brtrue :: Verifier ()
        verify_Brfalse_Brtrue = do
            target <- lift . stopOnError . verifyLabelReference $ bindLabel context instruction
            (value, stack') <- pop1 stack
            expectInt32OrRef value stack
            branchTo (labelSymbol_index target) stack'
            branchTo (index + 1) stack'

        verify_Beq_BneUn :: Verifier ()
        verify_Beq_BneUn = do
            target <- lift . stopOnError . verifyLabelReference $ bindLabel context instruction
            (value2, value1, stack') <- pop2 stack
            expectNumericOrRef value1 stack
            expectSame value1 value2 stack
            branchTo (labelSymbol_index target) stack'
            branchTo (index + 1) stack'

        verify_Bge_BgeUn_Bgt_BgtUn_Ble_BleUn_Blt_BltUn :: Verifier ()
        verify_Bge_BgeUn_Bgt_BgtUn_Ble_BleUn_Blt_BltUn = do
            target <- lift . stopOnError . verifyLabelReference $ bindLabel context instruction
            (value2, value1, stack') <- pop2 stack
            expectNumeric value1 stack
            expectSame value1 value2 stack
            branchTo (labelSymbol_index target) stack'
            branchTo (index + 1) stack'

        verify_Add_Div_DivUn_Mul_Rem_RemUn_Sub :: Verifier ()
        verify_Add_Div_DivUn_Mul_Rem_RemUn_Sub = do
            (value2, value1, stack') <- pop2 stack
            expectNumeric value1 stack
            expectSame value1 value2 stack
            branchTo (index + 1) (value1:stack')

        verify_And_Or_Xor :: Verifier ()
        verify_And_Or_Xor = do
            (value2, value1, stack') <- pop2 stack
            expectInt value1 stack
            expectSame value1 value2 stack
            branchTo (index + 1) (value1:stack')

        verify_Shl_Shr_ShrUn :: Verifier ()
        verify_Shl_Shr_ShrUn = do
            (amount, value, stack') <- pop2 stack
            expectInt value stack
            expectInt32 amount stack
            branchTo (index + 1) (value:stack')

        verify_Ceq :: Verifier ()
        verify_Ceq = do
            (value2, value1, stack') <- pop2 stack
            expectNumericOrRef value1 stack
            expectSame value1 value2 stack
            branchTo (index + 1) (intermediateType BoolTypeSymbol:stack')

        verify_Cgt_CgtUn_Clt_CltUn :: Verifier ()
        verify_Cgt_CgtUn_Clt_CltUn = do
            (value2, value1, stack') <- pop2 stack
            expectNumeric value1 stack
            expectSame value1 value2 stack
            branchTo (index + 1) (intermediateType BoolTypeSymbol:stack')

        verify_Conv_ConvUn = do
            toType <- lift . verifyTypeReference $ bindType context instruction
            check toType
            (value, stack') <- pop1 stack
            expectNumeric value stack
            branchTo (index + 1) (intermediateType toType:stack')
            where
                check :: TypeSymbol -> Verifier ()
                check Int8TypeSymbol    = return ()
                check Int16TypeSymbol   = return ()
                check Int32TypeSymbol   = return ()
                check Int64TypeSymbol   = return ()
                check UInt8TypeSymbol   = return ()
                check UInt16TypeSymbol  = return ()
                check UInt32TypeSymbol  = return ()
                check UInt64TypeSymbol  = return ()
                check Float32TypeSymbol = return ()
                check Float64TypeSymbol = return ()
                check _                 = reportAtPosAndStop "The type argument of the instruction must be i8, i16, i32, i64, u8, u16, u32, u64, f32, or f64"

--------------------------------------------------------------------------------

verificationType :: TypeSymbol -> TypeSymbol
verificationType   BoolTypeSymbol                                = Int8TypeSymbol
verificationType   Int8TypeSymbol                                = Int8TypeSymbol
verificationType   Int16TypeSymbol                               = Int16TypeSymbol
verificationType   Int32TypeSymbol                               = Int32TypeSymbol
verificationType   Int64TypeSymbol                               = Int64TypeSymbol
verificationType   UInt8TypeSymbol                               = Int8TypeSymbol
verificationType   UInt16TypeSymbol                              = Int16TypeSymbol
verificationType   UInt32TypeSymbol                              = Int32TypeSymbol
verificationType   UInt64TypeSymbol                              = Int64TypeSymbol
verificationType   Float32TypeSymbol                             = Float32TypeSymbol
verificationType   Float64TypeSymbol                             = Float64TypeSymbol
verificationType   VoidTypeSymbol                                = undefined
verificationType  (ArrayTypeSymbol elementType length_)          = ArrayTypeSymbol (verificationType elementType) length_
verificationType t@StructuredTypeSymbol {}                       = t
verificationType t@ErrorTypeSymbol {}                            = t
verificationType  (FunctionTypeSymbol returnType parameterTypes) = FunctionTypeSymbol (verificationType returnType) (L.map verificationType parameterTypes)
verificationType  (ReferenceTypeSymbol referencedType)           = ReferenceTypeSymbol (verificationType referencedType)
verificationType  (SpanTypeSymbol elementType)                   = SpanTypeSymbol (verificationType elementType)
verificationType   NullTypeSymbol                                = undefined

intermediateType :: TypeSymbol -> TypeSymbol
intermediateType t = go (verificationType t)
    where
        go Int8TypeSymbol  = Int32TypeSymbol
        go Int16TypeSymbol = Int32TypeSymbol
        go Int32TypeSymbol = Int32TypeSymbol
        go t'              = t'

verifierAssignableTo :: TypeSymbol -> TypeSymbol -> Bool
verifierAssignableTo s t = go s (verificationType t)
    where
        go NullTypeSymbol  ReferenceTypeSymbol {} = True
        go Int32TypeSymbol Int8TypeSymbol         = True
        go Int32TypeSymbol Int16TypeSymbol        = True
        go Int32TypeSymbol Int32TypeSymbol        = True
        go s'              t'                     = s' == t'

--------------------------------------------------------------------------------

branchTo :: Int -> [TypeSymbol] -> Verifier ()
branchTo target stack = do
    VerifierState state queue function pos <- get
    if target >= V.length state then
        lift (report (GenericError (symbolLocation function) "Not all code paths are terminated"))
    else
        case state!target of
            Nothing -> do
                put $ VerifierState (state // [(target, Just (VerifiedState stack))])
                                    (if L.elem target queue then queue else target:queue)
                                    function
                                    pos
            Just (VerifiedState existing) -> do
                stack' <- mergeStackStates existing stack
                if stack' == existing then
                    return ()
                else
                    put $ VerifierState (state // [(target, Just (VerifiedState stack'))])
                                        (if L.elem target queue then queue else target:queue)
                                        function
                                        pos

mergeStackStates :: [TypeSymbol] -> [TypeSymbol] -> Verifier [TypeSymbol]
mergeStackStates existing stack = go existing stack
    where
        go :: [TypeSymbol] -> [TypeSymbol] -> Verifier [TypeSymbol]
        go (x:xs) (y:ys) = (:) <$> merge x y <*> go xs ys
        go []     []     = return []
        go _      _      = reportAtPosAndStop $ "Could not merge stack states: Different number of items on the stack\n\nTarget Stack:\n" <> (prettyStack existing) <> "\n\nCurrent Stack:\n" <> (prettyStack stack)

        merge :: TypeSymbol -> TypeSymbol -> Verifier TypeSymbol
        merge VoidTypeSymbol           _                        = undefined
        merge _                        VoidTypeSymbol           = undefined
        merge NullTypeSymbol           t@ReferenceTypeSymbol {} = return t
        merge s@ReferenceTypeSymbol {} NullTypeSymbol           = return s
        merge s                        t                        | s == t    = return s
                                                                | otherwise = reportAtPosAndStop $ "Could not merge stack states: '" <> (show t) <> "' is not compatible with '" <> (show s) <> "'\n\nTarget Stack:\n" <> (prettyStack existing) <> "\n\nCurrent Stack:\n" <> (prettyStack stack)

--------------------------------------------------------------------------------

requireAssignableTo :: TypeSymbol -> TypeSymbol -> Verifier ()
requireAssignableTo s t | verifierAssignableTo s t = return ()
                        | otherwise                = reportAtPosAndStop $ "'" <> (show s) <> "' is not assignable to '" <> (show t) <> "'"

requireStackEmpty :: [TypeSymbol] -> String -> Verifier ()
requireStackEmpty [] _       = return ()
requireStackEmpty _  message = reportAtPosAndStop message

pop1 :: [TypeSymbol] -> Verifier (TypeSymbol, [TypeSymbol])
pop1 (x:xs) = return (x, xs)
pop1 stack  = reportAtPosAndStop $ "Instruction needs a value on the evaluation stack\n\nCurrent Stack:\n" <> (prettyStack stack)

pop2 :: [TypeSymbol] -> Verifier (TypeSymbol, TypeSymbol, [TypeSymbol])
pop2 (x2:x1:xs) = return (x2, x1, xs)
pop2 stack      = reportAtPosAndStop $ "Instruction needs two values on the evaluation stack\n\nCurrent Stack:\n" <> (prettyStack stack)

pop3 :: [TypeSymbol] -> Verifier (TypeSymbol, TypeSymbol, TypeSymbol, [TypeSymbol])
pop3 (x3:x2:x1:xs) = return (x3, x2, x1, xs)
pop3 stack         = reportAtPosAndStop $ "Instruction needs three values on the evaluation stack\n\nCurrent Stack:\n" <> (prettyStack stack)

expectSpan :: TypeSymbol -> [TypeSymbol] -> Verifier TypeSymbol
expectSpan (SpanTypeSymbol elementType) _     = return elementType
expectSpan t                            stack = reportAtPosAndStop $ "Instruction needs a span (actual type is '" <> (show t) <> "')\n\nCurrent Stack:\n" <> (prettyStack stack)

expectReference :: TypeSymbol -> [TypeSymbol] -> Verifier TypeSymbol
expectReference (ReferenceTypeSymbol referencedType) _     = return referencedType
expectReference t                                    stack = reportAtPosAndStop $ "Instruction needs a reference (actual type is '" <> (show t) <> "')\n\nCurrent Stack:\n" <> (prettyStack stack)

expectInt32 :: TypeSymbol -> [TypeSymbol] -> Verifier ()
expectInt32 Int32TypeSymbol _     = return ()
expectInt32 t               stack = reportAtPosAndStop $ "Instruction needs an i32 value (actual type is '" <> (show t) <> "')\n\nCurrent Stack:\n" <> (prettyStack stack)

expectInt32OrRef :: TypeSymbol -> [TypeSymbol] -> Verifier ()
expectInt32OrRef Int32TypeSymbol        _     = return ()
expectInt32OrRef ReferenceTypeSymbol {} _     = return ()
expectInt32OrRef t                      stack = reportAtPosAndStop $ "Instruction needs an i32 value or reference (actual type is '" <> (show t) <> "')\n\nCurrent Stack:\n" <> (prettyStack stack)

expectInt :: TypeSymbol -> [TypeSymbol] -> Verifier ()
expectInt Int32TypeSymbol _     = return ()
expectInt Int64TypeSymbol _     = return ()
expectInt t               stack = reportAtPosAndStop $ "Instruction needs an integer type (actual type is '" <> (show t) <> "')\n\nCurrent Stack:\n" <> (prettyStack stack)

expectNumeric :: TypeSymbol -> [TypeSymbol] -> Verifier ()
expectNumeric Int32TypeSymbol   _     = return ()
expectNumeric Int64TypeSymbol   _     = return ()
expectNumeric Float32TypeSymbol _     = return ()
expectNumeric Float64TypeSymbol _     = return ()
expectNumeric t                 stack = reportAtPosAndStop $ "Instruction needs a numeric value (actual type is '" <> (show t) <> "')\n\nCurrent Stack:\n" <> (prettyStack stack)

expectNumericOrRef :: TypeSymbol -> [TypeSymbol] -> Verifier ()
expectNumericOrRef Int32TypeSymbol        _     = return ()
expectNumericOrRef Int64TypeSymbol        _     = return ()
expectNumericOrRef Float32TypeSymbol      _     = return ()
expectNumericOrRef Float64TypeSymbol      _     = return ()
expectNumericOrRef ReferenceTypeSymbol {} _     = return ()
expectNumericOrRef t                      stack = reportAtPosAndStop $ "Instruction needs a numeric value or reference (actual type is '" <> (show t) <> "')\n\nCurrent Stack:\n" <> (prettyStack stack)

expectSame :: TypeSymbol -> TypeSymbol -> [TypeSymbol] -> Verifier ()
expectSame expected actual stack | actual == expected = return ()
                                 | otherwise          = reportAtPosAndStop $ (show actual) <> "' is not compatible with '" <> (show expected) <> "')\n\nCurrent Stack:\n" <> (prettyStack stack)

checkLifetime :: TypeSymbol -> Verifier ()
checkLifetime ReferenceTypeSymbol {} = reportAtPosAndStop "Cannot use instruction in this context because it may expose referenced variables outside of their declaration scope"
checkLifetime SpanTypeSymbol      {} = reportAtPosAndStop "Cannot use instruction in this context because it may expose referenced variables outside of their declaration scope"
checkLifetime _                      = return ()

prettyStack :: [TypeSymbol] -> String
prettyStack (x:xs) = "    " <> show x <> L.concatMap ((<>) "\n    " . show) xs
prettyStack []     = "    (empty)"

prettyStack' :: Int -> [TypeSymbol] -> String
prettyStack' n xs = if L.length xs > n then prettyStack (L.take n xs) <> "\n    ..." else prettyStack xs

--------------------------------------------------------------------------------

makeVerifiedInstructions :: FunctionSymbol -> Vector (Maybe VerifiedState) -> Vector InstructionSyntax -> Vector VerifiedInstruction
makeVerifiedInstructions function =
    V.zipWith $ maybe (\i -> DeadInstruction (syntaxLocation i) (VerifiedState [])) go
    where
        go :: VerifiedState -> InstructionSyntax -> VerifiedInstruction
        go s (LabeledInstructionSyntax _ labeledInstruction) = go s labeledInstruction

        go s i@NopInstructionSyntax      {} = NopInstruction      (syntaxLocation i) s
        go s i@BreakInstructionSyntax    {} = BreakInstruction    (syntaxLocation i) s

        go s i@PopInstructionSyntax      {} = PopInstruction      (syntaxLocation i) s value where VerifiedState (value:_) = s
        go s i@DupInstructionSyntax      {} = DupInstruction      (syntaxLocation i) s value where VerifiedState (value:_) = s

        go s i@NewobjInstructionSyntax   {} = NewobjInstruction   (syntaxLocation i) s (bindFunction function i)
        go s i@NewarrInstructionSyntax   {} = NewarrInstruction   (syntaxLocation i) s (bindType function i)
        go s i@LdlenInstructionSyntax    {} = LdlenInstruction    (syntaxLocation i) s

        go s i@CallInstructionSyntax     {} = CallInstruction     (syntaxLocation i) s (bindFunction function i)
        go s i@CalliInstructionSyntax    {} = CalliInstruction    (syntaxLocation i) s (bindType function i)
        go s i@SyscallInstructionSyntax  {} = SyscallInstruction  (syntaxLocation i) s (bindFunction function i)
        go s i@RetInstructionSyntax      {} = RetInstruction      (syntaxLocation i) s (if returnsVoid function then VoidTypeSymbol else intermediateType (functionSymbol_returnType function))

        go s i@BrInstructionSyntax       {} = BrInstruction       (syntaxLocation i) s (bindLabel function i)
        go s i@BrfalseInstructionSyntax  {} = BrfalseInstruction  (syntaxLocation i) s (bindLabel function i) value where VerifiedState (value:_) = s
        go s i@BrtrueInstructionSyntax   {} = BrtrueInstruction   (syntaxLocation i) s (bindLabel function i) value where VerifiedState (value:_) = s

        go s i@BeqInstructionSyntax      {} = BeqInstruction      (syntaxLocation i) s (bindLabel function i) value where VerifiedState (value:_) = s
        go s i@BgeInstructionSyntax      {} = BgeInstruction      (syntaxLocation i) s (bindLabel function i) value where VerifiedState (value:_) = s
        go s i@BgeUnInstructionSyntax    {} = BgeUnInstruction    (syntaxLocation i) s (bindLabel function i) value where VerifiedState (value:_) = s
        go s i@BgtInstructionSyntax      {} = BgtInstruction      (syntaxLocation i) s (bindLabel function i) value where VerifiedState (value:_) = s
        go s i@BgtUnInstructionSyntax    {} = BgtUnInstruction    (syntaxLocation i) s (bindLabel function i) value where VerifiedState (value:_) = s
        go s i@BleInstructionSyntax      {} = BleInstruction      (syntaxLocation i) s (bindLabel function i) value where VerifiedState (value:_) = s
        go s i@BleUnInstructionSyntax    {} = BleUnInstruction    (syntaxLocation i) s (bindLabel function i) value where VerifiedState (value:_) = s
        go s i@BltInstructionSyntax      {} = BltInstruction      (syntaxLocation i) s (bindLabel function i) value where VerifiedState (value:_) = s
        go s i@BltUnSInstructionSyntax   {} = BltUnSInstruction   (syntaxLocation i) s (bindLabel function i) value where VerifiedState (value:_) = s
        go s i@BneUnInstructionSyntax    {} = BneUnInstruction    (syntaxLocation i) s (bindLabel function i) value where VerifiedState (value:_) = s

        go s i@BrSInstructionSyntax      {} = BrSInstruction      (syntaxLocation i) s (bindLabel function i)
        go s i@BrfalseSInstructionSyntax {} = BrfalseSInstruction (syntaxLocation i) s (bindLabel function i) value where VerifiedState (value:_) = s
        go s i@BrtrueSInstructionSyntax  {} = BrtrueSInstruction  (syntaxLocation i) s (bindLabel function i) value where VerifiedState (value:_) = s

        go s i@BeqSInstructionSyntax     {} = BeqSInstruction     (syntaxLocation i) s (bindLabel function i) value where VerifiedState (value:_) = s
        go s i@BgeSInstructionSyntax     {} = BgeSInstruction     (syntaxLocation i) s (bindLabel function i) value where VerifiedState (value:_) = s
        go s i@BgeUnSInstructionSyntax   {} = BgeUnSInstruction   (syntaxLocation i) s (bindLabel function i) value where VerifiedState (value:_) = s
        go s i@BgtSInstructionSyntax     {} = BgtSInstruction     (syntaxLocation i) s (bindLabel function i) value where VerifiedState (value:_) = s
        go s i@BgtUnSInstructionSyntax   {} = BgtUnSInstruction   (syntaxLocation i) s (bindLabel function i) value where VerifiedState (value:_) = s
        go s i@BleSInstructionSyntax     {} = BleSInstruction     (syntaxLocation i) s (bindLabel function i) value where VerifiedState (value:_) = s
        go s i@BleUnSInstructionSyntax   {} = BleUnSInstruction   (syntaxLocation i) s (bindLabel function i) value where VerifiedState (value:_) = s
        go s i@BltSInstructionSyntax     {} = BltSInstruction     (syntaxLocation i) s (bindLabel function i) value where VerifiedState (value:_) = s
        go s i@BltUnInstructionSyntax    {} = BltUnInstruction    (syntaxLocation i) s (bindLabel function i) value where VerifiedState (value:_) = s
        go s i@BneUnSInstructionSyntax   {} = BneUnSInstruction   (syntaxLocation i) s (bindLabel function i) value where VerifiedState (value:_) = s

        go s i@LdcInstructionSyntax      {} = LdcInstruction      (syntaxLocation i) s (bindType function i) (parse (constantInstructionSyntax_constantValue i)) where parse NumericLiteralSyntax { numericLiteralSyntax_minus = minus, numericLiteralSyntax_token = token } = maybe id (const negate) minus $ B.foldl' (\a w -> a * 10 + fromIntegral (w - 48)) 0 (syntaxToken_value token)
        go s i@LdftnInstructionSyntax    {} = LdftnInstruction    (syntaxLocation i) s (bindFunction function i)
        go s i@LdnullInstructionSyntax   {} = LdnullInstruction   (syntaxLocation i) s

        go s i@LdargInstructionSyntax    {} = LdargInstruction    (syntaxLocation i) s (bindParameter function i)
        go s i@LdargaInstructionSyntax   {} = LdargaInstruction   (syntaxLocation i) s (bindParameter function i)
        go s i@StargInstructionSyntax    {} = StargInstruction    (syntaxLocation i) s (bindParameter function i)

        go s i@LdlocInstructionSyntax    {} = LdlocInstruction    (syntaxLocation i) s (bindLocal function i)
        go s i@LdlocaInstructionSyntax   {} = LdlocaInstruction   (syntaxLocation i) s (bindLocal function i)
        go s i@StlocInstructionSyntax    {} = StlocInstruction    (syntaxLocation i) s (bindLocal function i)

        go s i@LdindInstructionSyntax    {} = LdindInstruction    (syntaxLocation i) s (bindType function i)
        go s i@StindInstructionSyntax    {} = StindInstruction    (syntaxLocation i) s (bindType function i)

        go s i@LdfldInstructionSyntax    {} = LdfldInstruction    (syntaxLocation i) s (bindField function i)
        go s i@LdfldaInstructionSyntax   {} = LdfldaInstruction   (syntaxLocation i) s (bindField function i)
        go s i@StfldInstructionSyntax    {} = StfldInstruction    (syntaxLocation i) s (bindField function i)

        go s i@LdelemInstructionSyntax   {} = LdelemInstruction   (syntaxLocation i) s (bindType function i)
        go s i@LdelemaInstructionSyntax  {} = LdelemaInstruction  (syntaxLocation i) s (bindType function i)
        go s i@StelemInstructionSyntax   {} = StelemInstruction   (syntaxLocation i) s (bindType function i)

        go s i@AddInstructionSyntax      {} = AddInstruction      (syntaxLocation i) s value where VerifiedState (value:_) = s
        go s i@DivInstructionSyntax      {} = DivInstruction      (syntaxLocation i) s value where VerifiedState (value:_) = s
        go s i@DivUnInstructionSyntax    {} = DivUnInstruction    (syntaxLocation i) s value where VerifiedState (value:_) = s
        go s i@MulInstructionSyntax      {} = MulInstruction      (syntaxLocation i) s value where VerifiedState (value:_) = s
        go s i@RemInstructionSyntax      {} = RemInstruction      (syntaxLocation i) s value where VerifiedState (value:_) = s
        go s i@RemUnInstructionSyntax    {} = RemUnInstruction    (syntaxLocation i) s value where VerifiedState (value:_) = s
        go s i@SubInstructionSyntax      {} = SubInstruction      (syntaxLocation i) s value where VerifiedState (value:_) = s

        go s i@NegInstructionSyntax      {} = NegInstruction      (syntaxLocation i) s value where VerifiedState (value:_) = s

        go s i@AndInstructionSyntax      {} = AndInstruction      (syntaxLocation i) s value where VerifiedState (value:_) = s
        go s i@OrInstructionSyntax       {} = OrInstruction       (syntaxLocation i) s value where VerifiedState (value:_) = s
        go s i@XorInstructionSyntax      {} = XorInstruction      (syntaxLocation i) s value where VerifiedState (value:_) = s

        go s i@ShlInstructionSyntax      {} = ShlInstruction      (syntaxLocation i) s value where VerifiedState (_:value:_) = s
        go s i@ShrInstructionSyntax      {} = ShrInstruction      (syntaxLocation i) s value where VerifiedState (_:value:_) = s
        go s i@ShrUnInstructionSyntax    {} = ShrUnInstruction    (syntaxLocation i) s value where VerifiedState (_:value:_) = s

        go s i@NotInstructionSyntax      {} = NotInstruction      (syntaxLocation i) s value where VerifiedState (value:_) = s

        go s i@CeqInstructionSyntax      {} = CeqInstruction      (syntaxLocation i) s value where VerifiedState (value:_) = s
        go s i@CgtInstructionSyntax      {} = CgtInstruction      (syntaxLocation i) s value where VerifiedState (value:_) = s
        go s i@CgtUnInstructionSyntax    {} = CgtUnInstruction    (syntaxLocation i) s value where VerifiedState (value:_) = s
        go s i@CltInstructionSyntax      {} = CltInstruction      (syntaxLocation i) s value where VerifiedState (value:_) = s
        go s i@CltUnInstructionSyntax    {} = CltUnInstruction    (syntaxLocation i) s value where VerifiedState (value:_) = s

        go s i@ConvInstructionSyntax     {} = ConvInstruction     (syntaxLocation i) s (bindType function i) value where VerifiedState (value:_) = s
        go s i@ConvUnInstructionSyntax   {} = ConvUnInstruction   (syntaxLocation i) s (bindType function i) value where VerifiedState (value:_) = s

--------------------------------------------------------------------------------
