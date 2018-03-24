{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Mango.Compiler.Parser (
    parseText,
    parseBytes,
    parseFile,
    parseFiles,
    ) where

import Control.Applicative
import Control.Applicative.Combinators
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict (StateT (..))
import Data.Bool
import Data.ByteString (ByteString, readFile)
import Data.Char (Char)
import Data.Either (Either (..))
import Data.Eq
import Data.Function
import Data.Ord
import Data.String (String)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Tuple (fst, snd)
import Mango.Compiler.Error
import Mango.Compiler.Lexer
import Mango.Compiler.Syntax
import Prelude (undefined)
import System.IO (FilePath)
import Text.Show

import qualified Data.ByteString.Char8 as C
import qualified Data.List as L
import qualified Data.Vector as V

--------------------------------------------------------------------------------

type Parser = StateT [SyntaxToken] Result

data Result a = Result !a | Error ![(SyntaxToken, String)]

instance Functor Result where
    fmap _ (Error e)  = Error e
    fmap f (Result y) = Result (f y)

instance Applicative Result where
    pure   r       = Result r
    Error  e <*> _ = Error e
    Result f <*> r = fmap f r
    
instance Monad Result where
    Error  e >>= _ = Error e
    Result r >>= k = k r

instance Alternative Result where
    empty                = undefined
    Error e <|> Error e' = Error (e L.++ e')
    Error _ <|> n        = n
    m       <|> _        = m

instance MonadPlus Result

--------------------------------------------------------------------------------

satisfy :: (SyntaxToken -> Bool) -> String -> Parser SyntaxToken
satisfy test hint = StateT $ \(t:ts) -> if test t then Result (t, ts) else Error [(t, hint)]

identifierToken :: Parser SyntaxToken
identifierToken = satisfy test "identifier"
    where
        test IdentifierToken {} = True
        test _ = False

numericLiteralToken :: Parser SyntaxToken
numericLiteralToken = satisfy test "numeric literal"
    where
        test NumericLiteralToken {} = True
        test _ = False

keywordToken :: String -> Parser SyntaxToken
keywordToken k = satisfy test (show k)
    where
        test KeywordToken { syntaxToken_value = value } = C.pack k == value
        test _ = False

punctuatorToken :: Char -> Parser SyntaxToken
punctuatorToken p = satisfy test (show p)
    where
        test PunctuatorToken { syntaxToken_value = value } = C.singleton p == value
        test _ = False

endOfFileToken :: Parser SyntaxToken
endOfFileToken = satisfy test "end of file"
    where
        test EndOfFileToken {} = True
        test _ = False

--------------------------------------------------------------------------------

simpleNameSyntax :: Parser SimpleNameSyntax
simpleNameSyntax = SimpleNameSyntax <$> identifierToken

moduleNameSyntax :: Parser ModuleNameSyntax
moduleNameSyntax = ModuleNameSyntax <$> sepBy1 identifierToken (punctuatorToken '.')

--------------------------------------------------------------------------------

literalSyntax :: Parser LiteralSyntax
literalSyntax = NumericLiteralSyntax <$> optional (punctuatorToken '-') <*> numericLiteralToken

--------------------------------------------------------------------------------

typeSyntax :: Parser TypeSyntax
typeSyntax =
    flip (L.foldl (flip (.)) id) <$> (predefinedTypeSyntax <|> declaredTypeSyntax) <*> many (referenceTypeSyntax <|> arrayOrSpanTypeSyntax <|> functionTypeSyntax)
    where
        predefinedTypeSyntax  =
            BoolTypeSyntax    <$> keywordToken "bool" <|>
            Int8TypeSyntax    <$> keywordToken "i8"   <|>
            Int16TypeSyntax   <$> keywordToken "i16"  <|>
            Int32TypeSyntax   <$> keywordToken "i32"  <|>
            Int64TypeSyntax   <$> keywordToken "i64"  <|>
            UInt8TypeSyntax   <$> keywordToken "u8"   <|>
            UInt16TypeSyntax  <$> keywordToken "u16"  <|>
            UInt32TypeSyntax  <$> keywordToken "u32"  <|>
            UInt64TypeSyntax  <$> keywordToken "u64"  <|>
            Float32TypeSyntax <$> keywordToken "f32"  <|>
            Float64TypeSyntax <$> keywordToken "f64"  <|>
            VoidTypeSyntax    <$> keywordToken "void"
        declaredTypeSyntax    = DeclaredTypeSyntax <$> optional (between (punctuatorToken '<') (punctuatorToken '>') moduleNameSyntax) <*> simpleNameSyntax
        referenceTypeSyntax   = ReferenceTypeSyntax <$ punctuatorToken '&'
        functionTypeSyntax    = flip FunctionTypeSyntax <$> between (punctuatorToken '(') (punctuatorToken ')') (sepBy typeSyntax (punctuatorToken ','))
        arrayOrSpanTypeSyntax = between (punctuatorToken '[') (punctuatorToken ']') (option SpanTypeSyntax (flip ArrayTypeSyntax <$> literalSyntax))

--------------------------------------------------------------------------------

compilationUnitSyntax :: Parser CompilationUnitSyntax
compilationUnitSyntax = do
    modules                 <- manyTill moduleDeclarationSyntax endOfFileToken
    return (CompilationUnitSyntax modules)

moduleDeclarationSyntax :: Parser ModuleDeclarationSyntax
moduleDeclarationSyntax = do
    moduleKeyword           <- keywordToken "module"
    moduleName              <- moduleNameSyntax
    _                       <- punctuatorToken '{'
    importDirectives        <- many importDirectiveSyntax
    moduleMembers           <- manyTill  moduleMemberSyntax (punctuatorToken '}')
    return (ModuleDeclarationSyntax moduleKeyword moduleName importDirectives moduleMembers)

importDirectiveSyntax :: Parser ImportDirectiveSyntax
importDirectiveSyntax = do
    importKeyword           <- keywordToken "import"
    moduleName              <- moduleNameSyntax
    return (ImportDirectiveSyntax importKeyword moduleName)

moduleMemberSyntax :: Parser ModuleMemberSyntax
moduleMemberSyntax = typeDeclarationSyntax <|> functionDeclarationSyntax

typeDeclarationSyntax :: Parser ModuleMemberSyntax
typeDeclarationSyntax = do
    typeKeyword             <- keywordToken "type"
    typeName                <- simpleNameSyntax
    _                       <- punctuatorToken '{'
    fieldDeclarations       <- manyTill fieldDeclarationSyntax (punctuatorToken '}')
    return (TypeDeclarationSyntax typeKeyword typeName fieldDeclarations)

fieldDeclarationSyntax :: Parser FieldDeclarationSyntax
fieldDeclarationSyntax = do
    fieldKeyword            <- keywordToken "field"
    fieldType               <- typeSyntax
    fieldName               <- simpleNameSyntax
    return (FieldDeclarationSyntax fieldKeyword fieldType fieldName)

functionDeclarationSyntax :: Parser ModuleMemberSyntax
functionDeclarationSyntax =
    functionDeclaration <|> functionDefinition

functionDeclaration :: Parser ModuleMemberSyntax
functionDeclaration = do
    functionKeyword         <- keywordToken "declare"
    returnType              <- typeSyntax
    functionName            <- simpleNameSyntax
    parameterDeclarations   <- between (punctuatorToken '(') (punctuatorToken ')') (sepBy parameterDeclarationSyntax (punctuatorToken ','))
    systemCallOrdinal       <- literalSyntax
    return (FunctionDeclarationSyntax functionKeyword returnType functionName parameterDeclarations (Right systemCallOrdinal))

functionDefinition :: Parser ModuleMemberSyntax
functionDefinition = do
    functionKeyword         <- keywordToken "define"
    returnType              <- typeSyntax
    functionName            <- simpleNameSyntax
    parameterDeclarations   <- between (punctuatorToken '(') (punctuatorToken ')') (sepBy parameterDeclarationSyntax (punctuatorToken ','))
    functionBody            <- functionBodySyntax
    return (FunctionDeclarationSyntax functionKeyword returnType functionName parameterDeclarations (Left functionBody))

parameterDeclarationSyntax :: Parser ParameterDeclarationSyntax
parameterDeclarationSyntax = do
    parameterType           <- typeSyntax
    parameterName           <- simpleNameSyntax
    return (ParameterDeclarationSyntax parameterType parameterName)

functionBodySyntax :: Parser FunctionBodySyntax
functionBodySyntax = do
    _                       <- punctuatorToken '{'
    localDeclarations       <- many localDeclarationSyntax
    instructions            <- manyTill (instructionSyntax <|> labeledInstructionSyntax) (punctuatorToken '}')
    return (FunctionBodySyntax localDeclarations (V.fromList instructions))

localDeclarationSyntax :: Parser LocalDeclarationSyntax
localDeclarationSyntax = do
    localKeyword            <- keywordToken "local"
    localType               <- typeSyntax
    localName               <- simpleNameSyntax
    return (LocalDeclarationSyntax localKeyword localType localName)

labeledInstructionSyntax :: Parser InstructionSyntax
labeledInstructionSyntax = do
    name                    <- simpleNameSyntax
    _                       <- punctuatorToken ':'
    instruction             <- instructionSyntax
    return (LabeledInstructionSyntax name instruction)

instructionSyntax :: Parser InstructionSyntax
instructionSyntax =
    noneInstruction     AddInstructionSyntax        "add"       <|>
    noneInstruction     AndInstructionSyntax        "and"       <|>
    nameInstruction     BeqSInstructionSyntax       "beq.s"     <|>
    nameInstruction     BeqInstructionSyntax        "beq"       <|>
    nameInstruction     BgeUnSInstructionSyntax     "bge.un.s"  <|>
    nameInstruction     BgeUnInstructionSyntax      "bge.un"    <|>
    nameInstruction     BgeSInstructionSyntax       "bge.s"     <|>
    nameInstruction     BgeInstructionSyntax        "bge"       <|>
    nameInstruction     BgtUnSInstructionSyntax     "bgt.un.s"  <|>
    nameInstruction     BgtUnInstructionSyntax      "bgt.un"    <|>
    nameInstruction     BgtSInstructionSyntax       "bgt.s"     <|>
    nameInstruction     BgtInstructionSyntax        "bgt"       <|>
    nameInstruction     BleUnSInstructionSyntax     "ble.un.s"  <|>
    nameInstruction     BleUnInstructionSyntax      "ble.un"    <|>
    nameInstruction     BleSInstructionSyntax       "ble.s"     <|>
    nameInstruction     BleInstructionSyntax        "ble"       <|>
    nameInstruction     BltUnSInstructionSyntax     "blt.un.s"  <|>
    nameInstruction     BltUnInstructionSyntax      "blt.un"    <|>
    nameInstruction     BltSInstructionSyntax       "blt.s"     <|>
    nameInstruction     BltInstructionSyntax        "blt"       <|>
    nameInstruction     BneUnSInstructionSyntax     "bne.un.s"  <|>
    nameInstruction     BneUnInstructionSyntax      "bne.un"    <|>
    noneInstruction     BreakInstructionSyntax      "break"     <|>
    nameInstruction     BrfalseSInstructionSyntax   "brfalse.s" <|>
    nameInstruction     BrfalseInstructionSyntax    "brfalse"   <|>
    nameInstruction     BrSInstructionSyntax        "br.s"      <|>
    nameInstruction     BrInstructionSyntax         "br"        <|>
    nameInstruction     BrtrueSInstructionSyntax    "brtrue.s"  <|>
    nameInstruction     BrtrueInstructionSyntax     "brtrue"    <|>
    typeInstruction     CalliInstructionSyntax      "calli"     <|>
    functionInstruction CallInstructionSyntax       "call"      <|>
    noneInstruction     CeqInstructionSyntax        "ceq"       <|>
    noneInstruction     CgtUnInstructionSyntax      "cgt.un"    <|>
    noneInstruction     CgtInstructionSyntax        "cgt"       <|>
    noneInstruction     CltUnInstructionSyntax      "clt.un"    <|>
    noneInstruction     CltInstructionSyntax        "clt"       <|>
    typeInstruction     ConvUnInstructionSyntax     "conv.un"   <|>
    typeInstruction     ConvInstructionSyntax       "conv"      <|>
    noneInstruction     DivUnInstructionSyntax      "div.un"    <|>
    noneInstruction     DivInstructionSyntax        "div"       <|>
    noneInstruction     DupInstructionSyntax        "dup"       <|>
    nameInstruction     LdargaInstructionSyntax     "ldarga"    <|>
    nameInstruction     LdargInstructionSyntax      "ldarg"     <|>
    constantInstruction LdcInstructionSyntax        "ldc"       <|>
    typeInstruction     LdelemaInstructionSyntax    "ldelema"   <|>
    typeInstruction     LdelemInstructionSyntax     "ldelem"    <|>
    fieldInstruction    LdfldaInstructionSyntax     "ldflda"    <|>
    fieldInstruction    LdfldInstructionSyntax      "ldfld"     <|>
    functionInstruction LdftnInstructionSyntax      "ldftn"     <|>
    typeInstruction     LdindInstructionSyntax      "ldind"     <|>
    noneInstruction     LdlenInstructionSyntax      "ldlen"     <|>
    nameInstruction     LdlocaInstructionSyntax     "ldloca"    <|>
    nameInstruction     LdlocInstructionSyntax      "ldloc"     <|>
    noneInstruction     LdnullInstructionSyntax     "ldnull"    <|>
    noneInstruction     MulInstructionSyntax        "mul"       <|>
    noneInstruction     NegInstructionSyntax        "neg"       <|>
    typeInstruction     NewarrInstructionSyntax     "newarr"    <|>
    functionInstruction NewobjInstructionSyntax     "newobj"    <|>
    noneInstruction     NopInstructionSyntax        "nop"       <|>
    noneInstruction     NotInstructionSyntax        "not"       <|>
    noneInstruction     OrInstructionSyntax         "or"        <|>
    noneInstruction     PopInstructionSyntax        "pop"       <|>
    noneInstruction     RemUnInstructionSyntax      "rem.un"    <|>
    noneInstruction     RemInstructionSyntax        "rem"       <|>
    noneInstruction     RetInstructionSyntax        "ret"       <|>
    noneInstruction     ShlInstructionSyntax        "shl"       <|>
    noneInstruction     ShrUnInstructionSyntax      "shr.un"    <|>
    noneInstruction     ShrInstructionSyntax        "shr"       <|>
    nameInstruction     StargInstructionSyntax      "starg"     <|>
    typeInstruction     StelemInstructionSyntax     "stelem"    <|>
    fieldInstruction    StfldInstructionSyntax      "stfld"     <|>
    typeInstruction     StindInstructionSyntax      "stind"     <|>
    nameInstruction     StlocInstructionSyntax      "stloc"     <|>
    noneInstruction     SubInstructionSyntax        "sub"       <|>
    functionInstruction SyscallInstructionSyntax    "syscall"   <|>
    noneInstruction     XorInstructionSyntax        "xor"
    where
        constantInstruction constructor keyword = do
            keyword'        <- keywordToken keyword
            constantType    <- typeSyntax
            constantValue   <- literalSyntax
            return (constructor keyword' constantType constantValue)

        fieldInstruction constructor keyword = do
            keyword'        <- keywordToken keyword
            fieldType       <- typeSyntax
            moduleName      <- optional (between (punctuatorToken '<') (punctuatorToken '>') moduleNameSyntax)
            typeName        <- simpleNameSyntax
            _               <- punctuatorToken '/'
            fieldName       <- simpleNameSyntax
            return (constructor keyword' fieldType moduleName typeName fieldName)

        functionInstruction constructor keyword = do
            keyword'        <- keywordToken keyword
            returnType      <- typeSyntax
            moduleName      <- optional (between (punctuatorToken '<') (punctuatorToken '>') moduleNameSyntax)
            functionName    <- simpleNameSyntax
            parameterTypes  <- between (punctuatorToken '(') (punctuatorToken ')') (sepBy typeSyntax (punctuatorToken ','))
            return (constructor keyword' returnType moduleName functionName parameterTypes)

        nameInstruction constructor keyword = do
            keyword'        <- keywordToken keyword
            name            <- simpleNameSyntax
            return (constructor keyword' name)

        noneInstruction constructor keyword = do
            keyword'        <- keywordToken keyword
            return (constructor keyword')

        typeInstruction constructor keyword = do
            keyword'        <- keywordToken keyword
            instructionType <- typeSyntax
            return (constructor keyword' instructionType)

--------------------------------------------------------------------------------

parseText :: (Monad m) => FilePath -> Text -> CompilerT Diagnostic m SyntaxTree
parseText path text =
    parseBytes path (encodeUtf8 text)

parseBytes :: (Monad m) => FilePath -> ByteString -> CompilerT Diagnostic m SyntaxTree
parseBytes path text =
    case runStateT compilationUnitSyntax (tokenizeBytes path text) of
        Error errors -> do
            let unexpected = L.maximumBy (comparing syntaxToken_location) (L.map fst errors)
            let expecting = L.map snd (L.filter ((==) unexpected . fst) errors)
            report (SyntaxError (syntaxToken_location unexpected) unexpected expecting)
            stop
        Result (root, _) -> do
            return (SyntaxTree root path text)

parseFile :: (MonadIO m) => FilePath -> CompilerT Diagnostic m SyntaxTree
parseFile path =
    liftIO (readFile path) >>= parseBytes path

parseFiles :: (MonadIO m) => [FilePath] -> CompilerT Diagnostic m [SyntaxTree]
parseFiles paths =
    sequentialC (L.map parseFile paths)

--------------------------------------------------------------------------------
