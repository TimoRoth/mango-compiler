{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Mango.Compiler.Syntax (
    -- Syntax Tokens
    Location (..),
    SyntaxTrivia (..),
    SyntaxToken (..),
    -- Syntax Nodes
    SimpleNameSyntax (..),
    ModuleNameSyntax (..),
    LiteralSyntax (..),
    TypeSyntax (..),
    CompilationUnitSyntax (..),
    ModuleDeclarationSyntax (..),
    ImportDirectiveSyntax (..),
    ModuleMemberSyntax (..),
    FieldDeclarationSyntax (..),
    ParameterDeclarationSyntax (..),
    FunctionBodySyntax (..),
    LocalDeclarationSyntax (..),
    InstructionSyntax (..),
    NameSyntax (..),
    SyntaxNode (..),
    -- Compilation
    Compilation (..),
    SyntaxTree (..),
    ) where

import Data.Bool
import Data.ByteString
import Data.Either (Either)
import Data.Eq
import Data.Function
import Data.Int
import Data.Maybe (Maybe (..), maybe)
import Data.Ord
import Data.String (String)
import Data.Vector (Vector)
import System.IO (FilePath)
import Text.Show

import qualified Data.ByteString.Char8 as C
import qualified Data.List as L

--------------------------------------------------------------------------------

data Location
    = Location { location_path :: !FilePath, location_line :: {-# UNPACK #-} !Int, location_column :: {-# UNPACK #-} !Int, location_sourceText :: !ByteString }

data SyntaxTrivia
    = EndOfLineTrivia           { syntaxTrivia_value :: {-# UNPACK #-} !ByteString }
    | WhitespaceTrivia          { syntaxTrivia_value :: {-# UNPACK #-} !ByteString }
    | SingleLineCommentTrivia   { syntaxTrivia_value :: {-# UNPACK #-} !ByteString }
    | MultiLineCommentTrivia    { syntaxTrivia_value :: {-# UNPACK #-} !ByteString }
    | ErrorTrivia               { syntaxTrivia_value :: {-# UNPACK #-} !ByteString }

data SyntaxToken
    = IdentifierToken           { syntaxToken_leadingTrivia :: [SyntaxTrivia], syntaxToken_value :: {-# UNPACK #-} !ByteString, syntaxToken_trailingTrivia :: [SyntaxTrivia], syntaxToken_location :: !Location }
    | NumericLiteralToken       { syntaxToken_leadingTrivia :: [SyntaxTrivia], syntaxToken_value :: {-# UNPACK #-} !ByteString, syntaxToken_trailingTrivia :: [SyntaxTrivia], syntaxToken_location :: !Location }
    | KeywordToken              { syntaxToken_leadingTrivia :: [SyntaxTrivia], syntaxToken_value :: {-# UNPACK #-} !ByteString, syntaxToken_trailingTrivia :: [SyntaxTrivia], syntaxToken_location :: !Location }
    | PunctuatorToken           { syntaxToken_leadingTrivia :: [SyntaxTrivia], syntaxToken_value :: {-# UNPACK #-} !ByteString, syntaxToken_trailingTrivia :: [SyntaxTrivia], syntaxToken_location :: !Location }
    | EndOfFileToken            { syntaxToken_leadingTrivia :: [SyntaxTrivia], syntaxToken_value :: {-# UNPACK #-} !ByteString, syntaxToken_trailingTrivia :: [SyntaxTrivia], syntaxToken_location :: !Location }
    | BadToken                  { syntaxToken_leadingTrivia :: [SyntaxTrivia], syntaxToken_value :: {-# UNPACK #-} !ByteString, syntaxToken_trailingTrivia :: [SyntaxTrivia], syntaxToken_location :: !Location }

instance Show Location where
    show (Location path line column _) = L.concat [path, ":", show line, ":", show column]

instance Eq Location where
    (Location path line column _) == (Location path' line' column' _) = path == path' && line == line' && column == column'

instance Ord Location where
    compare (Location path line column _) (Location path' line' column' _) = compare (path, line, column) (path', line', column')

instance Show SyntaxTrivia where
    show trivia = show (C.unpack (syntaxTrivia_value trivia))

instance Show SyntaxToken where
    show EndOfFileToken {} = "end of file"
    show token = show (C.unpack (syntaxToken_value token))

instance Eq SyntaxToken where
    token == token' = (syntaxToken_location token) == (syntaxToken_location token')

instance Ord SyntaxToken where
    compare token token' = compare (syntaxToken_location token) (syntaxToken_location token')

--------------------------------------------------------------------------------

data SimpleNameSyntax
    = SimpleNameSyntax { simpleNameSyntax_token :: !SyntaxToken }

data ModuleNameSyntax
    = ModuleNameSyntax { moduleNameSyntax_tokens :: ![SyntaxToken] }

data LiteralSyntax
    = NumericLiteralSyntax { numericLiteralSyntax_minus :: !(Maybe SyntaxToken), numericLiteralSyntax_token :: !SyntaxToken }

data TypeSyntax
    = BoolTypeSyntax { predefinedTypeSyntax_keyword :: !SyntaxToken }
    | Int8TypeSyntax { predefinedTypeSyntax_keyword :: !SyntaxToken }
    | Int16TypeSyntax { predefinedTypeSyntax_keyword :: !SyntaxToken }
    | Int32TypeSyntax { predefinedTypeSyntax_keyword :: !SyntaxToken }
    | Int64TypeSyntax { predefinedTypeSyntax_keyword :: !SyntaxToken }
    | UInt8TypeSyntax { predefinedTypeSyntax_keyword :: !SyntaxToken }
    | UInt16TypeSyntax { predefinedTypeSyntax_keyword :: !SyntaxToken }
    | UInt32TypeSyntax { predefinedTypeSyntax_keyword :: !SyntaxToken }
    | UInt64TypeSyntax { predefinedTypeSyntax_keyword :: !SyntaxToken }
    | Float32TypeSyntax { predefinedTypeSyntax_keyword :: !SyntaxToken }
    | Float64TypeSyntax { predefinedTypeSyntax_keyword :: !SyntaxToken }
    | VoidTypeSyntax { predefinedTypeSyntax_keyword :: !SyntaxToken }
    | ArrayTypeSyntax { arrayTypeSyntax_elementType :: TypeSyntax, arrayTypeSyntax_length :: LiteralSyntax }
    | DeclaredTypeSyntax { declaredTypeSyntax_moduleName :: Maybe ModuleNameSyntax, declaredTypeSyntax_typeName :: SimpleNameSyntax }
    | FunctionTypeSyntax { functionTypeSyntax_returnType :: TypeSyntax, functionTypeSyntax_parameterTypes :: [TypeSyntax] }
    | ReferenceTypeSyntax { referenceTypeSyntax_referencedType :: TypeSyntax }
    | SpanTypeSyntax { spanTypeSyntax_elementType :: TypeSyntax }

data CompilationUnitSyntax
    = CompilationUnitSyntax { compilationUnitSyntax_modules :: [ModuleDeclarationSyntax] }

data ModuleDeclarationSyntax
    = ModuleDeclarationSyntax { moduleDeclarationSyntax_keyword :: !SyntaxToken, moduleDeclarationSyntax_name :: ModuleNameSyntax, moduleDeclarationSyntax_imports :: [ImportDirectiveSyntax], moduleDeclarationSyntax_members :: [ModuleMemberSyntax] }

data ImportDirectiveSyntax
    = ImportDirectiveSyntax { importDirectiveSyntax_keyword :: !SyntaxToken, importDirectiveSyntax_name :: ModuleNameSyntax }

data ModuleMemberSyntax
    = TypeDeclarationSyntax { typeDeclarationSyntax_keyword :: !SyntaxToken, typeDeclarationSyntax_name :: SimpleNameSyntax, typeDeclarationSyntax_fields :: [FieldDeclarationSyntax] }
    | FunctionDeclarationSyntax { functionDeclarationSyntax_keyword :: !SyntaxToken, functionDeclarationSyntax_returnType :: TypeSyntax, functionDeclarationSyntax_name :: SimpleNameSyntax, functionDeclarationSyntax_parameters :: [ParameterDeclarationSyntax], functionDeclarationSyntax_body :: Either FunctionBodySyntax LiteralSyntax }

data FieldDeclarationSyntax
    = FieldDeclarationSyntax { fieldDeclarationSyntax_keyword :: !SyntaxToken, fieldDeclarationSyntax_type :: TypeSyntax, fieldDeclarationSyntax_name :: SimpleNameSyntax }

data ParameterDeclarationSyntax
    = ParameterDeclarationSyntax { parameterDeclarationSyntax_type :: TypeSyntax, parameterDeclarationSyntax_name :: SimpleNameSyntax }

data FunctionBodySyntax
    = FunctionBodySyntax { functionBodySyntax_locals :: [LocalDeclarationSyntax], functionBodySyntax_instructions :: Vector InstructionSyntax }

data LocalDeclarationSyntax
    = LocalDeclarationSyntax { localDeclarationSyntax_keyword :: !SyntaxToken, localDeclarationSyntax_type :: TypeSyntax, localDeclarationSyntax_name :: SimpleNameSyntax }

data InstructionSyntax
    = LabeledInstructionSyntax { labeledInstructionSyntax_label :: SimpleNameSyntax, labeledInstructionSyntax_instruction :: InstructionSyntax }

    | NopInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken }
    | BreakInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken }

    | PopInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken }
    | DupInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken }

    | NewobjInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, functionInstructionSyntax_returnType :: TypeSyntax, functionInstructionSyntax_moduleName :: Maybe ModuleNameSyntax, functionInstructionSyntax_functionName :: SimpleNameSyntax, functionInstructionSyntax_parameterTypes :: [TypeSyntax] }
    | NewarrInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, typedInstructionSyntax_type :: TypeSyntax }
    | LdlenInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken }

    | CallInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, functionInstructionSyntax_returnType :: TypeSyntax, functionInstructionSyntax_moduleName :: Maybe ModuleNameSyntax, functionInstructionSyntax_functionName :: SimpleNameSyntax, functionInstructionSyntax_parameterTypes :: [TypeSyntax] }
    | CalliInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, typedInstructionSyntax_type :: TypeSyntax }
    | SyscallInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, functionInstructionSyntax_returnType :: TypeSyntax, functionInstructionSyntax_moduleName :: Maybe ModuleNameSyntax, functionInstructionSyntax_functionName :: SimpleNameSyntax, functionInstructionSyntax_parameterTypes :: [TypeSyntax] }
    | RetInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken }

    | BrInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, branchInstructionSyntax_target :: SimpleNameSyntax }
    | BrfalseInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, branchInstructionSyntax_target :: SimpleNameSyntax }
    | BrtrueInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, branchInstructionSyntax_target :: SimpleNameSyntax }

    | BeqInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, branchInstructionSyntax_target :: SimpleNameSyntax }
    | BgeInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, branchInstructionSyntax_target :: SimpleNameSyntax }
    | BgeUnInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, branchInstructionSyntax_target :: SimpleNameSyntax }
    | BgtInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, branchInstructionSyntax_target :: SimpleNameSyntax }
    | BgtUnInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, branchInstructionSyntax_target :: SimpleNameSyntax }
    | BleInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, branchInstructionSyntax_target :: SimpleNameSyntax }
    | BleUnInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, branchInstructionSyntax_target :: SimpleNameSyntax }
    | BltInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, branchInstructionSyntax_target :: SimpleNameSyntax }
    | BltUnSInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, branchInstructionSyntax_target :: SimpleNameSyntax }
    | BneUnInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, branchInstructionSyntax_target :: SimpleNameSyntax }

    | BrSInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, branchInstructionSyntax_target :: SimpleNameSyntax }
    | BrfalseSInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, branchInstructionSyntax_target :: SimpleNameSyntax }
    | BrtrueSInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, branchInstructionSyntax_target :: SimpleNameSyntax }

    | BeqSInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, branchInstructionSyntax_target :: SimpleNameSyntax }
    | BgeSInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, branchInstructionSyntax_target :: SimpleNameSyntax }
    | BgeUnSInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, branchInstructionSyntax_target :: SimpleNameSyntax }
    | BgtSInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, branchInstructionSyntax_target :: SimpleNameSyntax }
    | BgtUnSInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, branchInstructionSyntax_target :: SimpleNameSyntax }
    | BleSInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, branchInstructionSyntax_target :: SimpleNameSyntax }
    | BleUnSInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, branchInstructionSyntax_target :: SimpleNameSyntax }
    | BltSInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, branchInstructionSyntax_target :: SimpleNameSyntax }
    | BltUnInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, branchInstructionSyntax_target :: SimpleNameSyntax }
    | BneUnSInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, branchInstructionSyntax_target :: SimpleNameSyntax }

    | LdcInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, typedInstructionSyntax_type :: TypeSyntax, constantInstructionSyntax_constantValue :: LiteralSyntax }
    | LdftnInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, functionInstructionSyntax_returnType :: TypeSyntax, functionInstructionSyntax_moduleName :: Maybe ModuleNameSyntax, functionInstructionSyntax_functionName :: SimpleNameSyntax, functionInstructionSyntax_parameterTypes :: [TypeSyntax] }
    | LdnullInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken }

    | LdargInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, argumentInstructionSyntax_parameterName :: SimpleNameSyntax }
    | LdargaInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, argumentInstructionSyntax_parameterName :: SimpleNameSyntax }
    | StargInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, argumentInstructionSyntax_parameterName :: SimpleNameSyntax }

    | LdlocInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, localInstructionSyntax_localName :: SimpleNameSyntax }
    | LdlocaInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, localInstructionSyntax_localName :: SimpleNameSyntax }
    | StlocInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, localInstructionSyntax_localName :: SimpleNameSyntax }

    | LdindInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, typedInstructionSyntax_type :: TypeSyntax }
    | StindInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, typedInstructionSyntax_type :: TypeSyntax }

    | LdfldInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, fieldInstructionSyntax_fieldType :: TypeSyntax, fieldInstructionSyntax_moduleName :: Maybe ModuleNameSyntax, fieldInstructionSyntax_typeName :: SimpleNameSyntax, fieldInstructionSyntax_fieldName :: SimpleNameSyntax }
    | LdfldaInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, fieldInstructionSyntax_fieldType :: TypeSyntax, fieldInstructionSyntax_moduleName :: Maybe ModuleNameSyntax, fieldInstructionSyntax_typeName :: SimpleNameSyntax, fieldInstructionSyntax_fieldName :: SimpleNameSyntax }
    | StfldInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, fieldInstructionSyntax_fieldType :: TypeSyntax, fieldInstructionSyntax_moduleName :: Maybe ModuleNameSyntax, fieldInstructionSyntax_typeName :: SimpleNameSyntax, fieldInstructionSyntax_fieldName :: SimpleNameSyntax }

    | LdelemInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, typedInstructionSyntax_type :: TypeSyntax }
    | LdelemaInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, typedInstructionSyntax_type :: TypeSyntax }
    | StelemInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, typedInstructionSyntax_type :: TypeSyntax }

    | AddInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken }
    | DivInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken }
    | DivUnInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken }
    | MulInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken }
    | RemInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken }
    | RemUnInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken }
    | SubInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken }

    | NegInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken }

    | AndInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken }
    | OrInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken }
    | XorInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken }

    | ShlInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken }
    | ShrInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken }
    | ShrUnInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken }

    | NotInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken }

    | CeqInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken }
    | CgtInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken }
    | CgtUnInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken }
    | CltInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken }
    | CltUnInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken }

    | ConvInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, typedInstructionSyntax_type :: TypeSyntax }
    | ConvUnInstructionSyntax { instructionSyntax_keyword :: !SyntaxToken, typedInstructionSyntax_type :: TypeSyntax }

class SyntaxNode a where
    syntaxLocation :: a -> Location

instance SyntaxNode SimpleNameSyntax where
    syntaxLocation SimpleNameSyntax { simpleNameSyntax_token = token } = syntaxToken_location token

instance SyntaxNode ModuleNameSyntax where
    syntaxLocation ModuleNameSyntax { moduleNameSyntax_tokens = tokens } = syntaxToken_location (L.head tokens)

instance SyntaxNode LiteralSyntax where
    syntaxLocation NumericLiteralSyntax { numericLiteralSyntax_minus = minus, numericLiteralSyntax_token = token } = maybe (syntaxToken_location token) syntaxToken_location minus

instance SyntaxNode TypeSyntax where
    syntaxLocation ArrayTypeSyntax { arrayTypeSyntax_elementType = elementType } = syntaxLocation elementType
    syntaxLocation DeclaredTypeSyntax { declaredTypeSyntax_moduleName = Just moduleName } = syntaxLocation moduleName
    syntaxLocation DeclaredTypeSyntax { declaredTypeSyntax_moduleName = Nothing, declaredTypeSyntax_typeName = typeName } = syntaxLocation typeName
    syntaxLocation FunctionTypeSyntax { functionTypeSyntax_returnType = returnType } = syntaxLocation returnType
    syntaxLocation ReferenceTypeSyntax { referenceTypeSyntax_referencedType = referencedType } = syntaxLocation referencedType
    syntaxLocation SpanTypeSyntax { spanTypeSyntax_elementType = elementType } = syntaxLocation elementType
    syntaxLocation predefinedType = syntaxToken_location (predefinedTypeSyntax_keyword predefinedType)

instance SyntaxNode ModuleDeclarationSyntax where
    syntaxLocation ModuleDeclarationSyntax { moduleDeclarationSyntax_keyword = keyword } = syntaxToken_location keyword

instance SyntaxNode ImportDirectiveSyntax where
    syntaxLocation ImportDirectiveSyntax { importDirectiveSyntax_keyword = keyword } = syntaxToken_location keyword

instance SyntaxNode ModuleMemberSyntax where
    syntaxLocation TypeDeclarationSyntax { typeDeclarationSyntax_keyword = keyword } = syntaxToken_location keyword
    syntaxLocation FunctionDeclarationSyntax { functionDeclarationSyntax_keyword = keyword } = syntaxToken_location keyword

instance SyntaxNode FieldDeclarationSyntax where
    syntaxLocation FieldDeclarationSyntax { fieldDeclarationSyntax_keyword = keyword } = syntaxToken_location keyword

instance SyntaxNode ParameterDeclarationSyntax where
    syntaxLocation ParameterDeclarationSyntax { parameterDeclarationSyntax_name = name } = syntaxLocation name

instance SyntaxNode LocalDeclarationSyntax where
    syntaxLocation LocalDeclarationSyntax { localDeclarationSyntax_name = name } = syntaxLocation name

instance SyntaxNode InstructionSyntax where
    syntaxLocation LabeledInstructionSyntax { labeledInstructionSyntax_label = label } = syntaxLocation label
    syntaxLocation instruction = syntaxToken_location (instructionSyntax_keyword instruction)

class NameSyntax a where
    displayName :: a -> String

instance NameSyntax SimpleNameSyntax where
    displayName SimpleNameSyntax { simpleNameSyntax_token = token } = (C.unpack . syntaxToken_value) token

instance NameSyntax ModuleNameSyntax where
    displayName ModuleNameSyntax { moduleNameSyntax_tokens = tokens } = L.intercalate "." (L.map (C.unpack . syntaxToken_value) tokens)

--------------------------------------------------------------------------------

data Compilation
    = Compilation { compilation_syntaxTrees :: [SyntaxTree] }

data SyntaxTree
    = SyntaxTree { syntaxTree_root :: CompilationUnitSyntax, syntaxTree_path :: !FilePath, syntaxTree_text :: !ByteString }

--------------------------------------------------------------------------------
