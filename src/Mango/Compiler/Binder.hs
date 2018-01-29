{-# LANGUAGE NoImplicitPrelude #-}

module Mango.Compiler.Binder (
    -- Semantic Model
    createSemanticModel,
    -- Declared Symbols
    declaredModule,
    declaredType,
    declaredField,
    declaredFunction,
    declaredParameter,
    declaredLocal,
    declaredLabel,
    -- Instructions
    bindModule,
    bindType,
    bindField,
    bindFunction,
    bindParameter,
    bindLocal,
    bindLabel,
    ) where

import Data.Bits
import Data.Bool
import Data.Either (Either (..))
import Data.Eq
import Data.Function
import Data.Int (Int)
import Data.List
import Data.Maybe (Maybe (..), fromJust, maybe)
import Data.Ord
import Mango.Compiler.Error
import Mango.Compiler.Symbols
import Mango.Compiler.Syntax
import Prelude (Num (..), Integer, fromIntegral, undefined)

import qualified Data.Array as A
import qualified Data.ByteString as B
import qualified Data.Vector as V

--------------------------------------------------------------------------------

createSemanticModel :: Compilation -> SemanticModel
createSemanticModel compilation =
    semanticModel
    where
        semanticModel = SemanticModel modules moduleGraph
        modules       = map (makeModuleSymbol semanticModel) $ zip [0..] $ concatMap (compilationUnitSyntax_modules . syntaxTree_root) (compilation_syntaxTrees compilation)
        adjacencyList = [[moduleSymbol_index import_ | import_@ModuleSymbol {} <- moduleSymbol_imports module_] | module_ <- modules]
        moduleGraph   = A.listArray (0, length adjacencyList - 1) adjacencyList

makeModuleSymbol :: SemanticModel -> (Int, ModuleDeclarationSyntax) -> ModuleSymbol
makeModuleSymbol semanticModel (index, moduleDeclaration) =
    symbol
    where
        symbol        = ModuleSymbol semanticModel name index imports types functions entryPoint (syntaxLocation moduleDeclaration) []
        name          = displayName (moduleDeclarationSyntax_name moduleDeclaration)
        imports       = [bindModule         symbol importDirective     | importDirective <- moduleDeclarationSyntax_imports moduleDeclaration]
        types         = [makeTypeSymbol     symbol typeDeclaration     | typeDeclaration@TypeDeclarationSyntax {} <- moduleDeclarationSyntax_members moduleDeclaration]
        functions     = [makeFunctionSymbol symbol functionDeclaration | functionDeclaration@FunctionDeclarationSyntax {} <- moduleDeclarationSyntax_members moduleDeclaration]
        entryPoint    = findEntryPoint symbol

makeTypeSymbol :: ModuleSymbol -> ModuleMemberSyntax -> TypeSymbol
makeTypeSymbol container typeDeclaration =
    symbol
    where
        symbol        = StructuredTypeSymbol container name fields layout (syntaxLocation typeDeclaration) []
        name          = displayName (typeDeclarationSyntax_name typeDeclaration)
        ((alignment, size), fields) = case typeDeclarationSyntax_fields typeDeclaration of
            [] -> ((1, 1), [])
            fs -> mapAccumL (makeFieldSymbol symbol) (1, 0) fs
        layout        = TypeLayout alignment ((size + (alignment - 1)) .&. (complement (alignment - 1)))

makeFieldSymbol :: TypeSymbol -> (Int, Int) -> FieldDeclarationSyntax -> ((Int, Int), FieldSymbol)
makeFieldSymbol container (alignment, offset) fieldDeclaration =
    ((alignment', offset'), symbol)
    where
        symbol        = FieldSymbol container name fieldType fieldOffset (syntaxLocation fieldDeclaration) []
        name          = displayName (fieldDeclarationSyntax_name fieldDeclaration)
        fieldType     = resolveType (containingModule container) (fieldDeclarationSyntax_type fieldDeclaration)
        fieldOffset   = (offset + (fieldAlignment - 1)) .&. (complement (fieldAlignment - 1))
        alignment'    = max alignment fieldAlignment
        offset'       = fieldOffset + fieldSize
        TypeLayout fieldAlignment fieldSize = typeLayout fieldType

makeFunctionSymbol :: ModuleSymbol -> ModuleMemberSyntax -> FunctionSymbol
makeFunctionSymbol moduleSymbol functionDeclaration@FunctionDeclarationSyntax { functionDeclarationSyntax_body = Left functionBody } =
    symbol
    where
        symbol        = FunctionSymbol moduleSymbol name returnType parameters locals labels Nothing (syntaxLocation functionDeclaration) []
        name          = displayName (functionDeclarationSyntax_name functionDeclaration)
        returnType    = resolveType moduleSymbol (functionDeclarationSyntax_returnType functionDeclaration)
        parameters    = [makeParameterSymbol symbol parameterDeclaration | parameterDeclaration <- functionDeclarationSyntax_parameters functionDeclaration]
        locals        = [makeLocalSymbol     symbol localDeclaration     | localDeclaration <- functionBodySyntax_locals functionBody]
        labels        = concatMap (makeLabelSymbols symbol) (V.indexed (functionBodySyntax_instructions functionBody))
makeFunctionSymbol moduleSymbol functionDeclaration@FunctionDeclarationSyntax { functionDeclarationSyntax_body = Right ordinal } =
    symbol
    where
        symbol        = FunctionSymbol moduleSymbol name returnType parameters [] [] (Just (parseLiteral ordinal)) (syntaxLocation functionDeclaration) []
        name          = displayName (functionDeclarationSyntax_name functionDeclaration)
        returnType    = resolveType moduleSymbol (functionDeclarationSyntax_returnType functionDeclaration)
        parameters    = [makeParameterSymbol symbol parameterDeclaration | parameterDeclaration <- functionDeclarationSyntax_parameters functionDeclaration]
makeFunctionSymbol _ TypeDeclarationSyntax {} =
    undefined

makeParameterSymbol :: FunctionSymbol -> ParameterDeclarationSyntax -> ParameterSymbol
makeParameterSymbol container parameterDeclaration =
    symbol
    where
        symbol        = ParameterSymbol container name parameterType (syntaxLocation parameterDeclaration) []
        name          = displayName (parameterDeclarationSyntax_name parameterDeclaration)
        parameterType = resolveType (containingModule container) (parameterDeclarationSyntax_type parameterDeclaration)

makeLocalSymbol :: FunctionSymbol -> LocalDeclarationSyntax -> LocalSymbol
makeLocalSymbol container localDeclaration =
    symbol
    where
        symbol        = LocalSymbol container name localType (syntaxLocation localDeclaration) []
        name          = displayName (localDeclarationSyntax_name localDeclaration)
        localType     = resolveType (containingModule container) (localDeclarationSyntax_type localDeclaration)

makeLabelSymbols :: FunctionSymbol -> (Int, InstructionSyntax) -> [LabelSymbol]
makeLabelSymbols container (index, instruction@LabeledInstructionSyntax {}) =
    symbol:symbols
    where
        symbol        = LabelSymbol container name index (syntaxLocation instruction) []
        name          = displayName (labeledInstructionSyntax_label instruction)
        symbols       = makeLabelSymbols container (index, labeledInstructionSyntax_instruction instruction)
makeLabelSymbols _ _ =
    []

--------------------------------------------------------------------------------

declaredModule :: SemanticModel -> ModuleDeclarationSyntax -> ModuleSymbol
declaredModule SemanticModel { semanticModel_modules = modules } moduleDeclaration =
    fromJust (find match modules)
    where
        match symbol = syntaxLocation moduleDeclaration == symbolLocation symbol

declaredType :: ModuleSymbol -> ModuleMemberSyntax -> TypeSymbol
declaredType ModuleSymbol { moduleSymbol_types = types } typeDeclaration@TypeDeclarationSyntax {} =
    fromJust (find match types)
    where
        match symbol = syntaxLocation typeDeclaration == symbolLocation symbol
declaredType _ _ =
    undefined

declaredField :: TypeSymbol -> FieldDeclarationSyntax -> FieldSymbol
declaredField StructuredTypeSymbol { structuredTypeSymbol_fields = fields } fieldDeclaration =
    fromJust (find match fields)
    where
        match symbol = syntaxLocation fieldDeclaration == symbolLocation symbol
declaredField  _ _ =
    undefined

declaredFunction :: ModuleSymbol -> ModuleMemberSyntax -> FunctionSymbol
declaredFunction ModuleSymbol { moduleSymbol_functions = functions } functionDeclaration@FunctionDeclarationSyntax {} =
    fromJust (find match functions)
    where
        match symbol = syntaxLocation functionDeclaration == symbolLocation symbol
declaredFunction _ _ =
    undefined

declaredParameter :: FunctionSymbol -> ParameterDeclarationSyntax -> ParameterSymbol
declaredParameter FunctionSymbol { functionSymbol_parameters = parameters } parameterDeclaration =
    fromJust (find match parameters)
    where
        match symbol = syntaxLocation parameterDeclaration == symbolLocation symbol
declaredParameter _ _ =
    undefined

declaredLocal :: FunctionSymbol -> LocalDeclarationSyntax -> LocalSymbol
declaredLocal FunctionSymbol { functionSymbol_locals = locals } localDeclarationSyntax =
    fromJust (find match locals)
    where
        match symbol = syntaxLocation localDeclarationSyntax == symbolLocation symbol
declaredLocal _ _ =
    undefined

declaredLabel :: FunctionSymbol -> InstructionSyntax -> LabelSymbol
declaredLabel FunctionSymbol { functionSymbol_labels = labels } labeledInstruction@LabeledInstructionSyntax {} =
    fromJust (find match labels)
    where
        match symbol = syntaxLocation labeledInstruction == symbolLocation symbol
declaredLabel _ _ =
    undefined

--------------------------------------------------------------------------------

bindModule :: (Symbol a) => a -> ImportDirectiveSyntax -> ModuleSymbol
bindModule context importDirective =
    findModule (containingModel context) (importDirectiveSyntax_name importDirective)

bindType :: (Symbol a) => a -> InstructionSyntax -> TypeSymbol
bindType context instruction =
    resolveType context (typedInstructionSyntax_type instruction)

bindField :: (Symbol a) => a -> InstructionSyntax -> FieldSymbol
bindField context instruction = do
    findField containingType_ (fieldInstructionSyntax_fieldName instruction) fieldType
    where
        containingModule_   = case fieldInstructionSyntax_moduleName instruction of
            Nothing         -> containingModule context
            Just moduleName -> findModule (containingModel context) moduleName
        containingType_     = findType containingModule_ (fieldInstructionSyntax_typeName instruction)
        fieldType           = resolveType context (fieldInstructionSyntax_fieldType instruction)

bindFunction :: (Symbol a) => a -> InstructionSyntax -> FunctionSymbol
bindFunction context instruction =
    findFunction containingModule_ (functionInstructionSyntax_functionName instruction) returnType parameterTypes
    where
        containingModule_   = case functionInstructionSyntax_moduleName instruction of
            Nothing         -> containingModule context
            Just moduleName -> findModule (containingModel context) moduleName
        returnType          = resolveType context (functionInstructionSyntax_returnType instruction)
        parameterTypes      = map (resolveType context) (functionInstructionSyntax_parameterTypes instruction)

bindParameter :: (Symbol a) => a -> InstructionSyntax -> ParameterSymbol
bindParameter context instruction =
    findParameter (containingFunction context) (argumentInstructionSyntax_parameterName instruction)

bindLocal :: (Symbol a) => a -> InstructionSyntax -> LocalSymbol
bindLocal context instruction =
    findLocal (containingFunction context) (localInstructionSyntax_localName instruction)

bindLabel :: (Symbol a) => a -> InstructionSyntax -> LabelSymbol
bindLabel context instruction =
    findLabel (containingFunction context) (branchInstructionSyntax_target instruction)

--------------------------------------------------------------------------------

resolveType :: (Symbol a) => a -> TypeSyntax -> TypeSymbol
resolveType context =
    go
    where
        go (BoolTypeSyntax _)                              = BoolTypeSymbol
        go (Int8TypeSyntax _)                              = Int8TypeSymbol
        go (Int16TypeSyntax _)                             = Int16TypeSymbol
        go (Int32TypeSyntax _)                             = Int32TypeSymbol
        go (Int64TypeSyntax _)                             = Int64TypeSymbol
        go (UInt8TypeSyntax _)                             = UInt8TypeSymbol
        go (UInt16TypeSyntax _)                            = UInt16TypeSymbol
        go (UInt32TypeSyntax _)                            = UInt32TypeSymbol
        go (UInt64TypeSyntax _)                            = UInt64TypeSymbol
        go (Float32TypeSyntax _)                           = Float32TypeSymbol
        go (Float64TypeSyntax _)                           = Float64TypeSymbol
        go (VoidTypeSyntax _)                              = VoidTypeSymbol
        go (ArrayTypeSyntax elementType length_)           = ArrayTypeSymbol (go elementType) (parseLiteral length_)
        go (DeclaredTypeSyntax Nothing typeName)           = findType (containingModule context) typeName
        go (DeclaredTypeSyntax (Just moduleName) typeName) = findType (findModule (containingModel context) moduleName) typeName
        go (FunctionTypeSyntax returnType parameterTypes)  = FunctionTypeSymbol (go returnType) (map go parameterTypes)
        go (ReferenceTypeSyntax referencedType)            = ReferenceTypeSymbol (go referencedType)
        go (SpanTypeSyntax elementType)                    = SpanTypeSymbol (go elementType)

parseLiteral :: LiteralSyntax -> Integer
parseLiteral NumericLiteralSyntax { numericLiteralSyntax_minus = minus, numericLiteralSyntax_token = token } =
    maybe id (const negate) minus $ B.foldl' (\a w -> a * 10 + fromIntegral (w - 48)) 0 (syntaxToken_value token)

--------------------------------------------------------------------------------

findModule :: SemanticModel -> ModuleNameSyntax -> ModuleSymbol
findModule semanticModel moduleName =
    case filter match (semanticModel_modules semanticModel) of
        [symbol] -> symbol
        symbols  -> ErrorModuleSymbol semanticModel (displayName moduleName) [] [] [] Nothing (syntaxLocation moduleName) [bindError moduleName symbols]
    where
        match ModuleSymbol { moduleSymbol_name = moduleName' } = moduleName' == displayName moduleName
        match _ = False

findType :: ModuleSymbol -> SimpleNameSyntax -> TypeSymbol
findType container typeName =
    case filter match (moduleSymbol_types container) of
        [symbol] -> symbol
        symbols  -> ErrorTypeSymbol container (displayName typeName) [] (TypeLayout 1 0) (syntaxLocation typeName) [bindError typeName symbols]
    where
        match StructuredTypeSymbol { structuredTypeSymbol_name = typeName' } = typeName' == displayName typeName
        match _ = False

findField :: TypeSymbol -> SimpleNameSyntax -> TypeSymbol -> FieldSymbol
findField container fieldName fieldType =
    case filter match (structuredTypeSymbol_fields container) of
        [symbol] -> symbol
        symbols  -> ErrorFieldSymbol container (displayName fieldName) fieldType 0 (syntaxLocation fieldName) [bindError fieldName symbols]
    where
        match FieldSymbol { fieldSymbol_name = fieldName' } = fieldName' == displayName fieldName
        match _ = False

findFunction :: ModuleSymbol -> SimpleNameSyntax -> TypeSymbol -> [TypeSymbol] -> FunctionSymbol
findFunction container functionName returnType parameterTypes =
    case filter match (moduleSymbol_functions container) of
        [symbol] -> symbol
        symbols  -> let symbol = ErrorFunctionSymbol container (displayName functionName) returnType (map (\t -> ParameterSymbol symbol "?" t (syntaxLocation functionName) []) parameterTypes) [] [] Nothing (syntaxLocation functionName) [bindError functionName symbols] in symbol
    where
        match FunctionSymbol { functionSymbol_name = functionName', functionSymbol_returnType = returnType', functionSymbol_parameters = parameters' } = functionName' == displayName functionName && returnType' == returnType && map parameterSymbol_parameterType parameters' == parameterTypes
        match _ = False

findParameter :: FunctionSymbol -> SimpleNameSyntax -> ParameterSymbol
findParameter container parameterName =
    case filter match (functionSymbol_parameters container) of
        [symbol] -> symbol
        symbols  -> ErrorParameterSymbol container (displayName parameterName) (syntaxLocation parameterName) [bindError parameterName symbols]
    where
        match ParameterSymbol { parameterSymbol_name = parameterName' } = parameterName' == displayName parameterName
        match _ = False

findLocal :: FunctionSymbol -> SimpleNameSyntax -> LocalSymbol
findLocal container localName =
    case filter match (functionSymbol_locals container) of
        [symbol] -> symbol
        symbols  -> ErrorLocalSymbol container (displayName localName) (syntaxLocation localName) [bindError localName symbols]
    where
        match LocalSymbol { localSymbol_name = localName' } = localName' == displayName localName
        match _ = False

findLabel :: FunctionSymbol -> SimpleNameSyntax -> LabelSymbol
findLabel container labelName =
    case filter match (functionSymbol_labels container) of
        [symbol] -> symbol
        symbols  -> ErrorLabelSymbol container (displayName labelName) (syntaxLocation labelName) [bindError labelName symbols]
    where
        match LabelSymbol { labelSymbol_name = labelName' } = labelName' == displayName labelName
        match _ = False

findEntryPoint :: ModuleSymbol -> Maybe FunctionSymbol
findEntryPoint container =
    case filter match (moduleSymbol_functions container) of
        [symbol] -> Just symbol
        _        -> Nothing
    where
        match FunctionSymbol { functionSymbol_name = "@main", functionSymbol_returnType = VoidTypeSymbol, functionSymbol_parameters = [] } = True
        match _ = False

bindError :: (SyntaxNode n, NameSyntax n, Symbol a) => n -> [a] -> Diagnostic
bindError name candidates = BindError (syntaxLocation name) (displayName name) (map symbolPretty candidates)

--------------------------------------------------------------------------------
