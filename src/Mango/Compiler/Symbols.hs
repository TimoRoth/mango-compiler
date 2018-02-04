{-# LANGUAGE NoImplicitPrelude #-}

module Mango.Compiler.Symbols (
    -- Semantic Model
    SemanticModel (..),
    ModuleSymbol (..),
    TypeSymbol (..),
    FieldSymbol (..),
    FunctionSymbol (..),
    ParameterSymbol (..),
    LocalSymbol (..),
    LabelSymbol (..),
    Symbol (..),
    TypeLayout (..),
    typeLayout,
    moduleDependencies,
    functionType,
    returnsVoid,
    ) where

import Data.Bool
import Data.Eq
import Data.Function
import Data.Graph (Graph, reachable)
import Data.Int (Int)
import Data.List
import Data.Maybe
import Data.String (String)
import Mango.Compiler.Error
import Mango.Compiler.Syntax (Location (..))
import Prelude (Num (..), Integer, fromIntegral, undefined)
import Text.Show

--------------------------------------------------------------------------------

data SemanticModel
    = SemanticModel { semanticModel_modules :: [ModuleSymbol], semanticModel_moduleGraph :: Graph }

data ModuleSymbol
    = ModuleSymbol      { moduleSymbol_containingModel :: !SemanticModel, moduleSymbol_name :: !String, moduleSymbol_index :: {-# UNPACK #-} !Int, moduleSymbol_imports :: [ModuleSymbol], moduleSymbol_types :: [TypeSymbol], moduleSymbol_functions :: [FunctionSymbol], moduleSymbol_entryPoint :: Maybe FunctionSymbol, moduleSymbol_location :: !Location, moduleSymbol_diagnostics :: [Diagnostic] }
    | ErrorModuleSymbol { moduleSymbol_containingModel :: !SemanticModel, moduleSymbol_name :: !String,                                            moduleSymbol_imports :: [ModuleSymbol], moduleSymbol_types :: [TypeSymbol], moduleSymbol_functions :: [FunctionSymbol], moduleSymbol_entryPoint :: Maybe FunctionSymbol, moduleSymbol_location :: !Location, moduleSymbol_diagnostics :: [Diagnostic] }

data TypeSymbol
    = BoolTypeSymbol
    | Int8TypeSymbol
    | Int16TypeSymbol
    | Int32TypeSymbol
    | Int64TypeSymbol
    | UInt8TypeSymbol
    | UInt16TypeSymbol
    | UInt32TypeSymbol
    | UInt64TypeSymbol
    | Float32TypeSymbol
    | Float64TypeSymbol
    | VoidTypeSymbol
    | ArrayTypeSymbol      { arrayTypeSymbol_elementType :: !TypeSymbol, arrayTypeSymbol_length :: !Integer }
    | StructuredTypeSymbol { structuredTypeSymbol_containingSymbol :: !ModuleSymbol, structuredTypeSymbol_name :: !String, structuredTypeSymbol_fields :: ![FieldSymbol], structuredTypeSymbol_layout :: TypeLayout, structuredTypeSymbol_location :: !Location, structuredTypeSymbol_diagnostics :: [Diagnostic] }
    | ErrorTypeSymbol      { structuredTypeSymbol_containingSymbol :: !ModuleSymbol, structuredTypeSymbol_name :: !String, structuredTypeSymbol_fields :: ![FieldSymbol], structuredTypeSymbol_layout :: TypeLayout, structuredTypeSymbol_location :: !Location, structuredTypeSymbol_diagnostics :: [Diagnostic] }
    | FunctionTypeSymbol   { functionTypeSymbol_returnType :: !TypeSymbol, functionTypeSymbol_parameterTypes :: ![TypeSymbol] }
    | ReferenceTypeSymbol  { referenceTypeSymbol_referencedType :: !TypeSymbol }
    | SpanTypeSymbol       { spanTypeSymbol_elementType :: !TypeSymbol }
    | NullTypeSymbol

data FieldSymbol
    = FieldSymbol      { fieldSymbol_containingSymbol :: !TypeSymbol, fieldSymbol_name :: !String, fieldSymbol_fieldType :: TypeSymbol, fieldSymbol_offset :: Int, fieldSymbol_location :: !Location, fieldSymbol_diagnostics :: [Diagnostic] }
    | ErrorFieldSymbol { fieldSymbol_containingSymbol :: !TypeSymbol, fieldSymbol_name :: !String, fieldSymbol_fieldType :: TypeSymbol, fieldSymbol_offset :: Int, fieldSymbol_location :: !Location, fieldSymbol_diagnostics :: [Diagnostic] }

data FunctionSymbol
    = FunctionSymbol      { functionSymbol_containingSymbol :: !ModuleSymbol, functionSymbol_name :: !String, functionSymbol_returnType :: TypeSymbol, functionSymbol_parameters :: [ParameterSymbol], functionSymbol_locals :: [LocalSymbol], functionSymbol_labels :: [LabelSymbol], functionSymbol_ordinal :: !(Maybe Integer), functionSymbol_location :: !Location, functionSymbol_diagnostics :: [Diagnostic] }
    | ErrorFunctionSymbol { functionSymbol_containingSymbol :: !ModuleSymbol, functionSymbol_name :: !String, functionSymbol_returnType :: TypeSymbol, functionSymbol_parameters :: [ParameterSymbol], functionSymbol_locals :: [LocalSymbol], functionSymbol_labels :: [LabelSymbol], functionSymbol_ordinal :: !(Maybe Integer), functionSymbol_location :: !Location, functionSymbol_diagnostics :: [Diagnostic] }

data ParameterSymbol
    = ParameterSymbol      { parameterSymbol_containingSymbol :: !FunctionSymbol, parameterSymbol_name :: !String, parameterSymbol_parameterType :: TypeSymbol, parameterSymbol_location :: !Location, parameterSymbol_diagnostics :: [Diagnostic] }
    | ErrorParameterSymbol { parameterSymbol_containingSymbol :: !FunctionSymbol, parameterSymbol_name :: !String,                                              parameterSymbol_location :: !Location, parameterSymbol_diagnostics :: [Diagnostic] }

data LocalSymbol
    = LocalSymbol      { localSymbol_containingSymbol :: !FunctionSymbol, localSymbol_name :: !String, localSymbol_localType :: TypeSymbol, localSymbol_location :: !Location, localSymbol_diagnostics :: [Diagnostic] }
    | ErrorLocalSymbol { localSymbol_containingSymbol :: !FunctionSymbol, localSymbol_name :: !String,                                      localSymbol_location :: !Location, localSymbol_diagnostics :: [Diagnostic] }

data LabelSymbol
    = LabelSymbol      { labelSymbol_containingSymbol :: !FunctionSymbol, labelSymbol_name :: !String, labelSymbol_index :: !Int, labelSymbol_location :: !Location, labelSymbol_diagnostics :: [Diagnostic] }
    | ErrorLabelSymbol { labelSymbol_containingSymbol :: !FunctionSymbol, labelSymbol_name :: !String,                            labelSymbol_location :: !Location, labelSymbol_diagnostics :: [Diagnostic] }

class Symbol a where
    containingModel    :: a -> SemanticModel
    containingModule   :: a -> ModuleSymbol
    containingType     :: a -> TypeSymbol
    containingFunction :: a -> FunctionSymbol
    symbolName         :: a -> String
    symbolLocation     :: a -> Location
    symbolPretty       :: a -> String

instance Symbol ModuleSymbol where
    containingModel    = moduleSymbol_containingModel
    containingModule   = id
    containingType     = undefined
    containingFunction = undefined
    symbolName         = moduleSymbol_name
    symbolLocation     = moduleSymbol_location
    symbolPretty s     = concat ["module ", moduleSymbol_name s, " at ", show (moduleSymbol_location s)]

instance Symbol TypeSymbol where
    containingModel    = moduleSymbol_containingModel . structuredTypeSymbol_containingSymbol
    containingModule   = structuredTypeSymbol_containingSymbol
    containingType     = undefined
    containingFunction = undefined
    symbolName         = structuredTypeSymbol_name
    symbolLocation     = structuredTypeSymbol_location
    symbolPretty s     = concat ["type ", structuredTypeSymbol_name s, " at ", show (structuredTypeSymbol_location s)]

instance Symbol FieldSymbol where
    containingModel    = moduleSymbol_containingModel . structuredTypeSymbol_containingSymbol . fieldSymbol_containingSymbol
    containingModule   = structuredTypeSymbol_containingSymbol . fieldSymbol_containingSymbol
    containingType     = fieldSymbol_containingSymbol
    containingFunction = undefined
    symbolName         = fieldSymbol_name
    symbolLocation     = fieldSymbol_location
    symbolPretty s     = concat ["field ", fieldSymbol_name s, " at ", show (fieldSymbol_location s)]

instance Symbol FunctionSymbol where
    containingModel    = moduleSymbol_containingModel . functionSymbol_containingSymbol
    containingModule   = functionSymbol_containingSymbol
    containingType     = undefined
    containingFunction = id
    symbolName         = functionSymbol_name
    symbolLocation     = functionSymbol_location
    symbolPretty s     = concat ["function ", show (functionSymbol_returnType s), " ", functionSymbol_name s, "(", intercalate ", " (map (show . parameterSymbol_parameterType) (functionSymbol_parameters s)), ") at ", show (functionSymbol_location s)]

instance Symbol ParameterSymbol where
    containingModel    = moduleSymbol_containingModel . functionSymbol_containingSymbol . parameterSymbol_containingSymbol
    containingModule   = functionSymbol_containingSymbol . parameterSymbol_containingSymbol
    containingType     = undefined
    containingFunction = parameterSymbol_containingSymbol
    symbolName         = parameterSymbol_name
    symbolLocation     = parameterSymbol_location
    symbolPretty s     = concat ["parameter ", show (parameterSymbol_parameterType s), " ", show (parameterSymbol_name s), " at ", show (parameterSymbol_location s)]

instance Symbol LocalSymbol where
    containingModel    = moduleSymbol_containingModel . functionSymbol_containingSymbol . localSymbol_containingSymbol
    containingModule   = functionSymbol_containingSymbol . localSymbol_containingSymbol
    containingType     = undefined
    containingFunction = localSymbol_containingSymbol
    symbolName         = localSymbol_name
    symbolLocation     = localSymbol_location
    symbolPretty s     = concat ["local ", show (localSymbol_localType s), " ", show (localSymbol_name s), " at ", show (localSymbol_location s)]

instance Symbol LabelSymbol where
    containingModel    = moduleSymbol_containingModel . functionSymbol_containingSymbol . labelSymbol_containingSymbol
    containingModule   = functionSymbol_containingSymbol . labelSymbol_containingSymbol
    containingType     = undefined
    containingFunction = labelSymbol_containingSymbol
    symbolName         = labelSymbol_name
    symbolLocation     = labelSymbol_location
    symbolPretty s     = concat ["label ", show (labelSymbol_name s), " at ", show (labelSymbol_location s), ")"]

instance Eq ModuleSymbol where
    (ModuleSymbol _ _ _ _ _ _ _ location _)                 == (ModuleSymbol _ _ _ _ _ _ _ location' _)                 = location == location'
    (ErrorModuleSymbol _ name _ _ _ _ location _)           == (ErrorModuleSymbol _ name' _ _ _ _ location' _)          = location_path location == location_path location' && name == name'
    _                                                       == _                                                        = False

instance Eq TypeSymbol where
    BoolTypeSymbol                                          == BoolTypeSymbol                                           = True
    Int8TypeSymbol                                          == Int8TypeSymbol                                           = True
    Int16TypeSymbol                                         == Int16TypeSymbol                                          = True
    Int32TypeSymbol                                         == Int32TypeSymbol                                          = True
    Int64TypeSymbol                                         == Int64TypeSymbol                                          = True
    UInt8TypeSymbol                                         == UInt8TypeSymbol                                          = True
    UInt16TypeSymbol                                        == UInt16TypeSymbol                                         = True
    UInt32TypeSymbol                                        == UInt32TypeSymbol                                         = True
    UInt64TypeSymbol                                        == UInt64TypeSymbol                                         = True
    Float32TypeSymbol                                       == Float32TypeSymbol                                        = True
    Float64TypeSymbol                                       == Float64TypeSymbol                                        = True
    VoidTypeSymbol                                          == VoidTypeSymbol                                           = True
    (ArrayTypeSymbol elementType length_)                   == (ArrayTypeSymbol elementType' length_')                  = elementType == elementType' && length_ == length_'
    (StructuredTypeSymbol _ _ _ _ location _)               == (StructuredTypeSymbol _ _ _ _ location' _)               = location == location'
    (ErrorTypeSymbol container name _ _ _ _)                == (ErrorTypeSymbol container' name' _ _ _ _)               = container == container' && name == name'
    (FunctionTypeSymbol returnType parameterTypes)          == (FunctionTypeSymbol returnType' parameterTypes')         = returnType == returnType' && parameterTypes == parameterTypes'
    (ReferenceTypeSymbol referencedType)                    == (ReferenceTypeSymbol referencedType')                    = referencedType == referencedType'
    (SpanTypeSymbol elementType)                            == (SpanTypeSymbol elementType')                            = elementType == elementType'
    NullTypeSymbol                                          == NullTypeSymbol                                           = True
    _                                                       == _                                                        = False

instance Eq FieldSymbol where
    (FieldSymbol _ _ _ _ location _)                        == (FieldSymbol _ _ _ _ location' _)                        = location == location'
    (ErrorFieldSymbol container name _ _ _ _)               == (ErrorFieldSymbol container' name' _ _ _ _)              = container == container' && name == name'
    _                                                       == _                                                        = False

instance Eq FunctionSymbol where
    (FunctionSymbol _ _ _ _ _ _ _ location _)               == (FunctionSymbol _ _ _ _ _ _ _ location' _)               = location == location'
    (ErrorFunctionSymbol container name _ _ _ _ _ _ _)      == (ErrorFunctionSymbol container' name' _ _ _ _ _ _ _)     = container == container' && name == name'
    _                                                       == _                                                        = False

instance Eq ParameterSymbol where
    (ParameterSymbol _ _ _ location _)                      == (ParameterSymbol _ _ _ location' _)                      = location == location'
    (ErrorParameterSymbol container name _ _)               == (ErrorParameterSymbol container' name' _ _)              = container == container' && name == name'
    _                                                       == _                                                        = False

instance Eq LocalSymbol where
    (LocalSymbol _ _ _ location _)                          == (LocalSymbol _ _ _ location' _)                          = location == location'
    (ErrorLocalSymbol container name _ _)                   == (ErrorLocalSymbol container' name' _ _)                  = container == container' && name == name'
    _                                                       == _                                                        = False

instance Eq LabelSymbol where
    (LabelSymbol _ _ _ location _)                          == (LabelSymbol _ _ _ location' _)                          = location == location'
    (ErrorLabelSymbol container name _ _)                   == (ErrorLabelSymbol container' name' _ _)                  = container == container' && name == name'
    _                                                       == _                                                        = False

instance Show TypeSymbol where
    show BoolTypeSymbol                                 = "bool"
    show Int8TypeSymbol                                 = "i8"
    show Int16TypeSymbol                                = "i16"
    show Int32TypeSymbol                                = "i32"
    show Int64TypeSymbol                                = "i64"
    show UInt8TypeSymbol                                = "u8"
    show UInt16TypeSymbol                               = "u16"
    show UInt32TypeSymbol                               = "u32"
    show UInt64TypeSymbol                               = "u64"
    show Float32TypeSymbol                              = "f32"
    show Float64TypeSymbol                              = "f64"
    show VoidTypeSymbol                                 = "void"
    show (ArrayTypeSymbol elementType length_)          = concat [show elementType, "[", show length_, "]"]
    show (StructuredTypeSymbol container name _ _ _ _)  = concat ["<", symbolName container, "> ", name]
    show (ErrorTypeSymbol container name _ _ _ _)       = concat ["<", symbolName container, "> ", name]
    show (FunctionTypeSymbol returnType parameterTypes) = concat [show returnType, "(", intercalate ", " (map show parameterTypes), ")"]
    show (ReferenceTypeSymbol referencedType)           = concat [show referencedType, "&"]
    show (SpanTypeSymbol elementType)                   = concat [show elementType, "[]"]
    show NullTypeSymbol                                 = "null"

--------------------------------------------------------------------------------

data TypeLayout
    = TypeLayout { typeLayout_alignment :: Int, typeLayout_size :: Int }

typeLayout :: TypeSymbol -> TypeLayout
typeLayout   BoolTypeSymbol       {} = TypeLayout 1 1
typeLayout   Int8TypeSymbol       {} = TypeLayout 1 1
typeLayout   Int16TypeSymbol      {} = TypeLayout 2 2
typeLayout   Int32TypeSymbol      {} = TypeLayout 4 4
typeLayout   Int64TypeSymbol      {} = TypeLayout 4 8
typeLayout   UInt8TypeSymbol      {} = TypeLayout 1 1
typeLayout   UInt16TypeSymbol     {} = TypeLayout 2 2
typeLayout   UInt32TypeSymbol     {} = TypeLayout 4 4
typeLayout   UInt64TypeSymbol     {} = TypeLayout 4 8
typeLayout   Float32TypeSymbol    {} = TypeLayout 4 4
typeLayout   Float64TypeSymbol    {} = TypeLayout 4 8
typeLayout   VoidTypeSymbol       {} = TypeLayout 1 0
typeLayout t@ArrayTypeSymbol      {} = TypeLayout a (s * (fromIntegral (arrayTypeSymbol_length t))) where TypeLayout a s = typeLayout (arrayTypeSymbol_elementType t)
typeLayout t@StructuredTypeSymbol {} = structuredTypeSymbol_layout t
typeLayout t@ErrorTypeSymbol      {} = structuredTypeSymbol_layout t
typeLayout   FunctionTypeSymbol   {} = TypeLayout 4 4
typeLayout   ReferenceTypeSymbol  {} = TypeLayout 4 4
typeLayout   SpanTypeSymbol       {} = TypeLayout 4 8 
typeLayout   NullTypeSymbol       {} = TypeLayout 4 4

--------------------------------------------------------------------------------

moduleDependencies :: ModuleSymbol -> [ModuleSymbol]
moduleDependencies moduleSymbol =
    map (modules!!) $ nub $ concat [reachable moduleGraph (moduleSymbol_index symbol) | symbol@ModuleSymbol {} <- moduleSymbol_imports moduleSymbol]
    where
        modules = semanticModel_modules (containingModel moduleSymbol)
        moduleGraph = semanticModel_moduleGraph (containingModel moduleSymbol)

functionType :: FunctionSymbol -> TypeSymbol
functionType functionSymbol =
    FunctionTypeSymbol (functionSymbol_returnType functionSymbol) (map parameterSymbol_parameterType (functionSymbol_parameters functionSymbol))

returnsVoid :: FunctionSymbol -> Bool
returnsVoid FunctionSymbol { functionSymbol_returnType = VoidTypeSymbol } = True
returnsVoid _ = False

--------------------------------------------------------------------------------
