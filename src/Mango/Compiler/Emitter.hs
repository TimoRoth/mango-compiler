{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Mango.Compiler.Emitter (
    -- Emitter
    emitPackage,
    packageAsBytes,
    emitPackageAsC,
    packageAsC,
    dumpPackage,
    packageAsDump,
    ) where

import Data.Bool
import Data.ByteString.Builder
import Data.ByteString.Lazy
import Data.Int
import Data.Monoid
import Data.Ord
import Data.String
import Data.Word
import Mango.Compiler.CodeGen
import Mango.Compiler.Symbols
import Prelude (Num (..), fromIntegral)
import System.IO (IO, FilePath)

import qualified Data.ByteString as B
import qualified Data.List as L

--------------------------------------------------------------------------------

emitPackage :: FilePath -> CompiledPackage -> IO ()
emitPackage path package =
    writeFile path (packageAsBytes package)
    
packageAsBytes :: CompiledPackage -> ByteString
packageAsBytes (CompiledPackage modules) =
    toLazyByteString (word32LE (fromIntegral (L.length modules')) <> packageIndex modules' (fromIntegral (4 + 18 * L.length modules')) <> mconcat (L.map (\(CompiledModule _ _ _ _ _ _ _ _ _ image _) -> lazyByteString image) modules'))
    where
        modules' = L.sortOn (\(CompiledModule _ _ _ _ _ _ _ _ _ _ fingerprint) -> fingerprint) modules

packageIndex :: [CompiledModule] -> Int64 -> Builder
packageIndex [] _ =
    mempty
packageIndex (CompiledModule _ _ _ _ _ _ _ _ _ image fingerprint:rest) offset =
    byteString fingerprint <> word32LE (fromIntegral offset) <> word16LE (fromIntegral (length image)) <> packageIndex rest (offset + length image)

--------------------------------------------------------------------------------

emitPackageAsC :: FilePath -> CompiledPackage -> IO ()
emitPackageAsC path package =
    writeFile path (packageAsC package)

packageAsC :: CompiledPackage -> ByteString
packageAsC (CompiledPackage modules) =
    toLazyByteString (modulesAsC modules)

modulesAsC :: [CompiledModule] -> Builder
modulesAsC modules =
    string7 "#define ARRAY(...) { __VA_ARGS__ }\n" <>
    mconcat (L.map moduleAsC (L.sortOn (\(CompiledModule _ _ _ _ _ _ _ _ _ _ fingerprint) -> fingerprint) modules)) <>
    string7 "#undef ARRAY\n"

moduleAsC :: CompiledModule -> Builder
moduleAsC (CompiledModule symbol _ _ _ _ _ _ _ _ image fingerprint) =
    string7 "MODULE(" <> bytesAsC (fromStrict fingerprint) <> string7 ", \"" <> stringUtf8 (symbolName symbol) <> string7 "\", " <> nameAsC (symbolName symbol) <> string7 ", " <> int64Dec (length image) <> string7 ", " <> bytesAsC image <> string7 ")\n"

nameAsC :: String -> Builder
nameAsC name =
    L.foldr (\x xs -> char7 (if ((x >= 'A') && (x <= 'Z')) || ((x >= 'a') && (x <= 'z')) || ((x >= '0') && (x <= '9')) then x else '_') <> xs) mempty name

bytesAsC :: ByteString -> Builder
bytesAsC bytes =
    string7 "ARRAY( " <> foldr (\x xs -> string7 "0x" <> word8HexFixed x <> string7 ", " <> xs) mempty bytes <> string7 ")"

--------------------------------------------------------------------------------

dumpPackage :: FilePath -> CompiledPackage -> IO ()
dumpPackage path package =
    writeFile path (packageAsDump package)

packageAsDump :: CompiledPackage -> ByteString
packageAsDump (CompiledPackage modules) =
    toLazyByteString (mconcat (L.map moduleAsDump modules))

moduleAsDump :: CompiledModule -> Builder
moduleAsDump (CompiledModule symbol _ _ _ _ _ _ imports functions image fingerprint) =
    headerAsDump 0 (unpack (take 8 image)) symbol fingerprint <>
    mconcat (L.map (\(i, bytes) -> char7 '\n' <> bytesAsDump (8 + 12 * i) (B.unpack bytes) "-- import #" <> int64Dec i <> char7 '\n') (L.zip [0..] imports)) <>
    (if L.null functions then char7 '\n' else functionsAsDump image x xs ys) <>
    bytesAsDump (length image) [] "-- " <> string7 "eof" <> char7 '\n'
    where
        (x:xs) = L.map (\(CompiledFunction _ offset _ _ _ _) -> offset) functions
        ys = L.map (\(CompiledFunction symbol' _ _ _ _ _) -> symbol') functions

headerAsDump :: Int64 -> [Word8] -> ModuleSymbol -> B.ByteString -> Builder
headerAsDump p bytes symbol fingerprint =
    char7 '\n' <> bytesAsDump p (L.take 4 bytes) "-- " <> stringUtf8 (symbolPretty symbol) <> char7 '\n' <>
    mconcat (L.replicate 59 (char7 ' ')) <> string7 "-- fingerprint " <> mconcat (L.map word8HexFixed (B.unpack fingerprint)) <> char7 '\n' <> char7 '\n' <>
    mconcat (L.intersperse (char7 '\n') (instructionsAsDump (p + 4) (L.drop 4 bytes)))

functionsAsDump :: ByteString -> Int64 -> [Int64] -> [FunctionSymbol] -> Builder
functionsAsDump image p []     ys = functionAsDump p (unpack (drop p image)) (L.head ys) <> char7 '\n'
functionsAsDump image p (x:xs) ys = functionAsDump p (unpack (drop p (take x image))) (L.head ys) <> functionsAsDump image x xs (L.tail ys)

functionAsDump :: Int64 -> [Word8] -> FunctionSymbol -> Builder
functionAsDump p bytes symbol =
    char7 '\n' <> bytesAsDump p (L.take 3 bytes) "-- " <> stringUtf8 (symbolPretty symbol) <> char7 '\n' <> char7 '\n' <>
    mconcat (L.intersperse (char7 '\n') (instructionsAsDump (p + 3) (L.drop 3 bytes)))

instructionsAsDump :: Int64 -> [Word8] -> [Builder]
instructionsAsDump _ [] = mempty
instructionsAsDump p (0x00:                xs) = bytesAsDump p [0x00]                 "NOP"                                                                                            :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x01:                xs) = bytesAsDump p [0x01]                 "BREAK"                                                                                          :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x02:                xs) = bytesAsDump p [0x02]                 "HALT"                                                                                           :mempty:instructionsAsDump (p + 1) xs
instructionsAsDump p (0x04:                xs) = bytesAsDump p [0x04]                 "POP_X32"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x05:                xs) = bytesAsDump p [0x05]                 "POP_X64"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x06:                xs) = bytesAsDump p [0x06]                 "DUP_X32"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x07:                xs) = bytesAsDump p [0x07]                 "DUP_X64"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x0C:a:              xs) = bytesAsDump p [0x0C,a]               "LDLOC_I8 "  <> word8Dec a                                                                       :       instructionsAsDump (p + 2) xs
instructionsAsDump p (0x0D:a:              xs) = bytesAsDump p [0x0D,a]               "LDLOC_U8 "  <> word8Dec a                                                                       :       instructionsAsDump (p + 2) xs
instructionsAsDump p (0x0E:a:              xs) = bytesAsDump p [0x0E,a]               "LDLOC_I16 " <> word8Dec a                                                                       :       instructionsAsDump (p + 2) xs
instructionsAsDump p (0x0F:a:              xs) = bytesAsDump p [0x0F,a]               "LDLOC_U16 " <> word8Dec a                                                                       :       instructionsAsDump (p + 2) xs
instructionsAsDump p (0x10:a:              xs) = bytesAsDump p [0x10,a]               "LDLOC_X32 " <> word8Dec a                                                                       :       instructionsAsDump (p + 2) xs
instructionsAsDump p (0x11:a:              xs) = bytesAsDump p [0x11,a]               "LDLOC_X64 " <> word8Dec a                                                                       :       instructionsAsDump (p + 2) xs
instructionsAsDump p (0x12:a:              xs) = bytesAsDump p [0x12,a]               "LDLOCA "    <> word8Dec a                                                                       :       instructionsAsDump (p + 2) xs
instructionsAsDump p (0x13:a:              xs) = bytesAsDump p [0x13,a]               "STLOC_X32 " <> word8Dec a                                                                       :       instructionsAsDump (p + 2) xs
instructionsAsDump p (0x14:a:              xs) = bytesAsDump p [0x14,a]               "STLOC_X64 " <> word8Dec a                                                                       :       instructionsAsDump (p + 2) xs
instructionsAsDump p (0x15:                xs) = bytesAsDump p [0x15]                 "UNUSED21"                                                                                       :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x16:                xs) = bytesAsDump p [0x16]                 "UNUSED22"                                                                                       :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x17:                xs) = bytesAsDump p [0x17]                 "UNUSED23"                                                                                       :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x18:                xs) = bytesAsDump p [0x18]                 "RET"                                                                                            :mempty:instructionsAsDump (p + 1) xs
instructionsAsDump p (0x19:                xs) = bytesAsDump p [0x19]                 "RET_X32"                                                                                        :mempty:instructionsAsDump (p + 1) xs
instructionsAsDump p (0x1A:                xs) = bytesAsDump p [0x1A]                 "RET_X64"                                                                                        :mempty:instructionsAsDump (p + 1) xs
instructionsAsDump p (0x1B:                xs) = bytesAsDump p [0x1B]                 "CALLI"                                                                                          :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x1C:a:b:            xs) = bytesAsDump p [0x1C,a,b]             "CALL_S 0x"  <> word8HexFixed b <> word8HexFixed a                                               :       instructionsAsDump (p + 3) xs
instructionsAsDump p (0x1D:a:b:c:          xs) = bytesAsDump p [0x1D,a,b,c]           "CALL #"     <> word8Dec a <> string7 " 0x" <> word8HexFixed c <> word8HexFixed b                :       instructionsAsDump (p + 4) xs
instructionsAsDump p (0x1E:a:b:c:          xs) = bytesAsDump p [0x1E,a,b,c]           "SYSCALL "   <> int8Dec (fromIntegral a) <> string7 " 0x" <> word8HexFixed c <> word8HexFixed b  :       instructionsAsDump (p + 4) xs
instructionsAsDump p (0x1F:                xs) = bytesAsDump p [0x1F]                 "UNUSED31"                                                                                       :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x20:a:              xs) = bytesAsDump p [0x20,a]               "BR_S "      <> int8Dec (fromIntegral a)                                                         :mempty:instructionsAsDump (p + 2) xs
instructionsAsDump p (0x21:a:              xs) = bytesAsDump p [0x21,a]               "BRFALSE_S " <> int8Dec (fromIntegral a)                                                         :mempty:instructionsAsDump (p + 2) xs
instructionsAsDump p (0x22:a:              xs) = bytesAsDump p [0x22,a]               "BRTRUE_S "  <> int8Dec (fromIntegral a)                                                         :mempty:instructionsAsDump (p + 2) xs
instructionsAsDump p (0x23:a:b:            xs) = bytesAsDump p [0x23,a,b]             "BR 0x"      <> word8HexFixed b <> word8HexFixed a                                               :mempty:instructionsAsDump (p + 3) xs
instructionsAsDump p (0x24:a:b:            xs) = bytesAsDump p [0x24,a,b]             "BRFALSE 0x" <> word8HexFixed b <> word8HexFixed a                                               :mempty:instructionsAsDump (p + 3) xs
instructionsAsDump p (0x25:a:b:            xs) = bytesAsDump p [0x25,a,b]             "BRTRUE 0x"  <> word8HexFixed b <> word8HexFixed a                                               :mempty:instructionsAsDump (p + 3) xs
instructionsAsDump p (0x26:                xs) = bytesAsDump p [0x26]                 "UNUSED38"                                                                                       :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x27:                xs) = bytesAsDump p [0x27]                 "UNUSED39"                                                                                       :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x28:                xs) = bytesAsDump p [0x28]                 "LDC_I32_M1"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x29:                xs) = bytesAsDump p [0x29]                 "LDC_I32_0"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x2A:                xs) = bytesAsDump p [0x2A]                 "LDC_I32_1"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x2B:                xs) = bytesAsDump p [0x2B]                 "LDC_I32_2"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x2C:                xs) = bytesAsDump p [0x2C]                 "LDC_I32_3"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x2D:                xs) = bytesAsDump p [0x2D]                 "LDC_I32_4"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x2E:                xs) = bytesAsDump p [0x2E]                 "LDC_I32_5"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x2F:                xs) = bytesAsDump p [0x2F]                 "LDC_I32_6"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x30:                xs) = bytesAsDump p [0x30]                 "LDC_I32_7"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x31:                xs) = bytesAsDump p [0x31]                 "LDC_I32_8"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x32:a:              xs) = bytesAsDump p [0x32,a]               "LDC_I32_S 0x" <> word8HexFixed a                                                                :       instructionsAsDump (p + 2) xs
instructionsAsDump p (0x33:a:b:c:d:        xs) = bytesAsDump p [0x33,a,b,c,d]         "LDC_X32 0x"   <> mconcat (L.map word8HexFixed [d,c,b,a])                                        :       instructionsAsDump (p + 5) xs
instructionsAsDump p (0x34:a:b:c:d:e:f:g:h:xs) = bytesAsDump p [0x34,a,b,c,d,e,f,g,h] "LDC_X64 0x"   <> mconcat (L.map word8HexFixed [h,g,f,e,d,c,b,a])                                :       instructionsAsDump (p + 9) xs
instructionsAsDump p (0x35:a:b:c:          xs) = bytesAsDump p [0x35,a,b,c]           "LDFTN #"      <> word8Dec a <> string7 " 0x" <> word8HexFixed c <> word8HexFixed b              :       instructionsAsDump (p + 4) xs
instructionsAsDump p (0x36:                xs) = bytesAsDump p [0x36]                 "UNUSED54"                                                                                       :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x37:                xs) = bytesAsDump p [0x37]                 "UNUSED55"                                                                                       :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x38:                xs) = bytesAsDump p [0x38]                 "UNUSED56"                                                                                       :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x39:                xs) = bytesAsDump p [0x39]                 "UNUSED57"                                                                                       :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x3A:                xs) = bytesAsDump p [0x3A]                 "UNUSED58"                                                                                       :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x3B:                xs) = bytesAsDump p [0x3B]                 "UNUSED59"                                                                                       :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x3C:                xs) = bytesAsDump p [0x3C]                 "UNUSED60"                                                                                       :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x3D:                xs) = bytesAsDump p [0x3D]                 "UNUSED61"                                                                                       :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x3E:                xs) = bytesAsDump p [0x3E]                 "UNUSED62"                                                                                       :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x3F:                xs) = bytesAsDump p [0x3F]                 "UNUSED63"                                                                                       :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x40:                xs) = bytesAsDump p [0x40]                 "ADD_I32"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x41:                xs) = bytesAsDump p [0x41]                 "SUB_I32"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x42:                xs) = bytesAsDump p [0x42]                 "MUL_I32"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x43:                xs) = bytesAsDump p [0x43]                 "DIV_I32"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x44:                xs) = bytesAsDump p [0x44]                 "DIV_I32_UN"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x45:                xs) = bytesAsDump p [0x45]                 "REM_I32"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x46:                xs) = bytesAsDump p [0x46]                 "REM_I32_UN"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x47:                xs) = bytesAsDump p [0x47]                 "NEG_I32"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x48:                xs) = bytesAsDump p [0x48]                 "SHL_I32"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x49:                xs) = bytesAsDump p [0x49]                 "SHR_I32"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x4A:                xs) = bytesAsDump p [0x4A]                 "SHR_I32_UN"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x4B:                xs) = bytesAsDump p [0x4B]                 "AND_I32"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x4C:                xs) = bytesAsDump p [0x4C]                 "OR_I32"                                                                                         :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x4D:                xs) = bytesAsDump p [0x4D]                 "XOR_I32"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x4E:                xs) = bytesAsDump p [0x4E]                 "NOT_I32"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x4F:                xs) = bytesAsDump p [0x4F]                 "CEQ_I32"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x50:                xs) = bytesAsDump p [0x50]                 "CNE_I32"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x51:                xs) = bytesAsDump p [0x51]                 "CGT_I32"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x52:                xs) = bytesAsDump p [0x52]                 "CGT_I32_UN"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x53:                xs) = bytesAsDump p [0x53]                 "CGE_I32"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x54:                xs) = bytesAsDump p [0x54]                 "CGE_I32_UN"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x55:                xs) = bytesAsDump p [0x55]                 "CLT_I32"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x56:                xs) = bytesAsDump p [0x56]                 "CLT_I32_UN"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x57:                xs) = bytesAsDump p [0x57]                 "CLE_I32"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x58:                xs) = bytesAsDump p [0x58]                 "CLE_I32_UN"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x59:                xs) = bytesAsDump p [0x59]                 "CONV_I8_I32"                                                                                    :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x5A:                xs) = bytesAsDump p [0x5A]                 "CONV_U8_I32"                                                                                    :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x5B:                xs) = bytesAsDump p [0x5B]                 "CONV_I16_I32"                                                                                   :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x5C:                xs) = bytesAsDump p [0x5C]                 "CONV_U16_I32"                                                                                   :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x5D:                xs) = bytesAsDump p [0x5D]                 "UNUSED93"                                                                                       :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x5E:                xs) = bytesAsDump p [0x5E]                 "UNUSED94"                                                                                       :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x5F:                xs) = bytesAsDump p [0x5F]                 "UNUSED95"                                                                                       :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x60:a:b:            xs) = bytesAsDump p [0x60,a,b]             "NEWOBJ 0x" <> word8HexFixed b <> word8HexFixed a                                                :       instructionsAsDump (p + 3) xs
instructionsAsDump p (0x61:a:b:            xs) = bytesAsDump p [0x61,a,b]             "NEWARR 0x" <> word8HexFixed b <> word8HexFixed a                                                :       instructionsAsDump (p + 3) xs
instructionsAsDump p (0x64:                xs) = bytesAsDump p [0x64]                 "UNUSED100"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x65:                xs) = bytesAsDump p [0x65]                 "UNUSED101"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x66:                xs) = bytesAsDump p [0x66]                 "UNUSED102"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x67:                xs) = bytesAsDump p [0x67]                 "UNUSED103"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x68:a:b:            xs) = bytesAsDump p [0x68,a,b]             "LDFLD_I8 0x" <> word8HexFixed b <> word8HexFixed a                                              :       instructionsAsDump (p + 3) xs
instructionsAsDump p (0x69:a:b:            xs) = bytesAsDump p [0x69,a,b]             "LDFLD_U8 0x" <> word8HexFixed b <> word8HexFixed a                                              :       instructionsAsDump (p + 3) xs
instructionsAsDump p (0x6A:a:b:            xs) = bytesAsDump p [0x6A,a,b]             "LDFLD_I16 0x" <> word8HexFixed b <> word8HexFixed a                                             :       instructionsAsDump (p + 3) xs
instructionsAsDump p (0x6B:a:b:            xs) = bytesAsDump p [0x6B,a,b]             "LDFLD_U16 0x" <> word8HexFixed b <> word8HexFixed a                                             :       instructionsAsDump (p + 3) xs
instructionsAsDump p (0x6C:a:b:            xs) = bytesAsDump p [0x6C,a,b]             "LDFLD_X32 0x" <> word8HexFixed b <> word8HexFixed a                                             :       instructionsAsDump (p + 3) xs
instructionsAsDump p (0x6D:a:b:            xs) = bytesAsDump p [0x6D,a,b]             "LDFLD_X64 0x" <> word8HexFixed b <> word8HexFixed a                                             :       instructionsAsDump (p + 3) xs
instructionsAsDump p (0x6E:a:b:            xs) = bytesAsDump p [0x6E,a,b]             "LDFLDA 0x" <> word8HexFixed b <> word8HexFixed a                                                :       instructionsAsDump (p + 3) xs
instructionsAsDump p (0x70:a:b:            xs) = bytesAsDump p [0x70,a,b]             "STFLD_X16 0x" <> word8HexFixed b <> word8HexFixed a                                             :       instructionsAsDump (p + 3) xs
instructionsAsDump p (0x71:a:b:            xs) = bytesAsDump p [0x71,a,b]             "STFLD_X32 0x" <> word8HexFixed b <> word8HexFixed a                                             :       instructionsAsDump (p + 3) xs
instructionsAsDump p (0x72:a:b:            xs) = bytesAsDump p [0x72,a,b]             "STFLD_X64 0x" <> word8HexFixed b <> word8HexFixed a                                             :       instructionsAsDump (p + 3) xs
instructionsAsDump p (0x73:                xs) = bytesAsDump p [0x73]                 "UNUSED115"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x74:                xs) = bytesAsDump p [0x74]                 "UNUSED116"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x75:                xs) = bytesAsDump p [0x75]                 "UNUSED117"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x76:                xs) = bytesAsDump p [0x76]                 "UNUSED118"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x77:                xs) = bytesAsDump p [0x77]                 "UNUSED119"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x78:                xs) = bytesAsDump p [0x78]                 "UNUSED120"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x79:                xs) = bytesAsDump p [0x79]                 "UNUSED121"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x7A:                xs) = bytesAsDump p [0x7A]                 "UNUSED122"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x7B:                xs) = bytesAsDump p [0x7B]                 "UNUSED123"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x7C:                xs) = bytesAsDump p [0x7C]                 "UNUSED124"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x7D:                xs) = bytesAsDump p [0x7D]                 "UNUSED125"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x7E:                xs) = bytesAsDump p [0x7E]                 "LDELEM_I8"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x7F:                xs) = bytesAsDump p [0x7F]                 "LDELEM_U8"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x80:                xs) = bytesAsDump p [0x80]                 "LDELEM_I16"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x81:                xs) = bytesAsDump p [0x81]                 "LDELEM_U16"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x82:                xs) = bytesAsDump p [0x82]                 "LDELEM_X32"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x83:                xs) = bytesAsDump p [0x83]                 "LDELEM_X64"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x84:a:b:            xs) = bytesAsDump p [0x84,a,b]             "LDELEMA 0x" <> word8HexFixed b <> word8HexFixed a                                               :       instructionsAsDump (p + 3) xs
instructionsAsDump p (0x85:                xs) = bytesAsDump p [0x85]                 "STELEM_X8"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x86:                xs) = bytesAsDump p [0x86]                 "STELEM_X16"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x87:                xs) = bytesAsDump p [0x87]                 "STELEM_X32"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x88:                xs) = bytesAsDump p [0x88]                 "STELEM_X64"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x89:                xs) = bytesAsDump p [0x89]                 "UNUSED137"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x8A:                xs) = bytesAsDump p [0x8A]                 "UNUSED138"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x8B:                xs) = bytesAsDump p [0x8B]                 "UNUSED139"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x8C:                xs) = bytesAsDump p [0x8C]                 "UNUSED140"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x8D:                xs) = bytesAsDump p [0x8D]                 "UNUSED141"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x8E:                xs) = bytesAsDump p [0x8E]                 "UNUSED142"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x8F:                xs) = bytesAsDump p [0x8F]                 "UNUSED143"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x90:                xs) = bytesAsDump p [0x90]                 "ADD_I64"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x91:                xs) = bytesAsDump p [0x91]                 "SUB_I64"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x92:                xs) = bytesAsDump p [0x92]                 "MUL_I64"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x93:                xs) = bytesAsDump p [0x93]                 "DIV_I64"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x94:                xs) = bytesAsDump p [0x94]                 "DIV_I64_UN"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x95:                xs) = bytesAsDump p [0x95]                 "REM_I64"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x96:                xs) = bytesAsDump p [0x96]                 "REM_I64_UN"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x97:                xs) = bytesAsDump p [0x97]                 "NEG_I64"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x98:                xs) = bytesAsDump p [0x98]                 "SHL_I64"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x99:                xs) = bytesAsDump p [0x99]                 "SHR_I64"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x9A:                xs) = bytesAsDump p [0x9A]                 "SHR_I64_UN"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x9B:                xs) = bytesAsDump p [0x9B]                 "AND_I64"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x9C:                xs) = bytesAsDump p [0x9C]                 "OR_I64"                                                                                         :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x9D:                xs) = bytesAsDump p [0x9D]                 "XOR_I64"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x9E:                xs) = bytesAsDump p [0x9E]                 "NOT_I64"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0x9F:                xs) = bytesAsDump p [0x9F]                 "CEQ_I64"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xA0:                xs) = bytesAsDump p [0xA0]                 "CNE_I64"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xA1:                xs) = bytesAsDump p [0xA1]                 "CGT_I64"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xA2:                xs) = bytesAsDump p [0xA2]                 "CGT_I64_UN"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xA3:                xs) = bytesAsDump p [0xA3]                 "CGE_I64"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xA4:                xs) = bytesAsDump p [0xA4]                 "CGE_I64_UN"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xA5:                xs) = bytesAsDump p [0xA5]                 "CLT_I64"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xA6:                xs) = bytesAsDump p [0xA6]                 "CLT_I64_UN"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xA7:                xs) = bytesAsDump p [0xA7]                 "CLE_I64"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xA8:                xs) = bytesAsDump p [0xA8]                 "CLE_I64_UN"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xA9:                xs) = bytesAsDump p [0xA9]                 "CONV_I8_I64"                                                                                    :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xAA:                xs) = bytesAsDump p [0xAA]                 "CONV_U8_I64"                                                                                    :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xAB:                xs) = bytesAsDump p [0xAB]                 "CONV_I16_I64"                                                                                   :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xAC:                xs) = bytesAsDump p [0xAC]                 "CONV_U16_I64"                                                                                   :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xAD:                xs) = bytesAsDump p [0xAD]                 "CONV_I32_I64"                                                                                   :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xAE:                xs) = bytesAsDump p [0xAE]                 "CONV_U32_I64"                                                                                   :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xAF:                xs) = bytesAsDump p [0xAF]                 "CONV_I64_I32"                                                                                   :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xB0:                xs) = bytesAsDump p [0xB0]                 "CONV_U64_I32"                                                                                   :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xB1:                xs) = bytesAsDump p [0xB1]                 "CONV_I64_F32"                                                                                   :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xB2:                xs) = bytesAsDump p [0xB2]                 "CONV_U64_F32"                                                                                   :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xB3:                xs) = bytesAsDump p [0xB3]                 "CONV_I64_F64"                                                                                   :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xB4:                xs) = bytesAsDump p [0xB4]                 "CONV_U64_F64"                                                                                   :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xB5:                xs) = bytesAsDump p [0xB5]                 "UNUSED181"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xB6:                xs) = bytesAsDump p [0xB6]                 "UNUSED182"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xB7:                xs) = bytesAsDump p [0xB7]                 "UNUSED183"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xB8:                xs) = bytesAsDump p [0xB8]                 "UNUSED184"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xB9:                xs) = bytesAsDump p [0xB9]                 "UNUSED185"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xBA:                xs) = bytesAsDump p [0xBA]                 "UNUSED186"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xBB:                xs) = bytesAsDump p [0xBB]                 "UNUSED187"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xBC:                xs) = bytesAsDump p [0xBC]                 "UNUSED188"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xBD:                xs) = bytesAsDump p [0xBD]                 "UNUSED189"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xBE:                xs) = bytesAsDump p [0xBE]                 "UNUSED190"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xBF:                xs) = bytesAsDump p [0xBF]                 "UNUSED191"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xC0:                xs) = bytesAsDump p [0xC0]                 "ADD_F32"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xC1:                xs) = bytesAsDump p [0xC1]                 "SUB_F32"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xC2:                xs) = bytesAsDump p [0xC2]                 "MUL_F32"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xC3:                xs) = bytesAsDump p [0xC3]                 "DIV_F32"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xC4:                xs) = bytesAsDump p [0xC4]                 "REM_F32"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xC5:                xs) = bytesAsDump p [0xC5]                 "NEG_F32"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xC6:                xs) = bytesAsDump p [0xC6]                 "CEQ_F32"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xC7:                xs) = bytesAsDump p [0xC7]                 "CEQ_F32_UN"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xC8:                xs) = bytesAsDump p [0xC8]                 "CNE_F32"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xC9:                xs) = bytesAsDump p [0xC9]                 "CNE_F32_UN"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xCA:                xs) = bytesAsDump p [0xCA]                 "CGT_F32"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xCB:                xs) = bytesAsDump p [0xCB]                 "CGT_F32_UN"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xCC:                xs) = bytesAsDump p [0xCC]                 "CGE_F32"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xCD:                xs) = bytesAsDump p [0xCD]                 "CGE_F32_UN"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xCE:                xs) = bytesAsDump p [0xCE]                 "CLT_F32"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xCF:                xs) = bytesAsDump p [0xCF]                 "CLT_F32_UN"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xD0:                xs) = bytesAsDump p [0xD0]                 "CLE_F32"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xD1:                xs) = bytesAsDump p [0xD1]                 "CLE_F32_UN"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xD2:                xs) = bytesAsDump p [0xD2]                 "CONV_I8_F32"                                                                                    :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xD3:                xs) = bytesAsDump p [0xD3]                 "CONV_U8_F32"                                                                                    :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xD4:                xs) = bytesAsDump p [0xD4]                 "CONV_I16_F32"                                                                                   :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xD5:                xs) = bytesAsDump p [0xD5]                 "CONV_U16_F32"                                                                                   :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xD6:                xs) = bytesAsDump p [0xD6]                 "CONV_I32_F32"                                                                                   :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xD7:                xs) = bytesAsDump p [0xD7]                 "CONV_U32_F32"                                                                                   :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xD8:                xs) = bytesAsDump p [0xD8]                 "CONV_F32_I32"                                                                                   :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xD9:                xs) = bytesAsDump p [0xD9]                 "CONV_F32_I32_UN"                                                                                :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xDA:                xs) = bytesAsDump p [0xDA]                 "CONV_F32_I64"                                                                                   :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xDB:                xs) = bytesAsDump p [0xDB]                 "CONV_F32_I64_UN"                                                                                :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xDC:                xs) = bytesAsDump p [0xDC]                 "CONV_F32_F64"                                                                                   :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xDD:                xs) = bytesAsDump p [0xDD]                 "UNUSED221"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xDE:                xs) = bytesAsDump p [0xDE]                 "UNUSED222"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xDF:                xs) = bytesAsDump p [0xDF]                 "UNUSED223"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xE0:                xs) = bytesAsDump p [0xE0]                 "ADD_F64"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xE1:                xs) = bytesAsDump p [0xE1]                 "SUB_F64"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xE2:                xs) = bytesAsDump p [0xE2]                 "MUL_F64"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xE3:                xs) = bytesAsDump p [0xE3]                 "DIV_F64"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xE4:                xs) = bytesAsDump p [0xE4]                 "REM_F64"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xE5:                xs) = bytesAsDump p [0xE5]                 "NEG_F64"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xE6:                xs) = bytesAsDump p [0xE6]                 "CEQ_F64"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xE7:                xs) = bytesAsDump p [0xE7]                 "CEQ_F64_UN"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xE8:                xs) = bytesAsDump p [0xE8]                 "CNE_F64"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xE9:                xs) = bytesAsDump p [0xE9]                 "CNE_F64_UN"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xEA:                xs) = bytesAsDump p [0xEA]                 "CGT_F64"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xEB:                xs) = bytesAsDump p [0xEB]                 "CGT_F64_UN"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xEC:                xs) = bytesAsDump p [0xEC]                 "CGE_F64"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xED:                xs) = bytesAsDump p [0xED]                 "CGE_F64_UN"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xEE:                xs) = bytesAsDump p [0xEE]                 "CLT_F64"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xEF:                xs) = bytesAsDump p [0xEF]                 "CLT_F64_UN"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xF0:                xs) = bytesAsDump p [0xF0]                 "CLE_F64"                                                                                        :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xF1:                xs) = bytesAsDump p [0xF1]                 "CLE_F64_UN"                                                                                     :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xF2:                xs) = bytesAsDump p [0xF2]                 "CONV_I8_F64"                                                                                    :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xF3:                xs) = bytesAsDump p [0xF3]                 "CONV_U8_F64"                                                                                    :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xF4:                xs) = bytesAsDump p [0xF4]                 "CONV_I16_F64"                                                                                   :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xF5:                xs) = bytesAsDump p [0xF5]                 "CONV_U16_F64"                                                                                   :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xF6:                xs) = bytesAsDump p [0xF6]                 "CONV_I32_F64"                                                                                   :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xF7:                xs) = bytesAsDump p [0xF7]                 "CONV_U32_F64"                                                                                   :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xF8:                xs) = bytesAsDump p [0xF8]                 "CONV_F64_I32"                                                                                   :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xF9:                xs) = bytesAsDump p [0xF9]                 "CONV_F64_I32_UN"                                                                                :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xFA:                xs) = bytesAsDump p [0xFA]                 "CONV_F64_I64"                                                                                   :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xFB:                xs) = bytesAsDump p [0xFB]                 "CONV_F64_I64_UN"                                                                                :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xFC:                xs) = bytesAsDump p [0xFC]                 "CONV_F64_F32"                                                                                   :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xFD:                xs) = bytesAsDump p [0xFD]                 "UNUSED253"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xFE:                xs) = bytesAsDump p [0xFE]                 "UNUSED254"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p (0xFF:                xs) = bytesAsDump p [0xFF]                 "UNUSED255"                                                                                      :       instructionsAsDump (p + 1) xs
instructionsAsDump p bytes                     = bytesAsDump p bytes                  ""                                                                                               :       []

bytesAsDump :: Int64 -> [Word8] -> String -> Builder
bytesAsDump p xs s = int64HexFixed p <> string7 "    " <> mconcat (L.intersperse (char7 ' ') (L.map word8HexFixed xs L.++ L.replicate (12 - L.length xs) (char7 ' ' <> char7 ' '))) <> string7 "    " <> stringUtf8 s

--------------------------------------------------------------------------------
