
module Parser(
  loadLuaChunk, LuaBinaryFile(..), LuaString(..), LuaHeader(..), LuaFunctionHeader(..),
  LuaInstructionList(..), LuaConst(..), LuaConstList(..), LuaFunctionList(..),
  LuaInstructionPositionList(..),  LuaLocalList(..), LuaUpvalueList(..), getTopFunction,
  getIndexedFunction) where

import Data.Word
import Data.Attoparsec.ByteString as PBS
import Data.Attoparsec.Binary
import Data.ByteString as B
import Data.ByteString.Char8 as B8
import qualified Data.Bits as Bits
import Data.Binary.IEEE754
import Debug.Trace
import Data.Vector.Unboxed as VU
import Data.Vector as V


luaHeaderDefinition :: ByteString
luaHeaderDefinition = B8.pack "\x1b\x4c\x75\x61"

data LuaBinaryFile = LuaBinaryFile
  LuaHeader
  [LuaFunctionHeader]
  deriving (Eq, Show)

getTopFunction :: LuaBinaryFile -> LuaFunctionHeader
getTopFunction (LuaBinaryFile _ functions) = Prelude.head functions

getIndexedFunction :: LuaFunctionHeader -> Int -> LuaFunctionHeader
getIndexedFunction (LuaFunctionHeader _ _ _ _ _ _ _ _ _ functionList _ _ _) pos =
  functionList !! pos


loadLuaChunk :: PBS.Parser LuaBinaryFile
loadLuaChunk = luaBinaryFile

luaBinaryFile :: PBS.Parser LuaBinaryFile
luaBinaryFile = do
  header <- loadHeader
  functions <- luaFunctionHeader
  return $ LuaBinaryFile header [functions]

-- | Header of chunk, with Major Minor version and VM compitability to reference
--   Implementation.
-- LuaHeader major minor compatibel
data LuaHeader = LuaHeader Int Int Bool
  Word8 -- 1 = little endian
  Word8 -- int size on vm
  Word8 -- size_t size
  Word8 -- op code Size
  Word8 -- (8) lua number size
  Word8 -- 0 == floating point
  deriving (Eq, Show)

--Parses the lua 5.1 (and 5.2) file header
loadHeader :: PBS.Parser LuaHeader
loadHeader = do
  PBS.string luaHeaderDefinition  --4 Bytes
  versionWord <- PBS.anyWord8     --1 Byte
  compatibleFlag <- PBS.anyWord8  --1 Byte
  endianness <- PBS.anyWord8      --1 Byte
  intSize <- PBS.anyWord8         --1 Byte
  size_tSize <- PBS.anyWord8      --1 Byte
  opSize <- PBS.anyWord8          --1 Byte
  numberSize <- PBS.anyWord8      --1 Byte
  numberType <- PBS.anyWord8      --1 Byte

  let version = decodeLuaVersion versionWord -- (major, minor)
  return $ uncurry LuaHeader version (compatibleFlag == 0) endianness intSize size_tSize opSize numberSize numberType

--Decode version word to MAjor/Minor version
decodeLuaVersion :: Word8 -> (Int, Int)
decodeLuaVersion w =
  let
  major = Bits.shiftR w 4
  minor = w Bits..&. 15
  in (fromIntegral major, fromIntegral minor)

data LuaFunctionHeader = LuaFunctionHeader
  {  fhName :: LuaString -- name
  ,  fhStart :: Word32 --line defined
  ,  fhEnd :: Word32 --line end
  ,  fhUpvalueCount ::Word8 --upvalueCount
  ,  fhParameterCount :: Word8 --parameterCount
  ,  fhVarargFlag :: Word8 --varargFlag
  ,  fhMaxStacksize :: Word8 --maxStackSize
  ,  fhInstructions :: LuaInstructionList
  ,  fhConstants :: LuaConstList
  ,  fhFunctions :: LuaFunctionList
  ,  fhInstPos :: LuaInstructionPositionList
  ,  fhLocalNames :: LuaLocalList
  ,  fhUpvalueNames :: LuaUpvalueList
  }
  deriving (Eq, Show, Ord)

-- | Read a function header from a lua chunk, assumes 4 byte ints, opcodes and sizeOf fields as well as little endian
luaFunctionHeader :: PBS.Parser LuaFunctionHeader
luaFunctionHeader = do
  sourceName <- luaString
  startLine <- anyWord32le
  endLine <- anyWord32le
  upvalueCount <- anyWord8
  parameterCount <- anyWord8
  varargFlag <- anyWord8 --not implemented
  maxStackSize <- anyWord8
  instList <- luaInstructionList
  constList <- luaConstList
  functionList <- luaFunctionList
  --debug info
  linePositions <- luaLinePositionList
  localList <- luaLocalList
  upvalues <- luaUpvalueList
  return $
    LuaFunctionHeader sourceName startLine endLine upvalueCount
      parameterCount varargFlag maxStackSize instList constList functionList
      linePositions localList upvalues

data LuaInstructionList = LuaInstructionList { lilLength :: Word32, lilInstructions :: !(VU.Vector Word32) } deriving (Eq, Show, Ord)

luaInstructionList :: PBS.Parser LuaInstructionList
luaInstructionList =
  do
  size <- anyWord32le
  instructions <- PBS.count (fromIntegral size) anyWord32le
  return $ LuaInstructionList size $ VU.fromList instructions
  PBS.<?> "InstructionList"

data LuaConst = LuaNil |
  LuaConstNumber Double |
  LuaConstBool Bool |
  LuaConstString LuaString
  deriving (Eq, Show, Ord)

data LuaConstList = LuaConstList Word32 !(V.Vector LuaConst) deriving (Eq, Show, Ord)

luaConstList :: PBS.Parser LuaConstList
luaConstList = do
  x <- anyWord32le
  y <- PBS.count (fromIntegral x) luaConst
  return $ LuaConstList x $! V.fromList y
  PBS.<?> "ConstList"

luaConst :: PBS.Parser LuaConst
luaConst =
  do
  t <- anyWord8
  case t of
    0 -> return LuaNil
    1 -> do
      x <- anyWord8
      return $ LuaConstBool $ x /= 0
    3 -> do
      x <- anyWord64le
      return $ LuaConstNumber $ wordToDouble  x
    4 -> do
      x <- luaString
      return $ LuaConstString x
  PBS.<?> "Const"


type LuaFunctionList = [LuaFunctionHeader]
--  deriving (Eq, Show)

luaFunctionList :: PBS.Parser LuaFunctionList
luaFunctionList =
  do
    size <- anyWord32le
    let s = fromIntegral size
    PBS.count s luaFunctionHeader
  PBS.<?> "FunctionList"

-- the Nth instruction was genereated based on the n'th line of code in the origin file
type LuaInstructionPositionList = [Int]

luaLinePositionList :: PBS.Parser LuaInstructionPositionList
luaLinePositionList =
  do
    size <- anyWord32le
    positions <- PBS.count (fromIntegral size) anyWord32le
    return $ Prelude.map fromIntegral positions

-- | LuaLocalList size localList
data LuaLocalList = LuaLocalList
  Int [LuaLocal]
  deriving (Eq, Show, Ord)

luaLocalList :: PBS.Parser LuaLocalList
luaLocalList =
  do
    size <- anyWord32le
    let s = fromIntegral size
    locals <- PBS.count s luaLocal
    return $ LuaLocalList s []--locals
  PBS.<?> "luaLocalList"

data LuaLocal = LuaLocal LuaString Int Int deriving (Show, Eq, Ord)

luaLocal :: PBS.Parser LuaLocal
luaLocal =
  do
    name <- luaString
    startOfScope <- anyWord32le
    endOfScope <- anyWord32le
    return $ LuaLocal name (fromIntegral startOfScope) (fromIntegral endOfScope)
  PBS.<?> "luaLocal"

data LuaUpvalue = LuaUpvalue LuaString deriving (Eq, Show, Ord)

data LuaUpvalueList = LuaUpvalueList Int [LuaUpvalue] deriving (Eq, Show, Ord)

luaUpvalueList :: PBS.Parser LuaUpvalueList
luaUpvalueList =
  do
    size <- anyWord32le
    let s = fromIntegral size
    upvalues <- PBS.count s luaString
    return $ LuaUpvalueList s $ fmap LuaUpvalue upvalues
  PBS.<?> "luaUpvalueList"

data LuaString = LuaString Int String deriving (Show, Eq, Ord)

luaString :: PBS.Parser LuaString
luaString = do
  l <- anyWord32le
  if l == 0 then return $ LuaString 0 []
    else
      do
        s <- PBS.take (fromIntegral l)
        return $ LuaString (fromIntegral l) $ B8.unpack s
