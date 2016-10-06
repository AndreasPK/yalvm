
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
import Data.Vector.Unboxed as VU
import Data.Vector as V

import Debug.Trace


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
  functions <- luaFunctionHeader header
  return $ LuaBinaryFile header [functions]

-- | given the size of Integers return a parser reading such a integer
getSize :: (Num sizeT) => LuaHeader -> PBS.Parser sizeT
getSize lh =
  let s = lhSizeT lh
  in
  case s of
      4 -> fromIntegral <$> anyWord32le
      8 -> fromIntegral <$> anyWord64le
      _ -> error "Unknown word size"

-- | given the size of Integers return a parser reading such a integer
getInt :: (Num int) => LuaHeader -> PBS.Parser int
getInt lh =
  let s = lhIntSize lh
  in
  case s of
      4 -> fromIntegral <$> anyWord32le
      8 -> fromIntegral <$> anyWord64le
      _ -> error "Unknown word size"

-- | Header of chunk, with Major Minor version and VM compitability to reference
--   Implementation.
-- LuaHeader major minor compatibel
data LuaHeader = LuaHeader
  { _lh1 :: Int
  , _lh2 ::Int
  , _lh3 :: Bool
  , _lh4 :: Word8 -- 1 = little endian
  , lhIntSize :: Word8 -- int size on vm
  , lhSizeT :: Word8 -- size_t size
  , _lh7 :: Word8 -- op code Size
  , _lh8 :: Word8 -- (8) lua number size
  , _lh9 :: Word8 -- 0 == floating point
  }
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
  ,  fhStart :: Int --line defined
  ,  fhEnd :: Int --line end
  ,  fhUpvalueCount :: Word8 --upvalueCount
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
luaFunctionHeader :: LuaHeader -> PBS.Parser LuaFunctionHeader
luaFunctionHeader lh = do
  sourceName <- luaString lh
  startLine <- getInt lh
  endLine <- getInt lh
  upvalueCount <- anyWord8
  parameterCount <- anyWord8
  varargFlag <- anyWord8 --not implemented
  maxStackSize <- anyWord8
  instList <- luaInstructionList lh
  constList <- luaConstList lh
  functionList <- luaFunctionList lh
  --debug info
  linePositions <- luaLinePositionList lh
  localList <- luaLocalList lh
  upvalues <- luaUpvalueList lh
  return $
    LuaFunctionHeader sourceName startLine endLine upvalueCount
      parameterCount varargFlag maxStackSize instList constList functionList
      linePositions localList upvalues

data LuaInstructionList = LuaInstructionList { lilLength :: Word32, lilInstructions :: !(VU.Vector Word32) } deriving (Eq, Show, Ord)

luaInstructionList :: LuaHeader -> PBS.Parser LuaInstructionList
luaInstructionList lh =
  do
  size <- getInt lh
  instructions <- PBS.count (fromIntegral size) anyWord32le
  return $ LuaInstructionList size $ VU.fromList instructions
  PBS.<?> "InstructionList"

data LuaConst = LuaNil |
  LuaConstNumber Double |
  LuaConstBool Bool |
  LuaConstString LuaString
  deriving (Eq, Show, Ord)

data LuaConstList = LuaConstList Int !(V.Vector LuaConst) deriving (Eq, Show, Ord)

luaConstList :: LuaHeader -> PBS.Parser LuaConstList
luaConstList lh = do
  x <- getInt lh
  y <- PBS.count (fromIntegral x) $ luaConst lh
  return $ LuaConstList x $! V.fromList y
  PBS.<?> "ConstList"

luaConst :: LuaHeader -> PBS.Parser LuaConst
luaConst ws =
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
      x <- luaString ws
      return $ LuaConstString x
  PBS.<?> "Const"


type LuaFunctionList = [LuaFunctionHeader]
--  deriving (Eq, Show)

luaFunctionList :: LuaHeader -> PBS.Parser LuaFunctionList
luaFunctionList ws =
  do
    size <- getInt ws
    let s = fromIntegral size
    PBS.count s $ luaFunctionHeader ws
  PBS.<?> "FunctionList"

-- the Nth instruction was genereated based on the n'th line of code in the origin file
type LuaInstructionPositionList = [Int]

luaLinePositionList :: LuaHeader -> PBS.Parser LuaInstructionPositionList
luaLinePositionList ws =
  do
    size <- getInt ws
    positions <- PBS.count (fromIntegral size) $ getInt ws
    return $ Prelude.map fromIntegral positions

-- | LuaLocalList size localList
data LuaLocalList = LuaLocalList
  Int [LuaLocal]
  deriving (Eq, Show, Ord)

luaLocalList :: LuaHeader -> PBS.Parser LuaLocalList
luaLocalList ws =
  do
    size <- getInt ws
    let s = fromIntegral size
    locals <- PBS.count s $ luaLocal ws
    return $ LuaLocalList s []--locals
  PBS.<?> "luaLocalList"

data LuaLocal = LuaLocal LuaString Int Int deriving (Show, Eq, Ord)

luaLocal :: LuaHeader -> PBS.Parser LuaLocal
luaLocal lh =
  do
    name <- luaString lh
    startOfScope <- getInt lh
    endOfScope <- getInt lh
    return $ LuaLocal name (fromIntegral startOfScope) (fromIntegral endOfScope)
  PBS.<?> "luaLocal"

data LuaUpvalue = LuaUpvalue LuaString deriving (Eq, Show, Ord)

data LuaUpvalueList = LuaUpvalueList Int [LuaUpvalue] deriving (Eq, Show, Ord)

luaUpvalueList :: LuaHeader -> PBS.Parser LuaUpvalueList
luaUpvalueList ws =
  do
    size <- getInt ws --anyWord32le
    let s = fromIntegral size
    upvalues <- PBS.count s $ luaString ws
    return $ LuaUpvalueList s $ fmap LuaUpvalue upvalues
  PBS.<?> "luaUpvalueList"

data LuaString = LuaString Int String deriving (Show, Eq, Ord)

luaString :: LuaHeader -> PBS.Parser LuaString
luaString ws = do
  l <- getSize ws --anyWord32le
  if l == 0 then return $ LuaString 0 []
    else
      do
        s <- PBS.take (fromIntegral l)
        return $ LuaString (fromIntegral l) $ B8.unpack s
