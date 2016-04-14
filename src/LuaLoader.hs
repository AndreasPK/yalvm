module LuaLoader(module LuaLoader) where

import Data.Word
import Data.Attoparsec.ByteString as PBS
import Data.Attoparsec.ByteString as APS
import Data.ByteString as B
import Data.ByteString.Char8 as B8
import Data.Bits as Bits
import Parser

type LuaInstruction = Word32

data LuaOPCode = MOVE | LOADK | LOADBOOL | LOADNIL | GETUPVAL | GETGLOBAL | GETTABLE | SETGLOBAL |
  SETUPVAL | SETTABLE | NEWTABLE | SELF | ADD | SUB | MUL | DIV | MOD | POW | UNM | NOT | LEN |
  CONCAT | JMP | EQ | LT | LE | TEST | TESTSET | CALL | TAILCALL | RETURN | FORLOOP | FORPREP |
  TFORLOOP | SETLIST | CLOSE | CLOSURE | VARARG
  deriving (Eq, Show, Enum)

class CLuaInstruction i where
  op :: i -> LuaOPCode
  ra :: i -> Int
  rb :: i -> Int
  rc :: i -> Int
  rbx :: i -> Int
  rsbx :: i -> Int
  create :: (LuaOPCode, Int, Int, Int) -> i

instance CLuaInstruction Word32 where
  op w = toEnum $ fromIntegral $ w .&. (2^6-1 :: Word32)
  ra w = fromIntegral $ shiftR w 6 .&. (2^8-1 :: Word32)
  rc w = fromIntegral $ shiftR w 14 .&. (2^9-1 :: Word32)
  rb w = fromIntegral $ shiftR w 23 .&. (2^9-1 :: Word32)
  rbx w = fromIntegral $ shiftR w 14 .&. (2^18-1 :: Word32)
  rsbx w = rbx w - 131071
  create (op, ra, rb, rc) = fromIntegral $
    fromEnum op +
    shiftL ra 6 +
    shiftL rc 14 +
    shiftL rb 23

getFunctionInstructions :: LuaFunctionHeader -> [LuaInstruction]
getFunctionInstructions (LuaFunctionHeader _ _ _ _ _ _ _ (LuaInstructionList _ instructions) _ _ _ _ _) = instructions

-- | decodeLuaInstruction instruction -> (opCode, RA, RB, RC, Bx, sBx)
decodeLuaInstruction :: LuaInstruction -> (LuaOPCode, Int, Int, Int, Int, Int)
decodeLuaInstruction w =
  (op, ra, rb, rc, rbx, rsbx)
  where
    op = toEnum $ fromIntegral $ w .&. (2^6-1 :: Word32)
    ra = fromIntegral $ shiftR w 6 .&. (2^8-1 :: Word32)
    rc = fromIntegral $ shiftR w 14 .&. (2^9-1 :: Word32)
    rb = fromIntegral $ shiftR w 23 .&. (2^9-1 :: Word32)
    rbx = fromIntegral $ shiftR w 14 .&. (2^18-1 :: Word32)
    rsbx = rbx - 131071

loadLua :: ByteString -> Either String LuaBinaryFile
loadLua = PBS.parseOnly loadLuaChunk --luaBinaryFile
