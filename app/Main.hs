module Main where

import           Data.ByteString            as BS
import           Data.ByteString.Char8      as BS8
import           Lib
import           LuaLoader

import           Data.Attoparsec.ByteString as PBS
import           Data.Bits                  as Bits
import           Data.ByteString            as B
import           Data.ByteString.Char8      as B8
import           Data.Word
import qualified LVM
import qualified Parser
import qualified Control.Monad              as Monad

file :: IO ByteString
file = BS.readFile "luac.out"

luaChunk :: IO Parser.LuaBinaryFile
luaChunk = do
  x <- file
  let res = PBS.parseOnly Parser.loadLuaChunk x :: Either String Parser.LuaBinaryFile
  let luaChunk = either (const undefined) id res :: Parser.LuaBinaryFile
  return luaChunk

main :: IO ()
main = do
  luaChunk <- Main.luaChunk
  let luaTopFunction = Parser.getTopFunction luaChunk :: Parser.LuaFunctionHeader
  state <- LVM.startExecution luaTopFunction :: IO LVM.LuaState
  print state

  result <- LVM.runLuaFunction $ return state

  Prelude.putStrLn "\nVM State:"
  print result

  Prelude.putStrLn "\nFinal Stack:"
  LVM.printStack $ return result

  print "lalala"

  -- BS8.putStrLn x
