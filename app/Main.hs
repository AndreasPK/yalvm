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

file :: IO ByteString
file = BS.readFile "luac.out"


main :: IO ()
main = do
  x <- file
  let res = PBS.parseOnly Parser.loadLuaChunk x :: Either String Parser.LuaBinaryFile
  let luaChunk = either (const undefined) id res :: Parser.LuaBinaryFile
  let luaTopFunction = Parser.getTopFunction luaChunk
  state <- LVM.startExecution luaTopFunction
  print $ state

  result <- LVM.runLuaFunction $ return state

  Prelude.putStrLn "\nVM State:"
  print result

  Prelude.putStrLn "\nFinal Stack:"
  LVM.printStack $ return result

  print "lalala"

  -- BS8.putStrLn x
