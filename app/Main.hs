module Main where

import           Data.ByteString            as BS
import           Data.ByteString.Char8      as BS8
import           LuaLoader

import           Data.Attoparsec.ByteString as PBS
import           Data.Bits                  as Bits
import           Data.ByteString            as B
import           Data.ByteString.Char8      as B8
import           Data.Word
import qualified LVM
import qualified Parser
import           LuaObjects
import qualified Control.Monad              as Monad
import qualified LRT
import qualified Data.Foldable              as Foldable
import Control.Monad.ST.Unsafe
import Control.Monad.ST
import System.Environment as Sys


testFile :: FilePath -> IO LVM.LuaState
testFile p = LVM.runLuaCode $ "D:\\Uni\\00_SS2016\\90_AbsMachine\\yalvm\\test\\testFiles\\" ++ p


file :: IO ByteString
file = BS.readFile "luac.out"

luaChunk :: IO Parser.LuaBinaryFile
luaChunk = do
  x <- file
  let res = PBS.parseOnly Parser.loadLuaChunk x :: Either String Parser.LuaBinaryFile
  let luaChunk = either (error "Parse error") id res :: Parser.LuaBinaryFile
  return luaChunk

main :: IO ()
main = do
  path <- Prelude.head <$> getArgs :: IO String
  print $ "Running " ++ path
  LVM.runLuaCode path
  return ()

  {-file <- BS.readFile path
  luaChunk <- Main.luaChunk
  let luaTopFunction = Parser.getTopFunction luaChunk :: Parser.LuaFunctionHeader
  state <- LVM.startExecution luaTopFunction :: IO LVM.LuaState

  state <- return $ LRT.setGlobal state "print" $ LOFunction LRT.lrtPrint

  result <- LVM.runLuaFunction $ return state-}

  --Prelude.putStrLn "\nVM State:"
  --print result

  --Prelude.putStrLn "\nFinal Stack:"
  --LVM.printStack $ return result

  --Prelude.putStrLn $ Prelude.replicate 10 '\n'
  --print "lalala"

  -- BS8.putStrLn x

stackWalk :: LVM.LuaState -> IO ()
stackWalk state =
  Monad.unless (1 == (execStop . LVM.stateExecutionThread) state) $ do
      let stack = LVM.lGetStateStack state
      ss <- stackSize stack
      Foldable.traverse_ (fmap print . getElement stack) [0..ss - 1]
      print "1-UP"
      stackWalk $ LVM.LuaState (execPrevInst . LVM.stateExecutionThread $ state) undefined
