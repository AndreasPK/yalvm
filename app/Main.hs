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
import           LuaObjects
import qualified Control.Monad              as Monad
import qualified LRT
import Control.Monad.ST.Unsafe
import Control.Monad.ST

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
  state <- stToIO $ LVM.startExecution luaTopFunction :: IO LVM.LuaState

  state <- return $ LRT.setGlobal state "print" $ LOFunction LRT.lrtPrint

  result <- stToIO$ LVM.runLuaFunction $ return state

  --Prelude.putStrLn "\nVM State:"
  --print result

  --Prelude.putStrLn "\nFinal Stack:"
  --LVM.printStack $ return result

  Prelude.putStrLn $ Prelude.replicate 10 '\n'
  --print "lalala"

  -- BS8.putStrLn x

stackWalk :: LVM.LuaState -> IO ()
stackWalk state = do
  ps state
  return ()
  where
    ps (LVM.LuaState (LuaObjects.LuaExecInstanceTop res) _) =
      mapM_ print res
    ps state = do
      let (LVM.LuaState (LuaExecutionThread (LuaFunctionInstance stack _ _ _ _ _) prevInst pc execState callInfo) globals) = state
      Monad.foldM (\_ n -> print $ getElement stack n) () [0..stackSize stack - 1]
      print "1-UP"
      ps $ LVM.LuaState prevInst undefined
