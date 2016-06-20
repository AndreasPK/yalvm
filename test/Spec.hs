import LVM
import LuaObjects
import LuaLoader
import Test.HUnit
import qualified LRT
import qualified Control.Monad              as Monad
import qualified LVM
import qualified Parser
import qualified Data.ByteString as BS
import qualified Data.Attoparsec.ByteString as PBS
import Control.Monad.ST


main :: IO ()
main = do
  r <- testFile "test/testFiles/testReturn.luac" [LOString "123\0"]
  print r
  r <- testFile "test/testFiles/nestedCall.luac" [LOString "123\0"]

  print r
  print . show <$> testFile "test/testFiles/callHaskell.luac" [LOString "123\0"]

  --putStrLn "Test suite not yet implemented"
  --runTestTT operatorTests
  return ()

operatorTests = TestList [
  True ~=? testStack (fromList [LONumber 1, LONumber 2]) (create (MUL, 0, 0, 0)) (fromList [LONumber 3, LONumber 2])
  ]

wrapStackTest :: LuaMap -> LuaInstruction -> LuaState
wrapStackTest stack instruction =  LuaState (LuaExecutionThread (LuaFunctionInstance stack [instruction] undefined undefined undefined undefined) undefined 0 undefined undefined) undefined

testStack :: LuaMap -> LuaInstruction -> LuaMap -> Bool
testStack stack instruction expectedStack =
  let state = wrapStackTest stack instruction
  in
  lGetStateStack (execPureOP state) == expectedStack

lcompareResults :: [LuaObject] -> LuaMap -> [(Bool, LuaObject, LuaObject)]
lcompareResults expected results = zipWith compareLO expected (toList results)

testFile :: FilePath -> [LuaObject] -> IO [(Bool, LuaObject, LuaObject)]
testFile path objs = do
  let vmResult = runLuaCode path
  stack <- fmap lGetStateStack vmResult
  return $ lcompareResults objs stack


runLuaCode :: FilePath -> IO LuaState
runLuaCode path = do
  fileContent <- BS.readFile path :: IO BS.ByteString
  let chunk = either (error "Failed to parse binary file") id $ LuaLoader.loadLua fileContent -- :: IO (Either String Parser.LuaBinaryFile)
  vm <- stToIO $ LVM.startExecution $ Parser.getTopFunction chunk :: IO LuaState
  let vm = LRT.registerAll vm
  stToIO $ LVM.runLuaFunction $ return vm

compareLO :: LuaObject -> LuaObject -> (Bool, LuaObject, LuaObject)
compareLO a b =
  (a == b, a, b)
