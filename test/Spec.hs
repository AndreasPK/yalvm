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
import Debug.Trace

baseDir = "D:/Uni/00_SS2016/90_AbsMachine/yalvm/test"

main :: IO ()
main = do
  --traceM "main"
  --print "testReturn"
  result <- mapM (uncurry testFile)
        [ ("test/testFiles/testReturn.luac", [LOString "123\0"])
        , ("test/testFiles/nestedCall.luac", [LONumber 6])
        , ("test/testFiles/callHaskell.luac", [LONumber 5])
        , ("test/testFiles/fac.luac", [LONumber 120])
        , ("test/testFiles/print.luac", [LOBool True])
        , ("test/testFiles/forNum.luac", [LONumber 6])
        , ("test/testFiles/test.luac", [LONumber 120])
        ]

  traverse print result



  --putStrLn "Test suite not yet implemented"
  --runTestTT operatorTests
  return ()

{-operatorTests = TestList [
  True ~=? testStack (fromList [LONumber 1, LONumber 2]) (create (MUL, 0, 0, 0)) (fromList [LONumber 3, LONumber 2])
  ] -}

wrapStackTest :: LuaMap -> LuaInstruction -> LuaState
wrapStackTest stack instruction =  LuaState (LuaExecutionThread (LuaFunctionInstance stack [instruction] undefined undefined undefined undefined) undefined 0 undefined undefined) undefined

{-testStack :: LuaMap -> LuaInstruction -> LuaMap -> IO Bool
testStack stack instruction expectedStack =
  let state = wrapStackTest stack instruction
  in
  fmap (==) (fmap lGetStateStack (execOP $ return state))  expectedStack-}

lcompareResults :: [LuaObject] -> LuaMap -> IO [(Bool, LuaObject, LuaObject)]
lcompareResults expected results = do
  let comp = zipWith compareLO expected (toList results)
  print comp
  return comp

testFile :: FilePath -> [LuaObject] -> IO [(Bool, LuaObject, LuaObject)]
testFile path objs = do
  --traceM "testFile"
  let vmResult = runLuaCode path
  stack <- fmap lGetStateStack vmResult
  lcompareResults objs stack


runLuaCode :: FilePath -> IO LuaState
runLuaCode path = do
  --traceM "runLuaCode"
  fileContent <- BS.readFile path :: IO BS.ByteString
  --traceM "fr"
  let chunk = either (error "Failed to parse binary file") id $ LuaLoader.loadLua fileContent -- :: IO (Either String Parser.LuaBinaryFile)
  vm <- LVM.startExecution $ Parser.getTopFunction chunk :: IO LuaState
  --traceM "vc"
  vm <- return $ LRT.registerAll vm
  --traceM "Run chunk"
  LVM.runLuaFunction $ return vm
  --traceM "ranLuaCode"


compareLO :: LuaObject -> LuaObject -> (Bool, LuaObject, LuaObject)
compareLO a b =
  (a == b, a, b)
