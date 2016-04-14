import LVM
import LuaObjects
import LuaLoader
import Test.HUnit

main :: IO ()
main = do
  putStrLn "Test suite not yet implemented"
  runTestTT operatorTests
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
