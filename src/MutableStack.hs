{-# LANGUAGE FlexibleInstances #-}

module MutableStack where

import Data.STRef
--import Data.Vector
import Data.Vector.Mutable as VM
import LuaObjects

x = return [1, 2, 3, 4] >>=
  \ a ->
    return $ 1 : a >>=
      \ b -> return $ 2 : a >>=

{-
class LuaStack l where
  createStack :: Int -> l
  setElement :: l -> Int -> LuaObject -> l
  getElement :: l -> Int -> LuaObject
  getRange :: l -> Int -> Int -> [LuaObject]--get elements stack[a..b]
  setRange :: l -> Int -> [LuaObject] -> l --placed given objects in stack starting at position p
  stackSize :: l -> Int
  setStackSize :: l -> Int -> l
  pushObjects :: l -> [LuaObject] -> l
  fromList :: [LuaObject] -> l
  fromList = pushObjects (createStack 0)
  shrink :: l -> Int -> l --shrink stack by x elements if possible
  shrink s x = setStackSize s $ stackSize s - x
  toList :: l -> [LuaObject]
  toList l = map (getElement l) [0..stackSize l -1]
  setRange stack n objects = foldl (\s (k, v) -> setElement s k v) stack $ zip [n..] objects --requires stack to be at least (n - 1) in size
-}
