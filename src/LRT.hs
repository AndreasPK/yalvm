module LRT where

import LuaObjects
import Parser
import LVM


definePrint :: LuaState -> LuaState
definePrint (LuaState exec globals) =
  LuaState exec globals
