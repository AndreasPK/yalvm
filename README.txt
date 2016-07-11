Yet another LuaVM

Das Projekt kann mit dem tool stack (http://docs.haskellstack.org/en/stable/README/) compiliert werden.
> stack build

Unter test/testFiles befinden sich einige Lua scripts welche von der VM aktuell unterstüzt werden.

Lua files können mit yalvm-exe <fileName> ausgeführt werden. Files müssen mit lua5.1 vorcompiliert werden.
> cd test/testFiles
> luac5.1 -o fac.luac fac.lua
> yalvm-exe fac.luac
