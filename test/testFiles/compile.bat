FOR %%X in (".\*.lua") DO luac5.1 -o %%~nX.luac %%~nX.lua
echo Done
pause