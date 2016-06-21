FOR %%X in (".\*.lua") DO (
	luac5.1 -o %%~nX.luac %%~nX.lua
	luac5.1 -l %%~nX.lua  > %%~nX.listing
	)
	
echo Done
pause