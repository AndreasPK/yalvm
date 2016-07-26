--calculate factorial
function fac(x)
  if x <= 1 then
    return 1
  else
    return (x * fac(x-1))
  end
end

--local i = 5
--for j=1,10 do
--      print(fac(j))
--end

--local x = 2

--return 120
return fac(5)
