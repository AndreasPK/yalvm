--calculate factorial
function fac(x)
  if x <= 1 then
    return 1
  else
    return (x * fac(x-1))
  end
end

for i=1,10 do
  print(fac(i))
end

--returns 120
return fac(5)
