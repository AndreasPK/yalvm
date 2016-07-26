--calculate factorial
function fac(x)
  if x <= 1 then
    return 1
  else
    return (x * fac(x-1))
  end
end

local i = 5
for j=1,5000 do
  for i=1,40 do
    if j == 5000 then
      if i % 20 == 0 then print(fac(i)) end
    end
  end
end

local x = 2

--returns 120
return fac(5)
