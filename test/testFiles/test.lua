function fac(x)
  if x <= 1 then
    return 1
  else
    return (x * fac(x-1))
  end
end

for x=1,10000 do
  local r = ""
  for i=1, 100 do
    r = r .. "\n," .. fac(i)
  end
  if(x%1000==0) then print(1) end
end

return fac(5)
