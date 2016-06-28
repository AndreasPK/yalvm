function fac(x)
  if x == 1 then
    return x
  else
    return x * fac(x-1)
  end
end

for i=1,10000 do
  --print("Iteration" ++ i)
  print(fac(25))
  print("Loop")
end
print("end")
print(fac(25))
