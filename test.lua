function f(x)
  if x == 1 then
    return 1
  else
    return x * f(x-1)
  end
end

local z = f(5)
print (z)
