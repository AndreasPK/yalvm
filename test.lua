function f(x)
  if x < 1 then
    return 1
  else
    return 1 + f(x-1)
  end
end

local z = f(10)
print("...................")
return f(10)
