function f(x)
  if x < 1 then
    return 1
  else
    return 1 + f(x-1)
  end
end

function ff(x) return f(x) end

--local z = ff(10)
--local y = f(5)
--local z = z + 1
--print("...................")
return ff(10)
