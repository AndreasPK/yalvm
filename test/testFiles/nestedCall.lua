function f(x)
  if x < 1 then
    return 1
  else
    return 1 + f(x-1)
  end
end

function ff(x) return f(x) end

return  ff(5)
