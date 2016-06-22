function f1(x)
  local foo = x
  local f2 = function ()
    foo = foo+1
    return foo
  end

  return f2
end

--local x = f1(1)

--print(x())

return x()
