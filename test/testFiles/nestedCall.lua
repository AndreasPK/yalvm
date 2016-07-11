
function f1(x)
  function f2(y)
    return y + 1
  end
  return f2(x)
end

--returns 6
return  f1(5)
