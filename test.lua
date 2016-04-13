function fac(x)
  if x > 1 then
    return x*fac(x-1)
  else
    return 1
  end
end

return fac(2)
--print( fac(5))
--return 6, f(2)
