local Fibonacci = {}

-- Naive recursive
function Fibonacci.naive(n)
    if n < 2 then
      return n
    end
    return inner(n-1) + inner(n-2)
end

function tail_call(n)
  local function inner(m, a, b)
    if m == 0 then
      return a
    end
    return inner(m-1, b, a+b)
  end
  return inner(n, 0, 1)
end

tail_call(30)
