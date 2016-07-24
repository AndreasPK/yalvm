local x = 0
for j = 1, 30 do
  x = 0
  for i = 1, 1000000 do
    x = x + i
  end
  print(x)
end
