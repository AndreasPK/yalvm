local x = 0
z = 0
for j = 1, 15 do
  local x = 0
  for i = 1, 5000000 do
    x = x + i
  end
  local y = x
  z = y
end
print(z)
