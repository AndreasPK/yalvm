local x = {}
x.a = 1
x.b = 2
x[1] = x.a + x.b

y = {}
y.a = 4

print(x[1])
print(y.a)
--prints 3
