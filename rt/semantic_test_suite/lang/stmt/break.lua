local b = 0
while true do
    b = b + 1
    break
    b = b + 1
end
assert(b == 1)

local b = 0
while true do
    if b > 5 then
        break
    end
    b = b + 1
end
assert(b == 6)

local a = 0
local b = 0
while a < 5 do
    while b < 5 do
        b = b + 1
        break
        b = b + 2
    end
    a = a + 1
end
assert(a == 5)
assert(b == 5)
