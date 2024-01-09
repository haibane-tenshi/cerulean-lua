local b = 0
repeat
    b = 1
until true
assert(b == 1)

local a = 1
local b = 3
repeat
    a = 2
    local b = 0
until b < a
assert(a == 2)
assert(b == 3)

local a = 0
local b = 3
repeat
    local b = 5
    a = a + 1
until a > b
assert(a == 6)
assert(b == 3)
