local a
assert(a == nil)

-- assignment
a = 2
assert(a == 2)

-- shadowing
do
    local a = 1
    assert(a == 1)
end
assert(a == 2)

local b
assert(a == 2)
assert(b == nil)

local b = 3
assert(a == 2)
assert(b == 3)

-- multivariable declarations
local c, d
assert(c == nil)
assert(d == nil)

c = 4
assert(c == 4)
assert(d == nil)

d = 5
assert(c == 4)
assert(d == 5)

local e, f = 6, 7
assert(e == 6)
assert(f == 7)

local g, h, i = 8
assert(g == 8)
assert(h == nil)
assert(i == nil)

-- variable loading
b = b
assert(b == b)

assert(a == 2)
assert(b == 3)
assert(c == 4)
assert(d == 5)
assert(e == 6)
assert(f == 7)
assert(g == 8)
assert(h == nil)
assert(i == nil)

b = c
assert(b == c)

assert(a == 2)
assert(b == 4)
assert(c == 4)
assert(d == 5)
assert(e == 6)
assert(f == 7)
assert(g == 8)
assert(h == nil)
assert(i == nil)

a, d, f = e, i

assert(a == e)
assert(d == i)

assert(a == 6)
assert(b == 4)
assert(c == 4)
assert(d == nil)
assert(e == 6)
assert(f == nil)
assert(g == 8)
assert(h == nil)
assert(i == nil)
