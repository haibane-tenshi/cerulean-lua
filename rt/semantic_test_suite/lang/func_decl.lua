-- decl + args
local f0 = function () end
local _ = f0()
local _ = f0(1)
local _ = f0(1, 2, 3)

local f1 = function(a) 
    assert(a == 1)
end
local _ = f1(1)
local _ = f1(1, 2)
local _ = f1(1, 2, 3, f1)

local f2 = function(a)
    assert(a == nil)
end

local _ = f2()
local _ = f2(nil)
local _ = f2(nil, 1, 2, 3)

-- multiple args
local f = function(a, b, c) 
    assert(a == nil)
    assert(b == 2)
    assert(c == nil)
end
local _ = f(nil, 2)

-- returns
local f = function()
    return
end

local err = pcall(function() assert(f()) end)
assert(not err)

local f = function() 
    return nil
end
assert(f() == nil)

local f = function()
    return 3
end
assert(f() == 3)

local f = function(a)
    return a
end
assert(f(3) == 3)
assert(f(1000) == 1000)
assert(f("") == "")
assert(f(f) == f)

-- variadic returns
local f = function()
    return 1, 2, 3
end
local a = f()
assert(a == 1)

local a, b = f()
assert(a == 1)
assert(b == 2)

local a, b, c = f()
assert(a == 1)
assert(b == 2)
assert(c == 3)

local a, b, c, d = f()
assert(a == 1)
assert(b == 2)
assert(c == 3)
assert(d == nil)
