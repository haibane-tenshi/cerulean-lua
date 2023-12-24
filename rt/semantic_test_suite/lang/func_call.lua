local f = function(a, b) 
    assert(a == "test")
    assert(b == nil)
end

local _ = f"test"

local g = function(a, b)
    assert(b == nil)
    return a
end

local _ = g{}

-- nested calls
local _ = f(g"test")
local _ = g(g(g{}))
