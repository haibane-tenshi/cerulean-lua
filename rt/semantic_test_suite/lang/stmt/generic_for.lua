local iter = function(_, i) 
    if i < 6 then
        return i + 1
    else
        return nil
    end
end

local b = 0
for i in iter, nil, 0 do
    b = b + 1
    assert(i == b)
end
assert(b == 6)

local f = function() 
    return iter, nil, 0
end

local b = 0
for i, j in f() do
    b = b + 1
    assert(i == b)
    assert(j == nil)
end
assert(b == 6)

local iter = function(_, i)
    if i < 5 then
        return i + 1, 0
    end
end

local b = 0
for i, j in iter, nil, 0 do
    b = b + 1
    assert(i == b)
    assert(j == 0)
end
assert(b == 5)
