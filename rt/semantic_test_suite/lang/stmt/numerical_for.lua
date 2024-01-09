local b = 0
for i = 1, 0 do
    b = 1
end
assert(b == 0)

local b = 0
for i = 0, 0 do
    b = b + 1
end
assert(b == 1)

local b = 0
for i=0, 5 do
    assert(b == i)
    b = b + 1
end

local b = 0
for i=0, 5, 1 do
    assert(b == i)
    b = b + 1
end

local b = 0
for i=0, 5, 3 do
    assert(b == i)
    b = b + 3
end

local b = 0
for i=0, 5, 6 do
    assert(i == 0)
    assert(b == 0)
    b = 1
end
assert(b == 1)

local b = 0
for i=0, 5, -1 do
    assert(false)
end

local err = pcall(function() 
    for _=0, 1, 0 do
        
    end
end)
assert(err == false)

local b = 0
for i = 0, -5, -1 do
    assert(i == b)
    b = b - 1
end

local b = 0
for i = 0, -5, -2 do
    assert(i == b)
    b = b - 2
end

local b = 0
for i = 0, -5, -6 do
    assert(i == 0)
    assert(b == 0)
    b = 1
end
assert(b == 1)
