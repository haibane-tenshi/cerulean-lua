-- main body
local b = 0

if true then
    b = 1
end
assert(b == 1)

if false then
    b = 2
end
assert(b == 1)

-- 2 branches
local b = 0
if true then
    b = 1
else
    b = 2
end
assert(b == 1)

local b = 0
if false then 
    b = 1
else
    b = 2
end
assert(b == 2)

-- elseif
local b = 0
if true then
    b = 1
elseif true then
    b = 2
else
    b = 3
end
assert(b == 1)

local b = 0
if true then
    b = 1
elseif false then
    b = 2
else
    b = 3
end
assert(b == 1)

local b = 0
if false then
    b = 1
elseif true then
    b = 2
else
    b = 3
end
assert(b == 2)

local b = 0
if false then
    b = 1
elseif false then
    b = 2
else
    b = 3
end
assert(b == 3)
