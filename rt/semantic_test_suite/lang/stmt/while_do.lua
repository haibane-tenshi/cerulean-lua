local b = 0
while false do
    b = 1
end
assert(b == 0)

local f = function()
    while true do
        return 1
    end
end
assert(f() == 1)

local b = 0
while b < 5 do
    b = b + 1
end
assert(b == 5)
