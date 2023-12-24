assert(true)

local err = pcall(function() return assert(false) end)
assert(not err)

local err = pcall(function() return assert() end)
assert(not err)
