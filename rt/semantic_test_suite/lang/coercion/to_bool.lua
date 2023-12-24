local err = pcall(function() assert(nil) end)
assert(err == false)

local err = pcall(function() assert(false) end)
assert(err == false)

assert(true)

assert(0)
assert(5)
assert(-5)

assert(0.0)
assert(0.5)
assert(-0.5)
assert(0.0/0.0)

assert("")
assert("test")

assert(function() end)
assert(assert)

assert({})
