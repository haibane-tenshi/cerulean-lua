local function fail(fn) 
    local err = pcall(fn)
    assert(not err)
end

-- arithmetic ops
fail(function() return nil + nil end)
fail(function() return nil - nil end)
fail(function() return nil * nil end)
fail(function() return nil / nil end)
fail(function() return nil // nil end)
fail(function() return nil % nil end)
fail(function() return nil ^ nil end)

fail(function() return -nil end)

-- bitwise ops
fail(function() return nil & nil end)
fail(function() return nil | nil end)
fail(function() return nil ~ nil end)
fail(function() return nil << nil end)
fail(function() return nil >> nil end)

fail(function() return ~nil end)

-- relation ops
assert(nil == nil)
assert(not (nil ~= nil))

fail(function() return nil < nil end)
fail(function() return nil <= nil end)
fail(function() return nil > nil end)
fail(function() return nil >= nil end)

-- logical ops
assert((nil and nil) == nil)
assert((nil and 0) == nil)
assert((0 and nil) == nil)

assert((nil or nil) == nil)
assert((0 or nil) == 0)
assert((nil or 0) == 0)

assert((not nil) == true)
