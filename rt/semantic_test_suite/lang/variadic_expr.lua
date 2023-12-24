local test = function(...)
    local f = function() 
        return 1, 2
    end
    
    local a, b, c = f()
    assert(a == 1)
    assert(b == 2)
    assert(c == nil)
    
    local a, b, c = 0, f()
    assert(a == 0)
    assert(b == 1)
    assert(c == 2)
    
    local a, b, c = f(), 0
    assert(a == 1)
    assert(b == 0)
    assert(c == nil)
    
    local a, b, c = ...
    assert(a == 4)
    assert(b == 5)
    assert(c == nil)
    
    local a, b, c = 0, ...
    assert(a == 0)
    assert(b == 4)
    assert(c == 5)
    
    local a, b, c = ..., 0
    assert(a == 4)
    assert(b == 0)
    assert(c == nil)
    
    local g = function(a, b, c)
        assert(a == 1)
        assert(b == 2)
        assert(c == nil)
    end
    
    local _ = g(f())
end

test(4, 5)


