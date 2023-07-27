local f = function () end

f()
f(nil)
f(nil, nil)
f(nil, nil, nil)

local f = function(_, _) end

f()
f(nil)
f(nil, nil)
f(nil, nil, nil)
