local _ = ...

local function f(...) end

local function f(_, ...) end

local function f(...)
    local a = ...
end