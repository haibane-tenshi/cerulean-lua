local a
local t = {}

local _ = t.a
local _ = t[a]
local _ = t["a"]

local _ = t.f()
local _ = t:f()

local _ = t.a.b.c
local _ = t[a][a][a]
local _ = t[1][2][3]

local _ = t.a().b().c()
local _ = t:a():b():c()
