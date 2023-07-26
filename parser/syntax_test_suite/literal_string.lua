local _ = ""
local _ = ''
local _ = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890 `~!@#$%^&*()-_+=[]{};:/?|"
local _ = "'"
local _ = '"'

local _ = "\a\b\f\n\r\t\v\'\"\\"
local _ = "abc\
def"
local _ = "abc\z
   def"

local _ = "\x30"
local _ = "\60\060"
local _ = "\u{30}"