local function test_entry(path)
    local script, err = loadfile(path)
    
    assert(script, err)
    
    local flag, err = pcall(script)
    
    print("test completed: ", path, "\n", "errors: ", err, "\n")
end

test_entry("./std/assert.lua")
test_entry("./lang/local_var.lua")
test_entry("./lang/func_decl.lua")
test_entry("./lang/func_call.lua")
test_entry("./lang/variadic_expr.lua")
test_entry("./lang/value/nil.lua")
test_entry("./lang/coercion/to_bool.lua")
test_entry("./lang/stmt/if_then.lua")
test_entry("./lang/stmt/while_do.lua")
test_entry("./lang/stmt/numerical_for.lua")
test_entry("./lang/stmt/generic_for.lua")
test_entry("./lang/stmt/repeat_until.lua")
test_entry("./lang/stmt/break.lua")
