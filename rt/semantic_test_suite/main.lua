local function test_entry(path)
    local script, err = loadfile(path)
    
    assert(script, err)
    
    local flag, err = pcall(script)
    
    if not flag then
        print("test failed:    ", path, "\n", err)
    else
        print("test completed: ", path)
    end
end

test_entry("./std/assert.lua")
test_entry("./lang/local_var.lua")
test_entry("./lang/func_decl.lua")
test_entry("./lang/func_call.lua")
test_entry("./lang/variadic_expr.lua")
test_entry("./lang/value/nil.lua")
test_entry("./lang/coercion/to_bool.lua")
test_entry("./lang/stmt/if_then.lua")
