module ESS

function all_help_topics()
    Base.Help.init_help()
    ## show all categories 
    for (func, _ ) in Base.Help.MODULE_DICT
        if !isempty(Base.Help.MODULE_DICT[func])
            println()
            show(func);
        end
    end
end

function help(topic::String)
  Base.Help.help(topic)
end    

## modified version of function show(io::IO, m::Method)
function fun_args(m::Method)
    tv, decls, file, line = Base.arg_decl_parts(m)
    io = STDOUT::IO
    if !isempty(tv)
        Base.show_delim_array(io, tv, '{', ',', '}', false)
    end
    li = m.func.code
    e = Base.uncompressed_ast(li)
    argnames = e.args[1]
    print(io, "(")
    print_joined(io, [isempty(d[2]) ? d[1] : d[1]*"::"*d[2] for d in decls], ",", ",")    
    print(io, ")")
end 

## modified versionof show(io::IO, mt::MethodTable)
function fun_args(f::Function)
    mt = f.env
    mod = Base.function_module(f)
    if mod == Main
        mod = "nil"
    end 
    print("(list \"$mod\" nil '(")
    d = mt.defs
    while !is(d, ())
        print("\"")
        ## method
        fun_args(d)
        print("\" ")
        d = d.next
        if is(d,())
            print("))")
        end
    end
end

function fun_args(s::ASCIIString)
    try
        m = eval(current_module(), parse(s))
        if typeof(m) != ASCIIString
            fun_args(m)
        end
    catch
        print("(list nil nil nil)")
    end
end 

function fun_args(t::DataType)
    print("(list nil nil '(")
    for d = names(t)
        print("\"$d\" ")
    end
    print("))")
end 


### OBJECT COMPLETION
function components(m::Module)
    for v in sort(names(m))
        s = string(v)
        if isdefined(m,v)
            println(rpad(s, 30), summary(eval(m,v)))
        end
    end
end

function components(t::DataType)
    for v in sort(names(t))
        println(rpad(string(v), 30), "field")
    end
end

function components(v)
    t = typeof(v)
    if isa(t, DataType)
        return components(t)
    end
end


### MISC
function main_modules()
    mainmod = current_module()
    for nm in names(mainmod)
        if isdefined(mainmod, nm)
            mod = eval(mainmod, nm)
            if isa(mod, Module)
                print("\"$nm\" ")
            end
        end
    end
end

end 
