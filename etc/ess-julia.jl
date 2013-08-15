module ESS

function help_categories()
    Base.Help.init_help()
    for cat = Base.Help.CATEGORY_LIST
        if !isempty(Base.Help.CATEGORY_DICT[cat])
            println("\"$cat\" ")
        end
    end
end 
    
function all_help_topics()
    Base.Help.init_help()
    ## show all categories 
    for cat = Base.Help.CATEGORY_LIST
        if !isempty(Base.Help.CATEGORY_DICT[cat])
            println()
            show(cat); println();
            for func = Base.Help.CATEGORY_DICT[cat]
                print("  ")
                show(func)
            end
        end
    end
end

function help(topic::String)
    Base.Help.init_help()
    if !haskey(Base.Help.CATEGORY_DICT, topic)
        # if it's not a category, try another named thing
        try
            obj = eval(current_module(), parse(topic))
            Base.Help.help(obj)
        catch
            print("No help information found")
        end 
    else
        ln = length(topic)
        bar = " ===" * "="^ln * "==="
        println(bar)
        println(" =  ", topic, "  =")
        println(bar, "\n")
        for func = Base.Help.CATEGORY_DICT[topic]
            Base.Help.help(func)
            println()
            println("-"^72)
        end
    end
end     

## modified version of function show(io::IO, m::Method)
function fun_args(m::Method)
        tv = m.tvars
        io = STDOUT::IO
        if !isa(tv,Tuple)
            tv = (tv,)
        end
        if !isempty(tv)
            Base.show_delim_array(io, tv, '{', ',', '}', false)
        end
        li = m.func.code
        e = Base.uncompressed_ast(li)
        argnames = e.args[1]
        decls = map(Base.argtype_decl_string, argnames, {m.sig...})
        print(io, "(")
        print_joined(io, decls, ",", ",")
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
    while !is(d,())
        print("\"")
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
