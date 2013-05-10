module ESS

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

## modified version of function show(io::IO, m::Method)
function fun_args(m::Method)
    tv = m.tvars
    io = OUTPUT_STREAM::IO
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
    print("(list nil nil '(")
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

function fun_args(s::String)
    fun_args(eval(parse(s)))
end

end 
