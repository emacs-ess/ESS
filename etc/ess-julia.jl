
function _ess_list_categories()
    Base._jl_init_help()
    show("*ALL*"); print(" ")
    for cat = Base._jl_help_category_list
        show(cat)
        print(" ")
    end
end

function _ess_print_index(cat::ASCIIString)
    Base._jl_init_help()
    if cat == "*ALL*"
        println("   All help items:\n\n")
        for cat = Base._jl_help_category_list
            print("\n", cat, ":\n")
            for func =  Base._jl_help_category_dict[cat]
                print(func, " ")
            end
            println()
        end
    elseif has(Base._jl_help_category_dict, cat)
        println("  Help is available for the following items:\n\n")
        for func = Base._jl_help_category_dict[cat]
            print(func, ":\n")
        end
    else 
        error("Category $(cat) not found ")
    end 
end

function _ess_list_topics()
    Base._jl_init_help()
    for cat = Base._jl_help_category_list
        show(cat); println()
        for el = Base._jl_help_category_dict[cat]
            show(el); print(" ")
        end
    end
end


