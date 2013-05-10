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

end 
