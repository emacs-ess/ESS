module ESS

export ess_test

import Base.CATEGORY_LIST
import Base.CATEGORY_DICT
import Base.clear_cache

function ess_help_topics()
    ## show all categories 
    for cat = CATEGORY_LIST
        if !isempty(CATEGORY_DICT[cat])
            print("  ")
            show(cat)
        end
    end
    
    println()
    
    for cat = CATEGORY_LIST
        if !isempty(CATEGORY_DICT[cat])
            for func = CATEGORY_DICT[cat]
                print("  ")
                show(func);
            end
        end
    end
end

end 
