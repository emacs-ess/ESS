#### S-mode session functions.
#### assign(..., where=0) makes them last only for the current session.

#### Well, turns out we have two slightly different versions of the S code.
#### This is the one that I think gets used with S-Plus; I'll send along
#### the other version as well, but morally they should be equivalent, there
#### are just a couple of changes I seem to have made for S4. /John

### Martin Maechler:  see also ./ess-s4.S
###		      which does *not* use assign (why?)
assign(".SmodeObs",
       function(where, pattern) {
           if(pattern == "") objects(where)
           else objects(where, pattern)
       }, where=0)

assign(".SmodeLoad",
       function(x)
       ## source function for S-mode
   {
       assign(".SmodeTmp", options(error=dump.calls), where=0, i=T)
       on.exit({options(.SmodeTmp); remove(".SmodeTmp",where=0)})
       source(x)
   },
       where = 0)

assign(".SmodeDump",
       function(x, name)
       ## dump function for S-mode
   {
       assign(".SmodeTmp", options(error=dump.calls), where=0, i=T)
       on.exit({options(.SmodeTmp); remove(".SmodeTmp",where=0)})
       dump(x, file=name)
   },
       where = 0)
assign(".SmodePaths",
       function()
       ## the paths for the directories in the search list
   {
       temp <- search()
       value <- character(length(temp))
       for(i in seq(along.with = temp)) {
           obj <- database.object(i)
           if(is.character(obj) && length(obj) == 1)
               value[[i]] <- obj
       }
       value
   },
       where = 0)
