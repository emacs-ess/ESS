## This is a dump of the S library used by S-mode with Version 4 of S.

".SmodeDump" <- 
function(x, name)
{
  ## dump function for S-mode
  assign(".SmodeTmp", options(error = dump.calls), where = 0, i = T)
  on.exit({
    options(.SmodeTmp)
    remove(".SmodeTmp", where = 0)
  }
  )
  dump(x, file = name)
}
".SmodeLoad" <- 
function(x)
{
  ## skeleton of a dump.calls interface, enough to keep s-mode
  ## informed that an error took place.
  pseudo <- function()
  cat(get.message(), "Dumped\n", sep = "", file = "|stderr")
  ## source function for S-mode
  assign(".SmodeTmp", options(error = pseudo), where = 0, i = T)
  on.exit({
    options(.SmodeTmp)
    remove(".SmodeTmp", where = 0)
  }
  )
  invisible(source(x))
}
".SmodeObs" <- 
## S-mode session functions.  
## assign(..., where=0) makes them last only for the current session.
function(where, pattern)
{
  if(pattern == "")
    objects(where)
  else objects(where, pattern)
}
".SmodePaths" <- 
function()
{
  ## the paths for the directories in the search list
  temp <- search()
  value <- character(length(temp))
  for(i in seq(along.with = temp)) {
    obj <- database.object(i)
    if(is.character(obj) && length(obj) == 1)
      value[[i]] <- obj
  }
  value
}
"smode.lvsave" <- 
c(".Last.value", ".SmodeDump", ".SmodeLoad", ".SmodeObs", ".SmodePaths", 
  "smode.lvsave")
