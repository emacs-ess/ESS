require(methods)

#############################
##   A.  FCSmetadata class
#############################
##  Methods include:
##   1.  show
##   2.  print
##   3.  summary
##   4.  [
##   5.  [ <-
##   6.  [[
##   7.  [[ <-
######################################
##  Examples: 
##  1/28/2004: all work
######################################

## Contains critical metadata at top level, rest subseted.
setClass("FCSmetadata",
         representation(mode="character",
                        size="numeric",
                        nparam="numeric",
                        shortnames="vector",
                        longnames="vector",
                        paramranges="vector",
                        filename="character",
                        objectname="character",
                        original="logical",
                        fcsinfo="list"), ## holds misc params from fcs file
         prototype=list(mode="",
           size=0,
           nparam=0,
           shortnames=vector(mode="character"),
           longnames=vector(mode="character"),
           paramranges=vector(mode="numeric"),
           filename="None",
           objectname="None",
           original=TRUE,
           fcsinfo=list()))



setMethod("show",
          signature(object="FCSmetadata"),
          function(object) {
            
            if (object@original){
              obj.size <- object@size
              obj.npar <- object@nparam
              orig.flag <- "original"
            } else {
              obj.size <- object@fcsinfo[["RFACSadd>>$TOT"]]
              obj.npar <- object@fcsinfo[["RFACSadd>>$PAR"]]
              orig.flag <- "non-original"
            }

            cat("FACSmetadata for",orig.flag, "FCS object:", object@objectname,
                  "from original file",object@filename,"\n",
                  "with ",obj.size,"cells and",
                  obj.npar,"parameters.\n")
            invisible(object)
          })


 
setMethod("print",
          signature(x="FCSmetadata"),
          function(x) {
            show(x)## do stuff
          })


setMethod("summary",
          signature(object="FCSmetadata"),
          function(object) {
            ##show(object)## do stuff
            str(object)
          })

## JYW: I put in the single brackets because the
## name of the slot is preserved in the output

## JYW: I also opted to put 

## NOTE ABOUT THE ORIGINAL FLAG:
## the original flag only suggests that the
## data has been changed, NOT the metadata
## the original flag allows the user to
## extract and change the correct metadata
## (ie, the RFACSadd>> variables are changed when
## the original is FALSE or the original variables
## are themselves changes when the original flag
## is false.


## if original==FALSE
## must include the following parameters in the metadata:
## RFACSadd>>$TOT (vector of number of rows)
## RFACSadd>>$PAR (vector of number of columns)
## RFACSadd>>$PnS (vector of longnames)
## RFACSadd>>$PnN (vector of shortnames)
## RFACSadd>>$PnR (vector of ranges)

## original flag is ONLY changed when the data is changed

## if only part of the numeric index is valid, then only part
## of the fcsinfo list is output with warnings

setMethod("[",
          signature(x="FCSmetadata"),
          function(x,i,j,...,drop) {
            ## "i" can ONLY be EITHER a single character input
            ## OR a vector of numeric values
            ## returns only a single slot if i=character
            ## returns list elements in "fcsinfo" if i=numeric
            
            if ( !is.character(i) & !is.numeric(i)){
              ## i is neither numeric nor character
              stop("Input index is neither character nor numeric.")
            } else {
              if (is.character(i)){
                ## try to find it in the metadata ...
                ## JYW: I opted not to do the nested if statements
                ## because they were hard to read for me...
                
                ## are there any slotnames?
                
                ## are there any fcsinfo slotnames?
                if (length(i)>1){
                  stop("Only single entry for indexing by character slot name allowed.")
                }
                
                ##  original flag... needs work
                
                if ( sum(i %in% slotNames(x))==1 ){
                  ## need original checks for
                  ## size, nparam, longnames, shortnames
                  if (x@original==FALSE){
                    if ( !(i %in% c("size", "nparam",
                                    "longnames", "shortnames",
                                    "paramranges"))){
                      return(slot(x,i))
                    } else { ## depends on the false original flag
                      if (i == "size"){
                        return(x@fcsinfo[["RFACSadd>>$TOT"]])
                      }
                      if ( i == "nparam"){
                        return(x@fcsinfo[["RFACSadd>>$PAR"]])
                      }
                      if (i == "longnames"){
                        return(x@fcsinfo[["RFACSadd>>$PnS"]])
                      }
                      if (i=="shortnames"){
                        return(x@fcsinfo[["RFACSadd>>$PnN"]])
                      }
                      
                      if (i=="paramranges"){
                        return(x@fcsinfo[["RFACSadd>>$PnR"]])
                      } 
                      
                    }
                  } else { ## x@original is TRUE
                    return(slot(x, i))
                  }
                } else {

                  if ( sum(i %in% names(x@fcsinfo))==1) {
                    return(x@fcsinfo[[which(names(x@fcsinfo)==i)]])
                  } else {
                    if (sum(i %in% c("$PnS", 
                                     unlist(strsplit(paste("$P",
                                                           1:x@nparam,
                                                           "S", sep="", collapse=","),
                                                     split=","))))==length(i)){
                      pos.index <- c("$PnS",
                                     unlist(strsplit(paste("$P", 1:x@nparam,
                                                           "S", sep="", collapse=","),
                                                     split=","))) %in% i
                      if (x@original==TRUE){
                        if (pos.index[1]==TRUE){
                          
                          return(x@longnames)
                          
                        } else {
                          return(x@longnames[(pos.index[-1])])
                        }
                      } else { ## x@original == FALSE
                        if (pos.index[1]==TRUE){
                        
                          return(x@fcsinfo[["RFACSadd>>$PnS"]])
                          
                        } else {
                          return(x@fcsinfo[["RFACSadd>>$PnS"]][(pos.index[-1])])
                        }
                      }
                    
                    } else {
                      if (sum(i %in% c("$PnN", 
                                       unlist(strsplit(paste("$P",
                                                             1:x@nparam,
                                                             "N", sep="", collapse=","),
                                                       split=","))))==length(i)){
                        pos.index <- c("$PnN", unlist(strsplit(paste("$P", 1:x@nparam,
                                                                     "N", sep="", collapse=","),
                                                               split=","))) %in% i
                        if (x@original==TRUE){
                          if (pos.index[1]==TRUE){
                            return(x@shortnames)
                          } else {
                            return(x@shortnames[(pos.index[-1])])
                          }
                        } else{ ## if (x@original==FALSE)
                          if (pos.index[1]==TRUE){
                            return(x@fcsinfo[["RFACSadd>>$PnN"]])
                          } else {
                            return(x@fcsinfo[["RFACSadd>>$PnN"]][(pos.index[-1])])
                          }
                        }
                      } else {
                        if (sum(i %in% c("$PnR", 
                                         unlist(strsplit(paste("$P",
                                                               1:x@nparam,
                                                               "R", sep="", collapse=","),
                                                         split=","))))==length(i)){
                          pos.index <- c("$PnR", unlist(strsplit(paste("$P", 1:x@nparam,
                                                                       "R", sep="", collapse=","),
                                                                 split=","))) %in% i
                          if (x@original==TRUE){
                            if (pos.index[1]==TRUE){
                              return(x@paramranges)
                            } else {
                              return(x@paramranges[(pos.index[-1])])
                            }
                          } else {## if (x@original==FALSE)
                            if (pos.index[1]==TRUE){
                              return(x@fcsinfo[["RFACSadd>>$PnR"]])
                            } else {
                              return(x@fcsinfo[["RFACSadd>>$PnR"]][(pos.index[-1])])
                            }
                          }
                        } else {
                          if (!(i %in% c("$MODE", "$TOT", "$PAR"))){
                            warning("The Slot Name cannot be found in the metadata.")
                            return(NULL)
                          } else {
                            if (i=="$MODE"){
                              return(x@mode)
                            }
                            if (i=="$TOT"){
                              if (x@original==TRUE){
                                return(x@size)
                              } else {
                                return(x@fcsinfo[["RFACSadd>>$TOT"]])
                              }
                            }
                            if (i=="$PAR"){
                              if (x@original==TRUE){
                                return(x@nparam)
                              } else {
                                return(x@fcsinfo[["RFACSadd>>$PAR"]])
                              }
                            }
                          }
                        
                        
                        }
                      }
                    }
                  }
                }
              } ## is.character(i)

              if (is.numeric(i)){
                ## JYW: will do it the long way, without x@metadata[[i]]
                ## will only return value if all of the indices are found in the fcsinfo
                
                index.i <- i %in% 1:length(x@fcsinfo)
                
                if (sum(index.i==1) !=length(i)){
                  warning("Part or all of the Index cannot be found in the metadata.")
                  if (sum(index.i==1)==0){
                    return(NULL)
                  }
                }
                return(x@fcsinfo[i[index.i==1]])
              }
            }
              
          
          })


## JYW: I put in the single brackets because the
## name of the slot is preserved in the output

setReplaceMethod("[",
          signature(x="FCSmetadata"),
                 function(x,i,j,...,value) {
                   ## "i" can ONLY be EITHER a single character input OR a vector of numeric values
                   ## returns only a single slot if i=character
                   ## returns list elements in "fcsinfo" if i=numeric
                   
                   if ( !is.character(i) & !is.numeric(i)){
                     ## i is neither numeric nor character
                     stop("Input index is neither character nor numeric.")
                   } else {
                     if (is.character(i)){
                       ## try to find it in the metadata ...
                       ## JYW: I opted not to do the nested if statements
                       ## because they were hard to read for me...
                       
                       ## are there any slotnames?
                       
                       ## are there any fcsinfo slotnames?
                       if (length(i)>1){
                         stop("Only single entry for indexing by character slot name allowed.")
                       }
                
                       ##  original flag... needs work
                
                       if ( sum(i %in% slotNames(x))==1 ){
                         ## need original checks for
                         ## size, nparam, longnames, shortnames
                         if (x@original==FALSE){
                           if ( !(i %in% c("size", "nparam",
                                           "longnames", "shortnames",
                                           "paramranges"))){
                             slot(x,i) <- value
                           } else { ## depends on the false original flag
                             if (i == "size"){
                               x@fcsinfo[["RFACSadd>>$TOT"]] <- value
                             }
                             if ( i == "nparam"){
                               x@fcsinfo[["RFACSadd>>$PAR"]] <- value
                             }
                             if (i == "longnames"){
                               x@fcsinfo[["RFACSadd>>$PnS"]] <- value
                             }
                             if (i=="shortnames"){
                               x@fcsinfo[["RFACSadd>>$PnN"]] <- value
                             }
                      
                             if (i=="paramranges"){
                               x@fcsinfo[["RFACSadd>>$PnR"]] <- value
                             } 
                             
                           }
                         } else { ## x@original is TRUE
                           slot(x, i) <- value
                         }
                       } else {

                         if ( sum(i %in% names(x@fcsinfo))==1) {
                           x@fcsinfo[[which(names(x@fcsinfo)==i)]] <- value
                         } else {
                           if (sum(i %in% c("$PnS", 
                                            unlist(strsplit(paste("$P",
                                                                  1:x@nparam,
                                                                  "S", sep="", collapse=","),
                                                            split=","))))==length(i)){
                             pos.index <- c("$PnS",
                                            unlist(strsplit(paste("$P", 1:x@nparam,
                                                                  "S", sep="", collapse=","),
                                                            split=","))) %in% i
                             if (x@original==TRUE){
                               if (pos.index[1]==TRUE){
                                 
                                 x@longnames <- value
                          
                               } else {
                                 x@longnames[(pos.index[-1])] <- value
                               }
                             } else { ## x@original == FALSE
                               if (pos.index[1]==TRUE){
                        
                                 x@fcsinfo[["RFACSadd>>$PnS"]] <- value
                          
                               } else {
                                 x@fcsinfo[["RFACSadd>>$PnS"]][(pos.index[-1])] <- value
                               }
                             }
                    
                           } else {
                             if (sum(i %in% c("$PnN", 
                                              unlist(strsplit(paste("$P",
                                                                    1:x@nparam,
                                                                    "N", sep="", collapse=","),
                                                              split=","))))==length(i)){
                               pos.index <- c("$PnN", unlist(strsplit(paste("$P", 1:x@nparam,
                                                                            "N", sep="", collapse=","),
                                                                      split=","))) %in% i
                               if (x@original==TRUE){
                                 if (pos.index[1]==TRUE){
                                   x@shortnames <- value
                                 } else {
                                   x@shortnames[(pos.index[-1])] <- value
                                 }
                               } else{ ## if (x@original==FALSE)
                                 if (pos.index[1]==TRUE){
                                   x@fcsinfo[["RFACSadd>>$PnN"]] <- value
                                 } else {
                                   x@fcsinfo[["RFACSadd>>$PnN"]][(pos.index[-1])] <- value
                                 }
                               }
                             } else {
                               if (sum(i %in% c("$PnR", 
                                                unlist(strsplit(paste("$P",
                                                                      1:x@nparam,
                                                                      "R", sep="", collapse=","),
                                                                split=","))))==length(i)){
                                 pos.index <- c("$PnR", unlist(strsplit(paste("$P", 1:x@nparam,
                                                                              "R", sep="", collapse=","),
                                                                        split=","))) %in% i
                                 if (x@original==TRUE){
                                   if (pos.index[1]==TRUE){
                                     x@paramranges <- value
                                   } else {
                                     x@paramranges[(pos.index[-1])] <- value
                                   }
                                 } else {## if (x@original==FALSE)
                                   if (pos.index[1]==TRUE){
                                     x@fcsinfo[["RFACSadd>>$PnR"]] <- value
                                   } else {
                                     x@fcsinfo[["RFACSadd>>$PnR"]][(pos.index[-1])] <- value
                                   }
                                 }
                               } else {
                                 if (!(i %in% c("$MODE", "$TOT", "$PAR"))){
                                   ## cannot be found, so we make a new slot
                                   len <- length(x@fcsinfo)
                                   x@fcsinfo[[len+1]] <- value
                                   ## we do not have the RFACSadd>> prefix when just
                                   ## changing/adding on to the metadata
                                   names(x@fcsinfo)[len+1] <- i ##paste("RFACSadd>>", i, sep="")
                       
                                  
                                 } else {
                                   if (i=="$MODE"){
                                     x@mode <- value
                                   }
                                   if (i=="$TOT"){
                                     if (x@original==TRUE){
                                       x@size <- value
                                     } else {
                                       x@fcsinfo[["RFACSadd>>$TOT"]] <- value
                                     }
                                   }
                                   if (i=="$PAR"){
                                     if (x@original==TRUE){
                                       x@nparam <- value
                                     } else {
                                       x@fcsinfo[["RFACSadd>>$PAR"]] <- value
                                     }
                                   }
                                 }
                                 
                                 
                               }
                             }
                           }
                         }
                       }
                     } ## is.character(i)

                     if (is.numeric(i)){
                       ## JYW: will do it the long way, without x@metadata[[i]]
                       ## will only return value if all of the indices are found in the fcsinfo
                       
                       index.i <- i %in% 1:length(x@fcsinfo)
                       
                       if (sum(index.i==1) !=length(i)){
                         warning("Part or all of the Index cannot be found in the metadata.")
                         
                       }
                       x@fcsinfo[i[index.i==1]] <- value
                     }
                   }
                   
                   
                   x
                 })


## JYW: I put in the single brackets because the
## name of the slot is preserved in the output

## JYW: I also opted to put 
setMethod("[[",
          signature(x="FCSmetadata"),
          function(x,i,j,...,drop) {
            ## "i" can ONLY be EITHER a single character input OR a vector of numeric values
            ## returns only a single slot if i=character
            ## returns list elements in "fcsinfo" if i=numeric
            if ( !is.character(i) & !is.numeric(i)){
              ## i is neither numeric nor character
              stop("Input index is neither character nor numeric.")
            } else {
              if (is.character(i)){
                ## try to find it in the metadata ...
                ## JYW: I opted not to do the nested if statements
                ## because they were hard to read for me...
                
                ## are there any slotnames?
                
                ## are there any fcsinfo slotnames?
                if (length(i)>1){
                  stop("Only single entry for indexing by character slot name allowed.")
                }
                
                ##  original flag... needs work
                
                if ( sum(i %in% slotNames(x))==1 ){
                  ## need original checks for
                  ## size, nparam, longnames, shortnames
                  if (x@original==FALSE){
                    if ( !(i %in% c("size", "nparam",
                                    "longnames", "shortnames",
                                    "paramranges"))){
                      return(slot(x,i))
                    } else { ## depends on the false original flag
                      if (i == "size"){
                        return(x@fcsinfo[["RFACSadd>>$TOT"]])
                      }
                      if ( i == "nparam"){
                        return(x@fcsinfo[["RFACSadd>>$PAR"]])
                      }
                      if (i == "longnames"){
                        return(x@fcsinfo[["RFACSadd>>$PnS"]])
                      }
                      if (i=="shortnames"){
                        return(x@fcsinfo[["RFACSadd>>$PnN"]])
                      }
                      
                      if (i=="paramranges"){
                        return(x@fcsinfo[["RFACSadd>>$PnR"]])
                      } 
                      
                    }
                  } else { ## x@original is TRUE
                    return(slot(x, i))
                  }
                } else {

                  if ( sum(i %in% names(x@fcsinfo))==1) {
                    return(x@fcsinfo[[which(names(x@fcsinfo)==i)]])
                  } else {
                    if (sum(i %in% c("$PnS", 
                                     unlist(strsplit(paste("$P",
                                                           1:x@nparam,
                                                           "S", sep="", collapse=","),
                                                     split=","))))==length(i)){
                      pos.index <- c("$PnS",
                                     unlist(strsplit(paste("$P", 1:x@nparam,
                                                           "S", sep="", collapse=","),
                                                     split=","))) %in% i
                      if (x@original==TRUE){
                        if (pos.index[1]==TRUE){
                          
                          return(x@longnames)
                          
                        } else {
                          return(x@longnames[(pos.index[-1])])
                        }
                      } else { ## x@original == FALSE
                        if (pos.index[1]==TRUE){
                        
                          return(x@fcsinfo[["RFACSadd>>$PnS"]])
                          
                        } else {
                          return(x@fcsinfo[["RFACSadd>>$PnS"]][(pos.index[-1])])
                        }
                      }
                    
                    } else {
                      if (sum(i %in% c("$PnN", 
                                       unlist(strsplit(paste("$P",
                                                             1:x@nparam,
                                                             "N", sep="", collapse=","),
                                                       split=","))))==length(i)){
                        pos.index <- c("$PnN", unlist(strsplit(paste("$P", 1:x@nparam,
                                                                     "N", sep="", collapse=","),
                                                               split=","))) %in% i
                        if (x@original==TRUE){
                          if (pos.index[1]==TRUE){
                            return(x@shortnames)
                          } else {
                            return(x@shortnames[(pos.index[-1])])
                          }
                        } else{ ## if (x@original==FALSE)
                          if (pos.index[1]==TRUE){
                            return(x@fcsinfo[["RFACSadd>>$PnN"]])
                          } else {
                            return(x@fcsinfo[["RFACSadd>>$PnN"]][(pos.index[-1])])
                          }
                        }
                      } else {
                        if (sum(i %in% c("$PnR", 
                                         unlist(strsplit(paste("$P",
                                                               1:x@nparam,
                                                               "R", sep="", collapse=","),
                                                         split=","))))==length(i)){
                          pos.index <- c("$PnR", unlist(strsplit(paste("$P", 1:x@nparam,
                                                                       "R", sep="", collapse=","),
                                                                 split=","))) %in% i
                          if (x@original==TRUE){
                            if (pos.index[1]==TRUE){
                              return(x@paramranges)
                            } else {
                              return(x@paramranges[(pos.index[-1])])
                            }
                          } else {## if (x@original==FALSE)
                            if (pos.index[1]==TRUE){
                              return(x@fcsinfo[["RFACSadd>>$PnR"]])
                            } else {
                              return(x@fcsinfo[["RFACSadd>>$PnR"]][(pos.index[-1])])
                            }
                          }
                        } else {
                          if (!(i %in% c("$MODE", "$TOT", "$PAR"))){
                            warning("The Slot Name cannot be found in the metadata.")
                            return(NULL)
                          } else {
                            if (i=="$MODE"){
                              return(x@mode)
                            }
                            if (i=="$TOT"){
                              if (x@original==TRUE){
                                return(x@size)
                              } else {
                                return(x@fcsinfo[["RFACSadd>>$TOT"]])
                              }
                            }
                            if (i=="$PAR"){
                              if (x@original==TRUE){
                                return(x@nparam)
                              } else {
                                return(x@fcsinfo[["RFACSadd>>$PAR"]])
                              }
                            }
                          }
                        
                        
                        }
                      }
                    }
                  }
                }
              } ## is.character(i)

              if (is.numeric(i)){
                ## JYW: will do it the long way, without x@metadata[[i]]
                ## will only return value if all of the indices are found in the fcsinfo
                
                index.i <- i %in% 1:length(x@fcsinfo)
                
                if (sum(index.i==1) != length(i)){
                  warning("Part or all of the Index cannot be found in the metadata.")
                  if (sum(index.i==1)==0){
                    return(NULL)
                  }
                }
                return(x@fcsinfo[i[index.i==1]])
              }
            }
              
           
          })
           
### LIST OF KEY METADATA WORDS/slotnames
## NOTE: (single value of length 1 is returned unless otherwise noted in ())
## 1.  mode, $MODE
## 2.  size, $TOT
## 3.  nparam, $PAR
## 4.  shortnames (vector), $PnN (vector), $P1N, $P2N, ...
## 5.  longnames (vector), $PnS(vector), $P1S, $P2S, ...
## 6.  paramranges (vector), $PnR (vector), $P1R, $P2R
## 7.  filename
## 8.  objectname
## 9.  original
## 10. fcsinfo (includes RFACSadd>> when the data
##          is changed resulting in changes with  #2-6 (above))
## 11. any other name will be added as a new slot

setReplaceMethod("[[",
                 signature(x="FCSmetadata"),
                 function(x,i,j,...,value) {
                   ## "i" can ONLY be EITHER a single character input OR a vector of numeric values
                   ## returns only a single slot if i=character
                   ## returns list elements in "fcsinfo" if i=numeric
                   if ( !is.character(i) & !is.numeric(i)){
                     ## i is neither numeric nor character
                     stop("Input index is neither character nor numeric.")
                   } else {
                     if (is.character(i)){
                       ## try to find it in the metadata ...
                       ## JYW: I opted not to do the nested if statements
                       ## because they were hard to read for me...
                       
                       ## are there any slotnames?
                       
                       ## are there any fcsinfo slotnames?
                       if (length(i)>1){
                         stop("Only single entry for indexing by character slot name allowed.")
                       }
                
                       ##  original flag... needs work
                
                       if ( sum(i %in% slotNames(x))==1 ){
                         ## need original checks for
                         ## size, nparam, longnames, shortnames
                         if (x@original==FALSE){
                           if ( !(i %in% c("size", "nparam",
                                           "longnames", "shortnames",
                                           "paramranges"))){
                             slot(x,i) <- value
                           } else { ## depends on the false original flag
                             if (i == "size"){
                               x@fcsinfo[["RFACSadd>>$TOT"]] <- value
                             }
                             if ( i == "nparam"){
                               x@fcsinfo[["RFACSadd>>$PAR"]] <- value
                             }
                             if (i == "longnames"){
                               x@fcsinfo[["RFACSadd>>$PnS"]] <- value
                             }
                             if (i=="shortnames"){
                               x@fcsinfo[["RFACSadd>>$PnN"]] <- value
                             }
                      
                             if (i=="paramranges"){
                               x@fcsinfo[["RFACSadd>>$PnR"]] <- value
                             } 
                             
                           }
                         } else { ## x@original is TRUE
                           slot(x, i) <- value
                         }
                       } else {

                         if ( sum(i %in% names(x@fcsinfo))==1) {
                           x@fcsinfo[[which(names(x@fcsinfo)==i)]] <- value
                         } else {
                           if (sum(i %in% c("$PnS", 
                                            unlist(strsplit(paste("$P",
                                                                  1:x@nparam,
                                                                  "S", sep="", collapse=","),
                                                            split=","))))==length(i)){
                             pos.index <- c("$PnS",
                                            unlist(strsplit(paste("$P", 1:x@nparam,
                                                                  "S", sep="", collapse=","),
                                                            split=","))) %in% i
                             if (x@original==TRUE){
                               if (pos.index[1]==TRUE){
                                 
                                 x@longnames <- value
                          
                               } else {
                                 x@longnames[(pos.index[-1])] <- value
                               }
                             } else { ## x@original == FALSE
                               if (pos.index[1]==TRUE){
                        
                                 x@fcsinfo[["RFACSadd>>$PnS"]] <- value
                          
                               } else {
                                 x@fcsinfo[["RFACSadd>>$PnS"]][(pos.index[-1])] <- value
                               }
                             }
                    
                           } else {
                             if (sum(i %in% c("$PnN", 
                                              unlist(strsplit(paste("$P",
                                                                    1:x@nparam,
                                                                    "N", sep="", collapse=","),
                                                              split=","))))==length(i)){
                               pos.index <- c("$PnN", unlist(strsplit(paste("$P", 1:x@nparam,
                                                                            "N", sep="", collapse=","),
                                                                      split=","))) %in% i
                               if (x@original==TRUE){
                                 if (pos.index[1]==TRUE){
                                   x@shortnames <- value
                                 } else {
                                   x@shortnames[(pos.index[-1])] <- value
                                 }
                               } else{ ## if (x@original==FALSE)
                                 if (pos.index[1]==TRUE){
                                   x@fcsinfo[["RFACSadd>>$PnN"]] <- value
                                 } else {
                                   x@fcsinfo[["RFACSadd>>$PnN"]][(pos.index[-1])] <- value
                                 }
                               }
                             } else {
                               if (sum(i %in% c("$PnR", 
                                                unlist(strsplit(paste("$P",
                                                                      1:x@nparam,
                                                                      "R", sep="", collapse=","),
                                                                split=","))))==length(i)){
                                 pos.index <- c("$PnR", unlist(strsplit(paste("$P", 1:x@nparam,
                                                                              "R", sep="", collapse=","),
                                                                        split=","))) %in% i
                                 if (x@original==TRUE){
                                   if (pos.index[1]==TRUE){
                                     x@paramranges <- value
                                   } else {
                                     x@paramranges[(pos.index[-1])] <- value
                                   }
                                 } else {## if (x@original==FALSE)
                                   if (pos.index[1]==TRUE){
                                     x@fcsinfo[["RFACSadd>>$PnR"]] <- value
                                   } else {
                                     x@fcsinfo[["RFACSadd>>$PnR"]][(pos.index[-1])] <- value
                                   }
                                 }
                               } else {
                                 if (!(i %in% c("$MODE", "$TOT", "$PAR"))){
                                   ## cannot be found, so we make a new slot
                                   len <- length(x@fcsinfo)
                                   x@fcsinfo[[len+1]] <- value
                                   ## we do not have the RFACSadd>> prefix when just
                                   ## changing/adding on to the metadata
                                   names(x@fcsinfo)[len+1] <- i ##paste("RFACSadd>>", i, sep="")
                       
                                  
                                 } else {
                                   if (i=="$MODE"){
                                     x@mode <- value
                                   }
                                   if (i=="$TOT"){
                                     if (x@original==TRUE){
                                       x@size <- value
                                     } else {
                                       x@fcsinfo[["RFACSadd>>$TOT"]] <- value
                                     }
                                   }
                                   if (i=="$PAR"){
                                     if (x@original==TRUE){
                                       x@nparam <- value
                                     } else {
                                       x@fcsinfo[["RFACSadd>>$PAR"]] <- value
                                     }
                                   }
                                 }
                                 
                                 
                               }
                             }
                           }
                         }
                       }
                     } ## is.character(i)

                     if (is.numeric(i)){
                       ## JYW: will do it the long way, without x@metadata[[i]]
                       ## will only return value if all of the indices are found in the fcsinfo
                       
                       index.i <- i %in% 1:length(x@fcsinfo)
                       
                       if (sum(index.i==1) !=length(i)){
                         warning("Part or all of the Index cannot be found in the metadata.")
                         
                       }
                       x@fcsinfo[i[index.i==1]] <- value
                     }
                   }
                   
                   
                   x
                 
                 
                 })
## NOTE: [[<- for FCSmetdata
## can replace via numeric index with a value that is a list or vector
## ie.  ex.s4@metadata[[c(1,100000)]]<-c("g", "wrong")  or list("g", "wrong")


####################################
##  FCS Class
####################################
##  Methods include:
## 1.  as.matrix(FCS)
## 2.  as.data.frame(FCS)
## 3.  as.FCS(matrix)
## 4.  as.FCS(data.frame)
## 5.  dim.FCS(FCS)
## 6.  show
## 7.  print
## 8.  plot
## 9.  summary
## 10. initialize
## 11. metaData
## 12. fluors
## 13. [  extracts data
## 14. [ <- replaces data
## 15. [[ extracts metadata
## 16. [[ <- replaces the metadata
## 17. is.FCS
## 18. addParameter
## 19. checkvars
## 20. fixvars
## 21. equals (are two FCS objects the same)
###################################

####################################
##  Examples have been checked?

###
### Data from an FCS file
###

setClass("FCS",
         representation(data="matrix", # flour data
                        metadata="FCSmetadata"),
         prototype=list(data=matrix(),
           metadata=new("FCSmetadata")))



setAs(from="FCS",to="matrix", ## as.matrix(FCS)
      def=function(from) {
        ## createFCSnamesFromMetadata(x)
        ## colnames(x..) <- x@metat
        from@data
      })


setAs(from="FCS",to="data.frame",
      def=function(from) {
        ## createFCSnamesFromMetadata(x)
        ## colnames(x..) <- x@metat
        as.data.frame(from@data)
      })


setAs(from="matrix",to="FCS",
      def=function(from) {
        ## createFCSnamesFromMetadata(x)
        ## colnames(x..) <- x@metat
        new("FCS",data=from)
      })


setAs(from="data.frame",to="FCS",
      def=function(from) {
        ## createFCSnamesFromMetadata(x)
        ## colnames(x..) <- x@metat
        new("FCS",data=as.matrix(from))
      })

setGeneric("dim.FCS",
             function(object) {
               standardGeneric("dim.FCS")
             })

                       
setMethod("dim.FCS",
          signature(object="FCS"),
         function(object) {
            dim(object@data)
          })



setMethod("show",
          signature(object="FCS"),
          function(object) {
            object
            ## JYW: the is.list(object) does not
            ## correctly identify it being a S3 object
            
            ##     if (!is.list(object)) {
            orig.stat <- ifelse(object@metadata@original==TRUE,
                                "Original", "Non-original")
            FourSpace <- "    "
            cat(FourSpace, orig.stat,
                "Object of class `FCS' from:",
                (object@metadata)@filename,"\n")
            cat(FourSpace,
                "Object name:",
                (object@metadata)@objectname,"\n")
            
            if (length(as.vector(object@data))==0){
              cat(FourSpace,
                  "Dimensions","0",
                  "by","0","\n")
            } else if (length(as.vector(object@data))==1){
              if (!is.na(object@data) ){
                cat(FourSpace,
                    "Dimensions",dim(object@data)[1],
                    "by",dim(object@data)[2],"\n")
              } else {
                cat(FourSpace,
                    "Dimensions","0",
                    "by","0","\n")
              }
            } else {
              cat(FourSpace,
                  "Dimensions",dim(object@data)[1],
                  "by",dim(object@data)[2],"\n")
            }
            ##    } else {
            ## S3 class...
            ##        print3.FCS(object)
            ##     }
          })

setMethod("print",
          signature(x="FCS"),
          function(x) {
            show(x)
          })


setMethod("plot",
          signature(x="FCS",y="missing"),
          function(x,image.parallel.plot=FALSE, joint=TRUE,...) {
            ## default is pairs plotting but should also be able to do parallel coordinates plotting
            
            ##  variable           decription
            ##-----------------------------------
            ## x                 FCS object
            ##
            ## image parallel.plot     boolean; if true the image parallel coordinates
            ##                   plot will be implemented instead of default
            ##                   pairs plot; default value of FALSE
            ## joint       boolean; if image.parallel.plot is TRUE, then this boolean establishes
            ##             if the image parallel coordinates plot is joint or not
            ## ...              more options for pairs.CSP, when parallel.plot=FALSE
            
            if (length(as.vector(x@data))==0){
              stop("There is no data to plot")
            } else if (length(as.vector(x@data))==1) {
              if (is.na(x@data)){
                stop("There is no data to plot")
              }
            }
            
            if (image.parallel.plot==TRUE){
                if (joint==FALSE){
                    ImageParCoord(x@data, ...)
                }
                else {
                    JointImageParCoord(x@data, ...)
                }
            }
            else{
                pairs.CSP(x@data, ...)
            }
        })

setClass("FCSsummary",
         representation(num.cells="numeric",
                        num.param="numeric",
                        univariate.stat="matrix",
                        metadata.info="list"),
         prototype=list(num.cells=0,
           num.param=0, univariate.stat=matrix(),
           metadata.info=list()))

setMethod("show",
          signature(object="FCSsummary"),
          function(object){
            cat("\n",
                "I. Data reports:",
                "\n\n")
            cat(paste("   A. Dimension Check: Dimensions: (row X col):",
                      object@num.cells, "X",
                      object@num.param,
                      "\n",
                      sep=" "))
            
            cat(paste("\n",
                      "   B. Data Column Names & Univariate Summary:",
                      "\n",
                      sep=""))
            cat("    Using Tukey's method for the five number summary", "\n")
            print(object@univariate.stat)
            cat("\n")
          
        
            cat("\n",
                "II. Metadata Variable/Slot reports:",
                "\n")
            
            cat("   A. Metadata Slots:",
                "\n")
           
            
            print(object@metadata.info$Description)
            cat("\n")
           
            print(object@metadata.info$ColumnParametersSummary)
            cat("\n")
            
            cat("   B. Metadata 'fcsinfo' slot length=",
               length(object@metadata.info$fcsinfoNames),
                " & slot names: \n\n")
            
         
            print(object@metadata.info$fcsinfoNames)

           
            cat("\n")
           
          })

setMethod("print",
          signature(x="FCSsummary"),
          function(x){
            show(x)

          })


setMethod("summary",
          signature(object="FCS"),
          function(object) {
            
            cat("\n")
            cat("I. Data reports:\n\n")
            if (length(as.vector(object@data))==0){
              cat("No Data", "\n\n")
              ranges.data <- NULL
              n.col <- n.row <- 0
            } else {
              if (length(as.vector(object@data))==1) {
                if (is.na(object@data)){
                  cat("No Data", "\n")
                  ranges.data <- NULL
                  n.col <- n.row <- 0
                } else {
                  cat("Single data value=",
                      as(object@data, "matrix"),
                      "\n\n")
                  ranges.data <- range(object@data)
                  n.col <- n.row <- 1
                }
              } else {
                n.row <- dim.FCS(object)[1]
                n.col <- dim.FCS(object)[2]
                
                cat(paste("   A. Dimension Check: Dimensions: (row X col):",
                          n.row, "X",
                          n.col,
                          "\n",
                          sep=" "))
                
                cat(paste("\n",
                          "   B. Data Column Names & Univariate Summary:",
                          "\n",
                          sep=""))
                cat("    Using Tukey's method for the five number summary",
                   "\n")
                ranges.data <- t(apply(object@data, 2,
                                       function(x) {
                                         round(c(fivenum(x),
                                                 mean(x),
                                                 sd(x)), 3)
                                       }))
                
                ranges.data <- cbind(1:length(ranges.data[,1]),
                                     ranges.data)
                colnames(ranges.data) <- c("column", "min",
                                           "lower-hinge", "median",
                                           "upper-hinge", "max", "mean", "sd")
                print(ranges.data)
                cat("\n")
              }
            }
            cat("\n",
                "II. Metadata Variable/Slot reports:",
                "\n")
            
            cat("   A. Metadata Slots:",
                "\n")
            slotnames <- c("mode",
                           "size/$TOT",
                           "nparam/$PAR",
                           "shortnames/$PnN",
                       "longnames/$PnS",
                           "paramranges/$PnR",
                           "filename",
                       "objectname",
                           "original",
                           "fcsinfo")
            description <- c("Mode",
                             "number of cells/rows",
                             "number of column params",
                             "Shortnames of column parameters",
                             "Longnames of column parameters",
                             "Ranges/max of column parameters",
                             "original FCS filename",
                             "name of current object",
                             "current object original status",
                             "misc. metadata info")
            
            values <- c(object[["mode"]],
                        object[["size"]],
                        object[["nparam"]],
                        "see below",
                        "see below",
                        "see below",
                        object[["filename"]],
                        object[["objectname"]],
                        object[["original"]],
                        "see part II B.")
            tot.info <- data.frame(cbind(slotnames, description, values))
            print(tot.info)
            cat("\n")
            col.stuff <- cbind(object[["shortnames"]],
                               object[["longnames"]],
                               object[["paramranges"]])
            colnames(col.stuff) <- c("$PnN", "$PnS", "$PnR")
            col.stuff <- list("ColumnParametersSummary"=col.stuff)
            print(col.stuff)
            cat("\n")
            
            cat("   B. Metadata 'fcsinfo' slot length=",
                length(object@metadata@fcsinfo), " & slot names: \n\n")
            
           
            fcsinfo.stuff <- list("fcsinfoNames"=names(object[["fcsinfo"]]))
                              
          
            print(fcsinfo.stuff)

            if (is.null(ranges.data)){
              ranges.data <- matrix()
            }
            
            cat("\n")
            result <- new("FCSsummary", num.cells=n.row, num.param=n.col,
                           univariate.stat=ranges.data,
                           metadata.info=c("Description"=list(tot.info), col.stuff, fcsinfo.stuff))
            ## MIXING S3 and S4
            ## class(result) <- "summary.FCS"

            ## now there is no mixing of the S3 and S4 classes
            invisible(result)
          })

                                        #setMethod("initialize",
                                        #          signature(.Object="FCS"),
                                        #          function(.Object) {
                                        #            ## test
                                        #          })

setGeneric("metaData",
             function(x) {
               standardGeneric("metaData")
             })


setMethod("metaData",
          signature(x="FCS"),
          function(x) {
            x@metadata
          })

setGeneric("fluors",
             function(x) {
               standardGeneric("fluors")
             })


setMethod("fluors",
          signature(x="FCS"),
          function(x) {
            x@data
          })

## index data
setMethod("[",
          signature(x = "FCS"),
          function (x, i, j, ..., drop) {
            if(missing(j) ) {
              if( missing(i) ) { ## i, j is missing
                nexprs <- fluors(x)
               
              } else { ## i present, j missing
                nexprs <- fluors(x)[i, ,drop=FALSE]
               
              }
              pos <- 1:(dim.FCS(x)[2])
            } else {
              if( missing(i) ) { ## j is present
                nexprs <- fluors(x)[,j, drop=FALSE]
              } else {
                nexprs <- fluors(x)[i, j, drop=FALSE]
              }
              pos <- j
            }
            ## JYW: need to update the metadata with fixvars?
            x <- new("FCS",data=nexprs,metadata=metaData(x))
            ## the data has changed so we
            ## update the metadata
            
            x[["RFACSadd>>$TOT"]] <- dim.FCS(x)[1]
            x[["RFACSadd>>$PAR"]] <- dim.FCS(x)[2]
            ## must be able to check against data range that is NA, (work up example)
          
            x[["RFACSadd>>$PnR"]] <- unlist(apply(x@data, 2, max))
            x[["RFACSadd>>$PnS"]] <- x@metadata@longnames[pos]
            x[["RFACSadd>>$PnN"]] <- x@metadata@shortnames[pos]
            x[["original"]] <- FALSE
            x
          })


## index data.  NEEDS VERIFICATION
setReplaceMethod("[",
                 signature(x = "FCS"),
                 function (x, i, j, ..., value) {
                    ## the names do not change b/c we return the whole FCS R object
                   x[["RFACSadd>>$PnS"]] <- x@metadata@longnames
                   x[["RFACSadd>>$PnN"]] <- x@metadata@shortnames

                   nexprs <- fluors(x)

                   if(missing(j) ) {
                     if( missing(i) ) { ## i, j is missing
                       nexprs <- value  ## nexprs <- flours(x)
                     } else { ## i present, j missing
                       nexprs[i,] <- value ## nexprs <- flours(x)[i, ,drop=FALSE]
                     }
                     
                   } else {
                     if( missing(i) ) { ## j is present
                       nexprs[,j] <- value ## nexprs <- flours(x)[,j, drop=FALSE]
                     } else {
                       nexprs[i,j] <- value ## nexprs <- flours(x)[i, j, drop=FALSE]
                     }
                    
                   }
                   ## JYW: need to update the metadata with fixvars?
                   x <- new("FCS",data=nexprs,metadata=metaData(x))
                   
                   ## the data has changed so we
                   ## update the metadata
                   
                   x[["RFACSadd>>$TOT"]] <- dim.FCS(x)[1]
                   x[["RFACSadd>>$PAR"]] <- dim.FCS(x)[2]
                   ## must be able to check against data range that is NA, (work up example)
                   
                   x[["RFACSadd>>$PnR"]] <- unlist(apply(x@data, 2, max))
                  
                   x[["original"]] <- FALSE
                   
                   x
                 })



## JYW: I am doing this as the reiteration of the metadata extraction 
setMethod("[[",
          signature(x="FCS"),
          function(x,i,j,...,drop) {
            ## "i" can ONLY be EITHER a single character input OR a vector of numeric values
            ## returns only a single slot if i=character
            ## returns list elements in "fcsinfo" if i=numeric
            
            x@metadata[i]
           
          })




setReplaceMethod("[[",
          signature(x="FCS"),
          function(x,i,j,...,value) {
            ## "i" can ONLY be EITHER a single character input OR a vector of numeric values
            ## returns only a single slot if i=character
            ## returns list elements in "fcsinfo" if i=numeric
            
            x@metadata[i] <- value
            x
          })


#######
#######  METHODS by JYW (ie, first pass, may break)
#######

## use:
## is(FCSobject,"FCS")

## adds a column parameter to the data of the FCS function

setGeneric("addParameter",
             function(x, colvar, shortname="",
                      longname="", use.shortname=FALSE) {
               standardGeneric("addParameter")
             })


## if use.shortname is TRUE then the shortname
## will be concatenated to the original datanames

setMethod("addParameter",
          signature(x="FCS",
                    colvar="vector"),
          function(x, colvar,
                   shortname="",
                   longname="",
                   use.shortname=FALSE){
            if (length(colvar)!=dim.FCS(x)[1]){
              stop(paste("Input Parameter vector length",
                         length(colvar),
                         "does not correspond to FCS data column length",
                         dim.FCS(x)[1], sep=","))
            }
            if (length(as.vector(x@data))==0){
              x@data <- matrix(colvar, ncol=1)
            } else if (length(as.vector(x@data))==1){
              if (is.na(x@data)){
                x@data <- matrix(colvar, ncol=1)
              } else {
                x@data <- cbind(x@data, colvar)
              }                
            } else {
              ## update the data
              x@data <- cbind(x@data, colvar)
            }
            col.pos <- dim(x@data)[2]
            
            if (!use.shortname){
              colnames(x@data)[col.pos] <- longname
            } else {
              colnames(x@data)[col.pos] <- shortname
            }
            ## update the metadata
            
            x[["RFACSadd>>$TOT"]] <- dim.FCS(x)[1]
            x[["RFACSadd>>$PAR"]] <- dim.FCS(x)[2]
            ## must be able to check against data range that is NA, (work up example)
           
            range.colvar <- ifelse(is.numeric(colvar), max(as.numeric(colvar)), NA)
            x[["RFACSadd>>$PnR"]] <- c(x[["paramranges"]], range.colvar)
            x[["RFACSadd>>$PnS"]] <- c(x[["longnames"]], longname)
            x[["RFACSadd>>$PnN"]] <- c(x[["shortnames"]], shortname)
            x[["original"]] <- FALSE
            x
          })

          

setGeneric("checkvars",
             function(x,
                      MY.DEBUG=TRUE,
                      range.max=NULL) {
               standardGeneric("checkvars")
             })



setMethod("checkvars",
          signature(x = "FCS"), 
	 ## not sure if the signature is corrects
          function (x, MY.DEBUG=TRUE, range.max=NULL) {
           
            ##  PURPOSE:
            ## Will check the following:
            ## 1.  Number of observations & number of parameters
            ## 2.  Range of Data
            ## 3.  Names of Data
            
            ## Use 'fixvars' to fix metadata based on data
            
         
            pass.check <- NULL
            
            
            ##  A.  Some initial checks
            
            
            ## Is this an FCS class object?
            if (!is(x, "FCS")) {
              
              ## will return FALSE and get out of the function
              warning("Bad input; not of class FCS")
              return(FALSE)
              
            } else {
              if (MY.DEBUG){ 
                print("Class is FCS")
              }
            }
            
            ## Is there any data?
            if (length(as.vector(fluors(x)))==0){
              warning("FCS object does not have data")
              return(FALSE)
            } else if (length(as.vector(fluors(x)))==1){
              if (is.na(fluors(x))) {
                ## will return FALSE and get out of the function
                warning("FCS object does not have data")
                return(FALSE)
                
              } else {
                if (MY.DEBUG) {
                  print("Object has data")
                }
              }
            } else { ## length is not 0 or 1
              
              if (MY.DEBUG) {
                print("Object has data")
              }
            }
            
            ## Is there metadata?
            if (is.null(metaData(x)) ) {
              ## will return FALSE and get out of the function
              
              warning("FCS object does not have data")
              return(FALSE)
            } else {
              if (MY.DEBUG) {
                print("Object has metadata")
              }
            }
            
            ## Is there an object name?
            if (x@metadata@objectname=="" || is.null(x@metadata@objectname) || (x@metadata@objectname=="None")) {
              if(MY.DEBUG){
                print("Object does not have a name.")
              }
            } else {
              if (MY.DEBUG) {
                print(paste("Object has a name:",x@metadata@objectname, sep="") )
              }
            }
            
            ## We extract the necessary data and metadata 
            ## for the checks.

            ## If the object is not the original
            ## we check RFACSadd parameters.

            ## If the object is the original
	    ## we check for the original parameter names.

            ## The original flag of the metadata is only changed
            ## when using the 'ExtractGatedData'.  Using [, [<-
	    ## will not change the original flag of the metadata.

            ## if the non original parameters cannot be found,
            ## then the check is skipped.
            
          

            ## The following should work by itself 2/20/04
              meta.size<-x[["size"]]
              meta.nparam<-x[["nparam"]]
              
           
            
            ## After initial checks, we
            ## are able to continue other
            ## checks mentioned in the purpose.

            ## B.  Checking the $TOT (number of rows)
            ##     and $PAR (the number of columns)
            
      
            ## initialize checks to NULL
            row.check <- col.check <- NULL
            
            ## if $TOT and $PAR are not in the metadata
            ## set the checks to FALSE
            
            if (is.null(meta.size) == TRUE) {
              row.check <- FALSE
            }
            
            if (is.null(meta.nparam) == TRUE) {
              col.check <- FALSE
            }
            
            ## make the row and column checks if they are still
            ## NULL
            
            if (is.null(row.check)==TRUE){
              row.check <- ifelse(dim.FCS(x)[1]==meta.size,
                                  TRUE, FALSE)
            }
            
            if (is.null(col.check)==TRUE){
              col.check <- ifelse(dim.FCS(x)[2]==meta.nparam,
                                  TRUE, FALSE)
            }
            
            ## Initial check
            if (MY.DEBUG == TRUE) {
              print("Data Dimension Check: Dimensions: (row X col)")
              print(paste("     ", "Data: (",
                          dim.FCS(x)[1], " X ",
                          dim.FCS(x)[2], ")", sep="")) 
              print(paste("     ", "Metadata: (",
                          meta.size,
                          " X ", meta.nparam,
                          ")", sep="")) 
              
            }
            ## IF there are FALSE checks:
            ## 1.  print out the debugging statement
            ## if indicated by MY.DEBUG
            
            
            if (row.check == FALSE) {
              if (MY.DEBUG == TRUE) {
                print("   Row number ($TOT/size) mismatch.")
              }
              
            }
            
            if (col.check == FALSE) {
              if (MY.DEBUG == TRUE) {
                print("   Column number ($PAR/nparam) mismatch.")
              }
            }    
            ##  JYW Question:
            ## Do we also need to remove all metadata parameters
            ## that have an index greater than $PAR
            ## no just comment that they are probably invalid in the docs 
            
            
            
            ## checks: row.check ; col.check
            
            
            ##  C. Checking the names only if the column dimensions
            ##  If there is a difference, then the metadata
            ##  names are changed to that of the data
               
            if (length(x[["longnames"]])==0){
              longnames.metadata <- rep(NA, dim.FCS(x)[2])
            } else {
              longnames.metadata <- x[["longnames"]]
            }
            if (length(x[["shortnames"]])==0){
              shortnames.metadata <- rep(NA, dim.FCS(x)[2])
            } else {
               ## we obtain the shortnames of the metadata
           
              shortnames.metadata <- x[["shortnames"]]
            }
            ## We obtain the names in the data
            names.data <- colnames(fluors(x))
            ## We note in which names in the data are NA

            if (!is.null(names.data)){
              names.data.na <- which(is.na(names.data))
              if (length(names.data.na)==0){
                names.data.na <- NULL
              }
            } else {
              names.data.na <- 1:(dim.FCS(x)[2])
              names.data <- rep(NA, dim.FCS(x)[2])
            }

            names.check <- original.stat.check <- TRUE
            ## if the lengths do not match up
            if ((length(names.data)!=length(longnames.metadata)) ||
                (length(names.data)!=length(shortnames.metadata)) ){
              ## there might be something wrong with the original status
              
              x[["original"]] <- ifelse(x[["original"]]==TRUE, FALSE, TRUE)
              longnames.metadata <- x[["longnames"]]
              shortnames.metadata <- x[["shortnames"]]
              if ((length(names.data)!=length(longnames.metadata)) ||
                  (length(names.data)!=length(shortnames.metadata)) ){
              
                names.check <- FALSE
                ## 
              } else {
                if (MY.DEBUG){
                  print(paste("Error Names length mismatch: x@metadata@original Status should be:",
                              x[["original"]], sep=" "))
                  original.stat.check <- FALSE
                }
                
              }
               ## change back to the previous original status
              x[["original"]] <- ifelse(x[["original"]]==TRUE, FALSE, TRUE)
              used.var <-  paste( x@metadata@objectname,"@metadata@longnames", sep="")
              o.names.used <- longnames.metadata <- x[["longnames"]]
            }

            if (original.stat.check & names.check){
              ## note: metadata names are NA if they are missing
              long.na <- which(is.na(longnames.metadata))
              if (length(long.na)==0){
                long.na <- NULL
              }
              longnames.var <- unlist(strsplit(paste("$P",
                                                   1:(dim.FCS(x)[2]),
                                                   "S", sep="",
                                                   collapse=","),
                                             split=","))
            
           
           
            ## note: metadata names are NA if they are missing
            short.na <- which(is.na(shortnames.metadata))
            if (length(short.na)==0){
              short.na <- NULL
            }
           
            shortnames.var <- unlist(strsplit(paste("$P",
                                                    1:(dim.FCS(x)[2]),
                                                    "N", sep="",
                                                    collapse=","),
                                              split=","))
            
            ## NOTE: we will change the missing metadata names to "None"
            if (!is.null(long.na) ){
              longnames.metadata[long.na] <- rep("None", length(long.na))
            }
           
            if (!is.null(short.na) ){
              shortnames.metadata[short.na] <- rep("None", length(short.na))
            }

            ## we will change the metadata names to NA if data names are NA
            if (!is.null(names.data.na) & length(names.data.na)!=0){
              longnames.metadata[names.data.na] <- rep(NA, length(names.data.na))
              shortnames.metadata[names.data.na] <- rep(NA, length(names.data.na))
            }
            
            ## here we check the nonmissing data names
            ## the data's names that are NOT NA
            long.pos.chk <- ifelse(1:length(longnames.metadata) %in% c(names.data.na), 0, 1)
            short.pos.chk <- ifelse(1:length(shortnames.metadata) %in% c(names.data.na), 0,1)
            long.match <- short.match <- NULL
            long.match[long.pos.chk==1] <- ifelse(names.data[long.pos.chk==1]==longnames.metadata[long.pos.chk==1],
                                 1, 0)
            long.match[long.pos.chk==0] <- 1  ## the NA's are skipped in the check
            short.match[short.pos.chk==1] <- ifelse(names.data[short.pos.chk==1]==shortnames.metadata[short.pos.chk==1],
                                  1, 0)
            short.match[short.pos.chk==0] <- 1 ## NA's as skipped in the check
            
            ## we will compare against the longnames of the metadata if there
            ## are more or equal number of matches comparing the data's names with the longnames
            ## we will compare against the shortnames of the metadata if there
            ## are more matches of the data's names with the longnames

           
            if (sum(long.match)>=sum(short.match)){
              ## longnames are used
              used.var <- paste(x@metadata@objectname,"@metadata@longnames", sep="")
              used.metadata.names <- longnames.var
             
              used.match <- long.match
              
              names.used <- longnames.metadata
              o.names.used <- x[["longnames"]]
              
            } else {
              ## shortnames are used
              used.var <- paste( x@metadata@objectname,"@metadata@shortnames", sep="")
              used.metadata.names <- shortnames.var
              
              used.match <- short.match
              
              names.used <- shortnames.metadata
              o.names.used <- x[["shortnames"]]
             
            }
          
          
            
            match.pos.fix <- which(used.match==0)
            if (length(match.pos.fix)==0){
              match.pos.fix <- NULL
            }
            
            
            fix.metadata.vars <- NULL
           
            if (is.null(match.pos.fix)==FALSE || length(match.pos.fix) != 0){
              names.check <- FALSE

              fix.metadata.vars <- used.metadata.names[match.pos.fix]
           
            }
          }
         
           if (MY.DEBUG == TRUE) {
              print("Names Check:")
              ## print(paste("     ", "Data Parameter Names:", sep=" "))
              names.df.output <- cbind(names.data, o.names.used)
              colnames(names.df.output) <- c("Data Parameter Names", used.var)
              print(names.df.output)
              ##  for (i in 1:length(names.data)){
              ##    print(paste("         ", names.data[i]))
              ##  }
              ##  print(paste("     ", used.var, ":", sep=" "))
              ##  for (j in 1:length(o.names.used)){
              ##    print(paste("         ", o.names.used[j]))
              ## }
            }

            if (names.check==FALSE){
              ## remark that there is a names discrepancy
              
              if (MY.DEBUG == TRUE) {
                print(paste("   ", used.var,"do not match with that of the data.", sep=" "))
                
              }
            }

            ## check: names.check
  

            ## C. Fixing the ranges in the metadata
            

            metadata <- x@metadata@fcsinfo
            ## an indicator of the RFACS heading, not used here
            
            is.RFACS.metadata <- unlist(lapply(names(metadata), function(x) {
              total.char <- nchar(x)
              words <- unlist(strsplit(x, "RFACSadd>>"))
              is.RFACS <- NULL
              if (nchar(words[1]) < total.char) {
                is.RFACS <- TRUE
              } else if (nchar(words[1]) == total.char) {
                  is.RFACS <- FALSE
                
              }
              return(is.RFACS)
            }))
            ## obtaining the ranges (max only) of the data
            ranges.data.o <- apply(fluors(x), 2, function(x){max(as.numeric(x))})
            ranges.data <- apply(fluors(x), 2, function(x) {
              if (!is.null(range.max)){
                if (max(as.numeric(x))<range.max){
                  return(range.max)
                } else {
                  return(max(as.numeric(x)))
                }
              } else {
                return(max(as.numeric(x)))
              }
            })

            ## do not check against the missing data ranges
            rng.data.idx <- !is.na(ranges.data)
            
            ## the metadata variables
            range.var <- as.vector(paste("$P", 1:(dim.FCS(x)[2]), 
                                         "R", sep = ""))
            names(ranges.data) <- range.var

            ## getting the ranges of the metadata
            ## that are indicated by metadata variable names
            ## and are not RFACS

            ## we want to replace the regular $PiR with RFACSadd>>$PiR
          
            ##  metadata <- metadata[is.RFACS.metadata == FALSE]
            ranges.metadata <-x[["$PnR"]]
             ## if ranges.metadata not the size of ranges.data
            if (length(ranges.data) > length(ranges.metadata)){
              ranges.check <- FALSE
              ## force the ranges.metadata to the same length
              diff.g <- length(ranges.data)-length(ranges.metadata)
              ranges.metadata <- c(ranges.metadata, rep(NA, diff.g))
              if (MY.DEBUG){
                print("Ranges of the Data is longer than Metadata.")
              }
              
            }

            if (length(ranges.data) < length(ranges.metadata)){
              ranges.check <- FALSE
              ## force the ranges.metadata to the same length
              
              ranges.metadata <- ranges.metadata[1:length(ranges.data)]
              if (MY.DEBUG){
                print("Ranges of the Metadata is longer than the Data.")
                print("  Only the first corresponding elements of the ")
                print("      metadata ranges are compared to the data.")
              }
            }
             
            ## find the missing metadata ranges
            missing.pos <- sapply(ranges.metadata, function(x) {
              ifelse(is.na(x) || is.null(x), 1, 0)
            })

            num.missing <- sum(missing.pos, na.rm = TRUE)
            
            if (num.missing > 0) {
            
              if (MY.DEBUG == TRUE) {
                print(paste("Range Check: Range parameter(s) missing in the metadata:"))
                
              
                ms <- as.matrix(range.var[missing.pos == 1])
                colnames(ms) <- "Missing Ranges"
                print(ms)
                
            
              }
            }

            ranges.info <- rbind(ranges.data, 
                                 ranges.metadata)
            ranges.correct <- apply(ranges.info, 2, function(x) {
              x <- as.numeric(x)
              if (is.element(NA, x)) {
                return(0)
              } else {
                ifelse(x[1] <= x[2], 0, 1)
              }
            })
            ranges.correct <- ifelse(is.na(ranges.correct), 0, ranges.correct)
            
            ranges.check <- sum(ranges.correct)==0
            
            if (ranges.check==FALSE) {
              fix.range.var <- range.var[which(ranges.correct==1)]
              fix.range.data <- ranges.data[which(ranges.correct==1)]
              fix.range.meta <- ranges.metadata[which(ranges.correct==1)]
             
              if (MY.DEBUG==TRUE){
                print("Ranges Check: Column parameters are NOT within the ranges specified in the metadata.")
                rng.df <- cbind(ranges.data.o, x[["paramranges"]])
                colnames(rng.df) <- c("Data Ranges", paste(x[["objectname"]], "@paramranges", sep=""))
                print(rng.df)
              }
            } else if (ranges.check==TRUE){
              if (MY.DEBUG == TRUE) {
                print("Range Check: Column parameters are within specified metadata range.")
                rng.df <- cbind(ranges.data.o, x[["paramranges"]])
                colnames(rng.df) <- c("Data Ranges", paste(x[["objectname"]], "@paramranges", sep=""))
                print(rng.df)
              
              }
            }


            ## check: ranges.check
            pass.check <- row.check & col.check & names.check & original.stat.check & ranges.check
            return(pass.check)
          })


## try an example on a FCSgate object

setGeneric("fixvars",
             function(x, x.name="", range.max=NULL, MY.DEBUG=TRUE) {
               standardGeneric("fixvars")
             })


setMethod("fixvars",
          signature(x = "FCS"),
          function (x,x.name="",range.max=NULL, MY.DEBUG=TRUE) {
           
            ## Upgraded to S4 class
            ##---------------------------------            

            ##  PURPOSE:
            ## Will check the following:
            ## 1.  Number of observations & number of parameters of the Data
            ## 2.  Range of Data
            ## 3.  Names of Data
            ## Will compare 1-3 against the Metadata!    
	
            ## If there is a discrepancy between the FCSdata
            ## and FCSmetadata, then the metadata (ONLY) will
            ## be changed.
            
            ## Relies on 'is.FCS' and 'setMetadata.FCS'(only for S3)
           
            ##-------------------------------------
            ##  A.  Some initial checks
            ##-------------------------------------
  
            ## Is this an FCS class object?
            if (!is(x, "FCS")) {
              stop("Bad input; not of class FCS")
            } else {
              if (MY.DEBUG){ 
                print("Class is FCS")
              }
            }
                    
            ## Is there any data?
            if (length(as.vector(fluors(x)))==0){
              warning("FCS object does not have data")
              return(x)
            } else if (length(as.vector(fluors(x)))==1){
              if (is.na(fluors(x))) {
                ## will return FALSE and get out of the function
                warning("FCS object does not have data")
                return(x)
                
              } else {
                if (MY.DEBUG) {
                  print("Object has data")
                }
              }
            } else { ## length is not 0 or 1
              
              if (MY.DEBUG) {
                print("Object has data")
              }
            }
            
          

            ## Is there metadata? what if it is the default?????
            ## the default is zero or empty lists
            ## which can be updated in this function

            if (is.null(metaData(x))) {
             ## doubt this will ever happen
              warning("FCS object does not have metadata")
              x <- new(data=x@data, metadata=new("FCSmetadata"), "FCS")
              
            } else {
              if (MY.DEBUG) {
                print("Object has metadata")
              }
            }
            exist.meta.objectname <- NULL
            ## Is there an object name?
            if ((x@metadata@objectname=="") || (is.null(x@metadata@objectname)) || (x@metadata@objectname=="None")) {              
              exist.meta.objectname<-FALSE
            } else {
              exist.meta.objectname <- TRUE
            }
            

            if (exist.meta.objectname==FALSE){
              if (x.name==""){
                if(MY.DEBUG){
                  print("Object does not have a name.")
                  print("User did not define an object name in x.name")
                  print("Object will remain with no name")
                }
              } else if (x.name != "") {  ## there is a name defined in x.name
                x@metadata@objectname <- x.name
                if (MY.DEBUG){
                  print("Object does not have a name.")
                  print(paste("User-defined x.name=", x.name, sep=""))
                  print(paste("Object will have new name: ", x.name, sep=""))
                }
              }
            } else if (exist.meta.objectname==TRUE){
              if (x.name==""){
                if (MY.DEBUG) {
                  print(paste("Object has a name: ",x[["objectname"]], sep="") )
                }
              } else if (x.name !=""){
                if (x[["objectname"]] != x.name) {
                  x[["objectname"]] <- x.name
                  if (MY.DEBUG){
                    print(paste("Object has a name:",x[["objectname"]], sep="") )
                    print(paste("User-defined x.name=", x.name, sep=""))
                    print(paste("Object will have new name:", x.name, sep=""))
                  }
                } else if (x[["objectname"]] == x.name){
                  print(paste("Object has a name:",x[["objectname"]], sep="") )
                  print(paste("User-defined x.name=", x.name, sep=""))
                }
              }
            }
          
            ##---------------------------
            ## After initial checks, we
            ## are able to continue other
            ## checks mentioned in the purpose.
            ##----------------------------
            

            
             ##-----------------------------------
            ## C.  Checking the $TOT (number of rows)
            ##     and $PAR (the number of columns)
            ##-------------------------------------
            
            ## Initial check
           
              
              meta.size<-x[["size"]]
              meta.nparam<-x[["nparam"]]
        
            if (MY.DEBUG == TRUE) {
              print("Data Dimension Check: Dimensions: (row X col)")
              print(paste("     ", "Data: (", dim.FCS(x)[1], " X ",   dim.FCS(x)[2], ")", sep="")) 
              print(paste("     ", "Metadata: (", meta.size, " X ", meta.nparam, ")", sep="")) 
              
            }
  
            dim.incorrect.msg <- function(dim.name, dim.pos = c("1","2")) {
              ## PURPOSE:  Will print out a message that the
              ## observations/rows ($TOT) or the number of parameters/
              ## columns ($PAR) of the metadata do NOT match and
              ## are changed to that of the data
              dim.pos <- as.numeric(match.arg(dim.pos))
              if (dim.pos == 1) {
                param <- meta.size
                param.name <- "rows/cells"
              } else if (dim.pos == 2) {
                param <- meta.nparam
                param.name <- "columns/parameters"
              }
              print(paste("Data Dimension Fix: The", dim.name, "of the metadata,", 
                          param, ",is incorrect will be set to the number of", 
                          param.name, "in the data,", dim.FCS(x)[dim.pos], 
                          sep = " "))
              
            }

            ## initialize checks to NULL
            row.check <- col.check <- NULL
            
            ## if $TOT and $PAR are not in the metadata
            ## set the checks to FALSE
  
            if ((is.null(meta.size) == TRUE) || (meta.size==0) || (length(meta.size)==0) ) {
              row.check <- FALSE
            }

            if (is.null(meta.nparam) == TRUE || (meta.nparam==0) || (length(meta.nparam)==0)){
              col.check <- FALSE
            }
            
            ## make the row and column checks if they are still
            ## NULL
  
            if (is.null(row.check)==TRUE){
              row.check <- ifelse(dim.FCS(x)[1]==meta.size,
                                  TRUE, FALSE)
            }
            
            if (is.null(col.check)==TRUE){
              col.check <- ifelse(dim.FCS(x)[2]==meta.nparam,
                        TRUE, FALSE)
            }
 
            ## IF there are FALSE checks:
            ## 1.  print out the debugging statement
            ## if indicated by MY.DEBUG
            ## 2.  change the metadata ($TOT or $PAR) to the number
            ##     of rows or columns in the data
            
            if (row.check == FALSE) {
              if (MY.DEBUG == TRUE) {
                dim.incorrect.msg("size", "1")
              }
              metadata.old.tot <- meta.size
             
              x[["size"]] <- dim.FCS(x)[1]
            }
  
          if (col.check == FALSE) {
            if (MY.DEBUG == TRUE) {
              dim.incorrect.msg("$PAR", "2")
            }
            metadata.old.par <- meta.nparam
            x[["nparam"]] <- dim.FCS(x)[2]
            
            ##  JYW Question:
            ## Do we also need to remove all metadata parameters
            ## that have an index greater than $PAR;
            ## ANS: no, just comment in docs, to beware
            
          }
            
            
            ## checks: row.check ; col.check

            ##-------------------------------------
            ##  B. Checking the names
            ##  If there is a difference, then the metadata
            ##  names are changed to that of the data
            ##----------------------------------
            
           
            ##  B. Checking the names only if the column dimensions
            ##  If there is a difference, then the metadata
            ##  names are changed to that of the data
            
            if (length(x[["longnames"]])==0){
              longnames.metadata <- rep(NA, dim.FCS(x)[2])
            } else {
              longnames.metadata <- x[["longnames"]]
            }
            if (length(x[["shortnames"]])==0){
              shortnames.metadata <- rep(NA, dim.FCS(x)[2])
            } else {
              shortnames.metadata <- x[["shortnames"]]

            }
            ## We obtain the names in the data
            names.data <- colnames(fluors(x))
            ## We note in which names in the data are NA

            if (!is.null(names.data)){
              names.data.na <- which(is.na(names.data))
              if (length(names.data.na)==0){
                names.data.na <- NULL
              }
            } else {
              names.data.na <- 1:(dim.FCS(x)[2])
              names.data <- rep(NA, dim.FCS(x)[2])
            }

            ##########  WHAT IF longnames.metadata/shortnames.metadata
            ## are not the same length as names.data
            ###
                  names.check <- original.stat.check <- TRUE
            ## if the lengths do not match up
            if ((length(names.data)!=length(longnames.metadata)) ||
                (length(names.data)!=length(shortnames.metadata)) ){
              ## there might be something wrong with the original status
              
              x[["original"]] <- ifelse(x[["original"]]==TRUE, FALSE, TRUE)
              longnames.metadata <- x[["longnames"]]
              shortnames.metadata <- x[["shortnames"]]
              if ((length(names.data)!=length(longnames.metadata)) ||
                  (length(names.data)!=length(shortnames.metadata)) ){
                
                ## names.check <- FALSE
                ## change back to the previous original status
                x[["original"]] <- ifelse(x[["original"]]==TRUE, FALSE, TRUE)
                longnames.metadata <- x[["longnames"]]
                shortnames.metadata <- x[["shortnames"]]
                ## concatenate the longer length or put in NAs and continue with name check
                if (length(x[["longnames"]]) > length(names.data)){
                  
                  if (MY.DEBUG){
                    print("metadata@longnames mismatch with data column names")
                    mll <- as.matrix(x[["longnames"]])
                    colnames(mll) <- "metadata@longname"
                    print(mll)
                   ## print(paste("   metadata@longnames:", paste(x[["longnames"]], collapse=","), sep=" "))
                  }
                  x[["longnames"]] <-  x[["longnames"]][1:length(names.data)]
                  longnames.metadata <- x[["longnames"]]
                  if (MY.DEBUG){
                    print(paste("   will be concatenated:"))
                    print(as.matrix(longnames.metadata))
                  }
                } else {
                  if (length(x[["longnames"]]) < length(names.data)){
                    diff <- length(names.data)-length(x[["longnames"]])
                    if (MY.DEBUG){
                      print("metadata@longnames mismatch length with data column names")
                      mll <- as.matrix(x[["longnames"]])
                      colnames(mll) <- "metadata@longnames"
                      print(mll)
                    }
                    x[["longnames"]] <-  c(x[["longnames"]], rep(NA, diff))
                    longnames.metadata <- x[["longnames"]]
                    if (MY.DEBUG){
                      print(paste("   will be changed to:"))
                      print(as.matrix(longnames.metadata))
                    }
                  }
                }
                ## concatenate the longer length or put in NAs and continue with name check
                if (length(x[["shortnames"]]) > length(names.data)){
                  
                  if (MY.DEBUG){
                    print("metadata@shortnames mismatch length with data column names")
                    mss <- as.matrix(x[["shortnames"]])
                    colnames(mss) <- "metadata@shortnames"
                    print(mss)
                    ## print(paste("   metadata@shortnames:", paste(x[["shortnames"]], collapse=","), sep=" "))
                  }
                  x[["shortnames"]] <-  x[["shortnames"]][1:length(names.data)]
                  shortnames.metadata <- x[["shortnames"]]
                  if (MY.DEBUG){
                    print(paste("   will be concatenated:"))
                    print(as.matrix(shortnames.metadata))
                  }
                } else {
                  if (length(x[["shortnames"]]) < length(names.data)){
                    diff <- length(names.data)-length(x[["shortnames"]])
                    if (MY.DEBUG){
                      print("metadata@shortnames mismatch length with data column names")
                      mss <- as.matrix(x[["shortnames"]])
                      colnames(mss) <- "metadata@shortnames"
                      print(mss)
                     ## print(paste("   metadata@shortnames:", paste(x[["shortnames"]], collapse=","), sep=" "))
                    }
                    x[["shortnames"]] <-  c(x[["shortnames"]], rep(NA, diff))
                    shortnames.metadata <- x[["shortnames"]]
                    if (MY.DEBUG){
                      print(paste("   will be changed to:"))
                      print(as.matrix(shortnames.metadata))
                    }
                  }
                }  
              } else {
                if (MY.DEBUG){
                  print(paste("Error Names length mismatch: x@metadata@original Status changed to:",
                              x[["original"]], sep=" "))
                  original.stat.check <- FALSE

                  ## what about row.check and col.check?
                  if (!row.check){
                    x[["size"]] <- dim.FCS(x)[1]
                  }

                  if (!col.check){
                    x[["nparam"]] <- dim.FCS(x)[2]
                  }
                  
                }
                
              }
              
              
            }


            
            if (names.check){
              long.na <- which(is.na(longnames.metadata))
              if (length(long.na)==0){
                long.na <- NULL
              }
              longnames.var <- unlist(strsplit(paste("$P",
                                                     1:x[["nparam"]],
                                                     "S", sep="",
                                                     collapse=","),
                                               split=","))
              
              
              ## note: metadata names are NA if they are missing
              short.na <- which(is.na(shortnames.metadata))
              if (length(short.na)==0){
                short.na <- NULL
              }
              
              shortnames.var <- unlist(strsplit(paste("$P",
                                                      1:x[["nparam"]],
                                                      "N", sep="",
                                                      collapse=","),
                                                split=","))
              
              ## NOTE: we will change the missing metadata names to "None"
              if (!is.null(long.na)){
                longnames.metadata[long.na] <- rep("None", length(long.na))
              }
              
              if (!is.null(short.na)){
                shortnames.metadata[short.na] <- rep("None", length(short.na))
              }
              
              ## we will change the metadata names to NA if data names are NA
              if (!is.null(names.data.na) & length(names.data.na)!=0){
                longnames.metadata[names.data.na] <- rep(NA, length(names.data.na))
                shortnames.metadata[names.data.na] <- rep(NA, length(names.data.na))
              }
              
              ## here we check the nonmissing data names
              ## the data's names that are NOT NA
              long.pos.chk <- ifelse(1:length(longnames.metadata) %in% c(names.data.na), 0, 1)
              short.pos.chk <- ifelse(1:length(shortnames.metadata) %in% c(names.data.na), 0,1)
              long.match <- short.match <- NULL
              long.match[long.pos.chk==1] <- ifelse(names.data[long.pos.chk==1]==longnames.metadata[long.pos.chk==1],
                           1, 0)
              long.match[long.pos.chk==0] <- 1  ## the NA's are skipped in the check
              short.match[short.pos.chk==1] <- ifelse(names.data[short.pos.chk==1]==shortnames.metadata[short.pos.chk==1],
                            1, 0)
              short.match[short.pos.chk==0] <- 1 ## NA's as skipped in the check
              
              ## we will compare against the longnames of the metadata if there
              ## are more or equal number of matches comparing the data's names with the longnames
              ## we will compare against the shortnames of the metadata if there
              ## are more matches of the data's names with the longnames
             
              if (sum(long.match)>=sum(short.match)){
                ## longnames are used
                used.var <- paste(x@metadata@objectname,"@metadata@longnames", sep="")
                used.metadata.names <- longnames.var
                
                used.match <- long.match
                
                names.used <- longnames.metadata
                o.names.used <- x[["longnames"]]
                
              } else {
                ## shortnames are used
                used.var <- paste( x@metadata@objectname,"@metadata@shortnames", sep="")
                used.metadata.names <- shortnames.var
                
                used.match <- short.match
                
                names.used <- shortnames.metadata
                o.names.used <- x[["shortnames"]]
                
              }

                match.pos.fix <- which(used.match==0)
            if (length(match.pos.fix)==0){
              match.pos.fix <- NULL
            }
            
            names.check <- TRUE
            fix.metadata.vars <- NULL
         
            if (is.null(match.pos.fix)==FALSE || length(match.pos.fix) != 0){
              names.check <- FALSE

              fix.metadata.vars <- used.metadata.names[match.pos.fix]
            
            }
            
       
            

            }

            if (MY.DEBUG == TRUE) {
              print("Names Check:")
              names.df.output <- cbind(names.data, o.names.used)
              colnames(names.df.output) <- c("Data Parameter Names", used.var)
              print(names.df.output)
              ##  print(paste("     ", "Data Parameter Names:", sep=" "))
              ##  for (i in 1:length(names.data)){
              ##    print(paste("          ", names.data[i]))
              ##  }
              ## print(paste("     ", used.var, ":", sep=" "))
              ##for (j in 1:length(o.names.used)){
              ## print(paste("          ", o.names.used[j], sep = " "))
              ##}
            }
            
          
            if (names.check==FALSE){
             
              ## change the metadata to names of the data
              slotname <- unlist(strsplit(used.var, split="metadata@"))[2]
            
              idx.vars <- sort(c(which(used.metadata.names %in% fix.metadata.vars), names.data.na), decreasing=FALSE)
              x[[slotname]][idx.vars] <- names.data[idx.vars]
              
              if (MY.DEBUG == TRUE) {
                ## remark that there is a names discrepancy & the fix
                
                print(paste("   ", used.var,"do not match with that of the data.", sep=" "))
                print("Names Fix: Replacement of the metadata parameter(s):")
                
                print(as.matrix(used.metadata.names[idx.vars]))
                print("  from the old name(s) of the original metadata:")
                print(as.matrix(o.names.used[idx.vars]))
                print("  to the following name(s) from the data:")
               
                print(as.matrix(names.data[idx.vars]))
              }
            }
            
            ## check: original.stat.check names.check
  
           
            
            ## C. Fixing the ranges in the metadata


            metadata <- x@metadata@fcsinfo
            ## indicator for the RFACS heading, not used here
            
            is.RFACS.metadata <- unlist(lapply(names(metadata), function(x) {
              total.char <- nchar(x)
              words <- unlist(strsplit(x, "RFACSadd>>"))
              is.RFACS <- NULL
              if (nchar(words[1]) < total.char) {
                is.RFACS <- TRUE
              } else if (nchar(words[1]) == total.char) {
                  is.RFACS <- FALSE
                
              }
              return(is.RFACS)
            }))
            ## obtaining the ranges (max only) of the data
            ranges.data.o <- apply(fluors(x), 2, function(x){ max(as.numeric(x))})
            ranges.data <- apply(fluors(x), 2, function(x) {
              if (!is.null(range.max)){
                if (max(as.numeric(x))<range.max){
                  return(range.max)
                } else {
                  return(max(as.numeric(x)))
                }
              } else {
                return(max(as.numeric(x)))
              }
            })

            ## the metadata variables
            range.var <- as.vector(paste("$P", 1:(dim.FCS(x)[2]), 
                                         "R", sep = ""))
            names(ranges.data) <- range.var
            
            ## getting the ranges of the metadata
            ## that are indicated by metadata variable names
            ## and are not RFACS

            ## we want to replace the regular $PiR with RFACSadd>>$PiR
          
            
            ranges.metadata <- x[["$PnR"]]

            ## if ranges.metadata not the size of ranges.data
            if (length(ranges.data) > length(ranges.metadata)){
              ranges.check <- FALSE
              ## force the ranges.metadata to the same length
              diff.g <- length(ranges.data)-length(ranges.metadata)
              ranges.metadata <- c(ranges.metadata, rep(NA, diff.g))
              if (MY.DEBUG){
                print("Ranges of the Data is longer than Metadata.")
              }
              
            }

            if (length(ranges.data) < length(ranges.metadata)){
              ranges.check <- FALSE
              ## force the ranges.metadata to the same length
              
              ranges.metadata <- ranges.metadata[1:length(ranges.data)]
              if (MY.DEBUG){
                print("Ranges of the Metadata is longer than the Data.")
                print("  Only the first corresponding elements of the")
                print("      metadata ranges are compared to the data.")
              }
            } 
           

            ## making sure the ranges.metadata is numeric
            ranges.metadata <- unlist(sapply(ranges.metadata, function(x) {
              if (is.null(x)) {
                return(NA)
              }
              else if (is.na(x)) {
                return(NA)
              }
              else {
                return(as.numeric(x))
              }
            }))
            ## find the missing metadata ranges
            missing.pos <- sapply(ranges.metadata, function(x) {
              ifelse(is.na(x) || is.null(x), 1, 0)
            })

            num.missing <- sum(missing.pos, na.rm = TRUE)
            
            if (num.missing > 0) {
              for (i in 1:length(range.var[missing.pos==1])){
                slotname <- range.var[missing.pos==1][i]
                x[[slotname]] <-ranges.data[missing.pos==1][i]
              }
              if (MY.DEBUG == TRUE) {
                print(paste("Range Check: Range parameter(s) missing in the metadata:"))
              
                print(as.matrix(range.var[missing.pos == 1]))
                print(paste("Range Fix: Range parameter(s) added to the metadata with range(s):" ))
                 
                print(as.matrix(ranges.data[missing.pos==1]))
                
              }
            }

            ranges.info <- rbind(ranges.data, 
                                 ranges.metadata)
            ranges.correct <- apply(ranges.info, 2, function(x) {
              x <- as.numeric(x)
              if (is.element(NA, x)) {
                return(0)
              } else {
                ifelse(x[1] <= x[2], 0, 1)
              }
            })
            ranges.correct <- ifelse(is.na(ranges.correct), 0, ranges.correct)

            ranges.check <- sum(ranges.correct)==0
            if (ranges.check==FALSE) {
              fix.range.var <- range.var[which(ranges.correct==1)]
              fix.range.data <- ranges.data[which(ranges.correct==1)]
              fix.range.meta <- ranges.metadata[which(ranges.correct==1)]
              if (MY.DEBUG==TRUE){
                print("Ranges Check: Column parameters are NOT within the ranges specified in the metadata.")
                rng.df <- cbind(ranges.data.o, x[["paramranges"]])
                colnames(rng.df) <- c("Data Ranges", paste(x[["objectname"]], "@paramranges", sep=""))
                print(rng.df)
                
               ## print(paste("     Data Ranges:", paste(ranges.data.o, collapse=","), sep=" "))
               ## print(paste("     ", x[["objectname"]], "@paramranges: ", paste(x[["paramranges"]], collapse=","), sep=""))
                print("Range Fix: Replacing the metadata parameter(s):")
                print(as.matrix(fix.range.var))
                print("  corresponding to the metadata range value(s):")
                print(as.matrix(fix.range.meta))
                print("  with the following range(s) from the data:")
                print(as.matrix(fix.range.data))
              }
              
              
              for (i in 1:length(fix.range.var)){
                slotname <- fix.range.var[i]
                range.max.i <- fix.range.data[i]
                x[[slotname]] <- range.max.i
              }
            } else if (ranges.check==TRUE){
              if (MY.DEBUG == TRUE) {
                print("Range Check: Column parameters are within specified metadata range.")
                rng.df <- cbind(ranges.data.o,x[["paramranges"]])
                colnames(rng.df) <- c("Data Ranges", paste(x[["objectname"]], "@paramranges", sep=""))
                print(rng.df)
               
              }
            }
            
            
            ## check: ranges.check
            ## structure(x, class ="FCS")
            x
          })


setGeneric("equals",
             function(x,y,type="FCS", check.filename=FALSE, check.objectname=FALSE) {
               standardGeneric("equals")
             })


setMethod("equals",
          signature(x = "FCS", y="FCS"),
          function (x,
                    y,
                    type="FCS",
                    check.filename=FALSE,
                    check.objectname=FALSE) {
            ## only checks the equality of two FCS objects
            ## returns boolean value
            if (!is(x, type) || !is(y, type)){
              stop("Input 'x' and 'y' should be of class 'FCS'")
            }

            ## metadata match

          
            metadata.match <- TRUE
            metadata.x <-x@metadata
            metadata.y <- y@metadata


            if (!identical(slotNames(metadata.x), slotNames(metadata.y))){
              metadata.match <- FALSE
              warning("Slot names of the metadata do not match")
            } else {
              sn <- slotNames(metadata.x)
              if (check.filename==FALSE) {
                sn <- sn[sn != "filename"]
              }
              if (check.objectname == FALSE){
                sn <- sn[sn!="objectname"]
              }
              ## check all slots:
              continue <- TRUE
              slotname.pos <- 1
              while (continue==TRUE){
              
                metadata.match <- metadata.match & identical(x[[sn[slotname.pos]]],
                                            y[[sn[slotname.pos]]])
                
                slotname.pos <- slotname.pos + 1
                continue <- (slotname.pos <= length(sn)) & (metadata.match==TRUE)
              }
              ## checking names of the fcsinfo list
              metadata.match <- metadata.match & identical(names(metadata.x@fcsinfo), names(metadata.y@fcsinfo))
            }
           
            ## data match
            data.match <- TRUE
            if (metadata.match==TRUE){
              data.x <- as(x@data, "matrix")
              data.y <- as(y@data, "matrix")
              colnam.x <- colnames(data.x)
              colnam.y <- colnames(data.y)

              data.match <- identical(data.x, data.y) & identical(colnam.x, colnam.y)
             
            }
            return(metadata.match & data.match)
            
          })



