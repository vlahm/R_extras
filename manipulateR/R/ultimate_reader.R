#' Read and Merge Multiple Files
#'
#' Reads any character-delimited filetype based on filename patterns.
#'   Merges data if desired.
#'
#' @author Mike Vlah, \email{vlahm13@gmail.com}
#' @param dir_args \code{list} of args passed to \code{dir}, which selects
#'   the files to be read/merged.
#' @param read_args \code{list} of args passed to \code{read.table}, 
#'   which by default is set up to read .csv files.
#' @param merge determines whether to merge each file and output the merged
#'   \code{data.frame} (TRUE) or leave them separate and add them to the
#'   global environment (FALSE).
#' @param ... supplies additional args to \code{merge}.
#' @keywords read merge
#' @return either returns a merged \code{data.frame} or adds individual
#'   \code{data.frame}s to the global environment.
#' @export 
#' @examples
#' ultimate_reader(dir_args=list(path='~/stuff/', pattern='\w{3}_2016.csv')
#'
#' merged <- ultimate_reader(merge=TRUE, by='date', all=TRUE)
ultimate_reader <- function(dir_args=list(path='./', pattern='.csv'),
                              read_args=list(sep=',', quote="\"", header=TRUE, 
                                             fill=TRUE, comment.char=""),
                              merge=FALSE, ...){
    
    if(!(substr(dir_args$path, nchar(dir_args$path), nchar(dir_args$path)) %in% c('/', '\\'))){
        stop("'path' must include trailing '/' or '\\'")
    }
    
    #create global objects for contents of all specified files
    files <- do.call('dir', dir_args)
    obj_names <- vector(length=length(files))
    for(i in 1:length(files)){
        obj_names[i] <- substr(files[i], 1, nchar(files[i])-4)
        full_read_args <- append(list(file=paste0(dir_args$path, files[i])), read_args)
        temp <- do.call('read.table', args=full_read_args)
        # temp <- read.table(paste0(dir_args$path, files[i]), sep=',', quote="\"",
        #                    header=TRUE, fill=TRUE, comment.char="")
        
        if(merge == FALSE){
            assign(obj_names[i], temp, pos='.GlobalEnv')
        } else {
            assign(obj_names[i], temp)
        }
    }
    
    #merge all objects and output, if specified
    if(merge == TRUE){
        merged <- Reduce(function(x, y) {merge(x, y, ...)}, 
                      eval(parse(text=paste('list(', paste(obj_names, collapse=','), ')'))))
        return(merged)
    }
    
    message(paste('files read:', paste(obj_names, collapse=', ')))
}
