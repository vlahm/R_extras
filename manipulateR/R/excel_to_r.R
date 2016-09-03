#' Comma-Separated Strings from Spreadsheet Cell Vectors
#' 
#' Paste values from spreadsheets into R in vector-ready
#'   (comma separated) format.
#' 
#' @author Mike Vlah, \email{vlahm13@gmail.com}
#' @param char_out logical; Should the output be printed with quotes 
#'   around each vector element?
#' @param spaces logical; Should each comma be followed by a space 
#'   (just for looks)?
#' @param ... additional arguments to \code{scan}.
#' @keywords 'data creation'
#' @export
#' @seealso \code{\link{comma_blaster}}
#' @return a comma-separated string that can be copied from the console 
#'   and used to define a vector.
#' @examples
#' excel_to_r(spaces=FALSE)
#'
#' #not run:
#' Paste Excel values into console, then hit ENTER
#' 1: 0    3   3.5 2.5 3   4   2.5 0
#' 9: 
#' Read 8 items
#' [1] "0,3,3.5,2.5,3,4,2.5,0"
#'
#' #run:
#' newvec <- c(0,3,3.5,2.5,3,4,2.5,0)
#'
#'
#' excel_to_r(char_out=TRUE, allowEscapes=FALSE)
#' 
#' #not run:
#' Paste Excel values into console, then hit ENTER
#' 1: 4    6   3.2 \n
#' 5: 
#' Read 4 items
#' [1] "'4', '6', '3.2', '\\n'"
excel_to_r <- function(char_out=FALSE, spaces=TRUE, ...){
    
    message('Paste cell values into console, then hit ENTER')
    x <- scan(what = 'character', ...)
    
    x <- paste(x, collapse=',')
    if(char_out == TRUE){
        x <- gsub(',', "','", x)
        x <- paste0("'", x, "'")
    }
    if(spaces == TRUE){
        x <- gsub(',', ', ', x)
    }
    
    return(x)
}
