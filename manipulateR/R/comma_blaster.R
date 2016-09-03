#' Comma-Separated Strings for Vector Definition
#' 
#' Format R console printouts for definition of new vectors.
#'   Sometimes faster than figuring out how to extract vectors from complex
#'   objects. Often useful during exploratory phase of analysis.
#'  
#' @author Mike Vlah, \email{vlahm13@gmail.com} 
#' @param x a character string to be comma-separated.
#' @param sep element in the input string to be replaced with commas.
#'   Defaults to " ".
#' @param char_out logical; should the output be printed with quotes around each 
#'   vector element?
#' @param spaces logical; should each comma be followed by a space (just for looks)?
#' @keywords 'data creation'
#' @seealso \code{\link{excel_to_r}}
#' @return a comma-separated character string that can be copied from the
#'   console and used to define a vector.
#' @export
#' @examples
#' > comma_blaster(month.abb[1:5], char_out=TRUE)
#' [1] "'Jan', 'Feb', 'Mar', 'Apr', 'May'"
#' > newvec <- c('Jan', 'Feb', 'Mar', 'Apr', 'May')
#' > 
#' > comma_blaster("1-800-555-0100", sep='-')
#' [1] "1, 800, 555, 0100"
comma_blaster <- function(x, sep=" ", char_out=FALSE, spaces=TRUE){
    x <- gsub('"', '', paste(x, collapse=' '), perl=TRUE)
    x <- gsub('\n', '', x, perl=TRUE)
    x <- gsub(paste0(sep, "+"), ',', x, perl=TRUE)
    # x <- gsub(' +', ',', x, perl=TRUE)
    
    if(char_out == TRUE){
        x <- gsub(',', "','", x)
        x <- paste0("'", x, "'")
    }
    if(spaces == TRUE){
        x <- gsub(',', ', ', x)
    }
    
    return(x)
}
