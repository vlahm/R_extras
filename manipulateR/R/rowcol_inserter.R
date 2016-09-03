#' Insert a Row or Column Into a Matrix or Data Frame
#'
#' @author Mike Vlah, \email{vlahm13@gmail.com}
#' @param x data.frame or matrix to modify.
#' @param vec a vector to be inserted.
#' @param pos the numerical position in the data.frame 
#'   or matrix where the vector will be inserted.
#' @param name a character name for the row or column (optional).
#' @param dim 'row' to insert a row, 'col' to insert a column. 
#'   Defaults to 'row'.
#' @keywords 'data manipulation'
#' @return a data.frame or matrix with row or column inserted.
#' @export
#' @examples
#' #example dataframe
#' x <- data.frame('col1'=1:10, 'col2'=rnorm(10), 'col3'=letters[1:10],
#' stringsAsFactors=FALSE)
#'
#' #insert new row at position 8
#' rowcol_inserter(x, c(99,100,'new'), 8)
#'
#' #insert new column at position 3
#' rowcol_inserter(x, 101:110, 3, name='newcol', dim='col')
rowcol_inserter <- function(x, vec, pos, name=pos, dim='row'){
    if(dim == 'row'){
        x[(pos+1):(nrow(x)+1),] <- x[pos:nrow(x),]
        x[pos,] <- as.vector(vec)
        rownames(x)[pos] <- name
    } else if(dim == 'col'){
        namestore <- colnames(x)[pos:ncol(x)]
        x[,(pos+1):(ncol(x)+1)] <- x[,pos:ncol(x)]
        x[,pos] <- vec
        colnames(x)[pos:ncol(x)] <- c(name, namestore)
    } else message('dim must be "row" or "col"')
    return(x)
}
