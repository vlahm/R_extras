#' Proportion-Based Data Subsetting
#'
#' Remove rows or columns according to the proportion of elements that 
#'   meet the condition specified.
#'
#' @author Mike Vlah, \email{vlahm13@gmail.com}
#' @param x the matrix or data.frame to be subset.
#' @param mar the margin along which the function will be applied - either
#'   1 for rows or 2 for columns.
#' @param cond character expression; the completion of a Boolean expression,
#'   e.g. ">= 0", which is passed to \code{apply}.
#' @keywords subsetting, slicing
#' @return a matrix or data.frame with columns or rows removed according to
#'   the condition or function specified.
#' @export
#' @examples
#' > df <- data.frame(a=c(2,2,3,3), b=c(1,2,NA,1), c=c(2,NA,NA,3))
#' > df
#'   a  b  c
#' 1 2  1  2
#' 2 2  2 NA
#' 3 3 NA NA
#' 4 3  1  3
#' 
#' > matfilter(df, mar=2, fun=is.na, thresh=0.3)
#' $column_proportions
#'    a    b    c 
#' 0.00 0.25 0.50 
#' 
#'   a  b
#' 1 2  1
#' 2 2  2
#' 3 3 NA
#' 4 3  1
#' 
#' > matfilter(df, mar=1, cond='== 2', thresh=0.5)
#' $row_proportions
#' [1] 0.6666667 0.6666667 0.0000000 0.0000000
#' 
#'   a  b  c
#' 3 3 NA NA
#' 4 3  1  3 
matfilter <- function(x, mar=2, cond=NA, fun=NULL, thresh=1, 
                      na.rm=TRUE, filter=TRUE){
    
    if(!(mar %in% 1:2)) stop("'mar' must be either '1' for rows or '2' for columns")
    
    if(!(is.na(cond))){
        options(warn = 2)
        prop <- try(apply(x, mar, 
                          function(ind){
                              sum(eval(parse(text=paste0('ind',cond))), 
                              na.rm=na.rm) / length(ind)
                          }), silent=TRUE)
        
        if(class(prop) == 'try-error'){
            stop(paste("\"cond\" must be a character string", 
                       "completing a Boolean expression,",
                       "e.g. \"== 5\""))
        }
        
    } else {
        if(!(is.null(fun))){
            prop <- apply(x, mar, function(ind) sum(fun(ind), 
                                                    na.rm=na.rm) / length(ind))
        } else stop("Either 'cond' or 'fun' must be supplied")
    }
    
    if(thresh < 0 | thresh > 1) stop("'thresh' must be [0,1]")
    
    if(mar == 1){
        print(list(row_proportions = prop))
    } else print(list(column_proportions = prop))
    
    if(filter == TRUE){
        if(mar == 1){
            x <- x[prop <= thresh,]
        } else {
            x <- x[,prop <= thresh]
        }
        return(x)
    } 
}
