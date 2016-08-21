#copy a column or row (or even a block) from Excel, run this function.
#it uses scan(), so just paste the values into the console and hit enter.
#The output will be vector-ready

excel_to_r <- function(type, spaces=TRUE){
    print('Paste Excel values into console, then hit ENTER')
    if(type=='logical' | type=='integer' | type=='numeric' | type=='complex' |
       type=='character' | type=='raw' | type=='list')
    {
        x <- scan(what = paste(type))
    } else {
        stop("type must be 'logical', 'integer', 'numeric', 'complex', 'character', 'raw', or 'list'")
    }
    x <- paste(x, collapse=' ')
    if(spaces == TRUE){
        x <- gsub(" ", ", ", x)
    } else {
        x <- gsub(" ", ",", x)
    }
    return(x)
}

excel_to_r('character')
