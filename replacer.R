replacer <- function(x, cond, replacement, rows=1:length(x[,1]), cols=1:length(x[1,]), asinsqrt=FALSE){
    for (i in rows)
    {
        for (j in cols)
        {
            if (cond=='==NA')
            {
                if (is.na(x[i,j]==TRUE))
                {
                    x[i,j] <- replacement
                }
            } else
            {
                if (eval(parse(text=paste('x[i,j]', cond))))
                {
                    x[i,j] <- replacement
                }
            }
            if (asinsqrt == TRUE)
            {
                x[i,j]<-asin(sqrt(x[i,j]/100))*(2/pi)
            }
        }
    }
    return(x)
}

notes: could easily be expanded so that 'replacement' can be an expression or function
'cond' must be a character expression like '==0' or '> 5'
probably won't work with data.frames
Can handle NAs if cond='==NA'