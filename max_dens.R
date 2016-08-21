#takes mcmc.list objects (JAGS standard output) and creates matrix of "MLE" values for each parameter
#works across all chains

max_dens <- function(mcmc.list){
    
    #extract all relevant info from mcmc.list
    nchains <- length(mcmc.list)
    chainlength <- length(mcmc.list[[1]][,1])
	paramlist <- gsub(" *\\[.*?\\] *", '', colnames(mcmc.list[[1]]))
	params <- levels(factor(paramlist))
	nparams <- length(params)
    param_lengths <- vector('numeric', length=nparams)
    for(i in 1:nparams){
        param_lengths[i] <- length(which(paramlist==paste(params[i])))
    }    
    longest_param <- max(param_lengths)
    
    #create matrix of "MLE" values across all chains
    temp <- matrix(data=NA, nrow=chainlength, ncol=nchains)
    out <- matrix(data=NA, nrow=longest_param, ncol=nparams)
    for(j in 1:nparams){
        indices <- which(paramlist==params[j])
        for(i in 1:length(indices)){
            for(k in 1:nchains){
                temp[,k] <- mcmc.list[[k]][,indices[i]][1:length(mcmc.list[[k]][,1])]
            }
            temp2 <- as.vector(temp)
            out[i,j] <- density(temp2, n=10000)$x[which.max(density(temp2, n=10000)$y)]
        }        
    }
    colnames(out) <- params
    
    return(out)
}