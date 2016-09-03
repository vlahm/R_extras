R tools
==


#Insert a row into a matrix or data.frame####

rowcol_inserter <- function(df, vec, pos, name=pos, dim='row'){
  if(dim == 'row'){
    df[(pos+1):(nrow(df)+1),] <- df[pos:nrow(df),]
    df[pos,] <- as.vector(vec)
    rownames(df)[pos] <- name
  } else if(dim == 'col'){
    namestore <- colnames(df)[pos:ncol(df)]
    df[,(pos+1):(ncol(df)+1)] <- df[,pos:ncol(df)]
    df[,pos] <- vec
    colnames(df)[pos:ncol(df)] <- c(name, namestore)
  } else message('dim must be "row" or "col"')
  return(df)
}

#replacer####   NOTICE - this can probably be replaced by x[x==cond] <- replacement

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
can also handle "NA"s (as from excel) with x <- replacer(x, '==NA', 'NA'); x <- replacer(x, "=='NA'", NA)

#max_dens####

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
  
  #create matrix of modes across all chains
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
#takes mcmc.list objects (JAGS standard output) and creates matrix of modes for each parameter
#works across all chains

mod1 <- jags.model(model_script, data = list(<data>), 
  inits = function()list(<inits>), n.chains = 3, n.adapt = 2000)

update(mod1, n.iter=5e3)

mod1_out <- coda.samples(mod1, c(<variables out>, n.iter=1e5, thin=100))

get_modes(mod1_out)


reassembler_MCMClist <- function(mcmc_list){ #return 'MLEs' and densities as arrays
    nchains <- length(mcmc_list)
    chain_length <- length(mcmc_list[[1]][,1])
    param_list <- gsub(" *\\[.*?\\] *", '', colnames(mcmc_list[[1]]))
    all_ps <- which(param_list == 'p' | param_list == 'mean_p_sample' | param_list == 'mean_p_predator')
    num_ps <- length(all_ps)
    first_p <- all_ps[1]
    total_vals <- num_ps * nchains
    sum_chain_lengths <- chain_length * nchains
    indices <- seq(1, npreds*npreytypes, npreytypes)
    MLE_arr <- array(data=NA, dim=c(npreytypes,npreds))
    dens_arr <- array(data=NA, dim=c(npreytypes,npreds,sum_chain_lengths))

    #create vector of MLEs across chains
    temp <- matrix(data=NA, nrow=chain_length, ncol=nchains)
    MLE_vec <- vector('numeric', length=num_ps)

    for(i in 1:num_ps){
        for(j in 1:nchains){
            temp[,j] <- mcmc_list[[j]][,i+first_p-1][1:chain_length]
        }
        chain <- as.vector(temp)
        MLE_vec[i] <- density(chain, n=10000)$x[which.max(density(chain, n=10000)$y)]
    }

    #assemble MLEs into array
    MLE_arr[1,1:31] <- MLE_vec[indices]
    MLE_arr[2,1:31] <- MLE_vec[indices+1]
    if(npreytypes == 3){
        MLE_arr[3,1:31] <- MLE_vec[indices+2]
    }

    #make separate array of combined chains
    temp2 <- matrix(data=NA, nrow=chain_length, ncol=nchains)
    temp3 <- matrix(data=NA, nrow=chain_length, ncol=nchains)
    temp4 <- matrix(data=NA, nrow=chain_length, ncol=nchains)

    for(i in 1:length(indices)){
        for(j in 1:nchains){
            temp2[,j] <- mcmc_list[[j]][,(first_p-1+indices[i])][1:length(mcmc_list[[j]][,1])]
            temp3[,j] <- mcmc_list[[j]][,(first_p-1+(indices+1)[i])][1:length(mcmc_list[[j]][,1])]
            if(npreytypes == 3){
                temp4[,j] <- mcmc_list[[j]][,(first_p-1+(indices+2)[i])][1:length(mcmc_list[[j]][,1])]
            }
        }
        dens_arr[1,i,] <- as.vector(temp2)
        dens_arr[2,i,] <- as.vector(temp3)
        if(npreytypes == 3){
            dens_arr[3,i,] <- as.vector(temp4)
        }
    }

    return(list(MLE_arr, dens_arr))
}
#extracts source proportion posteriors from jags output and assembles into array of posterior draws and matrix of modes

#excel_to_r####

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
#copy a column or row (or even a block) from Excel, run this function.
#it uses scan(), so just paste the values into the console and hit enter.
#The output will be vector-ready

#comma_blaster####

comma_blaster <- function(x){
    x <- gsub('"', '', paste(x, collapse=' '), perl=TRUE)
    x <- gsub('\n', '', x, perl=TRUE)
    out <- gsub(' +', ', ', x, perl=TRUE)
    return(out)
}
#takes printouts of R vectors (as char) and converts them to comma separated sequences 
#for use in defining new vectors

#return indices or values of bottom or top n items in a list or vector
head(order(vec), n)
tail(sort(vec), n)

#metarep####

metarep <- function(x){
    out <- 1
    for(i in 2:x){
        out <- c(out, rep(i, times=i))
    }
    return(out)
}
#repeat each element itself number of times

#checkNA (and then some)####

checkNA <- function(){
    na_prop <- rep(NA, ncol(nuts))
    for(i in 1:ncol(nuts)){
        na_prop[i] <- sum(is.na(nuts[,i]))/nrow(nuts)
    }
    out <- cbind(colnames(nuts), na_prop)
    return(out)
}
#this could easily be modified into something that find the proportion of any value across each column of a dataframe

#batch file reader####
# dir_args go to dir(), which selects the files to be read/merged.  
# read_args go to read.table, which by default is set up to read .csv files
# 'merge' determines whether to merge each file and output the merged dataframe (TRUE) or leave separate and send to global env (FALSE)
# ... supplies args to merge()

batch_file_reader <- function(dir_args=list(path='./', pattern='.csv'),
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

#add confidence intervals to linear model plots
n<-50
x<-seq(10,20,length.out=n)
y<-rnorm(n)
mod<-lm(y~x)
plot(x,y)
abline(mod)

CI<-function(model, xbounds=range(model$model[,2]), level=.95, ...)
{
  confx<-seq(xbounds[1],xbounds[2], length.out=length(model$fitted.values))
  int.vals<-predict(model,newdata=data.frame(confx),interval="confidence",
                    level=level)
  lines(confx,int.vals[,2], ...)
  lines(confx,int.vals[,3], ...)
}

CI(mod, lty=2, col='red')

#turbo di(c)e roller:
roller<-function(sides_vec, rolls_vec){
  results <- list()
  for(die in 1:length(sides_vec)){
    results[[die]] <- sample(1:sides_vec[die], rolls_vec[die], replace=TRUE)
    names(results)[die] <- paste0(sides_vec[die], '-sided')
  }
  return(results)
}

roller(c(6,12,20), 1:3)
