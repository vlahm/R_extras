# install.packages("devtools")
library(devtools)
# devtools::install_github("klutometis/roxygen") #didn't work, but was recommended
# install.packages('roxygen2')
library(roxygen2)

#go to the folder that will contain your package
setwd('~/git/tools_for_R')

#create the package folder (this also puts necessary files inside)
# create('manipulateR')

#go to the file called "DESCRIPTION" in that folder.  edit all necessary fields just like in
#the manipulateR example

#now generate actual documentation (.Rd files)
document(pkg='./manipulateR')

#and install (but must be run from the containing folder unless I do it from GitHub
install('manipulateR')
library(manipulateR)
detach('manipulateR')

?excel_to_r

#install from github
install_github('vlahm/manipulateR')
library(manipulateR)
