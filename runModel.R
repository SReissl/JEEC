source("Model.R")
library(foreach)
library(doParallel)
library(tictoc)
#Set number of cores to appropriate value!
cl<-makeCluster((5), outfile="")
registerDoParallel(cl)
mc=100
tic()
foreach(m=1:mc) %dopar% {
library(abind)
library(dyn)
source("Initialise.R")
initreturn<-init()
inits<-initreturn$init
params<-initreturn$param
bankinits<-initreturn$bankinit
#create folder "Rundata" before running the model!
runModel(seed=(m),nB=12,Time = 1632,nrun=34,params=params,inits=inits,bankinits=bankinits,foldername="Rundata/")
}
toc()
stopCluster(cl)