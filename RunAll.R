############################################################# Set your directory
setwd("C:\\Users\\cody_ross\\Dropbox\\Completed and Published Projects\\1-papers\\Why economic experiments\\Workflow")

################################################################# Load Libraries
library(igraph)
library(reshape2)
library(plyr)
library(kinship2)
library(geosphere)
library(GGally)
library(network)
library(ggplot2)
library(rethinking)
library(colorspace)
library(parallel)
library(Cairo)
library(qgraph)
options(mc.cores = parallel::detectCores())
################################################################# Build Database
#source("Code\\Build_Data.R")# Note that this file is run on private database
                             # Code is included here for review, but will not run

load("ColombianDataWithImputations.RData") # Loads anonymized and rescaled
                                                 # data with hard-coded
                                                 # imputations of missings
                                                                            
source("Code/PlotNetworks.R")    

################################### Model data with standard multinomial outcome
iter <- 2000
warmup <- 1000

fit_Basic   <- stan(file = "Code/Model_Controls_Basic.stan", data=model_dat_Coast, refresh=1, chains=2, iter=iter, warmup=warmup, control=list(adapt_delta=0.95))

source("Code\\Plots.R")
source("Code\\Check_Traceplots.R")

################################### Model data with truncated multinomial outcome
fit_Trunc    <- stan(file = "Code/Model_Controls_Trunc.stan", data=model_dat_Coast, refresh=1, chains=2, iter=iter, warmup=warmup, control=list(adapt_delta=0.95))

source("Code\\PlotsTrunc.R")
source("Code\\Check_Traceplots_Trunc.R")

################################### Model data with full SRM standard multinomial outcome
fit_Basic   <- stan(file = "RevisedCode/Full_SRM_Basic.stan", data=model_dat_Coast, refresh=1, chains=2, cores=2, iter=iter, warmup=warmup, control=list(adapt_delta=0.96))

source("RevisedCode/Plots.R")
source("RevisedCode/Check_Traceplots.R")

################################### Model data with full SRM truncated multinomial outcome
fit_Trunc    <- stan(file = "RevisedCode/Full_SRM_Truncated.stan", data=model_dat_Coast, refresh=1, chains=2, cores=2,  iter=iter, warmup=warmup, control=list(adapt_delta=0.96))

source("RevisedCode/PlotsTrunc.R")
source("RevisedCode/Check_Traceplots_Trunc.R")

