############################################################# Set your directory
setwd("C:\\Users\\cody_ross\\Dropbox\\Open Papers\\The Value of Games\\Workflow")

################################################################# Load Libraries
library(igraph)
library(reshape2)
library(plyr)
library(kinship2)
library(geosphere)
library(GGally)
library(network)
library(sna)
library(ggplot2)
library(rethinking)
library(colorspace)
library(parallel)

################################################################# Build Database
source("Code\\Build_Data.R")# Note that this file is run on private database
                            # Code is included here for review, but will not run

load("Data\\ColombianDataWithImputations.RData") # Loads anonymized and rescaled
                                                 # data with hard-coded
                                                 # imputations of missings
                                                 #                            
                            
################################### Model data with standard multinomial outcome
source("Code\\Model_Controls_Basic.R")

iter <- 2000
warmup <- 1000

fit_Basic   <- stan( model_code=model_code_controls_basic, data=model_dat_Coast,refresh=1,chains=2,iter=iter,warmup=warmup,init=0,control=list(adapt_delta=0.95))

source("Code\\Plots.R")
source("Code\\Check_Traceplots.R")

################################### Model data with truncated multinomial outcome
source("Code\\Model_Controls_Trunc.R")

iter <- 2000
warmup <- 1000

fit_Trunc   <- stan( model_code=model_code_controls_trunc, data=model_dat_Coast,refresh=1,chains=2,iter=iter,warmup=warmup,init=0,control=list(adapt_delta=0.95))

source("Code\\PlotsTrunc.R")
source("Code\\Check_Traceplots_Trunc.R")
