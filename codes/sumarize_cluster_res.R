rm(list = ls())


library(stringr)
library(withr)
library(tidyr)
library(elevatr)
library(sf)
library(stringi)

##################################################################################
run.nr <- 45
cluster.nr <- 2631


full.map <- NULL
i <- 1

for (i in 1:run.nr){
  load(paste0("./output/output_",cluster.nr,".",with_options(
    c(scipen = 999),
    str_pad(i, 6, pad = "0")
  ),".RData"))


  if(i==1){
    betas <-  postb.mean
  } else {
    betas <- betas %>% bind_rows(postb.mean)
  }

}


save(betas, file='input/ACCREU_betas.RData')







