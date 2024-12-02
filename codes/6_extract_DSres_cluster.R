rm(list = ls())
library(stringr)
library(withr)
require(progress)
library(tidyr)
library(dplyr)
##################################################################################
load("./input/workspace_ACCREU.RData")
rm(list = setdiff(ls(),c("est.grid")))
cluster.nr <- 2749

scens <- unique(est.grid$scens)
Ns <- unique(est.grid$Ns)

scen <- scens[2]
NN <- Ns[2]

for(scen in scens){
  luc.res <- NULL
  for(NN in Ns){
    pos <- which(scen==est.grid$scens&NN==est.grid$Ns)
    load(paste0("./output/output_",cluster.nr,".",with_options(c(scipen = 999), str_pad(pos, 6, pad = "0")),".RData"))

    res.all <- res.all %>% filter(value!=0)
    luc.res <- luc.res %>% bind_rows(res.all)
  }

  save(luc.res, file=paste0('./results/ACCREU_',scen,'.RData'))
}

#save(luc.res, file=paste0('../results/MINDSTEP_global_DSres_',curr.scen,'.RData'))


