rm(list = ls())

library(downscalr)
library(dplyr)
library(tidyr)
options(dplyr.summarise.inform = FALSE)
library(gdxrrw)
GAMSPath = c("C:/GAMS/40")
igdx(GAMSPath)


###############################################################################
###### choose options for the prior module                                    #
## choose years for Y                                                         #
years <- c(2000,2010) # vector of length 2, 2000,2010,2018                    #
## prior module estimate parameters per country or EEA biogeographical region #
N_REGION <- "EEA_Biogeo" #### EEA_Biogeo/country                              #
## choose a specific CLC to aggregated class mapping                          #
class.mapping.file <- "./input/class_mapping.csv"                             #
## choose classes for which no priors are estimated                           #
not.modelled <- c("OoB","NotRel", "OthAgri")                                  #
                                                                              #
cluster <- TRUE                                                               #
###############################################################################


# STAGE 1 of prior model, load grid for cluster/loop
class.mapping <- read.csv(class.mapping.file)
colnames(class.mapping)[1] <- "Class"

if(N_REGION=="country"){
  region.mapping <- read.csv("./input/EEAref_LAMAnuts_mapping.csv")
  regions <- unique(substr(region.mapping$NUTS_ID,1,2))
  colnames(region.mapping) <- c("n","N")
} else if(N_REGION=="EEA_Biogeo"){
  region.mapping <- read.csv("./input/EEAref_EEAbiogeo_mapping.csv")
  regions <- unique(region.mapping$EEAbiogeo.reg)
  colnames(region.mapping) <- c("n","N")
  regions <- setdiff(regions, c("OUT","ANA"))
}

classes <- unique(class.mapping$LU_mapping)
classes <- setdiff(classes, not.modelled)

estim.grid <- expand.grid(N=regions,LU.from=classes)

if(cluster){
  args <- commandArgs(trailingOnly=TRUE)
  JOB <- ifelse(.Platform$GUI == "RStudio",20,as.integer(args[[1]]))
  estim.grid <- estim.grid[JOB,]
}






cc <- 1
  # for(cc in 1:nrow(estim.grid)) {
  curr.N <- estim.grid$N[cc]
  curr.lu <- estim.grid$LU.from[cc]
  curr.n <- region.mapping$n[region.mapping$N==curr.N]

  #### get Y dependent on choice of mapping file and years
  source("./codes/get_Y.R")

  curr.n <- unique(Y$CELLCODE) #update to extract only those where we observe curr.LU in t0

  #### get X corresponding to Y
  source("./codes/get_X.R")

  #### choose only where data is available
  possible.n <- intersect(Y$CELLCODE, X$CELLCODE)

  Y <- Y %>% filter(CELLCODE%in%possible.n)
  Y <- Y[match(possible.n, Y$CELLCODE),]

  X <- X %>% filter(CELLCODE%in%possible.n)
  X <- X[match(possible.n, X$CELLCODE),]

  # remove <- colSums(t(as.matrix(X %>% dplyr::select(-CELLCODE))) %*% as.matrix(X %>% dplyr::select(-CELLCODE)))
  # remove <- names(remove)[remove==0]
  # X <- X %>% dplyr::select(-any_of(remove))


  baseline <- which(curr.lu == colnames(Y)[-c(1,2)])
res <- mnlogit(as.matrix(Y %>% dplyr::select(any_of(classes))),as.matrix(X %>% dplyr::select(-CELLCODE))[,-c(15)], baseline = baseline, niter = 1000, nburn = 500)


postb.mean <- reshape2::melt(apply(res$postb[,-baseline,],c(1,2),mean)) %>%
  dplyr::rename("lu.to" = "Var1", "ks" = "Var2" )
postb.mean <- data.frame(Ns = curr.N,
                         ks = postb.mean$ks,
                         lu.from = curr.lu,
                         postb.mean[,c("lu.to","value")])

save(res, missing, postb.mean, file="output/output.RData")

# if (iter == 1) {betas = postb.mean; marginal_effects[[1]] <- res$marginal_fx} else {
#   betas = rbind(betas,postb.mean)
#   marginal_effects[[iter]] <- res$marginal_fx
#
#
# ############ done with regressions for priors #############
# names(marginal_effects) <- paste0(est.grid$Ns.prior,"XXX",est.grid$from)
# me <- names(marginal_effects)[1]
# for(me in names(marginal_effects)){
#
#
#   ME_effect.quant <- apply(marginal_effects[[me]],c(1,2), function(x){ quantile(x,probs=c(0.01,0.05,0.16,0.5,0.84,0.95,0.99))})
#
#
#   dimnames(ME_effect.quant)  <-  list(paste0("uncertainty_q",  c("01","05",16,50,84,95,99)),dimnames(ME_effect.quant)[[2]],dimnames(ME_effect.quant)[[3]])
#
#   ME_effect.quant <- data.frame(ME_effect.quant)
#   colnames(ME_effect.quant) <- gsub("Serbia.Monte","SerbiaMonte",colnames(ME_effect.quant))
#   ME_effect.quant$uncertainty <- rownames(ME_effect.quant)
#   ME_effect.quant <- ME_effect.quant %>% pivot_longer(!uncertainty) %>% separate(col="name",sep="[.]", into=c("predictor", "to")) %>%
#     mutate(region=str_split(me,pattern="XXX")[[1]][1] ,from=str_split(me,pattern="XXX")[[1]][2])
#
#
#
#   if(me==names(marginal_effects)[1]) ME_effect.quant.full <- ME_effect.quant else ME_effect.quant.full <- bind_rows(ME_effect.quant.full,ME_effect.quant)
#
#
# }



# X.pred <- as.matrix(X %>% dplyr::select(any_of(classes)))
# betas <- as.matrix(apply(testrun$postb,c(1,2),mean))
#
# mu.hat <- X.pred %*% betas
#
# e.mu <- exp(mu.hat)
# pred.MNL <- e.mu / rowSums(e.mu)
#
#
# y <- apply(as.matrix(Y %>% dplyr::select(any_of(classes))), 1, function(p) sample(1:5, 1, prob = p))
#
# YY <- matrix(0, ncol = ncol(Y), nrow = nrow(Y))
# for(i in 1:length(y)){YY[i,y[i]] <- 1}
#
#
# df <- data.frame(X.pred, Class = factor(y))
# # Fit the multinomial logit model with neural network
# formul <- paste0("Class ~", paste0(classes, collapse = "+"))
#
# test <- multinom_nnet_MW(formul, data = df, probs=as.matrix(Y %>% dplyr::select(any_of(classes))))
#
# # Predict the probabilities for the training data
# predicted_probs <- predict(test, type = "probs")
#
# summary(Y[,-c(1,2)])
# summary(pred.MNL)
# summary(predicted_probs)
