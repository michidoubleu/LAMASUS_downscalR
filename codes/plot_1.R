rm(list=ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

load("./input/MINDSTEP_betas.RData")

custom_labels <- c("Negative","Positive", "Neutral")

betas <- betas %>% mutate(ks=as.character(ks), ks = ifelse(grepl("_", ks), "Crop.prod", ks)) %>% group_by(Ns, ks, lu.from, lu.to) %>% summarise(value=sum(value)) %>%mutate(value=ifelse(value>0,1,ifelse(value<0,-1,0))) %>% mutate(value=as.factor(value))

betas$lu.from <- factor(betas$lu.from, levels = c("CrpLnd", "Grass", "Forest", "OthNatLnd"))
betas$lu.to <- factor(betas$lu.to, levels = c("CrpLnd", "Grass", "Forest", "OthNatLnd"))

i <- "AustriaReg"
for(i in unique(betas$Ns)){


p <- ggplot(betas %>% filter(Ns==i, !ks%in%c("X","Y", "Crop.prod", "NotRel", "HarvCost")), aes(x = lu.to, y = lu.from, fill=value)) +
  geom_raster() +
  scale_fill_manual(values = c("1" = "#64B5F6", "0" = "white", "-1" = "#E57373"),labels = custom_labels) +
  labs(y="Initial land use class", x="Change towards land use class", title=paste0("Parameter matrix plot - ", i), fill="parameter sign") +
  facet_wrap(~ ks, ncol = 5, scales = "free") +
  theme_bw() + theme(axis.text.x=element_text(size=8, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=8),
                     axis.title = element_text(size = 10),
                     plot.title=element_text(size=11),
                     legend.title = element_text())


  ggsave(filename = paste0("plots/",i,"_plot.jpg"),
         plot = p,
         device = "jpeg",
         width = 14,  # Width in inches
         height = 7,  # Height in inches
         dpi = 300)  # Resolution in dots per inch

}
