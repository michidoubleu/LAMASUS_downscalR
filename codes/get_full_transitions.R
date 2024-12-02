LUC <- read.csv("./input/LUM_change.csv")

Y <- LUC %>% dplyr::select(CELLCODE, area, paste0("LUM", years))
colnames(Y)[c(3,4)] <- c("LU_from", "LU_to")

Y <- Y %>% filter(LU_from!=0, LU_to!=0) %>%
     left_join(class.mapping %>% rename("LU_from"="Class") %>% dplyr::select(-Name), by = join_by(LU_from)) %>%
     rename("LU_from_mapped"="LU_mapping") %>%
     left_join(class.mapping %>% rename("LU_to"="Class") %>% dplyr::select(-Name), by = join_by(LU_to)) %>%
     rename("LU_to_mapped"="LU_mapping") %>%
     group_by(CELLCODE, LU_from_mapped, LU_to_mapped) %>%
     summarise(area=sum(area)) %>%
     pivot_wider(id_cols = c(CELLCODE, LU_from_mapped), names_from = "LU_to_mapped", values_from = "area", values_fill=0)

Y$total <- rowSums(Y[,setdiff(colnames(Y),c("CELLCODE", "LU_from_mapped"))])
Y <- Y %>% dplyr::select(-total) %>% rename("LU_from" = "LU_from_mapped") %>% na.omit() %>% ungroup()
Y <- Y %>% pivot_longer(cols = where(is.numeric), names_to = "LU_to", values_to = "value")
Y <- Y %>% mutate(LU_from=as.factor(LU_from), LU_to=as.factor(LU_to))
write.csv(Y, file="for_Albert.csv", row.names = F)
