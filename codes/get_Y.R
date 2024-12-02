LUC <- read.csv("./input/LUM_change.csv")

Y <- LUC %>% dplyr::select(CELLCODE, area, paste0("LUM", years))
colnames(Y)[c(3,4)] <- c("LU_from", "LU_to")

Y <- Y %>% filter(LU_from!=0, LU_to!=0, CELLCODE%in%curr.n) %>%
  left_join(class.mapping %>% rename("LU_from"="Class") %>% dplyr::select(-Name), by = join_by(LU_from)) %>%
  rename("LU_from_mapped"="LU_mapping") %>%
  left_join(class.mapping %>% rename("LU_to"="Class") %>% dplyr::select(-Name), by = join_by(LU_to)) %>%
  rename("LU_to_mapped"="LU_mapping") %>%
  group_by(CELLCODE, LU_from_mapped, LU_to_mapped) %>%
  summarise(area=sum(area)) %>%
  filter(LU_from_mapped==curr.lu) %>%
  pivot_wider(id_cols = c(CELLCODE, LU_from_mapped), names_from = "LU_to_mapped", values_from = "area", values_fill=0) %>%
  dplyr::select(CELLCODE, LU_from_mapped, any_of(classes))
Y$total <- rowSums(Y[,setdiff(colnames(Y),c("CELLCODE", "LU_from_mapped"))])
Y[,setdiff(colnames(Y),c("CELLCODE", "LU_from_mapped"))] <- Y[,setdiff(colnames(Y),c("CELLCODE", "LU_from_mapped"))]/Y$total
Y <- Y %>% dplyr::select(-total) %>%
  rename("LU_from" = "LU_from_mapped") %>% na.omit() %>% ungroup()

if(ncol(Y)!=(length(classes)+2)){
  missing <- setdiff(classes, colnames(Y)[-c(1,2)])
  missing.df <- as.data.frame(matrix(0, ncol = length(missing), nrow = nrow(Y)))
  colnames(missing.df) <- missing
  Y <- Y  %>% bind_cols(missing.df)
}

Y <- Y[,c("CELLCODE","LU_from",classes)]
