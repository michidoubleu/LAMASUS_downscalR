X.lu.init <- LUC %>% dplyr::select(CELLCODE, area, paste0("LUM", years[1]))
colnames(X.lu.init)[c(3)] <- c("LU_from")

X.lu.init <- X.lu.init %>% filter(LU_from!=0, CELLCODE%in%curr.n) %>%
  left_join(class.mapping %>% rename("LU_from"="Class") %>% dplyr::select(-Name), by = join_by(LU_from)) %>%
  rename("LU_from_mapped"="LU_mapping") %>%
  group_by(CELLCODE, LU_from_mapped) %>%
  summarise(area=sum(area)) %>%
  pivot_wider(id_cols = c(CELLCODE), names_from = "LU_from_mapped", values_from = "area", values_fill=0) %>%
  dplyr::select(CELLCODE, any_of(classes)) %>%
  ungroup() %>%
  na.omit()


rm("LUC")
#### to be added, as many drivers as possible!

#load the mapping from SimU to the grid
ID.mapping <- read.csv("./input/CELLCODE_SimU_mapping.csv")
ID.mapping <- ID.mapping %>% filter(CELLCODE%in%curr.n) %>% distinct()


xmat.simu <- rgdx.param(file.path("./input/Xmat.gdx"), "xmat")
colnames(xmat.simu) <- c("SimUID", "REGION", "var", "value")
xmat.simu <- xmat.simu %>% filter(!var%in%c("Intercept","X","Y","MngFor","OthNatLnd","PriFor","Grass","urban","CrpLnd","PltFor")) %>% dplyr::select(-REGION) %>% distinct() %>% pivot_wider(id_cols = SimUID, names_from = "var", values_from = "value", values_fill = 0) %>% mutate(SimUID=as.numeric(as.character(SimUID)))

xmat.simu <- ID.mapping %>% left_join(xmat.simu)

crops <- which(nchar(colnames(xmat.simu))==4)
crop.prod <- rowSums(xmat.simu[,crops])

xmat.simu <- xmat.simu[,-crops]
xmat.simu$crop.prod <- crop.prod


xmat.simu <- xmat.simu %>% dplyr::select(-c("Soil2_Medium", "Soil3_Heavy",  "Soil4_Stony",  "Soil5_Peats", "Slope", "Altitude"))

X <- X.lu.init %>% left_join(xmat.simu, by="CELLCODE") %>% na.omit() %>% dplyr::select(-SimUID)
#
# X$Slope <- ifelse(X$Slope>=0.06,1,0)
# X$Altitude <- ifelse(X$Altitude>=0.03,1,0)
