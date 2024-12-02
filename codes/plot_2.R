LUC <- read.csv("./input/LUM_change.csv")
class.mapping.file <- "./input/class_mapping.csv"                             #
# STAGE 1 of prior model, load grid for cluster/loop
class.mapping <- read.csv(class.mapping.file)
colnames(class.mapping)[1] <- "Class"

years <- c(2000,2010)
Y <- LUC %>% dplyr::select(CELLCODE, area, paste0("LUM", years))
colnames(Y)[c(3,4)] <- c("LU_from", "LU_to")

Y <- Y %>% filter(LU_from!=0, LU_to!=0) %>%
  left_join(class.mapping %>% rename("LU_from"="Class") %>% dplyr::select(-Name), by = join_by(LU_from)) %>%
  rename("LU_from_mapped"="LU_mapping") %>%
  left_join(class.mapping %>% rename("LU_to"="Class") %>% dplyr::select(-Name), by = join_by(LU_to)) %>%
  rename("LU_to_mapped"="LU_mapping") %>%
  group_by(LU_from_mapped, LU_to_mapped) %>%
  summarise(area=sum(area))

Y.test <- Y %>% group_by(LU_from_mapped) %>% mutate(area=area/sum(area))

# Create the faceted bar plot
q <- ggplot(Y.test %>% filter(!LU_from_mapped%in%( "NotRel"), !LU_to_mapped%in%( "NotRel")), aes(x = LU_to_mapped, y = area)) +
  geom_bar(stat = "identity", fill = "steelblue") +   # Use geom_bar with stat = "identity" for bar heights based on values
  facet_wrap(~ LU_from_mapped, ncol=3, scales = "free") +    # Facet by LU_from_mapped, allow free scaling of x-axis
  theme_bw() +                                    # Use a minimal theme
  labs(x = "Land Use Type", y = "Area share",         # Label x and y axes
       title = "Bar Plot of LUC in Europe") +  # Title of the plot
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


Y.test2 <- Y %>% filter(LU_from_mapped!=LU_to_mapped) #%>% group_by(LU_from_mapped) %>% mutate(area=area/sum(area))


# Create the faceted bar plot
qq <- ggplot(Y.test2 %>% filter(!LU_from_mapped%in%( "NotRel"), !LU_to_mapped%in%( "NotRel")), aes(x = LU_to_mapped, y = area)) +
  geom_bar(stat = "identity", fill = "steelblue") +   # Use geom_bar with stat = "identity" for bar heights based on values
  facet_wrap(~ LU_from_mapped, ncol=3, scales = "free") +    # Facet by LU_from_mapped, allow free scaling of x-axis
  theme_bw() +                                    # Use a minimal theme
  labs(x = "Land Use Type", y = "Area in square km",         # Label x and y axes
       title = "Bar Plot of LUC in Europe") +  # Title of the plot
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability



ggsave(filename = paste0("plots/bar_self.jpg"),
       plot = q,
       device = "jpeg",
       width = 12,  # Width in inches
       height = 7,  # Height in inches
       dpi = 300)  # Resolution in dots per inch

ggsave(filename = paste0("plots/bar_noself.jpg"),
       plot = qq,
       device = "jpeg",
       width = 12,  # Width in inches
       height = 7,  # Height in inches
       dpi = 300)  # Resolution in dots per inch
