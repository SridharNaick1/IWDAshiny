# var <- c("score2.1","score2.2","score2.2.1","score2.4.5","score4.5","score4")
# grep("^score\\d+(\\.\\d{1,1})$",var,perl=TRUE,value = TRUE)
# 
# 
# summary(iris)
 library("collapsibleTree")
# 
# 
# pattern <- "^score\\d+?$"
# pattern_theme<- "^score\\d+(\\.\\d{1,1})$"
# pattern_indi<- "^score\\d+(\\.\\d{1,1})$"
# 
# df<- readRDS("fiji_newscored_cleaned_Oct19.rds")
# 
# temp<- colnames(df[,grep(pattern,colnames(df))])

Dimensions<- read.csv("dashboard.csv")


# collapsibleTree(
#   Dimensions,
#   hierarchy = c("Dimension", "Theme","Indicator"),
#   width = 800,
#   zoomable = TRUE
# )

library(dplyr)


Dimensions %>%
  group_by(Dimension,Theme, Indicator) %>%
  summarize(`Number of Indicators` = n()) %>%
  collapsibleTreeSummary(
    hierarchy = c("Dimension","Theme", "Indicator"),
    root = "Dimensions",
    width = 800,
    attribute = "Number of Indicators",
    zoomable = TRUE
  )

