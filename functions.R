###################
# functions.R
# 
# Need some functions for your ui logic or server?? Define em' here.
###################



################## functions ##############################

default_age_table_maker <- function(x)
{


x_dim <- x %>% mutate(category = case_when(round(x$dimen, 0) == 0 ~ "Most deprived", 
                                           round(x$dimen, 0) ==  1 ~ "Deprived", 
                                           round(x$dimen, 0) == 2 ~ "Somewhat deprived", 
                                           round(x$dimen, 0) == 3 ~ "Least deprived"))

x_dim <- x_dim %>%  mutate(age = case_when(data$age >= 18  & data$age <= 24 ~ 1, data$age >= 25  & data$age <= 59 ~ 2, data$age > 59  ~ 3))


x_dim <-  na.omit(x_dim)

x_dim<- x_dim %>% mutate(age = case_when(x_dim$age == 1 ~ "18-24", x_dim$age == 2 ~ "25-59", x_dim$age == 3 ~ "60+"))


plott3 <- ggplot(x_dim, aes(x=x_dim$category,  fill=factor(x_dim$newcat))) +
  geom_bar(position="dodge") +  scale_fill_brewer(name = "Age group")+
  ylab("Count") + theme(axis.text.x = element_text(size = 10, angle = 30))


new_table <- x_dim %>%
  tab_cells(age) %>%
  tab_cols(category) %>% 
  tab_stat_cases () %>% 
  tab_pivot() %>% htmlTable(caption = "")


return(new_table)

}

theme_age_table_maker <- function(x)
{

x_dim <- x %>%  mutate(age = case_when(data$age >= 18  & data$age <= 24 ~ 1, data$age >= 25  & data$age <= 59 ~ 2, data$age > 59  ~ 3))


x_dim <-  na.omit(x_dim)

x_dim<- x_dim %>% mutate(age = case_when(x_dim$age == 1 ~ "18-24", x_dim$age == 2 ~ "25-59", x_dim$age == 3 ~ "60+"))


new_table <- x_dim %>%
  tab_cells(age) %>%
  tab_cols(category) %>% 
  tab_stat_cases () %>% 
  tab_pivot() %>% htmlTable(caption = "")



return(new_table)
}



theme_sex_table_maker <- function(x)
{


x_dim <- x %>% mutate(gender = case_when(data$sex == 1 ~ "Male", 
													data$sex == 2 ~ "Female"))



new_table <- x_dim %>%
  tab_cells(gender) %>%
  tab_cols(category) %>% 
  tab_stat_cases () %>% 
  tab_pivot() %>% htmlTable(caption = "")

return(new_table)
}





default_sex_table_maker <- function(x)
{

x_dim <- x %>% mutate(category = case_when(round(x$dimen, 0) == 0 ~ "Most deprived", 
                                           round(x$dimen, 0) ==  1 ~ "Deprived", 
                                           round(x$dimen, 0) == 2 ~ "Somewhat deprived",
                                           round(x$dimen, 0) == 3 ~ "Least deprived"))

x_dim <- x_dim %>% mutate(gender = case_when(data$sex == 1 ~ "Male", data$sex == 2 ~ "Female"))



new_table <- x_dim %>%
  tab_cells(gender) %>%
  tab_cols(category) %>% 
  tab_stat_cases () %>% 
  tab_pivot() %>% htmlTable(caption = "")


return(new_table)
}



default_table_maker <- function(x)
{

x_dim <- x %>% mutate(category = case_when(round(x$dimen, 0) == 0 ~ "Most deprived", 
                                           round(x$dimen, 0) ==  1 ~ "Deprived", 
                                           round(x$dimen, 0) == 2 ~ "Somewhat deprived",
                                           round(x$dimen, 0) == 3 ~ "Least deprived"))
x_dim <- within(x_dim, category <- factor(category, 
                                          levels=names(sort(table(category), 
                                                            decreasing=FALSE))))
agg = aggregate(x_dim,
                by = list(x_dim$category),
                FUN = length)

agg$category = agg$Group.1
agg$count = agg$dimen

agg <- agg[,c("category", "count")]
formattable(agg)

return(formattable(agg))
}




theme_sex_plotter <- function(x, ap_text, theme, to_plot_theme)
{
x <- x %>% mutate(newcat = case_when(data$sex == 1 ~ "Male", data$sex == 2 ~ "Female"))
fill_name <- "sex"
	
subs <- subset(x, x$newcat == "Male")
subsF <- subset(x, x$newcat == "Female")

texttheme <- HTML("<b>", paste(ap_text), " - ", paste(theme), "</b></br>The mean score for ", paste(ap_text), " is <b>",round(mean(x[[to_plot_theme]],), 2),"</b>. For men: <b>",round(mean(subs[[to_plot_theme]]),2),"</b> and for women: <b>", round(mean(subsF[[to_plot_theme]]),2),"</b>." )


x$category <- factor(x$category,levels = c("Most deprived", "Deprived", "Somewhat deprived", "Least deprived"))



  	#label_text <- paste("IDM scores for", paste(ap_text), "deprivation", "(", paste(theme) ,")")
	label_text <- paste("Number of people deprived in each category", "(", paste(theme) ,")")

plott <- ggplot(x, aes(x=x$category,  fill=factor(x$newcat))) +
  geom_bar(position="dodge") + scale_fill_manual(values = c("#b8c3e3", "#6282c1"))+ labs(fill="Sex")+
  xlab(label_text)+ylab("Count") + 	theme(axis.text.x = element_text(size = 12, angle = 25))

return(list(plott, texttheme))


}


theme_age_plotter <- function(x, ap_text, theme, to_plot_theme)
{

x <- x %>%  mutate(newcat = case_when(data$age >= 18  & data$age <= 24 ~ 1, data$age >= 25  & data$age <= 59 ~ 2, data$age > 59  ~ 3))
x <-  na.omit(x)
x<- x %>% mutate(newcat = case_when(x$newcat == 1 ~ "18-24", x$newcat == 2 ~ "25-59", x$newcat == 3 ~ "60+"))


subset1 <- subset(x, x$newcat == "18-24")	
subset2 <- subset(x, x$newcat == "25-59")
subset3 <- subset(x, x$newcat ==  "60+")

fill_name <- "age"

texttheme <- HTML("<b>",paste(ap_text)," - ",paste(theme),"</b></br>The mean score for ",paste(ap_text)," is: <b>",round(mean(x[[to_plot_theme]]), 2),"</b>. For age 18 - 24 is: <b>",round(mean(subset1[[to_plot_theme]]), 2),"</b>. For age 25 - 59 is: <b>", round(mean(subset2[[to_plot_theme]]), 2),"</b>. For age over 60 is: <b>", round(mean(subset3[[to_plot_theme]]), 2), "</b>.")


x$category <- factor(x$category,levels = c("Most deprived", "Deprived", "Somewhat deprived", "Least deprived"))

#label_text <- paste("IDM scores for", paste(ap_text), "deprivation", "(", paste(theme) ,")")

label_text <- paste("Number of people deprived in each category", "(", paste(theme) ,")")

plott <- ggplot(x, aes(x=x$category,  fill=factor(x$newcat))) +
  geom_bar(position="dodge") + scale_fill_manual(values = c("#b8c3e3", "#6282c1", "#2c325d"))+ labs(fill="Age group")+
  xlab(label_text)+ylab("Count") + 	theme(axis.text.x = element_text(size = 12, angle = 25))

return(list(plott, texttheme))

}



bar_age_plotter <- function(x, ap_text)

{
 
  
  x_dim <- x %>% mutate(category = case_when(round(x$dimen, 0) == 0 ~ "Most deprived", 
                                                          round(x$dimen, 0) ==  1 ~ "Deprived", 
                                                          round(x$dimen, 0) == 2 ~ "Somewhat deprived", 
                                                          round(x$dimen, 0) == 3 ~ "Least deprived"))
  
  x_dim <- x_dim %>%  mutate(newcat = case_when(data$age >= 18  & data$age <= 24 ~ 1, data$age >= 25  & data$age <= 59 ~ 2, data$age > 59  ~ 3))
  
  subsetted1 <- subset(x_dim, x_dim$newcat == 1)
  subsetted2 <- subset(x_dim, x_dim$newcat == 2)
  subsetted3 <- subset(x_dim, x_dim$newcat == 3)
  
  textt <- HTML("The mean score for", paste(ap_text),"is: <b>",round(mean(x_dim$dimen, na.rm=TRUE), 2),"</b>. For age 18 - 24 is: <b>",round(mean(subsetted1$dimen, na.rm=TRUE), 2),"</b>. For age 25 - 59 is: <b>", round(mean(subsetted2$dimen, na.rm=TRUE), 2),"</b>. For age over 60 is: <b>", round(mean(subsetted3$dimen, na.rm=TRUE), 2), "</b>.")
  
x_dim$category <- factor(x_dim$category,levels = c("Most deprived", "Deprived", "Somewhat deprived", "Least deprived"))

  
  x_dim <-  na.omit(x_dim)
  
  x_dim<- x_dim %>% mutate(newcat = case_when(x_dim$newcat == 1 ~ "18-24", x_dim$newcat == 2 ~ "25-59", x_dim$newcat == 3 ~ "60+"))
  
  	# label_text <- paste("IDM scores for", paste(ap_text), "deprivation")
	label_text <- paste("Number of people deprived in each category")

  plott3 <- ggplot(x_dim, aes(x=x_dim$category,  fill=factor(x_dim$newcat))) +
    geom_bar(position="dodge") +  scale_fill_manual(values = c("#b8c3e3", "#6282c1", "#2c325d"))+ labs(fill="Age group")+
    xlab(label_text)+ylab("Count") + theme(axis.text.x = element_text(size = 10, angle = 30))
  

  return(list(plott3, textt))
}



bar_gender_plotter <- function(x, ap_text)

{
	x_dim <- x %>% mutate(category = case_when(round(x$dimen, 0) == 0 ~ "Most deprived", 
                                               round(x$dimen, 0) ==  1 ~ "Deprived", 
                                               round(x$dimen, 0) == 2 ~ "Somewhat deprived",
											   round(x$dimen, 0) == 3 ~ "Least deprived"))
											   									   
	x_dim <- x_dim %>% mutate(gender = case_when(data$sex == 1 ~ "Male", data$sex == 2 ~ "Female"))
  
	subsetted <- subset(x_dim, x_dim$gender == "Male")
	subsettedF <- subset(x_dim, x_dim$gender == "Female")
  
	textt <- HTML("The mean score for ", paste(ap_text), " is <b>",round(mean(x_dim$dimen, na.rm=TRUE), 2),"</b>. For men: <b>",round(mean(subsetted$dimen, na.rm=TRUE),2),"</b> and for women: <b>", round(mean(subsettedF$dimen, na.rm=TRUE),2),"</b>." )
	
	
x_dim$category <- factor(x_dim$category,levels = c("Most deprived", "Deprived", "Somewhat deprived", "Least deprived"))

  
	#label_text <- paste("IDM scores for", paste(ap_text), "deprivation")
	label_text <- paste("Number of people deprived in each category")
	plott1 <- ggplot(x_dim, aes(x=x_dim$category,  fill=factor(x_dim$gender))) +
    geom_bar(position="dodge") + scale_fill_manual(values = c("#b8c3e3", "#6282c1"))+ labs(fill="Sex")+
    xlab(label_text)+ylab("Count") + 	theme(axis.text.x = element_text(size = 10, angle = 30))
  
  return(list(plott1, textt))
}

bar_default_plotter <- function(x, ap_text)
{


x_dim <- x %>% mutate(category = case_when(round(x$dimen, 0) == 0 ~ "Most deprived", 
                                               round(x$dimen, 0) ==  1 ~ "Deprived", 
                                               round(x$dimen, 0) == 2 ~ "Somewhat deprived",
											   round(x$dimen, 0) == 3 ~ "Least deprived"))
  

  
x_dim$category <- factor(x_dim$category,levels = c("Most deprived", "Deprived", "Somewhat deprived", "Least deprived"))

  
  	textt <- HTML("The mean score for ", paste(ap_text), " is <b>",round(mean(x_dim$dimen, na.rm=TRUE), 2),"</b>." )
	
	#label_text <- paste("IDM scores for", paste(ap_text), "deprivation")
	label_text <- paste("Number of people deprived in each category")

	plott2 <- ggplot(x_dim, aes(x=x_dim$category,  fill=factor(x_dim$category))) +
    geom_bar() + scale_fill_manual(values = c("#b8c3e3","#6282c1", "#7874b6", "#2c325d"))+labs(fill="State")+#scale_fill_brewer(name = "State")+
    xlab(label_text)+ylab("Count") + 	
    theme(axis.text.x = element_text(size = 10, angle = 30))
	
  return(list(plott2, textt))

}




sunburst_plotter <- function(x_wise, ap_text)

{


x_wise <- x_wise %>% mutate(category = case_when(round(x_wise$dimen, 0) == 0 ~ "Most deprived", 
                                                       round(x_wise$dimen, 0) ==  1 ~ "Deprived", 
                                                       round(x_wise$dimen, 0) == 2 ~ "Somewhat deprived", 
                                                       round(x_wise$dimen, 0) == 3 ~ "Least deprived"))


x_dim <- x_wise %>% mutate(gender = case_when(data$sex == 1 ~ "Male", data$sex == 2 ~ "Female"))

x_dim <- within(x_dim, category <- factor(category, 
                                                levels=names(sort(table(category), 
                                                                  decreasing=FALSE))))
one <- x_dim
two <- subset(x_dim, x_dim$gender == "Male")
three <- subset(x_dim, x_dim$gender == "Female")
four <- subset(two, two$category == "Somewhat deprived")
five <- subset(two, two$category == "Least deprived")
six <- subset(two, two$category == "Most deprived")
seven <- subset(two, two$category == "Deprived")
eight <- subset(three, three$category == "Somewhat deprived")
nine <- subset(three, three$category == "Least deprived")
ten <- subset(three, three$category == "Most deprived")
eleven <- subset(three, three$category == "Deprived")

vec <- c(nrow(one),nrow(two),nrow(three),nrow(four),nrow(five),nrow(six),nrow(seven),nrow(eight),nrow(nine),nrow(ten), nrow(eleven))

p <- plot_ly(
  labels = c(ap_text, "Men",  "Women", 
             "Somewhat deprived", "Least deprived", "Most deprived", "Deprived", 
             "somewhat deprived", "least deprived", "most deprived", "deprived"),
  parents = c("",   ap_text, ap_text,   "Men",     "Men",  "Men", "Men",  "Women",   "Women","Women","Women"),
  values = vec,
  type = 'sunburst',
  branchvalues = 'total'
)
return(p)
}

sunburst_text_stat <- function(x, ap_text)

{
	x_dim <- x %>% mutate(category = case_when(x$dimen == 0 ~ "Most deprived", 
                                                      x$dimen ==  1 ~ "Deprived", 
                                                      x$dimen == 2 ~ "Somewhat deprived", 
                                                      x$dimen == 3 ~ "Least deprived"))

	x_dim <- x_dim %>% mutate(gender = case_when(data$sex == 1 ~ "Male", data$sex == 2 ~ "Female"))

	just_dep <- subset(x_dim, ((x_dim$category == "Most deprived") | (x_dim$category == "Deprived")))
	dep_per <- (nrow(just_dep)/nrow(x_dim))*100


	male <- subset(just_dep, just_dep$gender == "Male")
	male_dep_per <- (nrow(male)/nrow(just_dep))*100

	female_dep_per <- 100 - male_dep_per
    
	textt <- HTML( "</br></br></br></br></br></br></br></br><b>", round(dep_per,2),"%</b> were deprived out of the ", paste(ap_text)," sample. <b>",round(male_dep_per,2),"%</b>. amongst them were men and <b>",round(female_dep_per, 2),"%</b> were women.")
	return(textt)

}


import.clean <- function(csv)
{
  # Import data
  fiji_scored <- read.csv(csv)
  
  fiji_jst_scores <- fiji_scored
  # dplyr::select(sex, age.categories, area, EA, sector, ethnicity, disability3, relationship,
  #                score1, score2, score3,
  #                score4, score5, score6, score7, score8, score9, score12, score13, score14, score15)
  
  for (i in 1:ncol(fiji_jst_scores)){
    if (grepl('score', colnames(fiji_jst_scores)[i])) {
      fiji_jst_scores[i] <- fiji_jst_scores[i] %>%
        round() %>%
        sapply(paste0) %>%
        factor(ordered = TRUE, exclude = c(NA, "NA"))
    }
  }
  
  # fiji_jst_scores <- fiji_scored %>%
  #   select(HHID_unique, age) %>%
  #   bind_cols(fiji_jst_scores)
  
  # Set household head
  fiji_jst_scores <- fiji_scored %>%
    filter(relationship == 1) %>%
    select(sex, HHID_unique) %>%
    dplyr::rename(hhh_sex = sex) %>%
    right_join(fiji_jst_scores, by = "HHID_unique")
  
  # Set ethnicity dummy variables
  fiji_jst_scores <- fiji_scored %>%
    mutate(itaukei = ethnicity == 1, indian = ethnicity == 2, other.ethnicity = ethnicity > 2) %>%
    select(itaukei, indian, other.ethnicity) %>%
    bind_cols(fiji_jst_scores)
  
  # Set sector dummy variables
  fiji_jst_scores <- fiji_scored %>%
    mutate(rural = sector == 1, urban = sector == 2, informal = sector == 3) %>%
    select(rural, urban, informal) %>%
    bind_cols(fiji_jst_scores)
  
  # Convert factors
  fiji_jst_scores$hhh_sex <- as.factor(fiji_jst_scores$hhh_sex)
  fiji_jst_scores$HHID_unique <- as.factor(fiji_jst_scores$HHID_unique)
  
  # Add unrounded scores
  fiji_jst_scores <- fiji_scored %>%
    select(score1, score2, score3,score4, score5, score6, score7, score8, score9, 
           score12, score13, score14, score15) %>%
    rename_all(.funs = funs(paste0(., '_numeric'))) %>%
    bind_cols(fiji_jst_scores)
  
  return(fiji_jst_scores)
}

