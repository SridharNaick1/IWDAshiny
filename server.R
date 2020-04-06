###################
# server.R
# 
Dimensions<- read.csv("dashboard.csv")
data <- readRDS("fiji_newscored_cleaned_Oct19.rds")

to_compare <- c("score1", "score2","score3", "score4","score5", "score6","score7", "score8","score9", "score12","score13", "score14", "score15")

source('./functions.R')
# For all your server needs 
###################
server <- function(input, output, session) {

  	to_download <- reactiveValues()
  # Create plot  	
	dash_table_switch <- reactiveValues()
  output$SelectedPlot <- renderPlotly({

	dimensions <- data[,to_compare]
	dimensions
	colnames(dimensions) <- var_label(dimensions)

####################################################################### Dashboard start ######
  if(input$catogerytype == "age")
  
  {
    
	new_dimension <- dimensions %>% mutate(newcat = case_when(data$age >= 18  & data$age <= 24 ~ 1, data$age >= 25  & data$age <= 59 ~ 2, data$age > 59  ~ 3))
	new_dimension <- na.omit(new_dimension)
	 
	label = c("18-24", "25-59", "59+")
	brk = c(1,2,3)
		
	means<-aggregate(new_dimension,by=list(new_dimension$newcat), mean)
	means<-means[,2:length(means)]
	means.long<-melt(means,id.vars="newcat")
	
	means.long <- means.long %>%
	mutate(name = fct_reorder(means.long$variable, (means.long$value)))
	
	filll <- "Age"
	means.long <- means.long %>% mutate(newcat = case_when(means.long$newcat == 1 ~ "18-24", means.long$newcat == 2 ~ "25-59", means.long$newcat == 3 ~ "60+"))
	
	pallet <- scale_fill_manual(values = c("#b8c3e3", "#6282c1", "#2c325d"))
	
	
	
	output$DashSelectedPlot <- renderTable({
		
	new_dimension <- dimensions %>% mutate(newcat = case_when(data$age >= 18  & data$age <= 24 ~ 1, data$age >= 25  & data$age <= 59 ~ 2, data$age > 59  ~ 3))
	new_dimension <- na.omit(new_dimension)
	new_dimension <- new_dimension %>% mutate(age = case_when(new_dimension$newcat == 1 ~ "18-24", new_dimension$newcat == 2 ~ "25-59", new_dimension$newcat == 3 ~ "60+"))
	agg = aggregate(new_dimension,
                by = list(new_dimension$age),
                FUN = mean)
	agg <- agg %>% mutate(age = case_when(agg$newcat == 1 ~ "18-24", agg$newcat == 2 ~ "25-59", agg$newcat == 3 ~ "60+"))
	agg <- agg[,colnames(new_dimension)]
	agg <- agg[, c(15, 1:13)]
	formattable(agg)
	})

	
  }
  if(input$catogerytype == "sex")
  
  {

    new_dimension <- dimensions %>% mutate(newcat = case_when(data$sex == 1 ~ 1, data$sex == 2 ~ 2))
    new_dimension <- na.omit(new_dimension)
		
  	means<-aggregate(new_dimension,by=list(new_dimension$newcat), mean)
	means<-means[,2:length(means)]
	means.long<-melt(means,id.vars="newcat")
	
	
	means.long <- means.long %>%
	mutate(name = fct_reorder(means.long$variable, (means.long$value)))
	
	label = c("Male", "Female")
	brk = c(1,2)
	filll <- "Sex"

	pallet <- scale_fill_manual(values = c("#b8c3e3", "#6282c1"))
	means.long <- means.long %>% mutate(newcat = case_when(means.long$newcat == 1 ~ "Male", means.long$newcat == 2 ~ "Female"))

	
	output$DashSelectedPlot <- renderTable({
	new_dimension <- dimensions %>% mutate(newcat = case_when(data$sex == 1 ~ 1, data$sex == 2 ~ 2))
	new_dimension <- na.omit(new_dimension)
	new_dimension <- new_dimension %>% mutate(sex = case_when(new_dimension$newcat == 1 ~ "Male", new_dimension$newcat == 2 ~ "Female"))
	agg = aggregate(new_dimension,
                by = list(new_dimension$sex),
                FUN = mean)
	agg <- agg %>% mutate(sex = case_when(agg$newcat == 1 ~ "Male", agg$newcat == 2 ~ "Female"))
	agg <- agg[,colnames(new_dimension)]
	agg <- agg[, c(15, 1:13)]
	
	formattable(agg)
			 		 
			 })




  }
  
  if(input$catogerytype == "default")
  
  {
	plot_vec <- 0
	plot_name <- 0
	for (val in var_label(dimensions))
	{
	plot_vec[val] <-   mean(na.omit(dimensions[[val]]))

	plot_name[val] <-  val
	
	}
	
	new_datafram <- data.frame(plot_vec)
	new_datafram_name <- data.frame(plot_name)
	final_df <- cbind(new_datafram, new_datafram_name )
	final_df<-final_df[2:14,]
	
	
	final_df <- final_df %>%
	 mutate(name = fct_reorder(final_df$plot_name, desc(final_df$plot_vec)))
	
	means.long <- final_df
	name <- final_df$name
	value <- final_df$plot_vec
	newcat <- final_df$name
	filll <- "Dimensions"
	## pallets
	pallet <- scale_fill_grey(start = 0.65, end = 0.35, name = filll)


	output$DashSelectedPlot <- renderTable({

new_dimension <- na.omit(dimensions)
new_dimension <- new_dimension %>% mutate(new = case_when(new_dimension$Food != 5 ~ 1))
agg = aggregate(new_dimension,
                by = list(new_dimension$new),
                FUN = mean)
agg <- agg[,colnames(new_dimension)]
agg <- agg[, c(1:13)]
formattable(agg)
			 		 
			 })
	
	


  }
  
	plott <- 	ggplot(means.long,aes(x=name,y=value,fill=factor(newcat)))+
	geom_bar(stat="identity", position = 'dodge')+
	pallet +
	xlab("dimensions")+ylab("Mean")+ labs(fill=filll) + 
	theme(axis.text.x = element_text(size = 10, angle = 30))	
	
	to_download$dash_plt <- plott
	
	ggplotly(plott) %>% config(displayModeBar = F)
	
  

	
  })
  
  
  output$down <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$dash_plt)
 dev.off()
 }) 
  
  
 


  
  output$TreePlot <-  renderCollapsibleTree({
  
  	tree_graph <- Dimensions %>%
	group_by(Dimension,Theme, Indicator) %>%
	summarize(`Number of Indicators` = n()) %>%
	collapsibleTreeSummary(
    hierarchy = c("Dimension","Theme", "Indicator"),
    root = "Dimensions",
    width = 600,
	height = 700,
    attribute = "Number of Indicators",
    zoomable = FALSE
  )
  	tree_graph
  
  })
  
output$ICCtable <- renderTable({

fiji_jst_scores<- import.clean("fiji_newscored_cleaned_Oct19.csv")

## Get plain dim names
just_dim_names <- colnames(fiji_jst_scores)[grepl('numeric', 
                                                  colnames(fiji_jst_scores))]

# BUG rounding from factor to numeric and back
ICC_df <- as.data.frame(just_dim_names)
for (i in 1:length(just_dim_names)) {
  ICCresults <- ICCest(x = fiji_jst_scores[,'HHID_unique'], 
                       y = unlist(fiji_jst_scores[just_dim_names[i]]))
  ICC_df[i, 2] <- ICCresults$ICC
  ICC_df[i, 3] <- ICCresults$LowerCI
  ICC_df[i, 4] <- ICCresults$UpperCI
}

colnames(ICC_df) <- c('Dimension', 'ICCest', 'LowerCI', 'UpperCI')



all_dim <- data[, grep("^score\\d+$",colnames(data),perl=TRUE,value = TRUE)]

all_dim <- all_dim[, c(1,2,13,3,4,5,6,7,8,9,10,11,12)]

var <- var_label(all_dim)


ICC_df$Dimension<-var
ICC_df




formattable(ICC_df)


})
 






observeEvent(input$perc,{

 output$Ctable <- renderTable({
  
  
ase <- data[, c("sex","age","sector","ethnicity","relationship", "disability3")]

ase <- ase %>% mutate(age = case_when(ase$age >= 18 & ase$age <= 35 ~ "18-35", 
                                               ase$age >= 36 & ase$age <= 50 ~ "36-50", 
                                               ase$age >= 51 & ase$age <= 65 ~ "51-65",
                                               ase$age >= 66 ~ "66+"))

ase <- ase %>% mutate(sex = case_when(ase$sex == 1 ~ "Male", ase$sex == 2 ~ "Female"))

ase <- ase %>% mutate(sector = case_when(ase$sector == 0 ~ "Rural", ase$sector == 1 ~ "Urban", ase$sector == 2 ~ "Informal"))

ase <- ase %>% mutate(ethnicity = case_when(ase$ethnicity == 1 ~ "Fijian", ase$ethnicity == 2 ~ "Indian"))

ase <- ase %>% mutate(relationship = case_when(ase$relationship == 1 ~ "primary respondent", ase$relationship == 2 ~ "spouse",
                                          ase$relationship == 3 ~ "child", (ase$relationship != 1 || 2 || 3) ~ "other"))


ase <- ase %>% mutate(disability3 = case_when(ase$disability3 == 0 ~ "No", ase$disability3 == 1 ~ "Yes"))


to_change <- ase %>%
  tab_cells(age, sector, ethnicity, relationship, disability3) %>%
  tab_cols(total(), sex)


new_table <- ase %>%
  tab_cells(age, sector, ethnicity, relationship, disability3) %>%
  tab_cols(total(), sex) %>% 
  tab_stat_cpct () %>% 
  tab_pivot() %>% htmlTable(caption = "Demographics of study participants by sex, Fiji IDM study")

dash_table_switch$perc <- to_change
new_table


  
  
  }, sanitize.text.function = function(x) x)
  
})		

 observeEvent(input$cnt,{
 
   
 output$Ctable <- renderTable({
  
  
ase <- data[, c("sex","age","sector","ethnicity","relationship", "disability3")]

ase <- ase %>% mutate(age = case_when(ase$age >= 18 & ase$age <= 35 ~ "18-35", 
                                               ase$age >= 36 & ase$age <= 50 ~ "36-50", 
                                               ase$age >= 51 & ase$age <= 65 ~ "51-65",
                                               ase$age >= 66 ~ "66+"))

ase <- ase %>% mutate(sex = case_when(ase$sex == 1 ~ "Male", ase$sex == 2 ~ "Female"))

ase <- ase %>% mutate(sector = case_when(ase$sector == 0 ~ "Rural", ase$sector == 1 ~ "Urban", ase$sector == 2 ~ "Informal"))

ase <- ase %>% mutate(ethnicity = case_when(ase$ethnicity == 1 ~ "Fijian", ase$ethnicity == 2 ~ "Indian"))

ase <- ase %>% mutate(relationship = case_when(ase$relationship == 1 ~ "primary respondent", ase$relationship == 2 ~ "spouse",
                                          ase$relationship == 3 ~ "child", (ase$relationship != 1 || 2 || 3) ~ "other"))


ase <- ase %>% mutate(disability3 = case_when(ase$disability3 == 0 ~ "No", ase$disability3 == 1 ~ "Yes"))


to_change <- ase %>%
  tab_cells(age, sector, ethnicity, relationship, disability3) %>%
  tab_cols(total(), sex)


new_table <- ase %>%
  tab_cells(age, sector, ethnicity, relationship, disability3) %>%
  tab_cols(total(), sex) %>% 
  tab_stat_cases () %>% 
  tab_pivot() %>% htmlTable(caption = "Demographics of study participants by sex, Fiji IDM study")

dash_table_switch$perc <- to_change
new_table


  
  
  }, sanitize.text.function = function(x) x)
 
 
 })
 
 output$Ctable <- renderTable({
  
  
ase <- data[, c("sex","age","sector","ethnicity","relationship", "disability3")]

ase <- ase %>% mutate(age = case_when(ase$age >= 18 & ase$age <= 35 ~ "18-35", 
                                               ase$age >= 36 & ase$age <= 50 ~ "36-50", 
                                               ase$age >= 51 & ase$age <= 65 ~ "51-65",
                                               ase$age >= 66 ~ "66+"))

ase <- ase %>% mutate(sex = case_when(ase$sex == 1 ~ "Male", ase$sex == 2 ~ "Female"))

ase <- ase %>% mutate(sector = case_when(ase$sector == 0 ~ "Rural", ase$sector == 1 ~ "Urban", ase$sector == 2 ~ "Informal"))

ase <- ase %>% mutate(ethnicity = case_when(ase$ethnicity == 1 ~ "Fijian", ase$ethnicity == 2 ~ "Indian"))

ase <- ase %>% mutate(relationship = case_when(ase$relationship == 1 ~ "primary respondent", ase$relationship == 2 ~ "spouse",
                                          ase$relationship == 3 ~ "child", (ase$relationship != 1 || 2 || 3) ~ "other"))


ase <- ase %>% mutate(disability3 = case_when(ase$disability3 == 0 ~ "No", ase$disability3 == 1 ~ "Yes"))


to_change <- ase %>%
  tab_cells(age, sector, ethnicity, relationship, disability3) %>%
  tab_cols(total(), sex)


new_table <- ase %>%
  tab_cells(age, sector, ethnicity, relationship, disability3) %>%
  tab_cols(total(), sex) %>% 
  tab_stat_cpct () %>% 
  tab_pivot() %>% htmlTable(caption = "Demographics of study participants by sex, Fiji IDM study")

dash_table_switch$perc <- to_change
new_table


  
  
  }, sanitize.text.function = function(x) x)
  
  
  ####################################################################### Dashboard end ######



 ########################################## Food start ############################################
  output$SelectedPlot_food <- renderPlotly({

	food_wise <- data[, "score1"]
	names(food_wise)[names(food_wise) == "score1"] <- "dimen"
  
	if(input$Dimensiontype == "default")
	{
	
	returned <- bar_default_plotter(food_wise, "food")
	plott <- returned[[1]]
	textt <- returned[[2]]
	to_download$foo_plt <-  returned[[1]]

	
	
		output$food_table_plot <- renderTable({
			 		 table_returned <- default_table_maker(food_wise)
					 table_returned
			 })
	}
	
    if(input$Dimensiontype == "sex")
	{
	returned <- bar_gender_plotter(food_wise, "food")
	plott <- returned[[1]]
	textt <- returned[[2]]
	to_download$foo_plt <- returned[[1]]

	
		output$food_table_plot <- renderTable({
			 		 table_returned <- default_sex_table_maker(food_wise)
					 table_returned
			 },sanitize.text.function = function(x) x)
	}

  
	if(input$Dimensiontype == "age")
	{
	returned <- bar_age_plotter(food_wise, "food")
	plott <- returned[[1]]
	textt <- returned[[2]]
	to_download$foo_plt <- returned[[1]]

	
			output$food_table_plot <- renderTable({
			 		 table_returned <- default_age_table_maker(food_wise)
					 table_returned
			 },sanitize.text.function = function(x) x)
	
}

      output$htmltxt <- renderUI({
        textt    
    
  })
  
 ### sunburst 
 
output$sunburst <- renderPlotly({

 	returned <- sunburst_plotter(food_wise, "food")


output$caption <- renderUI({
        
    
	cap <- HTML( "<b>Number of people deprived categorized by gender</b>")
	cap
    
  })

output$htmltxt1 <- renderUI({
        
 textt <- sunburst_text_stat(food_wise, "food")
 textt
    
  })

returned
})
  ## food plot
  
  ggplotly(plott) %>% config(displayModeBar = F)
  ## food plot
  
  })
  
  output$down_food <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$foo_plt)
 dev.off()
 }) 
  
 output$SelectedPlot_theme <- renderPlotly({
 
food_wise <- data[, c( "score1.1")]

names(food_wise)[names(food_wise) == c("score1.1")] <- c("dimen")



food_dim <- food_wise %>% mutate(category = case_when(food_wise$dimen == 0 ~ "Most deprived", 
                                                      food_wise$dimen ==  1 ~ "Deprived", 
                                                      food_wise$dimen == 2 ~ "Somewhat deprived", 
                                                      food_wise$dimen == 3 ~ "Least deprived"))



if(input$themetype == "sex")
{
food_dim <- food_dim %>% mutate(newcat = case_when(data$sex == 1 ~ "Male", data$sex == 2 ~ "Female"))
fill_name <- "sex"
	
subs <- subset(food_dim, food_dim$newcat == "Male")
subsF <- subset(food_dim, food_dim$newcat == "Female")

plott <- ggplot(food_dim, aes(x=food_dim$category,  fill=factor(food_dim$newcat))) +
  geom_bar(position="dodge") + scale_fill_manual(values = c("#b8c3e3", "#6282c1"))+ labs(fill="Sex")+
  xlab("Number of people deprived in each category (security)")+ylab("Count") + 	theme(axis.text.x = element_text(size = 12, angle = 25))


texttheme <- HTML("<b>Food - Security</b></br>The mean score for food is <b>",round(mean(food_dim$dimen), 2),"</b>. For men: <b>",round(mean(subs$dimen),2),"</b> and for women: <b>", round(mean(subsF$dimen),2),"</b>." )

		output$theme_food_table_plot <- renderTable({
			 		 table_returned <- default_sex_table_maker(food_dim)
					 table_returned
			 },sanitize.text.function = function(x) x)


}

if(input$themetype == "age")
{
  
food_dim <- food_dim %>%  mutate(newcat = case_when(data$age >= 18  & data$age <= 24 ~ 1, data$age >= 25  & data$age <= 59 ~ 2, data$age > 59  ~ 3))
to_table_df <- food_dim
food_dim <-  na.omit(food_dim)
food_dim<- food_dim %>% mutate(newcat = case_when(food_dim$newcat == 1 ~ "18-24", food_dim$newcat == 2 ~ "25-59", food_dim$newcat == 3 ~ "60+"))


subset1 <- subset(food_dim, food_dim$newcat == "18-24")	
subset2 <- subset(food_dim, food_dim$newcat == "25-59")
subset3 <- subset(food_dim, food_dim$newcat ==  "60+")

fill_name <- "age"

plott <- ggplot(food_dim, aes(x=food_dim$category,  fill=factor(food_dim$newcat))) +
  geom_bar(position="dodge") + scale_fill_manual(values = c("#b8c3e3", "#6282c1", "#2c325d"))+ 
				labs(fill="Age group")+
  xlab("Number of people deprived in each category (security)")+ylab("Count") + 	theme(axis.text.x = element_text(size = 12, angle = 25))


texttheme <- HTML("<b>Food - Security</b></br>The mean score for food is: <b>",round(mean(food_dim$dimen), 2),"</b>. For age 18 - 24 is: <b>",round(mean(subset1$dimen), 2),"</b>. For age 25 - 59 is: <b>", round(mean(subset2$dimen), 2),"</b>. For age over 60 is: <b>", round(mean(subset3$dimen), 2), "</b>.")

			output$theme_food_table_plot <- renderTable({
			 		 table_returned <- default_age_table_maker(to_table_df)
					 table_returned
			 },sanitize.text.function = function(x) x)


}


food_dim <- within(food_dim, category <- factor(category, 
                                                levels=names(sort(table(category), 
                                                                  decreasing=FALSE))))

# plott <- ggplot(food_dim, aes(x=food_dim$catogery,  fill=factor(food_dim$newcat))) +
  # geom_bar(position="dodge") + scale_fill_brewer(name = fill_name)+
  # xlab("IDM scores for food deprivation (security)")+ylab("Count") + 	theme(axis.text.x = element_text(size = 12, angle = 25))

to_download$theme_down_food <- plott

      output$themetext <- renderUI({
         texttheme   
    
  })
  
  
ggplotly(plott) %>% config(displayModeBar = F)  
  
  })

  output$theme_down_food <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$theme_down_food)
 dev.off()
 }) 


output$SelectedPlot_question <- renderPlotly({


que_code <- data[,c("food.resources", "food.sleep", "food.wholeday", "food.year")]


if (input$food_questions == "In the past 4 weeks, was there ever no food for you to eat because of a lack of resources to get food")
{

que_code <- que_code %>% mutate(newcat = case_when(que_code$food.resources == 1 ~ "Yes", que_code$food.resources == 2 ~ "No"))


}

if (input$food_questions == "In the past 4 weeks, did you go to sleep at night hungry because there was not enough food")
{

que_code <- que_code %>% mutate(newcat = case_when(que_code$food.sleep == 1 ~ "Yes", que_code$food.sleep == 2 ~ "No"))


}
if (input$food_questions == "In the past 4 weeks, did you go a whole day and night without eating because there was not enough food")
{

que_code <- que_code %>% mutate(newcat = case_when(que_code$food.wholeday == 1 ~ "Yes", que_code$food.wholeday == 2 ~ "No"))

}

if (input$food_questions == "In the past 12 months, was there ever no food for you to eat because of a lack of resources to get food")
{

que_code <- que_code %>% mutate(newcat = case_when(que_code$food.year == 1 ~ "Yes", que_code$food.year == 2 ~ "No"))


}

	

plott <- ggplot(que_code, aes(x=que_code$newcat,  fill=factor(newcat))) +
  geom_bar(position="dodge", width = 0.5)+ scale_fill_manual(values = c("#b8c3e3", "#6282c1")) + labs(fill= "Response") + xlab("Response to survey questions")+ scale_fill_brewer(name = "Response")

 to_download$ques_down_food <- plott



  
   output$foodTreePlot <-  renderCollapsibleTree({
  
dimension_data <- read.csv("dashboard.csv")
water_dim <- subset(dimension_data, Dimension == "Food")

tree_graph <- water_dim %>%
  group_by(Dimension,Theme, Indicator) %>%
  summarize(`Number of Indicators` = n()) %>%
  collapsibleTreeSummary(
    hierarchy = c("Theme", "Indicator"),
    root = "Food",
    width = 600,
    height = 700,
    attribute = "Number of Indicators",
    zoomable = FALSE
  )
tree_graph
  
  })
  
ggplotly(plott) %>% config(displayModeBar = F)  

})

  output$ques_down_food <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$ques_down_food)
 dev.off()
 }) 
  
 ################################### Food end ##########################################
 
 ################################### Water start #######################################
   output$SelectedPlot_water <- renderPlotly({
   
   
   	water_wise <- data[, "score2"]
	names(water_wise)[names(water_wise) == "score2"] <- "dimen"
		
  
   if(input$waterDimensiontype == "default") {
  
		returned <- bar_default_plotter(water_wise, "Water")
		plott <- returned[[1]]
		textt <- returned[[2]]
		
		to_download$down_water <- plott
		output$water_table_plot <- renderTable({
			 		 table_returned <- default_table_maker(water_wise)
					 table_returned
			 })

  
 }
 

 
 if(input$waterDimensiontype == "sex")
 {
 
 
  	returned <- bar_gender_plotter(water_wise, "Water")
	plott <- returned[[1]]
	textt <- returned[[2]]
	
	to_download$down_water <- plott
	
output$water_table_plot <- renderTable({
			 		 table_returned <- default_sex_table_maker(water_wise)
					 table_returned
			 },sanitize.text.function = function(x) x)

 
 }

 
if (input$waterDimensiontype == "age")
{

 returned <- bar_age_plotter(water_wise, "Water")
 plott <- returned[[1]]
 textt <- returned[[2]]
 
 to_download$down_water <- plott
 
 output$water_table_plot <- renderTable({
			 		 table_returned <- default_age_table_maker(water_wise)
					 table_returned
			 },sanitize.text.function = function(x) x)

 }

output$whtmltxt <- renderUI({
        textt    
    
  })


    ggplotly(plott) %>% config(displayModeBar = F)

 

 
 })
 
 
   output$down_water <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$down_water)
 dev.off()
 }) 
 
 
 output$wsunburst <- renderPlotly({
	water_wise <- data[, "score2"]
	names(water_wise)[names(water_wise) == "score2"] <- "dimen"
		
	returned <- sunburst_plotter(water_wise, "Water")
	
	output$wcaption <- renderUI({
        
    
	cap <- HTML( "<b>Number of people deprived categorized by gender</b>")
	cap
    
  })
  
  output$whtmltxt1 <- renderUI({
        
 textt <- sunburst_text_stat(water_wise, "Water")
 textt
    
  })
	returned
 
 
 })
 
  output$w_SelectedPlot_theme <- renderPlotly({
 

  all_dim <- data[, grep("^score\\d+(\\.\\d{1,1})$",colnames(data),perl=TRUE,value = TRUE)]
  
  
  water_theme <- all_dim[, grep("score2", colnames(all_dim))]
  
  colnames(water_theme) <- var_label(water_theme) 
  theme <- input$inputthemetype
	
	to_plot_theme <- paste("Water -", paste(theme))

  water_dim_th <- water_theme %>% mutate(category = case_when(round(water_theme[to_plot_theme],0) == 0 ~ "Most deprived", 
                                                              round(water_theme[to_plot_theme],0) == 1 ~ "Deprived", 
                                                              round(water_theme[to_plot_theme],0) == 2 ~ "Somewhat deprived", 
                                                              round(water_theme[to_plot_theme],0) == 3 ~ "Least deprived"))



if(input$water_th_type == "sex")
{

	returned <- theme_sex_plotter(water_dim_th, "Water", theme, to_plot_theme)
	plott <- returned[[1]]
	texttheme <- returned[[2]]
	
 to_download$theme_down_water <- plott

			output$theme_water_table_plot <- renderTable({
			 		 table_returned <- theme_sex_table_maker(water_dim_th)
					 table_returned
			 },sanitize.text.function = function(x) x)

}

if(input$water_th_type == "age")
{
	returned <- theme_age_plotter(water_dim_th, "Water", theme, to_plot_theme)
	plott <- returned[[1]]
	texttheme <- returned[[2]]
  to_download$theme_down_water <- plott

	
				output$theme_water_table_plot <- renderTable({
			 		 table_returned <- theme_age_table_maker(water_dim_th)
					 table_returned		
			 },sanitize.text.function = function(x) x)
			 
}

      output$wthemetype <- renderUI({
         texttheme   
    
  })
  
ggplotly(plott) %>% config(displayModeBar = F)  
  
  })
  
 
  
  output$theme_down_water <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$theme_down_water)
 dev.off()
 }) 
 
  
  
   output$WaterTreePlot <-  renderCollapsibleTree({
  
dimension_data <- read.csv("dashboard.csv")
water_dim <- subset(dimension_data, Dimension == "Water")

tree_graph <- water_dim %>%
  group_by(Dimension,Theme, Indicator) %>%
  summarize(`Number of Indicators` = n()) %>%
  collapsibleTreeSummary(
    hierarchy = c("Theme", "Indicator"),
    root = "Water",
    width = 600,
    height = 700,
    attribute = "Number of Indicators",
    zoomable = FALSE
  )
tree_graph
  
  })
  
 
 
 
 #########################
 
 output$W_SelectedPlot_question <- renderPlotly({


que_code <- data[,c("water.quantity", "water", "treat.water", "how.treat")]


if (input$water_questions == "How often do you have enough water to meet all your personal needs")
{

que_code$newcat <- que_code$water.quantity
que_code <- que_code[order(que_code$newcat), ]


que_code <- que_code %>% mutate(newcat = case_when(que_code$water.quantity == 1 ~ "Never", 
												   que_code$water.quantity == 2 ~ "Rarely",
												   que_code$water.quantity == 3 ~ "sometimes",
												   que_code$water.quantity == 4 ~ "often",
												   que_code$water.quantity == 5 ~ "always"))

que_code$newcat <- as.character(que_code$newcat)
que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat))


}

if (input$water_questions == "What is the main source of drinking water for members of your household")
{

que_code$newcat <- que_code$water
que_code <- que_code[order(que_code$newcat), ]

que_code <- que_code %>% mutate(newcat = case_when(que_code$water == 1 ~ "unprotected surface",
												   que_code$water == 2 ~ "unprotected well", 
												   que_code$water == 3 ~ "private vendor",
												   que_code$water == 4 ~ "protected spring/well",
												   que_code$water == 5 ~ "public tap",
												   que_code$water == 6 ~ "piped outside dwelling",
												   que_code$water == 7 ~ "piped into dwelling",
												   que_code$water == 8 ~ "other"))



que_code$newcat <- as.character(que_code$newcat)
que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat))

}
if (input$water_questions == "Do you treat your water in any way to make it safer to drink")
{

que_code$newcat <- que_code$treat.water
que_code <- que_code[order(que_code$newcat), ]

que_code <- que_code %>% mutate(newcat = case_when(que_code$treat.water == 1 ~ "Yes", que_code$treat.water == 2 ~ "No"))

que_code$newcat <- as.character(que_code$newcat)
que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat))
}

if (input$water_questions == "What do you usually do to the water to make it safer")
{

que_code$newcat <- que_code$how.treat
que_code <- que_code[order(que_code$newcat), ]

que_code <- que_code %>% mutate(newcat = case_when(que_code$how.treat == 1 ~ "filter", 
                                                   que_code$how.treat == 2 ~ "iodine/mineral",
                                                   que_code$how.treat == 4 ~ "boil"))


que_code$newcat[is.na(que_code$newcat)] <- "Unknown"

que_code$newcat <- as.character(que_code$newcat)
que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat))


}

# que_code <- within(que_code, newcat <- factor(newcat, 
                                          # levels=names(sort(table(newcat), 
                                                            # decreasing=TRUE))))


plott <- ggplot(que_code, aes(x=que_code$newcat,  fill=factor(newcat))) +
  geom_bar(position="dodge") + labs(fill= "Response") + xlab("Response to survey questions")+ scale_fill_brewer(name = "Response")+ 	theme(axis.text.x = element_text(size = 12, angle = 25))
  
 to_download$ques_down_water <- plott 
ggplotly(plott) %>% config(displayModeBar = F)  


})
 
  output$ques_down_water <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$ques_down_water)
 dev.off()
 }) 
 ######################## water end ##################################################
 
 
 
 
 #################################################################################################
 
 
  ################################### Work start #######################################
   output$SelectedPlot_work <- renderPlotly({
   
   
  work_wise <- data[, "score15"]
	names(work_wise)[names(work_wise) == "score15"] <- "dimen"
		
  
   if(input$workDimensiontype == "default") {
  
		returned <- bar_default_plotter(work_wise, "Work")
		plott <- returned[[1]]
		textt <- returned[[2]]
		
		to_download$down_work <- plott

				output$work_table_plot <- renderTable({
			 		 table_returned <- default_table_maker(work_wise)
					 table_returned
			 })
		

  
 }
 

 
 if(input$workDimensiontype == "sex")
 {
 
 
  	returned <- bar_gender_plotter(work_wise, "Work")
	plott <- returned[[1]]
	textt <- returned[[2]]
	
	to_download$down_work <- plott

	output$work_table_plot <- renderTable({
			 		 table_returned <- default_sex_table_maker(work_wise)
					 table_returned
			 },sanitize.text.function = function(x) x)
	


 
 }


if (input$workDimensiontype == "age")
{

 returned <- bar_age_plotter(work_wise, "Work")
 plott <- returned[[1]]
 textt <- returned[[2]]
 
 to_download$down_work <- plott

 
  output$work_table_plot <- renderTable({
			 		 table_returned <- default_age_table_maker(work_wise)
					 table_returned
			 },sanitize.text.function = function(x) x)

 }

output$work_htmltxt <- renderUI({
        textt    
    
  })


    ggplotly(plott) %>% config(displayModeBar = F)

 

 
 })
 
 
   output$down_work <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$down_work)
 dev.off()
 }) 
 
 
 output$work_sunburst <- renderPlotly({
	work_wise <- data[, "score15"]
	names(work_wise)[names(work_wise) == "score15"] <- "dimen"
		
	returned <- sunburst_plotter(work_wise, "Work")
	
	output$work_caption <- renderUI({
        
    
	cap <- HTML( "<b>Number of people deprived categorized by gender</b>")
	cap
    
  })
  
  output$work_htmltxt1 <- renderUI({
        
 textt <- sunburst_text_stat(work_wise, "Work")
 textt
    
  })
	returned
 
 
 })
 
  output$work_SelectedPlot_theme <- renderPlotly({
 

  all_dim <- data[, grep("^score\\d+(\\.\\d{1,1})$",colnames(data),perl=TRUE,value = TRUE)]
  
  
  work_theme <- all_dim[, grep("score15", colnames(all_dim))]
  
  colnames(work_theme) <- var_label(work_theme) 
  theme <- input$work_inputthemetype
	
	to_plot_theme <- paste("Work -", paste(theme))

  work_dim_th <- work_theme %>% mutate(category = case_when(round(work_theme[to_plot_theme],0) == 0 ~ "Most deprived", 
                                                              round(work_theme[to_plot_theme],0) == 1 ~ "Deprived", 
                                                              round(work_theme[to_plot_theme],0) == 2 ~ "Somewhat deprived", 
                                                              round(work_theme[to_plot_theme],0) == 3 ~ "Least deprived"))



if(input$work_th_type == "sex")
{

	returned <- theme_sex_plotter(work_dim_th, "Work", theme, to_plot_theme)
	plott <- returned[[1]]
	texttheme <- returned[[2]]
	
	    to_download$theme_down_work <- plott

				output$theme_work_table_plot <- renderTable({
			 		 table_returned <- theme_sex_table_maker(work_dim_th)
					 table_returned
			 },sanitize.text.function = function(x) x)

}

if(input$work_th_type == "age")
{
	returned <- theme_age_plotter(work_dim_th, "Work", theme, to_plot_theme)
	plott <- returned[[1]]
	texttheme <- returned[[2]]
	
	    to_download$theme_down_work <- plott

	
		
				output$theme_work_table_plot <- renderTable({
			 		 table_returned <- theme_age_table_maker(work_dim_th)
					 table_returned
			 },sanitize.text.function = function(x) x)
}

      output$work_themetype <- renderUI({
         texttheme   
    
  })
  

ggplotly(plott) %>% config(displayModeBar = F)  
  
  })
  
    output$theme_down_work <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$theme_down_work)
 dev.off()
 }) 
  
   output$WorkTreePlot <-  renderCollapsibleTree({
  
dimension_data <- read.csv("dashboard.csv")
work_dim <- subset(dimension_data, Dimension == "Work")

tree_graph <- work_dim %>%
  group_by(Dimension,Theme, Indicator) %>%
  summarize(`Number of Indicators` = n()) %>%
  collapsibleTreeSummary(
    hierarchy = c("Theme", "Indicator"),
    root = "Work",
    width = 600,
    height = 700,
    attribute = "Number of Indicators",
    zoomable = FALSE
  )
tree_graph
  
  })
  
 
 
 
 #########################
 
 output$Work_SelectedPlot_question <- renderPlotly({


que_code <- data[,c("work.pay", "kind.work", "injury.work", "effect.injury", 
                    "respect.paid", "treated.respect", "unpaid.work", "injury.unpaid", "effect.injury.unpaid")]


if (input$work_questions == "Did you do any work for money during the last 6 months")
{
que_code$newcat <- que_code$work.pay
que_code <- que_code[order(que_code$newcat), ]

que_code <- que_code %>% mutate(newcat = case_when(que_code$work.pay == 1 ~ "Yes", 
												   que_code$work.pay == 2 ~ "No"))

que_code$newcat[is.na(que_code$newcat)] <- "Unknown"


que_code$newcat <- as.character(que_code$newcat)
que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat))

}

if (input$work_questions == "What is the main kind of work that you regularly do")
{

que_code$newcat <- que_code$kind.work
que_code <- que_code[order(que_code$newcat), ]

que_code <- que_code %>% mutate(newcat = case_when(que_code$kind.work == 1 ~ "farm",
												   que_code$kind.work == 2 ~ "construction", 
												   que_code$kind.work == 3 ~ "transport",
												   que_code$kind.work == 4 ~ "domestic",
												   que_code$kind.work == 5 ~ "selling",
												   que_code$kind.work == 6 ~ "professional",
												   que_code$kind.work == 7 ~ "skilled",
												   que_code$kind.work == 8 ~ "office",
												   que_code$kind.work == 9 ~ "security",
												   que_code$kind.work == 10 ~ "factory",
												   que_code$kind.work == 11 ~ "scavenging",
												   que_code$kind.work == 12 ~ "begging",
												   que_code$kind.work == 13 ~ "seasonal",
												   que_code$kind.work == c(0, 14, NA) ~ "others"))
												   

que_code$newcat <- as.character(que_code$newcat)
que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat))											


}
if (input$work_questions == "Have you suffered any injury/illness from your paid work in the last 12 months")
{

que_code$newcat <- que_code$injury.work
que_code <- que_code[order(que_code$newcat), ]

que_code <- que_code %>% mutate(newcat = case_when(que_code$injury.work == 1 ~ "Yes", que_code$injury.work == 2 ~ "No"))
que_code$newcat[is.na(que_code$newcat)] <- "Unknown"

que_code$newcat <- as.character(que_code$newcat)
que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat))

}

if (input$work_questions == "What effect did this injury/illness or other harm have on you")
{

que_code$newcat <- que_code$effect.injury
que_code <- que_code[order(que_code$newcat), ]

que_code <- que_code %>% mutate(newcat = case_when(que_code$effect.injury == 1 ~ "long term no work at all", 
                                                   que_code$effect.injury == 2 ~ "long term-not same work",
                                                   que_code$effect.injury == 3 ~ "long term- same work before",
                                                   que_code$effect.injury == 4 ~ "not long term"))


que_code$newcat[is.na(que_code$newcat)] <- "Unknown"



que_code$newcat <- as.character(que_code$newcat)
que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat))

}

if (input$work_questions == "Members of my community respect the paid work that I do")
{
 

que_code$newcat <- que_code$respect.paid
que_code <- que_code[order(que_code$newcat), ]

  que_code <- que_code %>% mutate(newcat = case_when(que_code$respect.paid == 1 ~ "strongly disagree", 
                                                     que_code$respect.paid == 2 ~ "disagree",
                                                     que_code$respect.paid == 3 ~ "agree"))
  
  
  que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
  
 
 
que_code$newcat <- as.character(que_code$newcat)
que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat))
}



if (input$work_questions == "I am treated with respect when I do paid work")
{

que_code$newcat <- que_code$treated.respect
que_code <- que_code[order(que_code$newcat), ]
  
  que_code <- que_code %>% mutate(newcat = case_when(que_code$treated.respect == 1 ~ "strongly disagree", 
                                                     que_code$treated.respect == 2 ~ "disagree",
                                                     que_code$treated.respect == 3 ~ "agree"))
  
  
  que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
 
 que_code$newcat <- as.character(que_code$newcat)
que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat))
  
}




if (input$work_questions == "Do you regularly do unpaid work")
{
 
 que_code$newcat <- que_code$unpaid.work
que_code <- que_code[order(que_code$newcat), ]
 
  que_code <- que_code %>% mutate(newcat = case_when(que_code$unpaid.work == 1 ~ "yes", 
                                                     que_code$unpaid.work == 2 ~ "no"))
  
  
  que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
 
  que_code$newcat <- as.character(que_code$newcat)
que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat))
  
}




if (input$work_questions == "Have you suffered any injury/illness from your unpaid work in the last 12 months")
{
 
  que_code$newcat <- que_code$injury.unpaid
que_code <- que_code[order(que_code$newcat), ]

  que_code <- que_code %>% mutate(newcat = case_when(que_code$injury.unpaid == 1 ~ "yes", 
                                                     que_code$injury.unpaid == 2 ~ "no"))
  
  
  que_code$newcat[is.na(que_code$newcat)] <- "Unknown"

  que_code$newcat <- as.character(que_code$newcat)
que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat)) 
  
}



if (input$work_questions == "What effect did this injury/illness have on you")
{
 
   que_code$newcat <- que_code$effect.injury.unpaid
que_code <- que_code[order(que_code$newcat), ]

 
  que_code <- que_code %>% mutate(newcat = case_when(que_code$effect.injury.unpaid == 1 ~ "long term no work at all", 
                                                     que_code$effect.injury.unpaid == 2 ~ "long term not same work",
                                                     que_code$effect.injury.unpaid == 3 ~ "long term same work before",
                                                     que_code$effect.injury.unpaid == 4 ~ "not long term"))
  
  
  que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
  
 
   que_code$newcat <- as.character(que_code$newcat)
que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat)) 
}





plott <- ggplot(que_code, aes(x=que_code$newcat,  fill=factor(newcat))) +
  geom_bar(position="dodge") + labs(fill= "Response") + xlab("Response to survey questions")+ scale_fill_brewer(name = "Response")+ 	theme(axis.text.x = element_text(size = 12, angle = 25))
  
 to_download$ques_down_work <- plott
ggplotly(plott) %>% config(displayModeBar = F)  


})

    output$ques_down_work <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$ques_down_work)
 dev.off()
 }) 
 
 ######################## work end #########################
 
 
   ################################### Voice start #######################################
   output$SelectedPlot_voice <- renderPlotly({
   
   
  voice_wise <- data[, "score13"]
	names(voice_wise)[names(voice_wise) == "score13"] <- "dimen"
		
  
   if(input$voiceDimensiontype == "default") {
  
		returned <- bar_default_plotter(voice_wise, "Voice")
		plott <- returned[[1]]
		textt <- returned[[2]]
	
		to_download$down_voice <- plott
				output$voice_table_plot <- renderTable({
			 		 table_returned <- default_table_maker(voice_wise)
					 table_returned
			 })
		

  
 }
 

 
 if(input$voiceDimensiontype == "sex")
 {
 
 
  	returned <- bar_gender_plotter(voice_wise, "Voice")
	plott <- returned[[1]]
	textt <- returned[[2]]
	to_download$down_voice <- plott

	output$voice_table_plot <- renderTable({
			 		 table_returned <- default_sex_table_maker(voice_wise)
					 table_returned
			 },sanitize.text.function = function(x) x)
	


 
 }


if (input$voiceDimensiontype == "age")
{

 returned <- bar_age_plotter(voice_wise, "Voice")
 plott <- returned[[1]]
 textt <- returned[[2]]
 to_download$down_voice <- plott

  output$voice_table_plot <- renderTable({
			 		 table_returned <- default_age_table_maker(voice_wise)
					 table_returned
			 },sanitize.text.function = function(x) x)

 }

output$voice_htmltxt <- renderUI({
        textt    
    
  })


    ggplotly(plott) %>% config(displayModeBar = F)

 

 
 })
 
    output$down_voice <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$down_voice)
 dev.off()
 }) 
 
 
 output$voice_sunburst <- renderPlotly({
	voice_wise <- data[, "score13"]
	names(voice_wise)[names(voice_wise) == "score13"] <- "dimen"
		
	returned <- sunburst_plotter(voice_wise, "Voice")
	
	output$voice_caption <- renderUI({
        
    
	cap <- HTML( "<b>Number of people deprived categorized by gender</b>")
	cap
    
  })
  
  output$voice_htmltxt1 <- renderUI({
        
 textt <- sunburst_text_stat(voice_wise, "Voice")
 textt
    
  })
	returned
 
 
 })
 
  output$voice_SelectedPlot_theme <- renderPlotly({
 

  all_dim <- data[, grep("^score\\d+(\\.\\d{1,1})$",colnames(data),perl=TRUE,value = TRUE)]
  
  
  voice_theme <- all_dim[, grep("score13", colnames(all_dim))]
  
  colnames(voice_theme) <- var_label(voice_theme) 
  theme <- input$voice_inputthemetype
	
	to_plot_theme <- paste("Voice -", paste(theme))

  voice_dim_th <- voice_theme %>% mutate(category = case_when(round(voice_theme[to_plot_theme],0) == 0 ~ "Most deprived", 
                                                              round(voice_theme[to_plot_theme],0) == 1 ~ "Deprived", 
                                                              round(voice_theme[to_plot_theme],0) == 2 ~ "Somewhat deprived", 
                                                              round(voice_theme[to_plot_theme],0) == 3 ~ "Least deprived"))



if(input$voice_th_type == "sex")
{

	returned <- theme_sex_plotter(voice_dim_th, "Voice", theme, to_plot_theme)
	plott <- returned[[1]]
	texttheme <- returned[[2]]
	
	to_download$theme_down_voice <- plott
	
			output$theme_voice_table_plot <- renderTable({
			 		 table_returned <- theme_sex_table_maker(voice_dim_th)
					 table_returned
			 },sanitize.text.function = function(x) x)
}

if(input$voice_th_type == "age")
{
	returned <- theme_age_plotter(voice_dim_th, "Voice", theme, to_plot_theme)
	plott <- returned[[1]]
	texttheme <- returned[[2]]
	to_download$theme_down_voice <- plott

				output$theme_voice_table_plot <- renderTable({
			 		 table_returned <- theme_age_table_maker(voice_dim_th)
					 table_returned
			 },sanitize.text.function = function(x) x)
}

      output$voice_themetype <- renderUI({
         texttheme   
    
  })
  
  
ggplotly(plott) %>% config(displayModeBar = F)  
  
  })
 
    output$theme_down_voice <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$theme_down_voice)
 dev.off()
 }) 
  
   output$VoiceTreePlot <-  renderCollapsibleTree({
  
dimension_data <- read.csv("dashboard.csv")
voice_dim <- subset(dimension_data, Dimension == "Voice")

tree_graph <- voice_dim %>%
  group_by(Dimension,Theme, Indicator) %>%
  summarize(`Number of Indicators` = n()) %>%
  collapsibleTreeSummary(
    hierarchy = c("Theme", "Indicator"),
    root = "Voice",
    width = 600,
    height = 700,
    attribute = "Number of Indicators",
    zoomable = FALSE
  )
tree_graph
  
  })
  
 
 
 
 #########################
 
 output$Voice_SelectedPlot_question <- renderPlotly({


que_code <- data[,c("raise.issues", "change", "control.amount")]


if (input$voice_questions == "To what extent are you able to raise issues in your community that you feel strongly about")
{
   que_code$newcat <- que_code$raise.issues
que_code <- que_code[order(que_code$newcat), ]

que_code <- que_code %>% mutate(newcat = case_when(que_code$raise.issues == 1 ~ "not at all", 
												                           que_code$raise.issues == 2 ~ "great difficulty",
												                           que_code$raise.issues == 3 ~ "some difficulty",
												                           que_code$raise.issues == 4 ~ "fairly easily",
												                           que_code$raise.issues == 5 ~ "very easily"))

que_code$newcat[is.na(que_code$newcat)] <- "Unknown"


que_code$newcat <- as.character(que_code$newcat)
que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat)) 
}

if (input$voice_questions == "To what extent do you think people like you can change things in their community")
{
que_code$newcat <- que_code$change
que_code <- que_code[order(que_code$newcat), ]

que_code <- que_code %>% mutate(newcat = case_when(que_code$change == 1 ~ "not at all",
												   que_code$change == 2 ~ "great difficulty", 
												   que_code$change == 3 ~ "some difficulty",
												   que_code$change == 4 ~ "fairly easily",
												   que_code$change == 5 ~ "very easily"))
que_code$newcat[is.na(que_code$newcat)] <- "Unknown"


que_code$newcat <- as.character(que_code$newcat)
que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat)) 

}
if (input$voice_questions == "In general, how much control do you have over personal decisions that have a major impact on your life")
{

que_code$newcat <- que_code$control.amount
que_code <- que_code[order(que_code$newcat), ]

que_code <- que_code %>% mutate(newcat = case_when(que_code$control.amount == 1 ~ "none", 
                                                   que_code$control.amount == 2 ~ "very little",
                                                   que_code$control.amount == 3 ~ "some",
                                                   que_code$control.amount == 4 ~ "fair",
                                                   que_code$control.amount == 5 ~ "full"))
que_code$newcat[is.na(que_code$newcat)] <- "Unknown"


que_code$newcat <- as.character(que_code$newcat)
que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat)) 

}


plott <- ggplot(que_code, aes(x=que_code$newcat,  fill=factor(newcat))) +
  geom_bar(position="dodge") + labs(fill= "Response") + xlab("Response to survey questions")+ scale_fill_brewer(name = "Response")+ 	theme(axis.text.x = element_text(size = 12, angle = 25))
  
 to_download$ques_down_voice <- plott
ggplotly(plott) %>% config(displayModeBar = F)  


})

  output$ques_down_voice <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$ques_down_voice)
 dev.off()
 }) 
 
 ########################
 
   ################################### Environment start #######################################
   output$SelectedPlot_env <- renderPlotly({
   
   
  env_wise <- data[, "score12"]
	names(env_wise)[names(env_wise) == "score12"] <- "dimen"
		
  
   if(input$envDimensiontype == "default") {
  
		returned <- bar_default_plotter(env_wise, "Environment")
		plott <- returned[[1]]
		textt <- returned[[2]]
		
		to_download$down_env <- plott
				output$env_table_plot <- renderTable({
			 		 table_returned <- default_table_maker(env_wise)
					 table_returned
			 })
		

  
 }
 

 
 if(input$envDimensiontype == "sex")
 {
 
 
  	returned <- bar_gender_plotter(env_wise, "Environment")
	plott <- returned[[1]]
	textt <- returned[[2]]
	to_download$down_env <- plott
	output$env_table_plot <- renderTable({
			 		 table_returned <- default_sex_table_maker(env_wise)
					 table_returned
			 },sanitize.text.function = function(x) x)


 
 }


if (input$envDimensiontype == "age")
{

 returned <- bar_age_plotter(env_wise, "Environment")
 plott <- returned[[1]]
 textt <- returned[[2]]
 to_download$down_env <- plott
  output$env_table_plot <- renderTable({
			 		 table_returned <- default_age_table_maker(env_wise)
					 table_returned
			 },sanitize.text.function = function(x) x)

 }

output$env_htmltxt <- renderUI({
        textt    
    
  })


    ggplotly(plott) %>% config(displayModeBar = F)

 

 
 })
 
   output$down_env <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$down_env)
 dev.off()
 }) 
 
 
 output$env_sunburst <- renderPlotly({
	env_wise <- data[, "score12"]
	names(env_wise)[names(env_wise) == "score12"] <- "dimen"
		
	returned <- sunburst_plotter(env_wise, "Environment")
	
	output$env_caption <- renderUI({
        
    
	cap <- HTML( "<b>Number of people deprived categorized by gender</b>")
	cap
    
  })
  
  output$env_htmltxt1 <- renderUI({
        
 textt <- sunburst_text_stat(env_wise, "environment")
 textt
    
  })
	returned
 
 
 })
 
  output$env_SelectedPlot_theme <- renderPlotly({
 

  all_dim <- data[, grep("^score\\d+(\\.\\d{1,1})$",colnames(data),perl=TRUE,value = TRUE)]
  
  
  env_theme <- all_dim[, grep("score12", colnames(all_dim))]
  
  colnames(env_theme) <- var_label(env_theme) 
  theme <- input$env_inputthemetype
	
	to_plot_theme <- paste("Environment -", paste(theme))

  env_dim_th <- env_theme %>% mutate(category = case_when(round(env_theme[to_plot_theme],0) == 0 ~ "Most deprived", 
                                                              round(env_theme[to_plot_theme],0) == 1 ~ "Deprived", 
                                                              round(env_theme[to_plot_theme],0) == 2 ~ "Somewhat deprived", 
                                                              round(env_theme[to_plot_theme],0) == 3 ~ "Least deprived"))



if(input$env_th_type == "sex")
{

	returned <- theme_sex_plotter(env_dim_th, "Environment", theme, to_plot_theme)
	plott <- returned[[1]]
	texttheme <- returned[[2]]
	to_download$theme_down_env <- plott
	output$theme_env_table_plot <- renderTable({
			 		 table_returned <- theme_sex_table_maker(env_dim_th)
					 table_returned
			 },sanitize.text.function = function(x) x)

}

if(input$env_th_type == "age")
{
	returned <- theme_age_plotter(env_dim_th, "Environment", theme, to_plot_theme)
	plott <- returned[[1]]
	texttheme <- returned[[2]]
	to_download$theme_down_env <- plott
					output$theme_env_table_plot <- renderTable({
			 		 table_returned <- theme_age_table_maker(env_dim_th)
					 table_returned
			 },sanitize.text.function = function(x) x)
}

      output$env_themetype <- renderUI({
         texttheme   
    
  })
  
  
ggplotly(plott) %>% config(displayModeBar = F)  
  
  })
  
  
    output$theme_down_env <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$theme_down_env)
 dev.off()
 }) 
  
  
  
   output$envTreePlot <-  renderCollapsibleTree({
  
dimension_data <- read.csv("dashboard.csv")
env_dim <- subset(dimension_data, Dimension == "Environment")

tree_graph <- env_dim %>%
  group_by(Dimension,Theme, Indicator) %>%
  summarize(`Number of Indicators` = n()) %>%
  collapsibleTreeSummary(
    hierarchy = c("Theme", "Indicator"),
    root = "Environment",
    width = 600,
    height = 700,
    attribute = "Number of Indicators",
    zoomable = FALSE
  )
tree_graph
  
  })
  
 
 
 
 #########################
 
 output$env_SelectedPlot_question <- renderPlotly({


que_code <- data[,c("rubbish", "sewage", "pollution", "stagnant", 
                    "waste", "traffic", "noise", "other")]


if (input$env_questions == "Large amounts of rubbish")
{

que_code <- que_code %>% mutate(newcat = case_when(que_code$rubbish == 1 ~ "Yes", 
												                           que_code$rubbish == 2 ~ "No"))

que_code$newcat[is.na(que_code$newcat)] <- "Unknown"


}

if (input$env_questions == "Open sewage")
{

que_code <- que_code %>% mutate(newcat = case_when(que_code$sewage == 1 ~ "Yes",
												   que_code$sewage == 2 ~ "No"))


}
if (input$env_questions == "Air pollution")
{

que_code <- que_code %>% mutate(newcat = case_when(que_code$pollution == 1 ~ "Yes", que_code$pollution == 2 ~ "No"))
que_code$newcat[is.na(que_code$newcat)] <- "Unknown"


}


if (input$env_questions == "Pools of water")
{

que_code <- que_code %>% mutate(newcat = case_when(que_code$stagnant == 1 ~ "Nes", 
                                                   que_code$stagnant == 2 ~ "No"))


que_code$newcat[is.na(que_code$newcat)] <- "Unknown"


}

if (input$env_questions == "Stores of unsecured waste")
{
  
  que_code <- que_code %>% mutate(newcat = case_when(que_code$waste == 1 ~ "Yes", 
                                                     que_code$waste == 2 ~ "No"))
  
  
  que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
  
  
}



if (input$env_questions == "Heavy vehicle traffic")
{
  
  que_code <- que_code %>% mutate(newcat = case_when(que_code$traffic == 1 ~ "Yes", 
                                                     que_code$traffic == 2 ~ "No"))
  
  
  que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
  
  
}




if (input$env_questions == "High levels of noise")
{
  
  que_code <- que_code %>% mutate(newcat = case_when(que_code$noise == 1 ~ "yes", 
                                                     que_code$noise == 2 ~ "no"))
  
  
  que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
  
  
}




if (input$env_questions == "Any other significant hazard")
{
  
  que_code <- que_code %>% mutate(newcat = case_when(que_code$other == 1 ~ "yes", 
                                                     que_code$other == 2 ~ "no"))
  
  
  que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
  
  
}




plott <- ggplot(que_code, aes(x=que_code$newcat,  fill=factor(newcat))) +
  geom_bar(position="dodge") + labs(fill= "Response") + xlab("Response to survey questions")+ scale_fill_brewer(name = "Response")+ 	theme(axis.text.x = element_text(size = 12, angle = 25))
  
  to_download$ques_down_env <- plott
ggplotly(plott) %>% config(displayModeBar = F)  


})

    output$ques_down_env <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$ques_down_env)
 dev.off()
 }) 
 
 ######################## Environment end #########################
 
 

   
   ################################### Shelter start #######################################
   output$SelectedPlot_shelter <- renderPlotly({
     
     
     shelter_wise <- data[, "score3"]
     names(shelter_wise)[names(shelter_wise) == "score3"] <- "dimen"
     
     
     if(input$shelterDimensiontype == "default") {
       
       returned <- bar_default_plotter(shelter_wise, "Shelter")
       plott <- returned[[1]]
       textt <- returned[[2]]
	   to_download$down_shelter <- plott
	   		output$shelter_table_plot <- renderTable({
			 		 table_returned <- default_table_maker(shelter_wise)
					 table_returned
			 })
       
       
       
     }
     
     
     
     if(input$shelterDimensiontype == "sex")
     {
       
       
       returned <- bar_gender_plotter(shelter_wise, "Shelter")
       plott <- returned[[1]]
       textt <- returned[[2]]
	   to_download$down_shelter <- plott
       output$shelter_table_plot <- renderTable({
			 		 table_returned <- default_sex_table_maker(shelter_wise)
					 table_returned
			 },sanitize.text.function = function(x) x)
       
       
       
     }
     
     
     if (input$shelterDimensiontype == "age")
     {
       
       returned <- bar_age_plotter(shelter_wise, "Shelter")
       plott <- returned[[1]]
       textt <- returned[[2]]
	   to_download$down_shelter <- plott
	    output$shelter_table_plot <- renderTable({
			 		 table_returned <- default_age_table_maker(shelter_wise)
					 table_returned
			 },sanitize.text.function = function(x) x)
       
     }
     
     output$shelter_htmltxt <- renderUI({
       textt    
       
     })
     
     
     ggplotly(plott) %>% config(displayModeBar = F)
     
     
   })
   
     output$down_shelter <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$down_shelter)
 dev.off()
 }) 
 
   
   
   output$shelter_sunburst <- renderPlotly({
     shelter_wise <- data[, "score3"]
     names(shelter_wise)[names(shelter_wise) == "score3"] <- "dimen"
     
     returned <- sunburst_plotter(shelter_wise, "Shelter")
     
     output$shelter_caption <- renderUI({
       
       
       cap <- HTML( "<b>Number of people deprived categorized by gender</b>")
       cap
       
     })
     
     output$shelter_htmltxt1 <- renderUI({
       
       textt <- sunburst_text_stat(shelter_wise, "Shelter")
       textt
       
     })
     returned
     
     
   })
   
   output$shelter_SelectedPlot_theme <- renderPlotly({
     
     
     all_dim <- data[, grep("^score\\d+(\\.\\d{1,1})$",colnames(data),perl=TRUE,value = TRUE)]
     
     
     shelter_theme <- all_dim[, grep("score3", colnames(all_dim))]
     
     colnames(shelter_theme) <- var_label(shelter_theme) 
     theme <- input$shelter_inputthemetype
     
     to_plot_theme <- paste("Shelter -", paste(theme))
     
     shelter_dim_th <- shelter_theme %>% mutate(category = case_when(round(shelter_theme[to_plot_theme],0) == 0 ~ "Most deprived", 
                                                                 round(shelter_theme[to_plot_theme],0) == 1 ~ "Deprived", 
                                                                 round(shelter_theme[to_plot_theme],0) == 2 ~ "Somewhat deprived", 
                                                                 round(shelter_theme[to_plot_theme],0) == 3 ~ "Least deprived"))
     
     
     
     if(input$shelter_th_type == "sex")
     {
       
       returned <- theme_sex_plotter(shelter_dim_th, "Shelter", theme, to_plot_theme)
       plott <- returned[[1]]
       texttheme <- returned[[2]]
	   to_download$theme_down_shelter <- plott
	   
	   output$theme_shelter_table_plot <- renderTable({
			 		 table_returned <- theme_sex_table_maker(shelter_dim_th)
					 table_returned
			 },sanitize.text.function = function(x) x)
       
     }
     
     if(input$shelter_th_type == "age")
     {
       returned <- theme_age_plotter(shelter_dim_th, "Shelter", theme, to_plot_theme)
       plott <- returned[[1]]
       texttheme <- returned[[2]]
	   to_download$theme_down_shelter <- plott
	   
	   				output$theme_shelter_table_plot <- renderTable({
			 		 table_returned <- theme_age_table_maker(shelter_dim_th)
					 table_returned
			 },sanitize.text.function = function(x) x)
     }
     
     output$shelter_themetype <- renderUI({
       texttheme   
       
     })
     
     
     ggplotly(plott) %>% config(displayModeBar = F)  
     
   })
   
   
        output$theme_down_shelter <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$theme_down_shelter)
 dev.off()
 }) 
   
   output$shelter_TreePlot <-  renderCollapsibleTree({
     
     dimension_data <- read.csv("dashboard.csv")
     shelter_dim <- subset(dimension_data, Dimension == "Shelter")
     
     tree_graph <- shelter_dim %>%
       group_by(Dimension,Theme, Indicator) %>%
       summarize(`Number of Indicators` = n()) %>%
       collapsibleTreeSummary(
         hierarchy = c("Theme", "Indicator"),
         root = "Shelter",
         width = 600,
         height = 700,
         attribute = "Number of Indicators",
         zoomable = FALSE
       )
     tree_graph
     
   })
   
   
   
   
   #########################
   
   output$shelter_SelectedPlot_question <- renderPlotly({
     
     
     que_code <- data[,c("roofing", "flooring", "walling", "overall.condition", "other.households")]
     
     
     if (input$shelter_questions == "Roofing Material")
     {
       
	   que_code$newcat <- que_code$roofing
	   que_code <- que_code[order(que_code$newcat), ]
	   
       que_code <- que_code %>% mutate(newcat = case_when(que_code$roofing == 10 ~ "Tin/Iron", 
                                                          que_code$roofing == 9 ~ "Wood",
                                                          que_code$roofing == 14 ~ "Other",
                                                          que_code$roofing == 12 ~ "Cement",
                                                          que_code$roofing == 6 ~ "Wood planks",
                                                          que_code$roofing == 2 ~ "Thatch/Palm",
                                                          que_code$roofing == 7 ~ "Cardboard",
                                                          que_code$roofing == 13 ~ "Roofing shingles")  
                                       )
       que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
       
	   que_code$newcat <- as.character(que_code$newcat)
	   que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat)) 
       
     }
     
     if (input$shelter_questions == "Flooring Material")
     {
	        
	   que_code$newcat <- que_code$flooring
	   que_code <- que_code[order(que_code$newcat), ]
       
       que_code <- que_code %>% mutate(newcat = case_when(que_code$flooring == 1 ~ "Earth/Mud",
                                                          que_code$flooring == 2 ~ "Wood Planks", 
                                                          que_code$flooring == 3 ~ "Palm/Bamboo",
                                                          que_code$flooring == 4 ~ "Parquet/Polished wood",
                                                          que_code$flooring == 9 ~ "Other",
                                                          que_code$flooring == 6 ~ "Ceramic",
                                                          que_code$flooring == 7 ~ "Cement",
                                                          que_code$flooring == 8 ~ "Carpet"))
       que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
       
	   que_code$newcat <- as.character(que_code$newcat)
	   que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat)) 
       
     }
     if (input$shelter_questions == "Exterior wall Material")
     {
       	   que_code$newcat <- que_code$walling
	   que_code <- que_code[order(que_code$newcat), ]
	   
       que_code <- que_code %>% mutate(newcat = case_when(que_code$walling == 1 ~ "No Walls",
                                                          que_code$walling == 2 ~ "Cane/Palm/Trunks", 
                                                          que_code$walling == 13 ~ "Cement bricks",
                                                          que_code$walling == 4 ~ "Bamboo w mud",
                                                          que_code$walling == 15 ~ "Other",
                                                          que_code$walling == 6 ~ "Tin/Iron",
                                                          que_code$walling == 10 ~ "Cement",
                                                          que_code$walling == 7 ~ "Plywood",
                                                          que_code$walling == 9 ~ "Re-use wood",
                                                          que_code$walling == 11 ~ "Stone w lime/Cement",
                                                          que_code$walling == 8 ~ "Cardboard",
                                                          que_code$walling == 12 ~ "Bricks")
                                       )
       que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
	   
   	   que_code$newcat <- as.character(que_code$newcat)
	   que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat)) 
           
     }
     
     if (input$shelter_questions == "Overall condition of the dwellings")
     {
		que_code$newcat <- que_code$overall.condition
	   que_code <- que_code[order(que_code$newcat), ]
	   
       que_code <- que_code %>% mutate(newcat = case_when(que_code$overall.condition == 1 ~ "Very Bad", 
                                                          que_code$overall.condition == 2 ~ "Poor",
                                                          que_code$overall.condition == 3 ~ "Moderate",
                                                          que_code$overall.condition == 4 ~ "Good",
                                                          que_code$overall.condition == 5 ~ "Excellent"))
       
       
       que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
       
	   que_code$newcat <- as.character(que_code$newcat)
	   que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat)) 
       
     }
     
     
     if (input$shelter_questions == "In the last year, did you ever sleep outdoors, in public places, or in temporary shelters, because you did not have access to suitable shelter of your own?")
     {
       
	   que_code$newcat <- que_code$other.households
	   que_code <- que_code[order(que_code$newcat), ]
       que_code <- que_code %>% mutate(newcat = case_when(que_code$other.households == 1 ~ "Yes", 
                                                          que_code$other.households == 2 ~ "No"))
       
       
       que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
       
	   que_code$newcat <- as.character(que_code$newcat)
	   que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat)) 
       
     }
     
     
     plott <- ggplot(que_code, aes(x=que_code$newcat,  fill=factor(newcat))) +
       geom_bar(position="dodge") + labs(fill= "Response") + xlab("Response to survey questions")+ scale_fill_brewer(name = "Response")+ 	theme(axis.text.x = element_text(size = 12, angle = 25))
     
	 to_download$ques_down_shelter <- plott
     
     ggplotly(plott) %>% config(displayModeBar = F)  
     
     
   })
   
   
output$ques_down_shelter <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$ques_down_shelter)
 dev.off()
 }) 
   ######################## 
   
   ################################### Health start #######################################
   output$SelectedPlot_health <- renderPlotly({
     
     
     health_wise <- data[, "score4"]
     names(health_wise)[names(health_wise) == "score4"] <- "dimen"
     
     
     if(input$healthDimensiontype == "default") {
       
       returned <- bar_default_plotter(health_wise, "Health")
       plott <- returned[[1]]
       textt <- returned[[2]]
	   to_download$down_health <- plott
	   		output$health_table_plot <- renderTable({
			 		 table_returned <- default_table_maker(health_wise)
					 table_returned
			 })
       
       
       
     }
     
     
     
     if(input$healthDimensiontype == "sex")
     {
       
       
       returned <- bar_gender_plotter(health_wise, "Health")
       plott <- returned[[1]]
       textt <- returned[[2]]
	   to_download$down_health <- plott
	   output$health_table_plot <- renderTable({
			 		 table_returned <- default_sex_table_maker(health_wise)
					 table_returned
			 },sanitize.text.function = function(x) x)
       
       
       
       
     }
     
     
     if (input$healthDimensiontype == "age")
     {
       
       returned <- bar_age_plotter(health_wise, "Health")
       plott <- returned[[1]]
       textt <- returned[[2]]
	   to_download$down_health <- plott
	    output$health_table_plot <- renderTable({
			 		 table_returned <- default_age_table_maker(health_wise)
					 table_returned
			 },sanitize.text.function = function(x) x)
       
     }
     
     output$health_htmltxt <- renderUI({
       textt    
       
     })
     
     
     ggplotly(plott) %>% config(displayModeBar = F)
     
 
   })


output$down_health <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$down_health)
 dev.off()
 }) 
   
   
   output$health_sunburst <- renderPlotly({
     health_wise <- data[, "score4"]
     names(health_wise)[names(health_wise) == "score4"] <- "dimen"
     
     returned <- sunburst_plotter(health_wise, "Health")
     
     output$health_caption <- renderUI({
       
       
       cap <- HTML( "<b>Number of people deprived categorized by gender</b>")
       cap
       
     })
     
     output$health_htmltxt1 <- renderUI({
       
       textt <- sunburst_text_stat(health_wise, "Health")
       textt
       
     })
     returned
     
     
   })
   
   output$health_SelectedPlot_theme <- renderPlotly({
     
     
     all_dim <- data[, grep("^score\\d+(\\.\\d{1,1})$",colnames(data),perl=TRUE,value = TRUE)]
     
     
     health_theme <- all_dim[, grep("score4", colnames(all_dim))]
     
     colnames(health_theme) <- var_label(health_theme) 
     theme <- input$health_inputthemetype
     
     to_plot_theme <- paste("Health -", paste(theme))
     
     health_dim_th <- health_theme %>% mutate(category = case_when(round(health_theme[to_plot_theme],0) == 0 ~ "Most deprived", 
                                                                     round(health_theme[to_plot_theme],0) == 1 ~ "Deprived", 
                                                                     round(health_theme[to_plot_theme],0) == 2 ~ "Somewhat deprived", 
                                                                     round(health_theme[to_plot_theme],0) == 3 ~ "Least deprived"))
     
     
     
     if(input$health_th_type == "sex")
     {
       
       returned <- theme_sex_plotter(health_dim_th, "Health", theme, to_plot_theme)
       plott <- returned[[1]]
       texttheme <- returned[[2]]
	   to_download$theme_down_health <- plott
	   output$theme_health_table_plot <- renderTable({
			 		 table_returned <- theme_sex_table_maker(health_dim_th)
					 table_returned
			 },sanitize.text.function = function(x) x)
       
     }
     
     if(input$health_th_type == "age")
     {
       returned <- theme_age_plotter(health_dim_th, "Health", theme, to_plot_theme)
       plott <- returned[[1]]
       texttheme <- returned[[2]]
	   
	   to_download$theme_down_health <- plott
	   
	   				output$theme_health_table_plot <- renderTable({
			 		 table_returned <- theme_age_table_maker(health_dim_th)
					 table_returned
			 },sanitize.text.function = function(x) x)
     }
     
     output$health_themetype <- renderUI({
       texttheme   
       
     })
     
     
     ggplotly(plott) %>% config(displayModeBar = F)  
     
   })
   
 
output$theme_down_health <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$theme_down_health)
 dev.off()
 }) 
   
   output$health_TreePlot <-  renderCollapsibleTree({
     
     dimension_data <- read.csv("dashboard.csv")
     health_dim <- subset(dimension_data, Dimension == "Health")
     
     tree_graph <- health_dim %>%
       group_by(Dimension,Theme, Indicator) %>%
       summarize(`Number of Indicators` = n()) %>%
       collapsibleTreeSummary(
         hierarchy = c("Theme", "Indicator"),
         root = "Health",
         width = 600,
         height = 700,
         attribute = "Number of Indicators",
         zoomable = FALSE
       )
     tree_graph
     
   })
   
   
   output$health_SelectedPlot_question <- renderPlotly({
     
     
     que_code <- data[,c("illness.injury", "affect.work", "length.affect","health.exposure","rate.exposure","receive.care","skill","clean","available","respect","wait","location")]
     
     
     if (input$health_questions == "When was the last time you had a significant illness or injury")
     {
	 		que_code$newcat <- que_code$illness.injury
	   que_code <- que_code[order(que_code$newcat), ]
       
       que_code <- que_code %>% mutate(newcat = case_when(que_code$illness.injury == 1 ~ "Within last 4 weeks", 
                                                          que_code$illness.injury == 2 ~ "Within last six months",
                                                          que_code$illness.injury == 3 ~ "Within last year",
                                                          que_code$illness.injury == 4 ~ "Over one year"
                                                         )  
       )
       que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
	   
	   que_code$newcat <- as.character(que_code$newcat)
	   que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat)) 
       
       
     }
     
     if (input$health_questions == "Did this illness/injury make it impossible or very difficult to perform your usual paid or unpaid activity")
     {
		
		que_code$newcat <- que_code$affect.work
	   que_code <- que_code[order(que_code$newcat), ]
	   
       que_code <- que_code %>% mutate(newcat = case_when(que_code$affect.work == 1 ~ "Yes",
                                                          que_code$affect.work == 2 ~ "No"
                                                         ))
       que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
       
	   	   
	   que_code$newcat <- as.character(que_code$newcat)
	   que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat)) 
       
     }
     if (input$health_questions == "How long was it difficult or impossible for you to perform your usual paid or unpaid activity because of this illness/injury")
     {
		que_code$newcat <- que_code$length.affect
	   que_code <- que_code[order(que_code$newcat), ]
	          
	   
       que_code <- que_code %>% mutate(newcat = case_when(que_code$length.affect == 1 ~ "More than 2 weeks",
                                                          que_code$length.affect == 2 ~ "1-2 weeks", 
                                                          que_code$length.affect == 3 ~ "Several days",
                                                          que_code$length.affect == 4 ~ "Don't know"
                                                          )
       )
       que_code$newcat[is.na(que_code$newcat)] <- "Unknown"

	   que_code$newcat <- as.character(que_code$newcat)
	   que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat))        
     }
     
     if (input$health_questions == "Do you experience any health problems from exposure to the smoke and fumes from your cooking and/or heating fuel")
     {
       
	   
       que_code <- que_code %>% mutate(newcat = case_when(que_code$health.exposure == 1 ~ "Yes", 
                                                          que_code$health.exposure == 2 ~ "No"
                                                          ))
       
       
       que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
       
       
     }
     
     
     if (input$health_questions == "How would you rate these problems")
     {
	 que_code$newcat <- que_code$rate.exposure
	   que_code <- que_code[order(que_code$newcat), ]
	          
       que_code <- que_code %>% mutate(newcat = case_when(que_code$rate.exposure == 1 ~ "Severe", 
                                                          que_code$rate.exposure == 2 ~ "Moderate",
                                                          que_code$rate.exposure == 3 ~ "Minor"
                                                          ))
       
       
       que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
       
	   que_code$newcat <- as.character(que_code$newcat)
	   que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat))     
       
     }
     
     
     if (input$health_questions == "The last time you had an illness/injury that needed healthcare, did you receive this care")
     {
       
       que_code <- que_code %>% mutate(newcat = case_when(que_code$receive.care == 1 ~ "Yes",
                                                          que_code$receive.care == 2 ~ "No"
       ))
       que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
       
       
     }
     
     if (input$health_questions == "skill of the healthcare provider")
     {
       
       que_code <- que_code %>% mutate(newcat = case_when(que_code$skill == 1 ~ "Yes",
                                                          que_code$skill == 2 ~ "No"
       ))
       que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
       
       
     }
     
     if (input$health_questions == "cleanliness of the treatment facility")
     {
       
       que_code <- que_code %>% mutate(newcat = case_when(que_code$clean == 1 ~ "Yes",
                                                          que_code$clean == 2 ~ "No"
       ))
       que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
       
       
     }
     
     
     if (input$health_questions == "availability of prescribed drugs")
     {
       
       que_code <- que_code %>% mutate(newcat = case_when(que_code$available == 1 ~ "Yes",
                                                          que_code$available == 2 ~ "No"
       ))
       que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
       
       
     }
     
     if (input$health_questions == "level of respect with which you were treated")
     {
       
       que_code <- que_code %>% mutate(newcat = case_when(que_code$respect == 1 ~ "Yes",
                                                          que_code$respect == 2 ~ "No"
       ))
       que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
       
       
     }
     
     if (input$health_questions == "waiting time to receive treatment")
     {
       
       que_code <- que_code %>% mutate(newcat = case_when(que_code$wait == 1 ~ "Yes",
                                                          que_code$wait == 2 ~ "No"
       ))
       que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
       
       
     }
     
     if (input$health_questions == "the location of the health-care provider")
     {
       
       que_code <- que_code %>% mutate(newcat = case_when(que_code$location == 1 ~ "Yes",
                                                          que_code$location == 2 ~ "No"
       ))
       que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
       
       
     }
     
     
     plott <- ggplot(que_code, aes(x=que_code$newcat,  fill=factor(newcat))) +
       geom_bar(position="dodge") + labs(fill= "Response") + xlab("Response to survey questions")+ scale_fill_brewer(name = "Response")+ 	theme(axis.text.x = element_text(size = 12, angle = 25))
     to_download$ques_down_health <- plott
     
     ggplotly(plott) %>% config(displayModeBar = F)  
     
     
   })
   
   
   output$ques_down_health <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$ques_down_health)
 dev.off()
 }) 
   
   ######################## 
   
   
   ################################### Education start #######################################
   output$SelectedPlot_edu <- renderPlotly({
     
     
     edu_wise <- data[, "score5"]
     names(edu_wise)[names(edu_wise) == "score5"] <- "dimen"
     
     
     if(input$eduDimensiontype == "default") {
       
       returned <- bar_default_plotter(edu_wise, "Education")
       plott <- returned[[1]]
       textt <- returned[[2]]
	   
	   to_download$down_edu <- plott
	   		output$edu_table_plot <- renderTable({
			 		 table_returned <- default_table_maker(edu_wise)
					 table_returned
			 })
       
       
       
     }
     
     
     
     if(input$eduDimensiontype == "sex")
     {
       
       
       returned <- bar_gender_plotter(edu_wise, "Education")
       plott <- returned[[1]]
       textt <- returned[[2]]
	   to_download$down_edu <- plott
	   output$edu_table_plot <- renderTable({
			 		 table_returned <- default_sex_table_maker(edu_wise)
					 table_returned
			 },sanitize.text.function = function(x) x)
       
       
       
       
     }
     
     
     if (input$eduDimensiontype == "age")
     {
       
       returned <- bar_age_plotter(edu_wise, "Education")
       plott <- returned[[1]]
       textt <- returned[[2]]
	   
	   to_download$down_edu <- plott
	   
        output$edu_table_plot <- renderTable({
			 		 table_returned <- default_age_table_maker(edu_wise)
					 table_returned
			 },sanitize.text.function = function(x) x)
     }
     
     output$edu_htmltxt <- renderUI({
       textt    
       
     })
     
     
     ggplotly(plott) %>% config(displayModeBar = F)

     
   })

   output$down_edu <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$down_edu)
 dev.off()
 }) 

   
   
   output$edu_sunburst <- renderPlotly({
     edu_wise <- data[, "score5"]
     names(edu_wise)[names(edu_wise) == "score5"] <- "dimen"
     
     returned <- sunburst_plotter(edu_wise, "Education")
     
     output$edu_caption <- renderUI({
       
       
       cap <- HTML( "<b>Number of people deprived categorized by gender</b>")
       cap
       
     })
     
     output$edu_htmltxt1 <- renderUI({
       
       textt <- sunburst_text_stat(edu_wise, "Education")
       textt
       
     })
     returned
     
     
   })
   
   output$edu_SelectedPlot_theme <- renderPlotly({
     
     
     all_dim <- data[, grep("^score\\d+(\\.\\d{1,1})$",colnames(data),perl=TRUE,value = TRUE)]
     
     
     edu_theme <- all_dim[, grep("score5", colnames(all_dim))]
     
     colnames(edu_theme) <- var_label(edu_theme) 
     theme <- input$edu_inputthemetype
     
     to_plot_theme <- paste("Education -", paste(theme))
     
     edu_dim_th <- edu_theme %>% mutate(category = case_when(round(edu_theme[to_plot_theme],0) == 0 ~ "Most deprived", 
                                                                   round(edu_theme[to_plot_theme],0) == 1 ~ "Deprived", 
                                                                   round(edu_theme[to_plot_theme],0) == 2 ~ "Somewhat deprived", 
                                                                   round(edu_theme[to_plot_theme],0) == 3 ~ "Least deprived"))
     
     
     
     if(input$edu_th_type == "sex")
     {
       
       returned <- theme_sex_plotter(edu_dim_th, "Education", theme, to_plot_theme)
       plott <- returned[[1]]
       texttheme <- returned[[2]]
	   
	   to_download$theme_down_edu <- plott
	   
	   output$theme_edu_table_plot <- renderTable({
			 		 table_returned <- theme_sex_table_maker(edu_dim_th)
					 table_returned
			 },sanitize.text.function = function(x) x)
       
     }
     
     if(input$edu_th_type == "age")
     {
       returned <- theme_age_plotter(edu_dim_th, "Education", theme, to_plot_theme)
       plott <- returned[[1]]
       texttheme <- returned[[2]]
	   to_download$theme_down_edu <- plott
	   
	   				output$theme_edu_table_plot <- renderTable({
			 		 table_returned <- theme_age_table_maker(edu_dim_th)
					 table_returned
			 },sanitize.text.function = function(x) x)
     }
     
     output$edu_themetype <- renderUI({
       texttheme   
       
     })
     
     
     ggplotly(plott) %>% config(displayModeBar = F)  
     
   })
  
 
 output$theme_down_edu <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$theme_down_edu)
 dev.off()
 }) 
   
   output$edu_TreePlot <-  renderCollapsibleTree({
     
     dimension_data <- read.csv("dashboard.csv")
     edu_dim <- subset(dimension_data, Dimension == "Education")
     
     tree_graph <- edu_dim %>%
       group_by(Dimension,Theme, Indicator) %>%
       summarize(`Number of Indicators` = n()) %>%
       collapsibleTreeSummary(
         hierarchy = c("Theme", "Indicator"),
         root = "Education",
         width = 600,
         height = 700,
         attribute = "Number of Indicators",
         zoomable = FALSE
       )
     tree_graph
     
   })
   
   
   output$edu_SelectedPlot_question <- renderPlotly({
     
     
     que_code <- data[,c("attend.school","years.schooling","read","read.english","check.read.english","read.fijian","check.read.other","write","write.english","check.write","write.other","check.write.other","arithmetic","check.add","check.multiply")]
     
     
     if (input$edu_questions == "Have you ever attended school")
     {
       
       que_code <- que_code %>% mutate(newcat = case_when(que_code$attend.school == 1 ~ "Yes", 
                                                          que_code$attend.school == 2 ~ "No"
                                                          
       )  
       )
       que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
       
       
     }
     
     if (input$edu_questions == "How many years were you in formal schooling")
     {
       
	   	 que_code$newcat <- que_code$years.schooling
	   que_code <- que_code[order(que_code$newcat), ]
	   
       que_code <- que_code %>% mutate(newcat = case_when(que_code$years.schooling == 16 ~ "16", 
                                                          que_code$years.schooling == 9 ~ "9",
                                                          que_code$years.schooling == 10 ~ "10",
                                                          que_code$years.schooling == 17 ~ "17",
                                                          que_code$years.schooling == 7 ~ "7",
                                                          que_code$years.schooling == 8 ~ "8",
                                                          que_code$years.schooling == 5 ~ "5",
                                                          que_code$years.schooling == 4 ~ "4",
                                                          que_code$years.schooling == 11 ~ "11",
                                                          que_code$years.schooling == 14 ~ "14"
       ))
       que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
       
       que_code$newcat <- as.character(que_code$newcat)
	   que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat))     
	   
     }
     if (input$edu_questions == "Are you able to read at all")
     {
       
	          
	   	 que_code$newcat <- que_code$read
	   que_code <- que_code[order(que_code$newcat), ]
	   
       que_code <- que_code %>% mutate(newcat = case_when(que_code$read == 1 ~ "Yes",
                                                          que_code$read == 2 ~ "No", 
                                                          que_code$read == 0 ~ "Others"
                                                         
       )
       )
       que_code$newcat[is.na(que_code$newcat)] <- "Unknown"

       que_code$newcat <- as.character(que_code$newcat)
	   que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat))         
	
     }
     
     if (input$edu_questions == "Are you able to read English")
     {
       
       que_code <- que_code %>% mutate(newcat = case_when(que_code$read.english == 1 ~ "Yes", 
                                                          que_code$read.english == 2 ~ "No"
       ))
       
       
       que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
       
       
     }
     
     
     if (input$edu_questions == "Please read the following sentences aloud to me")
     {
	 
	 	   	 que_code$newcat <- que_code$check.read.english
	   que_code <- que_code[order(que_code$newcat), ]
       
       que_code <- que_code %>% mutate(newcat = case_when(que_code$check.read.english == 1 ~ "Cannot read", 
                                                          que_code$check.read.english == 2 ~ "Able only parts",
                                                          que_code$check.read.english == 3 ~ "Able full sentences"
       ))
       
       
       que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
  
       que_code$newcat <- as.character(que_code$newcat)
	   que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat))          
       
     }
     
     
     if (input$edu_questions == "Are you able to read ITaukei, Hindi or other language")
     {
       
       que_code <- que_code %>% mutate(newcat = case_when(que_code$read.fijian == 1 ~ "Yes",
                                                          que_code$read.fijian == 2 ~ "No"
       ))
       que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
       
       
     }
     
     if (input$edu_questions == "Please read the following sentences aloud to me")
     {

	 	   	 que_code$newcat <- que_code$check.read.other
	   que_code <- que_code[order(que_code$newcat), ]
	   
       que_code <- que_code %>% mutate(newcat = case_when(que_code$check.read.other == 1 ~ "Cannot read",
                                                          que_code$check.read.other == 2 ~ "Able only parts",
                                                          que_code$check.read.other == 3 ~ "Able full sentences",
                                                          que_code$check.read.other == 4 ~ "Show card not available"
       ))
       que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
 
       que_code$newcat <- as.character(que_code$newcat)
	   que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat))    
       
     }
     
     if (input$edu_questions == "Are you able to write at all")
     {
       
       que_code <- que_code %>% mutate(newcat = case_when(que_code$write == 1 ~ "Yes",
                                                          que_code$write == 2 ~ "No"
       ))
       que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
       
       
     }
     
     
     if (input$edu_questions == "Are you able to write in English")
     {
       
       que_code <- que_code %>% mutate(newcat = case_when(que_code$write.english == 1 ~ "Yes",
                                                          que_code$write.english == 2 ~ "No"
       ))
       que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
       
       
     }
     
     if (input$edu_questions == "Please write two sentences about what you did yesterday")
     {
       
	   que_code$newcat <- que_code$check.write
	   que_code <- que_code[order(que_code$newcat), ]
	   
       que_code <- que_code %>% mutate(newcat = case_when(que_code$check.write == 1 ~ "Illegible",
                                                          que_code$check.write == 2 ~ "Legible but poor",
                                                          que_code$check.write == 3 ~ "Legible and good"
       ))
       que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
       

       que_code$newcat <- as.character(que_code$newcat)
	   que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat)) 
     }
     
     if (input$edu_questions == "Are you able to write ITaukei, Hindi or other language")
     {
       
       que_code <- que_code %>% mutate(newcat = case_when(que_code$write.other == 1 ~ "Yes",
                                                          que_code$write.other == 2 ~ "No"
       ))
       que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
       
       
     }
     
     if (input$edu_questions == "Please write two sentences in ITaukei, Hindi or other language")
     {
	 
	   que_code$newcat <- que_code$check.write.other 
	   que_code <- que_code[order(que_code$newcat), ]
       
       que_code <- que_code %>% mutate(newcat = case_when(que_code$check.write.other == 1 ~ "Illegible",
                                                          que_code$check.write.other == 2 ~ "Legible but poor",
                                                          que_code$check.write.other == 2 ~ "Legible and good"
       ))
       que_code$newcat[is.na(que_code$newcat)] <- "Unknown"

       que_code$newcat <- as.character(que_code$newcat)
	   que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat)) 
       
     }
     
     if (input$edu_questions == "Are you able to do some mathematics")
     {
	 	   que_code$newcat <- que_code$arithmetic 
	   que_code <- que_code[order(que_code$newcat), ]
       
       que_code <- que_code %>% mutate(newcat = case_when(que_code$arithmetic == 1 ~ "Illegible",
                                                          que_code$arithmetic == 2 ~ "Legible but poor"
                                                          
       ))
       que_code$newcat[is.na(que_code$newcat)] <- "Unknown"

       que_code$newcat <- as.character(que_code$newcat)
	   que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat))   
       
     }
     if (input$edu_questions == "Addition and subtraction problem")
     {
       
       que_code <- que_code %>% mutate(newcat = case_when(que_code$check.add == 1 ~ "Correct",
                                                          que_code$check.add == 2 ~ "Incorrect"
                                                          
       ))
       que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
       
       
     }
     if (input$edu_questions == "Multiplication and division problem")
     {
       
       que_code <- que_code %>% mutate(newcat = case_when(que_code$check.multiply == 1 ~ "Correct",
                                                          que_code$check.multiply == 2 ~ "Incorrect"
                                                          
       ))
       que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
       
       
     }
     
     
     plott <- ggplot(que_code, aes(x=que_code$newcat,  fill=factor(newcat))) +
       geom_bar(position="dodge") + labs(fill= "Response") + xlab("Response to survey questions")+ scale_fill_brewer(name = "Response")+ 	theme(axis.text.x = element_text(size = 12, angle = 25))
     
	 to_download$ques_down_edu <- plott
     
     ggplotly(plott) %>% config(displayModeBar = F)  
     
     
   })
 
  output$ques_down_edu <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$ques_down_edu)
 dev.off()
 }) 
   ######################## 
   
   ################################### Energy/Fuel start #######################################
   output$SelectedPlot_energy <- renderPlotly({
     
     
     energy_wise <- data[, "score6"]
     names(energy_wise)[names(energy_wise) == "score6"] <- "dimen"
     
     
     if(input$energyDimensiontype == "default") {
       
       returned <- bar_default_plotter(energy_wise, "Energy/Fuel")
       plott <- returned[[1]]
       textt <- returned[[2]]
	   
	   to_download$down_energy <- plott
	   		output$energy_table_plot <- renderTable({
			 		 table_returned <- default_table_maker(energy_wise)
					 table_returned
			 })
       
       
       
     }
     
     
     
     if(input$energyDimensiontype == "sex")
     {
       
       
       returned <- bar_gender_plotter(energy_wise, "Energy/Fuel")
       plott <- returned[[1]]
       textt <- returned[[2]]
	   to_download$down_energy <- plott
	   
       output$energy_table_plot <- renderTable({
			 		 table_returned <- default_sex_table_maker(energy_wise)
					 table_returned
			 },sanitize.text.function = function(x) x)
       
       
       
     }
     
     
     if (input$energyDimensiontype == "age")
     {
       
       returned <- bar_age_plotter(energy_wise, "Energy/Fuel")
       plott <- returned[[1]]
       textt <- returned[[2]]
	   to_download$down_energy <- plott
	    output$energy_table_plot <- renderTable({
			 		 table_returned <- default_age_table_maker(energy_wise)
					 table_returned
			 },sanitize.text.function = function(x) x)
       
     }
     
     output$energy_htmltxt <- renderUI({
       textt    
       
     })
     
     
     ggplotly(plott) %>% config(displayModeBar = F)

     
   })


 
  output$down_energy <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$down_energy)
 dev.off()
 }) 
   
   
   output$energy_sunburst <- renderPlotly({
     energy_wise <- data[, "score6"]
     names(energy_wise)[names(energy_wise) == "score6"] <- "dimen"
     
     returned <- sunburst_plotter(energy_wise, "Energy")
     
     output$energy_caption <- renderUI({
       
       
       cap <- HTML( "<b>Number of people deprived categorized by gender</b>")
       cap
       
     })
     
     output$energy_htmltxt1 <- renderUI({
       
       textt <- sunburst_text_stat(energy_wise,"energy")
       textt
       
     })
     returned
     
     
   })
   
   output$energy_SelectedPlot_theme <- renderPlotly({
     
     
     all_dim <- data[, grep("^score\\d+(\\.\\d{1,1})$",colnames(data),perl=TRUE,value = TRUE)]
     
     
     energy_theme <- all_dim[, grep("score6", colnames(all_dim))]
     
     colnames(energy_theme) <- var_label(energy_theme) 
     theme <- input$energy_inputthemetype
     
     to_plot_theme <- paste("Energy/Fuel -", paste(theme))
     
     energy_dim_th <- energy_theme %>% mutate(category = case_when(round(energy_theme[to_plot_theme],0) == 0 ~ "Most deprived", 
                                                                 round(energy_theme[to_plot_theme],0) == 1 ~ "Deprived", 
                                                                 round(energy_theme[to_plot_theme],0) == 2 ~ "Somewhat deprived", 
                                                                 round(energy_theme[to_plot_theme],0) == 3 ~ "Least deprived"))
     
     
     
     if(input$energy_th_type == "sex")
     {
       
       returned <- theme_sex_plotter(energy_dim_th, "Energy/Fuel", theme, to_plot_theme)
       plott <- returned[[1]]
       texttheme <- returned[[2]]
	   
	   to_download$theme_down_energy <- plott
	   
	   output$theme_energy_table_plot <- renderTable({
			 		 table_returned <- theme_sex_table_maker(energy_dim_th)
					 table_returned
			 },sanitize.text.function = function(x) x)
       
     }
     
     if(input$energy_th_type == "age")
     {
       returned <- theme_age_plotter(energy_dim_th, "Energy/Fuel", theme, to_plot_theme)
       plott <- returned[[1]]
       texttheme <- returned[[2]]
	   
	   to_download$theme_down_energy <- plott
	   
	   				output$theme_energy_table_plot <- renderTable({
			 		 table_returned <- theme_age_table_maker(energy_dim_th)
					 table_returned
			 },sanitize.text.function = function(x) x)
     }
     
     output$energy_themetype <- renderUI({
       texttheme   
       
     })
     
     
     ggplotly(plott) %>% config(displayModeBar = F)  
     
   })
 
 
   output$theme_down_energy <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$theme_down_energy)
 dev.off()
 }) 
   
   output$energy_TreePlot <-  renderCollapsibleTree({
     
     dimension_data <- read.csv("dashboard.csv")
     energy_dim <- subset(dimension_data, Dimension == "Energy")
     
     tree_graph <- energy_dim %>%
       group_by(Dimension,Theme, Indicator) %>%
       summarize(`Number of Indicators` = n()) %>%
       collapsibleTreeSummary(
         hierarchy = c("Theme", "Indicator"),
         root = "Energy",
         width = 600,
         height = 700,
         attribute = "Number of Indicators",
         zoomable = FALSE
       )
     tree_graph
     
   })
   
   
   
   output$energy_SelectedPlot_question <- renderPlotly({
     
     
     que_code <- data[,c("with.electricity", "hrs.electricity", "reliable.electricity", "cooking.fuel")]
     
     
     if (input$energy_questions == "Does your dwelling have access to electricity")
     {
			   que_code$newcat <- que_code$with.electricity 
	   que_code <- que_code[order(que_code$newcat), ]
       
       que_code <- que_code %>% mutate(newcat = case_when(que_code$with.electricity == 1 ~ "Yes", 
                                                          que_code$with.electricity == 2 ~ "No"
                                                         ))
       
              que_code$newcat <- as.character(que_code$newcat)
	   que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat)) 
     }
     
     if (input$energy_questions == "Approximately how many hours per day on average does your dwelling have electricity")
     {
	 			   que_code$newcat <- que_code$hrs.electricity 
	   que_code <- que_code[order(que_code$newcat), ]
       
       que_code <- que_code %>% mutate(newcat = case_when(que_code$hrs.electricity == 4 ~ "4",
                                                          que_code$hrs.electricity == 24 ~ "24", 
                                                          que_code$hrs.electricity == 9 ~ "9",
                                                          que_code$hrs.electricity == 12 ~ "12",
                                                          que_code$hrs.electricity == NA ~ "NA",
                                                          que_code$hrs.electricity == 15 ~ "15",
                                                          que_code$hrs.electricity == 18 ~ "18",
                                                          que_code$hrs.electricity == 3 ~ "3",
                                                          que_code$hrs.electricity == 8 ~ "8",
                                                          que_code$hrs.electricity == 10 ~ "10",
                                                          que_code$hrs.electricity == 7 ~ "7",
                                                          que_code$hrs.electricity == 2 ~ "2",
                                                          que_code$hrs.electricity == 6 ~ "6",
                                                          que_code$hrs.electricity == 5 ~ "5",
                                                          que_code$hrs.electricity == 1 ~ "1",
                                                          que_code$hrs.electricity == 0 ~ "0"))
       

              que_code$newcat <- as.character(que_code$newcat)
	   que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat)) 
     }
     if (input$energy_questions == "How reliable is your dwelling's access to electricity")
     {
	 	 			   que_code$newcat <- que_code$reliable.electricity 
	   que_code <- que_code[order(que_code$newcat), ]
       
       que_code <- que_code %>% mutate(newcat = case_when(que_code$reliable.electricity == 1 ~ "Very unreliable",
                                                          que_code$reliable.electricity == 2 ~ "Somewhat reliable",
                                                          que_code$reliable.electricity == 3 ~ "Quite unreliable",
                                                          que_code$reliable.electricity == 4 ~ "Very reliable",
                                                          que_code$reliable.electricity == NA ~ "NA"))
                     que_code$newcat <- as.character(que_code$newcat)
	   que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat)) 
     }
     
     if (input$energy_questions == "What is the primary source of cooking fuel in this household")
     {
	 	 	 			   que_code$newcat <- que_code$cooking.fuel
	   que_code <- que_code[order(que_code$newcat), ]
       
       que_code <- que_code %>% mutate(newcat = case_when(que_code$cooking.fuel == 1 ~ "Crop residue", 
                                                          que_code$cooking.fuel == 3 ~ "Firewood",
                                                          que_code$cooking.fuel == 4 ~ "Charcoal",
                                                          que_code$cooking.fuel == 5 ~ "Kerosene", 
                                                          que_code$cooking.fuel == 6 ~ "Gas",
                                                          que_code$cooking.fuel == 7 ~ "Electricity",
                                                          que_code$cooking.fuel == 8 ~ "Other"))
       
       
       que_code$newcat[is.na(que_code$newcat)] <- "Unknown"
       
                          que_code$newcat <- as.character(que_code$newcat)
	   que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat))   
     }
     
     
     plott <- ggplot(que_code, aes(x=que_code$newcat,  fill=factor(newcat))) +
       geom_bar(position="dodge") + labs(fill= "Response") + xlab("Response to survey questions")+ scale_fill_brewer(name = "Response")+ 	theme(axis.text.x = element_text(size = 12, angle = 25))
     
     to_download$ques_down_energy <- plott
     ggplotly(plott) %>% config(displayModeBar = F)  
     
     
   })
   
   
 output$ques_down_energy <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$ques_down_energy)
 dev.off()
 }) 
   
   ########################
   
   
   ################################### Sanitation start #######################################
   output$SelectedPlot_san <- renderPlotly({
     
     
     san_wise <- data[, "score7"]
     names(san_wise)[names(san_wise) == "score7"] <- "dimen"
     
     
     if(input$sanDimensiontype == "default") {
       
       returned <- bar_default_plotter(san_wise, "Sanitation")
       plott <- returned[[1]]
       textt <- returned[[2]]
	   to_download$down_sanitation <- plott
       		output$sanitation_table_plot <- renderTable({
			 		 table_returned <- default_table_maker(san_wise)
					 table_returned
			 })
       
       
     }
     
     
     
     if(input$sanDimensiontype == "sex")
     {
       
       
       returned <- bar_gender_plotter(san_wise, "Sanitation")
       plott <- returned[[1]]
       textt <- returned[[2]]
	   to_download$down_sanitation <- plott
       
       output$sanitation_table_plot <- renderTable({
			 		 table_returned <- default_sex_table_maker(san_wise)
					 table_returned
			 },sanitize.text.function = function(x) x)
       
       
     }
     
     
     if (input$sanDimensiontype == "age")
     {
       
       returned <- bar_age_plotter(san_wise, "Sanitation")
       plott <- returned[[1]]
       textt <- returned[[2]]
	   to_download$down_sanitation <- plott
        output$sanitation_table_plot <- renderTable({
			 		 table_returned <- default_age_table_maker(san_wise)
					 table_returned
			 },sanitize.text.function = function(x) x)
     }
     
     output$san_htmltxt <- renderUI({
       textt    
       
     })
     
     
     ggplotly(plott) %>% config(displayModeBar = F)
     
     
     
     
   })

 output$down_sanitation <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$down_sanitation)
 dev.off()
 })   
 
 
   output$san_sunburst <- renderPlotly({
     san_wise <- data[, "score7"]
     names(san_wise)[names(san_wise) == "score7"] <- "dimen"
     
     returned <- sunburst_plotter(san_wise, "Sanitation")
     
     output$san_caption <- renderUI({
       
       
       cap <- HTML( "<b>Number of people deprived categorized by gender</b>")
       cap
       
     })
     
     output$san_htmltxt1 <- renderUI({
       
       textt <- sunburst_text_stat(san_wise, "sanitation")
       textt
       
     })
     returned
     
     
   })
   
   output$san_SelectedPlot_theme <- renderPlotly({
     
     
     all_dim <- data[, grep("^score\\d+(\\.\\d{1,1})$",colnames(data),perl=TRUE,value = TRUE)]
     
     
     san_theme <- all_dim[, grep("score7", colnames(all_dim))]
     
     colnames(san_theme) <- var_label(san_theme) 
     theme <- input$san_inputthemetype
     
     to_plot_theme <- paste("Sanitation -", paste(theme))
     
     san_dim_th <- san_theme %>% mutate(category = case_when(round(san_theme[to_plot_theme],0) == 0 ~ "Most deprived", 
                                                                   round(san_theme[to_plot_theme],0) == 1 ~ "Deprived", 
                                                                   round(san_theme[to_plot_theme],0) == 2 ~ "Somewhat deprived", 
                                                                   round(san_theme[to_plot_theme],0) == 3 ~ "Least deprived"))
     
     
     
     if(input$san_th_type == "sex")
     {
       
       returned <- theme_sex_plotter(san_dim_th, "Sanitation", theme, to_plot_theme)
       plott <- returned[[1]]
       texttheme <- returned[[2]]
	   to_download$theme_down_sanitation <- plott
	   output$theme_sanitation_table_plot <- renderTable({
			 		 table_returned <- theme_sex_table_maker(san_dim_th)
					 table_returned
			 },sanitize.text.function = function(x) x)
       
     }
     
     if(input$san_th_type == "age")
     {
       returned <- theme_age_plotter(san_dim_th, "Sanitation", theme, to_plot_theme)
       plott <- returned[[1]]
       texttheme <- returned[[2]]
	   to_download$theme_down_sanitation <- plott
	   				output$theme_sanitation_table_plot <- renderTable({
			 		 table_returned <- theme_age_table_maker(san_dim_th)
					 table_returned
			 },sanitize.text.function = function(x) x)
     }
     
     output$san_themetype <- renderUI({
       texttheme   
       
     })
     
     
     ggplotly(plott) %>% config(displayModeBar = F)  
     
   })
   
    output$theme_down_sanitation <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$theme_down_sanitation)
 dev.off()
 })   
   
   
   output$san_TreePlot <-  renderCollapsibleTree({
     
     dimension_data <- read.csv("dashboard.csv")
     san_dim <- subset(dimension_data, Dimension == "Sanitation")
     
     tree_graph <- san_dim %>%
       group_by(Dimension,Theme, Indicator) %>%
       summarize(`Number of Indicators` = n()) %>%
       collapsibleTreeSummary(
         hierarchy = c("Theme", "Indicator"),
         root = "Sanitation",
         width = 600,
         height = 700,
         attribute = "Number of Indicators",
         zoomable = FALSE
       )
     tree_graph
     
   })
   
   
   
   output$san_SelectedPlot_question <- renderPlotly({
     
     
     que_code <- data[,c("toilet")]
     
     
     if (input$san_questions == "What toilet facilities do the household use")
     {
	 que_code$newcat <- que_code$toilet
	   que_code <- que_code[order(que_code$newcat), ]
       que_code <- que_code %>% mutate(newcat = case_when(que_code$toilet == 1 ~ "Bush/field/river", 
                                                          que_code$toilet == 2 ~ "Bucket",
                                                          que_code$toilet == 3 ~ "Pit without slab", 
                                                          que_code$toilet == 4 ~ "Pit slab",
                                                          que_code$toilet == 5 ~ "Water sealed toilet", 
                                                          que_code$toilet == 6 ~ "Public flush toilet",
                                                          que_code$toilet == 7 ~ "Private flush toilet", 
                                                          que_code$toilet == 8 ~ "Other"
       ))
       
	   que_code$newcat <- as.character(que_code$newcat)
	   que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat)) 
	   
     }
     
    
     plott <- ggplot(que_code, aes(x=que_code$newcat,  fill=factor(newcat))) +
       geom_bar(position="dodge") + labs(fill= "Response") + xlab("Response to survey questions")+ scale_fill_brewer(name = "Response")+ 	theme(axis.text.x = element_text(size = 12, angle = 25))
     to_download$ques_down_sanitation <- plott
     
     ggplotly(plott) %>% config(displayModeBar = F)  
     
     
   })
   
   output$ques_down_sanitation <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$ques_down_sanitation)
 dev.off()
 })   
   
   
   
   ################################### Relationships start #######################################
   output$SelectedPlot_rel <- renderPlotly({
     
     
     rel_wise <- data[, "score8"]
     names(rel_wise)[names(rel_wise) == "score8"] <- "dimen"
     
     
     if(input$relDimensiontype == "default") {
       
       returned <- bar_default_plotter(rel_wise, "Relationships")
       plott <- returned[[1]]
       textt <- returned[[2]]
	   to_download$down_relation <- plott
       		output$relation_table_plot <- renderTable({
			 		 table_returned <- default_table_maker(rel_wise)
					 table_returned
			 })
       
       
     }
     
     
     
     if(input$relDimensiontype == "sex")
     {
       
       
       returned <- bar_gender_plotter(rel_wise, "Relationships")
       plott <- returned[[1]]
       textt <- returned[[2]]
	   to_download$down_relation <- plott
       output$relation_table_plot <- renderTable({
			 		 table_returned <- default_sex_table_maker(rel_wise)
					 table_returned
			 },sanitize.text.function = function(x) x)
       
       
       
     }
     
     
     if (input$relDimensiontype == "age")
     {
       
       returned <- bar_age_plotter(rel_wise, "Relationships")
       plott <- returned[[1]]
       textt <- returned[[2]]
	   to_download$down_relation <- plott
	    output$relation_table_plot <- renderTable({
			 		 table_returned <- default_age_table_maker(rel_wise)
					 table_returned
			 },sanitize.text.function = function(x) x)
       
     }
     
     output$rel_htmltxt <- renderUI({
       textt    
       
     })
     
     
     ggplotly(plott) %>% config(displayModeBar = F)
     
     
     
     
   })
 
   output$down_relation <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$down_relation)
 dev.off()
 })   
   
   
   
   output$rel_sunburst <- renderPlotly({
     rel_wise <- data[, "score8"]
     names(rel_wise)[names(rel_wise) == "score8"] <- "dimen"
     
     returned <- sunburst_plotter(rel_wise, "Relationships")
     
     output$rel_caption <- renderUI({
       
       
       cap <- HTML( "<b>Number of people deprived categorized by gender</b>")
       cap
       
     })
     
     output$rel_htmltxt1 <- renderUI({
       
       textt <- sunburst_text_stat(rel_wise, "relationship")
       textt
       
     })
     returned
     
     
   })
   
   output$rel_SelectedPlot_theme <- renderPlotly({
     
     
     all_dim <- data[, grep("^score\\d+(\\.\\d{1,1})$",colnames(data),perl=TRUE,value = TRUE)]
     
     
     rel_theme <- all_dim[, grep("score8", colnames(all_dim))]
     
     colnames(rel_theme) <- var_label(rel_theme) 
     theme <- input$rel_inputthemetype
     
     to_plot_theme <- paste("Relationships -", paste(theme))
     
     rel_dim_th <- rel_theme %>% mutate(category = case_when(round(rel_theme[to_plot_theme],0) == 0 ~ "Most deprived", 
                                                             round(rel_theme[to_plot_theme],0) == 1 ~ "Deprived", 
                                                             round(rel_theme[to_plot_theme],0) == 2 ~ "Somewhat deprived", 
                                                             round(rel_theme[to_plot_theme],0) == 3 ~ "Least deprived"))
     
     
     
     if(input$rel_th_type == "sex")
     {
       
       returned <- theme_sex_plotter(rel_dim_th, "Relationships", theme, to_plot_theme)
       plott <- returned[[1]]
       texttheme <- returned[[2]]
	   
	   to_download$theme_down_relation <- plott
	   
	   output$theme_relation_table_plot <- renderTable({
			 		 table_returned <- theme_sex_table_maker(rel_dim_th)
					 table_returned
			 },sanitize.text.function = function(x) x)
       
     }
     
     if(input$rel_th_type == "age")
     {
       returned <- theme_age_plotter(rel_dim_th, "Relationships", theme, to_plot_theme)
       plott <- returned[[1]]
       texttheme <- returned[[2]]
	   to_download$theme_down_relation <- plott
	   
	   				output$theme_relation_table_plot <- renderTable({
			 		 table_returned <- theme_age_table_maker(rel_dim_th)
					 table_returned
			 },sanitize.text.function = function(x) x)
     }
     
     output$rel_themetype <- renderUI({
       texttheme   
       
     })
     
     
     ggplotly(plott) %>% config(displayModeBar = F)  
     
   })
   
    output$theme_down_relation <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$theme_down_relation)
 dev.off()
 })   
   
 
   output$rel_TreePlot <-  renderCollapsibleTree({
     
     dimension_data <- read.csv("dashboard.csv")
     rel_dim <- subset(dimension_data, Dimension == "Relationships")
     
     tree_graph <- rel_dim %>%
       group_by(Dimension,Theme, Indicator) %>%
       summarize(`Number of Indicators` = n()) %>%
       collapsibleTreeSummary(
         hierarchy = c("Theme", "Indicator"),
         root = "Relationships",
         width = 600,
         height = 700,
         attribute = "Number of Indicators",
         zoomable = FALSE
       )
     tree_graph
     
   })
   
   
   
   output$rel_SelectedPlot_question <- renderPlotly({
     
     
     que_code <- data[,c("support.amount")]
     
     
     if (input$rel_questions == "If you were in trouble, how much support could you count on from friends and family")
     {
	 que_code$newcat <- que_code$support.amount
	   que_code <- que_code[order(que_code$newcat), ]
       
       que_code <- que_code %>% mutate(newcat = case_when(que_code$support.amount == 1 ~ "None", 
                                                          que_code$support.amount == 2 ~ "Very little",
                                                          que_code$support.amount == 3 ~ "Some", 
                                                          que_code$support.amount == 4 ~ "Fair",
                                                          que_code$support.amount == 5 ~ "Full" 
                                                          
       ))
      
	  que_code$newcat <- as.character(que_code$newcat)
	   que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat)) 
       
     }
     
     
     plott <- ggplot(que_code, aes(x=que_code$newcat,  fill=factor(newcat))) +
       geom_bar(position="dodge") + labs(fill= "Response") + xlab("Response to survey questions")+ scale_fill_brewer(name = "Response")+ 	theme(axis.text.x = element_text(size = 12, angle = 25))
     
     to_download$ques_down_relation <- plott
     ggplotly(plott) %>% config(displayModeBar = F)  
     
     
   })
 
     output$ques_down_relation <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$ques_down_relation)
 dev.off()
 })  
   ################################### Clothing start #######################################
   output$SelectedPlot_cloth <- renderPlotly({
     
     
     cloth_wise <- data[, "score9"]
     names(cloth_wise)[names(cloth_wise) == "score9"] <- "dimen"
     
     
     if(input$clothDimensiontype == "default") {
       
       returned <- bar_default_plotter(cloth_wise, "Clothing")
       plott <- returned[[1]]
       textt <- returned[[2]]
	   
	   to_download$down_clothing <- plott
				output$clothes_table_plot <- renderTable({
			 		 table_returned <- default_table_maker(cloth_wise)
					 table_returned
			 })
       
       
     }
     
     
     
     if(input$clothDimensiontype == "sex")
     {
       
       
       returned <- bar_gender_plotter(cloth_wise, "Clothing")
       plott <- returned[[1]]
       textt <- returned[[2]]
	   
	   to_download$down_clothing <- plott
       output$clothes_table_plot <- renderTable({
			 		 table_returned <- default_sex_table_maker(cloth_wise)
					 table_returned
			 },sanitize.text.function = function(x) x)
       
       
       
     }
     
     
     if (input$clothDimensiontype == "age")
     {
       
       returned <- bar_age_plotter(cloth_wise, "Clothing")
       plott <- returned[[1]]
       textt <- returned[[2]]
	   to_download$down_clothing <- plott
	   
	    output$clothes_table_plot <- renderTable({
			 		 table_returned <- default_age_table_maker(cloth_wise)
					 table_returned
			 },sanitize.text.function = function(x) x)
       
     }
     
     output$cloth_htmltxt <- renderUI({
       textt    
       
     })
     
     
     ggplotly(plott) %>% config(displayModeBar = F)
     
     
     
     
   })
   
   
     output$down_clothing <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$down_clothing)
 dev.off()
 })
   
   
   output$cloth_sunburst <- renderPlotly({
     cloth_wise <- data[, "score9"]
     names(cloth_wise)[names(cloth_wise) == "score9"] <- "dimen"
     
     returned <- sunburst_plotter(cloth_wise, "Clothing")
     
     output$cloth_caption <- renderUI({
       
       
       cap <- HTML( "<b>Number of people deprived categorized by gender</b>")
       cap
       
     })
     
     output$cloth_htmltxt1 <- renderUI({
       
       textt <- sunburst_text_stat(cloth_wise, "cloth")
       textt
       
     })
     returned
     
     
   })
   
   output$cloth_SelectedPlot_theme <- renderPlotly({
     
     
     all_dim <- data[, grep("^score\\d+(\\.\\d{1,1})$",colnames(data),perl=TRUE,value = TRUE)]
     
     
     cloth_theme <- all_dim[, grep("score9", colnames(all_dim))]
     
     colnames(cloth_theme) <- var_label(cloth_theme) 
     theme <- input$cloth_inputthemetype
     
     to_plot_theme <- paste("Clothing -", paste(theme))
     
     cloth_dim_th <- cloth_theme %>% mutate(category = case_when(round(cloth_theme[to_plot_theme],0) == 0 ~ "Most deprived", 
                                                             round(cloth_theme[to_plot_theme],0) == 1 ~ "Deprived", 
                                                             round(cloth_theme[to_plot_theme],0) == 2 ~ "Somewhat deprived", 
                                                             round(cloth_theme[to_plot_theme],0) == 3 ~ "Least deprived"))
     
     
     
     if(input$cloth_th_type == "sex")
     {
       
       returned <- theme_sex_plotter(cloth_dim_th, "Clothing", theme, to_plot_theme)
       plott <- returned[[1]]
       texttheme <- returned[[2]]
	   to_download$theme_down_clothing <- plott
	   
	   output$themes_clothes_table_plot <- renderTable({
			 		 table_returned <- theme_sex_table_maker(cloth_dim_th)
					 table_returned
			 },sanitize.text.function = function(x) x)
       
     }
     
     if(input$cloth_th_type == "age")
     {
       returned <- theme_age_plotter(cloth_dim_th, "Clothing", theme, to_plot_theme)
       plott <- returned[[1]]
       texttheme <- returned[[2]]
	   
	   to_download$theme_down_clothing <- plott
	   
	   				output$themes_clothes_table_plot <- renderTable({
			 		 table_returned <- theme_age_table_maker(cloth_dim_th)
					 table_returned
			 },sanitize.text.function = function(x) x)
     }
     
     output$cloth_themetype <- renderUI({
       texttheme   
       
     })
     
     
     ggplotly(plott) %>% config(displayModeBar = F)  
     
   })
   
        output$theme_down_clothing <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$theme_down_clothing)
 dev.off()
 })
   
   
   
   output$cloth_TreePlot <-  renderCollapsibleTree({
     
     dimension_data <- read.csv("dashboard.csv")
     cloth_dim <- subset(dimension_data, Dimension == "Clothing")
     
     tree_graph <- cloth_dim %>%
       group_by(Dimension,Theme, Indicator) %>%
       summarize(`Number of Indicators` = n()) %>%
       collapsibleTreeSummary(
         hierarchy = c("Theme", "Indicator"),
         root = "Clothing",
         width = 600,
         height = 700,
         attribute = "Number of Indicators",
         zoomable = FALSE
       )
     tree_graph
     
   })
   
   
   
   output$cloth_SelectedPlot_question <- renderPlotly({
     
     
     que_code <- data[,c("protect.amount","present.amount")]
     
     
     if (input$cloth_questions == "To what extent does your clothing and footwear protect you from the weather and from hazards in your environment")
     {
	 que_code$newcat <- que_code$protect.amount
	   que_code <- que_code[order(que_code$newcat), ]

	   
       que_code <- que_code %>% mutate(newcat = case_when(que_code$protect.amount == 1 ~ "None", 
                                                          que_code$protect.amount == 2 ~ "Very little",
                                                          que_code$protect.amount == 3 ~ "Some", 
                                                          que_code$protect.amount == 4 ~ "Fair",
                                                          que_code$protect.amount == 5 ~ "Full" 
                                                          
       ))
	 que_code$newcat <- as.character(que_code$newcat)
	   que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat)) 
       
     }
     if (input$cloth_questions == "To what extent are you able to present yourself in public, in terms of clothing, body odour and grooming, in a way that is acceptable by standards of your community")
     {
      
	  que_code$newcat <- que_code$present.amount
	   que_code <- que_code[order(que_code$newcat), ]
	   
       que_code <- que_code %>% mutate(newcat = case_when(que_code$present.amount == 1 ~ "Never", 
                                                          que_code$present.amount == 2 ~ "Rarely",
                                                          que_code$present.amount == 3 ~ "Sometimes", 
                                                          que_code$present.amount == 4 ~ "Often",
                                                          que_code$present.amount == 5 ~ "Always" 
                                                          
       ))

	   que_code$newcat <- as.character(que_code$newcat)
	   que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat)) 
       
     }
     
     
     plott <- ggplot(que_code, aes(x=que_code$newcat,  fill=factor(newcat))) +
       geom_bar(position="dodge") + labs(fill= "Response") + xlab("Response to survey questions")+ scale_fill_brewer(name = "Response")+ 	theme(axis.text.x = element_text(size = 12, angle = 25))
     
     to_download$ques_down_clothing <- plott
     ggplotly(plott) %>% config(displayModeBar = F)  
     
     
   })
 
 
 output$ques_down_clothing <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$ques_down_clothing)
 dev.off()
 })
   
   ################################### TimeUse start #######################################
   output$SelectedPlot_time <- renderPlotly({
     
     
     time_wise <- data[, "score14"]
     names(time_wise)[names(time_wise) == "score14"] <- "dimen"
     
     
     if(input$timeDimensiontype == "default") {
       
       returned <- bar_default_plotter(time_wise, "Time use")
       plott <- returned[[1]]
       textt <- returned[[2]]
	   to_download$down_timeuse <- plott
       		output$timeuse_table_plot <- renderTable({
			 		 table_returned <- default_table_maker(time_wise)
					 table_returned
			 })
       
       
     }
     
     
     
     if(input$timeDimensiontype == "sex")
     {
       
       
       returned <- bar_gender_plotter(time_wise, "Time Use")
       plott <- returned[[1]]
       textt <- returned[[2]]
	   
	   to_download$down_timeuse <- plott
       output$timeuse_table_plot <- renderTable({
			 		 table_returned <- default_sex_table_maker(time_wise)
					 table_returned
			 },sanitize.text.function = function(x) x)
       
       
       
     }
     
     
     if (input$timeDimensiontype == "age")
     {
       
       returned <- bar_age_plotter(time_wise, "Time Use")
       plott <- returned[[1]]
       textt <- returned[[2]]
	   to_download$down_timeuse <- plott
	    output$timeuse_table_plot <- renderTable({
			 		 table_returned <- default_age_table_maker(time_wise)
					 table_returned
			 },sanitize.text.function = function(x) x)
       
     }
     
     output$time_htmltxt <- renderUI({
       textt    
       
     })
     
     
     ggplotly(plott) %>% config(displayModeBar = F)
     
     
     
     
   })

 output$down_timeuse <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$down_timeuse)
 dev.off()
 })

   
   output$time_sunburst <- renderPlotly({
     time_wise <- data[, "score14"]
     names(time_wise)[names(time_wise) == "score14"] <- "dimen"
     
     returned <- sunburst_plotter(time_wise, "Time Use")
     
     output$time_caption <- renderUI({
       
       
       cap <- HTML( "<b>Number of people deprived categorized by gender</b>")
       cap
       
     })
     
     output$time_htmltxt1 <- renderUI({
       
       textt <- sunburst_text_stat(time_wise, "time")
       textt
       
     })
     returned
     
     
   })
   
   output$time_SelectedPlot_theme <- renderPlotly({
     
     
     all_dim <- data[, grep("^score\\d+(\\.\\d{1,1})$",colnames(data),perl=TRUE,value = TRUE)]
     
     
     time_theme <- all_dim[, grep("score14", colnames(all_dim))]
     
     colnames(time_theme) <- var_label(time_theme) 
     theme <- input$time_inputthemetype
     
     to_plot_theme <- paste("Time use -", paste(theme))
     
     time_dim_th <- time_theme %>% mutate(category = case_when(round(time_theme[to_plot_theme],0) == 0 ~ "Most deprived", 
                                                                 round(time_theme[to_plot_theme],0) == 1 ~ "Deprived", 
                                                                 round(time_theme[to_plot_theme],0) == 2 ~ "Somewhat deprived", 
                                                                 round(time_theme[to_plot_theme],0) == 3 ~ "Least deprived"))
     
     
     
     if(input$time_th_type == "sex")
     {
       
       returned <- theme_sex_plotter(time_dim_th, "Time Use", theme, to_plot_theme)
       plott <- returned[[1]]
       texttheme <- returned[[2]]
	   to_download$theme_down_timeuse <-plott
	   
	   output$theme_timeuse_table_plot <- renderTable({
			 		 table_returned <- theme_sex_table_maker(time_dim_th)
					 table_returned
			 },sanitize.text.function = function(x) x)
       
     }
     
     if(input$time_th_type == "age")
     {
       returned <- theme_age_plotter(time_dim_th, "Time Use", theme, to_plot_theme)
       plott <- returned[[1]]
       texttheme <- returned[[2]]
	   
	   to_download$theme_down_timeuse <-plott
	   
	   				output$theme_timeuse_table_plot <- renderTable({
			 		 table_returned <- theme_age_table_maker(time_dim_th)
					 table_returned
			 },sanitize.text.function = function(x) x)
     }
     
     output$time_themetype <- renderUI({
       texttheme   
       
     })
     
     
     ggplotly(plott) %>% config(displayModeBar = F)  
     
   })
 
  output$theme_down_timeuse <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$theme_down_timeuse)
 dev.off()
 })
   
   
   output$time_TreePlot <-  renderCollapsibleTree({
     
     dimension_data <- read.csv("dashboard.csv")
     time_dim <- subset(dimension_data, Dimension == "Time Use")
     
     tree_graph <- time_dim %>%
       group_by(Dimension,Theme, Indicator) %>%
       summarize(`Number of Indicators` = n()) %>%
       collapsibleTreeSummary(
         hierarchy = c("Theme", "Indicator"),
         root = "Time Use",
         width = 600,
         height = 700,
         attribute = "Number of Indicators",
         zoomable = FALSE
       )
     tree_graph
     
   })
   
   
   
   output$time_SelectedPlot_question <- renderPlotly({
     
     
     que_code <- data[,c("j.s.time")]
     
     
     if (input$time_questions == "Record how much (primary) time was spent on personal care, leisure, entertainment and religious activities")
     {
     
		que_code$newcat <- que_code$j.s.time
	   que_code <- que_code[order(que_code$newcat), ]	 
	 
       que_code <- que_code %>% mutate(newcat = case_when(que_code$j.s.time == 9 ~ "9", 
                                                          que_code$j.s.time == 8.3 ~ "8.3",
                                                          que_code$j.s.time == 6 ~ "6", 
                                                          que_code$j.s.time == 5 ~ "5",
                                                          que_code$j.s.time == 12 ~ "12",
                                                          que_code$j.s.time == 9.2 ~ "9.2", 
                                                          que_code$j.s.time == 6.3 ~ "6.3",
                                                          que_code$j.s.time == 4 ~ "4", 
                                                          que_code$j.s.time == 11 ~ "11",
                                                          que_code$j.s.time == 7.3 ~ "7.3"
                                                          
       ))
       
	   que_code$newcat <- as.character(que_code$newcat)
	   que_code$newcat <- factor(que_code$newcat, levels=unique(que_code$newcat)) 
       
     }
    
     
     plott <- ggplot(que_code, aes(x=que_code$newcat,  fill=factor(newcat))) +
       geom_bar(position="dodge") + labs(fill= "Response") + xlab("Response to survey questions")+ scale_fill_brewer(name = "Response")+ 	theme(axis.text.x = element_text(size = 12, angle = 25))
     
     to_download$ques_down_timeuse <- plott
     ggplotly(plott) %>% config(displayModeBar = F)  
     
     
   })
   
 
 output$ques_down_timeuse <- downloadHandler(
 filename = function(){
 paste("plot", "eps", sep = ".")
 },
 content = function(file){
 png(file, width = 1000, height = 700)
 ## plot
 print(to_download$ques_down_timeuse)
 dev.off()
 })
 
 
   output$corr_plot<- renderPlotly({
   
   
   data_f	<- data[, grep("^score\\d+$",colnames(data),perl=TRUE,value = TRUE)]
   data_f['score15'][is.na(data_f['score15'])] = mean(data_f['score15'], na.rm=TRUE)
   colnames(data_f)<-var_label(data[, grep("^score\\d+$",colnames(data),perl=TRUE,value = TRUE)])
   corr <- round(cor(data_f), 1)
   plt<-ggcorrplot(corr, hc.order = TRUE,
					 colors = c("coral", "white", "#906968"),
					lab = TRUE) #+ scale_fill_distiller(palette = "Blues")
 
 
        ggplotly(plt) %>% config(displayModeBar = F)  

   
   
   })


##### observers

  observeEvent(input$plt_btn,{
    hide("DashSelectedPlot")
    show("SelectedPlot")
})

  observeEvent(input$tbl_btn,{
    show("DashSelectedPlot")
    hide("SelectedPlot")
})

  observeEvent(input$catogerytype,{
    hide("DashSelectedPlot")
    show("SelectedPlot")
})		
  
  
 ############## food ###############
   observeEvent(input$plt_btn_1,{
    hide("food_table_plot")
    show("SelectedPlot_food")
})

  observeEvent(input$tbl_btn_1,{
    show("food_table_plot")
    hide("SelectedPlot_food")
})

  observeEvent(input$Dimensiontype,{
    hide("food_table_plot")
    show("SelectedPlot_food")
})		
  
  
  ############## Water ###############
   observeEvent(input$plt_btn_2,{
    hide("water_table_plot")
    show("SelectedPlot_water")
})

  observeEvent(input$tbl_btn_2,{
    show("water_table_plot")
    hide("SelectedPlot_water")
})

  observeEvent(input$waterDimensiontype,{
    hide("water_table_plot")
    show("SelectedPlot_water")
})		
  
   ############## work ###############
   observeEvent(input$plt_btn_3,{
    hide("work_table_plot")
    show("SelectedPlot_work")
})

  observeEvent(input$tbl_btn_3,{
    show("work_table_plot")
    hide("SelectedPlot_work")
})

  observeEvent(input$workDimensiontype,{
    hide("work_table_plot")
    show("SelectedPlot_work")
})	
  
     ############## Voice ###############
   observeEvent(input$plt_btn_4,{
    hide("voice_table_plot")
    show("SelectedPlot_voice")
})

  observeEvent(input$tbl_btn_4,{
    show("voice_table_plot")
    hide("SelectedPlot_voice")
})

  observeEvent(input$voiceDimensiontype,{
    hide("voice_table_plot")
    show("SelectedPlot_voice")
})	
  
      ############## Environment ###############
   observeEvent(input$plt_btn_5,{
    hide("env_table_plot")
    show("SelectedPlot_env")
})

  observeEvent(input$tbl_btn_5,{
    show("env_table_plot")
    hide("SelectedPlot_env")
})

  observeEvent(input$envDimensiontype,{
    hide("env_table_plot")
    show("SelectedPlot_env")
})	

      ############## Shelter ###############
   observeEvent(input$plt_btn_6,{
    hide("shelter_table_plot")
    show("SelectedPlot_shelter")
})

  observeEvent(input$tbl_btn_6,{
    show("shelter_table_plot")
    hide("SelectedPlot_shelter")
})

  observeEvent(input$shelterDimensiontype,{
    hide("shelter_table_plot")
    show("SelectedPlot_shelter")
})	
  
      ############## Health ###############
   observeEvent(input$plt_btn_7,{
    hide("health_table_plot")
    show("SelectedPlot_health")
})

  observeEvent(input$tbl_btn_7,{
    show("health_table_plot")
    hide("SelectedPlot_health")
})

  observeEvent(input$healthDimensiontype,{
    hide("health_table_plot")
    show("SelectedPlot_health")
})	
  

      ############## edu ###############
   observeEvent(input$plt_btn_8,{
    hide("edu_table_plot")
    show("SelectedPlot_edu")
})

  observeEvent(input$tbl_btn_8,{
    show("edu_table_plot")
    hide("SelectedPlot_edu")
})

  observeEvent(input$eduDimensiontype,{
    hide("edu_table_plot")
    show("SelectedPlot_edu")
})	
 
      ############## energy ###############
   observeEvent(input$plt_btn_9,{
    hide("energy_table_plot")
    show("SelectedPlot_energy")
})

  observeEvent(input$tbl_btn_9,{
    show("energy_table_plot")
    hide("SelectedPlot_energy")
})

  observeEvent(input$energyDimensiontype,{
    hide("energy_table_plot")
    show("SelectedPlot_energy")
})	

      ############## sanitation ###############
   observeEvent(input$plt_btn_10,{
    hide("sanitation_table_plot")
    show("SelectedPlot_san")
})

  observeEvent(input$tbl_btn_10,{
    show("sanitation_table_plot")
    hide("SelectedPlot_san")
})

  observeEvent(input$sanDimensiontype,{
    hide("sanitation_table_plot")
    show("SelectedPlot_san")
})	 

      ############## relation ###############
   observeEvent(input$plt_btn_11,{
    hide("relation_table_plot")
    show("SelectedPlot_rel")
})

  observeEvent(input$tbl_btn_11,{
    show("relation_table_plot")
    hide("SelectedPlot_rel")
})

  observeEvent(input$relDimensiontype,{
    hide("relation_table_plot")
    show("SelectedPlot_rel")
})	

      ############## Clothing ###############
   observeEvent(input$plt_btn_12,{
    hide("clothes_table_plot")
    show("SelectedPlot_cloth")
})

  observeEvent(input$tbl_btn_12,{
    show("clothes_table_plot")
    hide("SelectedPlot_cloth")
})

  observeEvent(input$clothDimensiontype,{
    hide("clothes_table_plot")
    show("SelectedPlot_cloth")
})	

      ############## timeuse ###############
   observeEvent(input$plt_btn_13,{
    hide("timeuse_table_plot")
    show("SelectedPlot_time")
})

  observeEvent(input$tbl_btn_13,{
    show("timeuse_table_plot")
    hide("SelectedPlot_time")
})

  observeEvent(input$timeDimensiontype,{
    hide("timeuse_table_plot")
    show("SelectedPlot_time")
})	
########### observers for theme ###############

      ############## food ###############
   observeEvent(input$plt_btn_t1,{
    hide("theme_food_table_plot")
    show("SelectedPlot_theme")
})

  observeEvent(input$tbl_btn_t1,{
    show("theme_food_table_plot")
    hide("SelectedPlot_theme")
})

  observeEvent(input$themetype,{
    hide("theme_food_table_plot")
    show("SelectedPlot_theme")
})	
      ############## water ###############
   observeEvent(input$plt_btn_t2,{
    hide("theme_water_table_plot")
    show("w_SelectedPlot_theme")
})

  observeEvent(input$tbl_btn_t2,{
    show("theme_water_table_plot")
    hide("w_SelectedPlot_theme")
})

  observeEvent(input$water_th_type,{
    hide("theme_water_table_plot")
    show("w_SelectedPlot_theme")
})	

      ############## work ###############
   observeEvent(input$plt_btn_t3,{
    hide("theme_work_table_plot")
    show("work_SelectedPlot_theme")
})

  observeEvent(input$tbl_btn_t3,{
    show("theme_work_table_plot")
    hide("work_SelectedPlot_theme")
})

  observeEvent(input$work_th_type,{
    hide("theme_work_table_plot")
    show("work_SelectedPlot_theme")
})	

      ############## voice ###############
   observeEvent(input$plt_btn_t4,{
    hide("theme_voice_table_plot")
    show("voice_SelectedPlot_theme")
})

  observeEvent(input$tbl_btn_t4,{
    show("theme_voice_table_plot")
    hide("voice_SelectedPlot_theme")
})

  observeEvent(input$voice_th_type,{
    hide("theme_voice_table_plot")
    show("voice_SelectedPlot_theme")
})	

     ############## Environment ###############
   observeEvent(input$plt_btn_t5,{
    hide("theme_env_table_plot")
    show("env_SelectedPlot_theme")
})

  observeEvent(input$tbl_btn_t5,{
    show("theme_env_table_plot")
    hide("env_SelectedPlot_theme")
})

  observeEvent(input$env_th_type,{
    hide("theme_env_table_plot")
    show("env_SelectedPlot_theme")
})	

      ############## Shelter ###############
   observeEvent(input$plt_btn_t6,{
    hide("theme_shelter_table_plot")
    show("shelter_SelectedPlot_theme")
})

  observeEvent(input$tbl_btn_t6,{
    show("theme_shelter_table_plot")
    hide("shelter_SelectedPlot_theme")
})

  observeEvent(input$shelter_th_type,{
    hide("theme_shelter_table_plot")
    show("shelter_SelectedPlot_theme")
})	
  
      ############## Health ###############
   observeEvent(input$plt_btn_t7,{
    hide("theme_health_table_plot")
    show("health_SelectedPlot_theme")
})

  observeEvent(input$tbl_btn_t7,{
    show("theme_health_table_plot")
    hide("health_SelectedPlot_theme")
})

  observeEvent(input$health_th_type,{
    hide("theme_health_table_plot")
    show("health_SelectedPlot_theme")
})	
  

      ############## edu ###############
   observeEvent(input$plt_btn_t8,{
    hide("theme_edu_table_plot")
    show("edu_SelectedPlot_theme")
})

  observeEvent(input$tbl_btn_t8,{
    show("theme_edu_table_plot")
    hide("edu_SelectedPlot_theme")
})

  observeEvent(input$edu_th_type,{
    hide("theme_edu_table_plot")
    show("edu_SelectedPlot_theme")
})	
 
      ############## energy ###############
   observeEvent(input$plt_btn_t9,{
    hide("theme_energy_table_plot")
    show("energy_SelectedPlot_theme")
})

  observeEvent(input$tbl_btn_t9,{
    show("theme_energy_table_plot")
    hide("energy_SelectedPlot_theme")
})

  observeEvent(input$energy_th_type,{
    hide("theme_energy_table_plot")
    show("energy_SelectedPlot_theme")
})	

      ############## sanitation ###############
   observeEvent(input$plt_btn_t10,{
    hide("theme_sanitation_table_plot")
    show("san_SelectedPlot_theme")
})

  observeEvent(input$tbl_btn_t10,{
    show("theme_sanitation_table_plot")
    hide("san_SelectedPlot_theme")
})

  observeEvent(input$san_th_type,{
    hide("theme_sanitation_table_plot")
    show("san_SelectedPlot_theme")
})	 

      ############## relation ###############
   observeEvent(input$plt_btn_t11,{
    hide("theme_relation_table_plot")
    show("rel_SelectedPlot_theme")
})

  observeEvent(input$tbl_btn_t11,{
    show("theme_relation_table_plot")
    hide("rel_SelectedPlot_theme")
})

  observeEvent(input$rel_th_type,{
    hide("theme_relation_table_plot")
    show("rel_SelectedPlot_theme")
})	

      ############## Clothing ###############
   observeEvent(input$plt_btn_t12,{
    hide("themes_clothes_table_plot")
    show("cloth_SelectedPlot_theme")
})

  observeEvent(input$tbl_btn_t12,{
    show("themes_clothes_table_plot")
    hide("cloth_SelectedPlot_theme")
})

  observeEvent(input$cloth_th_type,{
    hide("themes_clothes_table_plot")
    show("cloth_SelectedPlot_theme")
})	

      ############## timeuse ###############
   observeEvent(input$plt_btn_t13,{
    hide("theme_timeuse_table_plot")
    show("time_SelectedPlot_theme")
})

  observeEvent(input$tbl_btn_t13,{
    show("theme_timeuse_table_plot")
    hide("time_SelectedPlot_theme")
})

  observeEvent(input$time_th_type,{
    hide("theme_timeuse_table_plot")
    show("time_SelectedPlot_theme")
})	



}


