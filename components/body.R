###################
# body.R
# 
# Create the body for the ui. 
# If you had multiple tabs, you could split those into their own
# components as well.
###################
body <-   ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard", 
	  		  fluidRow(
			  column(width = 8,
			  box(width = NULL, solidheader = TRUE,
              h1("Overall Statistics"),
			  selectInput(inputId = "catogerytype", label = "category: ",
                  choices = c("default" , "age", "sex"),
                  selected = "default"),
				  #radioButtons("visuBtn", NULL, choices = c(Plot = "Plot", Table = "Table")),
				    actionButton("plt_btn", "Plot"),
					actionButton("tbl_btn", "Table"),
				  useShinyjs(), 
				  downloadButton(outputId = "down", label = "Download the plot", class = "butt"),
				  tags$head(tags$style(" .butt{color: #3399cc;}")), 

				  plotlyOutput(outputId = "SelectedPlot", height = 450),
				  tableOutput('DashSelectedPlot'),
				  h4("Inter Class Correlation Scores"),
				  tableOutput('ICCtable')
				  )),
			
			  column(width = 4, 
			  box(width = NULL, solidheader = TRUE,
					h4("Tree plot"),
				   collapsibleTreeOutput("TreePlot", height = "600px"),
				   #h4("Table"),
				   actionButton("perc", "Percentage"),
				  actionButton("cnt", "Count"),
				  tableOutput('Ctable')	)),
			
      )),
      
      # Food tab content
      tabItem(tabName = "Food",
              h1("Food"),
			  fluidRow(
			  column(width = 6,
			  box(width = NULL, solidheader = TRUE,
			  uiOutput("htmltxt"),
			  selectInput(inputId = "Dimensiontype", label = "category: ",
                  choices = c("default" , "age", "sex"),
                  selected = "default"),
				actionButton("plt_btn_1", "Plot"),
			 	actionButton("tbl_btn_1", "Table"),
				useShinyjs(), 
				downloadButton(outputId = "down_food", label = "Download the plot", class = "butt"),
				tags$head(tags$style(" .butt{color: #3399cc;}")),
				plotlyOutput(outputId = "SelectedPlot_food", height = 450),
				tableOutput('food_table_plot'),
				br(),br(),
				column(width = 6,
				box(width = NULL, solidheader = TRUE,
				plotlyOutput(outputId = "sunburst", height = 350, width = 350),
				uiOutput("caption"),
				)),uiOutput("htmltxt1"),
			  
				)
			),
			column(width=6,
			 box(width = NULL, solidheader = TRUE,
				uiOutput("themetext"),
			 	selectInput(inputId = "themetype", label = "category: ",
                choices = c("age", "sex"),
                selected = "sex"),
				actionButton("plt_btn_t1", "Plot"),
			 	actionButton("tbl_btn_t1", "Table"),
				useShinyjs(), 
				 downloadButton(outputId = "theme_down_food", label = "Download the plot", class = "butt"),
				 tags$head(tags$style(" .butt{color: #3399cc;}")),
				plotlyOutput(outputId = "SelectedPlot_theme", height = 450),
				tableOutput('theme_food_table_plot'),
				selectInput(inputId = "food_questions", label = "Questions: ",
				choices = c("In the past 4 weeks, was there ever no food for you to eat because of a lack of resources to get food",
				 "In the past 4 weeks, did you go to sleep at night hungry because there was not enough food",
				 "In the past 4 weeks, did you go a whole day and night without eating because there was not enough food",
				 "In the past 12 months, was there ever no food for you to eat because of a lack of resources to get food"),
                selected = "In the past 4 weeks, was there ever no food for you to eat because of a lack of resources to get food"),
				useShinyjs(), 
				 downloadButton(outputId = "ques_down_food", label = "Download the plot", class = "butt"),
				 tags$head(tags$style(" .butt{color: #3399cc;}")),
				plotlyOutput(outputId = "SelectedPlot_question", height = 450),
				h4("Dimensional tree map"),
				collapsibleTreeOutput("foodTreePlot", height = "600px")),

			 ),
			
			
		)
	 ),
	 
      # Water tab content
      tabItem(tabName = "Water",
              h1("Water"),
			  fluidRow(
			  			  column(width = 6,
			  box(height = 1270 ,width = NULL, solidheader = TRUE,
			  uiOutput("whtmltxt"),
			  selectInput(inputId = "waterDimensiontype", label = "category: ",
                  choices = c("default" , "age", "sex"),
                  selected = "default"),
				  actionButton("plt_btn_2", "Plot"),
			actionButton("tbl_btn_2", "Table"),
			useShinyjs(), 
				downloadButton(outputId = "down_water", label = "Download the plot", class = "butt"),
				tags$head(tags$style(" .butt{color: #3399cc;}")),
				plotlyOutput(outputId = "SelectedPlot_water", height = 550),
				tableOutput('water_table_plot'),
				br(),br(),
				column(width = 6,
				box(width = NULL, solidheader = TRUE,
				plotlyOutput(outputId = "wsunburst", height = 450, width = 350),
				uiOutput("wcaption"),
				)),uiOutput("whtmltxt1"),
			  
				)
			),
			column(width=6,
			 box(width = NULL, solidheader = TRUE,
				uiOutput("wthemetype"),
			 	selectInput(inputId = "inputthemetype", label = "Themes: ",
                choices = c("Quality", "Accessibility", "Sufficiency"),
                selected = "Quality"),
				selectInput(inputId = "water_th_type", label = "category: ",
                  choices = c("age", "sex"),
                  selected = "age"),
				actionButton("plt_btn_t2", "Plot"),
				actionButton("tbl_btn_t2", "Table"),
				useShinyjs(), 
				downloadButton(outputId = "theme_down_water", label = "Download the plot", class = "butt"),
				tags$head(tags$style(" .butt{color: #3399cc;}")),
				plotlyOutput(outputId = "w_SelectedPlot_theme", height = 450),
				tableOutput('theme_water_table_plot'),
				selectInput(inputId = "water_questions", label = "Questions: ",
                  choices = c("How often do you have enough water to meet all your personal needs", 
							  "What is the main source of drinking water for members of your household",
							  "Do you treat your water in any way to make it safer to drink",
							  "What do you usually do to the water to make it safer"),
                  selected = "How often do you have enough water to meet all your personal needs"),
				 downloadButton(outputId = "ques_down_water", label = "Download the plot", class = "butt"),
				tags$head(tags$style(" .butt{color: #3399cc;}")),
				  plotlyOutput(outputId = "W_SelectedPlot_question", height = 450),
				  h4("Dimensional tree map"),
				collapsibleTreeOutput("WaterTreePlot", height = "600px")),
			 ),
			
			
		)
	 ),
	 
	 
	       # Work tab content
      tabItem(tabName = "Work",
              h1("Work"),
			  fluidRow(
			  			  column(width = 6,
			  box(height = 1270 ,width = NULL, solidheader = TRUE,
			  uiOutput("work_htmltxt"),
			  selectInput(inputId = "workDimensiontype", label = "category: ",
                  choices = c("default" , "age", "sex"),
                  selected = "default"),
				  actionButton("plt_btn_3", "Plot"),
				actionButton("tbl_btn_3", "Table"),
				useShinyjs(), 
				downloadButton(outputId = "down_work", label = "Download the plot", class = "butt"),
				tags$head(tags$style(" .butt{color: #3399cc;}")),
				plotlyOutput(outputId = "SelectedPlot_work", height = 550),
				tableOutput('work_table_plot'),
				br(),br(),
				column(width = 6,
				box(width = NULL, solidheader = TRUE,
				plotlyOutput(outputId = "work_sunburst", height = 450, width = 350),
				uiOutput("work_caption"),
				)),uiOutput("work_htmltxt1"),
			  
				)
			),
			column(width=6,
			 box(width = NULL, solidheader = TRUE,
				uiOutput("work_themetype"),
			 	selectInput(inputId = "work_inputthemetype", label = "Themes: ",
                choices = c("Paid work", "Unpaid work"),
                selected = "Paid work"),
				selectInput(inputId = "work_th_type", label = "category: ",
                  choices = c("age", "sex"),
                  selected = "age"),
				actionButton("plt_btn_t3", "Plot"),
				actionButton("tbl_btn_t3", "Table"),
				useShinyjs(), 
				downloadButton(outputId = "theme_down_work", label = "Download the plot", class = "butt"),
				tags$head(tags$style(" .butt{color: #3399cc;}")),
				plotlyOutput(outputId = "work_SelectedPlot_theme", height = 450),
				tableOutput('theme_work_table_plot'),

				selectInput(inputId = "work_questions", label = "Questions: ",
                  choices = c("Did you do any work for money during the last 6 months", 
							  "What is the main kind of work that you regularly do",
							  "Have you suffered any injury/illness from your paid work in the last 12 months",
							  "What effect did this injury/illness or other harm have on you",
							  "Members of my community respect the paid work that I do",
							  "I am treated with respect when I do paid work",
							  "Do you regularly do unpaid work",
							  "Have you suffered any injury/illness from your unpaid work in the last 12 months",
							  "What effect did this injury/illness have on you"),
                  selected = "Did you do any work for money during the last 6 months"),
				downloadButton(outputId = "ques_down_work", label = "Download the plot", class = "butt"),
				tags$head(tags$style(" .butt{color: #3399cc;}")),
				  plotlyOutput(outputId = "Work_SelectedPlot_question", height = 450),
				  h4("Dimensional tree map"),
				collapsibleTreeOutput("WorkTreePlot", height = "600px")),
			 ),
			
			
		)
	 ),
	 # Voice tab content
	    tabItem(tabName = "Voice",
              h1("Voice"),
			  fluidRow(
			  			  column(width = 6,
			  box(height = 1270 ,width = NULL, solidheader = TRUE,
			  uiOutput("voice_htmltxt"),
			  selectInput(inputId = "voiceDimensiontype", label = "category: ",
                  choices = c("default" , "age", "sex"),
                  selected = "default"),
				  actionButton("plt_btn_4", "Plot"),
				actionButton("tbl_btn_4", "Table"),
				useShinyjs(), 
				downloadButton(outputId = "down_voice", label = "Download the plot", class = "butt"),
				tags$head(tags$style(" .butt{color: #3399cc;}")),
				plotlyOutput(outputId = "SelectedPlot_voice", height = 550),
				tableOutput('voice_table_plot'),
				br(),br(),
				column(width = 6,
				box(width = NULL, solidheader = TRUE,
				plotlyOutput(outputId = "voice_sunburst", height = 450, width = 350),
				uiOutput("voice_caption"),
				)),uiOutput("voice_htmltxt1"),
			  
				)
			),
			column(width=6,
			 box(width = NULL, solidheader = TRUE,
				uiOutput("voice_themetype"),
			 	selectInput(inputId = "voice_inputthemetype", label = "Themes: ",
                choices = c("In the household", "In the community"),
                selected = "In the home"),
				selectInput(inputId = "voice_th_type", label = "category: ",
                  choices = c("age", "sex"),
                  selected = "age"),
				actionButton("plt_btn_t4", "Plot"),
				actionButton("tbl_btn_t4", "Table"),
				useShinyjs(), 
				downloadButton(outputId = "theme_down_voice", label = "Download the plot", class = "butt"),
				tags$head(tags$style(" .butt{color: #3399cc;}")),
				plotlyOutput(outputId = "voice_SelectedPlot_theme", height = 450),
				tableOutput('theme_voice_table_plot'),
				selectInput(inputId = "voice_questions", label = "Questions: ",
                  choices = c("To what extent are you able to raise issues in your community that you feel strongly about", 
							  "To what extent do you think people like you can change things in their community",
							  "In general, how much control do you have over personal decisions that have a major impact on your life"),
                  selected = "To what extent are you able to raise issues in your community that you feel strongly about"),
				useShinyjs(), 
				downloadButton(outputId = "ques_down_voice", label = "Download the plot", class = "butt"),
				tags$head(tags$style(" .butt{color: #3399cc;}")),				 
				 plotlyOutput(outputId = "Voice_SelectedPlot_question", height = 450),
				 h4("Dimensional tree map"),
				collapsibleTreeOutput("VoiceTreePlot", height = "600px")),
			 ),
			
			
		)
	 ),
	 
	 
	 
	 	       # Environment tab content
      tabItem(tabName = "Environment",
              h1("Environment"),
			  fluidRow(
			  			  column(width = 6,
			  box(height = 1270 ,width = NULL, solidheader = TRUE,
			  uiOutput("env_htmltxt"),
			  selectInput(inputId = "envDimensiontype", label = "category: ",
                  choices = c("default" , "age", "sex"),
                  selected = "default"),
				  actionButton("plt_btn_5", "Plot"),
				actionButton("tbl_btn_5", "Table"),
				useShinyjs(), 
				downloadButton(outputId = "down_env", label = "Download the plot", class = "butt"),
				tags$head(tags$style(" .butt{color: #3399cc;}")),
				plotlyOutput(outputId = "SelectedPlot_env", height = 550),
				tableOutput('env_table_plot'),
				br(),br(),
				column(width = 6,
				box(width = NULL, solidheader = TRUE,
				plotlyOutput(outputId = "env_sunburst", height = 450, width = 350),
				uiOutput("env_caption"),
				)),uiOutput("env_htmltxt1"),
			  
				)
			),
			column(width=6,
			 box(width = NULL, solidheader = TRUE,
				uiOutput("env_themetype"),
			 	selectInput(inputId = "env_inputthemetype", label = "Themes: ",
                choices = c("Hazards"),
                selected = "Hazards"),
				selectInput(inputId = "env_th_type", label = "category: ",
                  choices = c("age", "sex"),
                  selected = "age"),
				actionButton("plt_btn_t5", "Plot"),
				actionButton("tbl_btn_t5", "Table"),
				useShinyjs(), 
				downloadButton(outputId = "theme_down_env", label = "Download the plot", class = "butt"),
				tags$head(tags$style(" .butt{color: #3399cc;}")),
				plotlyOutput(outputId = "env_SelectedPlot_theme", height = 450),
				tableOutput('theme_env_table_plot'),
				selectInput(inputId = "env_questions", label = "Questions: ",
                  choices = c("Large amounts of rubbish", 
							  "Open sewage",
							  "Air pollution",
							  "Pools of water",
							  "Stores of unsecured waste",
							  "Heavy vehicle traffic",
							  "High levels of noise",
							  "Any other significant hazard"),
                  selected = "Large amounts of rubbish"),
				  useShinyjs(), 
				downloadButton(outputId = "ques_down_env", label = "Download the plot", class = "butt"),
				tags$head(tags$style(" .butt{color: #3399cc;}")),
				  plotlyOutput(outputId = "env_SelectedPlot_question", height = 450),
				  h4("Dimensional tree map"),
				collapsibleTreeOutput("envTreePlot", height = "600px")),
			 ),
			
			
			
		)
	 ),
	 

# Shelter tab content
		tabItem(tabName = "Shelter",
		        h1("Shelter"),
		        fluidRow(
		          column(width = 6,
		                 box(height = 1270 ,width = NULL, solidheader = TRUE,
		                     uiOutput("shelter_htmltxt"),
		                     selectInput(inputId = "shelterDimensiontype", label = "category: ",
		                                 choices = c("default" , "age", "sex"),
		                                 selected = "default"),
							actionButton("plt_btn_6", "Plot"),
							actionButton("tbl_btn_6", "Table"),
							useShinyjs(), 
							downloadButton(outputId = "down_shelter", label = "Download the plot", class = "butt"),
							tags$head(tags$style(" .butt{color: #3399cc;}")),
		                     plotlyOutput(outputId = "SelectedPlot_shelter", height = 550),
							 tableOutput('shelter_table_plot'),
							br(),br(),br(),br(),			
		                     column(width = 6,
		                            box(width = NULL, solidheader = TRUE,
		                                plotlyOutput(outputId = "shelter_sunburst", height = 450, width = 350),
		                                uiOutput("shelter_caption")
		                            )),uiOutput("shelter_htmltxt1")
		                     
		                 )
		          ),
		          column(width=6,
		                 box(width = NULL, solidheader = TRUE,
		                     uiOutput("shelter_themetype"),
		                     selectInput(inputId = "shelter_inputthemetype", label = "Themes: ",
		                                 choices = c("Homelessness", "Habitability"),
		                                 selected = "Homelessness"),
		                     selectInput(inputId = "shelter_th_type", label = "category: ",
		                                 choices = c("age", "sex"),
		                                 selected = "age"),
							actionButton("plt_btn_t6", "Plot"),
							actionButton("tbl_btn_t6", "Table"),
							useShinyjs(), 
							downloadButton(outputId = "theme_down_shelter", label = "Download the plot", class = "butt"),
							tags$head(tags$style(" .butt{color: #3399cc;}")),
		                     plotlyOutput(outputId = "shelter_SelectedPlot_theme", height = 450),
							tableOutput('theme_shelter_table_plot'),
		                     selectInput(inputId = "shelter_questions", label = "Questions: ",
		                                 choices = c("Roofing Material", 
		                                             "Flooring Material",
		                                             "Exterior wall Material",
		                                             "Overall condition of the dwellings",
		                                             "In the last year, did you ever sleep outdoors, in public places, or in temporary shelters, because you did not have access to suitable shelter of your own?"),
		                                 selected = "Roofing Material"),
							useShinyjs(), 
							downloadButton(outputId = "ques_down_shelter", label = "Download the plot", class = "butt"),
							tags$head(tags$style(" .butt{color: #3399cc;}")),
		                     plotlyOutput(outputId = "shelter_SelectedPlot_question", height = 450),
							 h4("Dimensional tree map"),
		                     collapsibleTreeOutput("shelter_TreePlot", height = "600px"))
		          )
		          
		          
		        )
		),
		
		# Health tab content
		tabItem(tabName = "Health",
		        h1("Health"),
		        fluidRow(
		          column(width = 6,
		                 box(height = 1270 ,width = NULL, solidheader = TRUE,
		                     uiOutput("health_htmltxt"),
		                     selectInput(inputId = "healthDimensiontype", label = "category: ",
		                                 choices = c("default" , "age", "sex"),
		                                 selected = "default"),
							actionButton("plt_btn_7", "Plot"),
							actionButton("tbl_btn_7", "Table"),
							useShinyjs(), 
							downloadButton(outputId = "down_health", label = "Download the plot", class = "butt"),
							tags$head(tags$style(" .butt{color: #3399cc;}")),
		                     plotlyOutput(outputId = "SelectedPlot_health", height = 550),
							 tableOutput('health_table_plot'),
							br(),br(),
		                     column(width = 6,
		                            box(width = NULL, solidheader = TRUE,
		                                plotlyOutput(outputId = "health_sunburst", height = 450, width = 350),
		                                uiOutput("health_caption")
		                            )),uiOutput("health_htmltxt1")
		                     
		                 )
		          ),
		          column(width=6,
		                 box(width = NULL, solidheader = TRUE,
		                     uiOutput("health_themetype"),
		                     selectInput(inputId = "health_inputthemetype", label = "Themes: ",
		                                 choices = c("Health care", "Health status"),
		                                 selected = "Healthcare"),
		                     selectInput(inputId = "health_th_type", label = "category: ",
		                                 choices = c("age", "sex"),
		                                 selected = "age"),
		                     actionButton("plt_btn_t7", "Plot"),
							actionButton("tbl_btn_t7", "Table"),
							useShinyjs(), 
							downloadButton(outputId = "theme_down_health", label = "Download the plot", class = "butt"),
							tags$head(tags$style(" .butt{color: #3399cc;}")),
		                     plotlyOutput(outputId = "health_SelectedPlot_theme", height = 450),
							 tableOutput('theme_health_table_plot'),

		                     selectInput(inputId = "health_questions", label = "Questions: ",
		                                 choices = c("When was the last time you had a significant illness or injury", 
		                                             "Did this illness/injury make it impossible or very difficult to perform your usual paid or unpaid activity",
		                                             "How long was it difficult or impossible for you to perform your usual paid or unpaid activity because of this illness/injury",
		                                             "Do you experience any health problems from exposure to the smoke and fumes from your cooking and/or heating fuel",
		                                             "How would you rate these problems",
		                                             "The last time you had an illness/injury that needed healthcare, did you receive this care",
		                                             "skill of the healthcare provider",
		                                             "cleanliness of the treatment facility",
		                                             "availability of prescribed drugs",
		                                             "level of respect with which you were treated",
		                                             "waiting time to receive treatment",
		                                             "the location of the health-care provider"
		                                             ),
		                                 selected = "When was the last time you had a significant illness or injury"),
		                     useShinyjs(), 
							downloadButton(outputId = "ques_down_health", label = "Download the plot", class = "butt"),
							tags$head(tags$style(" .butt{color: #3399cc;}")),
							 plotlyOutput(outputId = "health_SelectedPlot_question", height = 450),
							 h4("Dimensional tree map"),
		                     collapsibleTreeOutput("health_TreePlot", height = "600px"))
		          )
		          
		          
		        )
		),
		
		# Education tab content
		tabItem(tabName = "Education",
		        h1("Education"),
		        fluidRow(
		          column(width = 6,
		                 box(height = 1270 ,width = NULL, solidheader = TRUE,
		                     uiOutput("edu_htmltxt"),
		                     selectInput(inputId = "eduDimensiontype", label = "category: ",
		                                 choices = c("default" , "age", "sex"),
		                                 selected = "default"),
							actionButton("plt_btn_8", "Plot"),
							actionButton("tbl_btn_8", "Table"),	
							useShinyjs(), 
							downloadButton(outputId = "down_edu", label = "Download the plot", class = "butt"),
							tags$head(tags$style(" .butt{color: #3399cc;}")),
		                     plotlyOutput(outputId = "SelectedPlot_edu", height = 550),
							 tableOutput('edu_table_plot'),
							br(),br(),
		                     column(width = 6,
		                            box(width = NULL, solidheader = TRUE,
		                                plotlyOutput(outputId = "edu_sunburst", height = 450, width = 350),
		                                uiOutput("edu_caption")
		                            )),uiOutput("edu_htmltxt1")
		                     
		                 )
		          ),
		          column(width=6,
		                 box(width = NULL, solidheader = TRUE,
		                     uiOutput("edu_themetype"),
		                     selectInput(inputId = "edu_inputthemetype", label = "Themes: ",
		                                 choices = c("Attainment"),
		                                 selected = "Attainment"),
		                     selectInput(inputId = "edu_th_type", label = "category: ",
		                                 choices = c("age", "sex"),
		                                 selected = "age"),
		                     actionButton("plt_btn_t8", "Plot"),
							actionButton("tbl_btn_t8", "Table"),
							useShinyjs(), 
							downloadButton(outputId = "theme_down_edu", label = "Download the plot", class = "butt"),
							tags$head(tags$style(" .butt{color: #3399cc;}")),							
		                     plotlyOutput(outputId = "edu_SelectedPlot_theme", height = 450),
							 tableOutput('theme_edu_table_plot'),
		                     selectInput(inputId = "edu_questions", label = "Questions: ",
		                                 choices = c("Have you ever attended school",
		                                             "How many years were you in formal schooling",
		                                             "Are you able to read at all",
		                                             "Are you able to read English",
		                                             "Please read the following sentences aloud to me",
		                                             "Are you able to read ITaukei, Hindi or other language",
		                                             "Please read the following sentences aloud to me",
		                                             "Are you able to write at all",
		                                             "Are you able to write in English",
		                                             "Please write two sentences about what you did yesterday",
		                                             "Are you able to write ITaukei, Hindi or other language",
		                                             "Please write two sentences in ITaukei, Hindi or other language",
		                                             "Are you able to do some mathematics",
		                                             "Addition and subtraction problem",
		                                             "Multiplication and division problem"
		                                 ),
		                                 selected = "Have you ever attended school"),
							useShinyjs(), 
							downloadButton(outputId = "ques_down_edu", label = "Download the plot", class = "butt"),
							tags$head(tags$style(" .butt{color: #3399cc;}")),
							 plotlyOutput(outputId = "edu_SelectedPlot_question", height = 450),
							 h4("Dimensional tree map"),
		                     collapsibleTreeOutput("edu_TreePlot", height = "600px"))
		          )
		          
		          
		        )
		),
		
	# Energy tab content
		tabItem(tabName = "Energy",
		        h1("Energy/Fuel"),
		        fluidRow(
		          column(width = 6,
		                 box(height = 1270 ,width = NULL, solidheader = TRUE,
		                     uiOutput("energy_htmltxt"),
		                     selectInput(inputId = "energyDimensiontype", label = "category: ",
		                                 choices = c("default" , "age", "sex"),
		                                 selected = "default"),
							actionButton("plt_btn_9", "Plot"),
							actionButton("tbl_btn_9", "Table"),
							useShinyjs(), 
							downloadButton(outputId = "down_energy", label = "Download the plot", class = "butt"),
							tags$head(tags$style(" .butt{color: #3399cc;}")),
		                     plotlyOutput(outputId = "SelectedPlot_energy", height = 550),
							 tableOutput('energy_table_plot'),
							br(),br(),
		                     column(width = 6,
		                            box(width = NULL, solidheader = TRUE,
		                                plotlyOutput(outputId = "energy_sunburst", height = 450, width = 350),
		                                uiOutput("energy_caption")
		                            )),uiOutput("energy_htmltxt1")
		                     
		                 )
		          ),
		          column(width=6,
		                 box(width = NULL, solidheader = TRUE,
		                     uiOutput("energy_themetype"),
		                     selectInput(inputId = "energy_inputthemetype", label = "Themes: ",
		                                 choices = c("Electricity", "Cooking fuel"),
		                                 selected = "Energy/Fuel - Electricity"),
		                     selectInput(inputId = "energy_th_type", label = "category: ",
		                                 choices = c("age", "sex"),
		                                 selected = "age"),
							actionButton("plt_btn_t9", "Plot"),
							actionButton("tbl_btn_t9", "Table"),
							useShinyjs(), 
							downloadButton(outputId = "theme_down_energy", label = "Download the plot", class = "butt"),
							tags$head(tags$style(" .butt{color: #3399cc;}")),
		                     plotlyOutput(outputId = "energy_SelectedPlot_theme", height = 450),
							tableOutput('theme_energy_table_plot'),

		                     selectInput(inputId = "energy_questions", label = "Questions: ",
		                                 choices = c("Does your dwelling have access to electricity", 
		                                             "Approximately how many hours per day on average does your dwelling have electricity",
		                                             "How reliable is your dwelling's access to electricity",
		                                             "What is the primary source of cooking fuel in this household"),
		                                 selected = "Does your dwelling have access to electricity"),
		                     useShinyjs(), 
							downloadButton(outputId = "ques_down_energy", label = "Download the plot", class = "butt"),
							tags$head(tags$style(" .butt{color: #3399cc;}")),
							 
							 plotlyOutput(outputId = "energy_SelectedPlot_question", height = 450),
							 h4("Dimensional tree map"),
		                     collapsibleTreeOutput("energy_TreePlot", height = "600px"))
		          )
		          
		          
		        )
		)
		,
		
		# Sanitation tab content
		tabItem(tabName = "Sanitation",
		        h1("Sanitation"),
		        fluidRow(
		          column(width = 6,
		                 box(height = 1270 ,width = NULL, solidheader = TRUE,
		                     uiOutput("san_htmltxt"),
		                     selectInput(inputId = "sanDimensiontype", label = "category: ",
		                                 choices = c("default" , "age", "sex"),
		                                 selected = "default"),
							actionButton("plt_btn_10", "Plot"),
							actionButton("tbl_btn_10", "Table"),
							useShinyjs(), 
							downloadButton(outputId = "down_sanitation", label = "Download the plot", class = "butt"),
							tags$head(tags$style(" .butt{color: #3399cc;}")),
		                     plotlyOutput(outputId = "SelectedPlot_san", height = 550),
							 tableOutput('sanitation_table_plot'),
							br(),br(),
		                     column(width = 6,
		                            box(width = NULL, solidheader = TRUE,
		                                plotlyOutput(outputId = "san_sunburst", height = 450, width = 350),
		                                uiOutput("san_caption")
		                            )),uiOutput("san_htmltxt1")
		                     
		                 )
		          ),
		          column(width=6,
		                 box(width = NULL, solidheader = TRUE,
		                     uiOutput("san_themetype"),
		                     selectInput(inputId = "san_inputthemetype", label = "Themes: ",
		                                 choices = c("Quality"),
		                                 selected = "Quality"),
		                     selectInput(inputId = "san_th_type", label = "category: ",
		                                 choices = c("age", "sex"),
		                                 selected = "age"),
							actionButton("plt_btn_t10", "Plot"),
							actionButton("tbl_btn_t10", "Table"),
							useShinyjs(), 
							downloadButton(outputId = "theme_down_sanitation", label = "Download the plot", class = "butt"),
							tags$head(tags$style(" .butt{color: #3399cc;}")),
		                     plotlyOutput(outputId = "san_SelectedPlot_theme", height = 450),
		                     tableOutput('theme_sanitation_table_plot'),
		                     selectInput(inputId = "san_questions", label = "Questions: ",
		                                 choices = c("What toilet facilities do the household use")),
		                    useShinyjs(), 
							downloadButton(outputId = "ques_down_sanitation", label = "Download the plot", class = "butt"),
							tags$head(tags$style(" .butt{color: #3399cc;}")),
							 plotlyOutput(outputId = "san_SelectedPlot_question", height = 450),
							 h4("Dimensional tree map"),
		                     collapsibleTreeOutput("san_TreePlot", height = "600px"))
		          )
		          
		          
		        )
		)
		,
		
		# Relationships tab content
		tabItem(tabName = "Relationships",
		        h1("Relationships"),
		        fluidRow(
		          column(width = 6,
		                 box(height = 1270 ,width = NULL, solidheader = TRUE,
		                     uiOutput("rel_htmltxt"),
		                     selectInput(inputId = "relDimensiontype", label = "category: ",
		                                 choices = c("default" , "age", "sex"),
		                                 selected = "default"),
							actionButton("plt_btn_11", "Plot"),
							actionButton("tbl_btn_11", "Table"),
							useShinyjs(), 
							downloadButton(outputId = "down_relation", label = "Download the plot", class = "butt"),
							tags$head(tags$style(" .butt{color: #3399cc;}")),
		                     plotlyOutput(outputId = "SelectedPlot_rel", height = 550),
							 tableOutput('relation_table_plot'),
							br(),br(),
		                     column(width = 6,
		                            box(width = NULL, solidheader = TRUE,
		                                plotlyOutput(outputId = "rel_sunburst", height = 450, width = 350),
		                                uiOutput("rel_caption")
		                            )),uiOutput("rel_htmltxt1")
		                     
		                 )
		          ),
		          column(width=6,
		                 box(width = NULL, solidheader = TRUE,
		                     uiOutput("rel_themetype"),
		                     selectInput(inputId = "rel_inputthemetype", label = "Themes: ",
		                                 choices = c("Support"),
		                                 selected = "Support"),
		                     selectInput(inputId = "rel_th_type", label = "category: ",
		                                 choices = c("age", "sex"),
		                                 selected = "age"),
							actionButton("plt_btn_t11", "Plot"),
							actionButton("tbl_btn_t11", "Table"),
							useShinyjs(), 
							downloadButton(outputId = "theme_down_relation", label = "Download the plot", class = "butt"),
							tags$head(tags$style(" .butt{color: #3399cc;}")),
		                     plotlyOutput(outputId = "rel_SelectedPlot_theme", height = 450),
							 tableOutput('theme_relation_table_plot'),
		                     selectInput(inputId = "rel_questions", label = "Questions: ",
		                                 choices = "If you were in trouble, how much support could you count on from friends and family"                                            ),
		                     useShinyjs(), 
							downloadButton(outputId = "ques_down_relation", label = "Download the plot", class = "butt"),
							tags$head(tags$style(" .butt{color: #3399cc;}")),
							 plotlyOutput(outputId = "rel_SelectedPlot_question", height = 450),
							 h4("Dimensional tree map"),
		                     collapsibleTreeOutput("rel_TreePlot", height = "600px"))
		          )
		          
		          
		        )
		)
	
		,
		
		# Clothing tab content
		tabItem(tabName = "Clothing",
		        h1("Clothing"),
		        fluidRow(
		          column(width = 6,
		                 box(height = 1270 ,width = NULL, solidheader = TRUE,
		                     uiOutput("cloth_htmltxt"),
		                     selectInput(inputId = "clothDimensiontype", label = "category: ",
		                                 choices = c("default" , "age", "sex"),
		                                 selected = "default"),
							actionButton("plt_btn_12", "Plot"),
							actionButton("tbl_btn_12", "Table"),
							useShinyjs(), 
							downloadButton(outputId = "down_clothing", label = "Download the plot", class = "butt"),
							tags$head(tags$style(" .butt{color: #3399cc;}")),
		                     plotlyOutput(outputId = "SelectedPlot_cloth", height = 550),
							 tableOutput('clothes_table_plot'),
							br(),br(),
		                     column(width = 6,
		                            box(width = NULL, solidheader = TRUE,
		                                plotlyOutput(outputId = "cloth_sunburst", height = 450, width = 350),
		                                uiOutput("cloth_caption")
		                            )),uiOutput("cloth_htmltxt1")
		                     
		                 )
		          ),
		          column(width=6,
		                 box(width = NULL, solidheader = TRUE,
		                     uiOutput("cloth_themetype"),
		                     selectInput(inputId = "cloth_inputthemetype", label = "Themes: ",
		                                 choices = c("Personal care"),
		                                 selected = "Personal care"),
		                     selectInput(inputId = "cloth_th_type", label = "category: ",
		                                 choices = c("age", "sex"),
		                                 selected = "age"),
							actionButton("plt_btn_t12", "Plot"),
							actionButton("tbl_btn_t12", "Table"),
							useShinyjs(), 
							downloadButton(outputId = "theme_down_clothing", label = "Download the plot", class = "butt"),
							tags$head(tags$style(" .butt{color: #3399cc;}")),
		                     plotlyOutput(outputId = "cloth_SelectedPlot_theme", height = 450),
							  tableOutput('themes_clothes_table_plot'),
		                     selectInput(inputId = "cloth_questions", label = "Questions: ",
		                                 choices = c("To what extent does your clothing and footwear protect you from the weather and from hazards in your environment",
		                                             "To what extent are you able to present yourself in public, in terms of clothing, body odour and grooming, in a way that is acceptable by standards of your community")),
		                     useShinyjs(), 
							downloadButton(outputId = "ques_down_clothing", label = "Download the plot", class = "butt"),
							tags$head(tags$style(" .butt{color: #3399cc;}")),
							 plotlyOutput(outputId = "cloth_SelectedPlot_question", height = 450),
							 h4("Dimensional tree map"),
		                     collapsibleTreeOutput("cloth_TreePlot", height = "600px"))
		          )
		          
		          
		        )
		)
		
		,
		
		# TimeUse tab content
		tabItem(tabName = "TimeUse",
		        h1("Time Use"),
		        fluidRow(
		          column(width = 6,
		                 box(height = 1270 ,width = NULL, solidheader = TRUE,
		                     uiOutput("time_htmltxt"),
		                     selectInput(inputId = "timeDimensiontype", label = "category: ",
		                                 choices = c("default" , "age", "sex"),
		                                 selected = "default"),
							actionButton("plt_btn_13", "Plot"),
							actionButton("tbl_btn_13", "Table"),
							useShinyjs(), 
							downloadButton(outputId = "down_timeuse", label = "Download the plot", class = "butt"),
							tags$head(tags$style(" .butt{color: #3399cc;}")),
		                     plotlyOutput(outputId = "SelectedPlot_time", height = 550),
							 tableOutput('timeuse_table_plot'),
							br(),br(),
		                     column(width = 6,
		                            box(width = NULL, solidheader = TRUE,
		                                plotlyOutput(outputId = "time_sunburst", height = 450, width = 350),
		                                uiOutput("time_caption")
		                            )),uiOutput("time_htmltxt1")
		                     
		                 )
		          ),
		          column(width=6,
		                 box(width = NULL, solidheader = TRUE,
		                     uiOutput("time_themetype"),
		                     selectInput(inputId = "time_inputthemetype", label = "Themes: ",
		                                 choices = c("Labour burden"),
		                                 selected = "Labour burden"),
		                     selectInput(inputId = "time_th_type", label = "category: ",
		                                 choices = c("age", "sex"),
		                                 selected = "age"),
							actionButton("plt_btn_t13", "Plot"),
							actionButton("tbl_btn_t13", "Table"),
							useShinyjs(), 
							downloadButton(outputId = "theme_down_timeuse", label = "Download the plot", class = "butt"),
							tags$head(tags$style(" .butt{color: #3399cc;}")),
		                     plotlyOutput(outputId = "time_SelectedPlot_theme", height = 450),
							 tableOutput('theme_timeuse_table_plot'),
		                     selectInput(inputId = "time_questions", label = "Questions: ",
		                                 choices = c("Record how much (primary) time was spent on personal care, leisure, entertainment and religious activities")),
		                     useShinyjs(), 
							downloadButton(outputId = "ques_down_timeuse", label = "Download the plot", class = "butt"),
							tags$head(tags$style(" .butt{color: #3399cc;}")),
							 plotlyOutput(outputId = "time_SelectedPlot_question", height = 450),
							 h4("Dimensional tree map"),
		                     collapsibleTreeOutput("time_TreePlot", height = "600px"))
		          )
		          
		          
		        )
		)
		
		,


      # DimensionsCorrelation tab content
      tabItem(tabName = "DimensionsCorrelation",
              h1("Dimensions Correlation"),
			  h2("Highly correlated dimensions indicate that those deprivations tend to be assosciated"),
			  plotlyOutput(outputId = "corr_plot", height = 550)
      )
    )
  )