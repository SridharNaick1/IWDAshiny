###################
# header.R
# 
# Create the header for the ui.
###################


header <- dashboardHeader(title = 'IDM',
							tags$li(class='dropdown',
							tags$a(target="_blank",tags$img(height="20px",src="IDM.png"))))
#header <-dashboardHeader(title = img(src="IDM.png", height = "50px", align = "left"))