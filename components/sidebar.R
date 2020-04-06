###################
# sidebar.R
# 
# Create the sidebar menu options for the ui.
###################
sidebar <- dashboardSidebar(
  sidebarMenu(

    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Food", tabName = "Food"),
    menuItem("Water", tabName = "Water"),
    menuItem("Shelter", tabName = "Shelter"),
    menuItem("Health", tabName = "Health"),
    menuItem("Education", tabName = "Education"),
    menuItem("Energy", tabName = "Energy"),
    menuItem("Sanitation", tabName = "Sanitation"),
    menuItem("Relationships", tabName = "Relationships"),
    menuItem("Clothing", tabName = "Clothing"),
    menuItem("TimeUse", tabName = "TimeUse"),
    menuItem("Work", tabName = "Work"),
    menuItem("Voice", tabName = "Voice"),
    menuItem("Environment", tabName = "Environment"),
    menuItem("DimensionsCorrelation", tabName = "DimensionsCorrelation")
    
  )
)
