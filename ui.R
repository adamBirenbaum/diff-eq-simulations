
library(ggplot2)
library(dplyr)
library(gganimate)
library(shiny)
library(shinydashboard)

###
# ONLY NEED TO EDIT THIS PATH TO A FOLDER WHERE TEMPORARY GIFS WILL BE STORED


common_path <- "public/project/diff-eq-simulations/"
if (Sys.info()["nodename"] == "ADAM-DROPLET"){
  path_to_folder<<- paste0("/var/www/adambirenbaum.com/",common_path)
}else if(Sys.info()["sysname"] == "Windows"){
  path_to_folder<<- "D:/abire/Documents/heat_transfer_sim/"
}else{
  path_to_folder <<- "~/diff-eq-simulations/"
  
}

successActionButton <- function(inputId,label) tags$button(id = inputId, type = "button", class = "btn btn-success action-button btn-lg", label)
warningActionButton <- function(inputId,label) tags$button(id = inputId, type = "button", class = "btn btn-warning action-button btn-lg", label)
infoActionButton <- function(inputId,label) tags$button(id = inputId, type = "button", class = "btn btn-info action-button btn-lg", label)
dangerActionButton <- function(inputId,label) tags$button(id = inputId, type = "button", class = "btn btn-danger action-button btn-lg", label)
primaryActionButton <- function(inputId,label) tags$button(id = inputId, type = "button", class = "btn btn-primary action-button btn-lg", label)



###
dashboardPage(
  dashboardHeader(title = "Mechanics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Pendulum",
               menuSubItem("Single", tabName = "single_pendulum"),
               menuSubItem("Double",tabName = "double_pendulum")
      ),
      menuItem("Orbits",
               menuSubItem("Launch", tabName = "launch")
               )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "single_pendulum",
              
              fluidRow(
                column(width = 4,
                       box(
                         title = "Single Pendulum Inputs",solidHeader = T, status = "primary",width = NULL,
                         sliderInput("l","Length (m)",min = .1,max = 10,value = 1),
                         sliderInput("theta","Initial Angle (Degrees)",min = -180,max = 180,value = 90),
                         sliderInput("theta_dot","Initial Ang. Velocity (Degrees / s)",min = -540,max = 540,value = 0)


               
                         
                         
                       ),
                       box(
                         title = "Simulation Options",solidHeader = T, status = "primary",width = NULL,
                         fluidRow(
                           column(width = 12,
                                  sliderInput("mu","Drag Coefficient",min = 0, max = 5, value = .5,step = .5),
                                  sliderInput("time","Simulation Time (s)",min = 1, max = 20,value = 5),
                                  radioButtons("fps","Frames / Second",choices = c("Low" = 1,"Medium" =2,"High" = 3,"Ultra" = 4),selected = 1,inline = T)
                                  )
                       

                         )
                       ),
                       successActionButton("simulate_single_pendulum","Simulate")
                       #actionButton("simulate_double_pendulum","Double pend")
                       
                       
                ),
                column(width = 6,
                       uiOutput("ui_single_pendulum_output")
                       

       
                       
                       )
              )
              
              
      ),
      tabItem(tabName = "double_pendulum",
              
              fluidRow(
                column(width = 4,
                       box(
                         title = "Top Pendulum Inputs",solidHeader = T, status = "primary",width = NULL,
                         sliderInput("double_m1","Mass (kg)", min = 0.1, max = 50, value = 1),
                         sliderInput("double_l1","Length (m)",min = .1,max = 10,value = 1),
                         sliderInput("double_theta1","Initial Angle (Degrees)",min = -180,max = 180,value = 90),
                         sliderInput("double_theta_dot1","Initial Ang. Velocity (Degrees / s)",min = -540,max = 540,value = 0)
                         
                       ),
                       box(
                         title = "Bottom Pendulum Inputs",solidHeader = T, status = "primary",width = NULL,
                         sliderInput("double_m2","Mass (kg)", min = 0.1, max = 50, value = 1),
                         sliderInput("double_l2","Length (m)",min = .1,max = 10,value = 1),
                         sliderInput("double_theta2","Initial Angle (Degrees)",min = -180,max = 180,value = 90),
                         sliderInput("double_theta_dot2","Initial Ang. Velocity (Degrees / s)",min = -540,max = 540,value = 0)

                         
                       ),
                       box(
                         title = "Simulation Options",solidHeader = T, status = "primary",width = NULL,
                         fluidRow(
                           column(width = 12,
                                  sliderInput("mu_double","Drag Coefficient",min = 0, max = 5, value = .5,step = .5),
                                  sliderInput("double_time","Simulation Time (s)",min = 1, max = 20,value = 5),
                                  radioButtons("double_fps","Frames / Second",choices = c("Low" = 1,"Medium" =2,"High" = 3,"Ultra" = 4),selected = 1,inline = T)
                           )
                           
                           
                         )
                       ),
                       successActionButton("simulate_double_pendulum","Simulate")
                       #actionButton("simulate_double_pendulum","Double pend")
                       
                       
                ),
                column(width = 6,
                       uiOutput("ui_double_pendulum_output")
                       
                       
                       
                       
                )
              )
              
              
      ),
      tabItem(tabName = "launch",
              fluidRow(
                column(width = 4,
                       box(width = NULL, title = "Launch Parameters",status = "primary",solidHeader = T,
                           
                           sliderInput("launch_initial_v","Initial Velocity (m/s)",min = 1000, max = 6500, value = 5000),
                           sliderInput("launch_initial_theta","Initial Angle (degrees)",min = 0, max = 180, value = 45)
                           ),
                       
                       box(
                         title = "Simulation Options",solidHeader = T, status = "primary",width = NULL,
                         fluidRow(
                           column(width = 12,
                                  sliderInput("launch_time","Simulation Time (hours)",min = 1, max = 10,value = 2,step = 0.5),
                                  radioButtons("launch_fps","Frame Rate",choices = c("Low" = 1,"Medium" =2,"High" = 3,"Ultra" = 4),selected = 1,inline = T)
                           )
                           
                           
                         )
                       ),
                       successActionButton("simulate_launch","Simulate")
                       ),
                column(width = 6,
                       uiOutput("ui_launch_output")
                       
                       )
              )
              
              
              )
      
    )
    
  )
  
  
  
  
)

