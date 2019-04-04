
library(ggplot2)
library(dplyr)
library(gganimate)
library(shiny)

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


###

fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("l","Length (m)",min = .1,max = 10,value = 1),
      sliderInput("mu","Drag Coeff",min = 0, max = 5, value = .5,step = .5),
      sliderInput("theta","Initial Angle (Degrees)",min = -180,max = 180,value = 90),
      sliderInput("theta_dot","Initial Ang. Velocity (Degrees / s)",min = -540,max = 540,value = 0),
      hr(),
      sliderInput("time","Simulation Time (s)",min = 1, max = 20,value = 5),
      fluidRow(
        column(width = 6,
               radioButtons("fps","Frames / Second",choices = c("Low" = 1,"Medium" =2,"High" = 3,"Ultra" = 4),selected = 1)
               ),
        column(width = 6,
               radioButtons("acc","Simulation Accuracy",choices = c("Low" = 1,"Medium" =2,"High" = 3,"Ultra" = 4),selected = 1)
               )
      ),
      actionButton("enter","Simulate")#,
      #actionButton("enter2","Double pend")
        
    ),
    mainPanel(
      fluidRow(
        column(width = 9,offset = 3,
               imageOutput(outputId = "gif2")
               ),
      fluidRow(
        column(width = 9,offset = 3,
               imageOutput(outputId = "gif1")
               )
      )
        
      )
      
      
    )
    
  )
  
)
