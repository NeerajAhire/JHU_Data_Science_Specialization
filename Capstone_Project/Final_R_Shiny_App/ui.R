#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)




shinyUI(fluidPage(
  
  titlePanel("Next Word Prediction App - Neeraj Ahire (07/16/22)"),
  div(style = "margin-top: 30px"),
  sidebarLayout(
    sidebarPanel (HTML(paste("<b>INSTRUCTIONS:</b>", 
                             "<b>1. Enter Sentence in the Tab on the right.</b>",
                             "<b>2. Please make sure the sentence is atleast 3 words long.</b>",
                             "<b>3. Look for the predicted words below the Tab.</b>", 
                             "<b>4. Very low probabilities would be shown as zero.</b>",
                             "<b>5. SBO and KBO stand for Stupid Backoff and Katz's Backoff model respectively.</b>",
                             sep="<br/>")) ),
    
    mainPanel (textInput("itxt",
                         "Enter sentence here:", value = ""),
               actionButton("goButton", "Go!", class = "btn-success"),div(style = "margin-top: 20px"),
               
               
               fluidPage(splitLayout(tableOutput("odf1"), tableOutput("odf2"), cellWidths = 220))
               
               
    )
  )
  
  
  
))








