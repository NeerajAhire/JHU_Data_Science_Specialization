

library(shiny)


shinyServer(function(input, output) {
    
    model <- reactive({
        brushed_data <- brushedPoints(swiss, input$brush1, xvar = "Education", yvar = "Fertility")
        if(nrow(brushed_data) < 2){
            return(NULL)
        }
        lm(Fertility ~ Education, data = brushed_data)
    })
    
    output$slopeOut <- renderText({
        if(input$show_values == FALSE){
            return("") 
        }
        if(is.null(model())){
            "No Model Found"
        } else {
            model()[[1]][2]}
    })
    
    output$intOut <- renderText({
        if(input$show_values == FALSE){
            return("")
        }
        if(is.null(model())){
            "No Model Found"
        } else {
            model()[[1]][1]
        }
    })
    
    output$plot1 <- renderPlot({
        plot(swiss[, input$variable], swiss$Fertility, xlab = input$variable,
             ylab = "Fertility", main = paste("Fertility as a measure of", input$variable, "(select required data points using brush)"),
             cex = 1.5, pch = 16, bty = "n")
        if(!is.null(model())){
            abline(model(), col = "blue", lwd = 2)
        }
    })
    
})

