#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking "Run App" above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(lattice)
library(ggplot2)
library(stats)
library(ipred)
library(plyr)
#library(dplyr)
library(e1071)
library(caret)

#library(shinyBS)
library(plotly)

#Sys.setlocale("LC_ALL", "en_US")

#library(rsconnect)

# e = local({load("Data/.RData"); environment()})
# tools:::makeLazyLoadDB(e, "New")
# 
# lazyLoad("New")
# ls()

#load("Data/.RData")

#source("www/global.R")

#require(shiny)

shinyServer(function(input, output) {
  #server <- function(input, output, session) {
  
  female.var = reactiveValues(data = "no")
  smoking.var = reactiveValues(data = "no")
  map.var = reactiveValues(data = "Western")
  
  observeEvent(input$female, {
    
    #updateButton(session, "female", disabled = TRUE, icon = icon("check"))
    #updateButton(session, "male", disabled = FALSE, icon = icon("check"))
    
    female.var$data = "yes"
  })
  
  observeEvent(input$male, {
    
    #updateButton(session, "male", disabled = TRUE, icon = icon("check"))
    #updateButton(session, "female", disabled = FALSE, icon = icon("check"))
    
    
    female.var$data = "no"
  })
  
  observeEvent(input$smoking, {
    
    #updateButton(session, "smoking", disabled = TRUE, icon = icon("check"))
    #updateButton(session, "nosmoking", disabled = FALSE, icon = icon("check"))
    
    smoking.var$data = "yes"
  })
  
  observeEvent(input$nosmoking, {
    
    #updateButton(session, "nosmoking", disabled = TRUE, icon = icon("check"))
    #updateButton(session, "smoking", disabled = FALSE, icon = icon("check"))
    
    
    smoking.var$data = "no"
  })
  
  # observeEvent(input$female, ({
  #   updateButton(session, "female", disabled = TRUE, icon = icon("check"))
  #   updateButton(session, "male", disabled = FALSE, icon = icon("check"))
  #   female = TRUE
  #   male = FALSE
  #   #validation.shiny.df$Sex = "Female"
  # }))  
  
  #output$mytable = renderDataTable({
  
  variables.list <- reactive({
    
    #output$coolplot <- renderPlotly({
    #shiny.predict.df = reactive({
    
    #input$male
    #input$female
    
    validation.shiny.df = data.frame(UnderlyingBroadGrp = as.factor(gsub("\\-", "\\.\\.\\.", input$UnderlyingBroadGrp)), 
                                     Sex = as.factor("Male"), 
                                     MStatus = as.factor(input$MStatus), 
                                     EduCode = as.factor(gsub("\\ ", "\\.\\.\\.", input$EduCode)), 
                                     OccupationGrp = as.factor(input$OccupationGrp), 
                                     #OccupationGrp = as.factor(substr(input$OccupationGrp, 1, regexpr("\\ ", input$OccupationGrp)[1] - 1)[1]), 
                                     Smoker = as.factor("Yes"), 
                                     ResProv = as.factor("Limpopo"))
    
    smoker = "smoker"
    province = "Limpopo"
    
    if (female.var$data == "yes"){
      validation.shiny.df$Sex = "Female"
    }else{
      validation.shiny.df$Sex = "Male"
    }
    
    if (smoking.var$data == "yes"){
      validation.shiny.df$Smoker = "Yes"
      smoker = "smoker"
    }else{
      validation.shiny.df$Smoker = "No"
      smoker = "non-smoker"
    }
    
    if (!is.null(input$province.click$x) && !is.null(input$province.click$y) && 
        input$province.click$x >= 30 && input$province.click$x <= 113 && 
        input$province.click$y >= 192 && input$province.click$y <= 245){
      validation.shiny.df$ResProv = "Western"
      province = "the Western Cape"
    }else if (!is.null(input$province.click$x) && !is.null(input$province.click$y) && 
              input$province.click$x >= 199 && input$province.click$x <= 237 && 
              input$province.click$y >= 104 && input$province.click$y <= 164){
      validation.shiny.df$ResProv = "KwaZulu.Natal"
      province = "KwaZulu-Natal"
    }else if (!is.null(input$province.click$x) && !is.null(input$province.click$y) && 
              input$province.click$x >= 194 && input$province.click$x <= 232 && 
              input$province.click$y >= 43 && input$province.click$y <= 101){
      validation.shiny.df$ResProv = "Mpumalanga"
      province = "Mpumalanga"
    }else if (!is.null(input$province.click$x) && !is.null(input$province.click$y) && 
              input$province.click$x >= 166 && input$province.click$x <= 222 && 
              input$province.click$y >= 7 && input$province.click$y <= 58){
      validation.shiny.df$ResProv = "Limpopo"
      province = "Limpopo"
    }else if (!is.null(input$province.click$x) && !is.null(input$province.click$y) && 
              input$province.click$x >= 168 && input$province.click$x <= 185 && 
              input$province.click$y >= 70 && input$province.click$y <= 91){
      validation.shiny.df$ResProv = "Gauteng"
      province = "Gauteng"
    }else if (!is.null(input$province.click$x) && !is.null(input$province.click$y) && 
              input$province.click$x >= 123 && input$province.click$x <= 160 && 
              input$province.click$y >= 58 && input$province.click$y <= 104){
      validation.shiny.df$ResProv = "North"
      province = "North West Province"
    }else if (!is.null(input$province.click$x) && !is.null(input$province.click$y) && 
              input$province.click$x >= 131 && input$province.click$x <= 182 && 
              input$province.click$y >= 99 && input$province.click$y <= 160){
      validation.shiny.df$ResProv = "Free"
      province = "the Free State"
    }else if (!is.null(input$province.click$x) && !is.null(input$province.click$y) && 
              input$province.click$x >= 114 && input$province.click$x <= 191 && 
              input$province.click$y >= 163 && input$province.click$y <= 229){
      validation.shiny.df$ResProv = "Eastern"
      province = "the Eastern Cape"
    }else if (!is.null(input$province.click$x) && !is.null(input$province.click$y) && 
              input$province.click$x >= 30 && input$province.click$x <= 105 && 
              input$province.click$y >= 99 && input$province.click$y <= 192){
      validation.shiny.df$ResProv = "Northern"
      province = "the Northern Cape"
    }
    
    
    #validation.shiny.df$Sex = renderText({
    #input$male
    # txt = "male"
    # return(txt)
    #})
    
    
    shiny.predict = data.frame(stats::predict(finalModel, newdata = validation.shiny.df, type="prob"))
    
    shiny.predict.df = data.frame(x = as.character(c(colnames(shiny.predict))), y = c(unlist(shiny.predict[1 , ])))
    
    shiny.predict.x = apply(shiny.predict.df, 1, function(m) list(as.character(paste(substring(m[["x"]], 2, nchar(m[["x"]]) - 1), "0 - ", 
                                                                                     substring(m[["x"]], 2, nchar(m[["x"]]) - 1), "9", sep = ""))))
    # y = apply(shiny.predict.df, 1, function(m) rowsum(grep(substring(m[["x"]], 2, nchar(m[["x"]]) - 1), ), m[["y"]])))
    
    shiny.predict.x = unique(shiny.predict.x)
    
    shiny.predict.y = vector("list", length(shiny.predict.x))
    
    for (a in 1:length(shiny.predict.y)){
      shiny.predict.y[[a]] = 0
    }
    
    for (a in 1:nrow(shiny.predict.df)){
      for(b in 1:length(shiny.predict.x)){
        if(substring(shiny.predict.df[a, "x"], 2, nchar(as.character(shiny.predict.df[a, "x"])) - 1) == substring(shiny.predict.x[[b]], 1, (regexpr(" -", shiny.predict.x[[b]]) - 2)[[1]])){
          shiny.predict.y[[b]] = shiny.predict.y[[b]] + shiny.predict.df[a, "y"]
        }
        
      }
    }
    
    shiny.predict.df = data.frame(x = c(unlist(shiny.predict.x)), y = c(unlist(shiny.predict.y)))
    shiny.predict.df = shiny.predict.df[order(as.numeric(as.character(substr(as.character(shiny.predict.df$x), 1, regexpr("\\ -", as.character(shiny.predict.df$x)) - 1)))), ]
    
    max.bar = which.max(shiny.predict.df$y)
    colors.list = vector("list", nrow(shiny.predict.df))
    colors.list[1:length(colors.list)] = "rgba(204,204,204,1)"
    colors.list[[max.bar]] = "rgba(222,45,38,0.8)"
    
    list("df1" = shiny.predict.df, 
         "colours" = colors.list, 
         "sex" = tolower(validation.shiny.df$Sex), 
         "smoker" = smoker, 
         "province" = province, 
         "minage" = substr(shiny.predict.df[max.bar, "x"], 1, regexpr(" -", shiny.predict.df[max.bar, "x"]) - 1), 
         "maxage" = substr(shiny.predict.df[max.bar, "x"], regexpr("- ", shiny.predict.df[max.bar, "x"]) + 2, nchar(c(as.character(shiny.predict.df[max.bar, "x"]))))
    )
    
  })  
  
  #shiny.predict.df = shiny.predict.df[shiny.predict.df$y > 0.05, ]
  
  #shiny.predict.df
  
  #})
  #shiny.predict
  
  # filtered <-
  #   bcl %>%
  #   filter(Price >= input$priceInput[1],
  #          Price <= input$priceInput[2],
  #          Type == input$typeInput,
  #          Country == input$countryInput
  #   )
  # output$plot <- renderPlotly({
  #plot_ly(shiny.predict, x = shiny.predict[, 2:ncol(shiny.predict)], y = shiny.predict[ , 1], type = "histogram")
  #output$coolplot <- renderPlot({ 
  
  output$mainplot <- renderPlotly({
    
    plot123 = plot_ly(variables.list()[["df1"]],
                      x = variables.list()[["df1"]][["x"]],
                      y = variables.list()[["df1"]][["y"]],
                      name = "Age of Death",
                      #type = "scatter",
                      #mode = "lines",
                      type = "bar", 
                      marker = list(color = variables.list()[["colours"]]))  %>% 
      layout(title = "Age of Death Probability Distribution", 
             xaxis = list(title = "Age Groups", 
                          type = "category",
                          categoryorder = "array",
                          categoryarray = order(as.numeric(as.character(substr(as.character(variables.list()[["df1"]][["x"]]), 1, regexpr("\\ -", as.character(variables.list()[["df1"]][["x"]])) - 1))))), 
             yaxis = list(title = "Probability"))
    #marker = list(color = "rgb(205, 12, 24)"))
    #line = list(color = "rgb(205, 12, 24)", width = 4)))
    
    
    # print(plot_ly(shiny.predict.df,
    #               x = ~x,
    #               y = ~y,
    #               name = "High 2014",
    #               #type = "scatter",
    #               #mode = "lines",
    #               type = "histogram", 
    #               color = "rgb(205, 12, 24)"))
    #               #line = list(color = "rgb(205, 12, 24)", width = 4)))
    
    plot123
    
    #})
  })
  
  output$map = renderImage({
    
    #outfile = tempfile(tempdir = "/female.jpg", fileext = ".jpg")
    
    list(
      src = "www/provincialmap.gif",
      width = "250",
      height = "250",
      filetype = "image/gif" 
      
      #alt = "This is a chainring"
    )
  }, deleteFile = FALSE)
  
  #tags$img(src= images[input$map])
  
  output$info <- renderText({
    paste0("<br>A ", variables.list()[["sex"]], " who is a ", variables.list()[["smoker"]], 
           ", living in ", variables.list()[["province"]], ", with ", 
           input$EduCode, " as highest qualification, who\"s occupation falls in the group of ", input$OccupationGrp, 
           ", and has a marital status of ", input$MStatus, " and has an illness with the code ", 
           #"<a href="https://data.code4sa.org/Government/Causes-Of-Death-2012/di7x-4ek4">", 
           input$UnderlyingBroadGrp, 
           #"</a>", 
           " is most likely to die between the ages of <b>", variables.list()[["minage"]], 
           "</b> and <b>", variables.list()[["maxage"]], "</b>.")
  })
  
}
)