#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(dplyr)
library(ipred)
library(plyr)
library(e1071)
library(caret)
library(shinyBS)
library(rsconnect)

#save(list = ls(all.names = TRUE), file = "C:/Users/jharmse/Documents/R/SatRday/Predictive - ML/SatRday Mortality/global/.RData", envir = .GlobalEnv)


require(shiny)

#load('C:/Users/jharmse/Documents/R/SatRday/Predictive - ML/.RData')
#rm(list=setdiff(ls(), c("training.df", 'finalModel', 'dat', 'dat1')))

#Underlying.list = unique(training.df$`Underlying_Broad_Grp`)
#Underlying.names = list()
# b = 0
# for (a in 1:length(Underlying.list)){
#   b = b + 1
#   Underlying.names[b] = substr(dat[match(paste0('(', substr(Underlying.list[[a]], 1, 3)), dat$V1), 'V1'], 
#                             regexpr('\\ ', dat[match(paste0('(', substr(Underlying.list[[a]], 1, 3)), dat$V1), 'V1']) + 1, 
#                             regexpr('\\ \\(', dat[match(paste0('(', substr(Underlying.list[[a]], 1, 3)), dat$V1), 'V1']) - 1)
# }
 
#bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

shinyUI <- fluidPage(
  
   titlePanel("Age of Death Calculator"),
   sidebarLayout(
     sidebarPanel(
       
       tags$head(
         tags$style(type="text/css", "select { max-width: 280px; }"),
         tags$style(type="text/css", ".span4 { max-width: 280px; }"),
         tags$style(type="text/css", ".well { max-width: 280px; }")
       ),
       
       p(bsButton('male', img(src = "/male.png", width = '72px', height = '72px')), 
         bsButton('female', img(src = "/female.jpg", width = '72px', height = '72px'))),
       p(bsButton('smoking', img(src = "/smoking.png", width = '72px', height = '72px')), 
       bsButton('nosmoking', img(src = "/nosmoking.png", width = '72px', height = '72px'))), 
       imageOutput('map', width = '250', height = '250', click = 'province.click'), 
       #WP = symbols(5 , 1,circle = 1.5,add = FALSE,inches=F, xlab = 'WP', fg = 10), 
       #selectInput('Sex', 'Sex', choices = unique(training.df$Sex), selected = paste(unique(training.df$Sex)[[1]])),
       selectInput('MStatus', 'Maritual Status', choices = sort(unique(training.df$MStatus)), selected = paste(unique(training.df$MStatus)[[1]])),
       selectInput('EduCode', 'Education', choices = sort(gsub('\\.\\.\\.', '\\ ', unique(training.df$EduCode))), selected = gsub('\\.\\.\\.', '\\ ', paste(unique(training.df$EduCode)[[1]]))),
       selectInput('OccupationGrp', 'Occupation', choices = sort(substr(dat1[523:532, 1], regexpr('[0-9]\\ ', dat1[523:532, 1])[1:nrow(dat1[523:532, ])] + 2, regexpr('\\ [0-9]', dat1[523:532, 1])[1:nrow(dat1[523:532, ])])), selected = paste(substr(dat1[523:532, 1], regexpr('[0-9]\\ ', dat1[523:532, 1])[1:nrow(dat1[523:532, ])] + 1, regexpr('\\ [0-9]', dat1[523:532, 1])[1:nrow(dat1[523:532, ])])[[1]])),
       #selectInput('Smoker', 'Smoker', choices = unique(training.df$Smoker), selected = paste(unique(training.df$Smoker)[[1]])),
       #selectInput('ResProv', 'ResProv', choices = unique(training.df$ResProv), selected = paste(unique(training.df$ResProv)[[1]])),
       selectInput('Underlying_Broad_Grp', label = 'Illness', choices = sort(gsub('\\.\\.\\.', '\\-', unique(training.df$`Underlying_Broad_Grp`))), selected = gsub('\\.\\.\\.', '\\-', paste(sort(unique(training.df$`Underlying_Broad_Grp`))[[1]])))  
       
       #sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
       #radioButtons("typeInput", "Product type",
      #             choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
      #             selected = "WINE"),
       #selectInput("countryInput", "Country",
      #             choices = c("CANADA", "FRANCE", "ITALY"))
     ),
     mainPanel(
        plotlyOutput("mainplot", height = '650'), 
        htmlOutput('info')
        #verbatimTextOutput("info")
       #showOutput('coolplot', 'plotly')
       # br(), br(),
       # tableOutput("results")
       #dataTableOutput('mytable')
     )
   )
 )
 


# server <- function(input, output, session) {
# #server <- function(input, output, session) {
#   
#   
#   female.var = reactiveValues(data = 'no')
#   smoking.var = reactiveValues(data = 'no')
#   map.var = reactiveValues(data = 'Western')
#   
#   observeEvent(input$female, {
#     
#     updateButton(session, "female", disabled = TRUE, icon = icon('check'))
#     updateButton(session, "male", disabled = FALSE, icon = icon('check'))
#     
#     female.var$data = 'yes'
#   })
#   
#   observeEvent(input$male, {
#     
#     updateButton(session, "male", disabled = TRUE, icon = icon('check'))
#     updateButton(session, "female", disabled = FALSE, icon = icon('check'))
#     
#     
#     female.var$data = 'no'
#   })
#   
#   observeEvent(input$smoking, {
#     
#     updateButton(session, "smoking", disabled = TRUE, icon = icon('check'))
#     updateButton(session, "nosmoking", disabled = FALSE, icon = icon('check'))
#     
#     smoking.var$data = 'yes'
#   })
#   
#   observeEvent(input$nosmoking, {
#     
#     updateButton(session, "nosmoking", disabled = TRUE, icon = icon('check'))
#     updateButton(session, "smoking", disabled = FALSE, icon = icon('check'))
#     
#     
#     smoking.var$data = 'no'
#   })
#   
#   # observeEvent(input$female, ({
#   #   updateButton(session, "female", disabled = TRUE, icon = icon('check'))
#   #   updateButton(session, "male", disabled = FALSE, icon = icon('check'))
#   #   female = TRUE
#   #   male = FALSE
#   #   #validation.shiny.df$Sex = 'Female'
#   # }))  
#   
#   #output$mytable = renderDataTable({
#   
#   variables.list <- reactive({
#     
#   #output$coolplot <- renderPlotly({
#     #shiny.predict.df = reactive({
#     
#     #input$male
#     #input$female
#       
#     validation.shiny.df = data.frame(Underlying_Broad_Grp = as.factor(gsub('\\-', '\\.\\.\\.', input$`Underlying_Broad_Grp`)), 
#                                      Sex = as.factor('Male'), 
#                                      MStatus = as.factor(input$MStatus), 
#                                      EduCode = as.factor(gsub('\\ ', '\\.\\.\\.', input$EduCode)), 
#                                      OccupationGrp = as.factor(substr(input$OccupationGrp, 1, regexpr('\\ ', input$OccupationGrp)[1] - 1)[1]), 
#                                      Smoker = as.factor('Yes'), 
#                                      ResProv = as.factor('Limpopo'))
#     
#     smoker = 'smoker'
#     province = 'Limpopo'
#     
#     if (female.var$data == 'yes'){
#       validation.shiny.df$Sex = 'Female'
#     }else{
#       validation.shiny.df$Sex = 'Male'
#     }
#     
#     if (smoking.var$data == 'yes'){
#       validation.shiny.df$Smoker = 'Yes'
#       smoker = 'smoker'
#     }else{
#       validation.shiny.df$Smoker = 'No'
#       smoker = 'non-smoker'
#     }
#     
#     if (!is.null(input$province.click$x) && !is.null(input$province.click$y) && 
#         input$province.click$x >= 30 && input$province.click$x <= 113 && 
#         input$province.click$y >= 192 && input$province.click$y <= 245){
#       validation.shiny.df$ResProv = 'Western'
#       province = 'the Western Cape'
#     }else if (!is.null(input$province.click$x) && !is.null(input$province.click$y) && 
#               input$province.click$x >= 199 && input$province.click$x <= 237 && 
#               input$province.click$y >= 104 && input$province.click$y <= 164){
#       validation.shiny.df$ResProv = 'KwaZulu-Natal'
#       province = 'KwaZulu-Natal'
#     }else if (!is.null(input$province.click$x) && !is.null(input$province.click$y) && 
#               input$province.click$x >= 194 && input$province.click$x <= 232 && 
#               input$province.click$y >= 43 && input$province.click$y <= 101){
#       validation.shiny.df$ResProv = 'Mpumalanga'
#       province = 'Mpumalanga'
#     }else if (!is.null(input$province.click$x) && !is.null(input$province.click$y) && 
#               input$province.click$x >= 166 && input$province.click$x <= 222 && 
#               input$province.click$y >= 7 && input$province.click$y <= 58){
#       validation.shiny.df$ResProv = 'Limpopo'
#       province = 'Limpopo'
#     }else if (!is.null(input$province.click$x) && !is.null(input$province.click$y) && 
#               input$province.click$x >= 168 && input$province.click$x <= 185 && 
#               input$province.click$y >= 70 && input$province.click$y <= 91){
#       validation.shiny.df$ResProv = 'Gauteng'
#       province = 'Gauteng'
#     }else if (!is.null(input$province.click$x) && !is.null(input$province.click$y) && 
#               input$province.click$x >= 123 && input$province.click$x <= 160 && 
#               input$province.click$y >= 58 && input$province.click$y <= 104){
#       validation.shiny.df$ResProv = 'North'
#       province = 'North West Province'
#     }else if (!is.null(input$province.click$x) && !is.null(input$province.click$y) && 
#               input$province.click$x >= 131 && input$province.click$x <= 182 && 
#               input$province.click$y >= 99 && input$province.click$y <= 160){
#       validation.shiny.df$ResProv = 'Free'
#       province = 'the Free State'
#     }else if (!is.null(input$province.click$x) && !is.null(input$province.click$y) && 
#               input$province.click$x >= 114 && input$province.click$x <= 191 && 
#               input$province.click$y >= 163 && input$province.click$y <= 229){
#       validation.shiny.df$ResProv = 'Eastern'
#       province = 'the Eastern Cape'
#     }else if (!is.null(input$province.click$x) && !is.null(input$province.click$y) && 
#               input$province.click$x >= 30 && input$province.click$x <= 105 && 
#               input$province.click$y >= 99 && input$province.click$y <= 192){
#       validation.shiny.df$ResProv = 'Northern'
#       province = 'the Northern Cape'
#     }
# 
#     
#     #validation.shiny.df$Sex = renderText({
#       #input$male
#      # txt = 'male'
#      # return(txt)
#     #})
#     
#     
#     shiny.predict = data.frame(predict(finalModel, newdata = validation.shiny.df, type="prob"))
#     
#     shiny.predict.df = data.frame(x = as.character(c(colnames(shiny.predict))), y = c(unlist(shiny.predict[1 , ])))
#     
#     shiny.predict.x = apply(shiny.predict.df, 1, function(m) list(as.character(paste(substring(m[['x']], 2, nchar(m[['x']]) - 1), '0 - ', 
#                                             substring(m[['x']], 2, nchar(m[['x']]) - 1), '9', sep = ''))))
#                                   # y = apply(shiny.predict.df, 1, function(m) rowsum(grep(substring(m[['x']], 2, nchar(m[['x']]) - 1), ), m[['y']])))
#     
#     shiny.predict.x = unique(shiny.predict.x)
#     
#     shiny.predict.y = vector('list', length(shiny.predict.x))
#     
#     for (a in 1:length(shiny.predict.y)){
#       shiny.predict.y[[a]] = 0
#     }
#     
#     for (a in 1:nrow(shiny.predict.df)){
#       for(b in 1:length(shiny.predict.x)){
#       if(substring(shiny.predict.df[a, 'x'], 2, nchar(as.character(shiny.predict.df[a, 'x'])) - 1) == substring(shiny.predict.x[[b]], 1, (regexpr(' -', shiny.predict.x[[b]]) - 2)[[1]])){
#         shiny.predict.y[[b]] = shiny.predict.y[[b]] + shiny.predict.df[a, 'y']
#       }
#         
#       }
#     }
#     
#     shiny.predict.df = data.frame(x = c(unlist(shiny.predict.x)), y = c(unlist(shiny.predict.y)))
#     shiny.predict.df = shiny.predict.df[order(as.numeric(as.character(substr(as.character(shiny.predict.df$x), 1, regexpr('\\ -', as.character(shiny.predict.df$x)) - 1)))), ]
#     
#     max.bar = which.max(shiny.predict.df$y)
#     colors.list = vector('list', nrow(shiny.predict.df))
#     colors.list[1:length(colors.list)] = 'rgba(204,204,204,1)'
#     colors.list[[max.bar]] = 'rgba(222,45,38,0.8)'
#     
#     list('df1' = shiny.predict.df, 
#                 'colours' = colors.list, 
#          'sex' = tolower(validation.shiny.df$Sex), 
#          'smoker' = smoker, 
#          'province' = province, 
#          'minage' = substr(shiny.predict.df[max.bar, 'x'], 1, regexpr(' -', shiny.predict.df[max.bar, 'x']) - 1), 
#          'maxage' = substr(shiny.predict.df[max.bar, 'x'], regexpr('- ', shiny.predict.df[max.bar, 'x']) + 2, nchar(c(as.character(shiny.predict.df[max.bar, 'x']))))
#          )
#     
#   })  
#     
#     #shiny.predict.df = shiny.predict.df[shiny.predict.df$y > 0.05, ]
#     
#     #shiny.predict.df
#     
#     #})
#     #shiny.predict
#     
#     # filtered <-
#     #   bcl %>%
#     #   filter(Price >= input$priceInput[1],
#     #          Price <= input$priceInput[2],
#     #          Type == input$typeInput,
#     #          Country == input$countryInput
#     #   )
#     # output$plot <- renderPlotly({
#     #plot_ly(shiny.predict, x = shiny.predict[, 2:ncol(shiny.predict)], y = shiny.predict[ , 1], type = 'histogram')
#   #output$coolplot <- renderPlot({ 
# 
#   output$mainplot <- renderPlotly({
#       
#   plot123 = plot_ly(variables.list()[['df1']],
#                         x = variables.list()[['df1']][['x']],
#                         y = variables.list()[['df1']][['y']],
#                         name = 'Age of Death',
#                         #type = 'scatter',
#                         #mode = 'lines',
#                         type = 'bar', 
#                         marker = list(color = variables.list()[['colours']]))  %>% 
#                         layout(title = 'Age of Death Probability Distribution', 
#                                xaxis = list(title = 'Age Groups', 
#                                             type = "category",
#                                             categoryorder = "array",
#                                             categoryarray = order(as.numeric(as.character(substr(as.character(variables.list()[['df1']][['x']]), 1, regexpr('\\ -', as.character(variables.list()[['df1']][['x']])) - 1))))), 
#                                yaxis = list(title = 'Probability'))
#                         #marker = list(color = 'rgb(205, 12, 24)'))
#     #line = list(color = 'rgb(205, 12, 24)', width = 4)))
#      
#      
#      # print(plot_ly(shiny.predict.df,
#      #               x = ~x,
#      #               y = ~y,
#      #               name = 'High 2014',
#      #               #type = 'scatter',
#      #               #mode = 'lines',
#      #               type = 'histogram', 
#      #               color = 'rgb(205, 12, 24)'))
#      #               #line = list(color = 'rgb(205, 12, 24)', width = 4)))
#      
#      plot123
#      
#      #})
#   })
#     
#     output$map = renderImage({
#       
#       #outfile = tempfile(tempdir = '/female.jpg', fileext = '.jpg')
#       
#       list(
#         src = 'www/provincial_map.gif',
#         width = '250',
#         height = '250',
#         filetype = "image/gif" 
#        
#         #alt = "This is a chainring"
#       )
#     }, deleteFile = FALSE)
#     
#     #tags$img(src= images[input$map])
#     
#     output$info <- renderText({
#       paste0('<br>A ', variables.list()[['sex']], ' who is a ', variables.list()[['smoker']], 
#              ', living in ', variables.list()[['province']], ', with ', 
#              input$EduCode, " as highest qualification, who\'s occupation falls in the group of ", input$OccupationGrp, 
#              ', and has a maritual status of ', input$MStatus, ' and has an illness with the code ', 
#              "<a href='https://data.code4sa.org/Government/Causes-Of-Death-2012/di7x-4ek4'>", 
#              input$`Underlying_Broad_Grp`, '</a>', ' is most likely to die between the ages of <b>', variables.list()[['minage']], 
#              '</b> and <b>', variables.list()[['maxage']], '</b>.')
#     })
#     
# }

 
 
#shinyApp(ui = shinyUI(), server = shinyServer(), options = list(height = 1080))

# library(shiny)
# 
# # Define UI for application that draws a histogram
# shinyUI(fluidPage(
#   
#   # Application title
#   titlePanel("Old Faithful Geyser Data"),
#   
#   # Sidebar with a slider input for number of bins 
#   sidebarLayout(
#     sidebarPanel(
#        sliderInput("bins",
#                    "Number of bins:",
#                    min = 1,
#                    max = 50,
#                    value = 30)
#     ),
#     
#     # Show a plot of the generated distribution
#     mainPanel(
#        plotOutput("distPlot")
#     )
#   )
# ))
