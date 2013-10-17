library(shiny)

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Exit-poll 8 сентября 2013. Псковская область"),
  
  sidebarPanel(
    
    wellPanel(
      h3("Отображение графиков"),
      checkboxInput(inputId = "turnout_plot", label = "Явка избирателей",
                    value = TRUE),
      checkboxInput(inputId = "props_plot", 
                    label = "Распределение голосов", value = TRUE),
      selectInput("props_plot_type", "Тип графика с распределением голосов:",
                  list("Круги" = "pie", 
                       "Полосы (с партиями)" = "bar"))
      
  ),
    
    wellPanel(
      h3("Отображение таблиц"),
      checkboxInput(inputId = "tbl_turn", label = "Явка избирателей", value = TRUE),
      br(),
      checkboxInput(inputId = "okrug_psk", label = "Псковский район", value = TRUE),
      checkboxInput(inputId = "okrug_nvl", label = "Невельский район", value = T),
      checkboxInput(inputId = "okrug_lkn", label = "Локнянский район", value = T),
      checkboxInput(inputId = "okrug_11", label = "11 округ", value = T)
   )),
  
  mainPanel(
    
    conditionalPanel(condition = "input.turnout_plot",
                     br(),
                     plotOutput('turnoutPlot', height = '200px')
                     ),
 
    conditionalPanel(condition = "input.props_plot",
                     br(),
                     plotOutput('propsPlot', width = '800px', height = '600px')
    ),
    
    conditionalPanel(condition = "input.tbl_turn",
                     br(),
                     p(strong("Явка")),
                     div(tableOutput(outputId = "tbl_turnpropt"))),
    
    conditionalPanel(condition = "input.okrug_psk",
                     br(),
                     p(strong("Псковский район")),
                     div(tableOutput(outputId = "tbl_psk"))),
    conditionalPanel(condition = "input.okrug_nvl",
                     br(),
                     p(strong("Невельский район")),
                     div(tableOutput(outputId = "tbl_nvl"))),
    conditionalPanel(condition = "input.okrug_lkn",
                     br(),
                     p(strong("Локнянский район")),
                     div(tableOutput(outputId = "tbl_lkn"))),
    conditionalPanel(condition = "input.okrug_11",
                     br(),
                     p(strong("11 округ")),
                     div(tableOutput(outputId = "tbl_11")))
  )
))
