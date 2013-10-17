library(shiny)

source('functions.r')

shinyServer(function(input, output, session) {
# shinyServer(function(input, output) {
 autoInvalidate <- reactiveTimer(60 * 10^4, session)
  
 data <- reactive({
   autoInvalidate()
   okrugs <- loadOkrugs()
   data <- loadData()
   parties <- extractParties(data)
   data <- extractData(data)
   props <- getProps(data)
   require(plyr)
   props <- join(props, parties)
   turnout <- getTurnout(data)
   list(data=data, props=props, turnout=turnout)
 })
 
 output$turnoutPlot <- renderPlot(print(plotTurnout(data()$turnout)))
 
 output$propsPlot <- renderPlot(print(arrangePlots(data()$props, input$props_plot_type, T)))
 
 
 # функция по созданию таблицы c долями голосов. 
 mk_tbl <- function(props, okrug) {
   t <- props[props$okrug==okrug, 2:5]
   t$variable <- factor(t$variable)
   lvls <- levels(t$variable)
   ref_ind <- seq_len(length(lvls))[lvls=='отказы']
   # переставляем отказы вниз
   t <- t[c(1:(ref_ind - 1),
            (ref_ind + 1):length(lvls),
            ref_ind),]
   names(t) <- c("Кандидант", "Голосов", "% с отказами",
                 "% без отказов")
   t
 }

 renderTableOpts <- function(...) {
   renderTable(..., include.rownames=F, digits=c(0, 0, 0, 1, 1))
 }
 
 output$tbl_psk <- renderTableOpts({mk_tbl(data()$props, "Псковский")})
 output$tbl_nvl <- renderTableOpts({mk_tbl(data()$props, "Невельский")})
 output$tbl_lkn <- renderTableOpts({mk_tbl(data()$props, "Локнянский")})
 output$tbl_11  <- renderTableOpts({mk_tbl(data()$props, "11 округ")})


# Создание таблицы с явкой
output$tbl_turnpropt <-renderTable({
  t <- ddply(data()$turnout, .(okrug), function(x) {
    round(100* sum(x$turnprop), 1)} )
  colnames(t) <- c("Округ", "Явка, %")
  t
             
}, include.rownames=F, digits=c(0, 0, 1))
 
})
