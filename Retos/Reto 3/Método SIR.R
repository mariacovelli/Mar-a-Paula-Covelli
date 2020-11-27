#Método SIR
library(shiny)
library (shinydashboard) 
library(shinyjs)
library(PolynomF)
library(deSolve)
library(phaseR)

datos<-c(4355584,3617355,3645939,3898308,3170414,3069859,3118128,3071649,2733622,2761138,3705369,4210146,3655672,4344529,4380496,3567492,3627090,4327999,4343049,4550122,4539056,4125452,3447602,3493095,4130045,4212180,4228805,4195093,4318378,3808534,3954443)
dias<-seq(1, length(datos), by = 1)
total<-c(datos[1])
for(i in 1:(length(datos)-1)){
  total <- c(total, total[i] + datos[i+1])}
total <- total/1000000
p <- 0
pr <- 0
cont <- 0
for(i in 1:(length(total)-1)){
  p <- (total[i+1]-total[i]) + p
  cont <- cont + 1}
p<- p/cont
p <- p/100
p
ui <- dashboardPage(dashboardHeader (), dashboardSidebar (), dashboardBody () )
header <- dashboardHeader(title = "Simulación Proyecto Análisis Numérico")
sidebar <- dashboardSidebar(sidebarMenu(menuItem("SIR", tabName = "SIR", icon = icon("calculator") )))
frow1 <- fluidRow(valueBoxOutput("value1"),valueBoxOutput("value2"),valueBoxOutput("value3"))
TabSIR <- fluidRow( box(title = "SIR", width = 8,shinyjs::useShinyjs(),
    sliderInput("AtacadosSIR", "Atacados:", 1, 1000, 1, step = 1),
    sliderInput("susceptiblesSIR", "Susceptibles:", 1, 1000, 1, step = 1),
    sliderInput("diasSIR", "Dias:", 1, 31, 1, step = 1),
    sliderInput("bethaSIR", "Lambda:", 0.1000, 0.2000, 0.1, step = 0.0001),
    sliderInput("gammaSIR", "Miu:", 0.2500, 0.3500, 0.2800, step = 0.0001),
    actionButton("botonCalcularSIR", "Calcular")),
tabBox(title = "Metodos",width = 8,tabPanel(" ",width = 400,#método adams
      plotOutput("plot2SIR", height = 380),
      plotOutput("plot2ErrorSIR", height = 400, width = 400),
      dataTableOutput("tablaErrorA2")),
tabPanel("RK4",width = 400,
      plotOutput("plot1SIR", height = 380),
      plotOutput("plot1ErrorSIR", height = 400, width = 400),
      dataTableOutput("tablaErrorR2"))))
body <- dashboardBody(tabItems(tabItem("SIR",TabSIR)))
ui <- dashboardPage(title = 'ProyectoAN', header, sidebar, body, skin='blue')
server <- function(input, output, session) {
CalcularSIR <- function(){
  output$plot1SIR <- renderPlot({
    N = 1
    init <- c(S = input$suceptiblesSIR,I = input$AtacadosSIR,R = 0)
    param <- c(beta = input$bethaSIR/100,gamma = input$gammaSIR)
    sir <- function(times, init, param) {
      with(as.list(c(init, param)), {
        dS <- -beta * S * I 
        dI <-  beta * S * I - gamma * I
        dR <-gamma * I
        return(list(c(dS, dI, dR)))})}
    times <- seq(0,input$diasSIR, by = 1)
    out <- ode(y = init, times = times, func = sir, parms = param, method = "rk4")
    out <- as.data.frame(out*N)
    attach(out)
    plot(out$time, out$S, type="l", col="red", ylim=c(0,800), xlab="Dias", ylab="Atacados",main = "Runge Kutta 4")
    par(new=T)
    plot(out$time, out$I ,col="blue", type="l", xlab="Dias", ylab="Atacados", ylim = c(0,800))
    par(new=T)
    plot(out$time, out$R ,type = "l", col="black", xlab="Dias", ylab="Atacados", ylim = c(0, 800))
    legend(x = "topright", legend=c("Susceptibles", "Atacados", "Recuperados"), col=c("red", "blue", "black"), lty=rep(1, 2))})
    output$plot1ErrorSIR <- renderPlot({
    N = 1
    init <- c(S = input$susceptiblesSIR,I = input$AtacadosSIR,R = 0)
    param <- c(beta = input$bethaSIR,gamma = input$gammaSIR)
    sir <- function(times, init, param) {
      with(as.list(c(init, param)),{
        dS <- -beta * S * I 
        dI <-  beta * S * I - gamma * I
        dR <-gamma * I
        return(list(c(dS, dI, dR)))})}
    times <- seq(0,input$diasSIR, by = 1)
    out <- ode(y = init, times = times, func = sir, parms = param, method = "lsoda")
    out2 <- ode(y = init, times = times, func = sir, parms = param, method = "adams")
    out <- as.data.frame(out*N)
    out2 <- as.data.frame(out2*N)
    attach(out)
    attach(out2)
    error <- c()
    i <- 1
    for(i in seq(1,input$diasSIR+1, by = 1)){
      error <- c(error, (abs(out$I[i] - out2$I[i]))/out$I[i])}
    plot(times, error, col = "black", lwd = "2", type = "l", xlab = "Dias", ylab = "Error relativo", main = "Error", ylim = c(0, 10))
  })
  output$tablaErrorR2 <- renderDataTable({
    N = 1
    init <- c(S = input$susceptiblesSIR,I = input$AtacadosSIR,R = 0)
    param <- c(beta = input$bethaSIR,gamma = input$gammaSIR)
    sir <- function(times, init, param) {
      with(as.list(c(init, param)), {
        dS <- -beta * S * I 
        dI <-beta * S * I - gamma * I
        dR <-gamma * I
        return(list(c(dS, dI, dR)))})}
    times <- seq(0,input$diasSIR, by = 1)
    out <- ode(y = init, times = times, func = sir, parms = param, method = "lsoda")
    out2 <- ode(y = init, times = times, func = sir, parms = param, method = "adams")
    out <- as.data.frame(out*N)
    out2 <- as.data.frame(out2*N)
    attach(out)
    attach(out2)
    error <- c()
    i <- 1
    for(i in seq(1,input$diasSIR+1, by = 1)){
    error <- c(error, (abs(out$I[i] - out2$I[i]))/out$I[i])}
    valores <- data.frame("Dia" = (1:(input$diasSIR + 1 )),"Real" = round(out$I, 4),"Aproximado" = round(out2$I, 4),"Error" = round(error,4))})
  output$plot2SIR <- renderPlot({
    N = 1
    init <- c(S = input$susceptiblesSIR,I = input$AtacadosSIR,R = 0)
    param <- c(beta = input$bethaSIR/100,gamma = input$gammaSIR)
    sir <- function(times, init, param) {
      with(as.list(c(init, param)), {
        dS <- -beta * S * I 
        dI <-  beta * S * I - gamma * I
        dR <-gamma * I
        return(list(c(dS, dI, dR)))
      })
    }
    times <- seq(0,input$diasSIR, by = 1)
    out <- ode(y = init, times = times, func = sir, parms = param, method = "adams")
    out <- as.data.frame(out*N)
    attach(out)
    plot(out$time, out$S, type="l", col="red", ylim=c(0,800), xlab="Dias", ylab="Atacados",main = " ")
    par(new=T)
    plot(out$time, out$I ,col="black", type="l", xlab="Dias", ylab="Atacados", ylim = c(0,800))
    par(new=T)
    plot(out$time, out$R ,type = "l", col="purple", xlab="Dias", ylab="Atacados", ylim = c(0, 800))
    legend(x = "topright", legend=c("Susceptibles", "Atacados", "Recuperados"), col=c("red", "black", "purple"), lty=rep(1, 2))
  })
    output$plot2ErrorSIR <- renderPlot({
    N = 1
    init <- c(S = input$susceptiblesSIR,I = input$AtacadosSIR,R = 0)
    param <- c(beta = input$bethaSIR, gamma = input$gammaSIR)
    sir <- function(times, init, param) {
      with(as.list(c(init, param)), {
        dS <- -beta * S * I 
        dI <-  beta * S * I - gamma * I
        dR <-gamma * I
        return(list(c(dS, dI, dR)))
      })
    }
    times <- seq(0,input$diasSIR, by = 1)
    out <- ode(y = init, times = times, func = sir, parms = param, method = "adams")
    out <- as.data.frame(out*N)
    attach(out)
    error <- c()
    i <- 1
    for(i in seq(1,input$diasSIR+1, by = 1)){
      error <- c(error, (abs(total[i] - out$I[i]))/total[i])
    }
    plot(times, error, col = "black", lwd = "2", type = "l", xlab = "Dias", ylab = "Error relativo", main = "Error", ylim = c(0, 10))
  })
    output$tablaErrorA2 <- renderDataTable({
    N = 1
    init <- c(S = input$susceptiblesSIR,I = input$AtacadosSIR,R = 0)
    param <- c(beta = input$bethaSIR,gamma = input$gammaSIR)
    sir <- function(times, init, param) {
      with(as.list(c(init, param)), {
        dS <- -beta * S * I 
        dI <-  beta * S * I - gamma * I
        dR <-gamma * I
        return(list(c(dS, dI, dR)))
      })
    }
    times <- seq(0,input$diasSIR, by = 1)
    out <- ode(y = init, times = times, func = sir, parms = param, method = "adams")
    out2 <- ode(y = init, times = times, func = sir, parms = param, method = "lsoda")
    out <- as.data.frame(out*N)
    out2 <- as.data.frame(out2*N)
    attach(out)
    attach(out2)
    error <- c()
    i <- 1
    for(i in seq(1,input$diasSIR+1, by = 1)){
      error <- c(error, (abs(out$I[i] - out2$I[i]))/out$I[i])
    }
valores <- data.frame("Dia" = (1:(input$diasSIR + 1 )),"Real" = round(out$I, 4),"Aproximado" = round(out2$I, 4),"Error" = round(error,4))})}
observeEvent(input$botonCalcularSIR, {CalcularSIR()})}
shinyApp(ui, server)

