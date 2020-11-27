#Método SI
library(shiny)
library (shinydashboard) 
library(shinyjs)
library(PolynomF)
library(deSolve)
library(phaseR)

datos<-c(4355584,3617355,3645939,3898308,3170414,3069859,3118128,3071649,2733622,2761138,3705369,4210146,3655672,4344529, 4380496,3567492,3627090,4327999,4343049,4550122,4539056,4125452,3447602,3493095,4130045,4212180,4228805,4195093,4318378,3808534,3954443)
dias<-seq(1, length(datos), by = 1)
total<-c(datos[1])
for(i in 1:(length(datos)-1)){
total <- c(total, total[i] + datos[i+1])
}
total <- total/1000000
p <- 0
pr <- 0
cont <- 0
for(i in 1:(length(total)-1)){
p <- (total[i+1]-total[i]) + p
cont <- cont + 1
}
p<- p/cont
p <- p/100
p
ui <- dashboardPage(dashboardHeader (), dashboardSidebar (), dashboardBody () )
header <- dashboardHeader(title = "Simulación Proyecto Análisis Numérico")
sidebar <- dashboardSidebar(  sidebarMenu(menuItem("SI", tabName = "SI", icon = icon("calculator"))  ))
frow1 <- fluidRow(valueBoxOutput("value1"),valueBoxOutput("value2"),valueBoxOutput("value3"))

TabSI <- fluidRow( 
box(title = "SI", width = 8,shinyjs::useShinyjs(),
sliderInput("AtacadosSI", "Atacados:", 1, 1000, 1, step = 1),
sliderInput("suceptiblesSI", "Suceptibles:", 1, 1000, 1, step = 1),
sliderInput("diasSI", "Dias:", 1, 31, 1, step = 1),
sliderInput("bethaSI", "Lambda:", 0.1000, 0.2000, round(p, 4), step = 0.0001),
actionButton("botonCalcularSI", "Calcular")),
tabBox(title = "Opciones",width = 8,tabPanel(" ",width = 400,#Adams
plotOutput("plot2SI", height = 380),
plotOutput("plot2ErrorSI", height = 400, width = 400),
dataTableOutput("tablaErrorA")),
tabPanel("RK4",
width = 400,
plotOutput("plot1SI", height = 380),
plotOutput("plot1ErrorSI", height = 400, width = 400),
dataTableOutput("tablaErrorR"))))
body <- dashboardBody(tabItems(tabItem("SI",frow1, TabSI)))
ui <- dashboardPage(title = 'ProyectoAN', header, sidebar, body, skin='blue')
server <- function(input, output, session) {
CalcularSI <- function(){
output$plot1SI <- renderPlot({ 
N = 1
init <- c(S = input$suceptiblesSI,
I = input$AtacadosSI)
param <- c(beta = input$bethaSI,gamma = input$gammaSI)
si <- function(times, init, param) {
with(as.list(c(init, param)), {
dS <- -beta * S * I / (S + I)
dI <-  beta * S * I / (S + I)
 return(list(c(dS, dI)))})}
times <- seq(0,input$diasSI, by = 1)
out <- ode(y = init, times = times, func = si, parms = param, method = "rk4")
out <- as.data.frame(out*N)
attach(out)
plot(dias, total, pch = 19, ylim = c(0, 800), xlim = c(0, input$diasSI),cex=1, col = "red")
splineAjuste=splinefun(dias, total)
curve(splineAjuste,add=T,lty=1,lwd=2, ylim = c(0, 800), xlim = c(0, input$diasSI), col= "red")
par(new=T)
plot(out$time, out$S, type="l", col="black", ylim=c(0,800), xlab="Dias", ylab="Atacados",main = "Runge Kutta 4")
par(new=T)
plot(out$time, out$I ,col="blue", type="l", xlab="Dias", ylab="Atacados", ylim = c(0,800))
legend(x = "topright", legend=c("Susceptibles", "Atacados aproximado", "infectados real"), col=c("black", "blue", "red"), lty=rep(1, 2))  })
 output$plot1ErrorSI <- renderPlot({
N = 1
init <- c(S = input$suceptiblesSI,
I = input$AtacadosSI)
param <- c(beta = input$bethaSI,
gamma = input$gammaSI)
si <- function(times, init, param) {
with(as.list(c(init, param)), {
dS <- -beta * S * I / (S + I)
dI <-  beta * S * I / (S + I)
return(list(c(dS, dI))) })}
times <- seq(0,input$diasSI, by = 1)
out <- ode(y = init, times = times, func = si, parms = param, method = "rk4")
out <- as.data.frame(out*N)
attach(out)
error <- c()
i <- 1
for(i in seq(1,input$diasSI+1, by = 1)){
error <- c(error, (abs(total[i] - out$I[i]))/total[i])}
plot(times, error, col = "black", lwd = "2",type = "l", xlab = "Dias", ylab = "Error relativo", main = "Error", ylim = c(0, 10))})
output$tablaErrorR <- renderDataTable({
N = 1
init <- c(S = input$suceptiblesSI,
I = input$AtacadosSI)
param <- c(beta = input$bethaSI,
gamma = input$gammaSI)
si <- function(times, init, param) {
with(as.list(c(init, param)), {
dS <- -beta * S * I / (S + I)
dI <-  beta * S * I / (S + I)
return(list(c(dS, dI)))})}
times <- seq(0,input$diasSI, by = 1)
out <- ode(y = init, times = times, func = si, parms = param, method = "rk4")
out <- as.data.frame(out*N)
attach(out)
error <- c()
i <- 1
for(i in seq(1,input$diasSI+1, by = 1)){
error <- c(error, (abs(total[i] - out$I[i]))/total[i])}
valores <- data.frame("Dia" = (1:(input$diasSI + 1 )),"Real" = round(total, 4),"Aproximado" = round(out$I, 4),"Error" = round(error,4))})
output$plot2SI <- renderPlot({
N = 1
init <- c(S = input$suceptiblesSI,
I = input$AtacadosSI)
param <- c(beta = input$bethaSI,
gamma = input$gammaSI)
si <- function(times, init, param) {
with(as.list(c(init, param)), {
dS <- -beta * S * I / (S + I)
dI <-  beta * S * I / (S + I)
return(list(c(dS, dI))) }) }
times <- seq(0,input$diasSI, by = 1)
out <- ode(y = init, times = times, func = si, parms = param, method = "adams")
out <- as.data.frame(out*N)
attach(out)
plot(dias, total, pch = 19, ylim = c(0, 800), xlim = c(0, input$diasSI),cex=1, col = "white")
splineAjuste=splinefun(dias, total)
curve(splineAjuste,add=T,lty=1,lwd=2, ylim = c(0, 800), xlim = c(0, input$diasSI), col= "red")
par(new=T)
plot(out$time, out$S, type="l", col="black", ylim=c(0,800), xlab="Dias", ylab="Atacados",main = " ")
par(new=T)
plot(out$time, out$I ,col="blue", type="l", xlab="Dias", ylab="Atacados", ylim = c(0,800))
legend(x = "topright", legend=c("Susceptibles", "Atacados"), col=c("black", "blue"), lty=rep(1, 2)) })
output$plot2ErrorSI <- renderPlot({
N = 1
init <- c(S = input$suceptiblesSI,I = input$AtacadosSI)
param <- c(beta = input$bethaSI,gamma = input$gammaSI)
si <- function(times, init, param) {
with(as.list(c(init, param)), {
dS <- -beta * S * I / (S + I)
dI <-  beta * S * I / (S + I)
return(list(c(dS, dI)))})}
times <- seq(0,input$diasSI, by = 1)
out <- ode(y = init, times = times, func = si, parms = param, method = "adams")
out <- as.data.frame(out*N)
attach(out)
error <- c()
i <- 1
for(i in seq(1,input$diasSI+1, by = 1)){
error <- c(error, (abs(total[i] - out$I[i]))/total[i])}
plot(times, error, col = "black", lwd = "2", type = "l", xlab = "Dias", ylab = "Error relativo", main = "Error", ylim = c(0, 10))})
output$tablaErrorA <- renderDataTable({
N = 1
init <- c(S = input$suceptiblesSI,I = input$AtacadosSI)
param <- c(beta = input$bethaSI,gamma = input$gammaSI)
si <- function(times, init, param) {
with(as.list(c(init, param)), {
dS <- -beta * S * I / (S + I)
dI <-  beta * S * I / (S + I)
return(list(c(dS, dI)))})}
times <- seq(0,input$diasSI, by = 1)
out <- ode(y = init, times = times, func = si, parms = param, method = "adams")
out <- as.data.frame(out*N)
attach(out)
error <- c()
i <- 1
for(i in seq(1,input$diasSI+1, by = 1)){
error <- c(error, (abs(total[i] - out$I[i]))/total[i])}
valores <- data.frame("Dia" = (1:(input$diasSI + 1 )),"Real" = round(total, 4),"Aproximado" = round(out$I, 4),"Error" = round(error,4))})}
observeEvent(input$botonCalcularSI, {CalcularSI()})  }
shinyApp(ui, server)
  









