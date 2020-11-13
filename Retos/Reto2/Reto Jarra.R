library(gridBezier)
library(grid)

#Primer nivel
c11 = BezierGrob(c(-0.05,0.25,0.48,0.54),c(-0.55,-0.49,-0.27,-0.06),
               stepFn=function(...) seq(0,1, length.out=1000))
pts11 <- BezierPoints(c11)
plot(pts11$x,pts11$y,type = "l",col="red", xlim = c (-20,20), ylim= c (-20,20))
points(pts11$x,pts11$y,col="red")

c12 = BezierGrob(c(0.54,0.48,0.26,-0.05),c(-0.06,0.25,0.47,0.53),
                stepFn=function(...) seq(0,1, length.out=1000))
par(new=T)
pts12 <- BezierPoints(c12)
plot(pts12$x,pts12$y,type = "l",col="red", xlim = c (-20,20), ylim= c (-20,20))
points(pts12$x,pts12$y,col="red")

c13 = BezierGrob(c(-0.05,-0.34,-0.52,-0.54),c(0.53,0.41,0.15,-0.06),
               stepFn=function(...) seq(0,1, length.out=1000))
par(new=T)
pts13 <- BezierPoints(c13)
plot(pts13$x,pts13$y,type = "l",col="red", xlim = c (-20,20), ylim= c (-20,20))
points(pts13$x,pts13$y,col="red")

c14 = BezierGrob(c(-0.54,-0.48,-0.26,-0.05),c(-0.06,-0.26,-0.49,-0.55),
                stepFn=function(...) seq(0,1, length.out=1000))
par(new=T)
pts14 <- BezierPoints(c14)
plot(pts14$x,pts14$y,type = "l",col="red", xlim = c (-20,20), ylim= c (-20,20))
points(pts14$x,pts14$y,col="red")
#Definir Altura

#Segundo nivel
par(new=T)
c21 = BezierGrob(c(0,0.34,0.65,0.73),c(-0.74,-0.66,-0.36,-0.01),
                 stepFn=function(...) seq(0,1, length.out=1000))
pts21 <- BezierPoints(c21)
plot(pts21$x,pts21$y,type = "l",col="blue", xlim = c (-20,20), ylim= c (-20,20))
points(pts21$x,pts21$y,col="blue")

c22 = BezierGrob(c(0.73,0.65,0.35,0),c(-0.01,0.34,0.64,0.72),
                 stepFn=function(...) seq(0,1, length.out=1000))
par(new=T)
pts22 <- BezierPoints(c22)
plot(pts22$x,pts22$y,type = "l",col="blue", xlim = c (-20,20), ylim= c (-20,20))
points(pts22$x,pts22$y,col="blue")

c23 = BezierGrob(c(0,-0.35,-0.65,-0.73),c(0.72,0.64,0.34,-0.01),
                 stepFn=function(...) seq(0,1, length.out=1000))
par(new=T)
pts23 <- BezierPoints(c23)
plot(pts23$x,pts23$y,type = "l",col="blue", xlim = c (-20,20), ylim= c (-20,20))
points(pts23$x,pts23$y,col="blue")

c24 = BezierGrob(c(-0.73,-0.65,-0.35,0),c(-0.01,-0.35,-0.66,-0.74),
                 stepFn=function(...) seq(0,1, length.out=1000))
par(new=T)
pts24 <- BezierPoints(c24)
plot(pts24$x,pts24$y,type = "l",col="blue", xlim = c (-20,20), ylim= c (-20,20))
points(pts24$x,pts24$y,col="blue")
#Definir Altura

#Tercer nivel
par(new=T)
c31 = BezierGrob(c(0,0.42,0.97,1.1),c(-1.11,-1.02,-0.53,-0.01),
                 stepFn=function(...) seq(0,1, length.out=1000))
pts31 <- BezierPoints(c31)
plot(pts31$x,pts31$y,type = "l",col="purple", xlim = c (-20,20), ylim= c (-20,20))
points(pts31$x,pts31$y,col="purple")

c32 = BezierGrob(c(1.1,0.97,0.52,0),c(-0.01,0.51,0.96,1.09),
                 stepFn=function(...) seq(0,1, length.out=1000))
par(new=T)
pts32 <- BezierPoints(c32)
plot(pts32$x,pts32$y,type = "l",col="purple", xlim = c (-20,20), ylim= c (-20,20))
points(pts32$x,pts32$y,col="purple")

c33 = BezierGrob(c(0,-0.52,-0.97,-1.1),c(1.09,0.96,0.51,-0.01),
                 stepFn=function(...) seq(0,1, length.out=1000))
par(new=T)
pts33 <- BezierPoints(c33)
plot(pts33$x,pts33$y,type = "l",col="purple", xlim = c (-20,20), ylim= c (-20,20))
points(pts33$x,pts33$y,col="purple")

c34 = BezierGrob(c(-1.1,-0.97,-0.52,0),c(-0.01,-0.53,-0.98,-1.11),
                 stepFn=function(...) seq(0,1, length.out=1000))
par(new=T)
pts34 <- BezierPoints(c34)
plot(pts34$x,pts34$y,type = "l",col="purple", xlim = c (-20,20), ylim= c (-20,20))
points(pts34$x,pts34$y,col="purple")

#Cuarto nivel
par(new=T)
c41 = BezierGrob(c(0,0.58,1.09,1.24),c(-1.25,-1.1,-0.6,-0.01),
                 stepFn=function(...) seq(0,1, length.out=1000))
pts41 <- BezierPoints(c41)
plot(pts41$x,pts41$y,type = "l",col="black", xlim = c (-20,20), ylim= c (-20,20))
points(pts41$x,pts41$y,col="black")

c42 = BezierGrob(c(1.24,1.09,0.59,0),c(-0.01,0.58,1.09,1.23),
                 stepFn=function(...) seq(0,1, length.out=1000))
par(new=T)
pts42 <- BezierPoints(c42)
plot(pts42$x,pts42$y,type = "l",col="black", xlim = c (-20,20), ylim= c (-20,20))
points(pts42$x,pts42$y,col="black")

c43 = BezierGrob(c(0,-0.59,-1.1,-1.24),c(1.23,1.09,0.58,-0.01),
                 stepFn=function(...) seq(0,1, length.out=1000))
par(new=T)
pts43 <- BezierPoints(c43)
plot(pts43$x,pts43$y,type = "l",col="black", xlim = c (-20,20), ylim= c (-20,20))
points(pts43$x,pts43$y,col="black")

c44 = BezierGrob(c(-1.24,-1.1,-0.59,0),c(-0.01,-0.59,-1.1,-1.25),
                 stepFn=function(...) seq(0,1, length.out=1000))
par(new=T)
pts44 <- BezierPoints(c44)
plot(pts44$x,pts44$y,type = "l",col="black", xlim = c (-20,20), ylim= c (-20,20))
points(pts44$x,pts44$y,col="black")
#Quinto nivel
par(new=T)
c51 = BezierGrob(c(0,0.58,1.09,1.23),c(-1.24,-1.1,-0.59,-0.01),
                 stepFn=function(...) seq(0,1, length.out=1000))
pts51 <- BezierPoints(c51)
plot(pts51$x,pts51$y,type = "l",col="green", xlim = c (-20,20), ylim= c (-20,20))
points(pts51$x,pts51$y,col="green")

c52 = BezierGrob(c(1.23,1.09,0.58,0),c(-0.01,0.57,1.08,1.22),
                 stepFn=function(...) seq(0,1, length.out=1000))
par(new=T)
pts52 <- BezierPoints(c52)
plot(pts52$x,pts52$y,type = "l",col="green", xlim = c (-20,20), ylim= c (-20,20))
points(pts52$x,pts52$y,col="green")

c53 = BezierGrob(c(0,-0.58,-1.09,-1.23),c(1.22,1.08,0.57,-0.01),
                 stepFn=function(...) seq(0,1, length.out=1000))
par(new=T)
pts53 <- BezierPoints(c53)
plot(pts53$x,pts53$y,type = "l",col="green", xlim = c (-20,20), ylim= c (-20,20))
points(pts53$x,pts53$y,col="green")

c54 = BezierGrob(c(-1.23,-1.09,-0.58,0),c(-0.01,-0.59,-1.1,-1.24),
                 stepFn=function(...) seq(0,1, length.out=1000))
par(new=T)
pts54 <- BezierPoints(c54)
plot(pts54$x,pts54$y,type = "l",col="green", xlim = c (-20,20), ylim= c (-20,20))
points(pts54$x,pts54$y,col="green")
#Sexto nivel 
par(new=T)
c61 = BezierGrob(c(0,0.42,0.78,0.89),c(-0.9,-0.79,-0.43,-0.01),
                 stepFn=function(...) seq(0,1, length.out=1000))
pts61 <- BezierPoints(c61)
plot(pts61$x,pts61$y,type = "l",col="yellow", xlim = c (-20,20), ylim= c (-20,20))
points(pts61$x,pts61$y,col="yellow")

c62 = BezierGrob(c(0.89,0.78,0.42,0),c(-0.01,0.41,0.77,0.88),
                 stepFn=function(...) seq(0,1, length.out=1000))
par(new=T)
pts62 <- BezierPoints(c62)
plot(pts62$x,pts62$y,type = "l",col="yellow", xlim = c (-20,20), ylim= c (-20,20))
points(pts62$x,pts62$y,col="yellow")

c63 = BezierGrob(c(0,-0.42,-0.78,-0.89),c(0.88,0.77,0.41,-0.01),
                 stepFn=function(...) seq(0,1, length.out=1000))
par(new=T)
pts63 <- BezierPoints(c63)
plot(pts63$x,pts63$y,type = "l",col="yellow", xlim = c (-20,20), ylim= c (-20,20))
points(pts63$x,pts63$y,col="yellow")

c64 = BezierGrob(c(-0.89,-0.78,-0.42,0),c(-0.01,-0.43,-0.79,-0.9),
                 stepFn=function(...) seq(0,1, length.out=1000))
par(new=T)
pts64 <- BezierPoints(c64)
plot(pts64$x,pts64$y,type = "l",col="yellow", xlim = c (-20,20), ylim= c (-20,20))
points(pts64$x,pts64$y,col="yellow")

#Septimo nivel 
par(new=T)
c71 = BezierGrob(c(0,0.32,0.59,0.67),c(-0.68,-0.6,-0.33,-0.01),
                 stepFn=function(...) seq(0,1, length.out=1000))
pts71 <- BezierPoints(c71)
plot(pts71$x,pts71$y,type = "l",col="orange", xlim = c (-20,20), ylim= c (-20,20))
points(pts71$x,pts71$y,col="orange")

c72 = BezierGrob(c(0.67,0.59,0.32,0),c(-0.01,0.31,0.58,0.66),
                 stepFn=function(...) seq(0,1, length.out=1000))
par(new=T)
pts72 <- BezierPoints(c72)
plot(pts72$x,pts72$y,type = "l",col="orange", xlim = c (-20,20), ylim= c (-20,20))
points(pts72$x,pts72$y,col="orange")

c73 = BezierGrob(c(0,-0.32,-0.59,-0.66),c(0.66,0.58,0.31,-0.01),
                 stepFn=function(...) seq(0,1, length.out=1000))
par(new=T)
pts73 <- BezierPoints(c73)
plot(pts73$x,pts73$y,type = "l",col="orange", xlim = c (-20,20), ylim= c (-20,20))
points(pts73$x,pts73$y,col="orange")

c74 = BezierGrob(c(-0.66,-0.59,-0.32,0),c(-0.01,-0.33,-0.6,-0.68),
                 stepFn=function(...) seq(0,1, length.out=1000))
par(new=T)
pts74 <- BezierPoints(c74)
plot(pts74$x,pts74$y,type = "l",col="orange", xlim = c (-20,20), ylim= c (-20,20))
points(pts74$x,pts74$y,col="orange")

#Octavo nivel
par(new=T)
c81 = BezierGrob(c(0,0.43,0.81,0.92),c(-0.93,-0.82,-0.44,-0.01),
                 stepFn=function(...) seq(0,1, length.out=1000))
pts81 <- BezierPoints(c81)
plot(pts81$x,pts81$y,type = "l",col="cyan", xlim = c (-20,20), ylim= c (-20,20))
points(pts81$x,pts81$y,col="cyan")

c82 = BezierGrob(c(0.92,0.81,0.43,0),c(-0.01,0.42,0.8,0.91),
                 stepFn=function(...) seq(0,1, length.out=1000))
par(new=T)
pts82 <- BezierPoints(c82)
plot(pts82$x,pts82$y,type = "l",col="cyan", xlim = c (-20,20), ylim= c (-20,20))
points(pts82$x,pts82$y,col="cyan")

c83 = BezierGrob(c(0,-0.43,-0.81,-0.92),c(0.91,0.8,0.42,-0.01),
                 stepFn=function(...) seq(0,1, length.out=1000))
par(new=T)
pts83 <- BezierPoints(c83)
plot(pts83$x,pts83$y,type = "l",col="cyan", xlim = c (-20,20), ylim= c (-20,20))
points(pts83$x,pts83$y,col="cyan")

c84 = BezierGrob(c(-0.92,-0.81,-0.43,0),c(-0.01,-0.44,-0.82,-0.93),
                 stepFn=function(...) seq(0,1, length.out=1000))
par(new=T)
pts84 <- BezierPoints(c84)
plot(pts84$x,pts84$y,type = "l",col="cyan", xlim = c (-20,20), ylim= c (-20,20))
points(pts84$x,pts84$y,col="cyan")




