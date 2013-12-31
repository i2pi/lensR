source('rayFunction.R')

x <- -15
y <- 0

# TODO: Do this to take into account diameter of lens
D <- 20
a <- seq (-pi/D, pi/D, length.out=15) + atan((y-v)/(x-u))

setupPlot (10)

ng <- 1.8

for (i in a) {
    out <- traceRay (x,y,i,                       -3.1,5, -6,0, ng, 1.0)
    #out <- traceRay (out$X[1],out$Y[1],out$B[1], -4,5,-6,0,  ng,1.0)
    #out <- traceRay (out$X[1],out$Y[1],out$B[1],  40,10, 44,0, 1.8,1)
}
