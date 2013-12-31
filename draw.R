setupPlot <- function (size) {
    # Create plot area
    M <- c(-1,1) * size
    plot (M, M, pch='', bty='n', axes=T, xlab='', ylab='')
}

drawLens <- function (R,d,u,v) {
    # Draw circle (R, u, v)
    t <- asin (d / (2*R)) # Half-angle of lens arc
    p <- seq (pi-t, pi+t, length.out=50)
    xx <- u + R*cos(p)
    yy <- v + R*sin(p)
    lines (xx, yy, col='grey40')
}

drawNormal <- function (R,u,v, t) {
    # Draw the surface normals
    n <- 2*sign(R)
    lines (u+cos(t)*R*c(1,n), v+sin(t)*R*c(1,n), col='grey', lwd=1, lty=3)
}

drawInbound <- function(x,y, R,u,v,t,col='red') {
    # Inbound rays
    lines (c(x,u+R*cos(t)), c(y,v+R*sin(t)), col=col, lwd=1)
}

drawOutbound <- function (R,u,v,t, b) {
    lines (u+R*cos(t) + c(0, 40*cos(b)), 
           v+R*sin(t) + c(0, 40*sin(b)), col='blue')
}