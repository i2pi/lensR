drawLens <- function (R,d,u,v, draw.circle=F) {
    # Draw circle (R, u, v)
    t <- asin (d / (2*R)) # Half-angle of lens arc
    p <- seq (pi-t, pi+t, length.out=50)
    xx <- u + R*cos(p)
    yy <- v + R*sin(p)
    lines (xx, yy, col='grey40', lwd=0.1)
    
    if (draw.circle) {
        p <- seq (0, 2*pi, length.out=50)
        xx <- u + R*cos(p)
        yy <- v + R*sin(p)
        lines (xx, yy, col='grey40', lwd=0.1)    
    }
    
}

drawNormal <- function (R,u,v, t) {
    # Draw the surface normals
    n <- 2*sign(R)
    lines (u+cos(t)*R*c(1,n), v+sin(t)*R*c(1,n), col='grey', lwd=1, lty=3)
}

drawInbound <- function(x,y, R,u,v,t,col='pink') {
    # Inbound rays
    lines (c(x,u+R*cos(t)), c(y,v+R*sin(t)), col=col, lwd=1)
}

drawOutbound <- function (R,u,v,t, b) {
    lines (u+R*cos(t) + c(0, 400*cos(b)), 
           v+R*sin(t) + c(0, 400*sin(b)), lwd=0.5, col='red')
}
traceRay <- function (x,y,a,  R,d,u,v, n1,n2, draw.out=F) {
    u <- u + R
    
    drawLens (R,d,u,v)    
    
    HT <- asin (d / (2*abs(R))) # Half-angle of lens arc
    if (R<0) HT <- HT + pi
    
    # TODO: Iterate instead of having a huge p[]
    # We know the rays are oming from the left, so let's
    #  find the angles of intersection, only considering the left
    p <- seq (-pi/2, pi/2, length.out=10000) + pi # coming from the left, so +pi
    xx <- u + R*cos(p)
    yy <- v + R*sin(p)
    
    dx <- xx - x
    dy <- yy - y
    w <- atan2(dy, dx)
    
    X <- a
    Y <- a
    B <- a
    
    for (j in 1:length(a)) {
        # Find the intersection of the ray with the circle
        i <- a[j]
        t <- (w - i) ^ 2
        t <- p[order(t)[1:2]]
        # Can cross twice, so, pick the one closest to pi (facing left)
        if (abs(t[1] - pi) < abs(t[2] - pi)) {
            t <- t[1]
        } else {
            t <- t[2]
        }
        # t = the angle within the circle of the intersecting point
            
        # check whether the ray actually hits the lens within the diameter
        if (R > 0) {
             if ((t < pi-HT) | (t > pi+HT)) {
                X[j] <- NA
                Y[j] <- NA
                next
             }
        } else {
            if ((t > HT) | (t < 2*pi - HT)) {
                X[j] <- NA
                Y[j] <- NA
                next 
            }
        }
        
        X[j] <- u+R*cos(t)
        Y[j] <- v+R*sin(t)
        
        
        
# THIS IS BROKEN
#       # Half way along the ray
#        hx <- (x + X[j]) / 2
#        hy <- (y + Y[j]) / 2    
#        
#        # Is the ray entering from the left side of the surface?
#        dd <- sqrt((hx - u)^2 + (hy - v)^2)
#        if (dd < abs(R))   {
#            X[j] <- NA
#            Y[j] <- NA
#            print(sprintf("WRONG SIDE %6.4f > %6.4f", dd, -R))
#            drawInbound (x,y, R,u,v,t,'pink')
#            next
#        } 
        
        drawInbound (x,y, R,u,v,t)
        #drawNormal (R,u,v,t)
        
        # Angle of incidence, wrt normal
        phi = pi + i - t
        
        # TODO: Wrong normal for R<0
        #if (R < 0) {
        #    phi = pi + i - (2*pi - t)
        #}
            
        ii <- (n1/n2) * sin(phi)
        if (abs(ii) > 1) {
            X[j] <- NA
            Y[j] <- NA
            print ("SKIPPING - TOTAL INTERNAL REFLECTION??")
            next
        }
        phi2 <- asin (ii)
        
        #t:R<0    t:R>0
        #0    --> pi
        #pi/2 --> pi + pi/2
        #pi   --> pi + pi
        
        # Find outgoing angle
        #if (R > 0) {
            B[j] = t + phi2 - pi
        #} else {
        #    B[j] = (2*pi-t) - phi2 - pi
        #}       
        
        if (draw.out) drawOutbound(R,u,v,t, B[j])
    }
 
    
    return (data.frame(cbind (X,Y,B)))
}




par(mar=rep(0,4))
plot (c(-10, 100), c(-5,5), pch='', bty='n', axes=F, xlab='', ylab='')

U <- 0


lensStack <- data.frame(t(matrix(nrow=5, c(
       #  R,  d,  u,  v,  n
         10,  2.5,  0,  1.25, 1.915,
        -10,  2.5,  0.6,  1.25,   1,
         
         10,  2.5,  0,  -1.25, 1.915,
        -10,  2.5,  0.6,  -1.25,   1,

        -10,  5,  0.8,  0, 1.5,
        -10,  5,  1.5,  0,   1,
         
         -10,  5,  2.0+U,  0, 1.7,
         10,  5,   2.6+U,  0,   1,
        
        5,  5,  10.1,  0, 1.2,
        5,  5,  10.6,  0,   1,
         
         10,  5,  11.0,  0, 1.6,
        -10,  5,  11.6,  0,   1
         
        ))))
colnames(lensStack) <- c('R', 'd', 'u', 'v', 'n')

lensStack$u <- lensStack$u - 5

X <- -10
Y <- -0
B <- NA

# TODO: Do this to take into account diameter of lens
D <- 18
a <- seq (-pi/D, pi/D, length.out=15) + atan((Y)/(X))


ng <- 1.8

exitAngle <- rep(NA,length(a))

for (i in 1:length(a)) {
    
    out <- data.frame(cbind(X,Y,B))
    pn <- 1.0
    out$B[1] <- a[i]
    for (j in 1:nrow(lensStack)) {
        #print (sprintf ("%d, %6.4f, X: %6.4f, Y: %6.4f, B: %6.4f", j, i, out$X[1], out$Y[1], out$B[1]))
        l <- lensStack[j,]
        out <- traceRay (out$X[1],out$Y[1],out$B[1],  l$R,l$d,l$u,l$v, pn,l$n, 
                         draw.out=(j == nrow(lensStack)))
        pn <- l$n
    }
    exitAngle[i] <- out$B[1]
}

print(sd(exitAngle)*100)
