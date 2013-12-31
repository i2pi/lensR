
x <- -10
y <- 7
R <- 4
d <- 4
u <- 0
v <- 0
n1 <- 1
n2 <- 1.8

# Ray angles, from (x,y)
D <- 15
a <- seq (-pi/D, pi/D, length.out=15) + atan((y-v)/(x-u))


# Create plot area
M <- c(-1,1) * max(abs(c(R,x,y))) * 1.2
plot (M, M, pch='', bty='n', axes=T, xlab='', ylab='')


# Draw circle (R, u, v)
p <- seq (0, 2*pi, length.out=1000)
xx <- u + R*cos(p)
yy <- v + R*sin(p)
lines (xx, yy, col='grey40')

# We know the rays are oming from the left, so let's
#  find the angles of intersection, only considering the left
p <- seq (-pi/2, pi/2, length.out=1000) + pi # coming from the left, so +pi
xx <- u + R*cos(p)
yy <- v + R*sin(p)
lines (xx, yy, col='red', lwd=2)

dx <- xx - x
dy <- yy - y
w <- atan2(dy, dx)

for (i in a) {
    #lines (c(x,x+10*R*cos(i)), c(y,y+10*R*sin(i)), col='grey20', lwd=0.6)
    
    t <- (w - i) ^ 2
    t <- p[order(t)[1:2]]
    # Can cross twice, so, pick the one closest to pi (facing left)
    if (abs(t[1] - pi) < abs(t[2] - pi)) {
        t <- t[1]
    } else {
        t <- t[2]
    }
    
    # Draw the surface normals
    lines (u+cos(t)*R*c(1,2), v+sin(t)*R*c(1,2), col='grey', lwd=1, lty=3)
 
    # Inbound rays
    lines (c(x,u+R*cos(t)), c(y,v+R*sin(t)), col='pink', lwd=1)
 
    
    # Angle of incidence, wrt normal
    phi = pi + i - t
    
    phi2 <- asin ( ((n1 / n2) * sin (phi)) )
    
    b = t + phi2 - pi 
    
    lines (u+R*cos(t) + c(0, 40*cos(b)), 
           v+R*sin(t) + c(0, 40*sin(b)), col='red')
}

