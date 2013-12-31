# Simple, single sided lens

theta <- function (r, d) {
    return ( 2 * asin (d / (2*r)) )
}

lensPoint <- function (x, y, r, d, w) {
    t2 <- theta (r,d) / 2
    q
    # (xp, yp) = coordinates where ray intersects lens surface,
    # with (0,0) = (plane of lens, axis of lens)
    
    xp = -r * ( cos(pi - t2) - cos(pi - t2 + w) )
    yp =  r * sin (t2 - w)
 
    return (c(xp,yp))
}



refraction <- function (x, y, r, d, w, n1=1, n2=1.8) {
    # x = distance behind plane of lens
    # y = distance above axis of lens
    # d = diameter of lens
    # r = radius of curvature
    # w = point where ray hits lens 0 --> theta(r,d)
    # Returns: Angle of incidence, wrt to the normal of the lens
    
    t2 <- theta (r,d) / 2
    
    lp = lensPoint (x, y, r, d, w) 
    a = atan2(lp[2]-y, lp[1]+x)
    
    g = t2 - w
    phi = pi - a - g
    
    b <- ((n1 / n2) * sin (phi)) 
    
    phi2 <- asin(b)
    
    # correct for quadrant
    if (phi > pi / 2) {
        phi2 = pi - phi2
    } 
    
#    print (sprintf ("phi=%6.4f, b=%6.4f, phi2=%6.4f, %+6.4f", phi, b, phi2, phi2-phi))
    
    return (a - phi2 + phi)
}

lensPlot <- function (x, y, r, d, X=NA, steps=100, rays=15) {
    
    if (is.na(X)) {
        X = x
    }
    H <- max(c(x,y,d))
    H <- c(-H,H)
    plot(H, H, pch='', bty='n', xlab='',ylab='')
    points (-x,y, pch='+', col='red' )
    abline (h=0, col='grey', lty=2)
    abline (v=0, col='grey', lty=2)
    
    # Draw lens surface
    W <- seq(0, theta(r,d), length.out=steps)
    l <- t(sapply(W, function(ww) lensPoint(x,y,r,d,ww)))
    lines(l, lwd=2)
    
    # Draw rays from point (x,y) to lens surface
    Wr <- W[seq(1,steps, length.out=rays)]
    I <- t(sapply (Wr, function(ww) lensPoint(x,y,r,d,ww)))
    for (i in 1:rays) {
        lines (c(-x,I[i,1]), c(y,I[i,2]), col='pink', lwd=1)
    }
    
    Ii <- sapply(Wr, function(ww) refraction(x,y,r,d,ww))
    for (i in 1:rays) {
        x <- I[i,1]
        y <- I[i,2]
        rr <- 3*r
        xx <- x + rr * cos(Ii[i])
        yy <- y + rr * sin(Ii[i])
        lines (c(x,xx), c(y,yy), col='red')
    }
    
    # TODO: Fresnel equations... maybe
}


lensPlot (10, 10, 4, 4)