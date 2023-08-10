`HOZscale` <-
    function(z, col, units="", SIDE=1, s1=.6, s2=0.95, format=1, digits=3, cex=1, cex.units=1)
  {
    if(missing(units)) { units="" }
    if(missing(SIDE)) {  SIDE=1}
    if(missing(s1)) {     s1=0.6 }
    if(missing(s2)) {     s2=0.95 }

    u = par("usr")
    f = par("pin")

    ## col[col<0] = NA

    
    raty = (u[4]-u[3])/f[2]

    
    dy = (u[4]-u[3])*.05

#     dy = raty*0.25

    
    dx = (u[2]-u[1])*.3
#    LU=list(x=c(u[1]+dx*0.1, u[1]+dx*0.1+dx), y = c(u[3]-2*dy, u[3]-2*dy - dy))

    if(SIDE==1)
      {
        ym = ymarginfo(SIDE=1, s1=s1, s2=s2)

        ## y1 = ym[1]
        
        LU=list(x=c(u[1]+dx*0.1, u[1]+dx*0.1+dx), y = ym)
      }
    else
      {
        ## y1 =u[4]+dy*0.2
        ym = ymarginfo(SIDE=3, s1=s1, s2=s2)
        
        LU=list(x=c(u[1]+dx*0.1, u[1]+dx*0.1+dx), y = ym)
        
      }
    
    ##    print(paste(sep=' ', 'HOZ SCALE:', LU$x[1], LU$y[1], LU$x[2], LU$y[2])) 
   ##   rect(LU$x[1], LU$y[1], LU$x[2], LU$y[2], xpd=TRUE)
    rng = range(z, na.rm = TRUE)
    
    i <- seq(along = col)
    BX = (LU$x[2]-LU$x[1])/length(i)
    x1 =LU$x[1]+(i-1)*BX
    x2 = x1+BX
    y1 = LU$y[1]
    y2 =  LU$y[2]
      
    rect(x1,y1,x2,y2,  col=col, xpd = TRUE, border=NA)
    
####  
    if(format==2)
        #### exponential notation is requested
        {
            char = format(rng[2], scientific = T)
            sp.char = as.numeric( unlist( strsplit(char, split='e') )  )
              mant = round(sp.char[1], digits=digits)
            expt =round(sp.char[2])
            if(expt==0)
                {
                    text(LU$x[2]+BX, (y1+y2)/2, labels=mant ,adj=c(0,.5),  xpd = TRUE , cex=cex )
                }
            else
                {
                    text(LU$x[2]+BX, (y1+y2)/2, labels=bquote(.(mant)%*%10^.(expt)) ,adj=c(0,.5),  xpd = TRUE, cex=cex  )
                }
           ### 
            char = format(rng[1], scientific = T)
            sp.char = as.numeric( unlist( strsplit(char, split='e') )  )

            mant = round(sp.char[1], digits=digits)
            expt =round(sp.char[2])
              if(expt==0)
                {
                    text(LU$x[1]-BX, (y1+y2)/2, labels=mant ,adj=c(1, 0.5),  xpd = TRUE, cex=cex  )
                }
            else
                {
                    text(LU$x[1]-BX/2,  (y1+y2)/2, labels=bquote(.(mant)%*%10^.(expt)), adj=c(1, 0.5) , xpd = TRUE, cex=cex)
                }
            ###########  this is for plotting the units
            if(SIDE==1)
                {
                    text( mean(LU$x), y1, labels=units, adj=c(0.5, -0.1) , xpd = TRUE, cex=cex.units)
                }
            else
                {
                    text( mean(LU$x), y2, labels=units, adj=c(0.5, -0.1) , xpd = TRUE, cex=cex.units)
                }
        }
    else
        {
            rlab = paste(sep=" ", format.default(rng[2], digits=3), units)
            text(LU$x[2]+BX, (y1+y2)/2, labels=rlab, adj=0, xpd = TRUE, cex=cex)
            text(LU$x[1]-BX/2,  (y1+y2)/2, labels=format.default(rng[1], digits=3), adj=1, xpd = TRUE, cex=cex)
        }
    rect(LU$x[1], LU$y[1], LU$x[2], LU$y[2], xpd=TRUE)


##########  send back the outline of the scale box
    
    invisible(c(LU$x[1], LU$y[1], LU$x[2], LU$y[2], rng[1], rng[2]) )
    
  }
