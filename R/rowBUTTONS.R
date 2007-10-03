`rowBUTTONS` <-
function(labs, col=6, pch=4)
  {
 ##X## set up a row of buttons at the top of the plotting region
    ##   plot(1:10, 1:10); box(); buttons = rowBUTTONS(labs)
    if(missing(col)) { col= 6 }
    if(missing(pch)) { pch = 4 }
    
    u = par("usr")
    fin = par("fin")
    pin = par("pin")
    
    imarg = (fin[2]-pin[2])/2
    uinch =   (u[4]-u[3])/pin[2]
    N = length(labs)

    NL = N

    xper = pin[1]/NL

    minxinch = 0.6

    lnx = par("xlog")

    Y0.5 = (0.5*imarg*uinch)
    Y0.1 =  (0.1*imarg*uinch)
    Y0.8 =  (0.8*imarg*uinch)
    ###  print(paste(sep=' ', "xper", xper, lnx))

    py =  rep((u[4]+ Y0.5), NL)
    y1 = rep((u[4]+ Y0.1), NL)
    y2 = rep((u[4]+ Y0.8), NL)
    
    nump = seq(from=1, to=NL, by=1)

    bx = (u[2] - u[1])*minxinch/pin[1]
    BX = bx+bx*0.1
     
    if(xper<minxinch)
      {
###  need row of buttons on bottom
        
        NJ = floor(pin[1]/minxinch)
        TOP = min(NJ, N)
       ###  BOT = N-TOP

        ATOP = 1:TOP
        ABOT = (TOP+1):N
        NTOP = length(ATOP)
        NBOT = length(ABOT)

       
        px = vector()

        if(par("xlog")==TRUE)
          {
            ##  bx = (u[2] - u[1])*0.2/NL
            px =10^(u[2]-bx*nump)
            
            x2 =10^(u[2]-bx*(nump-0.3))
            x1 =10^(u[2]-bx*(nump+0.3))
          }
        else
          {
            
            px[ATOP] = u[2]-(BX*seq(0,NTOP-1))
            px[ABOT] = u[2]-(BX*seq(0, NBOT-1))
            
            x1=px-bx/2
            x2=px+bx/2
          }

        if(par("ylog")==TRUE)
          {
            py =  rep(10^(u[4]+ Y0.5), NL)
            y1 = rep(10^(u[4]+ Y0.1), NL)
            y2 = rep(10^(u[4]+ Y0.8), NL)
          }
        else
          {
            py[ATOP] =  rep((u[4]+ Y0.5), NTOP)
            y1[ATOP] = rep((u[4]+ Y0.1), NTOP)
            y2[ATOP] = rep((u[4]+ Y0.8), NTOP)

               py[ABOT] =  rep((u[3]- Y0.5), NBOT)
            y2[ABOT] = rep((u[3]- Y0.1), NBOT)
            y1[ABOT] = rep((u[3]- Y0.8), NBOT)
          }

      }
    else
      {
        if(par("xlog")==TRUE)
          {
            
               
            px =10^(u[2]-bx*(nump))
            
            x2 =10^(u[2]-bx*(nump-0.3))
            x1 =10^(u[2]-bx*(nump+0.3))
          }
        else
          {
            ##  bx = (u[2]-u[1])*0.045
            
            px = u[2]-(BX*seq(0,NL-1))
            x1=px-bx/2
            x2=px+bx/2
          }

        if(par("ylog")==TRUE)
          {
            py =  rep(10^(u[4]+ Y0.5), NL)
            y1 = rep(10^(u[4]+ Y0.1), NL)
            y2 = rep(10^(u[4]+ Y0.8), NL)
          }
        else
          {
            py =  rep((u[4]+ Y0.5), NL)
            y1 = rep((u[4]+ Y0.1), NL)
            y2 = rep((u[4]+ Y0.8), NL)

          }
      }

    points(px, py, pch=pch, col=col, xpd=TRUE)
  
    buttons=list(N=length(px), labs=labs, x1=x1, x2=x2, y1=y1, y2=y2)
    rect(buttons$x1,buttons$y1 ,buttons$x2, buttons$y2,  border=col, xpd=TRUE)
    text((buttons$x1+buttons$x2)/2,buttons$y1, labels=labs, pos=3, col=col, xpd=TRUE)

    return(buttons)
    
  }

