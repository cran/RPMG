`SHOWPAL` <-
function(XMCOL, NAME=FALSE, ncol=5)
  {
    if(missing(NAME)) { NAME=FALSE }
    
    N  = length(XMCOL)
    
    if(missing(ncol))
      {

        if( (N%%2) == 0 )
          {
            ncol=6
          }
        else
          {
            ncol=5
          }
        
      }
    
     if( (N%%2) != (ncol%%2) )
          {
            ncol=ncol+1
          }
      
    ##  SHOWPAL(XMCOL)
   
    
    par(mfrow=c(1,1))
    
    plot(c(0,1), c(0,1), type='n', axes=FALSE, xlab='', ylab='')
    
    nrow = ceiling(N/ncol)

    dx = 1/ncol
    dy =  1/nrow

   ###  print(c(ncol, nrow))

          
    for(i in 1:N)
      {
        B =  itoxyz(i, ncol, nrow, 1)
        x = (B$ix-1)*dx
        y = (B$iy-1)*dy
        rect(x , y , x+dx, y+dy, lty=1, col=XMCOL[i] )
        if(NAME==TRUE)
          {
            lab = paste(sep=':', i,XMCOL[i])
            text(x+dx/2 , y+dy/2, lab, adj=0.5, col=1)
          }
      }
    title("Color Palette")
    return(list(N=N, ncol=ncol, nrow=nrow, dx=dx, dy=dy))
  }

