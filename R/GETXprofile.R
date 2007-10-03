`GETXprofile` <-
function(jx, jy, jz, LAB="A", myloc=NULL, PLOT=FALSE)
{
###############  get a cross section through a topographic DEM
############   GETXprofile : xsec  through topo

  if(missing(myloc)) {  myloc=NULL }
    if(missing(LAB)) {   LAB="A" }

  if(is.null(myloc))
    {
      myloc = locator(2, type='o')
    }

  dx = sign(diff(myloc$x))*mean(diff(jx))
  dy = mean(diff(jy))
  
  Lrunvent = lm ( myloc$y ~ myloc$x )

  newx = seq(from=myloc$x[1], to=myloc$x[2], by=dx)
  
  JY = Lrunvent$coefficients[1]+Lrunvent$coefficients[2]*newx
  
###  points(jx, JY)

  boxx = findInterval(newx, jx)

  boxy =  findInterval(JY, jy, all.inside = FALSE)

  flag = boxy>0 & boxy<length(jy)

### points(jx[flag],JY[flag], col=p2)

  pts = cbind(boxx[flag], boxy[flag])

  LX = newx[flag]
  LY = JY[flag]

  RX = sqrt((LX-LX[1])^2+ (LY-LY[1])^2)

  LZ = jz[pts]

  pnt1 = sqrt((myloc$x[1]-LX[1])^2+    (myloc$y[1]-LY[1])^2)
  pnt2 = sqrt((myloc$x[2]-LX[1])^2+    (myloc$y[2]-LY[1])^2)

  px1 = findInterval(pnt1, RX)
  px2 = findInterval(pnt2, RX)

  if(PLOT==TRUE)
    {
      ###screens(2)
      cdev = dev.cur()
      get(getOption("device"))()
      plot(RX, LZ, type='l', xlab="m", ylab="m", ylim=range(jz, na.rm=TRUE), asp=1)
      points( c(pnt1, pnt2), c(LZ[px1], LZ[px2] ), pch=c(6,8), col=c(2,4) )

      text(c(pnt1, pnt2),c(LZ[px1], LZ[px2] ) , labels=c(LAB, paste(sep="", LAB, "'")), col=c(2,4), pos=3 )
      dev.set(cdev)
    }
  invisible(list(RX=RX, RZ=LZ))

}

