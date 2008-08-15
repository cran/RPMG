`jpng` <-
function(file=NULL, P=NULL)
{

  if(missing(file)) { file="Jrast" }
  if(missing(P)) { P = NULL }



  if(is.null(P))
    {
      P = par('din')
      P = round(P, digits=2)
      
    }
  
  
  psname = local.file(file, "png")


  ## P = round(par('pin'))
   png(filename = psname,
         width =P[1] , height =P[2] , units = "in", res=300,
         pointsize = 12, bg = "transparent")
   
  
  return(psname)
  
}

