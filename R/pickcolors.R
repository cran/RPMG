`pickcolors` <-
  function(COLLIST=colors(), BACK="white")
  {
    if(missing(COLLIST)) { COLLIST=colors() }
    if(missing(BACK)) {  BACK="white" }

    ncol=5
    cex  = 1
    if(length(COLLIST)>50)
      {
        ncol=15
        cex=.6
      }
    
    DF = SELOPT(COLLIST,  onoff=-1, ncol=ncol, ocols =COLLIST, cex=.6 )

    
    if(!is.null(DF))
      {
        return(DF)
      }
    else
      {
        return(NULL)
      }
    
  }

