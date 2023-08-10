XPAND<-function (g, pct = 0.1) 
{
    if( length(g)<2)
    {
        cat('ERROR in XPAND: need 2 or more values\n')
        return(NULL)
    }
    
    r = range(g, na.rm=TRUE)
    dg = pct * diff(r)
    return(c(r[1] - dg, r[2] + dg))
}

