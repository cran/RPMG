`circle` <-
function(n=1, ang1=0)
    {
        ### ang1 = starting angle (degrees)
    if(missing(n)) n<-1
    ##%   create a circle for plotting
    
    i=pi*seq(from=ang1, to=ang1+360, by=n)/180
    
    cx = cos(i);
    cy = sin(i);
    
    return(list(x=cx, y=cy))
    
}

