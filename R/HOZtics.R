HOZtics<-function(HOZ, side=1)
{
### add tic values to HOZscale function
    pret.rng = pretty(c(HOZ[5], HOZ[6]) , 6)

    pret.rng  =pret.rng[pret.rng>HOZ[5] & pret.rng<HOZ[6] ]

    rscale = RESCALE(pret.rng,HOZ[1],HOZ[3], HOZ[5], HOZ[6]   )


    ## 

    if(side==1) text(rscale, HOZ[4], labels=pret.rng ,
                     pos=1 , cex=0.75, xpd=TRUE)

    if(side==2) text(rscale, HOZ[2], labels=pret.rng ,
                     pos=3 , cex=0.75, xpd=TRUE)

}
