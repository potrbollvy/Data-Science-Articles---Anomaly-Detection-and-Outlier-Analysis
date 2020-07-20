post = BernBeta( c(1,1) , c( rep(1,232) , rep(0,168) ) )
post = BernBeta( post , c( rep(1,57) , rep(0,43) ) )
> savePlot("example8b.png","png")