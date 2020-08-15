# Growth without inhinition and with substrate consumption for
# maintenance
# Juan Manuel
# 21/06/20
# Note: This model doesn't consider inihibition by product, however, it 
# considers that the subtrate can be used for other different reasons 
# that for growth, e.g., for the maintenance of the cells.  
######################################################################

#Set the model
model <-function(time, parms, state) {
        with(as.list(c(parms,state)),{
                
                if(s <= 0) {
                        
                        Km <- 0
                }
                #  reaction kinetic rates 
                v <- Vmax*s/(Ks+s)
                
                # differential equations
                dx <- v*x
                dp <- Ypx*v*x
                ds <- -(1/Yxs)*v*x - Km*x

                return(list(c(dx,dp,ds)))
        })
}

# Set parameters
p <- c(Vmax = 0.5, Ks = 80, Yxs = 0.8, Ypx = 12, Km = 0.05)
