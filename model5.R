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
                
                #  reaction kinetic rates 
                v <- vmax*s/(ks+s)
                
                # differential equations
                dx <- v*x
                dp <- ypx*v*x
                ds <- -(1/yxs)*v*x - km*x

                return(list(c(dx,dp,ds)))
        })
}

# Set parameters
p <- c(vmax = 0.5, ks = 80, yxs = 0.8, ypx = 12, km = 0.05)
