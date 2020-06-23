# Growth without inhinition
# Juan Manuel
# 21/06/20
# Note: This model is the simplest, it doesn't consider any 
# type of inhibition, also any type of cell death or substrate
# consumption for maintenance 
######################################################################

#Set the model
model <-function(time, parms, state) {
        with(as.list(c(parms,state)),{
                
                #  reaction kinetic rates 
                v <- vmax*s/(ks+s)
                
                # differential equations
                dx <- v*x
                dp <- ypx*v*x
                ds <- -(1/yxs)*v*x
                
                return(list(c(dx,dp,ds)))
        })
}

# Set parameters
p <- c(vmax = 0.5, ks = 80, yxs = 0.8, ypx = 12)
