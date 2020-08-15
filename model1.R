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
                v <- Vmax*s/(Ks+s)
                
                # differential equations
                dx <- v*x
                dp <- Ypx*v*x
                ds <- -(1/Yxs)*v*x
                
                return(list(c(dx,dp,ds)))
        })
}

# Set parameters
p <- c(Vmax = 0.5, Ks = 80, Yxs = 0.8, Ypx = 12)

