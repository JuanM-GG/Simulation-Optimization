# Growth without inhinition and with product partially linked to growth
# Juan Manuel
# 21/06/20
# Note: This model is the doesn't consider inhibition and also consider
#  product partially linked to growth, e.g., production in the stationary phase 
######################################################################

#Set the model
model <-function(time, parms, state) {
        with(as.list(c(parms,state)),{
                
                # reaction kinetic rates
                v <- vmax*s/(ks+s)
                
                # differential equations
                dx <- v*x
                dp <- alfa*v*x + beta*x 
                ds <- -(1/yxs)*v*x
                return(list(c(dx,dp,ds)))
        })
}

# Set parameters
p <- c(vmax = 0.5, ks = 80, yxs = 0.8, alfa = 12, beta = 0.1)
