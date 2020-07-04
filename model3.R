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
                v <- Vmax*s/(Ks+s)
                
                # differential equations
                dx <- v*x
                dp <- alpha*v*x + beta*x 
                ds <- -(1/Yxs)*v*x
                return(list(c(dx,dp,ds)))
        })
}

# Set parameters
p <- c(Vmax = 0.5, Ks = 80, Yxs = 0.8, alpha = 12, beta = 0.1)
