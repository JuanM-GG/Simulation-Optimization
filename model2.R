# Growth with inhinition by product
# Juan Manuel
# 21/06/20
# Note: This model considers inhibition by product, also it doesn't 
# consider any type of cell death or substrate consumption for
# maintenance 
######################################################################

#Set the model
model <-function(time, parms, state) {
        with(as.list(c(parms,state)),{
                
                # kinetic reactions 
                v <- Vmax*s/(Ks+s)*Kp/(Kp+p)
                
                # differential equations
                dx <- v*x
                dp <- Ypx*v*x
                ds <- -(1/Yxs)*v*x
                
                return(list(c(dx,dp,ds)))
        })
}

# Set parameters
p <- c(Vmax = 0.5, Ks = 80, Yxs = 0.8, Ypx = 12, Kp = 180)
