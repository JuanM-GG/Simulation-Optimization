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
                v <- vmax*s/(ks+s)*kp/(kp+p)
                
                # differential equations
                dx <- v*x
                dp <- ypx*v*x
                ds <- -(1/yxs)*v*x
                
                return(list(c(dx,dp,ds)))
        })
}

# Set parameters
p <- c(vmax = 0.5, ks = 80, yxs = 0.8, ypx = 12, kp = 180)
