# Growth without inhinition and with cell death
# Juan Manuel
# 21/06/20
# Note: This model donesn't consider any type of inhibition and also 
# it considers cell death. Because some processes can danger the cell, e.g.,
# the agitation and temperature changes inside the bioreactor, for these 
# reasons we can consider that it is important consider the rate at which 
# the cells death
######################################################################

#Set the model
model <-function(time, parms, state) {
        with(as.list(c(parms,state)),{
                
                #  reaction kinetic rates
                v <- vmax*s/(ks+s)
                
                # differential equations
                dx <- v*x - kd*x
                dp <- ypx*v*x
                ds <- -(1/yxs)*v*x
                return(list(c(dx,dp,ds)))
        })
}

# Set parameters
p <- c(vmax = 0.5, ks = 80, yxs = 0.8, ypx = 12, kd = 0.01)
