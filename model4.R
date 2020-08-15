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
                v <- Vmax*s/(Ks+s)
                
                # differential equations
                dx <- v*x - Kd*x
                dp <- Ypx*v*x
                ds <- -(1/Yxs)*v*x
                return(list(c(dx,dp,ds)))
        })
}

# Set parameters
p <- c(Vmax = 0.5, Ks = 80, Yxs = 0.8, Ypx = 12, Kd = 0.01)
