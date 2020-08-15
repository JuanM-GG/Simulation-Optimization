# Model1: Fed-Batch with a set operation volume
# Author: Juan Manuel Gutiérrez García
# Date: 17/07/20
# Note: This model considers a kinetic growth type Monod. The operational parameters are Fint, Fout, Sin and Vlim.
##########################################################################################################################

# Set model
model <- function(time, parms ,state) {
        
        with(as.list(c(parms, state)),{
                
                #  reaction kinetic rate 
                v <- Vmax*s/(Ks+s+s^2/Ki)
                
                if(V >= Vlim || time >= tf) {
                        
                        Q <- 0
                        
                } 
                
                # Differential equations
                dx <- v*x - (Q/V)*x
                dp <- Ypx*v*x - (Q/V)*p
                ds <- -(1/Yxs)*v*x + (Q/V)*sin
                dV <- Q
                
                return(list(c(dx,dp,ds,dV), Q = Q))
        })
}


# Set parms
p <- c(Vmax = 1.2, Ks = 280, Ki = 10, Yxs = 0.2, Ypx = 4, sin = 50, Q = 0.5, 
       tf = Inf, Vlim = 300)

