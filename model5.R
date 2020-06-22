# Growth without inhinition and with substrate consumption for
# maintenance
# Juan Manuel
# 21/06/20
# Note: This model doesn't consider inihibition by product, however, it 
# considers that the subtrate can be used for other different reasons 
# that for growth, e.g., for the maintenance of the cells.  
######################################################################

# Load libraries
library(deSolve)
library(GA)
library(tidyverse)

#Set the model
model <-function(time, parms, state) {
        with(as.list(c(parms,state)),{
                
                #  reaction kinetic rates 
                v <- vmax*s/(ks+s)
                
                # differential equations
                dx <- v*x
                ds <- -(1/yxs)*v*x - km*x
                dp <- ypx*v*x
                return(list(c(dx,ds,dp)))
        })
}

# Set parameters
p <- c(vmax = 0.5, ks = 80, yxs = 0.8, ypx = 12, km = 0.05)

# Set initial conditions
s <- c(x = 0.2, s = 40, p = 0)

# Run the model
out <- ode(y = s,
           times = seq(0,60),
           func = model,
           parms = p,
           method = "rk4")
out <- as.data.frame(out)

# Show output
ggplot(data = out) + 
        geom_point(mapping = aes(x = time, y = s, color = "s")) + 
        geom_point(mapping = aes(x = time, y = x, color = "x") )+
        geom_point(mapping = aes(x = time, y = p,color = "p")) + 
        labs(title = "simulation", x = "time (h)", y = "Density (g/L)")+
        theme_bw()

