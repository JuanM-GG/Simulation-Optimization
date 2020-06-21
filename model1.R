# Growth without inhinition
# Juan Manuel
# 21/06/20
# Note: This model is the simplest, it doesn't consider any 
# type of inhibition, also any type of cell death or substrate
# consumption for maintenance 
######################################################################

# Load libraries
library(deSolve)
library(GA)
library(tidyverse)

#Set the model
model <-function(time, parms, state) {
        with(as.list(c(parms,state)),{
                
                # reaction kinetic rates 
                v <- vmax*s/(ks+s)
                
                # differential equations
                dx <- v*x
                ds <- -(1/yxs)*v*x
                dp <- ypx*v*x
                return(list(c(dx,ds,dp)))
        })
}

# Set parameters
p <- c(vmax = 0.5, ks = 80, yxs = 0.8, ypx = 12)

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
        geom_point(mapping = aes(x = time, y = s), color = "red3") + 
        geom_point(mapping = aes(x = time, y = x), color = "midnightblue") +
        geom_point(mapping = aes(x = time, y = p),color = "yellowgreen") + 
        labs(title = "simulation", x = "time (h)", y = "Density (g/L)")+
        theme_bw()
