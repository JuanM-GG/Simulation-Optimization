# Growth with inhinition by product and with product partially linked to growth
# Juan Manuel
# 21/06/20
# Note: This model considers inhibition by product, also it considers that the product is produced in the
# stationary phase
######################################################################

#Set the model
model <-function(time, parms, state) {
        with(as.list(c(parms,state)),{
                
                # kinetic reactions 
                v <- Vmax*s/(Ks+s)*Kp/(Kp+p)
                
                # differential equations
                dx <- v*x
                dp <- alpha*v*x + beta*x 
                ds <- -(1/Yxs)*v*x
                
                return(list(c(dx,dp,ds)))
        })
}

# Set parameters
p <- c(Vmax = 0.5, Ks = 80, Yxs = 0.8, alpha = 12, Kp = 180, beta = 0.1)

s <- c(x = 0.2, p = 0, s = 20)

ode(y = s,times = seq(1:60),func = model,parms = p)
