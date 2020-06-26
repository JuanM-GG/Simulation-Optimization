# Data analysis using kinetic models
# Juan Manuel Gutiérrez García
# Note: This script is used to apply different models on all the 
# data sets and determine which model is the best for each one 

# Load libraries
library(deSolve)
library(GA)
library(FME)
library(tidyverse)

# Establish functions ###################################################


# Values required
s <- c(x=0.2,p=0,s=40)

# Function that simulates a model
simMod <- function(s,p) {
        results <- ode(y = s,
                      times = seq(0,60,4),
                      func = model,
                      parms = p,
                      method = "rk4")
        results <- as.data.frame(results)
        ggplot(data = results) + 
                geom_line(mapping = aes(x = time, y = s, color = "s"), size=1.5) + 
                geom_line(mapping = aes(x = time, y = x, color = "x"), size=1.5) +
                geom_line(mapping = aes(x = time, y = p,color = "p"), size=1.5) + 
                labs(title = "simulation", x = "time (h)", y = "Density (g/L)")+
                theme_bw()
}

# Function to show changes of sustrate, product and biomass on time
showPlot <- function(data) {
        ggplot(data = data) + 
                geom_point(mapping = aes(x = time, y = s, color = "s"), size = 3) + 
                geom_point(mapping = aes(x = time, y = x, color = "x"), size = 3) +
                geom_point(mapping = aes(x = time, y = p,color = "p"), size = 3) + 
                labs(title = "data", x = "time (h)", y = "Density (g/L)")+
                theme_bw()
}

# Function the get optimized parameters
getParms <- function(mydata) {
        
        data <<- mydata
        # Using GA to get parameters
        parmsOpt <- ga(type = 'real-valued',
                       fitness = function(parms) - costFun(parms),
                       lower = rep(0,length(p)),
                       upper = 5*p,
                       popSize = 50,
                       pcrossover = 0.8,
                       pmutation = 0.1,
                       names = names(p),
                       maxiter = 200,
                       maxFitness = -2000)
        
        newparms <- parmsOpt@solution %>% as.data.frame() %>% unlist()
        
        return(list("optimized_parameters" = newparms,
                    "fitness_value" = parmsOpt@fitnessValue))
        
}

# Fitness function
costFun <- function(parms) {
        
        names(parms) <- names(p)
        simOut <- ode(y =  c(x = data$x[1], p = data$p[1], s = data$s[1]),
                      times = seq(0,60,4),
                      func = model,
                      parms = parms,
                      method = "rk4")
        modCost(simOut,data)$model
}

# Function to compare simulation with optimized parameters and data
getComp <- function(optparms) {
        
        fitness <- ode(y = c(x = data$x[1], p = data$p[1], s = data$s[1]),
                       times = seq(0,60,4),
                       func = model,
                       parms = optparms,
                       method = "rk4")
        
        fitness <- as.data.frame(fitness)
        
        ggplot(data = fitness) + 
                geom_line(mapping = aes(x = time, y = s, color = "s")) + 
                geom_line(mapping = aes(x = time, y = x, color = "x")) +
                geom_line(mapping = aes(x = time, y = p,color = "p")) +
                geom_point(data = data, mapping = aes(x = time, y = s, color = "s")) + 
                geom_point(data = data, mapping = aes(x = time, y = x, color = "x")) +
                geom_point(data = data, mapping = aes(x = time, y = p,color = "p")) +
                labs(title = "simulation", x = "time (h)", y = "Density (g/L)")+
                theme_bw()
}

# This funtion makes t-test
myttest <- function() {
        
        # Get leves 
        levDat <- levels(data3[,1])
        # Get column names
        nameDat <- colnames(data3)
        
        # Let's split the data
        datA <- filter(data3, .data[[nameDat[[1]]]] == levDat[1]) %>% 
                select(nameDat[2]) %>% unlist()
        
        datB <- filter(data3, .data[[nameDat[[1]]]] == levDat[2]) %>% 
                select(nameDat[2]) %>% unlist()
        
        # The t-test
        t.test(datA,datB)
}
