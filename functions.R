# Data analysis using kinetic models
# Juan Manuel Gutiérrez García
# Note: This script is used to apply different models on all the 
# data sets and determine which model is the best for each one 


##### Simulation section ####################################################################
# Values required
#s <- c(x=0.2,p=0,s=40)

# Function that simulates a model
make_simulation_fun_sim <- function(s,p) {
        
        ode(y = s,
            times = seq(0,60,4),
            func = model,
            parms = p,
            method = "rk4") %>% as.data.frame() %>%
                
                ggplot() + 
                geom_line(mapping = aes(x = time, y = s, color = "s"), size=1.5) + 
                geom_line(mapping = aes(x = time, y = x, color = "x"), size=1.5) +
                geom_line(mapping = aes(x = time, y = p,color = "p"), size=1.5) + 
                labs(x = "time (h)", y = "concentration (g/L)")+
                theme_bw() 
}
#############################################################################################
##### Optimization section ##################################################################
# Function to show data #
plot_data_fun_opt <- function(data) {
        ggplot(data = data) + 
                geom_point(mapping = aes(x = time, y = s, color = "s"), size = 3) + 
                geom_point(mapping = aes(x = time, y = x, color = "x"), size = 3) +
                geom_point(mapping = aes(x = time, y = p,color = "p"), size = 3) + 
                labs(x = "time (h)", y = "concentration (g/L)")+
                theme_bw()
}

# Function to get optimized parameters #
get_parms_fun_opt <- function(data) {
        
        # Let's do this in order to opt_data be reached by cost_fun_opt() 
        # and comp_fun_opt() function
        opt_data <<- data
        
        # Using GA to get parameters
        parms_aux_opt <- ga(type = 'real-valued',
                       fitness = function(parms) - cost_fun_opt(parms),
                       lower = rep(0,length(p)),
                       upper = 5*p,
                       popSize = 50,
                       pcrossover = 0.8,
                       pmutation = 0.1,
                       names = names(p),
                       maxiter = 1, 
                       maxFitness = -1000)
        
        opt_parms_aux_opt <- parms_aux_opt@solution %>% 
                as.data.frame() %>% unlist()
        
        return(list("optimized_parameters" = opt_parms_aux_opt,
                    "fitness_value" = parms_aux_opt@fitnessValue))
        
}

# Fitness function #
cost_fun_opt <- function(parms) {
        
        names(parms) <- names(p)
        simOut <- ode(y =  c(x = opt_data$x[1], p = opt_data$p[1], s = opt_data$s[1]),
                      times = seq(0,60,4),
                      func = model,
                      parms = parms,
                      method = "rk4")
        modCost(simOut,opt_data)$model
}

# Function to compare simulation using the optimized parameters and data #
comp_fun_opt <- function(p) {
        
        fitness <- ode(y = c(x = opt_data$x[1], p = opt_data$p[1], s = opt_data$s[1]),
                       times = seq(0,60,4),
                       func = model,
                       parms = p,
                       method = "rk4")
        
        fitness <- as.data.frame(fitness)
        
        ggplot(data = fitness) + 
                geom_line(mapping = aes(x = time, y = s, color = "s"), size=1.5) + 
                geom_line(mapping = aes(x = time, y = x, color = "x"), size=1.5) +
                geom_line(mapping = aes(x = time, y = p,color = "p"), size=1.5) +
                geom_point(data = opt_data, mapping = aes(x = time, y = s, color = "s"),size=3) + 
                geom_point(data = opt_data, mapping = aes(x = time, y = x, color = "x"),size=3) +
                geom_point(data = opt_data, mapping = aes(x = time, y = p,color = "p"),size=3) +
                labs(x = "time (h)", y = "concentration (g/L)")+
                theme_bw()
}
##############################################################################################
##### t-test section #########################################################################
# This funtion makes the t-test
make_sts_fun_sts <- function(data_ttest) {
        
        # Get level names
        levels_data <- levels(data_ttest[,1])
        
        # The t-test
        my_ttest <- t.test(data_ttest[,2] ~ data_ttest[,1]) 
                
        data.frame(parameters = c("t statistic", "degrees of freedom", 
                                   "p-value", "confidence interval min",
                                   "confidence interval max", paste0("mean ",levels_data[1]),
                                  paste0("mean ",levels_data[2]), "alternative hypothesis"),
                    
                    values = c(my_ttest$statistic, 
                               my_ttest$parameter, my_ttest$p.value, 
                               my_ttest$conf.int[1],my_ttest$conf.int[2], 
                               my_ttest$estimate, "true difference in means is not equal to 0"))
}


# This funtion makes the ANOVA analysis
make_anova_fun_anova <- function(data_anova) {
        
        model.lm <- lm(data_anova[,2] ~ data_anova[,1])
        
        anova(model.lm)
}

# This function makes the tukey test

make_tukey_fun_tukey <- function(data_tukey) {
        
        model.lm <- lm(data_tukey[,2] ~ data_tukey[,1]) 
        
        my_anova <- aov(model.lm) 
        
        my_tukey <- TukeyHSD(my_anova)
                        
        data.frame("differences" = my_tukey$`data_tukey[, 1]`[,1], "p adj" = my_tukey$`data_tukey[, 1]`[,4])
        
        
}

# This function makes a boxplot from a dataset
make_boxplot_fun_sta <- function(data_bp) {
        
        # Get column names
        nameDat <- colnames(data_bp)
        
        # Make boxplot
        ggplot(data = data_bp) + 
                
                geom_boxplot(mapping = aes(x =  .data[[nameDat[[1]]]],
                                           y = .data[[nameDat[[2]]]],
                                           
                                           color = .data[[nameDat[[1]]]]))
        
}

# This function makes a scatter plot from a dataset
make_scatter_fun_lr <- function(data_sp,varx,vary) {
        
        # Make scatter plot
        ggplot(data = data_sp, mapping = aes(x =  .data[[varx]],
                                             
                                             y = .data[[vary]])) + 
                
                geom_point() +
                
                geom_smooth() +
                
                labs(title = "scatter plot", x = varx, 
                     y = vary) +
                
                theme_bw()
        
}

# This function makes linear regression analysis
make_lr_fun_lr <- function(data_lr, ind_var_lr,dep_var_lr) {
        
        formula_aux_lr <- make_formula_fun_lr(ind_var_lr,dep_var_lr)
        
        mymodel <- lm(formula_aux_lr, data = data_lr)
        
        mysummary <- summary(mymodel)
        
        myparms <- mysummary$coefficients[,1]
        
        my_r_squared <- sqrt(mysummary$r.squared)
        
        my_result <- c(myparms, my_r_squared) %>% as.numeric()
        
        names_result <- c(names(myparms), "R")
        
        myresult <- data.frame("parameters"= names_result,"values"= my_result)
        
        return(myresult)
        
}

make_formula_fun_lr <- function(ind_var_lr,dep_var_lr) {
        
        formula_aux_lr <- ind_var_lr[1]
        
        for (i in ind_var_lr[-1]) {
                
                formula_aux_lr <- paste(formula_aux_lr,i,sep = "+")
        }
        
        (formula_aux_lr <- paste(dep_var_lr,formula_aux_lr,sep = "~"))
        
}


make_result_anova_fun_anova <- function(my_anova) {
        
        data.frame("parameters" = c("degrees of freedom", "sum of squares",
                                    "mean of squares","F value",
                                    "Pr(>F)"),
                   "Values" = c(paste(my_anova$Df[1],my_anova$Df[2],sep = " -- "),
                                paste(round(my_anova$`Sum Sq`[1],5),round(my_anova$`Sum Sq`[2],5),sep = " -- "),
                                paste(round(my_anova$`Mean Sq`[1],5),round(my_anova$`Mean Sq`[2],5),sep = " -- "), 
                                round(my_anova$`F value`[1],5),
                                round(my_anova$`Pr(>F)`[1],5)))
}

make_means_fun_anova <- function(data_means) {
        
        colnames_data <- colnames(data_means)
        data_means %>%
                group_by(.data[[colnames_data[1]]]) %>%
                summarise(means = mean(.data[[colnames_data[2]]]))
}