# Load libraries
library(xlsx)
source("analysis_with_kinetic_models.R")

# Load data
# I.terricola 
dataIt <- read.xlsx("DATOS/DATOS.xlsx","IT")
head(dataIt)

# P.kluyveri
dataPk <- read.xlsx("DATOS/DATOS.xlsx","PK")
head(dataPk)

# K. marxianus
dataKm <- read.xlsx("DATOS/DATOS.xlsx","KM")
head(dataKm)

# Z. baili
dataZb <- read.xlsx("DATOS/DATOS.xlsx","ZB")
head(dataZb)

#######################################################################
# Using model 1 to analize the data ###################################
source("model1.R")

# I.terricola
# Show data 
showPlot(dataIt)

# Get parameters 
newParmsIt1 <- getParms(dataIt)
newParmsIt1
# Compare simulation with optimized parameters with data
getComp(newParmsIt1$optimized_parameters)

# P.kluyveri
# Show data
showPlot(dataPk)

# Get parameters
newParmsPk1 <- getParms(dataPk)
newParmsPk1
# Compare simulation with optimized parameters with data
getComp(newParmsPk1$optimized_parameters)

# K.marxianus
# Show data
showPlot(dataKm)

# Get parameters
newParmsKm1 <- getParms(dataKm)
newParmsKm1
# Compare simulation with optimized parameters with data
getComp(newParmsKm1$optimized_parameters)

# Z.baili
# Show data
showPlot(dataZb)

# Get parameters
newParmsZb1 <- getParms(dataZb)
newParmsZb1
# Compare simulation with optimized parameters with data
getComp(newParmsZb1$optimized_parameters)
#######################################################################
#######################################################################
# Using model 2 to analize the data ###################################
source("model2.R")

# I.terricola
# Show data
showPlot(dataIt)

# Get parameters
newParmsIt2 <- getParms(dataIt)
newParmsIt2
# Compare simulation with optimized parameters with data
getComp(newParmsIt2$optimized_parameters)

# P.kluyveri
# Show data
showPlot(dataPk)

# Get parameters
newParmsPk2 <- getParms(dataPk)
newParmsPk2

# Compare simulation with optimized parameters with data
getComp(newParmsPk2$optimized_parameters)

# K.marxianus
# Show data
showPlot(dataKm)

# Get parameters
newParmsKm2 <- getParms(dataKm)
newParmsKm2
# Compare simulation with optimized parameters with data
getComp(newParmsKm2$optimized_parameters)

# Z.baili
# Show data
showPlot(dataZb)

# Get parameters
newParmsZb2 <- getParms(dataZb)
newParmsZb2
# Compare simulation with optimized parameters with data
getComp(newParmsZb2$optimized_parameters)
#######################################################################
#######################################################################
# Using model 3 to analize the data ###################################
source("model3.R")

# I.terricola
# Show data
showPlot(dataIt)

# Get parameters
newParmsIt3 <- getParms(dataIt)
newParmsIt3
# Compare simulation with optimized parameters with data
getComp(newParmsIt3$optimized_parameters)

# P.kluyveri
# Show data
showPlot(dataPk)

# Get parameters
newParmsPk3 <- getParms(dataPk)
newParmsPk3
# Compare simulation with optimized parameters with data
getComp(newParmsPk3$optimized_parameters)

# K.marxianus
# Show data
showPlot(dataKm)

# Get parameters
newParmsKm3 <- getParms(dataKm)
newParmsKm3
# Compare simulation with optimized parameters with data
getComp(newParmsKm3$optimized_parameters)

# Z.baili
# Show data
showPlot(dataZb)

# Get parameters
newParmsZb3 <- getParms(dataZb)
newParmsZb3

# Compare simulation with optimized parameters with data
getComp(newParmsZb3$optimized_parameters)
#######################################################################
#######################################################################
# Using model 4 to analize the data ###################################
source("model4.R")

# I.tericola
# Show data
showPlot(dataIt)

# Get parameters
newParmsIt4 <- getParms(dataIt)
newParmsIt4

# Compare simulation with optimized parameters with data
getComp(newParmsIt4$optimized_parameters)

# P.kluyveri
# Show data
showPlot(dataPk)

# Get parameters
newParmsPk4 <- getParms(dataPk)
newParmsPk4

# Compare simulation with optimized parameters with data
getComp(newParmsPk4$optimized_parameters)

# K.marxianus
# Show data
showPlot(dataKm)

# Get parameters
newParmsKm4 <- getParms(dataKm)
newParmsKm4

# Compare simulation with optimized parameters with data
getComp(newParmsKm4$optimized_parameters)

# Z.baili
# Show data
showPlot(dataZb)

# Get parameters
newParmsZb4 <- getParms(dataZb)
newParmsZb4

# Compare simulation with optimized parameters with data
getComp(newParmsZb4$optimized_parameters)
#######################################################################
#######################################################################
# Using model 4 to analize the data ###################################
source("model5.R")

# I.tericola
# Show data
showPlot(dataIt)

# Get parameters
newParmsIt5 <- getParms(dataIt)
newParmsIt5

# Compare simulation with optimized parameters with data
getComp(newParmsIt5$optimized_parameters)

# P.kluyveri
# Show data
showPlot(dataPk)

# Get parameters
newParmsPk5 <- getParms(dataPk)
newParmsPk5

# Compare simulation with optimized parameters with data
getComp(newParmsPk5$optimized_parameters)

# K.marxianus
# Show data
showPlot(dataKm)

# Get parameters
newParmsKm5 <- getParms(dataKm)
newParmsKm5

# Compare simulation with optimized parameters with data
getComp(newParmsKm5$optimized_parameters)

# Z.baili
# Show data
showPlot(dataZb)

# Get parameters
newParmsZb5 <- getParms(dataZb)
newParmsZb5

# Compare simulation with optimized parameters with data
getComp(newParmsZb5$optimized_parameters)
#######################################################################