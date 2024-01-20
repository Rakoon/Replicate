#============================================================#
# Simulation: INconsistent Mediation with Absolute Prop. Mediated
# R version 4.3.2 (2023-10-31 ucrt)
# RStudio 2023.09.1+494 "Desert Sunflower" Release (cd7011dce393115d3a7c3db799dda4b1c7e88711, 2023-10-16) for windows
# Last updated = 2023-11-18
#============================================================#
### Load packages
library(lavaan) #v0.6-16
library(semTools) #v0.5-6
library(tidyverse) #v2.0.0
library(parallel) # v4.3.2 Added this package 
library(doParallel) # v1.0.17 # Added this package

# For full code runtime calculation
fullrun.start <- Sys.time()

#------------------------------------------------------------#
### Specify Population and Estimation Models
#------------------------------------------------------------#
# Population Models for data generation. Note that the direct effect is negative compared to the other sim conditions.
pop.m1 <- ' m ~ 0.138564*x
            y ~ 0.138564*m + (-1*0.1408)*x  '
pop.m2 <- ' m ~ 0.195959*x
            y ~ 0.195959*m + (-1*0.1216)*x  '
pop.m3 <- ' m ~ 0.246577*x
            y ~ 0.246577*m + (-1*0.0992)*x  '
pop.m4 <- ' m ~ 0.3020*x
            y ~ 0.3020*m + (-1*0.0688)*x  '
pop.m5 <- ' m ~ 0.348712*x
            y ~ 0.348712*m + (-1*0.0384)*x  '

# Estimation Model -- syntax guide @ https://lavaan.ugent.be/tutorial/
estim <- '  m ~ a*x
            y ~ b*m + c*x
            absPM  := abs(a*b)/(abs(c) + abs(a*b))' # direct effect labeled 'c' for convenience.

# Assign population AbsPM Values
pop1.PM <- 0.12
pop2.PM <- 0.24
pop3.PM <- 0.38
pop4.PM <- 0.57
pop5.PM <- 0.76

# Specify number of replications for the simulation function and the Monte Carlo method.
# For each Model x Sample Size combination, we run the simulation 'n' x 'rep' times.
Nrep = 1000 # number of simulation replications. Typically 1000
MCrep = 10000 # number of Monte Carlo replications used to find the 95% CI

# List all models and sample sizes combinations
model.list = c("m1n","m2n","m3n","m4n","m5n")
samplesize.list = c(100,300,500,1000)
# sim.list = c(outer(model.list, samplesize.list, paste, sep = "")) # list of model x sample sizes combinations; ordered by sample size then model number
sim.list <- c(sapply(model.list, function(x) paste0(x, samplesize.list))) # list of model x sample sizes combinations; ordered by model number then sample size
stats.list = c(outer(sim.list, ".stats", paste, sep = "")) # list of output stats to combine.

# Set seed globally
set.seed(555)

#--------------------------------------------------------------------------------#
# Set up functions
# 1. Model set-up
# 2. Perform simulations - Run for 'Nrep' times.
# 3. Extract Simulated Info
# 4. Function to perform steps 1-3
#--------------------------------------------------------------------------------#

# Function to set up model 
population.model <- function(sample.n, gen.data.model) {
  m.dta <- simulateData(gen.data.model, sample.nobs = sample.n) # generate data using supplied sample
  fit.m <- sem(estim, data = m.dta) # analyze
  m.PM <- monteCarloCI(fit.m, rep = MCrep)
}

# Function for performing simulations 
perform.simulations <- function(sample.n, gen.data.model) {
  # If simplify = TRUE, then you get a matrix of values instead of a list. Each column in the matrix is the output from one run of the function, so it's harder to extract individual values.
  replicate(n = Nrep, population.model(gen.data.model = gen.data.model, sample.n = sample.n), simplify = F)
}

# Function for extracting simulation info - Basic PM
extract.info<- function(pop.PM.value, modeled.data, data.name) {
  
  m.PMest <- as.data.frame(t(sapply(modeled.data, unlist))) # Convert list to dataframe
  m.PMest <- m.PMest %>% filter(est>=0 & est <=1) # Remove out of bound PM
  
  m.PM.bias   <- mean(m.PMest$est) - pop.PM.value # Avg. bias
  m.PM.relbias <- (mean(m.PMest$est) - pop.PM.value)/(pop.PM.value) # Relative bias
  m.PM.sd <- sd(m.PMest$est) # Efficiency (SD)
  m.PMest$ci.width <- m.PMest$ci.upper - m.PMest$ci.lower # Calculate CI width
  m.power <- sum(m.PMest$ci.lower>0) / nrow(m.PMest) # Power = proportion of reps in which the CI do not include zero when the true value is nonzero
  
  # We divide by nrow since we don't know beforehand how many rows will have PMs out of bound.
  m.cover <- sum(cbind(m.PMest$ci.lower <= pop.PM.value & m.PMest$ci.upper >= pop.PM.value), na.rm = TRUE) / nrow(m.PMest) # Coverage = proportion of reps in which CIs include the true value.
  
  ### Store all info in a new data.frame
  m.stats <- data.frame(
    bias = m.PM.bias, 
    rel.bias = m.PM.relbias, 
    SD = m.PM.sd, 
    power = m.power, 
    coverage = m.cover, 
    MeanCIWidth = mean(m.PMest$ci.width), 
    MedianCIWidth = median(m.PMest$ci.width), 
    Rel.MeanCIWidth = mean(m.PMest$ci.width)/pop.PM.value)
  
  rownames(m.stats) <- c(data.name)
  return(m.stats)
}

# Function wrapper
simulate.and.extract <- function(model.name) { # model.name based on 'sim.list' object created above
  
  model.info <- model.info[[model.name]]
  modeled.data <- perform.simulations(sample.n = model.info$sample.n, gen.data.model = model.info$pop.m)
  final.data <- extract.info(pop.PM.value = model.info$pop.PM.value, modeled.data = modeled.data, data.name = model.name)
  return(final.data)
  
}

#--------------------------------------------------------------------------------#
# Execute: Simulate and model data; Extract info; Combine data; Save data
# Note: Utilizing parallel processing
#--------------------------------------------------------------------------------#

# Store pre-requisite information about each model
model.info <- lapply(sim.list, function(sim) {
  
  sample.n <- as.numeric(sub(".*n", "", sim))
  model.name <- sub("n.*", "", sim)
  model.number <- as.numeric(sub("m", "", model.name))
  pop.m.name <- paste0("pop.", model.name)
  pop.PM.name <- paste0("pop", model.number, ".PM")
  
  m.list <- list(pop.m = get(pop.m.name), pop.PM.value = get(pop.PM.name), model.name = model.name, sample.n = sample.n)
  return(m.list)
  
})

names(model.info) <- sim.list

# Sequential or Parallel Processing ----------------------------
parallel.process <- TRUE # NOTE: If execution using parallel processing does not work, set this to FALSE for single-threaded processing

if (parallel.process) {
  no_cores <- detectCores(logical = TRUE)  # returns the number of available hardware threads, and if it is FALSE, returns the number of physical cores
  
  # Allocate cores; Provide number of clusters; Register clusters; Export necessary objects to clusters 
  cl <- makeCluster(no_cores-1)  
  registerDoParallel(cl)
  clusterExport(cl, list('estim','Nrep', 'MCrep', 'model.info', 'samplesize.list', 'population.model', 'perform.simulations', 'extract.info', 'simulate.and.extract'))
  
  # Performs parallel processing
  results <- parLapply(cl, sim.list, function(x) {
    
    library(lavaan) #v0.6-16
    library(semTools) #v0.5-6
    library(tidyverse) #v2.0.0
    set.seed(555) # Set seed in each cluster
    
    simulate.and.extract(x)
  })
  
  # Stop clusters
  stopCluster(cl)
  
} else {
  results <- lapply(sim.list, simulate.and.extract) # Regular, single-threaded processing in case parallel processing does not work.
}

# ----------------------------------------------

# Combine data
AbsPM.SimStats.Inconsistent <- bind_rows(results)

# Calculate sim run time
fullrun.end = Sys.time() - fullrun.start
fullrun.end

### Export results as CSV
setwd("output directory here")
write.csv(AbsPM.SimStats.Inconsistent, file="AbsPM.SimStats.Inconsistent.csv", row.names = TRUE)
### Save R Data for replication / backtracking
save.image("AbsPM.SimStats.Inconsistent_Results.RData")