
# Function for de-trending empirical speed estimates from the underlying movement model

detrend.speeds <- function(DATA, CTMM) {
  
  # estimate speeds conditional on the data
  EST <- speeds(DATA, CTMM, fast=TRUE, level=NULL)
  # null estimates of speed (no data, only model)
  EST.NULL <- speeds(CTMM, t=DATA$t, fast=TRUE, level=NULL)
  
  # reduce DOF by that of null distribution
  DOF <- EST$DOF - EST.NULL$DOF
  all(DOF>0) # check for validity... always has held so far
  DOF <- pmax(0,DOF) # haven't needed this, but just in case
  
  S2 <- EST$speed^2 + EST$VAR
  
  AVE <- (EST$DOF*S2)/DOF
  
  # Calculate CIs
  CI <- sapply(1:length(DATA$t), function(i){ctmm:::chisq.ci(AVE[i], DOF=DOF[i], level=0.95, robust=TRUE)})
  CI <- sqrt(CI)
  
  SPEEDS <- as.data.frame(t(CI))

  SPEEDS$time <- DATA$timestamp
  
  SPEEDS
}
