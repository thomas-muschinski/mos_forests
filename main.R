library(RainTyrol)
library(scoringRules)

source("fn_pp.R") # contains functions for postprocessing

p_mosf <- list()
p_distf <- list()
p_emos <- list()
p_qrf <- list()
crps <- list()

for (id in unique(RainTyrol$station)) { # loop over stations
  print(paste0("Working on ", id, " ..."))
 
  ## Prepare training and testing data
  dat <- subset(RainTyrol, subset = (station %in% id))[2:ncol(RainTyrol)]
  datTrain <- subset(dat, subset = (year <= 2008))
  datTest <- subset(dat, subset = (year > 2008))

  ## Train models and make predictions on test data
  p_mosf[[id]] <- mos_forest(datTrain, datTest) # parameters
  p_distf[[id]] <- dist_forest(datTrain, datTest) # parameters
  p_qrf[[id]] <- quant_forest(datTrain, datTest) # quantiles
  p_emos[[id]] <- emos(datTrain, datTest) # parameters

  ## Compute CRPS on test data
  crps[[id]] <- list()

  crps[[id]]$mos_forest <- crps_cnorm(datTest$robs,
			  	      location = p_mosf[[id]]$location, 
				      scale = p_mosf[[id]]$scale, 
				      lower = 0)

  crps[[id]]$dist_forest <- crps_cnorm(datTest$robs,
			  	       location = p_distf[[id]]$mu, 
				       scale = p_distf[[id]]$sigma, 
				       lower = 0)

  crps[[id]]$quant_forest <- crps_sample(y = datTest$robs,
			  	         dat = p_qrf[[id]])

  crps[[id]]$emos <- crps_cnorm(datTest$robs,
			  	location = p_emos[[id]]$location, 
				scale = p_emos[[id]]$scale, 
				lower = 0)
}    

## Compute mean CRPS for each station and method
crps_mean <- as.data.frame(t(sapply(crps, function(x) {sapply(x, mean)})))

## Compute CRPSS relative to EMOS
crpss <- 1 - crps_mean / crps_mean$emos


