library(model4you) # for MOS forests
library(crch) # for EMOS and MOS forests
library(disttree) # for distributional forests
library(quantregForest) # for quantile regression forests

###############################################
## Learn MOS forest and predict out-of-sample #
###############################################

mos_forest <- function(datTrain = datTrain, 
		       datTest = datTest, 
		       what = "param") { 
  mos <- crch(robs ~ tppow_mean, data = datTrain, left = 0)
  mf <- pmforest(mos,
                 data = datTrain,
                 ntree = 100,
                 type.tree = "ctree",
                 zformula = "~ .",
                 control =
                   ctree_control(teststat = "quad",
                                 testtype = "Univ",
                                 intersplit = TRUE,
                                 minsplit = 50,
                                 mincriterion = 0,
                                 minbucket = 20))
  if (what == "model") {
    return(pmodel(x = mf, newdata = datTest, fun = identity))
  } else if (what == "coeff") {
    return(pmodel(x = mf, newdata = datTest, fun = coef))
  } else if (what == "param") {
    tmp <- pmodel(x = mf, newdata = datTest, fun = identity)
    p <- do.call(rbind, 
                 lapply(names(tmp), 
 	   	        function(x) {predict(tmp[[x]], 
					     newdata = datTest[x,],
                                             type = "parameter")}))
    return(p)
  }
}


##########################################################
## Learn distributional forest and predict out-of-sample #
##########################################################

dist_forest <- function(datTrain = datTrain,          
                        datTest = datTest, 
                        what = "param") {
  df <- distforest(robs ~ ., 
		   family = dist_list_cens_normal,
                   censtype = "left", 
		   censpoint = 0,
                   data = datTrain, 
		   type.tree = "ctree",
                   ntree = 100, 
                   control = disttree_control(teststat = "quad",
                                              testtype = "Univ",
                                              intersplit = TRUE,
                                              mincriterion = 0,
                                              minsplit = 50,
                                              minbucket = 20))
  if (what == "model") {
    return(df)
  } else if (what == "coeff") {
    return(predict(df, newdata = datTest, type = "coef"))
  } else if (what == "param") {
    return(predict(df, newdata = datTest, type = "parameter"))  
  } 
}

###############################################################
## Learn quantile regression forest and predict out-of-sample #
###############################################################

quant_forest <- function(datTrain = datTrain,          
                        datTest = datTest, 
                        what = "quant") {
  qrf <- quantregForest(x = datTrain[, !(names(datTrain) %in% c("robs"))],
                        y = datTrain$robs)
  if (what == "quant") {
    return(predict(qrf, 
		   datTest[, !(names(datTrain) %in% c("robs"))],
		   what = 0.01 * (1:99)))
  }
}


############################################
## Estimate EMOS and predict out-of-sample #
############################################

emos <- function(datTrain = datTrain, 
                 datTest = datTest, 
                 what = "param") {
  emos <- crch(robs ~ tppow_mean | tppow_sprd, data = datTrain, left = 0)
  if (what == "model") {
    return(emos)
  } else if (what == "coeff") {
    return(predict(emos, newdata = datTest, type = "coef"))
  } else if (what == "param") {
    return(predict(emos, newdata = datTest, type = "parameter"))
  }
}	
	



