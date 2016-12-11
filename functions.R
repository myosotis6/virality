#################             Functions               ####################

# -------------- required functions loading ------------- #

library(corrplot)
library(ggplot2)
library(gridExtra)

# ------------------- basic_summary --------------------- #

#' @name basic_summary
#' 
#' @description This function plots density of chosen variable
#' and the density of this variable log-transformed. It also prints
#' basic statistics for this variable.
#' 
#' @param dat data frame
#' @param hour_ix index number of hour (e.g. if one wants to see plots 
#' and summary) for v168, hour_ix = 168

basic_summary <- function(dat, hour_ix){
  if(is.null(hour_ix)){
    stop("Hour's index must be specified")
  } else {
    dat_copy <- dat
    var_name <- paste("v", hour_ix, sep="")
    
    dat_copy$log_var <- log(dat_copy[,var_name])
    
    q1 <- ggplot(dat_copy, aes_string(x = var_name)) +
      geom_histogram(aes(y=..density..),
                     colour="black", fill="white") +
      geom_density(alpha=.2, fill="#FF6666") +
      xlab(var_name) + 
      ylab("density") +
      ggtitle(paste("Density of ",var_name, sep=""))
    
    q2 <- ggplot(dat_copy, aes(x=log_var)) +
      geom_histogram(aes(y=..density..),
                     colour="black", fill="white") +
      geom_density(alpha=.2, fill="#FF6666") +
      xlab(paste("log(",var_name,")",sep="")) + 
      ylab("density") +
      ggtitle(paste("Density of log-transformed ", var_name, sep=""))
    
    grid.arrange(q1, q2, nrow=2)
    
    print(summary(df[,var_name]))
  }
}

# ---------------------- remove_outliers_3sigma -------------------------- #

#' @name routl_3sigma
#' 
#' @description This function removes outliers from the data frame,
#' based on 3 sigma rule that is counted for chosen variable.
#' 
#' @param dat data frame
#' @param var_name name of the variable that 3 sigma rule is to be
#' applied to
#' 
#' @return data.frame

routl_3sigma <- function(dat, var_name){
  dat_copy <- dat
  
  mu <- mean(dat[,var_name])
  sig <- sd(dat[,var_name])
  
  dat_copy$is_outlier <- sapply(dat[,var_name], function(i){
    if((i >= mu-3*sig) & (i <= mu+3*sig)){
      0
    } else {
      1
    }
  })
  
  return(dat_copy[dat_copy$is_outlier == 0,])
}

# ----------------------- xyplot_funct --------------------- #

#' @name xyplot_funct
#' 
#' @description This function plots xyplot for two variables, predictor
#' and response variables.
#'
#' @param dat data frame
#' @param predictor name of the predictor
#' @param response_var name of the response variable

xyplot_funct <- function(dat, predictor, response_var){
  form <- formula(paste(response_var, "~", predictor))
  xyplot(form, data = dat, xlab=predictor, ylab = response_var,
         main = "Number of views")
}

# ----------------------- mRSE_compute --------------------- #

mRSE_compute <- function(predicted_values, true_values){
  if(length(predicted_values) != length(true_values)){
    stop("Number of predicted values must be equal to number of true values")
  }
  
  n <- length(true_values)
  mRSE <- (sum((predicted_values/true_values - 1)^2)/n)
  
  return(mRSE)
}

# ----------------------- build_LM ------------------------- #

#' @name build_LM
#' 
#' @description This function builds linear regression model, using
#' specified percent of observations for testing. Then mRSE value is 
#' calculated for test data and returned.
#' 
#' @param dat data frame
#' @param response_var
#' @param perc_test
#' @param seed
#' 
#' @param mRSE

build_LM <- function(dat, response_var, predictors, 
                     perc_test=0.1, seed=3234364){
  # Setting seed - for reproducibility
  set.seed(seed)
  
  dat_colnames <- colnames(dat)
  
  obs_to_remove_DF <- unlist(sapply(predictors, function(var_name){
    which(is.infinite(dat[,var_name]) | is.nan(dat[,var_name]) | is.na(dat[,var_name]))
  }), use.names=FALSE)
  obs_to_remove <- unique(as.vector(obs_to_remove_DF))
  nb_obs_to_remove <- length(obs_to_remove)
  
  if(nb_obs_to_remove>0){
    message(paste(nb_obs_to_remove, " observations detected with non-acceptable values, removed", sep=""))
    dat <- dat[-obs_to_remove, ]
  }
  
  # number of observations in cleaned_log_dfa set
  n_dat <- dim(dat)[1]
  
  # Randomly choosing observations for test set
  test_ix <- sample(1:n_dat, floor(perc_test*n_dat))
  
  # Creating train set and test set
  train_set <- dat[-test_ix,]
  test_set <- dat[test_ix,]
  
  n_train_set <- dim(train_set)[1]
  n_test_set <- dim(test_set)[1]
  
  # Building LM model
  model_formula <- formula(paste("log_",response_var, "~", paste(paste("log_",predictors,sep=""), collapse="+"), sep=""))
  fit <- lm(model_formula, data = train_set)
  
  predicted_log <- predict(fit, newdata=test_set)
  
  predicted_values <- exp(predicted_log)
  mRSE <- mRSE_compute(predicted_values, test_set[,"v168"])
  return(mRSE)
}