### Setting working directory
setwd("C:/Users/Gosia/Desktop/Tooploox_Data_Scientist_Exercise")

### Loading required libraries and functions

source("tooploox_functions.R")

### Reading data, creating vector of column names
cols_hours <- paste("v", seq(1:168), sep="")
df <- read.csv2("data.csv", sep=",", header = FALSE, 
                col.names = c("id", cols_hours))
n <- dim(df)[1]
k <- dim(df)[2]

### Data checking
v_types <- sapply(seq(1:k), function(i) class(df[,i]))
v_types[1] == "factor"
unique(v_types[-1]) == "integer"

### Any missing values?
any(is.na(df)) == FALSE

### Analysing basic statistics for a few variables:
basic_summary(df, 24)
basic_summary(df, 48)
basic_summary(df, 72)
basic_summary(df, 168)

# Based on the viewed plots, we can see that predictors need to be
# log-transformed.

### Adding log-transformed variables
log_cols_hours <- paste("log", cols_hours, sep="_")
df[log_cols_hours] <- lapply(df[cols_hours], log) # note: NaNs produced!

nan_columns <- which(sapply(1:dim(df)[2], function(i){
  any(is.infinite(df[,i]))
}))

idx_nan <- which(is.infinite(df[,nan_columns]))
print(length(idx_nan))
print(length(idx_nan)/n)

# There are 30 rows with missing values, which is around 3% of obs.
### Removing them from dataset
df <- df[-idx_nan,]
dim(df)
# 886 obs.

### Removing outliers
cleaned_df <- routl_3sigma(df, "log_v168")
dim(cleaned_df)
# 871 obs. - 15 were removed

### Correlation checking
vars_to_check_corr_for <- paste("log_v", c(seq(1,24),168), sep="")
cor_mtrx <- cor(cleaned_df[,vars_to_check_corr_for]) # get correlations

corrplot(cor_mtrx, method = "number")
corrplot(cor_mtrx, method = "square")

# View(cor_mtrx)
# Strong correlation is visible

### Does linear regression seem like a good choice?
xyplot_funct(cleaned_df, predictor = "log_v1", response_var="log_v168")
xyplot_funct(cleaned_df, predictor = "log_v24", response_var="log_v168")
xyplot_funct(cleaned_df, predictor = "log_v48", response_var="log_v168")
xyplot_funct(cleaned_df, predictor = "log_v72", response_var="log_v168")

# Seems to be good - building model
results_LR <- sapply(1:24, function(i){
  build_LM(cleaned_df, response_var="v168", predictors=paste("v",i,sep=""))
  })

results_MILR <- sapply(1:24, function(i){
  i_vec <- seq(1,i)
  build_LM(cleaned_df, response_var="v168", predictors=paste("v",i_vec,sep=""))
})

mRSE_tab <- data.frame(hour = rep(seq(1:24),2), 
                       mRSE = c(results_LR,results_MILR),
                       mod = c(rep("Linear Regression",24),
                               rep("Multiple-input Linear Regression",24)))

ggplot(data=mRSE_tab, 
       aes(x=hour, y=mRSE, group=mod, shape=mod, colour=mod)) + 
  geom_line(aes(linetype=mod), size=1) +     
  geom_point(size=3, fill="white") + 
  xlab("Reference time (n)") + ylab("mRSE") + 
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = c(.8,.8),
        legend.text=element_text(size=12))