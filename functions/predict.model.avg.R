predict.model.avg <- function(dd, model.max, newdata)
{
  # function to predict response variable with the AICc-weighted average model
  #
  # Args:
  # dd: the output of dredge
  # model.max: the maximal (full) model to which dredge was applied
  # newdata: the data for which you want to make predictions
  m <- model.avg(dd)
  pars <- m$coefficients["full", ]
  #form<-formula(m$formula,fixed.only=T)
  form <- formula(model.max, fixed.only = T)
  mm <-
    model.matrix(formula(paste("~", form[3], sep = "")), data = newdata)
  mm <- mm[, match(names(pars), colnames(mm))]
  return(drop(mm %*% pars))
}
# this function is used in script 3_0