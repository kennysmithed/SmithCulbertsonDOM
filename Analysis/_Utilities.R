library(broom)

#from https://sebastiansauer.github.io/convert_logit2prob/
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

#my inverse function
prob2logit <- function(prob) {
  odds <- prob/(1-prob)
  logodds <- log(odds)
  return(logodds)
}

#convenience function for returning estimate given fixed effect name
get.estimate <- function(model.df,fixed.effect) {
  as.numeric(model.df[which(model.df$term == fixed.effect), "estimate"])
}

get.estimate.se <- function(model.df,fixed.effect) {
  as.numeric(model.df[which(model.df$term == fixed.effect), "std.error"])
}

my.logodds.p <- function(baseline.proportion,model,predictor) {
  estimate.logodds <- summary(model)$coefficients[predictor,"Estimate"]
  estimate.se <- summary(model)$coefficients[predictor,"Std. Error"]
  baseline_logodds <- prob2logit(baseline.proportion)
  z.score = (estimate.logodds-baseline_logodds)/estimate.se #subtract baseline from coefficient and divide by standard error 
  p.value = 2*pnorm(-abs(z.score)) #obtain op value from z score
  return(p.value)
}
