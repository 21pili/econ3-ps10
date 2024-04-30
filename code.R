
### Libraries
library(mlogit)
library(tidyverse)


### Imports
data("Fishing", package = "mlogit")

# divide income by 1000
Fishing$income <- Fishing$income/1000

# change the order of the variables - will be helpful later on
Fishing <- Fishing[c(1,10,seq(4)+5,seq(4)+1)]

# drop "beach"
Fishing <- Fishing[which(Fishing$mode!="beach"),]             # drops observations that chose beach
to_drop <- names(Fishing) %in% c("price.beach","catch.beach")
Fishing <- Fishing[!to_drop]                                  # drops the variables in to_drop
Fishing <- droplevels(Fishing)                                # "cleans" the data

# (a) - replace ...
CL <- mlogit(..., data = Fishing, shape = "wide", reflevel = "boat")
summary(CL)

# (b)
# numerators of probabilities
Fishing$numpier <- exp(cbind(Fishing$price.pier,Fishing$catch.pier) %*% CL$coefficients)
# ...