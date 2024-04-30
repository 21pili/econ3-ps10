
### Libraries
library(mlogit)
library(tidyverse)
library(stargazer)


### Imports
data("Fishing", package = "mlogit")

# divide income by 1000
fish <- Fishing %>% mutate(income =  income / 1000)

### (a)

# turning the data.frame into a dfidx object
fish <- dfidx(fish, varying = 2:9, shape = "wide", choice = "mode")
# fitting the conditional logit
conditional_logit <- mlogit(mode ~ 0 + price + catch, data = fish, reflevel = "boat") #nolint
summary(conditional_logit)

### (c)

subset_logit <- mlogit(mode ~ 0 + price + catch, data = fish, alt.subset = c("charter", "pier")) #nolint
summary(subset_logit)

hausman_test <- hmftest(conditional_logit, subset_logit)
print(hausman_test)

### (d)

multinomial_logit <- mlogit(mode ~ 0 | income, data = fish)
summary(multinomial_logit)

### (e)

mixed_logit <- mlogit(mode ~ 0 + price + catch | income, data = fish)
summary(mixed_logit)


stargazer(conditional_logit, multinomial_logit, mixed_logit, type = "latex",
                    out = "OUTPUT/regressions.tex",
                    label = "reg",
                    title = "Coefficients of the Representative Utility")