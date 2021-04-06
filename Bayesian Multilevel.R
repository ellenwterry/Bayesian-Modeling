library(tidyverse)
library(lme4)
library(rstan)
library(cowplot)
library(ggplot2)
library(lubridate)
library(ggridges)
library(kableExtra)

SalesTrans = 
  read_csv("C:/Users/ellen/Documents/UH/Fall 2020/Class Materials/Section II/Class 1/Data/Sales.csv")
Location = 
  read_csv("C:/Users/ellen/Documents/UH/Fall 2020/Class Materials/Section II/Class 1/Data/Location.csv")
MerGroup = 
  read_csv("C:/Users/ellen/Documents/UH/Fall 2020/Class Materials/Section II/Class 1/Data/MerGroup.csv")
SalesTrans = SalesTrans %>% inner_join(Location, by = "LocationID")
SalesTrans = SalesTrans %>% inner_join(MerGroup, by = "MerGroup")
LocationID = as.factor(SalesTrans$LocationID)
SalesTrans$ProductID = as.factor(SalesTrans$ProductID)
SalesTrans$Description = as.factor(SalesTrans$Description)
SalesTrans$MerGroup = as.factor(SalesTrans$MerGroup)

# breaking out Q4 to simplify exercise

SalesTrans$Qtr = quarter(SalesTrans$Tdate)
SalesTrans = filter(SalesTrans, Qtr == 4)
SalesTransSummary = SalesTrans %>% 
  group_by(Description, MerGroup, MfgPromo, Wk ) %>% 
  summarise(Volume = n(), TotSales = sum(Amount) )


SalesSummarySub = filter(SalesTransSummary, 
                         Description %in% c("Arlington", "Boston", "San Jose", "Seattle"), 
                         MerGroup %in% c("Accessories", "Beauty", "Cooking", "Men" ))

p =  ggplot(SalesSummarySub, aes(Wk, TotSales, color = MerGroup)) + 
  geom_point(alpha = .2) + 
  geom_smooth(method = "lm", se = F, alpha = .05, linetype = "dashed", alpha = .5) +
  facet_wrap(~Description) +
  theme(panel.background = element_rect(fill = "white")) 
p 


Priors = lm(TotSales ~ Wk , data = SalesTransSummary) 
lmI = as.numeric(Priors$coefficients[1])
lmS = as.numeric(Priors$coefficients[2])


# get adjusted level coefficients


# build stan model
stanMod <- '

data {

  int<lower=0> N;
  vector[N] x; // Wk
  vector[N] y; // TotSales

  int<lower=0> J; // Description
  int Description[N];

  int<lower=0> K; // MerGroup
  int MerGroup[N];

  vector[J] p_alpha;
  vector[K] p_alpha2;

  real<lower=0> p_alphaSigma;
  real<lower=0> p_alphaSigma2;

  vector[J] p_beta;
  vector[K] p_beta2;

  real<lower=0> p_betaSigma;
  real<lower=0> p_betaSigma2;

}

parameters {

  // random effects we found
  vector[J] alpha; // intercept for Description
  vector[K] alpha2; // intercept for MerGroup

  vector[J] beta; // slope for Description
  vector[K] beta2; // slope for MerGroup

  real<lower=0> sigma; // to control sigma of y_hat

}

transformed parameters {

  vector[N] y_hat;
  for(i in 1:N) 
  y_hat[i]=alpha[Description[i]]+alpha2[MerGroup[i]]+(beta[Description[i]]*x[i])+(beta2[MerGroup[i]]*x[i]);
}

model {

  target += normal_lpdf(alpha | p_alpha, p_alphaSigma);
  target += normal_lpdf(alpha2 | p_alpha2, p_alphaSigma2);

  target += normal_lpdf(beta | p_beta, p_betaSigma);
  target += normal_lpdf(beta2 | p_beta2, p_betaSigma2);

  target += normal_lpdf(sigma | 50, 50);

  // y_hat: our prediction
  target += normal_lpdf(y | y_hat, sigma);
}
'

fit <- stan(model_code = stanMod,  data = list(
  N = nrow(SalesTransSummary),
  y = SalesTransSummary$TotSales, 
  x = SalesTransSummary$Wk,
  J = length(unique(SalesTransSummary$Description)),
  K = length(unique(SalesTransSummary$MerGroup)),
  Description = as.numeric(SalesTransSummary$Description),
  MerGroup = as.numeric(SalesTransSummary$MerGroup),
  
  p_alpha = rep(lmI,10), # Description intercept
  p_alpha2 = rep(lmI,10), # MerGroup intercept
  
  # 
  p_alphaSigma = 500,
  p_alphaSigma2 = 500,
  
  p_beta = rep(lmS,10), # Description slope (lmer model)
  p_beta2 = rep(lmS,10), # MerGroup slope (lmer model)
  
  p_betaSigma = 10,
  p_betaSigma2 = 10
  
), refresh = 0)


# Extract coefficients from stan model
sumFit <- data.frame(summary(fit))

# Description
Intercept1 <- summary(fit, pars = c("alpha"), probs = c(0.1, 0.9))$summary
# MerGroup
Intercept2 <- summary(fit, pars = c("alpha2"), probs = c(0.1, 0.9))$summary

# Description slope
Slope1 <- summary(fit, pars = c("beta"), probs = c(0.1, 0.9))$summary
# MerGroup slope
Slope2 <- summary(fit, pars = c("beta2"), probs = c(0.1, 0.9))$summary

# random effects for each Description
CoefMapD <- data.frame(Description = unique(SalesTransSummary$Description), 
                       DIntercept = Intercept1[,1], DSlope = Slope1[,1])
# random effects for each MerGroup
CoefMapM <- data.frame(MerGroup = unique(SalesTransSummary$MerGroup), MIntercept = Intercept2[,1], MSlope = Slope2[,1])

# add all intercepts and all slopes cross each row
CoefMapB = crossing(CoefMapD, CoefMapM) %>% 
  mutate(Intercept = DIntercept + MIntercept, Slope = DSlope + MSlope)

CoefMapBSub <- filter(CoefMapB,
                      Description %in% c('Seattle', "Arlington",  "San Jose", "Boston"),
                      MerGroup %in% c("Accessories", "Cooking", "Women", "Shoes"))

p1 <- p + geom_abline(data = CoefMapBSub, 
                       aes(intercept=Intercept, slope=Slope, color=MerGroup))
p1


# normally, your priors are the last periods posteriors ------------
# let's use those and open up the signmas

fit <- stan(model_code = stanMod,  data = list(
  N = nrow(SalesTransSummary),
  y = SalesTransSummary$TotSales, 
  x = SalesTransSummary$Wk,
  J = length(unique(SalesTransSummary$Description)),
  K = length(unique(SalesTransSummary$MerGroup)),
  Description = as.numeric(SalesTransSummary$Description),
  MerGroup = as.numeric(SalesTransSummary$MerGroup),
  
  p_alpha = as.numeric(CoefMapD[,2]), # Description intercept
  p_alpha2 = as.numeric(CoefMapM[,2]), # MerGroup intercept
  
  # 
  p_alphaSigma = 20000,
  p_alphaSigma2 = 20000,
  
  p_beta = as.numeric(CoefMapD[,3]), # Description slope (lmer model)
  p_beta2 = as.numeric(CoefMapM[,3]), # MerGroup slope (lmer model)
  
  p_betaSigma = 1000,
  p_betaSigma2 = 1000
  
), refresh = 0)


# Extract coefficients from stan model
sumFit <- data.frame(summary(fit))

# Description
Intercept1 <- summary(fit, pars = c("alpha"), probs = c(0.1, 0.9))$summary
# MerGroup
Intercept2 <- summary(fit, pars = c("alpha2"), probs = c(0.1, 0.9))$summary

# Description slope
Slope1 <- summary(fit, pars = c("beta"), probs = c(0.1, 0.9))$summary
# MerGroup slope
Slope2 <- summary(fit, pars = c("beta2"), probs = c(0.1, 0.9))$summary

# random effects for each Description
CoefMapD2 <- data.frame(Description = unique(SalesTransSummary$Description), 
                       DIntercept = Intercept1[,1], DSlope = Slope1[,1])
# random effects for each MerGroup
CoefMapM2 <- data.frame(MerGroup = unique(SalesTransSummary$MerGroup), MIntercept = Intercept2[,1], MSlope = Slope2[,1])

# add all intercepts and all slopes cross each row
CoefMapB2 = crossing(CoefMapD2, CoefMapM2) %>% 
  mutate(Intercept = DIntercept + MIntercept, Slope = DSlope + MSlope)

CoefMapBSub2 <- filter(CoefMapB2,
                      Description %in% c('Seattle', "Arlington",  "San Jose", "Boston"),
                      MerGroup %in% c("Accessories", "Cooking", "Women", "Shoes"))

p2 <- p + geom_abline(data = CoefMapBSub2, 
                     aes(intercept=Intercept, slope=Slope, color=MerGroup))
p2

# lets say that a big Macys opening in Seattle across the mall
# let's adjust the
CoefMapD2BU = CoefMapD2
CoefMapD2[10,2] = CoefMapD2[10,2] + 50000

fit <- stan(model_code = stanMod,  data = list(
  N = nrow(SalesTransSummary),
  y = SalesTransSummary$TotSales, 
  x = SalesTransSummary$Wk,
  J = length(unique(SalesTransSummary$Description)),
  K = length(unique(SalesTransSummary$MerGroup)),
  Description = as.numeric(SalesTransSummary$Description),
  MerGroup = as.numeric(SalesTransSummary$MerGroup),
  
  p_alpha = as.numeric(CoefMapD2[,2]), # Description intercept
  p_alpha2 = as.numeric(CoefMapM2[,2]), # MerGroup intercept
  
  # 
  p_alphaSigma = 5000,
  p_alphaSigma2 = 5000,
  
  p_beta = as.numeric(CoefMapD2[,3]), # Description slope (lmer model)
  p_beta2 = as.numeric(CoefMapM2[,3]), # MerGroup slope (lmer model)
  
  p_betaSigma = 100,
  p_betaSigma2 = 100
  
), refresh = 0)


# Extract coefficients from stan model
sumFit <- data.frame(summary(fit))

# Description
Intercept1 <- summary(fit, pars = c("alpha"), probs = c(0.1, 0.9))$summary
# MerGroup
Intercept2 <- summary(fit, pars = c("alpha2"), probs = c(0.1, 0.9))$summary

# Description slope
Slope1 <- summary(fit, pars = c("beta"), probs = c(0.1, 0.9))$summary
# MerGroup slope
Slope2 <- summary(fit, pars = c("beta2"), probs = c(0.1, 0.9))$summary

# random effects for each Description
CoefMapD3 <- data.frame(Description = unique(SalesTransSummary$Description), 
                       DIntercept = Intercept1[,1], DSlope = Slope1[,1])
# random effects for each MerGroup
CoefMapM3 <- data.frame(MerGroup = unique(SalesTransSummary$MerGroup), MIntercept = Intercept2[,1], MSlope = Slope2[,1])

# add all intercepts and all slopes cross each row
CoefMapB3 = crossing(CoefMapD3, CoefMapM3) %>% 
  mutate(Intercept = DIntercept + MIntercept, Slope = DSlope + MSlope)

CoefMapBSub3 <- filter(CoefMapB3,
                      Description %in% c('Seattle', "Arlington",  "San Jose", "Boston"),
                      MerGroup %in% c("Accessories", "Cooking", "Women", "Shoes"))

p3 <- p + geom_abline(data = CoefMapBSub, 
                      aes(intercept=Intercept, slope=Slope, color=MerGroup))
p3

