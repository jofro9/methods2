library(tidyverse)

# Part A
n = 100000
s = 1
pde = 0.1
pdu = 0.05
pe = 0.3

b0 = log(pdu / (1 - pdu))
b1 = log(pde / (1 - pde) / (pdu / (1 - pdu)))

b0
b1

# Part B
set.seed(8675309)

x = rbinom(n, s, pe)
pee = plogis(b0 + b1 * x)
y = rbinom(n, s, pee)

data = data.frame(
  "id" = 1:n,
  "outcome" = y,
  "exposure" = x
)

# check population proportions
sume = 0
counte = 0
sumu = 0
countu = 0

for (i in 1:dim(data)[1]) {
  if (data$exposure[i] == 0) {
    sumu = sumu + data$outcome[i]
    countu = countu + 1
    
  } else {
    sume = sume + data$outcome[i]
    counte = counte + 1
  }
}

sumu / countu
sume / counte

# Part C
N = 50
b = 1000

datae = data[data$exposure == 1,]
datau = data[data$exposure == 0,]
estimates = data.frame(
  "seed" = rep(NA, b),
  "b0_hat" = rep(NA, b),
  "b1_hat" = rep(NA, b)
)

for (i in 1:b) {
  set.seed(i)
  exposed = sample(datae$outcome, N )
  unexposed = sample(datau$outcome, N)
  temp_dat = data.frame(
    "exposure" = c(rep(1, N), rep(0, N)),
    "outcome" = c(exposed, unexposed)
  )
  
  model = glm(outcome ~ exposure, family = binomial, data = temp_dat)
  estimates$seed[i] = i
  estimates$b0_hat[i] = summary(model)$coefficients[1, 1]
  estimates$b1_hat[i] = summary(model)$coefficients[2, 1]
}

mean(estimates$b0_hat)
mean(estimates$b1_hat)

median(estimates$b0_hat)
median(estimates$b1_hat)

# Part D (edit this!)
datad = data[data$outcome == 1,]
datan = data[data$outcome == 0,]
estimatesd = data.frame(
  "seed" = rep(NA, b),
  "b0_hat" = rep(NA, b),
  "b1_hat" = rep(NA, b)
)

for (i in 1:b) {
  set.seed(i)
  disease = sample(datad$exposure, N)
  nodisease = sample(datan$exposure, N)
  temp_dat = data.frame(
    "outcome" = c(rep(1, N), rep(0, N)),
    "disease" = c(disease, nodisease)
  )
  
  model = glm(outcome ~ disease, family = binomial, data = temp_dat)
  estimatesd$seed[i] = i
  estimatesd$b0_hat[i] = summary(model)$coefficients[1, 1]
  estimatesd$b1_hat[i] = summary(model)$coefficients[2, 1]  
}

mean(estimatesd$b0_hat)
mean(estimatesd$b1_hat)

median(estimatesd$b0_hat)
median(estimatesd$b1_hat)

# Part E
# cohort study
mad(estimates$b0_hat)
mad(estimates$b1_hat)

# case-control
mad(estimatesd$b0_hat)
mad(estimatesd$b1_hat)

