data = data.frame(
  "strikes" = c("1", "2", "3"),
  "misconduct" = c(619, 355, 162),
  "no_misconduct" = c(1797, 416, 569)
)

# Part A
model1 = glm(
  cbind(misconduct, no_misconduct) ~ strikes, data, family = binomial
)

summary(model1)

# Part B
logLik(model1)

# Part C
model0 = glm(
  cbind(misconduct, no_misconduct) ~ 1, data, family = binomial
)

logLik(model0)

# Part D

