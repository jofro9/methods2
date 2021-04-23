n = 20
k = rep(1 / 100, n * 3)

data = data.frame(
  "hours" = c(8, 12, 16),
  "pos" = c(5, 10, 19)
)

data.glm = data.frame(
  "hours" = c(rep(data$hours[1], n), rep(data$hours[2], n), rep(data$hours[3], n)),
  "pos" = c(rep(1, data$pos[1]), rep(0, n - data$pos[1]), rep(1, data$pos[2]), rep(0, n - data$pos[2]), rep(1, data$pos[3]), rep(0, n - data$pos[3]))
)

# Part C
model1 = glm(pos ~ hours + offset(log(k)) - 1, family = binomial(link = "cloglog"), data = data.glm)
summary(model1)

# Part D
b1 = summary(model1)$coefficients[1, 1]
se = summary(model1)$coefficients[1, 2]
alpha = 0.05
z = qnorm(1 - (alpha / 2))
b1_ci = c(b1 - (z * se), b1 + (z * se))
theta = log(2) / b1
theta_ci = c(log(2) / b1_ci[1], log(2) / b1_ci[2])

# part E
model0 = glm(pos ~ offset(log(k)) - 1, family = binomial(link = "cloglog"), data = data.glm)
summary(model0)
anova(model1, model0, test = "LRT")
