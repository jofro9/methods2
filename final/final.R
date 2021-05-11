library(caret)
library(finalfit)
library(ggcorrplot)
library(ggplot2)
library(tableone)
library(tidyverse)

# define function to get predictive measures based on what model is input
get_predictive_measures = function(model_of_interest, model_name){
  r_squared = rcompanion::nagelkerke(model_of_interest)
  
  tibble(
    model = model_name,
    aic = AIC(model_of_interest),
    bic = BIC(model_of_interest),
    gen_Rsq = r_squared$Pseudo.R[2],
  )
}

data = read.csv("evanscounty.csv")

# table one
data_table1 = data

data_table1$chd <- as.factor(data_table1$chd)
levels(data_table1$chd) = c(
  `0` = "No Coronary Heart Disease" ,
  `1` = "Coronary Heart Disease"
)

data_table1$cat <- as.factor(data_table1$cat)
levels(data_table1$cat) = c(
  `0` = "Normal",
  `1` = "High"
)

data_table1$smk <- as.factor(data_table1$smk)
levels(data_table1$smk) = c(
  `0` = "Non-smoker" ,
  `1` = "smoker"
)

data_table1$ecg <- as.factor(data_table1$ecg)
levels(data_table1$ecg) = c(
  `0` = "Normal" ,
  `1` = "Abnormal"
)

data_table1$hpt <- as.factor(data_table1$hpt)
levels(data_table1$hpt) = c(
  `0` = "Normal" ,
  `1` = "High blood pressure"
)

names = dput(names(data_table1))
catvars = names[c(3, 7, 10)]
names = names[c(-1, -2)]

# table1 = CreateTableOne(vars = names, strata = "chd", data = data_table1, factorVars = catvars)
# 
# print(table1, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

data_table1 %>% summary_factorlist('chd', names, p = FALSE, na_include = TRUE)

# plots

# bar plot for counts of chd and cat
ggplot(data_table1, aes(x = chd, fill = cat)) +
  geom_bar(position = "stack") +
  ggtitle("Occurence of coronary heart disease by catecholamine level") +
  theme(axis.title.x = element_blank()) +
  labs(fill = "Catecholamine")

# correlations
corr = cor(data[-1])
ggcorrplot(corr)

# model with just reponse and outcomes
model0 = glm(chd ~ factor(cat), family = binomial, data = data)
output0 = summary(model0)

# model with all covariates
model_full = glm(chd ~ factor(cat) + factor(smk) + factor(ecg) + factor(hpt) + age + chl + dbp + sbp, family = binomial, data = data)
output_full = summary(model_full)

# model with insignificant covariates removed'
model1 = glm(chd ~ factor(cat) + factor(smk) + age + chl, family = binomial, data = data)
output1 = summary(model1)

# interaction model
model_int = glm(chd ~ factor(cat) + factor(smk) + age + chl + factor(cat) * chl, family = binomial, data = data)
output_int = summary(model_int)

measures1 = get_predictive_measures(model0, "traditional model")
measures2 = get_predictive_measures(model_full, "full model")
measures3 = get_predictive_measures(model1, "selection model")
measures4 = get_predictive_measures(model_int, "interaction model")

full_join(full_join(full_join(measures1, measures2), measures3), measures4)

# residuals
# model 0
data %>%
  mutate(pearson_resid = resid(model0, type = "pearson")) %>%
  ggplot(aes(id, pearson_resid)) +
  geom_hline(yintercept = 0, linetype = 2, color = "gray") +
  geom_point() +
  labs(x = "ID number", y = "Pearson residuals", title = "Pearson residuals")

data %>%
  mutate(deviance_resid = resid(model0, type = "deviance")) %>%
  ggplot(aes(id, deviance_resid)) +
  geom_hline(yintercept = 0, linetype = 2, color = "gray") +
  geom_point() +
  labs(x = "ID number", y = "Deviance residuals", title = "Deviance residuals")

# model 1
data %>%
  mutate(pearson_resid = resid(model1, type = "pearson")) %>%
  ggplot(aes(id, pearson_resid)) +
  geom_hline(yintercept = 0, linetype = 2, color = "gray") +
  geom_point() +
  labs(x = "ID number", y = "Pearson residuals", title = "Pearson residuals")

data %>%
  mutate(deviance_resid = resid(model1, type = "deviance")) %>%
  ggplot(aes(id, deviance_resid)) +
  geom_hline(yintercept = 0, linetype = 2, color = "gray") +
  geom_point() +
  labs(x = "ID number", y = "Deviance residuals", title = "Deviance residuals")

# confusion matrix
cmat = caret::confusionMatrix(
  data = factor(as.numeric(predict(model1, type = "response") > 0.5)),
  reference = factor(as.numeric(model1$y))
)

cmat$table
