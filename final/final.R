library(ggcorrplot)
library(ggplot2)
library(tableone)

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

table1 = CreateTableOne(vars = names, strata = "chd", data = data_table1, factorVars = catvars)

print(table1, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

# plots

# bar plot for counts of chd and cat
ggplot(data_table1, aes(x = chd, fill = cat)) +
  geom_bar(position = "stack") +
  ggtitle("Occurence of coronary heart disease by catecholamine level") +
  theme(axis.title.x = element_blank()) +
  labs(fill = "Catecholamine")

model0 = glm(chd ~ factor(cat) + , family = binomial, data = data)
output = summary(model0)

plot(residuals(model0))

