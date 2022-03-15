library(dplyr)

#loading dataset
data <- read.csv("C:\\Users\\fairy\\Desktop\\Replication\\Final_data.csv")
data

head(data)

#Columns description
# FEATURE: Usability = 0 and Security = 1
# THPARTY = No (only seller verified)
# ORDPREF = 1 (clear ordered preference selection) and 0 (if not)
# PROFFER = Price offered in the choice

str(data)

data$FEATURE <- as.factor(data$FEATURE)
data$WTP <- as.factor(data$WTP)
data$Scenario <- as.factor(data$Scenario)
data$ORDPREF <- as.factor(data$ORDPREF)
data$THPARTY <- as.factor(data$THPARTY)
str(data)

xtabs("~WTP + FEATURE + THPARTY", data = data)

lg <- glm(WTP ~ FEATURE, data = data, family = "binomial")
summary(lg)

lg1 <- glm(WTP ~ ., data = data, family = "binomial")
summary(lg1)

# filter to just security features == 1
security_feature_data = data %>% filter(FEATURE == 1) %>% select(-FEATURE)
security_feature_data %>% head()

## fit a new model on just the security-feature data
