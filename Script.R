library(tidyverse)
library(catboost)

# Load data--------------------------------------------------------------------
data <- read_csv("ks-projects-201801.csv")

# Clean up data----------------------------------------------------------------
data <- data %>%
  mutate(launched = as.Date(launched),
         target = as.numeric(state == "successful"),
         country = factor(country),
         category = factor(category))

data1 <- data %>%
  select(target, country, category, usd_goal_real, launched)

# Get Training, test, validation (2017) data------------------------------------
train1 <- data1 %>% filter(lubridate::year(launched) <= 2016) %>% select(-launched)
val <- data1 %>% filter(lubridate::year(launched) > 2016) %>% select(-launched)
trainid <- sample(nrow(train1), 0.7*nrow(train1))
train <- train1[trainid, ]
test <- train1[-trainid, ]

# Modeling-----------------------------------------------------------------------
pool <- catboost.load_pool(train[, -1], label = train$target, cat_features = 0:1)
pool_test <-  catboost.load_pool(test[, -1])
pool_val <- catboost.load_pool(val[, -1])

fit_params <- list(iterations = 100, 
                   thread_count = 10, 
                   loss_function = 'Logloss')

model <- catboost.train(pool, pool, fit_params)

# Evaluation-------------------------------------------------------------------
prediction <- catboost.predict(model, pool_test)
pred_val <- catboost.predict(model, pool_val)

auc <- function(score, label) {
  require(ROCR)
  pred <- prediction(score, label)
  auc <- performance(pred,  "auc")@y.values[[1]]
  auc
}

auc(prediction, test$target)

auc(pred_val, val$target)