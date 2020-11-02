library(car)
library(tidyverse)

users_churn_df = users_churn_df[c(2,4:11)]

logit = glm(is_churn ~ ., data = users_churn_df, family = "binomial")
influencePlot(logit)
summary(logit)

users_churn_df$is_churn <-  as.factor(users_churn_df$is_churn)

library(randomForest)
rf.user_churn = randomForest(is_churn ~ ., data = users_churn_df)

rf.user_churn

members_transact_df

library(mice)
md.pattern(members_transact_df)

members_transact_df = members_transact_df[!(names(members_transact_df) %in% 'X1')]
members_transact_df = members_transact_df[!(names(members_transact_df) %in% 'gender')]
members_transact_df = members_transact_df[!(names(members_transact_df) %in% 'bd')]
members_transact_df$is_churn <- as.factor(members_transact_df$is_churn)
members_transact_df$is_cancel <- as.factor(members_transact_df$is_cancel)
members_transact_df$is_auto_renew <- as.factor(members_transact_df$is_auto_renew)
members_transact_df$payment_method_id <- as.factor(members_transact_df$payment_method_id)
members_transact_df$registered_via <- as.factor(members_transact_df$registered_via)
members_transact_df$city <- as.factor(members_transact_df$city)

rf.members_transact = randomForest(is_churn ~ ., data = members_transact_df)
rf.members_transact
summary(rf.members_transact)
importance(rf.members_transact)

rf.features = data.frame(importance(rf.members_transact))
rf.features = rf.features %>% arrange(desc(MeanDecreaseGini))
rf.features


total_df <- merge(users_churn_df, members_transact_df, by = 'msno')
colnames(total_df)[21] = 'is_churn'
total_df = total_df[!(names(total_df) %in% 'is_churn.x')]

rf.total = randomForest(is_churn ~ ., data = total_df)

train = sample(1:nrow(members_transact_df), 7*nrow(members_transact_df)/10)
test = (-train)

rf.members_transact2 = randomForest(is_churn ~ ., data = members_transact_df[train,])
rf.members_transact2

y_pred = predict(rf.members_transact2, newdata = members_transact_df[test,])
table(as.numeric(y_pred) - 1, members_transact_df[test, 'is_churn'])

str(y_true)
str(y_pred)

y_pred = data.frame(y_pred)
y_true = data.frame(members_transact_df[test, 'is_churn'])

library(caret)
table(y_pred,y_true)

summary(y_pred)
summary(y_true)

as.character(y_true)

y_pred

colnames(y_pred) = "is_churn"

write_csv(y_pred, 'y_pred.csv')
write_csv(y_true, 'y_true.csv')
