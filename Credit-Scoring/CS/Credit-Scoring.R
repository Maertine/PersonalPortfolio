library(ggplot2)
library(tidyverse)
library(pROC)
library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(ranger)
theme_set(theme_bw())

set.seed(8)
data <- read.csv('cs-training.csv',row.names = 1)
training_index <- sample(seq_len(nrow(data)), size = 0.8 * nrow(data))

train_data <- data[training_index,]
test_data <- data[-training_index,]

### Monthly Income - Default Rate Plot
df_monthly_income_plot_data <- train_data %>%
  filter(!is.na(MonthlyIncome)) %>%
  mutate(IncomeBin = cut(MonthlyIncome,
                         breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000, 11000, 12000, Inf),
                         labels = c("<1k", "1k-2k", "2k-3k", "3k-4k", "4k-5k", "5k-6k", "6k-7k","7k-8k", "8k-9k", "9k-10k", "10k-11k","11k-12k", "12k+"),
                         right = FALSE)) %>%
  group_by(IncomeBin) %>%
  summarise(Default_Rate = mean(SeriousDlqin2yrs, na.rm = TRUE),
            Count = n())


ggplot(data=df_monthly_income_plot_data) +
  geom_bar(aes(x=IncomeBin,y=Count),stat="identity",fill='#4682B4',color='black') +
  geom_line(aes(x=IncomeBin,y=Default_Rate*100000,group = 1),stat="identity",linewidth=1.2,color='black') +
  scale_y_continuous(sec.axis=sec_axis(~.*1/100000,name="Default rate",breaks = seq(0, 0.15, by = 0.01))) +
  labs(x="Monthly Income",y="Number of observations")

model_loess_monthly_income <- loess(SeriousDlqin2yrs ~ MonthlyIncome, data = train_data, span=0.25)

monthly_income_dummy <- seq(0, 20000, length.out = 1000)
monthly_income_smooth_target <- predict(model_loess_monthly_income,data.frame(MonthlyIncome=monthly_income_dummy))

ggplot(data=data.frame(Default=monthly_income_dummy,Monthy_Income=monthly_income_smooth_target)) +
  geom_line(aes(x=Default,y=Monthy_Income),linewidth=1.2) +
  labs(x="Monthly Income",y="Default rate")


### Revolving Credit - Default Rate Plot

df_revolving_credit_plot_data <- train_data %>%
  mutate(CreditBin = cut(RevolvingUtilizationOfUnsecuredLines,
                         breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, Inf),
                         labels = c("<10%","10%-20%","20%-30%","30%-40%","40%-50%","50%-60%","60%-70%","70%-80%","80%-90%","90%-100%","100%+"),
                         right = FALSE)) %>%
  group_by(CreditBin) %>%
  summarise(Default_Rate = mean(SeriousDlqin2yrs, na.rm = TRUE),
            Count = n())


ggplot(data=df_revolving_credit_plot_data) +
  geom_bar(aes(x=CreditBin,y=Count),stat="identity",fill='#4682B4',color='black') +
  geom_line(aes(x=CreditBin,y=Default_Rate*120000,group = 1),stat="identity",linewidth=1.2,color='black') +
  scale_y_continuous(sec.axis=sec_axis(~.*1/120000,name="Default rate",breaks = seq(0, 0.5, by = 0.05))) +
  labs(x="Revolving Credit Utilization Ratio",y="Number of observations")


model_loess_revolving_credit <- loess(SeriousDlqin2yrs ~ RevolvingUtilizationOfUnsecuredLines, data = train_data %>% filter(RevolvingUtilizationOfUnsecuredLines<2), span=0.25)

revolving_credit_dummy <- seq(0, 1.5, length.out = 200)
revolving_credit_smooth_target <- predict(model_loess_revolving_credit,data.frame(RevolvingUtilizationOfUnsecuredLines=revolving_credit_dummy))

ggplot(data=data.frame(Default=revolving_credit_dummy,RevolvingUtilizationOfUnsecuredLines=revolving_credit_smooth_target)) +
  geom_line(aes(x=Default,y=RevolvingUtilizationOfUnsecuredLines),linewidth=1.2) +
  labs(x="Revolving Credit Utilization Ratio",y="Default rate")

### Age - Default Rate Plot
df_age_plot_data <- train_data %>%
  mutate(AgetBin = cut(age,
                       breaks = c(-Inf, 30, 40, 50, 60, 70, 80 ,Inf),
                       labels = c("<30", "30-39","40-49","50-59","60-69","70-79","80+"),
                       right = FALSE,)) %>%
  group_by(AgetBin) %>%
  summarise(Default_Rate = mean(SeriousDlqin2yrs, na.rm = TRUE),
            Count = n())

ggplot(data=df_age_plot_data) +
  geom_bar(aes(x=AgetBin,y=Count),stat="identity",fill='#4682B4',color='black') +
  geom_line(aes(x=AgetBin,y=Default_Rate*200000,group = 1),stat="identity",linewidth=1.2,color='black') +
  scale_y_continuous(sec.axis=sec_axis(~.*1/200000,name="Default rate",breaks = seq(0, 0.15, by = 0.01))) +
  labs(x="Age",y="Number of observations")

### Debt-to-Income Ratio - Default Rate Plot
df_debtratio_plot_data <- train_data %>%
  filter(MonthlyIncome > 500) %>%
  mutate(DebtRatiotBin = cut(DebtRatio,
                       breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 1, Inf),
                       labels = c("<10%", "10%-20%","20%-30%", "30%-40%", "40%-50%", "50%-100%","100%+"),
                       right = FALSE)) %>%
  group_by(DebtRatiotBin) %>%
  summarise(Default_Rate = mean(SeriousDlqin2yrs, na.rm = TRUE),
            Monthly_Income = median(MonthlyIncome, na.rm = TRUE),
            Count = n())

ggplot(data=df_debtratio_plot_data) +
  geom_bar(aes(x=DebtRatiotBin,y=Count),stat="identity",fill='#4682B4',color='black') +
  geom_line(aes(x=DebtRatiotBin,y=Default_Rate*120000,group = 1),stat="identity",linewidth=1.2,color='black') +
  scale_y_continuous(sec.axis=sec_axis(~.*1/120000,name="Default rate",breaks = seq(0, 0.15, by = 0.01))) +
  labs(x="Debt-to-Income Ratio",y="Number of observations")

### Open Loans and Lines - Default Rate Plot
df_openloandsalines_plot_data <- train_data %>%
  mutate(OpenAccountsBin = cut(NumberOfOpenCreditLinesAndLoans,
                             breaks = c(0, 2, 4, 7, 10, 15, 20, Inf),
                             labels = c("<2","2-3","4-6","7-9","10-14","15-19","20+"),
                             right = FALSE)) %>%
  group_by(OpenAccountsBin) %>%
  summarise(Default_Rate = mean(SeriousDlqin2yrs, na.rm = TRUE),
            Count = n())

ggplot(data=df_openloandsalines_plot_data) +
  geom_bar(aes(x=OpenAccountsBin,y=Count),stat="identity",fill='#4682B4',color='black') +
  geom_line(aes(x=OpenAccountsBin,y=Default_Rate*150000,group = 1),stat="identity",linewidth=1.2,color='black') +
  scale_y_continuous(sec.axis=sec_axis(~.*1/150000,name="Default rate",breaks = seq(0, 0.2, by = 0.02))) +
  labs(x="Total Open Accounts",y="Number of observations")

### Number of Real Estate Loans or Lines - Default Rate Plot
df_realestateloans_plot_data <- train_data %>%
  mutate(RealEstateLoansBin = cut(NumberRealEstateLoansOrLines,
                               breaks = c(0, 1, 2, 3 ,4, Inf),
                               labels = c("0","1","2","3","4+"),
                               right = FALSE)) %>%
  group_by(RealEstateLoansBin) %>%
  summarise(Default_Rate = mean(SeriousDlqin2yrs, na.rm = TRUE),
            Count = n())

ggplot(data=df_realestateloans_plot_data) +
  geom_bar(aes(x=RealEstateLoansBin,y=Count),stat="identity",fill='#4682B4',color='black') +
  geom_line(aes(x=RealEstateLoansBin,y=Default_Rate*300000,group = 1),stat="identity",linewidth=1.2,color='black') +
  scale_y_continuous(sec.axis=sec_axis(~.*1/300000,name="Default rate",breaks = seq(0, 0.2, by = 0.02))) +
  labs(x="Number of Real Estate Loans or Lines",y="Number of observations")

### Number Of Dependents - Default Rate Plot
df_dependents_plot_data <- train_data %>%
  filter(!is.na(NumberOfDependents)) %>%
  mutate(DependentsBin = cut(NumberOfDependents,
                                  breaks = c(0, 1, 2, 3 ,4, Inf),
                                  labels = c("0","1","2","3","4+"),
                                  right = FALSE)) %>%
  group_by(DependentsBin) %>%
  summarise(Default_Rate = mean(SeriousDlqin2yrs, na.rm = TRUE),
            Count = n())

ggplot(data=df_dependents_plot_data) +
  geom_bar(aes(x=DependentsBin,y=Count),stat="identity",fill='#4682B4',color='black') +
  geom_line(aes(x=DependentsBin,y=Default_Rate*700000,group = 1),stat="identity",linewidth=1.2,color='black') +
  scale_y_continuous(sec.axis=sec_axis(~.*1/700000,name="Default rate",breaks = seq(0, 0.2, by = 0.02))) +
  labs(x="Number of Dependents",y="Number of observations")

### Delinquency 30-59 - Default Rate Plot
df_delinquency59_plot_data <- train_data %>%
  mutate(DelinquencyBin = cut(NumberOfTime30.59DaysPastDueNotWorse,
                             breaks = c(0, 1, 2, 3 ,4, Inf),
                             labels = c("0","1","2","3","4+"),
                             right = FALSE)) %>%
  group_by(DelinquencyBin) %>%
  summarise(Default_Rate = mean(SeriousDlqin2yrs, na.rm = TRUE),
            Count = n())

ggplot(data=df_delinquency59_plot_data) +
  geom_bar(aes(x=DelinquencyBin,y=Count),stat="identity",fill='#4682B4',color='black') +
  geom_line(aes(x=DelinquencyBin,y=Default_Rate*200000,group = 1),stat="identity",linewidth=1.2,color='black') +
  scale_y_continuous(sec.axis=sec_axis(~.*1/200000,name="Default rate",breaks = seq(0, 0.7, by = 0.05))) +
  labs(x="Number of times the borrower has been 30–59 days past due in the last 24 months",y="Number of observations")

### Delinquency 60-89 - Default Rate Plot
df_delinquency89_plot_data <- train_data %>%
  mutate(DelinquencyBin = cut(NumberOfTime60.89DaysPastDueNotWorse,
                              breaks = c(0, 1, 2, 3 ,4, Inf),
                              labels = c("0","1","2","3","4+"),
                              right = FALSE)) %>%
  group_by(DelinquencyBin) %>%
  summarise(Default_Rate = mean(SeriousDlqin2yrs, na.rm = TRUE),
            Count = n())

ggplot(data=df_delinquency89_plot_data) +
  geom_bar(aes(x=DelinquencyBin,y=Count),stat="identity",fill='#4682B4',color='black') +
  geom_line(aes(x=DelinquencyBin,y=Default_Rate*200000,group = 1),stat="identity",linewidth=1.2,color='black') +
  scale_y_continuous(sec.axis=sec_axis(~.*1/200000,name="Default rate",breaks = seq(0, 0.7, by = 0.05))) +
  labs(x="Number of times the borrower has been 60-89 days past due in the last 24 months",y="Number of observations")

### Delinquency 90+ - Default Rate Plot
df_delinquency90_plot_data <- train_data %>%
  mutate(DelinquencyBin = cut(NumberOfTimes90DaysLate,
                              breaks = c(0, 1, 2, 3 ,4, Inf),
                              labels = c("0","1","2","3","4+"),
                              right = FALSE)) %>%
  group_by(DelinquencyBin) %>%
  summarise(Default_Rate = mean(SeriousDlqin2yrs, na.rm = TRUE),
            Count = n())

ggplot(data=df_delinquency90_plot_data) +
  geom_bar(aes(x=DelinquencyBin,y=Count),stat="identity",fill='#4682B4',color='black') +
  geom_line(aes(x=DelinquencyBin,y=Default_Rate*200000,group = 1),stat="identity",linewidth=1.2,color='black') +
  scale_y_continuous(sec.axis=sec_axis(~.*1/200000,name="Default rate",breaks = seq(0, 0.7, by = 0.05))) +
  labs(x="Number of times borrower has been 90 days or more past due",y="Number of observations")

### Total Debt
df_totaldebt_plot_data <- train_data %>%
  filter(MonthlyIncome > 500) %>%
  mutate(Debt = DebtRatio * MonthlyIncome * 12,
    DebtBin = cut(Debt,
                            breaks = c(0, 5000, 10000, 15000, 20000, 25000, 30000, 40000 ,50000, Inf),
                            labels = c("<5k", "5k-10k","10k-15k", "15k-20k", "20k-25k","25k-30k", "30k-40k", "40k-50k","50k+"),
                            right = FALSE)) %>%
  group_by(DebtBin) %>%
  summarise(Default_Rate = mean(SeriousDlqin2yrs, na.rm = TRUE),
            Monthly_Income = median(MonthlyIncome, na.rm = TRUE),
            Count = n())

ggplot(data=df_totaldebt_plot_data) +
  geom_bar(aes(x=DebtBin,y=Count),stat="identity",fill='#4682B4',color='black') +
  geom_line(aes(x=DebtBin,y=Default_Rate*200000,group = 1),stat="identity",linewidth=1.2,color='black') +
  scale_y_continuous(sec.axis=sec_axis(~.*1/200000,name="Default rate",breaks = seq(0, 0.7, by = 0.01))) +
  labs(x="Total Debt",y="Number of observations")


### Logistic Model
median(train_data$MonthlyIncome,na.rm=TRUE)

train_data_with_debt <- train_data %>%
  mutate(Debt = DebtRatio * MonthlyIncome * 12) %>%
  mutate(MonthlyIncome = if_else(is.na(MonthlyIncome),median(train_data$MonthlyIncome,na.rm=TRUE),MonthlyIncome),
         Debt = if_else(is.na(Debt),median(train_data$Debt,na.rm=TRUE),Debt),
         NumberOfDependents = if_else(is.na(NumberOfDependents),median(train_data$NumberOfDependents,na.rm=TRUE),NumberOfDependents)
         )

model <- glm(data=train_data_with_debt,SeriousDlqin2yrs~.,family='binomial')
model_2 <- glm(data=train_data_with_debt,SeriousDlqin2yrs~.^2,family='binomial')

summary(model)

true_labels = test_data$SeriousDlqin2yrs
test_data_with_debt <- test_data %>%
  mutate(Debt = DebtRatio * MonthlyIncome * 12) %>%
  mutate(MonthlyIncome = if_else(is.na(MonthlyIncome),median(train_data$MonthlyIncome,na.rm=TRUE),MonthlyIncome),
         Debt = if_else(is.na(Debt),median(train_data$Debt,na.rm=TRUE),Debt),
         NumberOfDependents = if_else(is.na(NumberOfDependents),median(train_data$NumberOfDependents,na.rm=TRUE),NumberOfDependents)
  )



predicted_probs <- predict(model, newdata = test_data_with_debt, type = "response")
predicted_probs_2 <- predict(model_2, newdata = test_data_with_debt, type = "response")

roc_obj <- roc(true_labels, predicted_probs)
roc_obj_2 <- roc(true_labels, predicted_probs_2)


plot(roc_obj,col = "blue", lwd = 2, asp = 1,xlim=c(1,0),ylin=c(0,1),xaxs = "i", yaxs = "i",
     xlab='True negative rate',ylab='True positive rate')
lines(roc_obj_2,col = "red", lwd = 2)

### XG-boost

task_train <- as_task_classif(train_data, target = "SeriousDlqin2yrs") 
task_test <- as_task_classif(test_data, target = "SeriousDlqin2yrs")

lrn_xgboost <- lrn("classif.xgboost",
                   predict_type = "prob",
                   nrounds = 200,
                   eta = 0.1,
                   max_depth = 6,
                   colsample_bytree = 1,
                   colsample_bylevel = 1,
                   lambda = 1,
                   alpha = 1,
                   subsample = 0.8)

kknn_pipe <- po("imputelearner", lrn("regr.rpart")) %>>% lrn("classif.kknn", predict_type = "prob", k=1000)
lrn_kkn <- GraphLearner$new(kknn_pipe)

nnet_pipe <- po("imputelearner", lrn("regr.rpart")) %>>%
  lrn("classif.nnet", predict_type = "prob", size = 10, decay = 0.01, rang = 0.5, maxit = 500)
lrn_nnet <- GraphLearner$new(nnet_pipe)


ranger_pipe <- po("imputelearner", lrn("regr.rpart")) %>>%
  lrn("classif.ranger", predict_type = "prob", num.trees = 500, mtry = 5, min.node.size = 10)
lrn_ranger <- GraphLearner$new(ranger_pipe)

lrn_xgboost$train(task_train)
pred_probs_xgboost <- lrn_xgboost$predict(task_test)$prob[, "1"]

roc_obj_xg_boost <- roc(true_labels, pred_probs_xgboost)
lines(roc_obj_xg_boost,col = "green", lwd = 2)

lrn_kkn$train(task_train)
pred_probs_kknn <- lrn_kkn$predict(task_test)$prob[,1]

roc_obj_kknn <- roc(true_labels, pred_probs_kknn)
lines(roc_obj_kknn,col = "cyan", lwd = 2)

lrn_nnet$train(task_train)
pred_probs_nnet <- lrn_nnet$predict(task_test)$prob[,1]

roc_obj_nnet <- roc(true_labels, pred_probs_nnet)
lines(roc_obj_nnet,col = "orange", lwd = 2)

lrn_ranger$train(task_train)
pred_probs_ranger <- lrn_ranger$predict(task_test)$prob[,1]

roc_obj_ranger <- roc(true_labels, pred_probs_ranger)
lines(roc_obj_ranger,col = "purple", lwd = 2)

legend("topright",
       legend = c("LogReg", "LogReg 2", "XG-Boost", "KNN", "NNet", "Ranger"),
       col = c("blue", "red", "green", "cyan", "orange", "purple"),
       lty = 1,
       cex = 0.8)

model_3 <- step(model_2)

summary(model_3)
predicted_probs_3 <- predict(model_3, newdata = test_data_with_debt, type = "response")
roc_obj_3 <- roc(true_labels, predicted_probs_3)
lines(roc_obj_3,col = "darkred", lwd = 2, lty = 3)


task_train_processed <- as_task_classif(train_data_with_debt, target = "SeriousDlqin2yrs") 
task_test_processed <- as_task_classif(test_data_with_debt, target = "SeriousDlqin2yrs")
lrn_xgboost_2 <- lrn("classif.xgboost",
                   predict_type = "prob",
                   nrounds = 200,
                   eta = 0.1,
                   max_depth = 6,
                   colsample_bytree = 1,
                   colsample_bylevel = 1,
                   lambda = 1,
                   alpha = 1,
                   subsample = 0.8)

lrn_xgboost_2$train(task_train_processed)
pred_probs_xgboost_2 <- lrn_xgboost_2$predict(task_test_processed)$prob[, "1"]

roc_obj_xg_boost_2 <- roc(true_labels, pred_probs_xgboost_2)
lines(roc_obj_xg_boost_2,col = "darkgreen", lwd = 2, lty=3)

xgboost_pipe <- po("mutate",
                   mutation = list(MonthlyIncomeMissing = ~as.integer(is.na(MonthlyIncome)),
                                   NumberOfDependentsMissing = ~as.integer(is.na(NumberOfDependents)))) %>>%
  po("imputelearner", learner = lrn("regr.rpart")) %>>%
  lrn("classif.xgboost",
      predict_type = "prob",
      nrounds = 200,
      eta = 0.1,
      max_depth = 6,
      colsample_bytree = 1,
      colsample_bylevel = 1,
      lambda = 1,
      alpha = 1,
      subsample = 0.8)
lrn_xgboost_pipe <- GraphLearner$new(xgboost_pipe)

lrn_xgboost_pipe$train(task_train)
pred_probs_xgboost_pipe <- lrn_xgboost_pipe$predict(task_test)$prob[, "1"]

roc_obj_xg_boost_pipe <- roc(true_labels, pred_probs_xgboost_pipe)
lines(roc_obj_xg_boost_pipe,col = "darkgreen", lwd = 2)



lda_pipe <- po("mutate",
                   mutation = list(MonthlyIncomeMissing = ~as.integer(is.na(MonthlyIncome)),
                                   NumberOfDependentsMissing = ~as.integer(is.na(NumberOfDependents)))) %>>%
  po("imputelearner", learner = lrn("regr.rpart")) %>>%
  lrn("classif.lda",predict_type = "prob")
lrn_lda <- GraphLearner$new(lda_pipe)

lrn_lda$train(task_train)
pred_probs_lda <- lrn_lda$predict(task_test)$prob[, "1"]

roc_obj_lda <- roc(true_labels, pred_probs_lda)
lines(roc_obj_lda, col = "pink", lwd = 2)
