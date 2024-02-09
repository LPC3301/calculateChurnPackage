library("ChurnProbability")


#Data Prep
file_path_data_cust = "C:/Users/raffa/Desktop/University/Master/7. Semester/Day5/data_customer.csv"
data_cust <- fread(file_path_data_cust)

file_path_data_pers = "C:/Users/raffa/Desktop/University/Master/7. Semester/Day5/data_personal.csv"
data_pers <- fread(file_path_data_pers)


cust_pers_table <- merge(data_cust,
                         data_pers,
                         by="CustomerId",
                         all=TRUE)

cust_pers_table$Gender <- as.factor(cust_pers_table$Gender)
cust_pers_table$Exited <- as.factor(cust_pers_table$Exited)

formula <- formula("Exited ~ CreditScore + Gender + Age + Tenure + Balance + NumOfProducts + HasCrCard + IsActiveMember + EstimatedSalary")
logistic_model <- glm(formula, data = cust_pers_table, family = "binomial")


predictions <- predict(logistic_model, cust_pers_table, type="response")
cust_pers_table[, Churn_Prob := predictions]


cust_highest_churn_prob <- cust_pers_table[Churn_Prob==max(Churn_Prob)]
cust_lowest_churn_prob <- cust_pers_table[Churn_Prob==min(Churn_Prob)]

test_that("Compare highest churn_prob with lowest churn_prob",{
  expect_true(cust_highest_churn_prob[, Churn_Prob] > cust_lowest_churn_prob[, Churn_Prob])
  })
