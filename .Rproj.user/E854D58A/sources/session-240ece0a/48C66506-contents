library("data.table")

calc_churn_prob  <- function(cust_table, cust_id){

  if (!(cust_id %in% cust_table$CustomerId)) {
    stop("Customer not found with the provided Customer ID")

  } else if (cust_id %in% cust_table$CustomerId){
    print("Customer Found. Calculating the Churn Probability...")

    #Factor the Gender and Exited Columns
    cust_table$Gender <- as.factor(cust_table$Gender)
    cust_table$Exited <- as.factor(cust_table$Exited)

    #Create a formula using the predictors
    formula <- formula("Exited ~ CreditScore + Gender + Age + Tenure + Balance + NumOfProducts + HasCrCard + IsActiveMember + EstimatedSalary")
    logistic_model <- glm(formula, data = cust_table, family = "binomial")

    #Predict the Model
    predictions <- predict(logistic_model, cust_table, type="response")
    cust_table[, Churn_Prob := predictions]

    return(cust_table[CustomerId==cust_id, Churn_Prob])
  }
}
