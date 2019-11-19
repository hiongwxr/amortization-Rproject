#In this project, we have to make an amortization table for a loan with price 200000. Firstly we have to set values(terms, loan price, annual interest rate) for the loan and then we hve to adjust annual interest rate to monthly interest rate. Secondly, we have to calculate the payment size by using the values we set. Finally, we use all the values we found to build amortization table. 

#set values
n <-10 
L <- 200000
i <- 0.03
monthly_rate <- (1+i)^(1/12)-1
conpound_terms <- 12
term <- 120
#calculate payment size
payment_size <- L/((1-(1/(1+monthly_rate))^(n*conpound_terms))/monthly_rate)

#amortization table
first_row <- c(0,0,L)
amortization_table <- matrix(ncol=3, nrow=1+term)
amortization_table[1,] <- first_row
for(time in 1: term) {
  interest_paid <- L*monthly_rate
  principal_repaid <- payment_size-interest_paid
  L <- L-principal_repaid
  amortization_table[time+1,] <- c(interest_paid,principal_repaid,L)
}

#label
rownames (amortization_table) = c (0,1:term)
colnames (amortization_table) = c("interest paid","principal repaid","outstanding balance")
amortization_table

