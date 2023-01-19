# R Mortgage and HELOC Calculators
# Author: Chad Bell
# Date: 1/16/23

# Import Libraries
library(tidyverse)
library(lubridate)

#---------- Mortgage Calculator ----------
mortgage = function(loan_amount = 100000, principal = 0, APR = 5, years = 30) {
  # Total Number of Payments
  payments = years * 12
  # Outstanding principal of loan 
  outstanding_principal = loan_amount - principal
  # Monthly rate (convert APR to percentage and divide by 12)
  monthly_rate = APR / 100 / 12
  # Present Value of an annuity formulas (https://www.investopedia.com/retirement/calculating-present-and-future-value-of-annuities/)
  r = (1 + monthly_rate) ^ (payments) - 1
  # Monthly payment
  payment = outstanding_principal * monthly_rate * (r + 1) / r
  # Create a data frame for the amortization schedule
  amort_table <- data.frame("Date" = 0,
                            "Payment" = 0,
                            "Principal" = 0,
                            "Interest" = 0,
                            "Balance" = 0)
  # Loop through payments
  for (i in 1:payments) {
    # Recalculate values for each loop
    interest = outstanding_principal * monthly_rate
    principal = payment - interest
    outstanding_principal = outstanding_principal - principal
    # Add one month to the date
    pay_date = today() %m+% months(i - 1, abbreviate = FALSE)
    # Add row to the amortization table
    amort_table[i,] <- c(pay_date, payment, principal, interest, outstanding_principal)
  }
  # Format date column correctly
  class(amort_table$Date) <- "Date"
  
  View(amort_table)
}
# Test value
mortgage(loan_amount = 280000, APR = 4.87)

#---------- End of Mortgage Calculator ----------




#---------- HELOC Calculator ----------
HELOC = function(loan_amount = 100000, principal = 0, rate = 5, income = 1000, expenses = 500, start_date = Sys.Date(), interest_pay_day = 1, income_pay_day = 1, expenses_pay_day = 1) {
  # Outstanding principal of loan 
  outstanding_principal = loan_amount - principal
  # Monthly rate (convert rate to percentage and divide by 12)
  daily_rate = rate / 100 / 365.25

  # Create a data frame for the HELOC schedule
  HELOC_table <- data.frame("date" = 0,
                            "income" = 0,
                            "expenses" = 0,
                            "daily_interest" = 0,
                            "monthly_interest" = 0,
                            "balance" = 0)
  i = 1
  last_interest_pay_day = 1
  while (outstanding_principal > 0) {
    curr_date = start_date + days(i)
    curr_day = day(curr_date)
  
    
    daily_income = ifelse(curr_day == income_pay_day, income, 0)
    daily_expenses = ifelse(curr_day == expenses_pay_day, expenses, 0)
    outstanding_principal = outstanding_principal - daily_income + daily_expenses
    daily_interest = outstanding_principal * daily_rate
    
    if (curr_day == interest_pay_day) {
      monthly_interest = sum(HELOC_table$daily_interest[last_interest_pay_day:i - 1])
      last_interest_pay_day = i
    }
    else {
      monthly_interest = 0
    }
    
    outstanding_principal = outstanding_principal + monthly_interest
    HELOC_table[i,] <- c(curr_date, daily_income, daily_expenses, daily_interest, monthly_interest, outstanding_principal)
    i = i + 1
  }
  
  class(HELOC_table$date) <- "Date"
  
  View(HELOC_table)
  View(filter(HELOC_table, day(HELOC_table$date) == 1))
}
HELOC(loan_amount = 200000, principal = 0, rate = 4.5, income = 5000, expenses = 3000, start_date = as.Date("2023-01-01"), interest_pay_day = 1, income_pay_day = 1, expenses_pay_day = 1)





# Thoughts:
# Should we keep a running total of interest vs principal? 
# Re-factor the code. You probably don't need a daily table Make a better monthly table. That should speed it up. 
# Add the other options to the calculator