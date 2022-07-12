monthly_money <- function(income1_name, income1, # Person 1
                          income2_name, income2, # Person 2
                          common_fixed_fees, rent, food, # Expenses
                          common_savings_longterm, common_savings_shortterm # Savings
                          ){
  # Total income
  tot_income <- income1 + income2
  # Total fixed costes
  fixed_costs <- sum(common_fixed_fees, rent, common_savings_longterm, common_savings_shortterm, food)
  # Money left after the fixed cost are covered
  left_to_share <- tot_income - fixed_costs
  # Individual money
  left_to_spend <- left_to_share/2
  # Person 1's bill costs
  p1_pay <- income1-left_to_spend
  # Person 2's bill costs
  p2_pay <- income2-left_to_spend
  
  # Warning messages:
  if(fixed_costs>tot_income){
     warning("De fasta utgifterna är större än inkomsterna!")
  }
  if(p1_pay<0){
    print(paste0("NOTERA! ", income2_name, " ska betala alla fasta kostader och ", abs(p1_pay), "kr till ", income1_name,". ",
                 income1_name, " har en inkomst på ", income1, "kr och ", income2_name, " har en inkomst på ", income2,
                 "kr, tillsammans har ni alltså en inkomst på ", tot_income, "kr. De fasta utgifterna är ", fixed_costs,
                 " kr, vilket innebär att ni får ", left_to_spend, "kr efter att de fasta utgifterna är betalda. ", income2_name,
                 " ska betala ", p2_pay, "kr till den gemensamma kassan och ", abs(p1_pay), "kr till ", income1_name,"."))
  } else if(p2_pay<0){
    print(paste0("NOTERA! ", income1_name, " ska betala alla fasta kostader och ", abs(p2_pay), "kr till ", income2_name,". ",
                 income1_name, " har en inkomst på ", income1, "kr och ", income2_name, " har en inkomst på ", income2,
                 "kr, tillsammans har ni alltså en inkomst på ", tot_income, "kr. De fasta utgifterna är ", fixed_costs,
                 " kr, vilket innebär att ni får ", left_to_spend, "kr efter att de fasta utgifterna är betalda. ", income1_name,
                 " ska betala ", p1_pay, "kr till den gemensamma kassan och ", abs(p2_pay), "kr till ", income2_name,"."))
  } else {
    # "Normal" text to print
    print(paste0(income1_name, " har en inkomst på ", income1, "kr och ", income2_name, " har en inkomst på ", income2,
                 "kr, tillsammans har ni alltså en inkomst på ", tot_income, "kr. De fasta utgifterna är ", fixed_costs,
                 " kr, vilket innebär att ni får ", left_to_spend, "kr efter att de fasta utgifterna är betalda. ", income1_name,
                 " ska betala ", p1_pay, "kr och ", income2_name, " ska betala ", p2_pay, "kr till den gemensamma kassan."))
    }
  # Tables to print
  # 1. Expenses summary
  expen <- data.frame(Kostnad=c(common_fixed_fees, rent, food, sum(common_fixed_fees, rent, food)))
  rownames(expen) <- c("Fasta avgifter", "Hyra", "Matkostnader", "Summa fasta avgifter")
  
  # 2. Savings summary
  savings <- data.frame(Kostnad=c(common_savings_longterm, common_savings_shortterm, sum(common_savings_longterm, common_savings_shortterm)))
  rownames(savings) <- c("Långsiktigt sparande", "Kortsiktigt sparande", "Totalt sparande")
  
  # 3. Money distribution summary 
  dist <- data.frame(c(income1, left_to_spend, p1_pay), c(income2, left_to_spend, p2_pay))
  colnames(dist) <- c(income1_name, income2_name)
  rownames(dist) <- c("Inkomst", "Kvar efter fasta avgifter", "Att betala till gemensam kassa")
  
  # 4. Total summay
  tot <- data.frame(c(sum(common_fixed_fees, rent, food, common_savings_longterm, common_savings_shortterm),
                      income1+income2))
  rownames(tot) <- c("Fasta kostander", "Total inkomst")
  colnames(tot) <- c("Totalt")
  
  # List to return
  ret <- list(expen, savings, dist, tot)
  names(ret) <- c("Utgifter", "Sparande", "Fördelning", "Total")
  return(ret)
}

monthly_money(income1_name="P1", income1 = 10000, # Person 1
              income2_name="P2", income2 = 15000, # Person 2
              common_fixed_fees = 500, rent = 11000, food = 4000, # Expenses
              common_savings_longterm = 500, common_savings_shortterm = 1000 # Savings
              )
