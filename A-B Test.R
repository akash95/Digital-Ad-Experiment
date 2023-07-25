#Exercise 1

setwd("D:/")

#Q1.a
library(readxl)
data = read_xlsx("data.xlsx")

library(dplyr)
test = filter(data, test ==1)
control = filter(data, test ==0)

#Income
avg_test_income = mean(test$income)
avg_control_income = mean(control$income)

percent_diff_inc = 100*(avg_test_income - avg_control_income)/avg_control_income
t.test(test$income, control$income, var.equal = TRUE)

paste("Average of test group in income:", round(avg_test_income,2))
paste("Average of control group in income:", round(avg_control_income,2))

paste("Percent diff:", round(percent_diff_inc,2))

#p-value is not significant

#Gender
avg_test_gender = mean(test$gender)
avg_control_gender = mean(control$gender)

percent_diff_gender = 100*(avg_test_gender - avg_control_gender)/avg_control_gender
t.test(test$gender, control$gender, var.equal = TRUE)

paste("Average of test group in gender:", round(avg_test_gender,2))
paste("Average of control group in gender:", round(avg_control_gender,2))

paste("Percent diff:", round(percent_diff_gender,2))

#P-value is not significant

#Gamer
avg_test_gamer = mean(test$gamer)
avg_control_gamer = mean(control$gamer)

percent_diff_gamer = 100*(avg_test_gamer - avg_control_gamer)/avg_control_gamer
t.test(test$gamer, control$gamer, var.equal = TRUE)

paste("Average of test group in gamer:", round(avg_test_gamer,2))
paste("Average of control group in gamer:", round(avg_control_gamer,2))

paste("Percent diff:", round(percent_diff_gamer,2))

#p-value is not significant

#Q1.b

#Based on the results of the t-tests for the income, gender, and gamer variables, it appears that the differences between the test and control groups are not statistically significant. This means that there is not enough evidence to conclude that the treatment had a significant effect on any of these variables.

# This suggests that we fail to reject the null hypothesis that the true difference in means is zero, and conclude that there is no significant difference between the treatment and control groups in terms of income, gender and gaming variables 

#Q1.c

# If there is large and statistically significant difference between the test and control groups it suggests that the groups are not probabilistically equivalent. In such a case, we should try to identify the source of the difference and address it before running the experiment.
# The reasons could be sampling bias or counfounding variable.

#Q1.d 
#When dealing with large datasets, traditional statistical significance tests may not be appropriate. Instead, we can use methods like bootstrapping or permutation tests, which are computationally intensive but can handle large samples. Bootstrapping involves repeatedly resampling the data to generate many simulated samples, calculating the statistic of interest for each sample, and then calculating the confidence interval or p-value from the distribution of the statistic. Permutation tests involve randomly permuting the treatment labels and computing the statistic of interest for each permutation. The p-value is then calculated as the proportion of permutations that yield a statistic as extreme or more extreme than the observed statistic.

#Q2.a All customers

avg_test_purchase = mean(test$purchase)
avg_control_purchase = mean(control$purchase)

diff_all = avg_test_purchase - avg_control_purchase

paste("Avg test purchase rate overall: ", round(avg_test_purchase,2))
paste("Avg control purchase rate overall: ", round(avg_test_purchase,2))

paste("Absolute differenece overall: ", round(diff_all,2))

#Q2.b Male vs Female Customers

test_m = filter(test, gender == 1)
test_f = filter(test, gender == 0)
control_m = filter(control, gender == 1)
control_f = filter(control, gender == 0)

avg_t_m_p = mean(test_m$purchase)
avg_c_m_p = mean(control_m$purchase)
avg_t_f_p = mean(test_f$purchase)
avg_c_f_p = mean(control_f$purchase)

diff_m = avg_t_m_p - avg_c_m_p
diff_f = avg_t_f_p - avg_c_f_p

paste("Avg test purchase rate male : ", round(avg_t_m_p,2))
paste("Avg control purchase rate male: ", round(avg_c_m_p,2))
paste("Absolute difference male: ", round(diff_m,2))

paste("Avg test purchase rate female : ", round(avg_t_f_p,2))
paste("Avg control purchase rate female: ", round(avg_c_f_p,2))
paste("Absolute difference female: ", round(diff_f,2))

#Q2.c Gamers vs Non-Gamers Customers

test_g = filter(test, gamer == 1)
test_ng = filter(test, gamer == 0)
control_g = filter(control, gamer == 1)
control_ng = filter(control, gamer == 0)

avg_t_g_p = mean(test_g$purchase)
avg_c_g_p = mean(control_g$purchase)
avg_t_ng_p = mean(test_ng$purchase)
avg_c_ng_p = mean(control_ng$purchase)

diff_g = avg_t_g_p - avg_c_g_p
diff_ng = avg_t_ng_p - avg_c_ng_p

paste("Avg test purchase rate gamers : ", round(avg_t_g_p,2))
paste("Avg control purchase rate gamers: ", round(avg_c_g_p,2))
paste("Absolute difference gamers: ", round(diff_g,2))

paste("Avg test purchase rate non gamers : ", round(avg_t_ng_p,2))
paste("Avg control purchase rate non gamers: ", round(avg_c_ng_p,2))
paste("Absolute difference non gamers: ", round(diff_ng,4))

#Q2.d Female Gamers vs Male Gamers Customers

test_f_g = filter(test, gamer == 1 & gender == 0)
test_m_g = filter(test, gamer == 1 & gender == 1)
control_f_g = filter(control, gamer == 1 & gender == 0)
control_m_g = filter(control, gamer == 1 & gender == 1)

avg_t_f_g_p = mean(test_f_g$purchase)
avg_c_f_g_p = mean(control_f_g$purchase)
avg_t_m_g_p = mean(test_m_g$purchase)
avg_c_m_g_p = mean(control_m_g$purchase)

diff_f_g = avg_t_f_g_p - avg_c_f_g_p
diff_m_g = avg_t_m_g_p - avg_c_m_g_p

paste("Avg test purchase rate female gamers : ", round(avg_t_f_g_p,2))
paste("Avg control purchase rate female gamers: ", round(avg_c_f_g_p,2))
paste("Absolute difference female gamers: ", round(diff_f_g,2))

paste("Avg test purchase rate male gamers : ", round(avg_t_m_g_p,2))
paste("Avg control purchase rate male gamers: ", round(avg_c_m_g_p,2))
paste("Absolute difference male gamers: ", round(diff_m_g,2))


#Q3.a

#Expected revenue of all customers
count_t = nrow(test)
count_c = nrow(control)

exp_rev_t  = 37.5 * avg_test_purchase * count_t
exp_rev_c  = 37.5 * avg_control_purchase * count_c

pct_diff_ov = 100*(exp_rev_t-exp_rev_c)/exp_rev_c

paste("Expected revenue of test group overall customers", exp_rev_t)
paste("Expected revenue of control group overall customers", exp_rev_c)

paste("Percentage difference in expected revenue for overall customers", round(pct_diff_ov,2))


#Expected revenue of female gamers vs male gamers

count_t_f_g = nrow(test_f_g)
count_c_f_g = nrow(control_f_g)
count_t_m_g = nrow(test_m_g)
count_c_m_g = nrow(control_m_g)


exp_rev_t_f_g  = 37.5 * avg_t_f_g_p * count_t_f_g
exp_rev_c_f_g  = 37.5 * avg_c_f_g_p * count_c_f_g

pct_diff_f_g = 100*(exp_rev_t_f_g-exp_rev_c_f_g)/exp_rev_c_f_g

paste("Expected revenue of test group female gamers", exp_rev_t_f_g)
paste("Expected revenue of control group female gamers", exp_rev_c_f_g)

paste("Percentage difference in expected revenue for female gamer customers", round(pct_diff_f_g,2))

exp_rev_t_m_g  = 37.5 * avg_t_m_g_p * count_t_m_g
exp_rev_c_m_g  = 37.5 * avg_c_m_g_p * count_c_m_g

pct_diff_m_g = 100*(exp_rev_t_m_g-exp_rev_c_m_g)/exp_rev_c_m_g

paste("Expected revenue of test group male gamers", exp_rev_t_m_g)
paste("Expected revenue of control group male gamers", exp_rev_c_m_g)

paste("Percentage difference in expected revenue for male gamer customers", round(pct_diff_m_g,2))
