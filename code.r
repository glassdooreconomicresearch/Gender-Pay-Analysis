y###############################################################
##### How to Audit Your Gender Pay Gap:
##### An Employers Guide Using R
#####
##### by Andrew Chamberlain, Ph.D.
##### Glassdoor, Inc.
#####
##### March 2017
#####
##### Contact:
#####   Web: www.glassdoor.com/research
#####   Email: economics@glassdoor.com
###############################################################

# Clear workspace. 
rm(list=ls())

# Load R libraries.
library(stargazer)
library(broom)
library(dplyr)

# Turn off scientific notation. 
options(scipen=999) 

# Load data. 
data <- read.csv("https://glassdoor.box.com/shared/static/beukjzgrsu35fqe59f7502hruribd5tt.csv", stringsAsFactors=FALSE) # N = 1000 total observations. 



#############################
# Data Cleaning and Prep.
#############################

# Create five employee age bins.
data$age_bin <- 0
data$age_bin <- ifelse(data$age < 25, 1, data$age_bin) # Below age 25
data$age_bin <- ifelse(data$age >= 25 & data$age < 35, 2, data$age_bin) # Age 25-34.
data$age_bin <- ifelse(data$age >= 35 & data$age < 45, 3, data$age_bin) # Age 35-44.
data$age_bin <- ifelse(data$age >= 45 & data$age < 55, 4, data$age_bin) # Age 45-54.
data$age_bin <- ifelse(data$age >= 55, 5, data$age_bin) # Age 55+.

# Create total compensation variable (base pay + bonus)
data$totalPay <- data$basePay + data$bonus

# Take natural logarithm of compensation variables (for percentage pay gap interpretation in regressions).
data$log_base <- log(data$basePay, base = exp(1)) # Base pay.
data$log_total <- log(data$totalPay, base = exp(1)) # Total comp.
data$log_bonus <- log(data$bonus + 1, base = exp(1)) # Incentive pay. Add 1 to allow for log of 0 bonus values. 

# Create gender dummies (male = 1, female = 0. 
data$male <- ifelse(data$gender == "Male", 1, 0) # Male = 1, Female = 0.

# Check the structure of the imported data.
str(data)

# Cast all categorial variable as factors for the regression analysis. 
data$jobTitle <- as.factor(data$jobTitle)
data$gender <- as.factor(data$gender)
data$edu <- as.factor(data$edu)
data$dept <- as.factor(data$dept)


#############################
# Summary Statistics. 
#############################

# Create an overall table of summary statistics for the data.
stargazer(data, type = "html", out = "summary.htm", digits = 2)

# Base pay summary stats.
summary_base <- group_by(data, gender)
summary_base <- summarise(summary_base, meanBasePay = mean(basePay, na.rm = TRUE), medBasePay = median(basePay, na.rm = TRUE), cnt = sum(!(is.na(basePay))) )
View(summary_base)

# Total pay summary stats.
summary_total <- group_by(data, gender)
summary_total <- summarise(summary_total, meanTotalPay = mean(totalPay, na.rm = TRUE), medTotalPay = median(totalPay, na.rm = TRUE), cnt = sum(!(is.na(totalPay))) )
View(summary_total)

# Bonus summary stats. 
summary_bonus <- group_by(data,gender)
summary_bonus <- summarise(summary_bonus, meanBonus = mean(bonus, na.rm = TRUE), medBonus = median(bonus, na.rm = TRUE), cnt = sum(!(is.na(bonus))) )
View(summary_bonus)

# Performance evaluations summary stats. 
summary_perf <- group_by(data, gender)
summary_perf <- summarise(summary_perf, meanPerf = mean(perfEval, na.rm = TRUE), cnt = sum(!(is.na(perfEval))))
View(summary_perf)

# Departmental distribution of employees.
summary_dept <- group_by(data, dept, gender)
summary_dept <- summarise(summary_dept, meanTotalPay = mean(totalPay, na.rm = TRUE), cnt = sum(!(is.na(dept))) ) %>% arrange(desc(dept, gender))
View(summary_dept)

# Job title distribution of employees (Note: There is a disproportionate number of men in Manager and Software Engineer jobs).
summary_job <- group_by(data, jobTitle, gender)
summary_job <- summarise(summary_job, meanTotalPay = mean(totalPay, na.rm = TRUE), cnt = sum(!(is.na(jobTitle))) ) %>% arrange(desc(jobTitle, gender))
View(summary_job) 



###########################################################################################
# Model Estimation: OLS with controls. 
# Coefficient on "male" has the interpretation of approximate male pay advantage ("gender pay gap").
###########################################################################################

#############################
# Logarithm of Base Pay
#############################

# No controls. ("unadjusted" pay gap.)
model1 <- lm(log_base ~ male, data = data)
summary(model1)

# Adding "human capital" controls (performance evals, age and education).
model2 <- lm(log_base ~ male + perfEval + age_bin + edu, data = data)
summary(model2)

# Adding all controls. ("adjusted" pay gap.)
model3 <- lm(log_base ~ male + perfEval + age_bin + edu + dept + seniority + jobTitle, data = data)
summary(model3)

# Print log base pay "adjusted" gender pay gap and p-value.
logbase_pay_gap <- coef(model3)["male"] # male coefficient for adjusted pay gap.
logbase_pay_pvalue <- coef(summary(model3))["male", "Pr(>|t|)"] # associated p value. 
print(logbase_pay_gap)
print(logbase_pay_pvalue)

# Publish HTML stargazer table of regression results.
stargazer(model1, model2, model3, type = "html", out = "results.htm", omit = c("jobTitle", "dept", "edu"),
            add.lines = list(
              c("Controls:"),
              c("Education","No","Yes", "Yes"),
              c("Department","No", "No", "Yes"),
              c("Job Title", "No", "No", "Yes"),
              c("-----") )
          )
      

#############################
# Results by Department
# (Interaction of male x dept)
# To test for differences by department, examine significance of each "male x dept" coefficient.
# For the gender pay gap by department, add the "male" + "male x dept" coefficients from this model. 
#############################

# All controls with department interaction terms. 
dept_results <- lm(log_base ~ male*dept + perfEval + age_bin + edu + seniority + jobTitle, data = data)
summary(dept_results)
dept_results_clean <- tidy(dept_results) # Produce clean file of regression results without "*".

# Publish HTML stargazer tables of regression results. 
stargazer(dept_results, type = "html", out = "dept.htm", omit = c("jobTitle", "edu"))
write.csv(dept_results_clean, file = "dept_clean.csv") #



#############################
# Results by Job Title 
# (Interaction of male x job title) 
# To test for differences by job title, examine significance of each "male x job title" coefficient.
# For the gender pay gap by job title, add the "male" + "male x job title" coefficients from this model. 
#############################

# All controls with job title interaction terms. 
job_results <- lm(log_base ~ male*jobTitle + perfEval + age_bin + edu + seniority + dept, data = data)
summary(job_results)
job_results_clean <- tidy(job_results) # Produce clean file of regression results without "*".

# Push out HTML stargazer tables. 
stargazer(job_results, type = "html", out = "job.htm", omit = c("department", "edu"))
write.csv(job_results_clean, "job_clean.csv")

# For additional analysis via Oaxaca-Blinder decomposition, please see documentation for the "oaxaca" package in R.
# https://cran.r-project.org/web/packages/oaxaca/index.html

##### END ######
