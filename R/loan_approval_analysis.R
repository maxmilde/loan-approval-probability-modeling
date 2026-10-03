library(dplyr)
library(tidyr)
library(ggplot2)
library(aod)
library(margins)
library(sandwich)
library(lmtest)

# 1) Data manipulation ---------------------------------------------------------
data_raw <- read.csv("data/state_NY.csv")
dim(data_raw)
head(data_raw, 2)
names(data_raw)
table(data_raw$action_taken)

data <- data_raw %>% filter(action_taken %in% c(1,3)) %>% #only take approved or denied
  mutate(approved = ifelse(action_taken==1, 1, 0))%>% 
  select(approved, loan_amount, loan_to_value_ratio, interest_rate, #selecting which vars we want
        loan_term, income, debt_to_income_ratio, property_value,
        applicant_age, applicant_sex, derived_race, derived_ethnicity,
        applicant_credit_score_type, occupancy_type, derived_dwelling_category, 
        total_units, ffiec_msa_md_median_family_income, tract_to_msa_income_percentage, loan_type, loan_purpose)
dim(data)

                          
data_main <- data %>% mutate(across(where(is.character), ~ na_if(., "Exempt"))) # put NA where there is Exempt in the column


data_main <- data_main %>% mutate( # format misformatted variables
  loan_to_value_ratio = as.numeric(loan_to_value_ratio),
  interest_rate = as.numeric(interest_rate),
  loan_term = as.numeric(loan_term),
  property_value = as.numeric(property_value),
  loan_type = as.numeric(loan_type),
  loan_purpose = as.numeric(loan_purpose),
  
  debt_to_income_ratio = factor(debt_to_income_ratio),
  applicant_age = factor(applicant_age),
  applicant_sex = factor(applicant_sex),
  derived_race = factor(derived_race),
  derived_ethnicity = factor(derived_ethnicity),
  applicant_credit_score_type = factor(applicant_credit_score_type),
  occupancy_type = factor(occupancy_type),
  derived_dwelling_category = factor(derived_dwelling_category),
  total_units = factor(total_units),
  loan_type = factor(loan_type),
  loan_purpose = factor(loan_purpose))
  
  

data_est <- data_main %>% drop_na(income, debt_to_income_ratio) %>% #important variables
                          filter(loan_purpose=="1") #we want only home purchase loans

str(data_est)
table(data_est$approved) #everything seems okay
mean(data_est$approved)

data_est <- data_est %>% filter(income>0) #we had negative income before

data_est <- data_est %>% mutate( # to make categories DTI
  dti_clean = case_when(
    as.character(debt_to_income_ratio)=="<20%" ~ "<20%",
    as.character(debt_to_income_ratio)=="20%-<30%" ~ "20%-<30%",
    as.character(debt_to_income_ratio)=="30%-<36%" ~ "30%-<36%",
    as.character(debt_to_income_ratio) %in% as.character(36:49) ~ "36%-<50%",
    as.character(debt_to_income_ratio)=="50%-60%" ~ "50%-60%",
    as.character(debt_to_income_ratio)==">60%" ~ ">60%"),
  dti_clean=factor(dti_clean, levels=c("<20%","20%-<30%","30%-<36%","36%-<50%","50%-60%",">60%")))


summary(data_est$derived_race)
data_est <- data_est %>% mutate( #fix the race categories
  race_clean = case_when(
    derived_race %in% c("2 or more minority races", "American Indian or Alaska Native", "Asian",
                        "Black or African American", "Native Hawaiian or Other Pacific Islander",
                        "White") ~ derived_race, 
    TRUE ~ "Other / Not reported")) #if not the races above
  

summary(data_est$derived_ethnicity)
data_est <- data_est %>% mutate( #fix the ethnicity categ.
  ethnicity_clean = case_when(
    derived_ethnicity %in% c("Hispanic or Latino", "Not Hispanic or Latino") ~ derived_ethnicity,
    TRUE ~ "Other / Not reported"))


#check some more key variables
summary(data_est[c("income", "loan_to_value_ratio", "interest_rate", "loan_amount")]) #loan_amount has a big outlier
q99 <- quantile(data_est$loan_amount, 0.99)
data_est <- data_est %>% filter(loan_amount <= q99)
data_est$loan_amount <- data_est$loan_amount/1000 #unit mismatch - not in 000s
data_est$ffiec_msa_md_median_family_income <- data_est$ffiec_msa_md_median_family_income/1000


table(data_est$applicant_age) 
data_est <- data_est %>% filter(applicant_age != "8888")
data_est$applicant_age <- droplevels(data_est$applicant_age)



str(data_est) #final check

data_est <- data_est %>% mutate(race_clean = as.factor(race_clean), #set some variables as factors
                                ethnicity_clean = as.factor(ethnicity_clean))
data_est <- data_est %>% select(-derived_ethnicity, -derived_race, -debt_to_income_ratio) #remove transformed columns


table(data_est$total_units, data_est$approved) #too many unnecessary categories in units_total
data_est <- data_est %>% mutate(
  total_units_clean=ifelse(total_units == "1", "1", "2+"),
  total_units_clean = factor(total_units_clean))
table(data_est$total_units_clean, data_est$approved)


table(data_est$derived_dwelling_category, data_est$approved) #not enough variation in multifamily so we drop it
data_est <- data_est %>% filter(derived_dwelling_category %in% 
                                  c("Single Family (1-4 Units):Manufactured",
                                    "Single Family (1-4 Units):Site-Built")) %>% droplevels()


summary(data_est$applicant_sex) #too many sex categories
data_est <- data_est %>% mutate(sex_clean = case_when(
  applicant_sex == "1" ~ "Male",
  applicant_sex == "2" ~ "Female",
  applicant_sex == "3" ~ "Joint")) %>% 
    filter(!is.na(sex_clean)) %>% 
    mutate(sex_clean=factor(sex_clean))
table(data_est$sex_clean, data_est$approved)


summary(data_est$loan_to_value_ratio) #extreme unreliable values
data_est <- data_est %>% filter(loan_to_value_ratio <= quantile(data_est$loan_to_value_ratio, 0.99, na.rm=TRUE))



#setting base levels for categorical variables
data_est$applicant_age <- relevel(data_est$applicant_age, ref="<25")
data_est$sex_clean <- relevel(data_est$sex_clean, ref="Male")
data_est$race_clean <- relevel(data_est$race_clean, ref="White")
data_est$ethnicity_clean <- relevel(data_est$ethnicity_clean, ref="Not Hispanic or Latino")
data_est$occupancy_type <- relevel(data_est$occupancy_type, ref="1")
data_est$total_units_clean <- relevel(data_est$total_units_clean, ref="1")
data_est$derived_dwelling_category <- relevel(data_est$derived_dwelling_category, ref="Single Family (1-4 Units):Site-Built")
data_est$dti_clean <- relevel(data_est$dti_clean, ref="<20%")
data_est$loan_type  <- relevel(data_est$loan_type, ref="1")
data_est$loan_purpose  <- relevel(data_est$loan_purpose, ref="1")



data_est$log_income <- log(data_est$income)
data_est$log_loan_amount <- log(data_est$loan_amount)

cont_mat <- data_est %>% transmute( 
    log_income = log_income,
    log_loan = log_loan_amount,
    ltv = loan_to_value_ratio,
    tract_pct = tract_to_msa_income_percentage,
    msa_mfi = ffiec_msa_md_median_family_income)
cor(cont_mat, use="complete.obs") #correl matrix for out cont variables



# 2) Summary Statistics --------------------------------------------------------
summary(data_est)

Z <- model.matrix(~ income + loan_amount + loan_to_value_ratio +  #so we get a nice summary statistics table
                    dti_clean + applicant_age + sex_clean +
                    total_units_clean + derived_dwelling_category +
                    tract_to_msa_income_percentage + ffiec_msa_md_median_family_income +
                    race_clean + ethnicity_clean,
                    data=data_est)
Z <- as.data.frame(Z)
summary_stats <- data.frame(obs=sapply(Z, function(x) sum(!is.na(x))),
                            mean=round(sapply(Z, function(x) mean(x, na.rm = TRUE)), 2),
                            sd=round(sapply(Z, function(x) sd(x, na.rm = TRUE)), 2),
                            min=round(sapply(Z, function(x) min(x, na.rm = TRUE)), 2),
                            max=round(sapply(Z, function(x) max(x, na.rm = TRUE)), 2))
summary_stats

table(data_est$approved)
sum(!is.na(data_est$income))
sum(!is.na(data_est$loan_amount))
sum(!is.na(data_est$loan_to_value_ratio))
sum(!is.na(data_est$ffiec_msa_md_median_family_income))
sum(!is.na(data_est$tract_to_msa_income_percentage))
sum(data_est$sex_clean=="Female")
sum(data_est$race_clean == "American Indian or Alaska Native")
sum(data_est$race_clean == "Asian")
sum(data_est$race_clean == "Black or African American")
sum(data_est$race_clean == "Native Hawaiian or Other Pacific Islander")
sum(data_est$ethnicity_clean == "Hispanic or Latino")


ggplot(data_est, aes(x=income, fill=factor(approved))) +  # Income distribution by approval
  scale_x_continuous(limits=c(0, quantile(data_est$income, 0.99))) + 
  geom_density(alpha=0.4) + labs(x="Applicant income", y="Density", fill="Approved")


ggplot(data_est, aes(x=loan_to_value_ratio, fill=factor(approved))) + #Loan to value ratio by approval
  geom_density(alpha=0.4) + 
  scale_x_continuous(limits=c(0, 150)) +
  labs(x="Loan-to-value Ratio", y="Density", fill="Approved")


ggplot(data_est, aes(x=dti_clean, y=approved)) + # Debt to income by approval rate
  stat_summary(fun=mean, geom="col") + coord_flip() + 
  labs(x="Debt-to-Income Category", y="Approval rate")


ggplot(data_est, aes(x=race_clean, y=approved)) + #Race by approval rate
  stat_summary(fun=mean, geom="col") + coord_flip() +
  labs(x="Race", y="Approval rate")

ggplot(data_est, aes(x=ethnicity_clean, y=approved)) + #Ethnicity by approval rate
  stat_summary(fun=mean, geom="col") + coord_flip() +
  labs(x="Ethnicity", y="Approval rate")


income_table <- data_est %>% group_by(approved) %>% summarise(
  min=min(income, na.rm=TRUE),
  mean=mean(income, na.rm=TRUE),
  median=median(income, na.rm=TRUE),
  max=max(income, na.rm=TRUE),
  sd=sd(income, na.rm=TRUE),
  N=n()) 
income_table
hist(data_est$income, breaks=10000, xlim=c(0, quantile(data_est$income, 0.99)), main="Applicant income", xlab="thousands USD")


# 3) Model Estimation ----------------------------------------------------------

#Restricted Logit (w/o ethnicity and race). once DTI is included log(income) coeff. becomes negative
m0_logit <- glm(approved ~ 
                  log_income + log_loan_amount + loan_to_value_ratio + dti_clean + #underwriting controls
                  applicant_age + sex_clean + #applicant controls
                  total_units_clean + derived_dwelling_category + #property controls
                  tract_to_msa_income_percentage + ffiec_msa_md_median_family_income, #area controls
                data=data_est, family=binomial(link="logit"))
summary(m0_logit)


#Unrestricted Logit
m1_logit <- update(m0_logit, . ~ . + race_clean + ethnicity_clean) #what we are after
summary(m1_logit)
sum(complete.cases(data_est[, all.vars(formula(m1_logit))])) #to see how many observations we are working with



#Restricted Probit (w/o ethnicity and race)
m0_probit <- glm(approved ~ 
                   log_income + log_loan_amount + loan_to_value_ratio + dti_clean + #underwriting controls
                   applicant_age + sex_clean + #applicant controls
                   total_units_clean + derived_dwelling_category + #property controls
                   tract_to_msa_income_percentage + ffiec_msa_md_median_family_income, #area controls
                 data=data_est, family=binomial(link="probit"))
summary(m0_probit)


#Unrestricted Probit
m1_probit <- update(m0_probit, . ~ . + race_clean + ethnicity_clean)
summary(m1_probit)




#Restricted comparison
coefficients_compare0 <- round(cbind("Logit R"=coef(m0_logit), "Probit R"=coef(m0_probit)),3)
coefficients_compare0


#Unrestricted comparison
coefficients_compare1 <- round(cbind("Logit U"=coef(m1_logit), "Probit U"=coef(m1_probit)),3)
coefficients_compare1





# 4) Marginal Effects ----------------------------------------------------------

##Logit
X <- model.matrix(m1_logit, data_est)
b <- coef(m1_logit) #to get b_hat from the model
xb <- as.vector(X %*% b) #X'b_hat

#continuous vars av. marg. eff. - derivative
f_bar <- mean(dlogis(xb)) #av. slope factor
cont_vars <- c("log_income", "log_loan_amount", "loan_to_value_ratio", 
               "ffiec_msa_md_median_family_income", "tract_to_msa_income_percentage") #we select the main cont. variables
ame_logit_cont <- f_bar*b[cont_vars] #Average Marginal Effects for cont. vars
ame_logit_cont

#categorical vars AME - av. discrete change vs reference
cat_vars <- c(
  "dti_clean20%-<30%", "dti_clean30%-<36%", "dti_clean36%-<50%", "dti_clean50%-60%", "dti_clean>60%",
  "race_clean2 or more minority races", "race_cleanAmerican Indian or Alaska Native", "race_cleanAsian",
  "race_cleanBlack or African American", "race_cleanNative Hawaiian or Other Pacific Islander",
  "race_cleanOther / Not reported",
  "ethnicity_cleanHispanic or Latino", "ethnicity_cleanOther / Not reported")

ame_dummy <- function(varname){
  i <- which(colnames(X)==varname) #get column i in X that corresp. to a given dummy var
  X1 <- X; X0 <- X #two copies of matrix X, where X1 dummy = 1, X0 dummy = 0
  X1[,i] <- 1
  X0[,i] <- 0
  mean(plogis(X1 %*% b) - plogis(X0 %*% b))}
ame_logit_cat <- sapply(cat_vars, ame_dummy)
ame_logit_cat #effect of being in category i vs reference



##Probit
X <- model.matrix(m1_probit, data_est)
b <- coef(m1_probit) 
xb <- as.vector(X %*% b) 

#continuous vars av. marg. eff. - derivative
f_bar <- mean(dnorm(xb)) 
ame_probit_cont <- f_bar*b[cont_vars]
ame_probit_cont

ame_dummy <- function(varname){
  i <- which(colnames(X)==varname) 
  X1 <- X; X0 <- X 
  X1[,i] <- 1
  X0[,i] <- 0
  mean(pnorm(X1 %*% b) - pnorm(X0 %*% b))}
ame_probit_cat <- sapply(cat_vars, ame_dummy)
ame_probit_cat 

# the marginal effects barely differ
ame_cont_table <- cbind("Logit U"=ame_logit_cont,
                        "Probit U"=ame_probit_cont)
ame_cont_table

ame_cat_table <- cbind("Logit U"=ame_logit_cat,
                       "Probit U"=ame_probit_cat)
ame_cat_table



###Second approach for AME table
#unrestricted models
vcov_rob_logit <- vcovHC(m1_logit, type="HC1")
vcov_rob_probit <- vcovHC(m1_probit, type="HC1")
coeftest(m1_logit, vcov=vcov_rob_logit)
coeftest(m1_probit, vcov=vcov_rob_probit)

me_logit1 <- margins(m1_logit, variables = c("log_income", #only the most key vars
                                             "log_loan_amount",
                                             "loan_to_value_ratio",
                                             "sex_clean",
                                             "ffiec_msa_md_median_family_income",
                                             "tract_to_msa_income_percentage",
                                             "race_clean",
                                             "ethnicity_clean"), vcov=vcov_rob_logit)
summary(me_logit1)
me_probit1 <- margins(m1_probit, variables = c("log_income", 
                                               "log_loan_amount",
                                               "loan_to_value_ratio",
                                               "sex_clean",
                                               "ffiec_msa_md_median_family_income",
                                               "tract_to_msa_income_percentage",
                                               "race_clean",
                                               "ethnicity_clean"), vcov=vcov_rob_probit)
summary(me_probit1)

#restricted models
vcov_rob_logit0 <- vcovHC(m0_logit, type="HC1")
vcov_rob_probit0 <- vcovHC(m0_probit, type="HC1")

me_logit0 <- margins(m0_logit, variables = c("log_income", #only the most key vars
                                             "log_loan_amount",
                                             "loan_to_value_ratio",
                                             "sex_clean",
                                             "ffiec_msa_md_median_family_income"),
                                              vcov=vcov_rob_logit0)
summary(me_logit0)
me_probit0 <- margins(m0_probit, variables = c("log_income", 
                                             "log_loan_amount",
                                             "loan_to_value_ratio",
                                             "sex_clean",
                                             "ffiec_msa_md_median_family_income"), 
                                              vcov=vcov_rob_probit0)
summary(me_probit0)


# 5) Model Evaluation ----------------------------------------------------------

# Likelihood Ratio Index
LRI_logit_0 <- with(m0_logit, deviance/null.deviance)
LRI_logit_1 <- with(m1_logit, deviance/null.deviance)
LRI_logit <- 1 - c(Restricted=LRI_logit_0, Unrestricted=LRI_logit_1)
round(LRI_logit, 3)

LRI_probit_0 <- with(m0_probit, deviance/null.deviance)
LRI_probit_1 <- with(m1_probit, deviance/null.deviance)
LRI_probit <- 1 - c(Restricted=LRI_probit_0, Unrestricted=LRI_probit_1)
round(LRI_probit, 3)




# 6 Hypothesis Testing ---------------------------------------------------------

##Likelihood Ratio Test

#change in deviance Logit
print("LR:")
with(m0_logit, deviance) - with(m1_logit, deviance)

#change in degrees of freedom
print("df:")
with(m0_logit, df.residual) - with(m1_logit, df.residual)

#p-value (Chi-sq test)
print("p-value:")
pchisq(with(m0_logit, deviance) - with(m1_logit, deviance), 
       with(m0_logit, df.residual) - with(m1_logit, df.residual), 
       lower.tail=FALSE)


#change in deviance Probit
print("LR:")
with(m0_probit, deviance) - with(m1_probit, deviance)

#change in degrees of freedom
print("df:")
with(m0_probit, df.residual) - with(m1_probit, df.residual)

#p-value (Chi-sq test)
print("p-value:")
pchisq(with(m0_probit, deviance) - with(m1_probit, deviance), 
       with(m0_probit, df.residual) - with(m1_probit, df.residual), 
       lower.tail=FALSE)



##Wald Test - Conditional on underwriting controls, do approval probab. differ by race/ethnicity relative to ref. groups
terms_restr_names <- c(
  "race_clean2 or more minority races", "race_cleanAmerican Indian or Alaska Native", "race_cleanAsian",
  "race_cleanBlack or African American", "race_cleanNative Hawaiian or Other Pacific Islander", "race_cleanOther / Not reported",
  "ethnicity_cleanHispanic or Latino", "ethnicity_cleanOther / Not reported")

terms_restr_logit <- which(names(coef(m1_logit)) %in% terms_restr_names) #make restr. terms selection for wald test
terms_restr_probit <- which(names(coef(m1_probit)) %in% terms_restr_names)

wald.test(b=coef(m1_logit), Sigma=vcov(m1_logit), Terms=terms_restr_logit) #joint significance of race/ethnicity regressors (Logit)
wald.test(b=coef(m1_probit), Sigma=vcov(m1_probit), Terms=terms_restr_probit) #(Probit)



























