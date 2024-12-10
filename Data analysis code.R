# Final Project 
# (1) Data Cleaning

library(data.table)
library(ggplot2)
library(dplyr)
library(knitr)

med_provider <- fread("MUP_PHY_R24_P07_V10_D22_Prov.csv")
med_service <- fread("MUP_PHY_R24_P05_V10_D22_Prov_Svc.csv")
zip_info <- fread("20zpallagi.csv")

# working column selection
col_mental_provider <- c("Rndrng_NPI", # identifier
          "Rndrng_Prvdr_Last_Org_Name", # name 
          "Rndrng_Prvdr_Gndr", # gender
          "Rndrng_Prvdr_Ent_Cd", # organization type 
          "Rndrng_Prvdr_City", # city
          "Rndrng_Prvdr_State_Abrvtn", # state abbreviation
          "Rndrng_Prvdr_State_FIPS", # state code 
          "Rndrng_Prvdr_Zip5", # zip code 
          "Rndrng_Prvdr_RUCA", # rural urban classification
          "Rndrng_Prvdr_RUCA_Desc", # RUCA description
          "Rndrng_Prvdr_Cntry", # country 
          "Rndrng_Prvdr_Type", # provider type
          "Tot_Benes", # number of beneficiary 
          "Tot_Srvcs", # number of service 
          "Tot_Sbmtd_Chrg", # total charge
          "Tot_Mdcr_Alowd_Amt", # total allowed amount 
          "Tot_Mdcr_Stdzd_Amt", # amount paid after deductible/coinsurance
          "Med_Tot_Srvcs", # number of medical service
          "Med_Sbmtd_Chrg", # total charge for medical services 
          "Med_Mdcr_Alowd_Amt", # medicare allowed amout for medical services
          "Med_Mdcr_Pymt_Amt", # Total amount that Medicare paid
          "Med_Mdcr_Stdzd_Amt", # standardization of the Medicare payment
          "Bene_CC_BH_ADHD_OthCD_V1_Pct", # the percentage of ADHD
          "Bene_CC_BH_Anxiety_V1_Pct", # the percentage of Anxiety Disorders
          "Bene_CC_BH_Bipolar_V1_Pct", # the percentage of Bipolar Disorder
          "Bene_CC_BH_Mood_V2_Pct", # the percentage of Depression, Bipolar Or Other Depressive Mood Disorders
          "Bene_CC_BH_Depress_V1_Pct", # the percentage of Depressive Affective 
          "Bene_CC_BH_PD_V1_Pct", # the percentage of Personality Disorders
          "Bene_CC_BH_PTSD_V1_Pct", # the percentage of Post-Traumatic Stress Disorder
          "Bene_CC_BH_Schizo_OthPsy_V1_Pct") # the percentage of Schizophrenia And Other Psychotic Disorders

col_service <- c("Rndrng_NPI", # identifier
          "Rndrng_Prvdr_Gndr", # gender
          "Rndrng_Prvdr_Ent_Cd", # organization type 
          "Rndrng_Prvdr_City", # city
          "Rndrng_Prvdr_State_Abrvtn", # state abbreviation
          "Rndrng_Prvdr_State_FIPS", # state code 
          "Rndrng_Prvdr_Zip5", # zip code 
          "Rndrng_Prvdr_RUCA", # rural urban classification
          "Rndrng_Prvdr_RUCA_Desc", # RUCA description
          "Rndrng_Prvdr_Cntry", # country 
          "HCPCS_Cd", # HCPCS Code
          "HCPCS_Desc", # HCPCS Description
          "Tot_Benes", # Number of distinct Medicare beneficiaries
          "Tot_Bene_Day_Srvcs", # Number of distinct Medicare beneficiary
          "Avg_Sbmtd_Chrg", # average submitted charge
          "Avg_Mdcr_Stdzd_Amt") # Average Medicare Standardized Payment Amount

col_zip <- c("zipcode", # ZIPCODE
          "N1", # Number of returns 
          "N2", # Number of individuals 
          "A00100", # Adjust gross income (AGI)
          "N02650", # Number of returns with total income
          "A02650", # Total income amount
          "N02300", # Number of returns with unemployment compensation
          "A02300", # Unemployment compensation amount
          "N19300", # Number of returns with Home mortgage interest paid
          "A19300", # Home mortgage interest paid amount
          "N07225", # Number of returns with child and other dependent credit
          "A07225") # Child and other dependent credit amount
          
          
mental_provider_work <- med_provider[ , ..col_mental_provider]
med_service_work <- med_service[ , ..col_service]
zip_work <- zip_info[ , ..col_zip] 

# Calculate mental service proportion with each "V1" and "V2" grouping 
med_amount <- med_provider[ , c(1, 57:81)]
col_v1 <- colnames(med_amount)[grep("V1", colnames(med_amount))]
med_v1 <- med_amount[ , ..col_v1]
col_v2 <- colnames(med_amount)[grep("V2", colnames(med_amount))]
med_v2 <- med_amount[ , ..col_v2]
col_v3 <- colnames(mental_provider_work)[grep("V1", colnames(mental_provider_work))]
mental_v1 <- mental_provider_work[ , ..col_v3]
col_v4 <- colnames(mental_provider_work)[grep("V2", colnames(mental_provider_work))]
mental_v2 <- mental_provider_work[ , ..col_v4]

med_sum_v1 <- apply(med_v1, 1, sum, na.rm = TRUE)
med_sum_v2 <- apply(med_v2, 1, sum, na.rm = TRUE)
mental_sum_v1 <- apply(mental_v1, 1, sum, na.rm = TRUE)
mental_sum_v2 <- apply(mental_v2, 1, sum, na.rm = TRUE)


# Provider with high proportion of mental service 
mean_total <- mean(mental_sum_v2/med_sum_v2, na.rm = TRUE)
sd_total <- sd(mental_sum_v2/med_sum_v2, na.rm = TRUE)
# mental_provider_pp <- mean_total + 1.65 * sd_total
mental_logic <- (mental_sum_v2/med_sum_v2) > 0
mental_npi <- mental_provider_work[mental_logic, "Rndrng_NPI"]
mental_npi <- as.matrix(mental_npi)
mental_service_work <- med_service[Rndrng_NPI %in% mental_npi, ..col_service]

# HCPCS Code for mental health care
mental_pp <- mental_sum_v2/med_sum_v2
max_group <- which(mental_pp > 0.1)
max_group_NPI <- mental_provider_work[max_group, Rndrng_NPI]
max_group_HCPCS <- mental_service_work[Rndrng_NPI %in% max_group_NPI, ]
uni_HCPCS <- unique(cbind(max_group_HCPCS$HCPCS_Cd, max_group_HCPCS$HCPCS_Desc))
pattern <- "[Pp]sychotherapy|[Pp]sychiatric|emotional|behavioral|counseling| carbamazepine" # HCPCS_Desc related with mental health care
mental_HCPCS <- uni_HCPCS[grepl(pattern, uni_HCPCS[ ,2]), ]
pattern2 <- "obesity|[Ss]moking|cardiovascular" # remove not related with mental health care
mental_HCPCS <- mental_HCPCS[!grepl(pattern2, mental_HCPCS[ ,2]), ]
mental_cd <- mental_HCPCS[ , 1]
mental_service <- mental_service_work[HCPCS_Cd %in% mental_cd, ]


# RUCA Distinction 
ruca <- unique(cbind(mental_provider_work$Rndrng_Prvdr_RUCA, 
                     mental_provider_work$Rndrng_Prvdr_RUCA_Desc))
ruca <- as.data.frame(ruca)
ruca[ ,1] <- as.numeric(ruca[ ,1])
ruca <- ruca[order(ruca$V1), ]
ruca[ ,2]

# renaming and calculating average charge, average income and etc.  

names(mental_service) <- c("npi", "gender", "type", "city", "state", "fips", "zipcode",  "ruca", "ruca_desc",  "country",  "hcpcs_code", "hcpcs_desc", "num_bene", "distinct_num_bene", "average_charge", "standard_charge")

mental_service <- mental_service[ , .(gender, type, city, state, fips, zipcode, ruca, ruca_desc, country, num_bene = sum(num_bene), total_charge = sum(average_charge*num_bene), total_sd_charge=sum(standard_charge*num_bene)), by = npi]

mental_service <- mental_service[ , .(npi, gender, type, city, state, fips, zipcode, ruca, ruca_desc, country, num_bene, average_charge = total_charge/num_bene, sd_charge=total_sd_charge/num_bene)]

mental_service[ , zipcode := as.numeric(zipcode)]

names(zip_work) <- c("zipcode", "num_return", "num_individual", "adjust_income",   "num_total_return", "total_income", "num_unemployment", "unemployment_comp", "num_mortgage", "mortgage_interest", "num_child", "child_credit") 

zip_work <- zip_work[ , .(num_individual = sum(num_individual), adjust_income = sum(adjust_income*num_return), total_income=sum(total_income*num_total_return), num_unemployment = sum(num_unemployment), unemployment_comp = sum(unemployment_comp*num_unemployment), num_mortgage = sum(num_mortgage), mortgage_interest = sum(mortgage_interest*num_mortgage), num_child = sum(num_child), child_credit = sum(child_credit*num_child)), by = zipcode]

zip_work <- zip_work[ , .(zipcode, adjust_income = adjust_income/num_individual, total_income = total_income/num_individual, unemployment_comp = unemployment_comp/num_unemployment, mortgage_interest = mortgage_interest/num_mortgage, child_credit = child_credit/num_child)]

#table join 
mental_service_provider <- mental_service[zip_work, , on="zipcode", nomatch = NULL]

mental_dt <- mental_service_provider[ , .(npi, gender = as.factor(gender), type = as.factor(type), fips, zipcode, ruca, num_bene = as.numeric(num_bene), average_charge= as.numeric(average_charge), sd_charge = as.numeric(sd_charge), adjust_income, total_income, unemployment_comp, mortgage_interest, child_credit)]


# (2) Visualization 
# The data analysis is focused on 75% higher percentile mental care services by average_amount because most mental care services show similar average amount across zipcode. Zipcode is characterized by average adjust income. In income level is divided by 20 groups where the group 1 has the lowest income and the group 20 has highest income.

with(mental_dt, boxplot(average_charge ~ adjust_income))
with(mental_dt, boxplot(average_charge ~ ruca))
with(mental_dt, boxplot(average_charge ~ gender))
with(mental_dt, boxplot(average_charge ~ type))

# income range labeling by 20 groups 
for(i in 1:20) {
  low <- quantile(mental_dt$adjust_income, (i-1)/20)
  if (i != 20) {
    high <- quantile(mental_dt$adjust_income, i/20)
    mental_dt[(mental_dt$adjust_income >= low & mental_dt$adjust_income < high),   
              "income_label"] <- i
  } else {
    mental_dt[mental_dt$adjust_income >= low, "income_label"] <- i
  }
}

hist(mental_dt$income_label)
with(mental_dt, boxplot(average_charge ~ income_label))

# 75% upper group charge
top_charge <- quantile(mental_dt$average_charge, 0.75)
mentaltop <- mental_dt[mental_dt$average_charge >= top_charge, ]

for(i in 1:20) {
  low <- quantile(mentaltop$adjust_income, (i-1)/20)
  if (i != 20) {
    high <- quantile(mentaltop$adjust_income, i/20)
    mentaltop[(mentaltop$adjust_income >= low & mentaltop$adjust_income < high),   
               "Income_Range"] <- i
  } else {
    mentaltop[mentaltop$adjust_income >= low, "Income_Range"] <- i
  }
}

mentalTop_median <- mentaltop[ , .(top_median = median(average_charge)), by = top_label]
names(mentalTop_median) <- c("Income_Range", "average_charge")

# boxplot - Income Group 
ggplot(mentaltop, aes(x = as.factor(Income_Range), y = average_charge)) +
  geom_boxplot(aes(linetype = "Boxplot"), outlier.shape = "x", outlier.size = 4) +
  geom_line(aes(group = NA, linetype = "Median"), data = mentalTop_median,
            linewidth = 0.5, col = "red") +
  labs(title = "Charges in Top 25% of Charges",
       x = "Income Range",
       y = "Avereage Charge in USD",
       linetype = "") +
  scale_linetype_manual(values = c("Boxplot" = "solid", "Median" = "dashed")) +
  theme(legend.position = "inside",
        legend.position.inside = c(.1, .95),
        legend.background = element_blank())


# 1) comparison between urban and rural 
# The median of the average charge of mental health were very similar between urban and rural area. However, the hightest amount of the charges were concentrated in the Metropolitan area(RUCA 1~3). It is also showed that all the most of the high income zipcode which have higher than 50 percentile of income are concentrated in the urban area(Metroplitan area; RUCA 1~3 and Micropolitan area; RUCA 4~6). 

ruca_median <- mentaltop[ , .(ruca_median = median(average_charge)), by = ruca]
names(ruca_median) <- c("ruca", "average_charge")

# box plot - RUCA
ggplot(mentaltop, aes(x = as.factor(ruca), y = average_charge)) +
  geom_boxplot(aes(linetype = "Boxplot"), outlier.shape = "x", outlier.size = 4) +
  geom_line(aes(group = NA, linetype = "Median"), data = ruca_median,
            linewidth = 0.5, col = "red") +
  labs(title = "Charges in Top 25% of Charges Across Urban and Rural area",
       x = "RUCA",
       y = "Avereage Charge in USD",
       linetype = "") +
  scale_linetype_manual(values = c("Boxplot" = "solid", "Median" = "dashed")) +
  theme(legend.position = "inside",
        legend.position.inside = c(.1, .95),
        legend.background = element_blank())

# RUCA 1~3: Metro politan, 4~6: Micropolitan, 7~10: small town and Rural area
paste(ruca$V1, " ",ruca$V2)

# barplot - RUCA 
ruca_income <- with(mentaltop, table(ruca, Income_Range))
ruca_income_perc <- ruca_income/apply(ruca_income, 1, sum)
ruca_income_percm <- as.data.frame(ruca_income_perc)
ruca_income_percm$Income_Range <- factor(ruca_income_percm$Income_Range, levels = rev(unique(ruca_income_percm$Income_Range)))

ggplot(ruca_income_percm, aes(y = Freq, x = ruca, fill = Income_Range)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(y = "Income_Range", x = "RUCA", title = "Income_group Proportion across Urban and Rural Area by Zipcode") 


# 2) comparison between male and female provider 
mentaltop$gender[mentaltop$type == "O"] <- NA
filtered_mentaltop <- mentaltop %>% filter(!is.na(gender))
gender_median <- filtered_mentaltop[ , .(gender_median = median(average_charge)), by = gender]
names(gender_median) <- c("gender", "average_charge")

# box plot - Provider's Gender 
# Even for the high mental charge therapy there is not a significant difference between male and female provider. It is till show that male provider have higher portioin than female one in therapies with the highest charges. 

ggplot(filtered_mentaltop, aes(x = gender, y = average_charge)) +
  geom_boxplot(aes(linetype = "Boxplot"), outlier.shape = "x", outlier.size = 4) +
  geom_line(aes(group = NA, linetype = "Median"), data = gender_median,
            linewidth = 0.5, col = "red") +
  labs(title = "Charges in Top 25% of Charges by Provider's Gender",
       x = "Gender",
       y = "Avereage Charge in USD",
       linetype = "") +
  scale_linetype_manual(values = c("Boxplot" = "solid", "Median" = "dashed")) +
  theme(legend.position = "inside",
        legend.position.inside = c(.1, .95),
        legend.background = element_blank())

# barplot - Provider's Gender
# Provider's gender is equally distributed across all income levels by zipcode.

gender_income <- with(filtered_mentaltop, table(gender, Income_Range))
gender_income <- gender_income[2:3, ]
gender_income_perc <- gender_income/apply(gender_income, 1, sum)
gender_income_percm <- as.data.frame(gender_income_perc)
gender_income_percm$Income_Range <- factor(gender_income_percm$Income_Range, levels = rev(unique(gender_income_percm$Income_Range)))

ggplot(gender_income_percm, aes(y = Freq, x = gender, fill = Income_Range)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(y = "Income_Range", x = "Gender", title = "Income_group Proportion across Provider's Gender by Zipcode") 


# 3) comparison between Individual and Organization 
type_median <- mentaltop[ , .(type_median = median(average_charge)), by = type]
names(type_median) <- c("type", "average_charge")

# box plot - Provider's Entity
# Mental health care are maily provided by individual than organization provider. 

ggplot(mentaltop, aes(x = type, y = average_charge)) +
  geom_boxplot(aes(linetype = "Boxplot"), outlier.shape = "x", outlier.size = 4) +
  geom_line(aes(group = 1, linetype = "Median"), data = type_median,
            linewidth = 0.5, col = "red") +
  labs(title = "Charges in Top 25% of Charges by Provider's Entity",
       x = "Entity",
       y = "Avereage Charge in USD",
       linetype = "") +
  scale_linetype_manual(values = c("Boxplot" = "solid", "Median" = "dashed")) +
  theme(legend.position = "inside",
        legend.position.inside = c(.1, .95),
        legend.background = element_blank())

# barplot - Provider's Entity
# Even for the high income area, Individual mental health care providers were providing mental health care service. Individual providers were located more in high income area by zipcode. Orgnizational providers services were mostly related with mental treatments by medication. Its charges are not so high. 

type_income <- with(mentaltop, table(type, Income_Range))
type_income_perc <- type_income/apply(type_income, 1, sum)
type_income_percm <- as.data.frame(type_income_perc)
type_income_percm$Income_Range <- factor(type_income_percm$Income_Range, levels = rev(unique(type_income_percm$Income_Range)))

ggplot(type_income_percm, aes(y = Freq, x = type, fill = Income_Range)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(y = "Income_Range", x = "Entity", title = "Income_group Proportion across Provider's Entity by Zipcode") 

temp_work <- mental_service_work[HCPCS_Cd %in% mental_cd, ]
unique(temp_work$HCPCS_Desc[temp_work$Rndrng_Prvdr_Ent_Cd=="O"])

# 4) 75% line graph across income level by zipcode
# The income level is divided by 20 where the lowest is 1 and the highest is 20. The high income zipcode area(Income Range 18~20) show the higher amount charge for mental health care as typically expected. However, the difference between high income and low income zipcode area is not so significant even for the 75% high percentile mental care service. 

mentaltop$urban[mentaltop$ruca < 7] <- "Urban"
mentaltop$urban[mentaltop$ruca >= 7] <- "Rural"
ruca75 <- mentaltop[ , .(median_charge = median(average_charge)), by = .(Income_Range, urban)]
ruca75 <- ruca75[!is.na(ruca75$urban)]
names(ruca75) <- c("Income_Range", "Factor", "Median_Charge")

gender75 <- filtered_mentaltop[ , .(median_charge = median(average_charge)), by = .(Income_Range, gender)]
names(gender75) <- c("Income_Range", "Factor", "Median_Charge")

entity75 <- mentaltop[ , .(median_charge = median(average_charge)), by = .(Income_Range, type)]
names(entity75) <- c("Income_Range", "Factor", "Median_Charge")

mental75 <- bind_rows(ruca75, gender75, entity75)
# mental75 <- mental75[Factor != "Rural" & Factor != "O"] # Small Sample Size
ggplot(mental75, aes(x = Income_Range, y = Median_Charge, 
                     color = Factor)) +
  geom_line() +
  scale_y_continuous(name = "Median Charge in USD",
                     breaks = seq(0, 1000, by = 50)) +
  scale_x_continuous(name = "Income Range",
                     breaks = seq(1, 20, by = 1)) +
  labs(title = "Median Charges by each Provider Type") +
  scale_color_manual(values = 1:6) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(.1, .75),
    legend.background = element_blank(),
    legend.title = element_blank()
  )

# (3) Data Analysis 
# Because the average charges of the mental health care serive are not so different across zipcode so the R-square is not so high. However, "gender", "ruca", "Income_Range" have explain power for the average charge. Still for the high cost mental care service, male provider show higher average charges than female provider. In more concentrated metropolitan area and higher income range area, it is discovered that average charge for mental health care is higher. 
# num_bene, unemployment compensation and child_credit are added as control variables. 

# 1) Regression 

lm_mental <- lm(log(average_charge) ~ gender + ruca + Income_Range + num_bene+ adjust_income + unemployment_comp + child_credit, data = mentaltop)
summary(lm_mental)

lm_test1 <- lm(average_charge ~ gender + ruca + Income_Range + num_bene + unemployment_comp + mortgage_interest + child_credit, data = mentaltop)
summary(lm_test1)

lm_test2 <- lm(log(average_charge) ~ gender + log(ruca) + log(Income_Range)  + num_bene + unemployment_comp + mortgage_interest + child_credit, data = mentaltop)
summary(lm_test2)

lm_test3 <- lm(average_charge ~ gender + log(ruca) + log(Income_Range)  + num_bene + unemployment_comp + mortgage_interest + child_credit, data = mentaltop)
summary(lm_test3)

lm_test4 <- lm(log(average_charge) ~ gender + ruca + Income_Range + num_bene + unemployment_comp + child_credit, data = mentaltop)
summary(lm_test4)

lm_test5 <- lm(log(average_charge) ~ gender + ruca + adjust_income + num_bene + unemployment_comp + child_credit, data = mentaltop)
summary(lm_test5)

cbind(summary(lm_mental)$r.squared, summary(lm_test)$r.squared, summary(lm_test2)$r.squared, summary(lm_test3)$r.squared, summary(lm_test4)$r.squared, summary(lm_test5)$r.squared)

anova(lm_test4, lm_mental)
anova(lm_test5, lm_mental)

fit_mental <- lm_mental$fitted.values
resid_mental <- lm_mental$residuals
sresid_mental <- rstandard(lm_mental)

# Linearity Check
par(mfrow = c(1,3), mar=c(3,3,2,1), mgp = c(1.8,.5, 0))
plot(fit_mental,resid_mental, main = "Residuals vs Fitted Values", xlab = "Fitted values", ylab = "Residuals",  pch = 16)
abline(h = 0, lty = 2, col = "red")

#homoscedasticity 
plot(fit_mental, sresid_mental, main = "Stand Residuals vs Fitted Values", xlab = "Fitted Values", ylab = "Stand Residuals",  pch = 16)
abline(h = 0, lty = 2, col = "red")
plot(fit_mental, sqrt(abs(sresid_mental)), main = "sqrt(|Stand Residuals|) vs Fitted Values", xlab = "Fitted Values", ylab = "sqrt(Stand Residuals)",  pch = 16)

# Normality
hist(sresid_mental, main= "Standard Residuals", xlab ="Standard Residuals")
qqnorm(sresid_mental, pch = 16)
qqline(sresid_mental)
dev.off()

# 2) Result Table

install.packages("gt")
install.packages("broom")
library(gt)
library(broom)

# Tidy the model output
tidy_lm <- tidy(lm_mental)
glance_lm <- glance(lm_mental)

# Create a gt table for the coefficients
coef_table <- tidy_lm %>%
  gt() %>%
  tab_header(
    title = "log(Average Charge) Regression Results"
  ) %>%
  fmt_number(
    columns = c(estimate, std.error, statistic, p.value),
    decimals = 3
  ) %>%
  fmt_scientific(
    columns = estimate,
    decimals = 3
  )

# Extract R-squared and F-statistic
r_squared <- glance_lm$r.squared
adj_r_squared <- glance_lm$adj.r.squared
f_statistic <- glance_lm$statistic
p_value <- glance_lm$p.value

# Add model summary statistics to the table as rows
coef_table <- coef_table %>%
  tab_source_note(
    source_note = md(paste0("R-squared: ", round(r_squared, 3), "<br>",
                            "Adj. R-squared: ", round(adj_r_squared, 3), "<br>",
                            "F-statistic: ", round(f_statistic, 3), "<br>",
                            "P-value: ", format(p_value, scientific = TRUE)))
  )

# Print the table
print(coef_table)

# (conclusion) If The effect of num_bene, unemployment compensation and child_credit is explored, we can get more valuable information for the average charges of mental health care. 