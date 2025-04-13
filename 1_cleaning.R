# Bradley Wood-MacLean
# POLI 504 Final Project Replication Code

library(dplyr)
library(ggplot2)
library(tidyr)
library(marginaleffects)
library(modelsummary)
library(tidyverse)
library(lme4)
library(zoo)
library(ggpubr)
library(xtable)
library(scales)
library(rio)
library(fixest)
library(skimr)
library(specr)
library(sensemakr)
library(boot)
library(TOSTER)
library(sandwich)
library(ggeffects)
library(flextable)
library(officer)
library(psych)

# RQ: Does rural resentment, as conceptualized by Cramer (2016), predict place-based affective polarization in Canada?

# IV: Place-Based Resentment 

# DV: Place-Based Affective Polarization

###############################################################################
# Load the Data
###############################################################################

dat <- import("ccpd_survey_data/ccpd_survey.dta")

###############################################################################
# Part 1: Rural Resentment
###############################################################################

# Filter to rural respondents only (Cramer's theory is about rural resentment only so leave out the urbans and suburban peeps)
dat_rural <- dat %>% filter(placetype_best == 3)

# NOTE: DON'T FORGET Rural respondents were split into two blocks:
# - Half answered resentment toward urban people (resent*_ru) ← use these to measure Cramer-style rural resentment
# - Half answered resentment toward suburban people (resent*_rs) ← not used here
# Only use cases with non-missing resent*_ru to align with Cramer's original concept

###############################################################################
# Construct unified experimental variables
###############################################################################

dat_rural <- dat_rural %>%
  mutate(
    exp_policy = case_when(
      !is.na(jcu_placeid) | !is.na(jcr_placeid) | !is.na(jlu_placeid) | !is.na(jlr_placeid) ~ "anti_enviro",
      !is.na(ccu_placeid) | !is.na(ccr_placeid) | !is.na(clu_placeid) | !is.na(clr_placeid) ~ "pro_enviro",
      TRUE ~ NA_character_
    ),
    exp_party = case_when(
      !is.na(jlu_placeid) | !is.na(jlr_placeid) | !is.na(clu_placeid) | !is.na(clr_placeid) ~ "Liberal",
      !is.na(jcu_placeid) | !is.na(jcr_placeid) | !is.na(ccu_placeid) | !is.na(ccr_placeid) ~ "Conservative",
      TRUE ~ NA_character_
    ),
    exp_ur = case_when(
      !is.na(jcu_placeid) | !is.na(jlu_placeid) | !is.na(ccu_placeid) | !is.na(clu_placeid) ~ "urban",
      !is.na(jcr_placeid) | !is.na(ccr_placeid) | !is.na(jlr_placeid) | !is.na(clr_placeid) ~ "rural",
      TRUE ~ NA_character_
    ),
    exp_score_placeid = coalesce(jcu_placeid, jcr_placeid, jlu_placeid, jlr_placeid, 
                                 ccu_placeid, ccr_placeid, clu_placeid, clr_placeid), 
    exp_score_partyid = coalesce(jcu_partyid, jcr_partyid, jlu_partyid, jlr_partyid, 
                                 ccu_partyid, ccr_partyid, clu_partyid, clr_partyid),
    exp_score_trust = coalesce(jcu_trust, jcr_trust, jlu_trust, jlr_trust, 
                               ccu_trust, ccr_trust, clu_trust, clr_trust),
    exp_score_like = coalesce(jcu_like, jcr_like, jlu_like, jlr_like, 
                              ccu_like, ccr_like, clu_like, clr_like),
    treatment_group = case_when(
      !is.na(jcu_trust) ~ "jcu",
      !is.na(jcr_trust) ~ "jcr",
      !is.na(jlu_trust) ~ "jlu",
      !is.na(jlr_trust) ~ "jlr",
      !is.na(ccu_trust) ~ "ccu",
      !is.na(ccr_trust) ~ "ccr",
      !is.na(clu_trust) ~ "clu",
      !is.na(clr_trust) ~ "clr"
    )
  )

###############################################################################
# Construct DV: Place-Based Affective Polarization (directional)
# - Rural respondent: Rural - Urban
###############################################################################

dat_rural <- dat_rural %>%
  mutate(
    place_ap = therm1_3 - therm1_1  # Rural - Urban
  )

###############################################################################
# Construct IV: Rural Resentment (Cramer’s 3 dimensions)
###############################################################################

# Step 1: Clean and code only valid rural-to-urban resentment items
dat_rural <- dat_rural %>%
  mutate(across(starts_with("resent"), ~ na_if(., 9))) %>%
  mutate(
    resent_rep = resent4_ru,     # Representation resentment
    resent_econ = resent2_ru,    # Economic resentment
    resent_cult = resent3_ru     # Cultural resentment
  ) %>%
  mutate(
    # Rescale each dimension to 0–1
    resent_rep = rescale(resent_rep, to = c(0, 1), na.rm = TRUE),
    resent_econ = rescale(resent_econ, to = c(0, 1), na.rm = TRUE),
    resent_cult = rescale(resent_cult, to = c(0, 1), na.rm = TRUE)
  ) %>%
  mutate(
    # Final composite: average of the 3 dimensions
    rural_resentment = rowMeans(cbind(resent_rep, resent_econ, resent_cult), na.rm = TRUE)
  )

###############################################################################
# Plot distribution of rural resentment among rural respondents
###############################################################################

ggplot(dat_rural, aes(x = rural_resentment)) +
  geom_histogram(binwidth = 0.05, fill = "darkred", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = mean(rural_resentment, na.rm = TRUE)),
             color = "black", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Rural Resentment Among Rural Respondents",
    x = "Rural Resentment (0–1)",
    y = "Count"
  ) +
  theme_minimal()


###############################################################################
# Regression Models - Rural Resentment
###############################################################################

###############################################################################
# Basic 2 variable model with no covariates
###############################################################################

model1 <- feols(place_ap ~ rural_resentment, data = dat_rural)
summary(model1)

# Interpretation:
# Among rural Canadians, those who express greater rural resentment — the belief that rural people are underrepresented, disrespected, and devalued — show stronger affective bias against urban people (compared to their in-group).
# This supports Cramer’s theory that rural resentment is not just economic or cultural — it's emotional and group-based, and it drives political perception

###############################################################################
# Full model
###############################################################################

# Clean the control variables
dat_rural %>%
  select(
    rural_resentment,  # IV
    place_ap,          # DV
    ideology, 
    partisanship, 
    agecat, 
    gender, 
    education, 
    province            # FE
  ) %>%
  skim()

# Removing NAs from ideology
dat_rural <- dat_rural %>%
  mutate(ideology = na_if(ideology, 99))

# Making partisanship categorical
dat_rural <- dat_rural %>%
  mutate(
    partisanship = case_when(
      partisanship == 1 ~ "Bloc Québécois",
      partisanship == 2 ~ "Conservative",
      partisanship == 3 ~ "Green",
      partisanship == 4 ~ "Liberal",
      partisanship == 5 ~ "NDP",
      partisanship == 6 ~ "Other",
      partisanship == 7 ~ "None",
      partisanship == 10 ~ "PPC",
      partisanship == 9 ~ NA_character_,
      TRUE ~ NA_character_
    ),
    partisanship = factor(partisanship)
  )

# Making Gender Categorical
dat_rural <- dat_rural %>%
  mutate(
    gender = case_when(
      gender == 1 ~ "Man",
      gender == 2 ~ "Woman",
      gender == 4 ~ "Other",
      gender == 5 ~ NA_character_,  # Drop "Prefer not to answer"
      TRUE ~ NA_character_
    ),
    gender = factor(gender)
  )

dat_rural <- dat_rural %>%
  mutate(gender = relevel(gender, ref = "Woman")) # women as refernce category

# Making less groups for education
dat_rural <- dat_rural %>%
  mutate(
    education = case_when(
      education %in% c(1, 2) ~ "High school or less",
      education == 3         ~ "Some postsecondary",
      education == 4         ~ "College diploma",
      education %in% c(5, 6, 7) ~ "University degree",
      education == 9         ~ NA_character_,
      TRUE ~ NA_character_
    ),
    education = factor(education, levels = c("High school or less", "Some postsecondary", "College diploma", "University degree"))
  )


# Actually running the model
model2 <- feols(place_ap ~ rural_resentment + ideology + partisanship + agecat + gender + education | province, 
                data = dat_rural)
etable(model1, model2, se = "hetero")

modelsummary(
  list("Bivariate Model" = model1, "Full Model" = model2),
  stars = TRUE,
  statistic = "std.error",
  vcov = "HC1",
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Adj|Std.Errors",
  coef_map = c(
    "rural_resentment" = "Rural Resentment",
    "ideology" = "Ideology",
    "partisanshipConservative" = "Conservative",
    "partisanshipGreen" = "Green",
    "partisanshipLiberal" = "Liberal",
    "partisanshipNDP" = "NDP",
    "partisanshipNone" = "None",
    "partisanshipOther" = "Other",
    "partisanshipPPC" = "PPC",
    "agecat" = "Age Category",
    "genderMan" = "Man",
    "genderOther" = "Other Gender",
    "educationSome postsecondary" = "Some Postsecondary",
    "educationCollege diploma" = "College Diploma",
    "educationUniversity degree" = "University Degree"
  ),
  notes = list("Standard errors in parentheses", 
               "Province fixed effects included in Full Model")
)
# word format
library(modelsummary)

modelsummary(
  list("Bivariate Model" = model1, "Full Model" = model2),
  stars = TRUE,
  statistic = "std.error",
  vcov = "HC1",
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Adj|Std.Errors",
  coef_map = c(
    "rural_resentment" = "Rural Resentment",
    "ideology" = "Ideology",
    "partisanshipConservative" = "Conservative",
    "partisanshipGreen" = "Green",
    "partisanshipLiberal" = "Liberal",
    "partisanshipNDP" = "NDP",
    "partisanshipNone" = "None",
    "partisanshipOther" = "Other",
    "partisanshipPPC" = "PPC",
    "agecat" = "Age Category",
    "genderMan" = "Man",
    "genderOther" = "Other Gender",
    "educationSome postsecondary" = "Some Postsecondary",
    "educationCollege diploma" = "College Diploma",
    "educationUniversity degree" = "University Degree"
  ),
  output = "results_table.docx",
  notes = list("Standard errors in parentheses", 
               "Province fixed effects included in Full Model")
)

# Takeaway: higher rural resentment predicts higher place-based affective polarization

###############################################################################
# Part 2: Urban Resentment
###############################################################################

###############################################################################
# Filter to urban respondents only (Flipped test of Cramer’s concept: 
# Urban resentment toward rural people)
###############################################################################

dat_urban <- dat %>% filter(placetype_best == 1)

# NOTE: DON'T FORGET Urban respondents were split into two blocks:
# - Half answered resentment toward rural people (resent*_ur) ← use these to measure urban resentment
# - Half answered resentment toward suburban people (resent*_us) ← not used here
# Only use cases with non-missing resent*_ur to align construct

###############################################################################
# Construct unified experimental variables
###############################################################################

dat_urban <- dat_urban %>%
  mutate(
    exp_policy = case_when(
      !is.na(jcu_placeid) | !is.na(jcr_placeid) | !is.na(jlu_placeid) | !is.na(jlr_placeid) ~ "anti_enviro",
      !is.na(ccu_placeid) | !is.na(ccr_placeid) | !is.na(clu_placeid) | !is.na(clr_placeid) ~ "pro_enviro",
      TRUE ~ NA_character_
    ),
    exp_party = case_when(
      !is.na(jlu_placeid) | !is.na(jlr_placeid) | !is.na(clu_placeid) | !is.na(clr_placeid) ~ "Liberal",
      !is.na(jcu_placeid) | !is.na(jcr_placeid) | !is.na(ccu_placeid) | !is.na(ccr_placeid) ~ "Conservative",
      TRUE ~ NA_character_
    ),
    exp_ur = case_when(
      !is.na(jcu_placeid) | !is.na(jlu_placeid) | !is.na(ccu_placeid) | !is.na(clu_placeid) ~ "urban",
      !is.na(jcr_placeid) | !is.na(ccr_placeid) | !is.na(jlr_placeid) | !is.na(clr_placeid) ~ "rural",
      TRUE ~ NA_character_
    ),
    exp_score_placeid = coalesce(jcu_placeid, jcr_placeid, jlu_placeid, jlr_placeid, 
                                 ccu_placeid, ccr_placeid, clu_placeid, clr_placeid), 
    exp_score_partyid = coalesce(jcu_partyid, jcr_partyid, jlu_partyid, jlr_partyid, 
                                 ccu_partyid, ccr_partyid, clu_partyid, clr_partyid),
    exp_score_trust = coalesce(jcu_trust, jcr_trust, jlu_trust, jlr_trust, 
                               ccu_trust, ccr_trust, clu_trust, clr_trust),
    exp_score_like = coalesce(jcu_like, jcr_like, jlu_like, jlr_like, 
                              ccu_like, ccr_like, clu_like, clr_like),
    treatment_group = case_when(
      !is.na(jcu_trust) ~ "jcu",
      !is.na(jcr_trust) ~ "jcr",
      !is.na(jlu_trust) ~ "jlu",
      !is.na(jlr_trust) ~ "jlr",
      !is.na(ccu_trust) ~ "ccu",
      !is.na(ccr_trust) ~ "ccr",
      !is.na(clu_trust) ~ "clu",
      !is.na(clr_trust) ~ "clr"
    )
  )

###############################################################################
# Construct DV: Place-Based Affective Polarization (directional)
# - Urban respondent: Urban - Rural
###############################################################################

dat_urban <- dat_urban %>%
  mutate(
    place_ap = therm1_1 - therm1_3  # Urban - Rural
  )

###############################################################################
# Construct IV: Urban Resentment (Cramer’s 3 dimensions, flipped)
###############################################################################

# Step 1: Clean and code only valid urban-to-rural resentment items
dat_urban <- dat_urban %>%
  mutate(across(starts_with("resent"), ~ na_if(., 9))) %>%
  mutate(
    resent_rep = resent4_ur,     # Representation resentment
    resent_econ = resent2_ur,    # Economic resentment
    resent_cult = resent1_ur     # Cultural resentment
  ) %>%
  mutate(
    # Rescale each dimension to 0–1
    resent_rep = rescale(resent_rep, to = c(0, 1), na.rm = TRUE),
    resent_econ = rescale(resent_econ, to = c(0, 1), na.rm = TRUE),
    resent_cult = rescale(resent_cult, to = c(0, 1), na.rm = TRUE)
  ) %>%
  mutate(
    # Final composite: average of the 3 dimensions
    urban_resentment = rowMeans(cbind(resent_rep, resent_econ, resent_cult), na.rm = TRUE)
  )

###############################################################################
# Plot distribution of urban resentment among urban respondents
###############################################################################

ggplot(dat_urban, aes(x = urban_resentment)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = mean(urban_resentment, na.rm = TRUE)),
             color = "black", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Urban Resentment Among Urban Respondents",
    x = "Urban Resentment (0–1)",
    y = "Count"
  ) +
  theme_minimal()

###############################################################################
# Regression Models – Urban Resentment
###############################################################################

###############################################################################
# Basic 2 variable model with no covariates
###############################################################################

model3 <- feols(place_ap ~ urban_resentment, data = dat_urban)
summary(model3)

# Interpretation:
# Among urban Canadians, those who express greater urban resentment — the belief that rural people are overrepresented, outdated, or obstructive — show stronger affective bias against rural people (compared to their in-group).
# This suggests the possibility that place-based resentment also exists among urban residents and may play a symmetrical role in place-based affective polarization.

###############################################################################
# Full model with covariates and province fixed effects
###############################################################################

# Clean ideology
dat_urban <- dat_urban %>%
  mutate(ideology = na_if(ideology, 99))

# Recode partisanship
dat_urban <- dat_urban %>%
  mutate(
    partisanship = case_when(
      partisanship == 1 ~ "Bloc Québécois",
      partisanship == 2 ~ "Conservative",
      partisanship == 3 ~ "Green",
      partisanship == 4 ~ "Liberal",
      partisanship == 5 ~ "NDP",
      partisanship == 6 ~ "Other",
      partisanship == 7 ~ "None",
      partisanship == 10 ~ "PPC",
      partisanship == 9 ~ NA_character_,
      TRUE ~ NA_character_
    ),
    partisanship = factor(partisanship)
  )

# Recode gender
dat_urban <- dat_urban %>%
  mutate(
    gender = case_when(
      gender == 1 ~ "Man",
      gender == 2 ~ "Woman",
      gender == 4 ~ "Other",
      gender == 5 ~ NA_character_,
      TRUE ~ NA_character_
    ),
    gender = factor(gender)
  ) %>%
  mutate(gender = relevel(gender, ref = "Woman"))

# Recode education into fewer groups
dat_urban <- dat_urban %>%
  mutate(
    education = case_when(
      education %in% c(1, 2)      ~ "High school or less",
      education == 3              ~ "Some postsecondary",
      education == 4              ~ "College diploma",
      education %in% c(5, 6, 7)   ~ "University degree",
      education == 9              ~ NA_character_,
      TRUE ~ NA_character_
    ),
    education = factor(education, levels = c("High school or less", "Some postsecondary", "College diploma", "University degree"))
  )

# Run full model
model4 <- feols(place_ap ~ urban_resentment + ideology + partisanship + agecat + gender + education | province, 
                data = dat_urban)

# Display both models side-by-side
etable(model3, model4, se = "hetero")

# Takeaway: higher urban resentment also predicts stronger place-based affective polarization

###############################################################################
# Part 3: Showing off some methodological chops
###############################################################################

# Test 1: Moderation
model_moderation <- feols(
  place_ap ~ rural_resentment * ideology + partisanship + agecat + gender + education | province,
  data = dat_rural
)

model_moderation2 <- feols(
  place_ap ~ rural_resentment * partisanship + ideology + agecat + gender + education | province,
  data = dat_rural
)

etable(
  model2,                 # Baseline full model
  model_moderation,       # Interaction with ideology
  model_moderation2,      # Interaction with partisanship
  se = "hetero",          # Heteroskedasticity-robust SEs
  dict = c(               
    rural_resentment = "Rural Resentment",
    ideology = "Ideology",
    partisanship = "Partisanship",
    `rural_resentment:ideology` = "Resentment × Ideology",
    `rural_resentment:partisanshipConservative` = "Resentment × Conservative",
    `rural_resentment:partisanshipLiberal` = "Resentment × Liberal",
    `rural_resentment:partisanshipNDP` = "Resentment × NDP",
    `rural_resentment:partisanshipGreen` = "Resentment × Green",
    `rural_resentment:partisanshipPPC` = "Resentment × PPC",
    `rural_resentment:partisanshipNone` = "Resentment × None",
    `rural_resentment:partisanshipOther` = "Resentment × Other"
  )
)

# Test 2: Specification Curve Analysis 

specs_rural <- setup(
  data = dat_rural,
  y = "place_ap",  # DV
  x = "rural_resentment",  # IV
  model = "lm",  # switch back to lm from feols :-(
  controls = c("ideology", "partisanship", "agecat", "gender", "education", "province")
)

results_rural <- specr(specs_rural)

plot(results_rural)

# Test 3: Sensitivity Analysis for OVB

model_lm <- lm(place_ap ~ rural_resentment + ideology + partisanship + agecat + gender + education + factor(province),
               data = dat_rural)


sense <- sensemakr(model_lm,
                   treatment = "rural_resentment",
                   benchmark_covariates = c("ideology", "partisanshipConservative"),
                   kd = 1)

summary(sense)
plot(sense)

# Test 4: Placebo Test
# Run placebo regression: Does rural resentment predict feelings toward suburban people?
placebo_model <- feols(therm1_2 ~ rural_resentment + ideology + partisanship + agecat + gender + education | province,
                       data = dat_rural)

summary(placebo_model)

# save to word
modelsummary(
  list("Placebo Model: Anti-Suburban Affect" = placebo_model),
  stars = TRUE,
  statistic = "std.error",
  vcov = "HC1",
  coef_map = c(
    "rural_resentment" = "Rural Resentment",
    "ideology" = "Ideology",
    "partisanshipConservative" = "Conservative",
    "partisanshipGreen" = "Green",
    "partisanshipLiberal" = "Liberal",
    "partisanshipNDP" = "NDP",
    "partisanshipNone" = "None",
    "partisanshipOther" = "Other",
    "partisanshipPPC" = "PPC",
    "agecat" = "Age Category",
    "genderMan" = "Man",
    "genderOther" = "Other Gender",
    "educationSome postsecondary" = "Some Postsecondary",
    "educationCollege diploma" = "College Diploma",
    "educationUniversity degree" = "University Degree"
  ),
  notes = list(
    "Standard errors in parentheses",
    "Province fixed effects included"
  ),
  output = "placebo_model_table.docx"
)

# Test 5: Bootstrapping
# Define bootstrap function
bootstrap_rural_model <- function(data, indices) {
  # Resample the data
  d <- data[indices, ]
  
  # Fit model using lm (since boot doesn’t play well with feols)
  model <- lm(place_ap ~ rural_resentment + ideology + partisanship + agecat + gender + education + factor(province),
              data = d)
  
  # Return rural_resentment coefficient
  return(coef(model)["rural_resentment"])
}

# Run bootstrap with 1000 replications
set.seed(123)  # for reproducibility
boot_results <- boot(data = dat_rural, statistic = bootstrap_rural_model, R = 1000)

# Summarize results
print(boot_results)
boot.ci(boot_results, type = "perc")  # Percentile-based CI

# Plot distribution of bootstrapped estimates
plot(boot_results, main = "Bootstrap Distribution of Rural Resentment Effect")

# Test 6: Equivalence Test
# Define values directly
m <- 3.237            # estimate
se <- 0.6261          # standard error
n <- 493              # sample size
df <- n - 15          # approx. degrees of freedom
low_eqbound <- -0.5
high_eqbound <- 0.5
alpha <- 0.05

# Compute t-statistics for both one-sided tests
t1 <- (m - low_eqbound) / se
t2 <- (m - high_eqbound) / se

# p-values for one-sided tests
p1 <- pt(t1, df, lower.tail = FALSE)
p2 <- pt(t2, df, lower.tail = TRUE)

# Report
cat("TOST equivalence test\n")
cat("----------------------\n")
cat("t1 (vs. lower bound):", round(t1, 2), "| p =", round(p1, 4), "\n")
cat("t2 (vs. upper bound):", round(t2, 2), "| p =", round(p2, 4), "\n")
cat("Conclusion: ",
    ifelse(p1 < alpha & p2 < alpha,
           "Equivalent (reject null of non-equivalence)",
           "Not equivalent (fail to reject null of non-equivalence)"),
    "\n")

# Test 7: Cronbach's Alpha
alpha(dat_rural %>% select(resent_rep, resent_econ, resent_cult))

alpha(dat_urban %>% select(resent_rep, resent_econ, resent_cult))

###############################################################################
# Part 4: Some Plotting
###############################################################################

## Plot 1: Resentment Effects Across Key Models
# Make a list of your models
model_list <- list(
  "Rural Only" = model2,
  "Urban Only" = model4,
  "Moderation (Ideology)" = model_moderation,
  "Moderation (Partisanship)" = model_moderation2
)

# Create a coefficient map for cleaner labels
coef_labels <- c(
  "rural_resentment" = "Rural Resentment",
  "urban_resentment" = "Urban Resentment",
  "rural_resentment:ideology" = "Resentment × Ideology",
  "rural_resentment:partisanshipConservative" = "Resentment × CPC"
)

# Plot only relevant terms using `coef_omit` and `coef_map`
modelplot(model_list,
          coef_map = coef_labels,
          coef_omit = "^(?!rural_resentment|urban_resentment).*",  # Keep only key terms
          draw = TRUE) +
  labs(title = "Figure 3 Key Resentment Effects Across Models",
       x = "Coefficient Estimate",
       y = NULL) +
  theme_minimal()

## Plot 2: Resentment x Ideology Interaction

# Predict values for chosen ideology levels
plot_data <- ggpredict(model_moderation, terms = c("rural_resentment", "ideology [2, 5, 8]"))

# Clean up labels
plot_data$group <- factor(plot_data$group, 
                          levels = c("2", "5", "8"),
                          labels = c("Left (2)", "Center (5)", "Right (8)"))

# Plot
ggplot(plot_data, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, color = NA) +
  labs(
    title = "Moderation: Rural Resentment × Ideology",
    x = "Rural Resentment (0–1)",
    y = "Predicted Place-Based Affective Polarization",
    color = "Ideological Position",
    fill = "Ideological Position"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5)
  )

## Plot 3: Distribution Plots
# Combine rural and urban resentment scores into a long-format dataframe
resent_long <- bind_rows(
  dat_rural %>% select(resentment = rural_resentment) %>% mutate(group = "Rural"),
  dat_urban %>% select(resentment = urban_resentment) %>% mutate(group = "Urban")
)

# Plot the distributions
ggplot(resent_long, aes(x = resentment, fill = group)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Figure 1. Distribution of Resentment by Place Identity",
    x = "Resentment Score (0–1)",
    fill = "Group"
  ) +
  theme_minimal()

## Plot 4: Clean Specification Curve
plot(results_rural) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Specification Curve: Rural Resentment on Place Polarization")



###############################################################################
# Making Nice Tables
###############################################################################

# Generate the flextable and assign to object
table_flex <- modelsummary(
  list("Bivariate Model" = model1, "Full Model" = model2),
  stars = TRUE,
  statistic = "std.error",
  vcov = "HC1",
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Adj|Std.Errors",
  coef_map = c(
    "rural_resentment" = "Rural Resentment",
    "ideology" = "Ideology",
    "partisanshipConservative" = "Conservative",
    "partisanshipGreen" = "Green",
    "partisanshipLiberal" = "Liberal",
    "partisanshipNDP" = "NDP",
    "partisanshipNone" = "None",
    "partisanshipOther" = "Other",
    "partisanshipPPC" = "PPC",
    "agecat" = "Age Category",
    "genderMan" = "Man",
    "genderOther" = "Other Gender",
    "educationSome postsecondary" = "Some Postsecondary",
    "educationCollege diploma" = "College Diploma",
    "educationUniversity degree" = "University Degree"
  ),
  notes = list("Standard errors in parentheses", 
               "Province fixed effects included in Full Model"),
  output = "flextable"
)

# Fix: manually apply width to all columns using colnames
table_flex <- table_flex |>
  fontsize(size = 8, part = "all") |>        # reduce font a bit more
  autofit() |>
  width(j = NULL, width = 0.85) |>           # reduce width slightly further
  set_table_properties(layout = "autofit", width = 1)

# Export to Word
doc <- read_docx() |> 
  body_add_flextable(value = table_flex) |> 
  body_add_par(" ", style = "Normal")

print(doc, target = "resentment_results_flextable.docx")

# HTML version
modelsummary(
  list("Bivariate Model" = model1, "Full Model" = model2),
  stars = TRUE,
  statistic = "std.error",
  vcov = "HC1",
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Adj|Std.Errors",
  coef_map = c(
    "rural_resentment" = "Rural Resentment",
    "ideology" = "Ideology",
    "partisanshipConservative" = "Conservative",
    "partisanshipGreen" = "Green",
    "partisanshipLiberal" = "Liberal",
    "partisanshipNDP" = "NDP",
    "partisanshipNone" = "None",
    "partisanshipOther" = "Other",
    "partisanshipPPC" = "PPC",
    "agecat" = "Age Category",
    "genderMan" = "Man",
    "genderOther" = "Other Gender",
    "educationSome postsecondary" = "Some Postsecondary",
    "educationCollege diploma" = "College Diploma",
    "educationUniversity degree" = "University Degree"
  ),
  notes = list("Standard errors in parentheses", 
               "Province fixed effects included in Full Model"),
  output = "resentment_results_table.html"
)

# All models
modelsummary(
  list(
    "Full Model" = model2,
    "Interaction: Ideology" = model_moderation,
    "Interaction: Partisanship" = model_moderation2
  ),
  stars = TRUE,
  statistic = "std.error",
  vcov = "HC1",
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Adj|Std.Errors",
  coef_map = c(
    "rural_resentment" = "Rural Resentment",
    "ideology" = "Ideology",
    "rural_resentment:ideology" = "Resentment × Ideology",
    "partisanshipConservative" = "Conservative",
    "partisanshipGreen" = "Green",
    "partisanshipLiberal" = "Liberal",
    "partisanshipNDP" = "NDP",
    "partisanshipNone" = "None",
    "partisanshipOther" = "Other",
    "partisanshipPPC" = "PPC",
    "rural_resentment:partisanshipConservative" = "Resentment × CPC",
    "rural_resentment:partisanshipGreen" = "Resentment × Green",
    "rural_resentment:partisanshipLiberal" = "Resentment × Liberal",
    "rural_resentment:partisanshipNDP" = "Resentment × NDP",
    "rural_resentment:partisanshipNone" = "Resentment × None",
    "rural_resentment:partisanshipOther" = "Resentment × Other",
    "rural_resentment:partisanshipPPC" = "Resentment × PPC",
    "agecat" = "Age Category",
    "genderMan" = "Man",
    "genderOther" = "Other Gender",
    "educationSome postsecondary" = "Some Postsecondary",
    "educationCollege diploma" = "College Diploma",
    "educationUniversity degree" = "University Degree"
  ),
  notes = list("Standard errors in parentheses", 
               "Province fixed effects included in all models")
)


# With urban resentment

modelsummary(
  list(
    "Full Model (Rural)" = model2,
    "Interaction: Ideology" = model_moderation,
    "Interaction: Partisanship" = model_moderation2,
    "Urban Resentment Model" = model4
  ),
  stars = TRUE,
  statistic = "std.error",
  vcov = "HC1",
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Adj|Std.Errors",
  coef_map = c(
    # Top: Main resentment variables
    "rural_resentment" = "Rural Resentment",
    "urban_resentment" = "Urban Resentment",
    
    # Middle: Controls
    "ideology" = "Ideology",
    "partisanshipConservative" = "Conservative",
    "partisanshipGreen" = "Green",
    "partisanshipLiberal" = "Liberal",
    "partisanshipNDP" = "NDP",
    "partisanshipNone" = "None",
    "partisanshipOther" = "Other",
    "partisanshipPPC" = "PPC",
    "agecat" = "Age Category",
    "genderMan" = "Man",
    "genderOther" = "Other Gender",
    "educationSome postsecondary" = "Some Postsecondary",
    "educationCollege diploma" = "College Diploma",
    "educationUniversity degree" = "University Degree",
    
    # Bottom: Interaction terms
    "rural_resentment:ideology" = "Resentment × Ideology",
    "rural_resentment:partisanshipConservative" = "Resentment × CPC",
    "rural_resentment:partisanshipGreen" = "Resentment × Green",
    "rural_resentment:partisanshipLiberal" = "Resentment × Liberal",
    "rural_resentment:partisanshipNDP" = "Resentment × NDP",
    "rural_resentment:partisanshipNone" = "Resentment × None",
    "rural_resentment:partisanshipOther" = "Resentment × Other",
    "rural_resentment:partisanshipPPC" = "Resentment × PPC"
  ),
  notes = list("Standard errors in parentheses", 
               "Province fixed effects included in all models")
)

# save to word:

modelsummary(
  list(
    "Full Model (Rural)" = model2,
    "Interaction: Ideology" = model_moderation,
    "Interaction: Partisanship" = model_moderation2,
    "Urban Resentment Model" = model4
  ),
  stars = TRUE,
  statistic = "std.error",
  vcov = "HC1",
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Adj|Std.Errors",
  coef_map = c(
    # Main effects at top
    "rural_resentment" = "Rural Resentment",
    "urban_resentment" = "Urban Resentment",
    
    # Controls in the middle
    "ideology" = "Ideology",
    "partisanshipConservative" = "Conservative",
    "partisanshipGreen" = "Green",
    "partisanshipLiberal" = "Liberal",
    "partisanshipNDP" = "NDP",
    "partisanshipNone" = "None",
    "partisanshipOther" = "Other",
    "partisanshipPPC" = "PPC",
    "agecat" = "Age Category",
    "genderMan" = "Man",
    "genderOther" = "Other Gender",
    "educationSome postsecondary" = "Some Postsecondary",
    "educationCollege diploma" = "College Diploma",
    "educationUniversity degree" = "University Degree",
    
    # Interactions at bottom
    "rural_resentment:ideology" = "Resentment × Ideology",
    "rural_resentment:partisanshipConservative" = "Resentment × CPC",
    "rural_resentment:partisanshipGreen" = "Resentment × Green",
    "rural_resentment:partisanshipLiberal" = "Resentment × Liberal",
    "rural_resentment:partisanshipNDP" = "Resentment × NDP",
    "rural_resentment:partisanshipNone" = "Resentment × None",
    "rural_resentment:partisanshipOther" = "Resentment × Other",
    "rural_resentment:partisanshipPPC" = "Resentment × PPC"
  ),
  notes = list("Standard errors in parentheses", 
               "Province fixed effects included in all models"),
  output = "resentment_results_table.docx"  # << Save to Word here
)

# AP Denstity Plot
# Combine rural and urban into one dataframe
pbap_long <- bind_rows(
  dat_rural %>% select(place_ap) %>% mutate(group = "Rural"),
  dat_urban %>% select(place_ap) %>% mutate(group = "Urban")
)

# Remove NAs
pbap_long <- pbap_long %>% filter(!is.na(place_ap))

# Plot the distribution
ggplot(pbap_long, aes(x = place_ap, fill = group)) +
  geom_density(alpha = 0.5, bw = 1.5) +  # try larger bw like 1 or 1.5
  labs(
    title = "Figure 2. Distribution of Place-Based Affective Polarization",
    x = "Affective Polarization Score",
    y = "Density",
    fill = "Group"
  ) +
  scale_fill_manual(values = c("Rural" = "brown", "Urban" = "skyblue")) +
  theme_minimal()

###############################################################################
# Appendix Code
###############################################################################

# Appendix 8.3 Ideology Moderation Verification 
cor(as.numeric(dat_rural$ideology), as.numeric(dat_rural$partisanship), use = "complete.obs")

###############################################################################
# ABOVE HERE WORKS 
###############################################################################
