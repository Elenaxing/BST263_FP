#install.packages(c("survey", "mgcv", "glmnet", "dplyr"))

library(survey)
library(mgcv)
library(glmnet)
library(dplyr)
library(pROC)

df <- readRDS("../data/brfss2024_clean.rds")

# create survey design object
options(survey.lonely.psu = "adjust")

brfss_design <- svydesign(
  id = ~PSU,
  strata = ~STSTR,
  weights = ~WEIGHT,
  data = df,
  nest = TRUE
)

# logistic regression
weighted_glm <- svyglm(
  MEDCOST1 ~ region +
    rurality +
    sex +
    agegroup +
    race +
    income +
    maritalstatus +
    education +
    employment +
    healthstatus +
    physhlth_days +
    menthlth_days +
    depression +
    chronic_count +
    any_disability +
    insurance +
    pers_doc +
    last_checkup,
  design = brfss_design,
  family = quasibinomial()
)
summary(weighted_glm)

unweighted_glm <- glm(
  MEDCOST1 ~  . - PSU - STSTR - WEIGHT,
  data = df,
  family = binomial()
)
summary(unweighted_glm)

weighted_glm_summary <- summary(weighted_glm)$coefficients
unweighted_glm_summary <- summary(unweighted_glm)$coefficients
weighted_glm_df <- data.frame(
  Variable = rownames(weighted_glm_summary),
  Weighted_Estimate = weighted_glm_summary[, 1],
  Weighted_SE = weighted_glm_summary[, 2],
  Weighted_p = weighted_glm_summary[, 4]
)

unweighted_glm_df <- data.frame(
  Variable = rownames(unweighted_glm_summary),
  Unweighted_Estimate = unweighted_glm_summary[, 1],
  Unweighted_SE = unweighted_glm_summary[, 2],
  Unweighted_p = unweighted_glm_summary[, 4]
)
comparison_glm_table <- merge(
  weighted_glm_df,
  unweighted_glm_df,
  by = "Variable"
)

comparison_glm_table

comparison_glm_table$Weighted_OR <- exp(comparison_glm_table$Weighted_Estimate)

comparison_glm_table$Unweighted_OR <- exp(comparison_glm_table$Unweighted_Estimate)

comparison_glm_table <- comparison_glm_table %>%
  mutate(across(where(is.numeric), round, 3))

comparison_glm_table

# logistic regression (with Lasso)
x <- model.matrix(
  MEDCOST1 ~ . - PSU - STSTR - WEIGHT,
  data = df
)[, -1]
y <- df$MEDCOST1

weighted_lasso <- cv.glmnet(
  x,
  y,
  family = "binomial",
  alpha = 1,
  weights = df$WEIGHT
)
coef(weighted_lasso, s = "lambda.1se")
plot(weighted_lasso$glmnet.fit, xvar = "lambda")
plot(weighted_lasso)
pred <- predict(weighted_lasso, newx = x, s = "lambda.1se", type = "response")
roc_obj <- roc(y, as.numeric(pred))
auc(roc_obj)

unweighted_lasso <- cv.glmnet(
  x,
  y,
  family = "binomial",
  alpha = 1
)
coef(unweighted_lasso, s = "lambda.1se")
plot(unweighted_lasso$glmnet.fit, xvar = "lambda")
plot(unweighted_lasso)
pred <- predict(unweighted_lasso, newx = x, s = "lambda.1se", type = "response")
roc_obj <- roc(y, as.numeric(pred))
auc(roc_obj)

lasso_out <- cbind(coef(weighted_lasso, s = "lambda.1se"),
                   coef(unweighted_lasso, s = "lambda.1se"))
colnames(lasso_out) <- c("Weighted", "Unweighted")
lasso_out

# GAM
df$survey_weight <- weights(brfss_design)
weighted_gam <- gam(
  MEDCOST1 ~
    s(physhlth_days) +
    s(menthlth_days) +
    s(chronic_count) +
    region +
    rurality +
    sex +
    agegroup +
    race +
    income +
    maritalstatus +
    education +
    employment +
    healthstatus +
    depression +
    any_disability +
    insurance +
    pers_doc +
    last_checkup,
  data = df,
  family = binomial(),
  weights = survey_weight
)

summary(weighted_gam)

unweighted_gam <- gam(
  MEDCOST1 ~
    s(physhlth_days) +
    s(menthlth_days) +
    s(chronic_count) +
    region +
    rurality +
    sex +
    agegroup +
    race +
    income +
    maritalstatus +
    education +
    employment +
    healthstatus +
    depression +
    any_disability +
    insurance +
    pers_doc +
    last_checkup,
  data = df,
  family = binomial()
)