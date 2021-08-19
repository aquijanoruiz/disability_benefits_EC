if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lmtest)) install.packages("lmtest", repos = "http://cran.us.r-project.org")
if(!require(sandwich)) install.packages("sandwich", repos = "http://cran.us.r-project.org")
if(!require(AER)) install.packages("AER", repos = "http://cran.us.r-project.org")
if(!require(stargazer)) install.packages("stargazer", repos = "http://cran.us.r-project.org")
if(!require(starpolishr)) install.packages("starpolishr", repos = "http://cran.us.r-project.org")

disability_ec <- readRDS("rds_files/disability_ec.rds")
disability_ec <- filter(disability_ec, between(age, 18, 64)) # filters people between 18 and 64

# -------- IV regressions on non-employment --------
# -------- OLS. dependent variable: non-employment --------

# MALES
# ols 1 -> males without controls
ols1_males <- lm(nonemployed ~ dis_id + as.factor(age) + education + marital_status + n_child_cat, 
                 data = disability_ec %>% filter(sex == "male"), weights = weight)

# ols 2 -> males + self assessment of the impairment
ols2_males <- lm(nonemployed ~ dis_id + as.factor(age) + education + marital_status + n_child_cat + dis_degree, 
                 data = disability_ec %>% filter(sex == "male"), weights = weight)

# ols 3 -> males + self assessment of the impairment + type of disability
ols3_males <- lm(nonemployed ~ dis_id + as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_type, 
                 data = disability_ec %>% filter(sex == "male"), weights = weight)

# FEMALES
# ols 1 -> males without controls
ols1_females <- lm(nonemployed ~ dis_id + as.factor(age) + education + marital_status + n_child_cat, 
                 data = disability_ec %>% filter(sex == "female"), weights = weight)

# ols 2 -> males + self assessment of the impairment
ols2_females <- lm(nonemployed ~ dis_id + as.factor(age) + education + marital_status + n_child_cat + dis_degree, 
                 data = disability_ec %>% filter(sex == "female"), weights = weight)

# ols 3 -> males + self assessment of the impairment + type of disability
ols3_females <- lm(nonemployed ~ dis_id + as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_type, 
                 data = disability_ec %>% filter(sex == "female"), weights = weight)

var_names <- c("Id holder", "Very difficult", "Extremely difficult", "Hearing", "Walking or walking up stars", 
               "Recalling or concentrating", "Bathing and dressing", "Communicating")

controls <- list(c("Age controls", "Yes","Yes", "Yes", "Yes","Yes", "Yes"), 
                 c("Education controls", "Yes","Yes", "Yes", "Yes","Yes", "Yes"),
                 c("Marital status controls", "Yes","Yes", "Yes", "Yes","Yes", "Yes"),
                 c("Num of children controls", "Yes","Yes", "Yes", "Yes","Yes", "Yes"))

stargazer(ols1_males, ols2_males, ols3_males, ols1_females, ols2_females, ols3_females, type = "text", align = TRUE, 
          keep = c("dis_id", "dis_degree", "dis_type"), covariate.labels = var_names, omit.stat = c("LL","ser","f"),
          dep.var.caption = "Dependent variable: non-employment", dep.var.labels.include = FALSE,
          add.lines = controls, column.labels = c("Men", "Women"), column.separate = c(3,3),
          title = "OLS regressions: relationship between disability benefits and non-employment")

# -------- first stage. independent variable: id_holder --------

# MALES
# first stage 1 -> males without controls
fs1_males <- lm(dis_id ~ dis_manuela + as.factor(age) + education + marital_status + n_child_cat, 
                 data = disability_ec %>% filter(sex == "male"), weights = weight)

# first stage 2 -> males + self assessment of the impairment
fs2_males <- lm(dis_id ~ dis_manuela + as.factor(age) + education + marital_status + n_child_cat + dis_degree, 
                 data = disability_ec %>% filter(sex == "male"), weights = weight)

# first stage 3 -> males + self assessment of the impairment + type of disability
fs3_males <- lm(dis_id ~ dis_manuela + as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_type, 
                 data = disability_ec %>% filter(sex == "male"), weights = weight)

# FEMALES
# first stage 1 -> males without controls
fs1_females <- lm(dis_id ~ dis_manuela + as.factor(age) + education + marital_status + n_child_cat, 
                   data = disability_ec %>% filter(sex == "female"), weights = weight)

# first stage 2 -> males + self assessment of the impairment
fs2_females <- lm(dis_id ~ dis_manuela + as.factor(age) + education + marital_status + n_child_cat + dis_degree, 
                   data = disability_ec %>% filter(sex == "female"), weights = weight)

# first stage 3 -> males + self assessment of the impairment + type of disability
fs3_females <- lm(dis_id ~ dis_manuela + as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_type, 
                   data = disability_ec %>% filter(sex == "female"), weights = weight)

stargazer(fs1_males, fs2_males, fs3_males, fs1_females, fs2_females, fs3_females, type = "text", align = TRUE, 
          keep = c("dis_manuela"), covariate.labels = c("Mision Manuela Espejo"), omit.stat = c("LL","ser","f"),
          dep.var.caption = "Dependent variable: Id holder", dep.var.labels.include = FALSE,
          add.lines = controls, column.labels = c("Men", "Women"), column.separate = c(3,3),
          title = "First stage: relationship between Mision Manuela Espejo and Id holders")

# -------- 2SLS. dependent variable: non-employment --------

# MALES
iv1_males <- ivreg(nonemployed ~ dis_id + as.factor(age) + education + marital_status + n_child_cat | 
                     as.factor(age) + education + marital_status + n_child_cat + dis_manuela, data = disability_ec %>% filter(sex == "male"), weights = weight)

iv2_males <- ivreg(nonemployed ~ dis_id + as.factor(age) + education + marital_status + n_child_cat + dis_degree | 
                     as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_manuela, data = disability_ec %>% filter(sex == "male"), weights = weight)

iv3_males <- ivreg(nonemployed ~ dis_id + as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_type | 
                     as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_type + dis_manuela, data = disability_ec %>% filter(sex == "male"), weights = weight)

# FEMALES
iv1_females <- ivreg(nonemployed ~ dis_id + as.factor(age) + education + marital_status + n_child_cat | 
                       as.factor(age) + education + marital_status + n_child_cat + dis_manuela, data = disability_ec %>% filter(sex == "female"), weights = weight)

iv2_females <- ivreg(nonemployed ~ dis_id + as.factor(age) + education + marital_status + n_child_cat + dis_degree | 
                       as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_manuela, data = disability_ec %>% filter(sex == "female"), weights = weight)

iv3_females <- ivreg(nonemployed ~ dis_id + as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_type | 
                       as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_type + dis_manuela, data = disability_ec %>% filter(sex == "female"), weights = weight)

stargazer(iv1_males, iv2_males, iv3_males, iv1_females, iv2_females, iv3_females, type = "text", align = TRUE, 
          keep = c("dis_id", "dis_degree", "dis_type"), covariate.labels = var_names, omit.stat = c("LL","ser","f", "rsq"),
          dep.var.caption = "", dep.var.labels.include = FALSE, column.labels = c("Men", "Women"), column.separate = c(3,3),
          title = "IV model: effects of disability benefits on non-employment")

stargazer(fs1_males, fs2_males, fs3_males, fs1_females, fs2_females, fs3_females, type = "text", align = TRUE, 
          keep = c("dis_manuela", "dis_degree", "dis_type"), covariate.labels = c("Mision Manuela Espejo", var_names[2:length(var_names)]), 
          omit.stat = c("LL","ser","f", "rsq"), dep.var.caption = "", dep.var.labels.include = FALSE, 
          column.labels = c("Men", "Women"), column.separate = c(3,3), title = "")

stargazer(ols1_males, ols2_males, ols3_males, ols1_females, ols2_females, ols3_females, type = "text", align = TRUE, 
          keep = c("dis_id"), covariate.labels = c("Id holder"), omit.stat = c("LL","ser","f", "rsq"),
          dep.var.caption = "", dep.var.labels.include = FALSE, column.labels = c("Men", "Women"), column.separate = c(3,3))

# -------- IV regressions on sick --------
# -------- OLS. independent variable: sick --------

# MALES
# ols 1 -> males without controls
ols1_males <- lm(sick ~ dis_id + as.factor(age) + education + marital_status + n_child_cat, 
                 data = disability_ec %>% filter(sex == "male"), weights = weight)

# ols 2 -> males + self assessment of the impairment
ols2_males <- lm(sick ~ dis_id + as.factor(age) + education + marital_status + n_child_cat + dis_degree, 
                 data = disability_ec %>% filter(sex == "male"), weights = weight)

# ols 3 -> males + self assessment of the impairment + type of disability
ols3_males <- lm(sick ~ dis_id + as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_type, 
                 data = disability_ec %>% filter(sex == "male"), weights = weight)

# FEMALES
# ols 1 -> males without controls
ols1_females <- lm(sick ~ dis_id + as.factor(age) + education + marital_status + n_child_cat, 
                   data = disability_ec %>% filter(sex == "female"), weights = weight)

# ols 2 -> males + self assessment of the impairment
ols2_females <- lm(sick ~ dis_id + as.factor(age) + education + marital_status + n_child_cat + dis_degree, 
                   data = disability_ec %>% filter(sex == "female"), weights = weight)

# ols 3 -> males + self assessment of the impairment + type of disability
ols3_females <- lm(sick ~ dis_id + as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_type, 
                   data = disability_ec %>% filter(sex == "female"), weights = weight)

var_names <- c("Id holder", "Very difficult", "Extremely difficult", "Hearing", "Walking or walking up stars", 
               "Recalling or concentrating", "Bathing and dressing", "Communicating")

controls <- list(c("Age controls", "Yes","Yes", "Yes", "Yes","Yes", "Yes"), 
                 c("Education controls", "Yes","Yes", "Yes", "Yes","Yes", "Yes"),
                 c("Marital status controls", "Yes","Yes", "Yes", "Yes","Yes", "Yes"),
                 c("Num of children controls", "Yes","Yes", "Yes", "Yes","Yes", "Yes"))

stargazer(ols1_males, ols2_males, ols3_males, ols1_females, ols2_females, ols3_females, type = "text", align = TRUE, 
          keep = c("dis_id", "dis_degree", "dis_type"), covariate.labels = var_names, omit.stat = c("LL","ser","f"),
          dep.var.caption = "Dependent variable: sick in the last 30 days", dep.var.labels.include = FALSE,
          add.lines = controls, column.labels = c("Men", "Women"), column.separate = c(3,3),
          title = "OLS regressions: relationship between disability benefits and being sick in the last 30 days")

# -------- first stage. independent variable: id_holder --------

# MALES
# first stage 1 -> males without controls
fs1_males <- lm(dis_id ~ dis_manuela + as.factor(age) + education + marital_status + n_child_cat, 
                data = disability_ec %>% filter(sex == "male"), weights = weight)

# first stage 2 -> males + self assessment of the impairment
fs2_males <- lm(dis_id ~ dis_manuela + as.factor(age) + education + marital_status + n_child_cat + dis_degree, 
                data = disability_ec %>% filter(sex == "male"), weights = weight)

# first stage 3 -> males + self assessment of the impairment + type of disability
fs3_males <- lm(dis_id ~ dis_manuela + as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_type, 
                data = disability_ec %>% filter(sex == "male"), weights = weight)

# FEMALES
# first stage 1 -> males without controls
fs1_females <- lm(dis_id ~ dis_manuela + as.factor(age) + education + marital_status + n_child_cat, 
                  data = disability_ec %>% filter(sex == "female"), weights = weight)

# first stage 2 -> males + self assessment of the impairment
fs2_females <- lm(dis_id ~ dis_manuela + as.factor(age) + education + marital_status + n_child_cat + dis_degree, 
                  data = disability_ec %>% filter(sex == "female"), weights = weight)

# first stage 3 -> males + self assessment of the impairment + type of disability
fs3_females <- lm(dis_id ~ dis_manuela + as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_type, 
                  data = disability_ec %>% filter(sex == "female"), weights = weight)

stargazer(fs1_males, fs2_males, fs3_males, fs1_females, fs2_females, fs3_females, type = "text", align = TRUE, 
          keep = c("dis_manuela"), covariate.labels = c("Mision Manuela Espejo"), omit.stat = c("LL","ser","f"),
          dep.var.caption = "Dependent variable: Id holder", dep.var.labels.include = FALSE,
          add.lines = controls, column.labels = c("Men", "Women"), column.separate = c(3,3),
          title = "First stage: relationship between Mision Manuela Espejo and Id holders")

# -------- 2SLS. independent variable: sick --------

# MALES
iv1_males <- ivreg(sick ~ dis_id + as.factor(age) + education + marital_status + n_child_cat | 
                     as.factor(age) + education + marital_status + n_child_cat + dis_manuela, data = disability_ec %>% filter(sex == "male"), weights = weight)

iv2_males <- ivreg(sick ~ dis_id + as.factor(age) + education + marital_status + n_child_cat + dis_degree | 
                     as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_manuela, data = disability_ec %>% filter(sex == "male"), weights = weight)

iv3_males <- ivreg(sick ~ dis_id + as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_type | 
                     as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_type + dis_manuela, data = disability_ec %>% filter(sex == "male"), weights = weight)

# FEMALES
iv1_females <- ivreg(sick ~ dis_id + as.factor(age) + education + marital_status + n_child_cat | 
                       as.factor(age) + education + marital_status + n_child_cat + dis_manuela, data = disability_ec %>% filter(sex == "female"), weights = weight)

iv2_females <- ivreg(sick ~ dis_id + as.factor(age) + education + marital_status + n_child_cat + dis_degree | 
                       as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_manuela, data = disability_ec %>% filter(sex == "female"), weights = weight)

iv3_females <- ivreg(sick ~ dis_id + as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_type | 
                       as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_type + dis_manuela, data = disability_ec %>% filter(sex == "female"), weights = weight)

stargazer(iv1_males, iv2_males, iv3_males, iv1_females, iv2_females, iv3_females, type = "text", align = TRUE, 
          keep = c("dis_id", "dis_degree", "dis_type"), covariate.labels = var_names, omit.stat = c("LL","ser","f", "rsq"),
          dep.var.caption = "", dep.var.labels.include = FALSE, column.labels = c("Men", "Women"), column.separate = c(3,3),
          title = "IV model: effects of disability benefits on being sick in the last 30 days")

stargazer(fs1_males, fs2_males, fs3_males, fs1_females, fs2_females, fs3_females, type = "text", align = TRUE, 
          keep = c("dis_manuela", "dis_degree", "dis_type"), covariate.labels = c("Mision Manuela Espejo", var_names[2:length(var_names)]), 
          omit.stat = c("LL","ser","f", "rsq"), dep.var.caption = "", dep.var.labels.include = FALSE, 
          column.labels = c("Men", "Women"), column.separate = c(3,3), title = "")

stargazer(ols1_males, ols2_males, ols3_males, ols1_females, ols2_females, ols3_females, type = "text", align = TRUE, 
          keep = c("dis_id"), covariate.labels = c("Id holder"), omit.stat = c("LL","ser","f", "rsq"),
          dep.var.caption = "", dep.var.labels.include = FALSE, column.labels = c("Men", "Women"), column.separate = c(3,3))

# -------- IV regressions on prev_care --------
# -------- OLS. independent variable: prev_care --------

# MALES
# ols 1 -> males without controls
ols1_males <- lm(prev_care ~ dis_id + as.factor(age) + education + marital_status + n_child_cat, 
                 data = disability_ec %>% filter(sex == "male"), weights = weight)

# ols 2 -> males + self assessment of the impairment
ols2_males <- lm(prev_care ~ dis_id + as.factor(age) + education + marital_status + n_child_cat + dis_degree, 
                 data = disability_ec %>% filter(sex == "male"), weights = weight)

# ols 3 -> males + self assessment of the impairment + type of disability
ols3_males <- lm(prev_care ~ dis_id + as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_type, 
                 data = disability_ec %>% filter(sex == "male"), weights = weight)

# FEMALES
# ols 1 -> males without controls
ols1_females <- lm(prev_care ~ dis_id + as.factor(age) + education + marital_status + n_child_cat, 
                   data = disability_ec %>% filter(sex == "female"), weights = weight)

# ols 2 -> males + self assessment of the impairment
ols2_females <- lm(prev_care ~ dis_id + as.factor(age) + education + marital_status + n_child_cat + dis_degree, 
                   data = disability_ec %>% filter(sex == "female"), weights = weight)

# ols 3 -> males + self assessment of the impairment + type of disability
ols3_females <- lm(prev_care ~ dis_id + as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_type, 
                   data = disability_ec %>% filter(sex == "female"), weights = weight)

var_names <- c("Id holder", "Very difficult", "Extremely difficult", "Hearing", "Walking or walking up stars", 
               "Recalling or concentrating", "Bathing and dressing", "Communicating")

controls <- list(c("Age controls", "Yes","Yes", "Yes", "Yes","Yes", "Yes"), 
                 c("Education controls", "Yes","Yes", "Yes", "Yes","Yes", "Yes"),
                 c("Marital status controls", "Yes","Yes", "Yes", "Yes","Yes", "Yes"),
                 c("Num of children controls", "Yes","Yes", "Yes", "Yes","Yes", "Yes"))

stargazer(ols1_males, ols2_males, ols3_males, ols1_females, ols2_females, ols3_females, type = "text", align = TRUE, 
          keep = c("dis_id", "dis_degree", "dis_type"), covariate.labels = var_names, omit.stat = c("LL","ser","f"),
          dep.var.caption = "Dependent variable: preventive care in the last 30 days", dep.var.labels.include = FALSE,
          add.lines = controls, column.labels = c("Men", "Women"), column.separate = c(3,3),
          title = "OLS regressions: relationship between disability benefits and being prev_care in the last 30 days")

# -------- first stage. independent variable: id_holder --------

# MALES
# first stage 1 -> males without controls
fs1_males <- lm(dis_id ~ dis_manuela + as.factor(age) + education + marital_status + n_child_cat, 
                data = disability_ec %>% filter(sex == "male"), weights = weight)

# first stage 2 -> males + self assessment of the impairment
fs2_males <- lm(dis_id ~ dis_manuela + as.factor(age) + education + marital_status + n_child_cat + dis_degree, 
                data = disability_ec %>% filter(sex == "male"), weights = weight)

# first stage 3 -> males + self assessment of the impairment + type of disability
fs3_males <- lm(dis_id ~ dis_manuela + as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_type, 
                data = disability_ec %>% filter(sex == "male"), weights = weight)

# FEMALES
# first stage 1 -> males without controls
fs1_females <- lm(dis_id ~ dis_manuela + as.factor(age) + education + marital_status + n_child_cat, 
                  data = disability_ec %>% filter(sex == "female"), weights = weight)

# first stage 2 -> males + self assessment of the impairment
fs2_females <- lm(dis_id ~ dis_manuela + as.factor(age) + education + marital_status + n_child_cat + dis_degree, 
                  data = disability_ec %>% filter(sex == "female"), weights = weight)

# first stage 3 -> males + self assessment of the impairment + type of disability
fs3_females <- lm(dis_id ~ dis_manuela + as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_type, 
                  data = disability_ec %>% filter(sex == "female"), weights = weight)

stargazer(fs1_males, fs2_males, fs3_males, fs1_females, fs2_females, fs3_females, type = "text", align = TRUE, 
          keep = c("dis_manuela"), covariate.labels = c("Mision Manuela Espejo"), omit.stat = c("LL","ser","f"),
          dep.var.caption = "Dependent variable: Id holder", dep.var.labels.include = FALSE,
          add.lines = controls, column.labels = c("Men", "Women"), column.separate = c(3,3),
          title = "First stage: relationship between Mision Manuela Espejo and Id holders")

# -------- 2SLS. independent variable: prev_care --------

# MALES
iv1_males <- ivreg(prev_care ~ dis_id + as.factor(age) + education + marital_status + n_child_cat | 
                     as.factor(age) + education + marital_status + n_child_cat + dis_manuela, data = disability_ec %>% filter(sex == "male"), weights = weight)

iv2_males <- ivreg(prev_care ~ dis_id + as.factor(age) + education + marital_status + n_child_cat + dis_degree | 
                     as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_manuela, data = disability_ec %>% filter(sex == "male"), weights = weight)

iv3_males <- ivreg(prev_care ~ dis_id + as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_type | 
                     as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_type + dis_manuela, data = disability_ec %>% filter(sex == "male"), weights = weight)

# FEMALES
iv1_females <- ivreg(prev_care ~ dis_id + as.factor(age) + education + marital_status + n_child_cat | 
                       as.factor(age) + education + marital_status + n_child_cat + dis_manuela, data = disability_ec %>% filter(sex == "female"), weights = weight)

iv2_females <- ivreg(prev_care ~ dis_id + as.factor(age) + education + marital_status + n_child_cat + dis_degree | 
                       as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_manuela, data = disability_ec %>% filter(sex == "female"), weights = weight)

iv3_females <- ivreg(prev_care ~ dis_id + as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_type | 
                       as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_type + dis_manuela, data = disability_ec %>% filter(sex == "female"), weights = weight)

stargazer(iv1_males, iv2_males, iv3_males, iv1_females, iv2_females, iv3_females, type = "text", align = TRUE, 
          keep = c("dis_id", "dis_degree", "dis_type"), covariate.labels = var_names, omit.stat = c("LL","ser","f", "rsq"),
          dep.var.caption = "", dep.var.labels.include = FALSE, column.labels = c("Men", "Women"), column.separate = c(3,3),
          title = "IV model: effects of disability benefits on preventive care in the last 30 days")

stargazer(fs1_males, fs2_males, fs3_males, fs1_females, fs2_females, fs3_females, type = "text", align = TRUE, 
          keep = c("dis_manuela", "dis_degree", "dis_type"), covariate.labels = c("Mision Manuela Espejo", var_names[2:length(var_names)]), 
          omit.stat = c("LL","ser","f", "rsq"), dep.var.caption = "", dep.var.labels.include = FALSE, 
          column.labels = c("Men", "Women"), column.separate = c(3,3), title = "")

stargazer(ols1_males, ols2_males, ols3_males, ols1_females, ols2_females, ols3_females, type = "text", align = TRUE, 
          keep = c("dis_id"), covariate.labels = c("Id holder"), omit.stat = c("LL","ser","f", "rsq"),
          dep.var.caption = "", dep.var.labels.include = FALSE, column.labels = c("Men", "Women"), column.separate = c(3,3))


# -------- IV regressions on good_health --------
# -------- OLS. independent variable: good_health --------

# ols 1 -> males without controls
ols1_males <- lm(good_health ~ dis_id + as.factor(age) + education + marital_status + n_child_cat, 
                 data = disability_ec %>% filter(sex == "male"), weights = weight)

# ols 2 -> males + self assessment of the impairment
ols2_males <- lm(good_health ~ dis_id + as.factor(age) + education + marital_status + n_child_cat + dis_degree, 
                 data = disability_ec %>% filter(sex == "male"), weights = weight)

# ols 3 -> males + self assessment of the impairment + type of disability
ols3_males <- lm(good_health ~ dis_id + as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_type, 
                 data = disability_ec %>% filter(sex == "male"), weights = weight)

# FEMALES
# ols 1 -> males without controls
ols1_females <- lm(good_health ~ dis_id + as.factor(age) + education + marital_status + n_child_cat, 
                   data = disability_ec %>% filter(sex == "female"), weights = weight)

# ols 2 -> males + self assessment of the impairment
ols2_females <- lm(good_health ~ dis_id + as.factor(age) + education + marital_status + n_child_cat + dis_degree, 
                   data = disability_ec %>% filter(sex == "female"), weights = weight)

# ols 3 -> males + self assessment of the impairment + type of disability
ols3_females <- lm(good_health ~ dis_id + as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_type, 
                   data = disability_ec %>% filter(sex == "female"), weights = weight)

var_names <- c("Id holder", "Very difficult", "Extremely difficult", "Hearing", "Walking or walking up stars", 
               "Recalling or concentrating", "Bathing and dressing", "Communicating")

controls <- list(c("Age controls", "Yes","Yes", "Yes", "Yes","Yes", "Yes"), 
                 c("Education controls", "Yes","Yes", "Yes", "Yes","Yes", "Yes"),
                 c("Marital status controls", "Yes","Yes", "Yes", "Yes","Yes", "Yes"),
                 c("Num of children controls", "Yes","Yes", "Yes", "Yes","Yes", "Yes"))

stargazer(ols1_males, ols2_males, ols3_males, ols1_females, ols2_females, ols3_females, type = "text", align = TRUE, 
          keep = c("dis_id", "dis_degree", "dis_type"), covariate.labels = var_names, omit.stat = c("LL","ser","f"),
          dep.var.caption = "Dependent variable: considers herself in good health", dep.var.labels.include = FALSE,
          add.lines = controls, column.labels = c("Men", "Women"), column.separate = c(3,3))

# -------- first stage. independent variable: id_holder --------

# MALES
# first stage 1 -> males without controls
fs1_males <- lm(dis_id ~ dis_manuela + as.factor(age) + education + marital_status + n_child_cat, 
                data = disability_ec %>% filter(sex == "male"), weights = weight)

# first stage 2 -> males + self assessment of the impairment
fs2_males <- lm(dis_id ~ dis_manuela + as.factor(age) + education + marital_status + n_child_cat + dis_degree, 
                data = disability_ec %>% filter(sex == "male"), weights = weight)

# first stage 3 -> males + self assessment of the impairment + type of disability
fs3_males <- lm(dis_id ~ dis_manuela + as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_type, 
                data = disability_ec %>% filter(sex == "male"), weights = weight)

# FEMALES
# first stage 1 -> males without controls
fs1_females <- lm(dis_id ~ dis_manuela + as.factor(age) + education + marital_status + n_child_cat, 
                  data = disability_ec %>% filter(sex == "female"), weights = weight)

# first stage 2 -> males + self assessment of the impairment
fs2_females <- lm(dis_id ~ dis_manuela + as.factor(age) + education + marital_status + n_child_cat + dis_degree, 
                  data = disability_ec %>% filter(sex == "female"), weights = weight)

# first stage 3 -> males + self assessment of the impairment + type of disability
fs3_females <- lm(dis_id ~ dis_manuela + as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_type, 
                  data = disability_ec %>% filter(sex == "female"), weights = weight)

stargazer(fs1_males, fs2_males, fs3_males, fs1_females, fs2_females, fs3_females, type = "text", align = TRUE, 
          keep = c("dis_manuela"), covariate.labels = c("Mision Manuela Espejo"), omit.stat = c("LL","ser","f"),
          dep.var.caption = "Dependent variable: Id holder", dep.var.labels.include = FALSE,
          add.lines = controls, column.labels = c("Men", "Women"), column.separate = c(3,3),
          title = "First stage: relationship between Mision Manuela Espejo and Id holders")

# -------- 2SLS. independent variable: good_health --------

# MALES
iv1_males <- ivreg(good_health ~ dis_id + as.factor(age) + education + marital_status + n_child_cat | 
                     as.factor(age) + education + marital_status + n_child_cat + dis_manuela, data = disability_ec %>% filter(sex == "male"), weights = weight)

iv2_males <- ivreg(good_health ~ dis_id + as.factor(age) + education + marital_status + n_child_cat + dis_degree | 
                     as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_manuela, data = disability_ec %>% filter(sex == "male"), weights = weight)

iv3_males <- ivreg(good_health ~ dis_id + as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_type | 
                     as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_type + dis_manuela, data = disability_ec %>% filter(sex == "male"), weights = weight)

# FEMALES
iv1_females <- ivreg(good_health ~ dis_id + as.factor(age) + education + marital_status + n_child_cat | 
                       as.factor(age) + education + marital_status + n_child_cat + dis_manuela, data = disability_ec %>% filter(sex == "female"), weights = weight)

iv2_females <- ivreg(good_health ~ dis_id + as.factor(age) + education + marital_status + n_child_cat + dis_degree | 
                       as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_manuela, data = disability_ec %>% filter(sex == "female"), weights = weight)

iv3_females <- ivreg(good_health ~ dis_id + as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_type | 
                       as.factor(age) + education + marital_status + n_child_cat + dis_degree + dis_type + dis_manuela, data = disability_ec %>% filter(sex == "female"), weights = weight)

stargazer(iv1_males, iv2_males, iv3_males, iv1_females, iv2_females, iv3_females, type = "text", align = TRUE, 
          keep = c("dis_id", "dis_degree", "dis_type"), covariate.labels = var_names, omit.stat = c("LL","ser","f", "rsq"),
          dep.var.caption = "", dep.var.labels.include = FALSE, column.labels = c("Men", "Women"), column.separate = c(3,3),
          title = "IV model: effects of disability benefits on health perception")

stargazer(fs1_males, fs2_males, fs3_males, fs1_females, fs2_females, fs3_females, type = "text", align = TRUE, 
          keep = c("dis_manuela", "dis_degree", "dis_type"), covariate.labels = c("Mision Manuela Espejo", var_names[2:length(var_names)]), 
          omit.stat = c("LL","ser","f", "rsq"), dep.var.caption = "", dep.var.labels.include = FALSE, 
          column.labels = c("Men", "Women"), column.separate = c(3,3), title = "")

stargazer(ols1_males, ols2_males, ols3_males, ols1_females, ols2_females, ols3_females, type = "text", align = TRUE, 
          keep = c("dis_id"), covariate.labels = c("Id holder"), omit.stat = c("LL","ser","f", "rsq"),
          dep.var.caption = "", dep.var.labels.include = FALSE, column.labels = c("Men", "Women"), column.separate = c(3,3))
