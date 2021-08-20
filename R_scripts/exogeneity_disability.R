if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")

# -------------------- loading the dataset --------------------

disability_ec <- readRDS("rds_files/disability_ec.rds")
disability_ec <- filter(disability_ec, between(age, 18, 64)) # filters people between 18 and 64

# -------------------- identification strategy --------------------

# control group -> people with disability percentage <40
# treatment group -> people with disability percentage equal to 40
disability_ec_3040 <- disability_ec %>% filter(between(dis_id_percent, 30, 40)) %>% 
  mutate(percent_40 = ifelse(dis_id_percent == 40, TRUE, FALSE))

# -------------------- exogeneity checks --------------------

# dis_perception34 -> true if the disability perception is 3 or 4 (very difficult or extremely difficult)
disability_ec_3040 <- mutate(disability_ec_3040, dis_perception34 = ifelse(dis_perception != 2, TRUE, FALSE))

reg_dis_perception34 <- lm(dis_perception34 ~ percent_40 + sex + as.factor(age), data = disability_ec_3040, weights = weight)

# dis_type -> type of disability (vision, hearing, walking_stairs, cognitive, bathing_dressing, communication)
disability_ec_3040$value <- TRUE
disability_ec_3040 <- spread(disability_ec_3040, key = dis_type, value = value, fill = FALSE, sep = "_" )

reg_vision <- lm(dis_type_vision ~ percent_40 + sex + as.factor(age), data = disability_ec_3040, weights = weight)
reg_hearing <- lm(dis_type_hearing ~ percent_40 + sex + as.factor(age), data = disability_ec_3040, weights = weight)
reg_walking_stairs <- lm(dis_type_walking_stairs ~ percent_40 + sex + as.factor(age), data = disability_ec_3040, weights = weight)
reg_cognitive <- lm(dis_type_cognitive ~ percent_40 + sex + as.factor(age), data = disability_ec_3040, weights = weight)
reg_bathing_dressing <- lm(dis_type_bathing_dressing ~ percent_40 + sex + as.factor(age), data = disability_ec_3040, weights = weight)
reg_communication <- lm(dis_type_communication ~ percent_40 + sex + as.factor(age), data = disability_ec_3040, weights = weight)

controls <- list(c("Sex controls", "Yes","Yes", "Yes", "Yes","Yes", "Yes", "Yes"), 
                 c("Age controls", "Yes","Yes", "Yes", "Yes","Yes", "Yes", "Yes"))

stargazer(reg_dis_perception34, reg_vision, reg_hearing, reg_walking_stairs, reg_cognitive, reg_bathing_dressing, reg_communication,
          type = "text", align = TRUE, keep = c("percent_40"), covariate.labels = c("Treatment: 40 disability"), 
          omit.stat = c("LL","ser","f", "rsq"), dep.var.caption = "", dep.var.labels.include = FALSE, add.lines = controls,
          column.labels = linebreak(c("Disability perception\nVery or extremely\ndifficult", "Vision \ndisability", "Hearing \ndisability", 
                                      "Walking and \nwalking up \nstairs", "Recalling and \nconcentrating", "Bathing and \ndressing", 
                                      "Communication \ndisability")), title = "Exogeneity checks")

# -------------------- regressions: treatment effect: percent_40 --------------------
disability_ec_3040 <- disability_ec %>% filter(between(dis_id_percent, 30, 40)) %>% 
  mutate(percent_40 = ifelse(dis_id_percent == 40, TRUE, FALSE), value = TRUE) %>%
  spread(key = sex, value = value, fill = FALSE, sep = "_" ) %>%
  mutate(female_40 = as.logical(percent_40 * sex_female))

# nonemployed
reg1 <- lm(nonemployed ~ percent_40 + sex_female + female_40 + as.factor(age) + education + marital_status + n_child_cat + dis_perception + dis_type, 
           data = disability_ec_3040, weights = weight)

# sick
reg2 <- lm(sick ~ percent_40 + sex_female + female_40 + as.factor(age) + education + marital_status + n_child_cat + dis_perception + dis_type, 
           data = disability_ec_3040, weights = weight)

# prev_care
reg3 <- lm(prev_care ~ percent_40 + sex_female + female_40 + as.factor(age) + education + marital_status + n_child_cat + dis_perception + dis_type, 
           data = disability_ec_3040, weights = weight)

# good_health
reg4 <- lm(good_health ~ percent_40 + sex_female + female_40 + as.factor(age) + education + marital_status + n_child_cat + dis_perception + dis_type, 
           data = disability_ec_3040, weights = weight)

# better_health
reg5 <- lm(better_health ~ percent_40 + sex_female + female_40 + as.factor(age) + education + marital_status + n_child_cat + dis_perception + dis_type, 
           data = disability_ec_3040, weights = weight)

var_names <- c("Treatment: 40 disability", "Woman", "Treatment X woman", "Very difficult", "Extremely difficult", "Hearing", "Walking or walking up stars", 
               "Recalling or concentrating", "Bathing and dressing", "Communication")

controls <- list(c("Age controls", "Yes","Yes", "Yes", "Yes","Yes", "Yes"), 
                 c("Education controls", "Yes","Yes", "Yes", "Yes","Yes", "Yes"),
                 c("Marital status controls", "Yes","Yes", "Yes", "Yes","Yes", "Yes"),
                 c("Num of children controls", "Yes","Yes", "Yes", "Yes","Yes", "Yes"))

stargazer(reg1, reg2, reg3, reg4, reg5, type = "text", align = TRUE, keep = c("percent_40", "sex_female", "female_40", "dis_perception", "dis_type"), 
          covariate.labels = var_names, omit.stat = c("LL","ser","f", "rsq"), dep.var.caption = "", dep.var.labels.include = FALSE, add.lines = controls,
          column.labels = linebreak(c("Non-employment", "Sick in the \nlast 30 days", "Received preventive \ncare in the \nlast 30 days", 
                                      "Considers herself \nin good \nhealth", "Considers herself \nin better \nhealth")), 
          title = "Effects of disability benefits on employment and health status using id holders with 40 disability as the treatment group")



