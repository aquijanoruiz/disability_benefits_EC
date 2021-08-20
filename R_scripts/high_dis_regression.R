disability_ec <- readRDS("rds_files/disability_ec.rds")
disability_ec <- filter(disability_ec, between(age, 18, 64)) # filters people between 18 and 64

# -------------------- identification strategy --------------------

# control group -> people with disability percentage between 45-49, 70-74, and 80-85
# treatment group -> people with disability percentage equal to 50, 75, 85

disability_ec <- filter(disability_ec, between(dis_id_percent, 45, 50) | 
                          between(dis_id_percent, 70, 75) | between(dis_id_percent, 80, 85))

disability_ec$treatment <- with(disability_ec, case_when(between(dis_id_percent, 45, 49) ~ TRUE,
                                                         between(dis_id_percent, 70, 74) ~ TRUE,
                                                         between(dis_id_percent, 80, 84) ~ TRUE,
                                                         TRUE ~ FALSE))

disability_ec$fixed_effect <- 
  with(disability_ec, case_when(between(dis_id_percent, 45, 50) ~ "group_2",
                                between(dis_id_percent, 70, 75) ~ "group_3",
                                between(dis_id_percent, 80, 85) ~ "group_4"))

disability_ec <- disability_ec %>% mutate(value = TRUE) %>% 
  spread(key = sex, value = value, fill = FALSE, sep = "_" ) %>%
  mutate(female_treat = as.logical(treatment * sex_female))

# -------------------- regression --------------------

# nonemployed
reg1 <- lm(nonemployed ~ treatment + sex_female + female_treat + fixed_effect + as.factor(age) + education + marital_status + n_child_cat + dis_perception + dis_type, 
           data = disability_ec, weights = weight)

# sick
reg2 <- lm(sick ~ treatment + sex_female + female_treat + fixed_effect + as.factor(age) + education + marital_status + n_child_cat + dis_perception + dis_type, 
           data = disability_ec, weights = weight)

# prev_care
reg3 <- lm(prev_care ~ treatment + sex_female + female_treat + fixed_effect + as.factor(age) + education + marital_status + n_child_cat + dis_perception + dis_type, 
           data = disability_ec, weights = weight)

# good_health
reg4 <- lm(good_health ~ treatment + sex_female + female_treat + fixed_effect + as.factor(age) + education + marital_status + n_child_cat + dis_perception + dis_type, 
           data = disability_ec, weights = weight)

# better_health
reg5 <- lm(better_health ~ treatment + sex_female + female_treat + fixed_effect + as.factor(age) + education + marital_status + n_child_cat + dis_perception + dis_type, 
           data = disability_ec, weights = weight)

var_names <- c("Treatment: above cutoff", "Woman", "Treatment X woman", "Group 3", "Group 4", "Very difficult", "Extremely difficult", "Hearing", "Walking or walking up stars", 
               "Recalling or concentrating", "Bathing and dressing", "Communication")

controls <- list(c("Group fixed effects", "Yes","Yes", "Yes", "Yes","Yes", "Yes"),
                 c("Age controls", "Yes","Yes", "Yes", "Yes","Yes", "Yes"), 
                 c("Education controls", "Yes","Yes", "Yes", "Yes","Yes", "Yes"),
                 c("Marital status controls", "Yes","Yes", "Yes", "Yes","Yes", "Yes"),
                 c("Num of children controls", "Yes","Yes", "Yes", "Yes","Yes", "Yes"))

stargazer(reg1, reg2, reg3, reg4, reg5, type = "text", align = TRUE, keep = c("treatment", "sex_female", "female_treat", "fixed_effect", "dis_perception", "dis_type"), 
          dep.var.caption = "", dep.var.labels.include = FALSE, add.lines = controls, covariate.labels = var_names, omit.stat = c("LL","ser","f", "rsq"),
          column.labels = linebreak(c("Non-employment", "Sick in the \nlast 30 days", "Received preventive \ncare in the \nlast 30 days", 
                                      "Considers herself \nin good \nhealth", "Considers herself \nin better \nhealth")), 
          title = "Effects of disability benefits on people with the highest percentages of disability")

       