if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lmtest)) install.packages("lmtest", repos = "http://cran.us.r-project.org")
if(!require(sandwich)) install.packages("sandwich", repos = "http://cran.us.r-project.org")

disability_ec <- readRDS("rds_files/disability_ec.rds")

# -------------------- identification strategy --------------------

# control group -> people with disability percentage <40
# treatment group -> people with disability percentage between 40 and 49
disability_ec_3040 <- disability_ec %>% filter( between(age, 18,64) & between(dis_id_percent, 30, 49)) %>% 
  mutate(percent_40 = ifelse(dis_id_percent >= 40, TRUE, FALSE))

# -------------------- exogeneity checks --------------------

# dis_34 -> true if the person answered 3 or 4 in the disability question
disability_ec_3040 <- mutate(disability_ec_3040, dis_34 = ifelse(dis_degree != 2, TRUE, FALSE))

reg_dis_34 <- lm(dis_34 ~ percent_40 + sex + age + age_sqr, data = disability_ec_3040, weights = weight)
summary(reg_dis_34) # does not pass exogeneity test, very high significance

# dis_type -> type of disability (vision, hearing, walking_stairs, cognitive, bathing_dressing)
disability_ec_3040$value <- TRUE
disability_ec_3040 <- spread(disability_ec_3040, key = dis_type, value = value, fill = FALSE, sep = "_" )

reg_vision <- lm(dis_type_vision ~ percent_40 + sex + age + age_sqr, data = disability_ec_3040, weights = weight)
summary(reg_vision) # significant at 0.1 level

reg_hearing <- lm(dis_type_hearing ~ percent_40 + sex + age + age_sqr, data = disability_ec_3040, weights = weight)
summary(reg_hearing) # not sifnificant

reg_walking_stairs <- lm(dis_type_walking_stairs ~ percent_40 + sex + age + age_sqr, data = disability_ec_3040, weights = weight)
summary(reg_walking_stairs) # not sifnificant

reg_cognitive <- lm(dis_type_cognitive ~ percent_40 + sex + age + age_sqr, data = disability_ec_3040, weights = weight)
summary(reg_cognitive) # significant at 0.1 level

reg_bathing_dressing <- lm(dis_type_bathing_dressing ~ percent_40 + sex + age + age_sqr, data = disability_ec_3040, weights = weight)
summary(reg_bathing_dressing) # not sifnificant

reg_communication <- lm(dis_type_communication ~ percent_40 + sex + age + age_sqr, data = disability_ec_3040, weights = weight)
summary(reg_communication) # does not pass exogeneity test, very high significance


