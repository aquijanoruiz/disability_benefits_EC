if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

# -------------------- disability variables --------------------

# Q: Do you have difficulty seeing (even with glasses)? hearing (even with a hearing aid)? 
# walking or climbing stairs? remembering or concentrating? bathing or dressing? 
# communicating, understanding or being understood? 1 (not difficult at all) 4 (extremely difficult)

people <- mutate(people, vision = f1_s2_10_1, hearing = f1_s2_10_2, walking_stairs = f1_s2_10_3, 
         cognitive = f1_s2_10_4, bathing_dressing = f1_s2_10_5, communication = f1_s2_10_6)
index <- c("vision", "hearing", "walking_stairs", "cognitive", "bathing_dressing", "communication")
people[,index] <- sapply(people[,index], FUN = as.integer)

# disabled people -> true if the person answered 2 or above in the disability question
people$disabled <- !apply(people[,index], 1, function(x) max(x)) == 1

# disability degree -> the maximum value among all kinds of disability
people$dis_degree <- apply(people[,index], 1, function(x) max(x))
people$dis_degree <- factor(people$dis_degree, levels = c("1", "2", "3", "4"))

attach(people)

# disability type -> the name of the disability with the maximum value among all kinds of disability
set.seed(2525) # if there are two or more types of disability with the same value, one is chosen at random
people$dis_type <- ifelse(!disabled, NA, colnames(people[,index])[max.col(people[,index], ties.method="random")])
people$dis_type <- factor(people$dis_type, levels = index)

# disability id -> true if the person with disability has a disability id issued by the government
people$dis_id <- case_when(is.na(disabled) | !disabled ~ NA, f1_s2_11 == "si" ~ TRUE, TRUE ~ FALSE)

# disability percentage -> the disability percentage shown on the disability id
people$dis_id_percent <- f1_s2_12

# Mision Manuela Espejo -> true if the person with disability was visited by Mision Manuela Espejo
people$dis_manuela <- case_when(is.na(disabled) | !disabled ~ NA, f1_s2_13 == "si" ~ TRUE, TRUE ~ FALSE)

# disability transfer -> true if anyone in the household receives disability transfers
# bdh transfer -> true if anyone in the household receives cash transfers (bono de desarrollo humano)
people <- left_join(people, people %>% group_by(id_hogar) %>% 
                      summarize(dis_transfer = any(f1_s3_29 == "si", na.rm = TRUE),
                                bdh_transfer = any(f1_s3_27 == "si", na.rm = TRUE)), by = "id_hogar")

# -------------------- sociodemographic variables --------------------

# sex, age, area, ethnic identity, narital status, education
people <- mutate(people, sex = sexo, age = edadanios, age_sqr = age^2, ethnicity = f1_s2_9, 
                 marital_status = f1_s2_16, education = f1_s2_19_1)

levels(people$sex) <- c("male", "female")
levels(people$area) <- c("urban", "rural")

levels(people$ethnicity) <-  c("indigenous", "black", "black", "mestizo", "mestizo", "mestizo", "white", "mestizo")
people$ethnicity <- factor(people$ethnicity, levels = c("mestizo", "white", "black", "indigenous"))

levels(people$marital_status) <- c("married", "cohabiting", "cohabiting", "non-partnered", 
                                   "non-partnered", "non-partnered", "non-partnered")
people$marital_status <- factor(people$marital_status, levels = c("non-partnered", "cohabiting", "married"))

levels(people$education) <- c("none", "none", "none", "primary", "primary", "secondary", "secondary", 
                              "tertiary", "tertiary", "tertiary")
people$education <- factor(people$education, levels = c("none", "primary", "secondary", "tertiary"))

# number of children -> number of children above 18 a person has
people <- people %>% mutate(person = as.integer(persona), 
                            mother = ifelse(age <= 18, f1_s2_15_1, NA), 
                            father = ifelse(age <= 18, f1_s2_14_1, NA)) %>% 
  group_by(id_hogar) %>% mutate(n_child_mom = sapply(person, function(x) sum(mother %in% x, na.rm = TRUE)),
                                n_child_dad = sapply(person, function(x) sum(father %in% x, na.rm = TRUE)),
                                n_child = n_child_mom + n_child_dad)

people$n_child_cat <- as.factor(people$n_child) # transforms numeric into factor
levels(people$n_child_cat) <- c("0", "1", "2", "3", "4", "5", rep("6ormore", 5))

# -------------------- income variables --------------------

# income is calculated as someones income as a business owner and employed worker in the previous month
index <- c("f1_s3_15", "f1_s3_16_2", "f1_s3_17", "f1_s3_18", "f1_s3_19", "f1_s3_20_2", "f1_s3_21", "f1_s3_22_2")
people[, index] <- sapply(people[, index], function(x){ 
  x = ifelse(x == 999999 | is.na(x), 0, x) # changes NAs and 999999s into zero
  return(x)
})
  
people <- mutate(people, inc_business_owner = f1_s3_15 + f1_s3_16_2 - f1_s3_17, # income minus expenses
                 inc_employed = f1_s3_18 + f1_s3_19 + f1_s3_20_2, # salary + benefits
                 inc_secondary = f1_s3_21 + f1_s3_22_2, # secondary income
                 inc_total = inc_business_owner + inc_employed + inc_secondary)

# -------------------- outcome variables --------------------

# unemployed -> true if the person did not do any job the previous week and does not have any job to return to
people$nonemployed <- case_when(is.na(f1_s3_1) ~ NA, as.integer(f1_s3_1) == 1 ~ FALSE, as.integer(f1_s3_2) != 12 ~ FALSE, 
                                as.integer(f1_s3_3) == 1 ~ FALSE, TRUE ~ TRUE)

# sick -> true if the person suffered from any sickness in the last 30 days
people$sick <- f1_s4_2 == "si"

# preventive medicine -> true if the person received any preventive care in the last 30 days
people$prev_care <- f1_s4_41 == "si"

# hospitalized -> true if the person was hospitalized in the last 30 days
people$hospital <- f1_s4_54 == "si"

# good health -> true if the person considers themselves in good, very good, or excellent health
people$good_health <- between(as.integer(f1_s4_58),1,3)

# better health -> true if the person considers themselves in better health than in the previous year
people$better_health <- as.integer(f1_s4_59) == 1
detach(people)

# -------------------- exporting the data --------------------
disability_ec <- select(people, id_hogar, id_per, fexp, sex, age, age_sqr, prov, area, ethnicity, education, marital_status, 
                        n_child, n_child_cat, inc_business_owner, inc_employed, inc_secondary, inc_total, bdh_transfer, nonemployed,
                        vision, hearing, walking_stairs, cognitive, bathing_dressing, communication, disabled, dis_degree, 
                        dis_type, dis_id, dis_id_percent, dis_manuela, dis_transfer, sick, prev_care, hospital, good_health, 
                        better_health) %>% rename(weight = fexp) %>% filter(disabled == TRUE)

saveRDS(disability_ec, file = "rds_files/disability_ec.rds")
