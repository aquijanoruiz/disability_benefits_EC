if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

people <- mutate(people, vision = f1_s2_10_1, hearing = f1_s2_10_2, walking_stairs = f1_s2_10_3, 
         cognitive = f1_s2_10_4, bathing_dressing = f1_s2_10_5, communication = f1_s2_10_6)

index <- c("vision", "hearing", "walking_stairs", "cognitive", "bathing_dressing", "communication")
people[,index] <- sapply(people[,index], FUN = as.integer)
people$disabled <- !apply(people[,index], 1, function(x) max(x)) == 1

people$dis_degree <- apply(people[,index], 1, function(x) max(x))
people$dis_degree <- factor(people$dis_degree, levels = c("1", "2", "3", "4"))

set.seed(2525)
people$dis_type <- ifelse(!disabled, NA, colnames(people[,index])[max.col(people[,index], ties.method="random")])
people$dis_type <- factor(people$dis_type, levels = index)

attach(people)
people$dis_id <- case_when(is.na(disabled) | !disabled ~ NA, f1_s2_11 == "si" ~ TRUE, TRUE ~ FALSE)
people$dis_id_percent <- f1_s2_12

people$dis_manuela <- case_when(is.na(disabled) | !disabled ~ NA, f1_s2_13 == "si" ~ TRUE, TRUE ~ FALSE)

people <- left_join(people, people %>% group_by(id_hogar) %>% 
                      summarize(dis_transfer = any(f1_s3_29 == "si", na.rm = TRUE),
                                bdh_transfer = any(f1_s3_27 == "si", na.rm = TRUE)), by = "id_hogar")

people <- mutate(people, age = edadanios, ethnicity = f1_s2_9, marital_status = f1_s2_16, education = f1_s2_19_1)

levels(people$ethnicity) <-  c("indigenous", "black", "black", "mestizo", "mestizo", "mestizo", "white", "mestizo")
people$ethnicity <- factor(people$ethnicity, levels = c("mestizo", "white", "black", "indigenous"))

levels(people$marital_status) <- c("married", "cohabiting", "cohabiting", "non-partnered", 
                                   "non-partnered", "non-partnered", "non-partnered")
people$marital_status <- factor(people$marital_status, levels = c("non-partnered", "cohabiting", "married"))

levels(people$education) <- c("none", "none", "none", "primary", "primary", "secondary", "secondary", 
                              "tertiary", "tertiary", "tertiary")
people$education <- factor(people$education, levels = c("none", "primary", "secondary", "tertiary"))

levels(people$area) <- c("urban", "rural")

index <- c("f1_s3_15", "f1_s3_16_2", "f1_s3_17", "f1_s3_18", "f1_s3_19", "f1_s3_20_2", "f1_s3_21", "f1_s3_22_2")
people[, index] <- sapply(people[, index], function(x){ 
  x = ifelse(x == 999999 | is.na(x), 0, x)
  return(x)
})
  
people <- mutate(people, inc_business_owner = f1_s3_15 + f1_s3_16_2 - f1_s3_17,
                 inc_employed = f1_s3_18 + f1_s3_19 + f1_s3_20_2,
                 inc_secondary = f1_s3_21 + f1_s3_22_2,
                 inc_total = inc_business_owner + inc_employed + inc_secondary)

people$nonemployed <- case_when(is.na(f1_s3_1) ~ NA, as.integer(f1_s3_1) == 1 ~ FALSE, as.integer(f1_s3_2) != 12 ~ FALSE, 
                                as.integer(f1_s3_3) == 1 ~ FALSE, TRUE ~ TRUE)

personas$trabaja <- factor(personas$trabaja, levels = c("no", "si"))

people$sick <- f1_s4_2 == "si"
people$prev_med <- f1_s4_41 == "si"
people$hospital <- f1_s4_54 == "si"

people$good_health <- between(as.integer(f1_s4_58),1,3) 
people$better_health <- as.integer(f1_s4_59) == 1

people <- people %>% mutate(person = as.integer(persona), mother = f1_s2_15_1, father = f1_s2_14_1) %>% 
  group_by(id_hogar) %>% mutate(n_child_mom = sapply(person, function(x) sum(mother %in% x, na.rm = TRUE)),
                                n_child_dad = sapply(person, function(x) sum(father %in% x, na.rm = TRUE)),
                                n_child = n_child_mom + n_child_dad)

saveRDS(people, file = "rds_files/disability_ec.rds")

