if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

# -------------------- disability variables --------------------

# one's own perception of the severity of their impairment -> 1 (not difficult at all) 4 (extremely difficult)
# Do you have difficulty seeing (even with glasses)? hearing (even with a hearing aid)? walking or climbing stairs? 
# recalling or concentrating? bathing or dressing? communicating, understanding or being understood? 
people <- mutate(people, vision = f1_s2_10_1, hearing = f1_s2_10_2, walking_stairs = f1_s2_10_3, 
                 cognitive = f1_s2_10_4, bathing_dressing = f1_s2_10_5, communication = f1_s2_10_6)
index <- c("vision", "hearing", "walking_stairs", "cognitive", "bathing_dressing", "communication")
people[,index] <- sapply(people[,index], FUN = as.integer)

# disabled people -> true if the person answered 2 or above to the previous question about disability
people$disabled <- !apply(people[,index], 1, function(x) max(x)) == 1

# disability type -> the name of the disability with the highest value among all types of disability
set.seed(2525) # if there are two or more types of disability with the same value, one is chosen at random
people$dis_type <- ifelse(!people$disabled, NA, colnames(people[,index])[max.col(people[,index], ties.method="random")])
people$dis_type <- factor(people$dis_type, levels = index)

# disability degree -> the highest value (1 to 4) among all types of disability
people$dis_perception <- apply(people[,index], 1, function(x) max(x))
people$dis_perception <- factor(people$dis_perception, levels = c("1", "2", "3", "4"))

# disability id -> true if the person with disability has a disability id
people$dis_id <- case_when(is.na(people$disabled) | !people$disabled ~ NA, people$f1_s2_11 == "si" ~ TRUE, TRUE ~ FALSE)

# disability percentage -> the disability percentage written on the disability id
people$dis_id_percent <- people$f1_s2_12

# disability percentage category -> categorizes disability percentage into percentage groups
people$dis_id_percent_cat <- with(people, case_when(is.na(dis_id_percent) ~ NA_character_, 
  dis_id_percent < 40 ~ "30-39", between(dis_id_percent, 40, 49) ~ "40-49", between(dis_id_percent, 50, 74) ~ "50-74", 
  between(dis_id_percent, 75, 84) ~ "75-84", dis_id_percent > 84 ~ "85-100"))
people$dis_id_percent_cat <- factor(people$dis_id_percent_cat, levels = c("30-39", "40-49", "50-74", "75-84", "85-100"))

# Mision Manuela Espejo -> true if the person with disability was visited by Mision Manuela Espejo
people$dis_manuela <- case_when(is.na(people$disabled) | !people$disabled ~ NA, people$f1_s2_13 == "si" ~ TRUE, TRUE ~ FALSE)

# disability transfer -> true if anyone in the household receives disability transfers and the person has a disability id 
people <- left_join(people, people %>% group_by(id_hogar) %>% 
                      summarize(dis_transfer = any(f1_s3_29 == "si", na.rm = TRUE)), by = "id_hogar")
people$dis_transfer <- people$dis_id & people$dis_transfer

# bdh transfer -> true if anyone in the household receives cash transfers (bono de desarrollo humano)
people <- left_join(people, people %>% group_by(id_hogar) %>% 
                      summarize(bdh_transfer = any(f1_s3_27 == "si", na.rm = TRUE)), by = "id_hogar")

# -------------------- outcome variables --------------------

# nonemployed -> true if the person did not do any job the previous week and does not have any job to return to
people$nonemployed <- with(people, case_when(is.na(f1_s3_1) ~ NA, as.integer(f1_s3_1) == 1 ~ FALSE, 
  as.integer(f1_s3_2) != 12 ~ FALSE, as.integer(f1_s3_3) == 1 ~ FALSE, TRUE ~ TRUE))

# sick -> true if the person suffered from any sickness in the last 30 days
people$sick <- people$f1_s4_2 == "si"

# preventive medicine -> true if the person received any preventive care in the last 30 days
people$prev_care <- people$f1_s4_41 == "si"

# hospitalized -> true if the person was hospitalized in the last 30 days
people$hospital <- people$f1_s4_54 == "si"

# good health -> true if the person considers themselves in good, very good, or excellent health
people$good_health <- between(as.integer(people$f1_s4_58),1,3)

# better health -> true if the person considers themselves in better health than in the previous year
people$better_health <- as.integer(people$f1_s4_59) == 1

# -------------------- sociodemographic variables --------------------
# geographic conglomeration (province, canton, parish, and conglomerate)
people <- mutate(people, province = factor(prov), canton = factor(substr(upm, 1, 4)),
                 parish = factor(substr(upm, 1, 6)), conglomerate = factor(upm))

levels(people$province) <- 
  c("Azuay", "Bolivar", "Cañar", "Carchi", "Cotopaxi", "Chimborazo", "El Oro", "Esmeraldas", "Guayas", 
    "Imbabura", "Loja", "Los Rios", "Manabi", "Morona Santiago", "Napo", "Pastaza", "Pichincha", 
    "Tungurahua", "Zamora Chinchipe", "Galápagos", "Sucumbios", "Orellana", "Santo Domingo de los Tsáchilas", 
    "Santa Elena", "Zona no delimitada")

# sex, age, area, ethnic identity, marital status, education
people <- mutate(people, sex = sexo, age = edadanios, ethnicity = f1_s2_9, 
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

# number of children -> number of children under 18
people <- people %>% mutate(person = as.integer(persona), 
                            mother = ifelse(age <= 18, people$f1_s2_15_1, NA), 
                            father = ifelse(age <= 18, people$f1_s2_14_1, NA)) %>% 
  group_by(id_hogar) %>% mutate(n_child_mom = sapply(person, function(x) sum(mother %in% x, na.rm = TRUE)),
                                n_child_dad = sapply(person, function(x) sum(father %in% x, na.rm = TRUE)),
                                n_child = n_child_mom + n_child_dad) %>% ungroup()

people$n_child_cat <- as.factor(people$n_child) # transforms numeric into factor
levels(people$n_child_cat) <- c("0", "1", "2", "3", "4", "5", rep("6ormore", 5))

# home characteristics
home$floor <- factor(home$f1_s1_3)
levels(home$floor) <- c("Ceramic tile, stone, vinyl, marble, faux marble, treated planks, or concrete slab",
                        "Ceramic tile, stone, vinyl, marble, faux marble, treated planks, or concrete slab",
                        "Ceramic tile, stone, vinyl, marble, faux marble, treated planks, or concrete slab",
                        "cement, bricks", "untreated planks, reed, other", "untreated planks, reed, other", 
                        "dirt", "untreated planks, reed, other")

home$shower <- factor(home$f1_s1_12)
levels(home$shower) <- c("private","shared","does not own")

home$toilet <- factor(home$f1_s1_20)
levels(home$toilet) <- c("inside", "outside", "outside")

# -------------------- income  --------------------

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

# -------------------- selecting the data --------------------

disability_ec <- 
  select(people, province, canton, parish, conglomerate, id_hogar, id_per, fexp, sex, age, area, ethnicity, education, 
         marital_status, n_child, n_child_cat, inc_business_owner, inc_employed, inc_secondary, inc_total, bdh_transfer, 
         nonemployed, vision, hearing, walking_stairs, cognitive, bathing_dressing, communication, disabled, dis_perception, 
         dis_type, dis_id, dis_id_percent, dis_id_percent_cat, dis_manuela, dis_transfer, sick, prev_care, hospital, 
         good_health, better_health) %>% rename(weight = fexp)

disability_ec <- left_join(disability_ec, select(home, id_hogar, floor, shower, toilet), by = "id_hogar")
disability_ec <- filter(disability_ec, disabled == TRUE) # selects only people with disability
disability_ec <- disability_ec[disability_ec$age >= 5,] # removes people aged under 5

# -------------------- removing errors --------------------

# removes people with disability percentage below 30 or above 100
disability_ec <- disability_ec[is.na(disability_ec$dis_id_percent) | between(disability_ec$dis_id_percent,30,100),]


  
# -------------------- saving the data into rds and dta --------------------

saveRDS(disability_ec, file = "rds_files/disability_ec.rds")

if(!require(foreign)) install.packages("foreign", repos = "http://cran.us.r-project.org")
write.dta(disability_ec, file = "rds_files/disability_ec.dta")
