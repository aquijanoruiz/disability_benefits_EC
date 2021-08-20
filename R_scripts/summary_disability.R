if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

# -------------------- loading the dataset --------------------

disability_ec <- readRDS("rds_files/disability_ec.rds")
disability_ec <- filter(disability_ec, between(age, 18, 64)) # filters people between 18 and 64

# -------------------- summary statistics table id holders vs non id holders --------------------

# age category -> splits ages into age groups
disability_ec$age_cat <- with(disability_ec, case_when(between(age, 18, 44) ~ "18-44", between(age, 45, 54) ~ "45-54", between(age, 55, 64) ~ "55-64"))
disability_ec$age_cat <- factor(disability_ec$age_cat, levels = c("18-44", "45-54", "55-64"))

# long to wide
disability_ec <- disability_ec %>% mutate(value = TRUE) %>% spread(key = sex, value = value, fill = FALSE, sep = "_" ) %>%
  mutate(value = TRUE) %>% spread(key = dis_type, value = value, fill = FALSE, sep = "_" ) %>%
  mutate(value = TRUE) %>% spread(key = dis_perception, value = value, fill = FALSE, sep = "_" ) %>%
  mutate(value = TRUE) %>% spread(key = age_cat, value = value, fill = FALSE, sep = "_" ) %>% 
  mutate(value = TRUE) %>% spread(key = marital_status, value = value, fill = FALSE, sep = "_" ) %>% 
  mutate(value = TRUE) %>% spread(key = education, value = value, fill = FALSE, sep = "_" ) %>% 
  mutate(value = TRUE) %>% spread(key = n_child_cat, value = value, fill = FALSE, sep = "_" )

disability_ec <- as.data.frame(disability_ec) # transforms tibble into data frame

var <- c("nonemployed", "sick", "prev_care", "hospital", "good_health", "better_health", "dis_manuela", "dis_transfer", 
         "bdh_transfer", "dis_perception_2", "dis_perception_3", "dis_perception_4", "dis_type_vision","dis_type_hearing", 
         "dis_type_walking_stairs", "dis_type_cognitive", "dis_type_bathing_dressing", "dis_type_communication", 
         "age_cat_18-44", "age_cat_45-54", "age_cat_55-64",  "education_primary", "education_secondary", "education_tertiary", "n_child_cat_0", "n_child_cat_1", 
         "marital_status_non-partnered", "marital_status_cohabiting", "marital_status_married",
         "n_child_cat_2", "n_child_cat_3", "n_child_cat_4", "n_child_cat_5", "n_child_cat_6ormore")

dis_id_sumstat <- matrix(nrow = length(var), ncol = 6, dimnames = list(var))

for(i in var){ # loop that fills the matrix
  dis_id_sumstat[i,1] <- mean(disability_ec[!disability_ec[,"dis_id"], i]) # non id holders
  dis_id_sumstat[i,2] <- mean(disability_ec[disability_ec[,"dis_id"], i])  # id holders
  dis_id_sumstat[i,3] <- mean(disability_ec[disability_ec[,"sex_male"] & !disability_ec[,"dis_id"],i]) # male non id holders
  dis_id_sumstat[i,4] <- mean(disability_ec[disability_ec[,"sex_male"] & disability_ec[,"dis_id"],i]) # male id holders
  dis_id_sumstat[i,5] <- mean(disability_ec[disability_ec[,"sex_female"] & !disability_ec[,"dis_id"],i]) # female non id holders
  dis_id_sumstat[i,6] <- mean(disability_ec[disability_ec[,"sex_female"] & disability_ec[,"dis_id"],i]) # female id holders
}

# number of observations
dis_id_sumstat <- rbind(dis_id_sumstat, c(sum(!disability_ec$dis_id), 
                                          sum(disability_ec$dis_id), 
                                          sum(!disability_ec$dis_id & disability_ec$sex_male), 
                                          sum(disability_ec$dis_id & disability_ec$sex_male), 
                                          sum(!disability_ec$dis_id & disability_ec$sex_female),sum(disability_ec$dis_id & disability_ec$sex_female)))

rownames(dis_id_sumstat)[length(var) + 1] <- "observations"
colnames(dis_id_sumstat) <- c("all_id_holder", "all_non_id_holder", "male_id_holder", 
                              "male_non_id_holder", "female_id_holder", "female_non_id_holder")

saveRDS(dis_id_sumstat, file = "rds_files/table_dis_id_sumstat.rds")

# -------------------- id holders disability percentage summary statistics --------------------

sum(disability_ec$dis_id == TRUE) # number of people with disability id

filter(disability_ec, dis_id == TRUE) %>% ggplot(aes(x = dis_id_percent)) + theme_minimal() +
  geom_histogram(binwidth = 1, fill = "white", col = "black") +
  labs(y = "number of people with disability", x = "disability percentage written on the disability id", title = "")


# -------------------- 30% id holders vs 40% id holders --------------------

disability_ec <- readRDS("rds_files/disability_ec.rds")
disability_ec <- filter(disability_ec, between(age, 18, 64)) # filters people between 18 and 64
disability_ec_3040 <- disability_ec %>% filter(between(dis_id_percent, 30, 40)) %>% 
  mutate(percent_40 = ifelse(dis_id_percent == 40, TRUE, FALSE))

# long to wide
disability_ec_3040 <- disability_ec_3040 %>% mutate(value = TRUE) %>% spread(key = sex, value = value, fill = FALSE, sep = "_" ) %>%
  mutate(value = TRUE) %>% spread(key = dis_type, value = value, fill = FALSE, sep = "_" ) %>%
  mutate(value = TRUE) %>% spread(key = dis_perception, value = value, fill = FALSE, sep = "_" )

disability_ec_3040 <- as.data.frame(disability_ec_3040) # transforms tibble into data frame

var_3040 <- c("nonemployed", "sick", "prev_care", "hospital", "good_health", "better_health", "dis_manuela", "dis_transfer", 
         "bdh_transfer", "dis_perception_2", "dis_perception_3", "dis_perception_4", "dis_type_vision","dis_type_hearing", 
         "dis_type_walking_stairs", "dis_type_cognitive", "dis_type_bathing_dressing", "dis_type_communication")

dis_id_3040_sumstat <- matrix(nrow = length(var_3040), ncol = 6, dimnames = list(var_3040))

for(i in var_3040){ # loop that fills the matrix
  dis_id_3040_sumstat[i,1] <- mean(disability_ec_3040[!disability_ec_3040[,"percent_40"], i]) # 30% 
  dis_id_3040_sumstat[i,2] <- mean(disability_ec_3040[disability_ec_3040[,"percent_40"], i]) # 40%
  dis_id_3040_sumstat[i,3] <- mean(disability_ec_3040[disability_ec_3040[,"sex_male"] & !disability_ec_3040[,"percent_40"],i]) # 30% males
  dis_id_3040_sumstat[i,4] <- mean(disability_ec_3040[disability_ec_3040[,"sex_male"] & disability_ec_3040[,"percent_40"],i]) # 40% males
  dis_id_3040_sumstat[i,5] <- mean(disability_ec_3040[disability_ec_3040[,"sex_female"] & !disability_ec_3040[,"percent_40"],i]) # 30% females
  dis_id_3040_sumstat[i,6] <- mean(disability_ec_3040[disability_ec_3040[,"sex_female"] & disability_ec_3040[,"percent_40"],i]) # 40% females
}

# number of observations
dis_id_3040_sumstat <- rbind(dis_id_3040_sumstat, c(sum(!disability_ec_3040$percent_40), sum(disability_ec_3040$percent_40), 
                       sum(!disability_ec_3040$percent_40 & disability_ec_3040$sex_male), sum(disability_ec_3040$percent_40 & disability_ec_3040$sex_male), 
                       sum(!disability_ec_3040$percent_40 & disability_ec_3040$sex_female), sum(disability_ec_3040$percent_40 & disability_ec_3040$sex_female)))

rownames(dis_id_3040_sumstat)[length(var_3040) + 1] <- "observations"
colnames(dis_id_3040_sumstat) <- c("3039_id_holder", "40_id_holder", "3039_id_holder", "40_id_holder", "3039_id_holder", "40_id_holder")

saveRDS(dis_id_3040_sumstat, file = "rds_files/table_dis_id_3040_sumstat.rds")


# -------------------- disability transfer --------------------

disability_ec <- readRDS("rds_files/disability_ec.rds")
dis_trans_data <- filter(disability_ec, between(age, 18, 64) & dis_id == TRUE & dis_transfer == TRUE) # filters people between 18 and 64

# long to wide
dis_trans_data <- dis_trans_data %>% mutate(value = TRUE) %>% spread(key = sex, value = value, fill = FALSE, sep = "_" ) %>%
  mutate(value = TRUE) %>% spread(key = dis_type, value = value, fill = FALSE, sep = "_" ) %>%
  mutate(value = TRUE) %>% spread(key = dis_perception, value = value, fill = FALSE, sep = "_" )

dis_trans_data <- as.data.frame(dis_trans_data) # transforms tibble into data frame

var_dis_transfer <- c("nonemployed", "sick", "prev_care", "hospital", "good_health", "better_health", "dis_manuela", "bdh_transfer", 
                      "dis_perception_2", "dis_perception_3", "dis_perception_4", "dis_type_vision","dis_type_hearing", "dis_type_walking_stairs", "dis_type_cognitive", "dis_type_bathing_dressing", "dis_type_communication")

dis_trans_sumstat <- matrix(nrow = length(var_dis_transfer), ncol = 3, dimnames = list(var_dis_transfer))

for(i in var_dis_transfer){ # loop that fills the matrix
  dis_trans_sumstat[i,1] <- mean(dis_trans_data[, i]) # all
  dis_trans_sumstat[i,2] <- mean(dis_trans_data[dis_trans_data[,"sex_male"],i]) # males
  dis_trans_sumstat[i,3] <- mean(dis_trans_data[!dis_trans_data[,"sex_male"],i]) # females
}
