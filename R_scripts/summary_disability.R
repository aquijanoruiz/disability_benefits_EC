if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

# -------------------- loading the dataset --------------------
disability_ec <- readRDS("rds_files/disability_ec.rds")
disability_ec <- filter(disability_ec, between(age, 18, 64)) # filters people between 18 and 64
attach(disability_ec)

# -------------------- summary statistics table id holders vs non id holders --------------------

# age category -> splits ages into age groups
disability_ec$age_cat <- case_when(between(age, 18, 44) ~ "18-44", between(age, 45, 54) ~ "45-54", between(age, 55, 64) ~ "55-64")
disability_ec$age_cat <- factor(disability_ec$age_cat, levels = c("18-44", "45-54", "55-64"))

# long to wide
disability_ec <- disability_ec %>% mutate(value = TRUE) %>% spread(key = sex, value = value, fill = FALSE, sep = "_" ) %>%
  mutate(value = TRUE) %>% spread(key = dis_type, value = value, fill = FALSE, sep = "_" ) %>%
  mutate(value = TRUE) %>% spread(key = dis_degree, value = value, fill = FALSE, sep = "_" ) %>%
  mutate(value = TRUE) %>% spread(key = age_cat, value = value, fill = FALSE, sep = "_" ) %>% 
  mutate(value = TRUE) %>% spread(key = ethnicity, value = value, fill = FALSE, sep = "_" ) %>% 
  mutate(value = TRUE) %>% spread(key = marital_status, value = value, fill = FALSE, sep = "_" ) %>% 
  mutate(value = TRUE) %>% spread(key = education, value = value, fill = FALSE, sep = "_" ) %>% 
  mutate(value = TRUE) %>% spread(key = n_child_cat, value = value, fill = FALSE, sep = "_" )

disability_ec <- as.data.frame(disability_ec) # transforms tibble into data frame

var <- c("nonemployed", "sick", "prev_care", "hospital", "good_health", "better_health", "age_cat_18-44", 
         "age_cat_45-54", "age_cat_55-64", "dis_degree_2", "dis_degree_3", "dis_degree_4", "dis_type_hearing", 
         "dis_type_walking_stairs", "dis_type_cognitive", "dis_type_bathing_dressing", "dis_type_communication",
         "marital_status_non-partnered", "marital_status_cohabiting", "marital_status_married", "education_primary", 
         "education_secondary", "education_tertiary", "n_child_cat_0", "n_child_cat_1", "n_child_cat_2", "n_child_cat_3",
         "n_child_cat_4", "n_child_cat_5", "n_child_cat_6ormore")

all_dis_id <- matrix(nrow = length(var), ncol = 6, dimnames = list(var))

for(i in var){ # loop that fills the matrix
  all_dis_id[i,1] <- mean(disability_ec[disability_ec[,"dis_id"], i])
  all_dis_id[i,2] <- mean(disability_ec[!disability_ec[,"dis_id"], i])
  all_dis_id[i,3] <- mean(disability_ec[disability_ec[,"sex_male"] & disability_ec[,"dis_id"],i])
  all_dis_id[i,4] <- mean(disability_ec[disability_ec[,"sex_male"] & !disability_ec[,"dis_id"],i])
  all_dis_id[i,5] <- mean(disability_ec[disability_ec[,"sex_female"] & disability_ec[,"dis_id"],i])
  all_dis_id[i,6] <- mean(disability_ec[disability_ec[,"sex_female"] & !disability_ec[,"dis_id"],i])
}

colnames(all_dis_id) <- c("all_id_holder", "all_non_id_holder", "male_id_holder", 
                          "male_non_id_holder", "female_id_holder", "female_non_id_holder")

saveRDS(all_dis_id, file = "rds_files/table1_all_dis_id.rds")

# -------------------- id holders disability percentage summary statistics --------------------

sum(disability_ec$dis_id == TRUE) # number of people with disability id

filter(disability_ec, dis_id == TRUE) %>% ggplot(aes(x = dis_id_percent)) + theme_minimal() +
  geom_histogram(binwidth = 1, fill = "white", col = "black") +
  labs(y = "number of people with disability", x = "disability percentage written on the disability id", title = "")

# disability percentage category -> splits disability percentage into percentage groups
disability_ec$dis_id_percent_cat <- case_when(dis_id_percent < 40 ~ "30-39", between(dis_id_percent, 40, 49) ~ "40-49",
  between(dis_id_percent, 50, 74) ~ "50-74", between(dis_id_percent, 75, 84) ~ "75-84", dis_id_percent > 84 ~ "85-100")
