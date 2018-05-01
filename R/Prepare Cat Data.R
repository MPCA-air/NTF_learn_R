# Clean the cat data
library(readr)
library(dplyr)


# Prepare tables

# Create cat shelter personality database
cat_traits <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/R/R_Camp/Intro to R/Sample data/Cat_Traits.csv")

# Clean names
names(cat_traits)

names(cat_traits) <- gsub("P[^_]+_", "", names(cat_traits)) %>% tolower()

names(cat_traits)[10] <- "grumpy"

# Select 5 traits
cats <- cat_traits[, c(4, 10, 21, 36, 42, 43, 53, 54)]

cat_names   <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/R/R_Camp/Intro to R/Sample data/cat_names.csv", col_names = F)[ , 1] %>% 
               filter(!is.na(X1), !grepl("([\\])", X1), !grepl("[[digit:]]", X1), !grepl("Kylie", X1))
  
cat_names <- cat_names[c(1:428, 430:1212, 1214:nrow(cat_names)), ]

mutate(cat_names, char_cnt = nchar(X1)) %>% arrange(-char_cnt)

cat_color   <- c("black", "tabby", "grey and brown", "brown","peaches and cream", "grey and black", "grey", "salt and pepper", "striped", 'white', "orange", "butterscotch")

cat_country <- c("USA", "Canada", "Thailand","France", "UK", "Spain", "Peru", "Australia", "New Zealand", "China", "Germany")

n_cats <- nrow(cats)

cats <- mutate(cats, 
               name       = sample(cat_names$X1, n_cats, replace = T),
               color      = sample(cat_color, n_cats, replace = T),
               tag_num    = 1:n_cats,
               gender     = cat_sex,
               age        = sample(c(3:7,2:11,1:20,1:21), n_cats, replace = T),
               country    = sample(cat_country, n_cats, replace = T)) %>%
  select(-tag_num, -cat_sex) %>%
  select(name, color, country, age, gender, everything())


#... cats <- unique(select(cats, -country, -name))

# Create lost cat trait tables

# Names and colors
tag_numbers <- c(456, 11, 3, 248, 12, 50, 44, 133, 9, 222, 34, 93, 77)

lost_names  <- c("Longmire", "Sidhartha", "Santa Claws", "Prince Casper", 
                 "Edward Scissorclaws", "Aloysius", "Feline Dion", "Sir Mauller",
                 "Frodo Holmes", "Ballyhoo", "Garfield", "Tumford Stout",
                 "Bamboozled")

lost_colors <- c("peaches and cream", "grey and brown", "grey", "salt and pepper",
                 "striped", 'butterscotch', 'white', 'spotted', "tabby", 
                 "brown", "orange", "salt and pepper", "black")

genders <- c("Male","Male","Male","Female","Male","Male","Female","Female","Male","Female","Male", "Female", "Male")

countries <- c("Peru","Thailand","Spain","New Zealand","UK","USA","Canada","Peru","Australia",
               "Germany","USA","Sweet apple green","USA")

# Set traits
clumsy    <- c(1,1,1,1,1,7,5,6,1,3,1,7,7)            
grumpy    <- c(6,3,2,7,1,2,4,5,5,2,6,3,2)           
greedy    <- c(6, 7, 7, 2, 4, 2, 3, 7, 5, 7, 7, 7, 1)              
fearful_of_people  <- c(5, 1, 3, 6, 1, 1, 4, 7, 2, 5, 1, 1, 4)
friendly_to_people <- c(4, 4, 3, 1, 7, 9, 5, 2, 5, 7, 1, 7, 6)
playful   <- c(2, 6, 7, 2, 5, 8, 3, 5, 3, 6, 1, 5, 6)

set.seed(27)

lostcats <- mutate(cat_traits[1:13, c(4, 10, 21, 36, 42, 43, 53, 54)], 
                   name    = lost_names[1:13],
                   color   = lost_colors[1:13],
                   gender  = genders,
                   age     = paste(sample(c("<",">"), 13, replace = T), sample(3:11, 13, replace = T), "yrs"),
                   tag_num = tag_numbers,
                   country = countries,
                   clumsy  = clumsy,         
                   grumpy  = grumpy,           
                   greedy  = greedy,               
                   fearful_of_people = fearful_of_people,    
                   friendly_to_people = friendly_to_people, 
                   playful = playful) %>%
  mutate(all_traits = paste(clumsy, grumpy, greedy, fearful_of_people,    
                            friendly_to_people, playful)) %>%
  select(-cat_sex) 

# Add lost cats to shelter table
lostcats$age <- c("> 8 yrs", "< 9 yrs",  "< 10 yrs", "> 5 yrs",  "> 3 yrs",  "> 8 yrs",  
                  "> 7 yrs",  "< 10 yrs", "< 9 yrs", "< 4 yrs",  "< 9 yrs",  "< 5 yrs",  "> 9 yrs") 

lostcats_all <- lostcats

cat(lostcats_all$age)

lostcats_all$age <- c(13, 8, 2, 6, 7, 10, 12, 9, 5, 3, 1, 4, 14)

cats <- bind_rows(filter(cats, 
                         !paste(clumsy, grumpy, greedy, fearful_of_people,    
                                friendly_to_people, playful) %in% lostcats$all_traits), 
                  select(lostcats_all, -all_traits, -tag_num))


cats <- sample(cats)

cats$owner_phone <- paste("651", sample(1500000:9019999, nrow(cats)), sep="-")

# Reduce ages for striped cats
cats <- mutate(cats, age = ifelse(color == "striped", 
                                   sample(c(3:7,2:11,1:12,1:15,1:5,1:2), nrow(cats), replace = T), 
                                   age))

# Check that the special lost cats are unique
cat_check <- left_join(cats, select(lostcats_all, -name, -country, -age)) %>% filter(!is.na(tag_num))


write_csv(select(cats, name, color, age, gender, country, everything()), "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/R/R_Camp/Student Folder/missing_cat_list.csv")

cats <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/R/R_Camp/Student Folder/missing_cat_list.csv")

# Save single missing cat data
lostcats <- lostcats %>% 
  select(tag_num,  color, age,  gender, everything()) %>%
  select(-name, -all_traits, -country)


for(i in 1:nrow(lostcats)) {
  
  write_csv(lostcats[i, ], paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/R/R_Camp/Student Folder/Found cats/foundcat_tag", lostcats[i, "tag_num"], ".csv"))
  
}


# Tag 453
found <- read_csv(paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/R/R_Camp/Student Folder/Found cats/foundcat_tag", 456, ".csv"))

for(i in 1:nrow(lostcats)) {
  
  print(read_csv(paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/R/R_Camp/Student Folder/Found cats/foundcat_tag", lostcats[i, "tag_num"], ".csv")))
  
}





#