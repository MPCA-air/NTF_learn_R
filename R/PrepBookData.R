library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
books <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/R/R_Camp/Student Folder/Sample data/Brooklyn_Public_Library_Catalog.csv")
glimpse(books)
unique(books$TITLE)

all_cats <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/R/R_Camp/Student Folder/missing_cat_list.csv")

cats_age <- all_cats %>% 
  group_by(country, color) %>% 
  summarize(mean_age = mean(age, na.rm = T))

ggplot(cats_age, aes(x = country, y = mean_age, fill = color)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(title = "put_title_here",
       subtitle = "put_title_here",
       x     = "put_title_here",
       y     = "put_title_here",
       caption  = "put_title_here")

years <- c(1971, 1900, 1908)
names <- c('Kristie Ellickson', 'Dorian Kvale', 'Derek Nagel')
names_years <- as.data.frame(cbind(years, names))


ggplot(all_cats, aes(x = country, y = age)) +
  geom_boxplot(aes(group = age)) + 
  geom_jitter(alpha = 0.3, aes(color = color))


library(readr)
library(dplyr)
library(stringr)
library(readxl)
library(RODBC)

books <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/R/R_Camp/Student Folder/Sample data/Brooklyn_Public_Library_Catalog.csv")
glimpse(books)

books_filter <- filter(books, !grepl("[videorecording]", TITLE))

url <- "https://raw.githubusercontent.com/uchidalab/book-dataset/master/Task2/book32-listing.csv"
books <- read_csv(url, col_names = FALSE)
colnames(books) <- c("ID", "CoverImage", "URL", "Title", 'Author', "NumbersofSomething", "Genre")

books <- filter(books, Genre %in% c("Mystery, Thriller & Suspense", "Science Fiction & Fantasy", "Romance", "Humor & Entertainment", "Literature & Fiction", "Gay & Lesbian", "Arts & Photography", "Education & Teaching", "Parenting & Relationships", "Self-Help", "Computers & Technology", "Medical Books", "Science & Math", "Biographies & Memoirs", "History", "Politics & Social Sciences", "Reference", "Christian Books & Bibles", "Religion & Spirituality", "Sports & Outdoors", "Teen & Young Adult", "Children's Books", "Travel"))

db="X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/R/R_Camp/Student Folder/Sample data/Varietal Data.accdb"
bigdb=odbcConnectAccess2007(db)
sqlDrop(bigdb, "Books")
sqlSave(bigdb, books, tablename = "Books", rownames=FALSE)
odbcClose(bigdb) 



# Skip top 2 rows of garbage
movies <- read_excel("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/R/R_Camp/Student Folder/Sample data/IMDB movie data.xlsx", sheet = "Sheet3", skip = 2)


# Rename the 'color' column
movies <- rename(movies, movie_color = color)


#Drop missing values
movies <- filter(movies, 
                 !is.na(movie_color), 
                 !is.na(content_rating),
                 !is.na(duration),
                 !is.na(gross))

#Use `mutate()` to create a column with the movie's `gross` in millions of dollars.
movies <- mutate(movies, gross_mil = gross / 1000000)

glimpse(movies)
books_movies <- left_join(movies, books, by= c("movie_title" = "Title"))
