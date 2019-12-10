# download data set from figshare
download.file("https://ndownloader.figshare.com/files/11492171",
              "data/SAFI_clean.csv", mode = "wb")


# load tidyverse
library(tidyverse)

# import 
interviews <- read_csv("data/SAFI_clean.csv", na = "NULL")

# inspect the data
interviews

# selecting columns and filtering rows
select(interviews, village, no_membrs, years_liv)

filter(interviews, village == "God")


# Pipes
interviews2 <- filter(interviews, village == "God")
interviews_god <- select(interviews2, no_membrs, years_liv)

interviews_god <- select(filter(interviews, village == "God"), 
                         no_membrs, years_liv)

interviews %>% filter(village == "God") %>% 
  select(no_membrs, years_liv)


interviews_god <- interviews %>% filter(village == "God") %>% 
  select(no_membrs, years_liv)

# exercise
interviews_irrig <- interviews %>% 
  filter(memb_assoc == "yes") %>% 
  select(affect_conflicts, liv_count, no_meals)

# Mutate
interviews %>% 
  mutate(people_per_room = no_membrs / rooms)

interviews %>% 
  filter(!is.na(memb_assoc)) %>% 
  mutate(people_per_room = no_membrs / rooms)

interviews_total_meals <- interviews %>% 
  mutate(total_meals = no_membrs * no_meals) %>% 
  filter(total_meals > 20) %>% 
  select(village, total_meals)

# split-apply-combine and the summarize() function

interviews %>% 
  group_by(village) %>% 
  summarize(mean_no_membrs = mean(no_membrs))


interviews %>% 
  filter(!is.na(memb_assoc)) %>% 
  group_by(village, memb_assoc) %>% 
  summarize(mean_no_membrs = mean(no_membrs),
            min_membrs = min(no_membrs)) %>% 
  arrange(desc(min_membrs))


interviews %>% 
  count(village, sort = TRUE)


interviews %>% 
  count(no_meals)

interviews %>% 
  group_by(village) %>% 
  summarize(mean_no_membrs = mean(no_membrs),
            min_no_membrs = min(no_membrs),
            max_no_membrs = max(no_membrs),
            num = n())

library(lubridate)
interviews %>% mutate(month = month(interview_date),
                      day = day(interview_date),
                      year = year(interview_date)) %>% 
  group_by(year, month) %>% 
  summarize(max_no_membrs = max(no_membrs))


# reshaping
interviews_spread <- interviews %>% 
  mutate(wall_type_logical = TRUE) %>% 
  spread(key = respondent_wall_type, value = wall_type_logical, fill = FALSE)

interviews_gather <- interviews_spread %>% 
  gather(key = respondant_wall_type, value = "wall_type_logical",
         burntbricks:sunbricks)

interviews_items_owned <- interviews %>% 
  separate_rows(items_owned, sep = ";") %>% 
  mutate(items_owned_logical = TRUE) %>% 
  spread(key = items_owned, value = items_owned_logical, fill = FALSE)


interviews_items_owned <- interviews_items_owned %>% 
  rename(no_listed_item = `<NA>`)
