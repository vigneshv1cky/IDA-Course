#ISE/DSA 5103 Demo to tidyr, dplyr, and magritter from the tidyverse
#Charles Nicholson, Ph.D.
#September 30, 2019
#Adpated from https://dplyr.tidyverse.org/



#load all of the packages associated with the tidyverse
#includes dplyr, tidyr, and magritter
library(tidyverse)

#data set for the example
data(starwars)

#take a quick look at the data...
head(starwars)

#an additional method for taking a quick look at the data
glimpse(starwars)

############ filter

#filter observations based on a criteria
starwars %>% 
  filter(species == "Droid")

#filter observations based on a criteria
starwars %>% filter(!is.na(gender)) 

############ select and select_if

#select certain columns
starwars %>% 
  select(name, height, mass, birth_year)

#select columns with criteria
starwars %>% 
  select(name, ends_with("color"))

starwars %>% 
  select_if(is.numeric)

starwars %>% 
  select_if(is.character)

############ select and filter

#combine filter and select columns
starwars %>% 
  filter(is.na(gender))  %>% 
  select(name, ends_with("color"))

############ mutate and mutate_if and trasmute/transmute_if

#create new variables through transformation 
starwars %>% 
  mutate(bmi = mass / ((height / 100)  ^ 2)) %>%
  select(name:mass, bmi)

#create new variables through transformation 
starwars %>% 
  mutate(bmi = mass / ((height / 100)  ^ 2)) %>%
  mutate_if(is.numeric, scale) %>%
  select(name:mass, bmi)

#selective mutation
starwars %>% 
  mutate(bmi = mass / ((height / 100)  ^ 2)) %>%
  transmute_if(is.numeric, scale) 

############ arrange

#sort the data
starwars %>% 
  arrange(desc(mass))

##sorting based on new variables created
starwars %>% 
  mutate(bmi = mass / ((height / 100)  ^ 2)) %>%
  select(name:mass, bmi) %>%
  arrange(desc(bmi))

############ group_by and summarize commands

#group by and summarization
starwars %>% 
  group_by(homeworld) %>%
  summarize(n = n()) %>%
  arrange(desc(n))


#group by and summarization
#filtering to make sure that mass and species are not missing
starwars %>% 
  drop_na(mass, species)  %>%
  group_by(species) %>%
  summarise(
    n = n(),
    mass = mean(mass, na.rm = TRUE)
  ) 

#group by and summarization with pre and post-filtering
starwars %>%
  drop_na(mass, species)  %>%
  group_by(species) %>%
  summarise(
    n = n(),
    mass = mean(mass, na.rm = TRUE)
  ) %>%
  filter(n > 1,
         mass > 40) %>% 
  arrange(desc(mass))

############ use with ggplot as well

#and connect it to ggplot if you like
starwars %>%
  drop_na(mass, species)  %>%
  group_by(species) %>%
  summarize(
    n = n(),
    height = mean(height, na.rm=TRUE),
    mass = mean(mass, na.rm = TRUE)
  ) %>%
  filter(n > 1,
         mass > 40) %>% 
     ggplot(aes(mass,height,color=species)) + geom_point(size=8)


############ gather and spread

#demonstrate reshaping data using gather...

#multiple characteristics per character on each line
starwars %>% 
  select(name, ends_with("color"))

#now, one characteristic per character on each line
starwars %>% 
  select(name, ends_with("color")) %>%
  gather(key = "attribute", value="color", -name)

#now, one characteristic per character on each line (sorted)
starwars %>% 
  select(name, ends_with("color")) %>%
  gather(key = "attribute", value="color", -name) %>%
  arrange(name)


#now, one characteristic per character on each line (sorted and stored as dataframe)
temp<-starwars %>% 
  select(name, ends_with("color")) %>%
  gather(key = "attribute", value="color", -name) %>%
  arrange(name)

#the new data frame
temp

#we can spread the gathered data back out...
temp %>% 
  spread (key = "attribute", value="color") %>%
  arrange(name)


