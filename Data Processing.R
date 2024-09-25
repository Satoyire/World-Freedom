library(tidyverse)
library(lubridate)
library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(ggthemes)
library(plotly)
library(shiny)


#DATASET 1
# Prepare raw World Freedom data  
WFI <- read_excel("C:\\Users\\satoy\\Desktop\\STA 504\\Project2\\data\\Aggregate_data.xlsx", sheet = "FIW06-24")

Freedom <- WFI %>% 
  rename(Country="Country/Territory", Territory = "C/T?", Year = Edition, CL_Score = CL, PR_Score = PR, Overall_Score = Total) %>% 
  mutate(Country = gsub(pattern = "^St\\.\\s", replacement = "Saint ", Country)) %>% 
  filter(Territory=="c") %>% 
  select(Country, Region, Year, Status, PR_Score, CL_Score, "PR Rating", "CL Rating", Overall_Score) 
str(Freedom)


#DATASET 2
#Prepare Aggregated World Freedom Data with no countries
Territory_data <- read_excel("C:\\Users\\satoy\\Desktop\\STA 504\\Project2\\data\\Country_and_Territory_Ratings.xlsx", sheet = "Historical distribution")

Distribution <- Territory_data %>% 
  rename(Year="Year(s) Under Review**") %>% 
  mutate(Year = ifelse(Year == "Dec 1, 2005-Dec 31, 2006", "2006",
                       ifelse(Year == "Dec 1, 2004-Nov 30, 2005", "2005",
                              ifelse(Year == "Dec 1, 2003-Nov 30, 2004", "2004",
                                     ifelse(Year == "Jan 1, 2003-Nov 30, 2003", "2003",
                                            Year))))) %>% 
  mutate(Year=as.numeric(Year)) %>% 
  filter(Year>=2006)

#Create a Distribution table containing the required columns
Dist <- Distribution[ , 2:9]
Dist_data <- Dist %>% 
  rename(No_of_F_Countries = "Number of F Countries", No_of_PF_Countries = "Number of PF Countries", No_of_NF_Countries = "Number of NF Countries")

#Create a new table showing the distribution Freedom statuses
NewDist <- Dist_data %>% 
  rename(Free = No_of_F_Countries, "Partially Free" = No_of_PF_Countries, "Not Free" = No_of_NF_Countries) %>%
  pivot_longer(cols = c("Free", "Partially Free", "Not Free"),
               names_to = "Freedom_Status",
               values_to = "Number") %>% 
  mutate("% of Countries" = Number / `Total Countries`) %>% 
  mutate(Year=as.character(Year)) %>% 
  select(-3:-5)
NewDist
str(Dist_data)


#Load World Map Data
World_Map <- read_sf("C:\\Users\\satoy\\Desktop\\STA 504\\Project2\\data\\World_Countries_(Generalized)\\World_Countries_Generalized.shp")


# Extract country names from both World_Map and Freedom data sets
country_names_World_Map <- unique(World_Map$COUNTRY)
country_names_Freedom <- unique(Freedom$Country)

# Compare country names
common_countries <- intersect(country_names_World_Map, country_names_Freedom)
unique_countries_World_Map <- setdiff(country_names_World_Map, country_names_Freedom)
unique_countries_Freedom <- setdiff(country_names_Freedom, country_names_World_Map)

# Output the comparison results
cat("Common countries:", common_countries, "\n")
cat("Unique countries in World_Map:", unique_countries_World_Map, "\n")
cat("Unique countries in Summary_Freedom:", unique_countries_Freedom, "\n")

#Change Country Names in Freedom data set to Match World Map names
# Define a named vector to map country names to their standardized versions
Map_Country_name <- c("Brunei" = "Brunei Darussalam",
                      "Congo (Brazzaville)" = "Congo",
                      "Congo (Kinshasa)" = "Congo DRC",
                      "The Gambia" = "Gambia")

# Apply the mapping using the named vector
Freedom_data <- Freedom %>%
  mutate(Country = ifelse(Country %in% names(Map_Country_name), 
                          Map_Country_name[Country], 
                          Country))


#Summarize Freedom_data for the world
World_Freedom_data <- Freedom_data %>% 
  select(Country, Region, Year, CL_Score, PR_Score, Overall_Score) %>% 
  group_by(Country) %>% 
  summarise(
    Overall_Score = round(mean(Overall_Score), 0), 
    "Political Rights" = round(mean(PR_Score), 0), 
    "Civil Liberties" = round(mean(CL_Score), 0), 
    .groups = "drop") %>% 
  mutate(Status= ifelse(Overall_Score<=39, "Not Free",
                        ifelse(Overall_Score<=69, "Partially Free",
                               "Free")))


#Change Country Names in World_Map Data to Match Freedom Names
World_Map_Data <- World_Map %>% 
  mutate(COUNTRY = ifelse(COUNTRY == "Russian Federation", "Russia", 
                          ifelse(COUNTRY == "Turkiye", "Turkey",
                                 ifelse(COUNTRY == "Côte d'Ivoire", "Cote d'Ivoire", COUNTRY))))

#Combine Summarized World Freedom data and World Map Data
Avg_World_Freedom <- left_join(World_Map_Data, World_Freedom_data, by = c("COUNTRY" = "Country")) %>% 
  filter(!is.na(Overall_Score))

save(Freedom_data, Dist_data, NewDist, World_Freedom_data, Avg_World_Freedom, 
     file="worldfreedomdata.RData")

load("worldfreedomdata.RData")
