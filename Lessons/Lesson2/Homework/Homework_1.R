# Find out, which __year__ was the __most terrific__ for portfolio you have identified as __most profitable__ during the lesson and 
# show it on the chart using `ggplot2` package. Write an explanation about your findings into the code as comment.
# __Commit__ it to your repository into `Lessons/Lesson2/Homework`.

## Code
# Načítanie potrebných R-kových knižníc:
library(dplyr)
library(ggplot2)

# Načítanie dát:
dt_KPI_raw <- read.csv("./Data/lesson2_KPI.csv")

# Očistenie od záporného poistného:
dt_KPI_raw %>% 
  mutate(Premium = ifelse(Premium < 0, 0, Premium))

# Hľadanie najprofitabilnejšieho portfólia:
dt_KPI_raw %>%  
  mutate(UWR = Premium - Expenses - Losses) %>% 
  group_by(Unit) %>% 
  summarize(UWR = sum(UWR, na.rm = TRUE)) %>% 
  arrange(UWR)

# Zoskupenie podľa roku:
dt_KPI_raw %>%  
  mutate(UWR = Premium - Expenses - Losses) %>% 
  filter(Unit == "Unit7") %>% 
  group_by(Year) %>% 
  summarize(UWR = sum(UWR, na.rm = TRUE)) %>% 
  arrange(UWR)

# Grafické znázornenie:
dt_KPI_raw %>% 
  mutate(UWR = Premium - Expenses - Losses) %>% 
  filter(Unit == "Unit7") %>% 
  group_by(Year) %>% 
  summarize(UWR = sum(UWR, na.rm = TRUE)) %>% 
  ggplot(aes(x = reorder(Year, UWR), y = UWR)) + 
  geom_col()




# Your Explanation about analysis:
1) Očistil som dáta od záporného poistného.
2) Vyhľadal som najprofitabilnejšie portfólio (t. j. s najvyššou hodnotou Underwriting Result) -> Unit7.
3) Vyfiltroval som Underwriting Result pre Unit7.
4) Zoskupil som to podľa rokov.
5) Porovnal som ich Underwriting Result a graficky znázornil.
Výsledok: Najhorší rok pre najprofitabilnejšie portfólio Unit7 vyšiel 2014.
Dôvod: Rok 2014 má najmenšiu hodnotu Underwriting Result.
