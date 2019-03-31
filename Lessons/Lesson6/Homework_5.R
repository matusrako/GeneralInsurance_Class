library(dplyr)
dt_pol_w_claims <- readRDS("./Data/lesson6_dt_pol_w_claims.rds")
set.seed(58742) 
ind <- sample(2, nrow(dt_pol_w_claims), replace = TRUE, prob = c(0.80, 0.20))
dt_pol_w_claims <- mutate(dt_pol_w_claims, data_status = ifelse(ind == 1, "Training",
                                                         ifelse(ind == 2, "Validation", "Unseen")))
train <- dt_pol_w_claims %>% filter(data_status == "Training")
val <- dt_pol_w_claims %>% filter(data_status == "Validation")

MSE <- function(prediction, actual)
{
  return(sum((prediction-actual)^2, na.rm = TRUE)/length(prediction))
}

# 1. Model z predošlej úlohy - treningová vzorka:
GLMmodel1 <- glm(data = train,
                formula = Burning_Cost ~ D_age + Construct_year,
                family = Gamma())
MSE(predict(GLMmodel1, train, type = "response"), train$Burning_Cost) # 198.7277

# 2. Model s pridanou novou premennou - Veh_type2:
GLMmodel2 <- glm(data = train,
              formula = Burning_Cost ~ D_age + Construct_year + Veh_type2,
              family = Gamma())
MSE(predict(GLMmodel2, train, type = "response"), train$Burning_Cost) # 193.9859
# Hodnota MSE o trochu klesla, model je teda trochu robustnejší.

# 3. Capping Strategy:
source("./Lessons/Lesson6/Support/emb_chart.R")
# Podľa D_age
emblem_graph(
  dt.frm = train %>% cbind(data.frame(pred = predict(GLMmodel2, train, type = "response"))),
  x_var = "D_age",
  target = "Burning_Cost",
  prediction = "pred" )
# Z grafu vidíme, že u vodičoch mladších ako 33, resp. starších ako 56, sú pomerne veľké výkyvy dôsledku malého počtu dát, čo negatívne ovplyvňuje trend.
# Zgrupime preto tieto skupiny. 
train <- train %>% mutate(D_age = ifelse(D_age <= 32, 32, D_age))
train <- train %>% mutate(D_age = ifelse(D_age >= 57, 57, D_age)) 

# Podľa Construct_year
emblem_graph(
  dt.frm = train %>% cbind(data.frame(pred = predict(GLMmodel2, train, type = "response"))),
  x_var = "Construct_year",
  target = "Burning_Cost",
  prediction = "pred" )
# Z grafu vidíme, že u vozidiel vyrobených pred rokom 2006, sú pomerne veľké výkyvy dôsledku malého počtu dát, čo negatívne ovplyvňuje trend.
# Zgrupime preto túto skupinu. 
train <- train %>% mutate(Construct_year = ifelse(Construct_year <= 2005, 2005, Construct_year))

GLMmodel3 <- glm(data = train,
                 formula = Burning_Cost ~ D_age + Construct_year + Veh_type2,
                 family = Gamma())
MSE(predict(GLMmodel3, train, type = "response"), train$Burning_Cost) # 194.0015
# Hodnota MSE sa veľmi mierne zvýšila, teda možno skonštatovať, že zgrupovanie nám nepomohlo model vylepšiť.

# 4. Category Grouping:
emblem_graph(
  dt.frm = train %>% cbind(data.frame(pred = predict(GLMmodel2, train, type = "response"))),
  x_var = "Veh_type2",
  target = "Burning_Cost",
  prediction = "pred" )
# Z grafu vidíme, že u vozidiel typu MOTORBIKE a TRUCK, sú pomerne veľké výkyvy dôsledku malého počtu dát, čo negatívne ovplyvňuje trend.
# Zgrupime preto tieto skupiny.
train <- train %>% mutate(Veh_type2 = ifelse(as.character(Veh_type2) == 'PICKUP' | as.character(Veh_type2) == 'CAR', 'CAR & PICKUP', as.character(Veh_type2)))

GLMmodel4 <- glm(data = train,
                 formula = Burning_Cost ~ D_age + Construct_year + Veh_type2,
                 family = Gamma())
MSE(predict(GLMmodel4, train, type = "response"), train$Burning_Cost) # 194.4265
# Hodnota MSE sa nám zvýšila ešte výraznejšie ako v prípade Capping Strategy, teda ani kategoriálne zgrupovanie nám nepomohlo.

# Záver: Za najlepší model je GLMmodel2 s tromi premennými bez zgrupovania dát.
