library(dplyr)
dt_Policy <- read.csv("./Data/lesson5_PolicyHistory.csv") %>% distinct(NrPolicy, NrObject, .keep_all = TRUE) 
dt_Claims <- read.csv("C./Data/lesson5_Claims.csv") %>% distinct(NrClaim, .keep_all = TRUE)

dt_pol_w_claims <- left_join(dt_Policy, 
                             dt_Claims, 
                             by = c("NrPolicy", "NrObject")
)
library(lubridate)
dt_pol_w_claims <- dt_pol_w_claims %>% 
                    mutate(Time_Exposure = lubridate::dmy(Dt_Exp_End) - lubridate::dmy(Dt_Exp_Start),
                           Ult_Loss = Paid + Reserves,
                           Burning_Cost = ifelse(is.na(Ult_Loss), 0,  Ult_Loss / as.integer(Time_Exposure)))

# 1. Use One-way analysis to find out 2 more features like Veh_type2, which can be usefull in the GLM model and might have potiantial influence on our target.

# Budem analyzova premenné D_age a Construct_year, keïe oboje majú potenciálny vplyv. 
# Od veku poistenca sa odráa napríklad skúsenosti, reakènı èas, ... 
# Od roku vıroby vozidla zas jeho technickı stav, hodnota, ...

# D_age:
library(ggplot2)
dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = D_age)) + 
  geom_jitter()
# Najväèšie hodnoty Burning_Cost sú u ¾udí vo veku 43-46 rokov. 
# Dôvod: Kríza stredného veku, alebo pouívanie vozidla detmi poistenca.
# Zaujímavosti: Viacero vozidiel majú poistenci nad 100 rokov. eby chybné dáta?

dt_pol_w_claims %>% 
  filter(Burning_Cost != 0) %>% 
  group_by(D_age) %>% 
  summarise(BC_avg = mean(Burning_Cost, na.rm  = TRUE),
            BC_median = median(Burning_Cost, na.rm = TRUE),
            cnt = n()) %>% 
  arrange(desc(BC_avg))
# Najvyšší priemer pre Burning_Cost majú ¾udia vo veku 60 rokov. 
# Dôvod: Je to zrejme spôsobené outlierermi viï predošlí graf.
# Najvyšší medián pre Burning_Cost majú ¾udia vo veku 69 rokov - aj keï z grafu to vyzeralo, e najrizikovejší budú vo veku 43-46 rokov. 
# Dôvod: Vo veku 43-46 rokov je ove¾a viac dát a teda aj ve¾a nízkych hodnôt, èo spôsobuje niší medián ako v roèníkoch s malım poètom dát.

dt_pol_w_claims[, 8] <- as.factor(dt_pol_w_claims[, 8])
dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = D_age)) + 
  geom_boxplot() +
  ylim(0, 100)
# Z grafu vidno, e je tam pri niektorıch roèníkoch ve¾a outliererov, obzvláš v roèníkoch s malım poètom dát, ktorí môu kazi našu predikciu.

# Construct_year:
library(ggplot2)
dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = Construct_year)) + 
  geom_jitter()
# Najväèšie hodnoty Burning_Cost sú u vozidiel vyrobenıch v roku 2005 a vyššom. 
# Dôvod: Vozidlá èasom strácajú svoju hodnotu, èím je odškodné u starších vozidiel menšie ako u novších vozidiel. 
# Zaujímavosti: V rokoch pred rokom 1995 je málo dát. Zrejme u také staré vozidlá nie sú pouívané, a ešte menej poisované.

dt_pol_w_claims %>% 
  filter(Burning_Cost != 0) %>% 
  group_by(Construct_year) %>% 
  summarise(BC_avg = mean(Burning_Cost, na.rm  = TRUE),
            BC_median = median(Burning_Cost, na.rm = TRUE),
            cnt = n()) %>% 
  arrange(desc(BC_avg))
# Najvyšší priemer pre Burning_Cost majú vozidlá vyrobené v roku 1997. 
# Dôvod: Málo dát - iba dve vozidlá, èo skres¾uje vısledky.
# Najvyšší priemer pre Burning_Cost pre roèník s relevantnım poètom dát (viac ako 5) je 2014.
# Najvyšší medián pre Burning_Cost majú opä vozidlá vyrobené v roku 1997.  
# Najvyšší medián pre Burning_Cost pre roèník s relevantnım poètom dát (viac ako 5) je 2004.

dt_pol_w_claims[, 17] <- as.factor(dt_pol_w_claims[, 17])
dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = Construct_year)) + 
  geom_boxplot() +
  ylim(0, 100)
# Z grafu vidno, e aj tu je pri niektorıch roèníkoch ve¾a outliererov, ktorí môu kazi našu predikciu.
# Obzvláš zavádzajúce môu by roky 2003-2006, kde je málo dát a outliereri tam lietajú veåmi vysoko.

# 2. Create simple GLM model with features you have found in previous exercise. Is the model predictive? Are you satisfied with your model? Use your knowledge about GLM theory to evaluate model, make a suggestion what could be improved and what would be your next steps. We will focus on your comments rather than code here.

# GLM model:
GLMmodel <- glm(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100),
                formula = Burning_Cost ~ D_age + Construct_year,
                family = Gamma())
summary(GLMmodel)
# Oboje premenné sú štatisticky vıznamné. Model je teda prediktívny.
# Pri Construct_year1999 sú NA hodnoty, èo by sa patrilo nejako do¾adi.
# Lepší model by bol zahàòajúci viacero faktorov, ako aj typ vozidla, èi typ zákazník, keïe aj tieto fakroty môu ma vıznamnı vplyv a vylepši tak našu predikciu.
# Ïalší postup by bol vyskúša viacero modelov (rôzny poèty a kombinácie faktorov) a potom na základe AIC alebo BIC kritéria vybra ten najvhodnejší a s ním robi ïalšie analızy.