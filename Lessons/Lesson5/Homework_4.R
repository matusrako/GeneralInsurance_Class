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

# Budem analyzovať premenné D_age a Construct_year, keďže oboje majú potenciálny vplyv. 
# Od veku poistenca sa odráža napríklad skúsenosti, reakčný čas, ... 
# Od roku výroby vozidla zas jeho technický stav, hodnota, ...

# D_age:
library(ggplot2)
dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = D_age)) + 
  geom_jitter()
# Najväčšie hodnoty Burning_Cost sú u ľudí vo veku 43-46 rokov. 
# Dôvod: Kríza stredného veku, alebo používanie vozidla detmi poistenca.
# Zaujímavosti: Viacero vozidiel majú poistenci nad 100 rokov. Žeby chybné dáta?

dt_pol_w_claims %>% 
  filter(Burning_Cost != 0) %>% 
  group_by(D_age) %>% 
  summarise(BC_avg = mean(Burning_Cost, na.rm  = TRUE),
            BC_median = median(Burning_Cost, na.rm = TRUE),
            cnt = n()) %>% 
  arrange(desc(BC_avg))
# Najvyšší priemer pre Burning_Cost majú ľudia vo veku 60 rokov. 
# Dôvod: Je to zrejme spôsobené outlierermi viď predošlí graf.
# Najvyšší medián pre Burning_Cost majú ľudia vo veku 69 rokov - aj keď z grafu to vyzeralo, že najrizikovejší budú vo veku 43-46 rokov. 
# Dôvod: Vo veku 43-46 rokov je oveľa viac dát a teda aj veľa nízkych hodnôt, čo spôsobuje nižší medián ako v ročníkoch s malým počtom dát.

dt_pol_w_claims[, 8] <- as.factor(dt_pol_w_claims[, 8])
dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = D_age)) + 
  geom_boxplot() +
  ylim(0, 100)
# Z grafu vidno, že je tam pri niektorých ročníkoch veľa outliererov, obzvlášť v ročníkoch s malým počtom dát, ktorí môžu kaziť našu predikciu.

# Construct_year:
library(ggplot2)
dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = Construct_year)) + 
  geom_jitter()
# Najväčšie hodnoty Burning_Cost sú u vozidiel vyrobených v roku 2005 a vyššom. 
# Dôvod: Vozidlá časom strácajú svoju hodnotu, čím je odškodné u starších vozidiel menšie ako u novších vozidiel. 
# Zaujímavosti: V rokoch pred rokom 1995 je málo dát. Zrejme už také staré vozidlá nie sú používané, a ešte menej poisťované.

dt_pol_w_claims %>% 
  filter(Burning_Cost != 0) %>% 
  group_by(Construct_year) %>% 
  summarise(BC_avg = mean(Burning_Cost, na.rm  = TRUE),
            BC_median = median(Burning_Cost, na.rm = TRUE),
            cnt = n()) %>% 
  arrange(desc(BC_avg))
# Najvyšší priemer pre Burning_Cost majú vozidlá vyrobené v roku 1997. 
# Dôvod: Málo dát - iba dve vozidlá, čo skresľuje výsledky.
# Najvyšší priemer pre Burning_Cost pre ročník s relevantným počtom dát (viac ako 5) je 2014.
# Najvyšší medián pre Burning_Cost majú opäť vozidlá vyrobené v roku 1997.  
# Najvyšší medián pre Burning_Cost pre ročník s relevantným počtom dát (viac ako 5) je 2004.

dt_pol_w_claims[, 17] <- as.factor(dt_pol_w_claims[, 17])
dt_pol_w_claims %>% 
  ggplot(aes(y = Burning_Cost, x = Construct_year)) + 
  geom_boxplot() +
  ylim(0, 100)
# Z grafu vidno, že aj tu je pri niektorých ročníkoch veľa outliererov, ktorí môžu kaziť našu predikciu.
# Obzvlášť zavádzajúce môžu byť roky 2003-2006, kde je málo dát a outliereri tam lietajú veĺmi vysoko.

# 2. Create simple GLM model with features you have found in previous exercise. Is the model predictive? Are you satisfied with your model? Use your knowledge about GLM theory to evaluate model, make a suggestion what could be improved and what would be your next steps. We will focus on your comments rather than code here.

# GLM model:
GLMmodel <- glm(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100),
                formula = Burning_Cost ~ D_age + Construct_year,
                family = Gamma())
summary(GLMmodel)
# Oboje premenné sú štatisticky významné. Model je teda prediktívny.
# Pri Construct_year1999 sú NA hodnoty, čo by sa patrilo nejako doľadiť. Taktiež nejako vyhodiť aspoň zopár veľmi vzdialených outliererov by zlepšilo predikciu.
# Lepší model by bol zahŕňajúci viacero faktorov, ako aj typ vozidla, či typ zákazníka, keďže aj tieto fakroty môžu mať významný vplyv a vylepšiť tak našu predikciu.
# Ďalší postup by bol vyskúšať viacero modelov (rôzny počty a kombinácie faktorov) a potom na základe AIC alebo BIC kritéria vybrať ten najvhodnejší a s ním robiť ďalšie analýzy.
