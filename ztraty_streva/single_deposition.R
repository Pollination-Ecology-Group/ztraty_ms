#----------------------------------------------------------#
#
#
#                   Single deposition
#
# 
#
#
#                       P. Svanda
#                         2025
#              
#----------------------------------------------------------#

#----------------------------------------------------------#
# 0. Priprava skriptu ----
#----------------------------------------------------------#

#skript jede rovnou na package {renv}, seznam packages bude prilozen ve slozce R.
renv::install("here")
renv::install("tidyverse")  
renv::install("readxl")
renv::install("usethis")
renv::install("lmtest")
renv::install("nortest")
renv::install("moments")
renv::install("pscl")
renv::install("AER")
renv::install("MASS")
renv::install("performance")
renv::install("DHARMa")
renv::install("mgcv")
renv::install("ggeffects")
renv::install("lmtest")
renv::install("topmodels", repos = "https://R-Forge.R-project.org")
renv::install("lme4")
renv::install("MuMIn")
renv::install("patchwork")
renv::install("ggplot2")
renv::install("sjPlot")
renv::install("effects")


library("here")  
library("tidyverse")  
library("readxl")
library("usethis")
library("lmtest")
library("nortest")
library("moments")
library("pscl")
library("AER")
library("MASS")
library("performance")
library("DHARMa")
library("mgcv")
library("ggeffects")
library("lmtest")
library("topmodels")
library("lme4")
library("MuMIn")
library("patchwork")
library("ggplot2")
library("sjPlot")
library("effects")

#----------------------------------------------------------#
# 1. Vlozeni dat ----
#----------------------------------------------------------#

handrkov <- readxl::read_xlsx(here::here("ztraty_streva","data_ztraty","Handrkov_raw.xlsx"), 
                              na = c("", NA), 
                              col_types = c("text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "numeric", "skip", "numeric", "date", "text", "numeric"),
                              col_names = c("id", "druh_kytky", "typ", "klec_pytlik", "mnozstvi_roztoku", "mnozstvi_pylu_konspecificky", "mnozstvi_pylu_celkem", "mnozstvi_pylu_heterospecificky", "vyska_rostliny", "barva_pytliku", "opylovac", "chovani", "delka_navstevy", "zacatek_konec_cas", "pocet_navstev_hodinu", "cas_navstevy", "pohlavi", "rok"),
                              skip = 1
)

## 1.1 Cisteni datasetu, sjednoceni jmen ----
#----------------------------------------------------------#

bom <- which(handrkov$opylovac == "Bom.Lap.")
handrkov$opylovac[bom] <- "bom.lap."

syl <- which(handrkov$opylovac == "bom.sylvarum")
handrkov$opylovac[syl] <- "bom.syl."

arb <- which(handrkov$opylovac == "Eri.Arb.")
handrkov$opylovac[arb] <- "eri.arb."

intricaria <- which(handrkov$opylovac == "intricaria")
handrkov$opylovac[intricaria] <- "eri.intr."

sca <- which(handrkov$opylovac == "scaera")
handrkov$opylovac[sca] <- "sca.spp."

sca2 <- which(handrkov$opylovac == "scaeva")
handrkov$opylovac[sca] <- "sca.spp."

interuptus <- which(handrkov$opylovac == "interuptus")
handrkov$opylovac[interuptus] <- "eri.inte."

ten <-  which(handrkov$opylovac == "tenax")
handrkov$opylovac[ten] <- "eri.ten."

hel <-  which(handrkov$opylovac == "helophilus")
handrkov$opylovac[hel] <- "hel.spp."

das <-  which(handrkov$opylovac == "dasysyrphus")
handrkov$opylovac[das] <- "das.spp."

sar <-  which(handrkov$opylovac == "sarcophaga")
handrkov$opylovac[sar] <- "sar.spp."

sph <-  which(handrkov$opylovac == "sphaerophoria")
handrkov$opylovac[sph] <- "sph.spp."


## 1.2 nove promenne pro rok, den, hodinu a minutu ----
#----------------------------------------------------------#

handrkov$rok <-  substring(handrkov$cas_navstevy,1,4)
handrkov$den <-  substring(handrkov$cas_navstevy,9,10)
handrkov$casH <- substring(handrkov$cas_navstevy,12,13)
handrkov$casM <- substring(handrkov$cas_navstevy,15,16)
handrkov$cashHM <- (as.numeric(handrkov$casH)*60)+as.numeric(handrkov$casM)

## 1.3 uprava hodnot tabulek podle poznamek (dokument handrkov_raw_raw) ----
#----------------------------------------------------------#

#22_suc_R_2: pouze 1 prasnik praskly (x4)
#22_suc_R_16,18,47,52,54,68,71,72: 1 tycinka chybi (x1.25)
#22_suc_R_103,107: chybi 2 ticinky (x2)
#24_suc_R_6: pouze 1 tycinka (x4)
#24_suc_R_31,32,98,105,107: 1 tycinka chybi (x1.25)
#24_suc_R_74,96: chybi 2 tycinky (x2)
handrkov$mnozstvi_pylu_celkem[handrkov$id == "22_Suc_R2"] <- 
  handrkov$mnozstvi_pylu_celkem[handrkov$id == "22_Suc_R2"] * 4
handrkov$mnozstvi_pylu_celkem[handrkov$id == "24_Suc_R6"] <- 
  handrkov$mnozstvi_pylu_celkem[handrkov$id == "24_Suc_R6"] * 4
handrkov$mnozstvi_pylu_celkem[handrkov$id %in% c("22_Suc_R16","22_Suc_R18","22_Suc_R47","22_Suc_R52",
                                                 "22_Suc_R54","22_Suc_R68","22_Suc_R71","22_Suc_R72" )] <- handrkov$mnozstvi_pylu_celkem[handrkov$id  %in% c("22_Suc_R16","22_Suc_R18","22_Suc_R47","22_Suc_R52",                                                                                                                                                             "22_Suc_R54","22_Suc_R68","22_Suc_R71","22_Suc_R72")] * 1.25
handrkov$mnozstvi_pylu_celkem[handrkov$id %in% c("22_Suc_R31","22_Suc_R32","22_Suc_R98","22_Suc_R105","22_Suc_R107")] <- 
  handrkov$mnozstvi_pylu_celkem[handrkov$id  %in% c("22_Suc_R31","22_Suc_R32","22_Suc_R98","22_Suc_R105","22_Suc_R107")] * 1.25
handrkov$mnozstvi_pylu_celkem[handrkov$id  %in% c("22_Suc_R103","22_Suc_R107")] <- 
  handrkov$mnozstvi_pylu_celkem[handrkov$id %in% c("22_Suc_R103","22_Suc_R107")] * 2
handrkov$mnozstvi_pylu_celkem[handrkov$id  %in% c("24_Suc_R74","24_Suc_R96")] <- 
  handrkov$mnozstvi_pylu_celkem[handrkov$id %in% c("24_Suc_R74","24_Suc_R96")] * 2
#22_suc_R_28,22: roztrhane nejde pocitat (odstranit)
handrkov <- subset(handrkov, id != "22_Suc_R28")
handrkov <- subset(handrkov, id != "22_Suc_R22")
#24_suc_R_34: roztrhane nejde pocitat (odstranit)
handrkov <- subset(handrkov, id != "24_Suc_R34")
#22_Suc_R22 jde po uprave do na 1320 a predtim mela velky removal i tak -> asi spatne nabrano kdyz pouze 1 tycinka -> bud nechat bez upravy nebo vyloucit, radsi vyloucim
handrkov <- subset(handrkov, id != "22_Suc_R2")


# 2.0 priprava tabulek----
#----------------------------------------------------------#


tabulka_depozice <- handrkov[handrkov$typ == "deposition",]
tabulka_depozice_2024 <- tabulka_depozice[tabulka_depozice$rok == "2024",]

tabulka_depozice_tenax <- tabulka_depozice[tabulka_depozice$opylovac == "eri.ten.",]
tabulka_depozice_bom <- tabulka_depozice[tabulka_depozice$opylovac == "bom.lap.",]
tabulka_depozice_tenax_bom <- rbind(tabulka_depozice_bom, tabulka_depozice_tenax)
tabulka_depozice_heltri <- tabulka_depozice[tabulka_depozice$opylovac == "hel.tri.",]
tabulka_depozice_eriinte <- tabulka_depozice[tabulka_depozice$opylovac == "eri.inte.",]
tabulka_depozice_pocetne <- rbind(tabulka_depozice_heltri,
                                  tabulka_depozice_tenax,
                                  tabulka_depozice_eriinte)
tabulka_depozice_pocetne <-  tabulka_depozice_pocetne[!is.na(tabulka_depozice_pocetne$id),]

tabulka_depozice_pocetne_pomer <- tabulka_depozice_pocetne

tabulka_depozice_pocetne_pomer$mnozstvi_pylu_heterospecificky<- tabulka_depozice_pocetne_pomer$mnozstvi_pylu_heterospecificky + 1

tabulka_depozice_pocetne_pomer$mnozstvi_pylu_konspecificky <-
  tabulka_depozice_pocetne_pomer$mnozstvi_pylu_konspecificky + 1

tabulka_depozice_pocetne_pomer$pomer_pylu <- tabulka_depozice_pocetne_pomer$mnozstvi_pylu_konspecificky/tabulka_depozice_pocetne_pomer$mnozstvi_pylu_heterospecificky

tabulka_depozice_pocetne_pomer <- tabulka_depozice_pocetne_pomer[!is.na(tabulka_depozice_pocetne_pomer$pomer_pylu),]

tabulka_depozice_pocetne_presence_absence <- tabulka_depozice_pocetne
tabulka_depozice_pocetne_presence_absence$mnozstvi_pylu_konspecificky <- ifelse( tabulka_depozice_pocetne_presence_absence$mnozstvi_pylu_konspecificky > 0, 1, tabulka_depozice_pocetne_presence_absence$mnozstvi_pylu_konspecificky)

tabulka_depozice_pocetne$vetsi_nez_nula <- ifelse(tabulka_depozice_pocetne$mnozstvi_pylu_konspecificky > 0, 1, 0)


tabulka_kon_depozice_nenula <- subset(tabulka_depozice, mnozstvi_pylu_konspecificky !=0)


tabulka_depozice_prines_neprines <- tabulka_depozice
tabulka_depozice_prines_neprines$mnozstvi_pylu_konspecificky[tabulka_depozice_prines_neprines$mnozstvi_pylu_konspecificky > 0] <- 1
tabulka_depozice_prines_neprines <- tabulka_depozice_prines_neprines[!is.na( tabulka_depozice_prines_neprines$mnozstvi_pylu_konspecificky),]

tabulka_depozice_model <- tabulka_depozice
tabulka_depozice_pro_model <- tabulka_depozice_model[!is.na(tabulka_depozice_model$pohlavi),]
tabulka_depozice <- tabulka_depozice_pro_model

tabulka_depozice_model_tenax <- tabulka_depozice_model[tabulka_depozice_model$opylovac == "eri.ten."]
chovani_pyl <- handrkov[handrkov$chovani == "pyl",]
chovani_nektar <- handrkov[handrkov$chovani == "nektar",]
tabulka_chovani <- rbind(chovani_nektar, chovani_pyl)
tabulka_chovani_depozice <- tabulka_chovani[tabulka_chovani$typ == "deposition",]

tabulka_depozice_presence_absence <- tabulka_depozice
tabulka_depozice_presence_absence$mnozstvi_pylu_konspecificky <- ifelse( tabulka_depozice_presence_absence$mnozstvi_pylu_konspecificky > 0, 1, tabulka_depozice_presence_absence$mnozstvi_pylu_konspecificky)
tabulka_depozice_presence_absence <- tabulka_depozice_presence_absence[!is.na(tabulka_depozice_presence_absence$pohlavi), ]

# 3.0 cesta k pouzžití správného modelu ----
#----------------------------------------------------------#

## 3.1 cesta k hurdle ----
#----------------------------------------------------------#

#Jasne, tohle nikdy nebude normální rozdělení -> glm poisson, nbrm
hist(tabulka_depozice$mnozstvi_pylu_konspecificky, breaks = 50, xlim = c(0,60))


#poisson - moc velká overdisperze
model_depozice_pocetne_druh_glm <- glm(mnozstvi_pylu_konspecificky ~ opylovac,
                                       data = tabulka_depozice_pocetne,
                                       family = "poisson")
summary(model_depozice_pocetne_druh_glm)


#quasipoisson se nehodí
model_depozice_pocetne_druh_glm_q <- glm(mnozstvi_pylu_konspecificky ~ opylovac,
                                         data = tabulka_depozice_pocetne,
                                         family = "quasipoisson")
summary(model_depozice_pocetne_druh_glm_q)


#glm.nb, tady vidím ze disperze v glm_poisson delala hodně, signifikatni je ted jen hel tri
model_depozice_pocetne_druh_nbrm <- MASS::glm.nb(mnozstvi_pylu_konspecificky ~ 
                                                   opylovac, data = tabulka_depozice_pocetne)
summary(model_depozice_pocetne_druh_nbrm)


logLik(model_depozice_pocetne_druh_nbrm) #opet vychazi lepe, kvuli overdispersion problemu glm
logLik(model_depozice_pocetne_druh_glm)

#pocet nul vypada v pohode, ze k pouzivani nbrm máme nuly v pohodě model
performance::check_zeroinflation(model_depozice_pocetne_druh_nbrm)



## 3.2 hurdle vychází nejlépe ----
#----------------------------------------------------------#
#zeroinflated model by mi presne řekl na co se ptám, kdo spíš nepřinese nulu a potom i numericky kdo spíš přinese více/méně
model_kon_dep_zip <- pscl::zeroinfl(mnozstvi_pylu_konspecificky ~ opylovac + cashHM|opylovac + cashHM, data = tabulka_depozice_pocetne)

summary(model_kon_dep_zip)

model_kon_dep_hurdle <- pscl::hurdle(mnozstvi_pylu_konspecificky ~ opylovac + cashHM|opylovac + cashHM, data = tabulka_depozice_pocetne)

summary(model_kon_dep_hurdle)

pscl::vuong(model_kon_dep_zip,model_kon_dep_hurdle)



## 3.3 jaký argument výpočtu použít v hurdle n/q/g ----
#----------------------------------------------------------#
#pouziti hurdle
#takze -> cas, opylovac, chovani, rok, pohlavi, delka_navstevy!


model_kon_dep_hurdle_pocetne_q <- pscl::hurdle(mnozstvi_pylu_konspecificky ~ opylovac + cashHM + delka_navstevy + chovani + pohlavi|opylovac + cashHM + delka_navstevy + chovani + pohlavi, data = tabulka_depozice_pocetne)


topmodels::rootogram(model_kon_dep_hurdle_pocetne_q)


model_kon_dep_hurdle_pocetne_n <- pscl::hurdle(mnozstvi_pylu_konspecificky ~ opylovac + cashHM + delka_navstevy + chovani + pohlavi|opylovac + cashHM + delka_navstevy + chovani + pohlavi, data = tabulka_depozice_pocetne, dist = "negbin")

topmodels::rootogram(model_kon_dep_hurdle_pocetne_n)


model_kon_dep_hurdle_pocetne_g <- pscl::hurdle(mnozstvi_pylu_konspecificky ~ opylovac + cashHM + delka_navstevy + chovani + pohlavi|opylovac + cashHM + delka_navstevy + chovani + pohlavi, data = tabulka_depozice_pocetne, dist = "geometric")

topmodels::rootogram(model_kon_dep_hurdle_pocetne_g)


summary(model_kon_dep_hurdle_pocetne_q)
summary(model_kon_dep_hurdle_pocetne_n)
summary(model_kon_dep_hurdle_pocetne_g)

AIC(model_kon_dep_hurdle_pocetne_q, model_kon_dep_hurdle_pocetne_n, model_kon_dep_hurdle_pocetne_g) # -> 


### 3.3.1 jaké proměnné použít ----
#----------------------------------------------------------#
#pouziti hurdle
#takze -> cas, opylovac, chovani, rok, pohlavi, delka_navstevy!




## 3.4 Cas, rok, chovani, pohlavi, delku navstev chci pres vsechny opylovace ----
#----------------------------------------------------------#

model_celkovy_depozice_n <- pscl::hurdle(mnozstvi_pylu_konspecificky ~ cashHM + pohlavi + chovani + rok, data = tabulka_depozice, dist = "negbin") #cas + opylovac + chovani + rok
summary(model_celkovy_depozice_n)


#binomial: samice méně častěji deponují vůbec něco, při pojídání pylu se deponuje méně pravděpodobně než při pojídání nektaru, v roce 2023 byly vyšší depozice než v roce 2022


model_celkovy_depozice_delka_navstev <- pscl::hurdle(mnozstvi_pylu_konspecificky ~ cashHM + pohlavi + chovani + rok + delka_navstevy, data = tabulka_depozice, dist = "negbin") #cas + opylovac + chovani + rok
summary(model_celkovy_depozice_delka_navstev)

#s delkou navstevy takhle dáva smysl to tam zahrnout, už jen když člověk vidí co to udělalo s tím rokem 2023
table(tabulka_depozice$delka_navstevy)


## 3.4 opylovace mezi sebou chci porovnavat jenom u pocetnych + z predchozího vím, že hlavně mají vliv pohlaví, chování a délka návštěvy a celý je to dohromady ucelený jakožto vlastnosti opylovače. ----
#----------------------------------------------------------#


model_celkovy_depozice_opylovac_vlastnosti <- pscl::hurdle(mnozstvi_pylu_konspecificky ~ opylovac + cashHM + pohlavi + delka_navstevy + chovani, data = tabulka_depozice_pocetne, dist = "negbin") 
summary(model_celkovy_depozice_opylovac_vlastnosti)

#no když tomu užerou variabilitu ostatní tak už prd
table(tabulka_depozice_pocetne$pohlavi[tabulka_depozice_pocetne$opylovac == "hel.tri."])
#aha no tak jestli mě zajímá hel.tri. tak brát pohlaví je asi oof
table(tabulka_depozice_pocetne$opylovac)
#dobry nejsou vymazany protože mají u pohlavi NA
table(tabulka_depozice_pocetne$opylovac[tabulka_depozice_pocetne$rok == "2024"])
#no všichni hel.tri byly skoro v roce 2024, rok tam taky nemá smysl dávat


model_celkovy_depozice_opylovac_vlastnosti <- pscl::hurdle(mnozstvi_pylu_konspecificky ~ opylovac + cashHM + delka_navstevy + chovani, data = tabulka_depozice_pocetne, dist = "negbin") 
summary(model_celkovy_depozice_opylovac_vlastnosti)

#tohle dává hlavu a patu

# 4.0 grafy ----
#----------------------------------------------------------#


## 4.1 grafy - příprava: pocení s gemini, jelikož package neustále brali kvantitativní koeficienty i když jim bylo specifikováno že mají brát zero část... ----
#----------------------------------------------------------#

#s vystupy tohoto modelu
model_hurdle <- pscl::hurdle(mnozstvi_pylu_konspecificky ~ cashHM + pohlavi + chovani + rok + delka_navstevy, data = tabulka_depozice, dist = "negbin") #cas + opylovac + chovani + rok
summary(model_celkovy_depozice_delka_navstev)


tabulka_depozice$pohlavi <- as.factor(tabulka_depozice$pohlavi)
tabulka_depozice$chovani <- as.factor(tabulka_depozice$chovani)
tabulka_depozice$rok <- as.factor(tabulka_depozice$rok)

# --- KROK 1: Příprava "průměrných" dat (beze změny) ---
model_data <- model_hurdle$model 
avg_cashHM <- mean(model_data$cashHM) 
ref_chovani <- levels(model_data$chovani)[1]
ref_rok <- levels(model_data$rok)[1]
avg_delka <- mean(model_data$delka_navstevy)

newdata_grid <- data.frame(
  pohlavi = c("samec", "samice"),
  cashHM = avg_cashHM,
  delka_navstevy = avg_delka,
  chovani = factor(ref_chovani, levels = levels(model_data$chovani)),
  rok = factor(ref_rok, levels = levels(model_data$rok))
)

# --- KROK 2: Predikce log-odds (beze změny) ---
pred_link <- predict(model_hurdle, 
                     newdata = newdata_grid, 
                     type = "zero")

# --- KROK 3: Transformace na pravděpodobnost (beze změny) ---
final_data <- cbind(newdata_grid, fit = pred_link)
final_data$prob <- plogis(final_data$fit) # plogis() = inverzní logit

# --- KROK 4: Ruční výpočet Intervalů Spolehlivosti (NOVÉ) ---

# 4a. Získáme matici modelu pro naše 'newdata_grid'
# Musíme říct R, jak má zacházet s faktory
mm <- model.matrix(delete.response(terms(model_hurdle, model = "zero")), 
                   newdata_grid, 
                   xlev = model_hurdle$xlevels$zero)

# 4b. Získáme varianční-kovarianční matici POUZE pro nulový model
vcov_zero <- vcov(model_hurdle, model = "zero")

# 4c. Vypočítáme standardní chyby na "link" škále (log-odds)
# Var(Xb) = X * Var(b) * X'
final_data$se_link <- sqrt(diag(mm %*% vcov_zero %*% t(mm)))

# 4d. Vypočítáme CI na "link" škále
final_data$lower_link <- final_data$fit - 1.96 * final_data$se_link
final_data$upper_link <- final_data$fit + 1.96 * final_data$se_link

# 4e. Převedeme CI na finální škálu pravděpodobnosti (0-1)
final_data$lower_prob <- plogis(final_data$lower_link)
final_data$upper_prob <- plogis(final_data$upper_link)


# --- KROK 5: Finální kontrola dat ---
# Teď byste měli vidět sloupce 'lower_prob' a 'upper_prob'
print(final_data)

# --- KROK 6: Vykreslení (konečně s 'geom_pointrange') ---
library(ggplot2)

Pohlavi <- ggplot(final_data, aes(x = pohlavi, y = prob, ymin = lower_prob, ymax = upper_prob)) +
  geom_pointrange(size = 0.8) + # Vrátili jsme 'geom_pointrange'
  labs(title = "Vliv pohlaví na P(pyl > 0)",
       x = "Pohlaví",
       y = "Pravděpodobnost přítomnosti pylu") +
  theme_minimal() +
  
  # Přidání hvězdiček
  annotate("text", 
           x = "samice", 
           y = final_data$upper_prob[final_data$pohlavi == "samice"] + 0.02,
           label = "**", 
           size = 8) +
  
  # Zajistíme, aby osa Y začínala na 0
  expand_limits(y = 0)

# --- KROK 1: Příprava "průměrných" dat pro CHOVANI ---
model_data <- model_hurdle$model 
avg_cashHM <- mean(model_data$cashHM) 
ref_pohlavi <- levels(model_data$pohlavi)[1] # Držíme pohlaví na referenční úrovni
ref_rok <- levels(model_data$rok)[1]     # Držíme rok na referenční úrovni
all_chovani <- levels(model_data$chovani) # Chceme predikci pro všechny úrovně chování
avg_delka <- mean(model_data$delka_navstevy)


newdata_grid_chovani <- expand.grid(
  chovani = all_chovani,
  cashHM = avg_cashHM,
  delka_navstevy = avg_delka,
  pohlavi = factor(ref_pohlavi, levels = levels(model_data$pohlavi)),
  rok = factor(ref_rok, levels = levels(model_data$rok))
)

# --- KROK 2: Predikce log-odds ---
pred_link_chovani <- predict(model_hurdle, 
                             newdata = newdata_grid_chovani, 
                             type = "zero")

# --- KROK 3: Transformace na pravděpodobnost ---
final_data_chovani <- cbind(newdata_grid_chovani, fit = pred_link_chovani)
final_data_chovani$prob <- plogis(final_data_chovani$fit) 

# --- KROK 4: Ruční výpočet Intervalů Spolehlivosti ---
mm_chovani <- model.matrix(delete.response(terms(model_hurdle, model = "zero")), 
                           newdata_grid_chovani, 
                           xlev = model_hurdle$xlevels$zero)
vcov_zero <- vcov(model_hurdle, model = "zero")

final_data_chovani$se_link <- sqrt(diag(mm_chovani %*% vcov_zero %*% t(mm_chovani)))
final_data_chovani$lower_link <- final_data_chovani$fit - 1.96 * final_data_chovani$se_link
final_data_chovani$upper_link <- final_data_chovani$fit + 1.96 * final_data_chovani$se_link

final_data_chovani$lower_prob <- plogis(final_data_chovani$lower_link)
final_data_chovani$upper_prob <- plogis(final_data_chovani$upper_link)

# --- KROK 5: Finální kontrola dat ---
print(final_data_chovani)

# --- KROK 6: Vykreslení grafu pro CHOVANI ---
Chovani <- ggplot(final_data_chovani, aes(x = chovani, y = prob, ymin = lower_prob, ymax = upper_prob)) +
  geom_pointrange(size = 0.8) + 
  labs(title = "Vliv chování na P(pyl > 0)",
       x = "Chování",
       y = "Pravděpodobnost přítomnosti pylu") +
  theme_minimal() +
  
  # Přidání hvězdičky (zde jedna *)
  annotate("text", 
           x = "pyl", # Kategorie, která byla signifikantní
           y = final_data_chovani$upper_prob[final_data_chovani$chovani == "pyl"] + 0.02,
           label = "*", # Jedna hvězdička
           size = 8) +
  expand_limits(y = 0)


## 4.2 grafy výstup pro binomickou a kvantitativní část pohlavi, rok, chovani ----
#----------------------------------------------------------#

#vystup pro binomickou část
Pohlavi + Chovani


#ale s pouzitim negbin distribuje neni absolutně signifikatní
efekt_cashHM_count <- ggpredict(model_hurdle, terms = "cashHM [all]", component = "conditional")

#vystup pro kvantitativní část
ggplot(efekt_cashHM_count, aes(x = x/60, y = predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(title = "Vliv cashHM na množství pylu (pokud > 0)",
       x = "čas",
       y = "Očekávané množství pylu") +
  annotate("text", 
           label = "p < 0.003",
           x = Inf,             
           y = Inf,             
           hjust = 1.2,         
           vjust = 1.5,         
           size = 4,          
           color = "grey20")


## 4.3 grafy výstup pro kvantitativní část - druhy opylovacu ----
#----------------------------------------------------------#
model_celkovy_depozice_opylovac_vlastnosti
summary(model_celkovy_depozice_opylovac_vlastnosti)

names(coef(model_celkovy_depozice_opylovac_vlastnosti))

coef_data <- data.frame(
  opylovac = c("eri.inte", "eri.ten.", "hel.tri."),
  estimate = c(-0.4885916, 1.2589329, 1.9961980),
  std_error = c(2.1948492, 0.8982616, 0.9461695)
)

coef_data$lower_ci <- coef_data$estimate - 1.96 * coef_data$std_error
coef_data$upper_ci <- coef_data$estimate + 1.96 * coef_data$std_error

coef_data$rr <- exp(coef_data$estimate)
coef_data$rr_lower <- exp(coef_data$lower_ci)
coef_data$rr_upper <- exp(coef_data$upper_ci)

ggplot(coef_data, aes(x = rr, y = opylovac)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbarh(aes(xmin = rr_lower, xmax = rr_upper), height = 0.2, color = "blue") +
  scale_x_log10(breaks = c(0.1, 0.5, 1, 2, 5, 10)) +
  labs(
    title = "Porovnání opylovačů (Kvantitativní část modelu)",
    subtitle = "Poměr očekávaného počtu (Rate Ratio)",
    x = "Rate Ratio (Poměr) - 95% CI (Logaritmická škála)",
    y = "Typ opylovače"
  ) +
  theme_bw() +
  annotate("text", 
           label = "*",
           x = coef_data$rr_upper[coef_data$opylovac == "hel.tri."] + 1,             
           y = 3.3,             
           hjust = 1.2,         
           vjust = 1.5,         
           size = 7,          
           color = "grey20"
    
  )


## 5. zkouska toho samého s glm a quasipoisson ----
#----------------------------------------------------------#

model_depozice_pocetne_druh_glm_q <- glm(mnozstvi_pylu_konspecificky ~ pohlavi + cashHM + chovani + rok + delka_navstevy,
                                         data = tabulka_depozice,
                                         family = "quasipoisson")
summary(model_depozice_pocetne_druh_glm_q)




model_depozice_pocetne_druh_glm_q <- glm(mnozstvi_pylu_konspecificky ~ opylovac + cashHM +chovani + delka_navstevy,
                                         data = tabulka_depozice_pocetne,
                                         family = "quasipoisson")
summary(model_depozice_pocetne_druh_glm_q)
        