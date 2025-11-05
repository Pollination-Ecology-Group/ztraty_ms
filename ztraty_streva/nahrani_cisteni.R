# Nahrání dat a jejich čištění 

library(readxl)

cisteni_streva<-read_excel("ztraty_zakl_tab.xlsx")
cisteni_casti<-read_excel("ztraty_pocet_pylu.xlsx")

data2024<-read_excel("streva_2024.xlsx")

depozice<-read_excel("Handrkov_raw.xlsx",    na = c("", NA), 
                     col_types = c("text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "numeric", "skip", "numeric", "date", "text", "numeric"),
                     col_names = c("id", "druh_kytky", "typ", "klec_pytlik", "mnozstvi_roztoku", "mnozstvi_pylu_konspecificky", "mnozstvi_pylu_celkem", "mnozstvi_pylu_heterospecificky", "vyska_rostliny", "barva_pytliku", "opylovac", "chovani", "delka_navstevy", "zacatek_konec_cas", "pocet_navstev_hodinu", "cas_navstevy", "pohlavi", "rok"),
                     skip = 1)
celk_depozice <- depozice[depozice$typ == "deposition_celkem",]

#visitační data

#produkce pylu čertkusu



# Data cisteni a casti 2022/2023 ####

summary(cisteni_streva)
head(cisteni_streva)

summary(cisteni_casti)
head(cisteni_casti)

cisteni_streva$cisteni<-as.factor(cisteni_streva$cisteni)
cisteni_casti$cisteni<-cisteni_streva$cisteni[match(cisteni_casti$newID,cisteni_streva$newID)]
cisteni_casti$pohlavi<-cisteni_streva$pohlavi[match(cisteni_casti$newID,cisteni_streva$newID)]

cisteni_casti$zrnzvire<-cisteni_streva$zrncelk[match(cisteni_casti$newID,cisteni_streva$newID)]

with(cisteni_streva,table(pohlavi,cisteni))

cisteni_streva$pohlcist<-as.factor(paste(cisteni_streva$pohlavi,cisteni_streva$cisteni))
cisteni_casti$pohlcist<-as.factor(paste(cisteni_casti$pohlavi,cisteni_casti$cisteni))
cisteni_casti$castcist<-as.factor(paste(cisteni_casti$cast_tela,cisteni_casti$cisteni))

# Data pollen load a streva 2024 ####

summary(data2024)
head(data2024)

data2024$suc_stery<-as.numeric(data2024$suc_stery)
summary(data2024$suc_stery)

data2024$ost_stery<-as.numeric(data2024$ost_stery)
summary(data2024$ost_stery)

data2024$suc_streva<-as.numeric(data2024$suc_streva)
summary(data2024$suc_streva)

data2024$ost_streva<-as.numeric(data2024$ost_streva)
summary(data2024$ost_streva)

# Data depozice ####

summary(celk_depozice)

# Data visitace ####


# Data produkce ####



