#----------------------------
# Author:     Carlos Ortega
# Date:       2022-06-21
# Purpose:    Assign Comercial people to a district and Company from Duns.
# Input:      Duns file 2021 + FTE + Estructura Staffing.
#----------------------------

rm(list = ls())
# tidytable::inv_gc()
# gc()
# cat("\014")  # ctrl+L

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(tictoc)
  library(stringr)
  library(stringi)
  library(lubridate)
  library(janitor)
  library(fasttime)
  library(ggcharts)
  library(ggeasy)
  library(forcats)
  library(readxl)
  library(broom)
  library(tictoc)
  library(tidytable)
  library(parallel)
  library(magrittr)
  library(corrplot)
})

tini <- Sys.time()
tic()

#----- Read several input needed files.
#----- Staffing Structure.
red_dir <- '/Users/carlosortega/Documents/00_Adecco/Para_J/01_Input_raw/Estructura/' 
file_struct <- 'Red Staffing 2022.xlsx'
filedirred <- paste(red_dir,file_struct, sep = "")


#--- Put all together all Sales people in each delegation + Postal Code.
nam_dele <- c("LP - Castilla-NW", "LP- Cat-Levante", "LP-Norte", "LP-Centro-Can", "LP-Sur")
dele_dt <- data.table()
for (i in 1:length(nam_dele)) {
  print(nam_dele[i]) 
  dattmp <- read_excel(filedirred, sheet = nam_dele[i]) %>%
    as.data.table() %>%
    select.(NEGOCIO, DT, CECO, DR, DELEGACIÓN, PUESTO, NOMBRE, APELLIDOS, DIRECCION, CP, LOCALIDAD, PROVINCIA) %>%
    as.data.table()
  
  dele_dt <- rbind(dele_dt, dattmp)
  
}

#-- Remove some rows with NA and change names
dele_dt %<>% 
  clean_names() %>%
  filter.(!is.na(cp)) %>%
  filter.(!is.na(localidad)) %>%
  mutate.( localidad = stri_trans_toupper(localidad)) %>%
  mutate.( provincia = stri_trans_toupper(provincia)) %>%
  mutate.( localidad = stri_replace_all_fixed(localidad, "L’", "")) %>% 
  mutate.( localidad = stri_replace_all_fixed(localidad, " HOSPITALET", "HOSPITALET")) %>% 
  as.data.table()
table(dele_dt$localidad)
#-- (END) --- Staffing Structure.


#----- LOCALIDAD AND POSTAL CODE - github
#-- In this file we have all postal codes avaialbles by location (nombre)
postal_dir  <- '/Users/carlosortega/Documents/00_Adecco/Para_J/01_Input_raw/CodigosPostales_Localidades/' 
file_postal <- 'codigos_postales_municipios_join.csv'
filedirpostal <- paste(postal_dir,file_postal, sep = "")
postallocal <- fread(filedirpostal)


#----- LOCALIDAD AND POSTAL CODE - Maestro
#-- Here we have all the "localidad" related to DR. but there many missing CPs. 
maestro_dir  <- '/Users/carlosortega/Documents/00_Adecco/Para_J/01_Input_raw/Relacion_CCAA_Provincia_Municipios/' 
file_maestr  <- 'CCAA_Provincia_Municipios.xlsx'
filedirmaes  <- paste(maestro_dir, file_maestr, sep = "")
maestrolocal <- read_excel(path = filedirmaes) %>% 
  clean_names() %>%
  as.data.table()

# #----- FTEs
# #-- FTEs is not needed, since we have everything in dele_dt
# rm(filedir)
# fte_dir <- '/Users/carlosortega/Documents/00_Adecco/Para_J/01_Input_raw/FTEs/'
# file_fte <- 'FTES comerciales.xlsx'
# filedir <- paste(fte_dir,file_fte, sep = "")
# 
# fte_dat <- read_excel(filedir, skip = 3 ) %>%
#   clean_names() %>%
#   as.data.table()


#----- DUNS
tic()
einforma_dir <- '/Users/carlosortega/Documents/00_Adecco/Para_J/01_Input_raw/eInforma_Duns/' 
duns_2021    <- 'VW_Bisnode_spain__202203311519.csv'

#--- Select just some fields of each  - merge and calculate based on those fields.
dat2021 <- fread(paste(einforma_dir, duns_2021, sep = ""), nThread = 4 )
tic()
dat2021red <- dat2021 %>% 
               clean_names() %>%
               filter.(national_identification_number != "") %>%
               select.(national_identification_number, business_name, city_name, 
                       state_province_name, city_code, postal_code_for_street_address,
                       primary_local_activity_code, 
                       annual_sales_local,  employees_total) %>%
               rename.( annualsales_2021    = annual_sales_local) %>%
               rename.( empleados2021       = employees_total) %>%
               # Companies bigger than 10 employees.
               filter.( empleados2021 > 10) %>%
               rename.( cif                 = national_identification_number) %>%
               rename.( localidad           = city_name) %>%
               rename.( provincia           = state_province_name) %>%
               rename.( codigo_ciudad       = city_code) %>%
               rename.( distrito_postal     = postal_code_for_street_address) %>%
               rename.( cnae                = primary_local_activity_code) %>%
               mutate.( cnae_2dig           = floor(cnae/100)) %>%
               # Companies bigger than 10 employees.
               arrange.(cif) %>%
               mutate.( provincia = stri_replace_all_fixed( 
                                    provincia,
                                    c("ASTURIAS (OVIEDO)", "CANTABRIA (SANTANDER)", "LA RIOJA (LOGRONO)", "PALMAS (LAS)"),
                                    c("ASTURIAS", "CANTABRIA", "LA RIOJA", "LAS PALMAS"),
                                    vectorize_all = FALSE
                                    )
                          ) %>%
               as.data.table()
toc(func.toc = toc.outmsg)
rm(dat2021)
#---------- END OF FILE ----------------
