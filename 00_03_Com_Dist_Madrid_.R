#----------------------------
# Author:     Carlos Ortega
# Date:       2022-06-21
# Purpose:    Assign Comercial people to a district and Company from Duns.
# Input:      Duns file 2021 + FTE + Estructura Staffing.
# Expanation:
# 1. Get file with Sales people by delegation / province. 
#    Filter the right Sales people: Director, Comercial responsible, etc.. (Sara's input)
# 2. Get Duns and filter companies with more than 10  employees
#    Change names of provinces to match the ones from the previous file.
# 3. For each province in the file of sales people, 
#    get the sales people of that province
#    From Duns get also all the companies in that province and left_join the sales people.
#    The sales people are associated repeteadly to  the companies.
# 4. Clean some variables names of the output file and save.
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


#----- FTEs
#-- Use official file with FTEs.
#-- Bad: It does not include delegation district_code but I can get the right people. 
rm(filedir)
fte_dir  <- '/Users/carlosortega/Documents/00_Adecco/Para_J/01_Input_raw/FTEs/'
file_fte <- 'FTES comerciales.xlsx'
filedir  <- paste(fte_dir,file_fte, sep = "")

fte_dat <- read_excel(filedir, skip = 3 ) %>%
  clean_names() %>%
  # Remove cases where nombre_delegacion is "ONSITE".
  filter.( !(nombre_delegacion %like% "ONSITE")) |> 
  # Remove cases where sales people is not active...
  filter.( situacion_actual == "ACTIVO") |> 
  filter.( is.na(excedencia) ) |> 
  mutate.( es_investment = ifelse.( stri_detect_fixed(nombre_delegacion, "Investment"), 1, 0)) %>%
  as.data.table()


#-------- GET DELEGATION and THEIR DISTRICT_CODE
#----- Staffing Structure.
red_dir     <- '/Users/carlosortega/Documents/00_Adecco/Para_J/01_Input_raw/Estructura/' 
file_struct <- 'Red Staffing 2022.xlsx'
filedirred  <- paste(red_dir,file_struct, sep = "")


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

#-- Get just CECO and District_Code and PROVINCIA
dele_red <- dele_dt %>%
  select.(CECO, CP, PROVINCIA) %>%
  filter.(!is.na(CECO)) %>%
  filter.(!is.na(PROVINCIA)) |> 
  #-- Add a starting cero when the CECO has just 4 digits
  mutate.(CECO = ifelse.(nchar(CECO) == 4, paste0("0", CECO) , CECO)) |> 
  distinct.() %>%
  as.data.table()



#---------- MERGE FTE + DISTRICT CODE
ftecp <- merge(
  fte_dat, dele_red,
  by.x = c('ceco_efectivo'), by.y = c('CECO'),
  sort = FALSE
) %>%
  rename.(codigo_postal_dele = CP) %>%
  rename.(provincia = PROVINCIA) %>%
  mutate.(provincia = stri_trans_toupper(provincia)) |> 
  as.data.table()


#----- DT - NOMBRE_DELEGACION - COMERCIAL - PROVINCIA - DISTRITO_POSTAL_DELE
fteprovcp <- ftecp |> 
  select.(dop,nombre_delegacion, nombre_comercial, provincia, codigo_postal_dele, puesto, es_investment) |> 
  distinct.() |> 
  # Modify provinces to match coordinates files
  mutate.(provincia = stri_replace_all_fixed(
    provincia,
    c("Á", "É", "Í", "Ó", "Ú", " ", "Ñ", "BIZKAIA", "GIPUZKOA", "ORENSE"),
    c("A", "E", "I", "O", "U", "_", "N", "VIZCAYA", "GUIPUZCOA" , "OURENSE"),
    vectorize_all = FALSE
  )
  ) |> 
  mutate.(provincia = stri_replace_all_fixed(provincia, c("PALMA_DE_MALLORCA"), c("BALEARES"))) |> 
  mutate.(nombre_delegacion = stri_trans_toupper(nombre_delegacion)) %>%
  arrange.(provincia, puesto) |> 
  as.data.table()



#----- CHANGE DISTRICT_CODE FOR CASES WHERE ALL DELEGATIONS ARE IN THE SAME DISTRICT_CODE
#----- LIKE MADRID - BARCELONA ... (¿Sevilla?)
#----- Staffing Structure.
red_dir       <- '/Users/carlosortega/Documents/00_Adecco/Para_J/01_Input_raw/Estructura/' 
file_terri    <- 'CP Territorios.xlsx'
filedirterri  <- paste(red_dir,file_terri, sep = "")
dat_terri     <- read_excel(filedirterri, sheet = "Copia_Maestro_CP" ) |> 
  clean_names() |> 
  as.data.table()
#-- For each delegation calculate which is the "mode" of the "cp". 
datterri_ext <- dat_terri |> 
  select.(provincia, delegacion, cp) |> 
  filter.(!is.na(provincia)) |> 
  filter.(!is.na(delegacion)) |> 
  mutate.(num_cp = n.(), .by = c(delegacion, cp)) |> 
  arrange.(delegacion, -num_cp) |> 
  slice.(n = 1, .by = delegacion) |> 
  as.data.table()
# Sevilla is one of the cases with many delegations in the same address.
# But we do not have all the details about "Sevilla Sur" delegation,
# so I cannot infer which is the right district_code for it.                

#----- FOR MADRID ----------
#-- Deles from the two sources.
tic()
delesfte <- ftecp |> 
  filter.(provincia == "MADRID") |> 
  select.(nombre_delegacion, codigo_postal_dele) |> 
  filter.(!(nombre_delegacion %like% "CALL CENTER")) |> 
  filter.(!(nombre_delegacion %like% "Urban Mobility")) |> 
  filter.(!(nombre_delegacion %like% "AUDIOVISUAL")) |> 
  filter.(!(nombre_delegacion %like% "AEROPUERTOS")) |> 
  filter.(!(nombre_delegacion %like% "HOSTELERIA")) |> 
  distinct.() |> 
  mutate.(nombre_delegacion = stri_trans_toupper(nombre_delegacion)) |> 
  as.data.table() 

delesterri <- datterri_ext |>  
  filter.(provincia == "Madrid") |> 
  arrange.(-num_cp) |>  
  mutate.(delegacion = stri_trans_toupper(delegacion)) |> 
  mutate.(cp = as.character(cp)) |> 
  as.data.table()

delesgod <- merge(
  delesfte, delesterri,
  by.x  = c('nombre_delegacion'), by.y = c('delegacion'),
  all.x = TRUE,
  sort  = FALSE
) |> 
  arrange.(nombre_delegacion) |> 
  mutate.( cp_new = ifelse.(is.na(cp), codigo_postal_dele, cp) ) |> 
  mutate.( provincia = ifelse(is.na(provincia), "Madrid", provincia)) |> 
  #--- Get just the center of Madrid
  filter.(nombre_delegacion != "ARANJUEZ") %>%
  filter.(nombre_delegacion != "ARGANDA") %>%
  filter.(!(nombre_delegacion %like% "SEBASTIAN")) %>%
  filter.(!(nombre_delegacion %like% "ALCALA")) %>%
  filter.(!(nombre_delegacion %like% "ALCORCON")) %>%
  filter.(!(nombre_delegacion %like% "FUENLABRADA")) %>%
  filter.(!(nombre_delegacion %like% "GETAFE")) %>%
  filter.(!(nombre_delegacion %like% "TORREJON")) %>%
  filter.(!(nombre_delegacion %like% "TRES CANTOS")) %>%
  filter.(!(nombre_delegacion %like% "ROZ")) %>%
  filter.(!(nombre_delegacion %like% "MAJ")) %>%
  filter.(!(nombre_delegacion %like% "ALC")) %>%
  as.data.table()  

toc(func.toc = toc.outmsg)


#-------- GET PROVINCES - POSTAL CODES AVAILABLE and COORDINATES
gis_dir  <- '/Users/carlosortega/Documents/00_Adecco/Para_J/01_Input_raw/Povincias_distritos_Gis/'
gis_file <- 'Provincia_Cod_Postal_Centroide.csv'
gistmp   <- fread(paste0(gis_dir,gis_file)) |> 
  # There are repeated cod_postal in some cases.. Get the first one
  slice.(1, .by = cod_postal) |> 
  as.data.table()


#-------- GET EINFORMA COMPANIES BY DISTRICT_CODE 
#----- DUNS
tic()
einforma_dir <- '/Users/carlosortega/Documents/00_Adecco/Para_J/01_Input_raw/eInforma_Duns/' 
duns_2021    <- 'VW_Bisnode_spain__202203311519.csv'

tic()
dat2021 <- fread(
  paste(einforma_dir, duns_2021, sep = ""),
  select = c('National_Identification_Number','Employees_Total',
             'Postal_Code_for_Street_Address'),
  nThread = 4 )
toc(func.toc = toc.outmsg)
# 4.02 sec elapsed

tic()
dat2021red <- dat2021 %>% 
  clean_names() %>%
  filter.(national_identification_number != "") %>%
  # filter.( employees_total > 20 & employees_total < 100) %>%
  filter.( employees_total > 20 ) %>%
  select.( postal_code_for_street_address, employees_total ) |> 
  rename.( distrito_postal     = postal_code_for_street_address) %>%
  # Companies bigger than 10 employees.
  mutate.( num_companies = n.(), .by = distrito_postal) |>  
  #-- Get just the needed variables - distrito_postal, num_companies
  select.( distrito_postal, num_companies) |>  
  distinct.() |> 
  filter.(!is.na(distrito_postal)) |> 
  arrange.(-num_companies) |> 
  as.data.table()
toc(func.toc = toc.outmsg)
# 0.579 sec elapsed
# rm(dat2021)

#---------- MERGING GIS + NUM_COMPANIES -------
gisdat <- merge(
  gistmp, dat2021red,
  by.x  = c("cod_postal"), by.y = c("distrito_postal"),
  all.x = TRUE,
  sort  = FALSE
) |> 
  # Some cod_postal has no companies, change NA to 0.
  mutate.(num_companies = ifelse.(is.na(num_companies), 0, num_companies)) |> 
  arrange.(-num_companies) |>  
  as.data.table()

#-------------------------------------------
#------------ MADRID ---------------
#-------------------------------------------
#--- Coordinates
gismad <- gisdat |> 
  filter.(provincia == "MADRID") |> 
  #-- Remove districts with no companies
  filter.(num_companies > 1) |> 
  mutate.(cod_postal = as.character(cod_postal)) |> 
  as.data.table()

#--- Deles
delesmad <- delesgod |> 
  select.(nombre_delegacion, cp_new) |> 
  mutate.(name_dele = stri_replace_all_fixed(nombre_delegacion, " ","_")) |> 
  as.data.table()

#--- Gis with Deles and number of Companies.
madgis <- merge(
  gismad, delesmad,
  by.x = c('cod_postal'), by.y = c('cp_new'),
  all.x = TRUE,
  sort = FALSE
) |> 
  mutate.(name_dele = ifelse.(is.na(name_dele), "no", name_dele)) |> 
  as.data.table()

# #---- Let's chart to see deles and the rest of postal codes.
# library(rayshader)
# mad_gr <-  ggplot(madgis, aes(x = centroide_longitud, y = centroide_latitud, 
#              label = nombre_delegacion)) +
#   geom_point(aes(color = nombre_delegacion, size = num_companies)) +
#   # geom_label_repel(size = 2) +
#   theme_minimal() +
#   easy_legend_at( to = c('none'))
# print(mad_gr)
# plot_gg(mad_gr, multicore = TRUE, width = 5, height = 5, scale = 250)


#-------------------------------------------------------
#----- CONCENTRATE IN "DT CENTRO - MADRID CENTRO" ------
fteprovcptmp <- fteprovcp |> 
  filter.(dop == "DT CENTRO CANARIAS") |> 
  as.data.table()

#-- Case MADRID AND SELECT JUST THE COMMERCIALS FROM "delesgod".
fteprovcase <- fteprovcptmp |>  
  filter.(provincia == "MADRID") |> 
  filter.(nombre_delegacion %chin% (delesgod  %>% select.(nombre_delegacion) %>% pull.(nombre_delegacion) )) %>%
  as.data.table()

gisdatcase <- madgis |> 
  filter.(provincia == "MADRID") |> 
  #-- Remove cod_postal without companies
  filter.(num_companies != 0) |>  
  as.data.table()

#-- To assign salespeople to cod_postal 
#-- Salespeople are ordered from high level to low level.
#-- The idea is to asign sales people in their order (first round).
#-- For the second round in reverse.
#-- Third round as the first..
#-- and so on..
#-- In this way we assure that in each assignment each sales people get 
#-- less and less companies.
#-- The dataframe with the companies (gisdat) is sorted from high number 
#-- of companies to low number of companies.
#-- Let's automate that.
salespeople      <- fteprovcase |> 
  select.(nombre_comercial) |> 
  # just in case a salespeople is repeated.
  distinct.() %>%
  pull.(nombre_comercial)
salespeople_rev  <- rev(salespeople)
salespeople_both <- c(salespeople, salespeople_rev)

#-- Assignment
salespeople_rep <- rep(salespeople_both, length.out = nrow(gisdatcase))
gisdatcase_ass <- gisdatcase |> 
  mutate.( sales_assigned = salespeople_rep) |> 
  as.data.table()


#--- Save intermediate file - Sales people and province and postal code delegation.
fwrite(
  gisdatcase_ass, 
  file = "./output/MADRID_CENTRO_SalesPeople_Province_District_Position.csv",
  encoding = "UTF-8",
  sep = "|"
)


#--- checks
#-- Companies by salespeople
gisdatend <- copy(gisdatcase_ass)
gisdatend |> 
  mutate.(tot_compa = sum(num_companies), .by = sales_assigned) |> 
  select.(sales_assigned, tot_compa) |> 
  distinct.() |> 
  arrange.(-tot_compa) |> 
  as.data.table()

#-- Salespeople by district
gisdatend |> 
  mutate.(tot_cp = n.(), .by = cod_postal) |> 
  select.(cod_postal, tot_cp) |> 
  filter.(tot_cp > 1) |> 
  arrange.(-tot_cp) |> 
  as.data.table()

tend <- Sys.time(); tend - tini
# Time difference of 14.42478 secs
#----------------- END OF FILE -----------