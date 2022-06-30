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
  mutate.( es_investment = ifelse.( stri_detect_fixed(nombre_delegacion, "Investment"), 1, 0)) %>%
  as.data.table()


#-------- GET DELEGATION and THEIR DISTRIC_CODE
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

#-- check
# Is there a salesperson assigned to two district_codes ?
# - Yes, a group is assigned to three and some others to two.
duplicases <- ftecp |> 
  select.(nombre_comercial, codigo_postal) |> 
  distinct.() %>%
  mutate.(num_sales = n.(), .by = c(codigo_postal, nombre_comercial)) |> 
  select.(nombre_comercial, num_sales) |> 
  distinct.() |> 
  arrange.(-num_sales) |> 
  as.data.table()
duplicases

#- check number of salespeople by province
ftecp |> 
  select.(nombre_comercial, provincia) |> 
  distinct.() |> 
  count.(provincia) |> 
  arrange.(-n) |> 
  as.data.table()

# provincia     n
# <char> <int>
#   1:      MADRID    36
# 2:      MURCIA    18
# 3:     SEVILLA    18
# 4:       CÁDIZ    16
# 5:     GRANADA    15
# 6:      MÁLAGA    15
# 7:     ALMERÍA    14

# In ALMERÍA 13 investment people.
# num_investment by provincia
ftecp |> select.(provincia, es_investment) |>  mutate.(num_inves = sum(es_investment), .by = provincia) |> select.(provincia, num_inves) |>  distinct.() |> arrange.(-num_inves)

# provincia num_inves
# <chr>         <dbl>
# 1 MÁLAGA           13
# 2 MURCIA           13
# 3 GRANADA          13
# 4 SEVILLA          13
# 5 CÁDIZ            13
# 6 CORDOBA          13
# 7 ALMERÍA          13
# 8 CÁCERES          13
# 9 BADAJOZ          13
# 10 CUENCA           0

#----- COMERCIAL - PROVINCIA - DISTRITO_POSTAL_DELE
fteprovcp <- ftecp |> 
  select.(nombre_comercial, provincia, codigo_postal_dele) |> 
  distinct.() |> 
  as.data.table()

# There are salespeople in different provinces
# 266:     LOPEZ CIRCUJANO,ALEJANDRO     MURCIA              30800
# 267:     LOPEZ CIRCUJANO,ALEJANDRO     MÁLAGA              29007
# 268:     LOPEZ CIRCUJANO,ALEJANDRO    BADAJOZ               6002

#-------- GET PROVINCES - POSTAL CODES AVAILABLE and COORDINATES
