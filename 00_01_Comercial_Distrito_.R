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
  mutate.( is_investment = ifelse.( stri_detect_fixed(nombre_delegacion, "Investment"), 1, 0)) %>%
  as.data.table()


