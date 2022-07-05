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
  library(sf)
  library(leaflet)
  library(tmap)
  library(maptools)
})

tini <- Sys.time()
tic()

dir_zip <- '/Users/carlosortega/Documents/00_Adecco/Para_J/01_Input_raw/Povincias_distritos_Gis/ds-codigos-postales-master/archive/'
dir_list  <- list.dirs(path = dir_zip, full.names = FALSE) 
dir_list  <- dir_list[nchar(dir_list) > 0]
              

provlonlat <- data.table()
for (i in 1:length(dir_list)) {
  
  prov_val <- word(dir_list[i], 2, sep = fixed("-")) 
  file_tmp <- paste(dir_zip, dir_list[i],"/", prov_val, ".shp", sep = "")
  print(prov_val)
  
  codigos_postales_provincia_shp <- sf::st_read(file_tmp) 
 
  #-- CRS 
  codigos_postales_provincia_shp <- st_transform(codigos_postales_provincia_shp, st_crs("EPSG:25830"))
  st_crs(codigos_postales_provincia_shp)$input
  sf::st_crs(codigos_postales_provincia_shp)$input
  
  #-- Centroides
  codigos_postales_centroids              <- sf::st_centroid(st_make_valid(codigos_postales_provincia_shp))
  codigos_postales_centroids              <- st_transform(codigos_postales_centroids, st_crs("EPSG:4326"))
  
  codigos_postales_centroids2             <- st_coordinates(codigos_postales_centroids)
  codigos_postales_provincia_shp$longitud <- codigos_postales_centroids2[, 1]
  codigos_postales_provincia_shp$latitud  <- codigos_postales_centroids2[, 2] 
  
  # Get just what I need
  to_save <- data.table(
    provincia            = prov_val,
    cod_postal           = codigos_postales_provincia_shp$COD_POSTAL,
    centroide_longitud   = codigos_postales_provincia_shp$longitud,
    centroide_latitud    = codigos_postales_provincia_shp$latitud
  ) 
  
  # Dataframe with everything
  provlonlat <- rbind(provlonlat, to_save)
}

#-- Save file with everything
fwrite(
  provlonlat,
  file = "./output/Provincia_Cod_Postal_Centroide.csv",
  sep = "|"
  )

system("cp ./output/Provincia_Cod_Postal_Centroide.csv /Users/carlosortega/Documents/00_Adecco/Para_J/01_Input_raw/Povincias_distritos_Gis/")

tend <- Sys.time() ; tend - tini
#----- END OF FILE ----------

provlonlat %>%
  ggplot(aes( x = centroide_longitud, y = centroide_latitud, alpha = 0.25, color = provincia)) +
  geom_point( size = 0.5) +
  easy_legend_at(to = c('none'))

# Stackoverflow.com
# https://stackoverflow.com/questions/44533952/calculating-distances-between-all-possible-combinations-of-lats-and-longs

alava <- provlonlat %>%
  filter.(provincia == "ALAVA") %>%
  # select.(centroide_longitud, centroide_latitud) %>%
  as.data.frame()
alava_dist <- geosphere::distm(alava[, 3:4 ])  
  
p <- data.frame(lat = runif(6, -90, 90), 
                 lon = runif(6, -180, 180))

geosphere::distm(p[, 2:1])


# https://stackoverflow.com/questions/44533952/calculating-distances-between-all-possible-combinations-of-lats-and-longs
p <- data.frame(lat = runif(6, -90, 90), 
                lon = runif(6, -180, 180))

# get row indices of pairs
row_pairs <- combn(nrow(p), 2)

# make data.frame of pairs
df_dist <- cbind(x = p[row_pairs[1,],], 
                 y = p[row_pairs[2,],])
# add distance column by calling distHaversine (vectorized) on each pair
df_dist$dist <- geosphere::distHaversine(df_dist[2:1], df_dist[4:3])

df_dist
#>          x.lat      x.lon      y.lat      y.lon     dist
#> 1   -10.281070 -156.30519  -7.027720 -104.76897  5677699
#> 1.1 -10.281070 -156.30519 -51.142344 -100.99517  6750255
#> 1.2 -10.281070 -156.30519  -3.979805 -141.43436  1785251
#> 1.3 -10.281070 -156.30519 -21.239130  -65.97719  9639637
#> 1.4 -10.281070 -156.30519  66.292704 -154.52851  8525401
#> 2    -7.027720 -104.76897 -51.142344 -100.99517  4923176
#> 2.1  -7.027720 -104.76897  -3.979805 -141.43436  4075742
#> 2.2  -7.027720 -104.76897 -21.239130  -65.97719  4459657
#> 2.3  -7.027720 -104.76897  66.292704 -154.52851  9085777
#> 3   -51.142344 -100.99517  -3.979805 -141.43436  6452943
#> 3.1 -51.142344 -100.99517 -21.239130  -65.97719  4502520
#> 3.2 -51.142344 -100.99517  66.292704 -154.52851 13833468
#> 4    -3.979805 -141.43436 -21.239130  -65.97719  8350236
#> 4.1  -3.979805 -141.43436  66.292704 -154.52851  7893225
#> 5   -21.239130  -65.97719  66.292704 -154.52851 12111227o


#------ SPANISH CHART -----------
provlonlat |> 
  filter.(provincia == "MADRID") |> 
  ggplot( aes(x = centroide_longitud, y = centroide_latitud)) +
  geom_point() +
  theme_bw()
