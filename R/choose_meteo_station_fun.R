# file to chose the best meteo station given traps coordinates
###############################################################################
#libraries ----
library(tidyverse)

choose_meteo_stat_func = function(SitiEntomoLoc = 'Processed data',
                                  SitiEntomoNameFile = 'Metadata_EtomoSurvRL_2016_2018.RDS',
                                  StazMeteoLoc = 'Processed data',
                                  StazMeteoNameFile = 'Metadata_StazMeteo.RDS',
                                  Return = 'ElencoStazioni' # 'StazioniECoord
                                  ) {

  SitiEntomo = readRDS(paste0(SitiEntomoLoc,'/', SitiEntomoNameFile))
  StazMeteo = readRDS(paste0(StazMeteoLoc, '/', StazMeteoNameFile)) %>%
    mutate(Longitudine = as.numeric(lng),
           Latitudine = as.numeric(lat)) %>%
    select(IdSensore, Tipologia, Provincia, Longitudine, Latitudine)

  list = lapply(levels(SitiEntomo$Anno), function(anno) {
    what_meteo_sensor_function(SitiEntomo = SitiEntomo %>%
                                 filter(Anno == anno),
                               StazMeteo = StazMeteo)
  })

  list_small = sapply(list, function(a) {
    a$IdSensore.meteo
  })

  if(Return == 'ElencoStazioni')
    ret = list_small
  if(Return == 'StazioniECoord')
    ret = list

    return(ret)
}
