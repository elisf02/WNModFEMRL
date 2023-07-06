# libraries ----
library(tidyverse)
library(readxl)

# create a Rds to share file from RL database ----
# db entomo RL ----
extract_metadata_RLdb_funct = function(fileLoc = 'C:/Users/elisa/OneDrive - Università degli Studi di Milano/Documenti/Lavoro/Progetto_WNV/Preparatory_analysis_db_WN/Database_Entomologico WestNile RL',
                                       fileName = 'Siti entomologico ',
                                       fileExt = 'xls',
                                       years = c(2016,2017,2018),
                                       SaveIn = 'Processed data' #NA
) {
  message('
  Arg:
  - fileLoc: folder where the db is (NB= the file needs to be one sheet only with the metadata)
  - fileName: must be the name of the file (NB= EXCLUDING the year and the extension)
  - fileExt: the extension of the file (xls included)
  - years must be a vec of all the years we want to include in the final .RDS file
  - SaveIn: if non NA, folder where to save the file. If NA the file is not saved
  **filename = Metadata_EtomoSurvRL_min(years)_max(years).RDS'
  )

  finalDb = c() # metadati siti entomo
  for (a in years) {
    message(a)
    tmp = read_excel(paste0(fileLoc,'/', fileName, a, '.', fileExt)) %>%
      mutate(CodiceTrappola = as.factor(`Codice Trappola`),
             ComunePrelievo = as.factor(ComunePrelievo),
             Anno = as.factor(Anno),
             ComunePrelievo = as.factor(ComunePrelievo),
             CodiceAllevamento = as.factor(`Codice Allevamento`),
             Provincia = as.factor(Provincia),
             Longitudine = as.numeric(Longitudine),
             Latitudine = as.numeric(Latitudine)
      ) %>%
      select(CodiceTrappola, ComunePrelievo, Anno,
             ComunePrelievo,CodiceAllevamento,
             Provincia, Longitudine, Latitudine)

    finalDb = rbind(finalDb, tmp)
  }

  if(!is.na(SaveIn))
    saveRDS(finalDb, file = paste0(SaveIn,
                                   '/Metadata_EtomoSurvRL_',min(years),
                                   '_', max(years),'.RDS'))
  return(finalDb)
}

prepare_entomo_db = function(fileLoc = 'C:/Users/elisa/OneDrive - Università degli Studi di Milano/Documenti/Lavoro/Progetto_WNV/Preparatory_analysis_db_WN/Database_Entomologico WestNile RL',
                             fileName = 'Estrazione dati entomologico WestNile ',
                             fileExt = 'xls',
                             years = c(2016,2017,2018),
                             Species = "Culex pipiens: femmine",
                             SaveIn = 'Processed data', #NA
                             Return = 'EntomoRaw' # 'EntomoRaw', 'NZanz', 'EpiZanz'
) {
  message('
  Arg:
  - fileLoc: folder where the db is (NB= the file needs to be one sheet only with the metadata)
  - fileName: must be the name of the file (NB= EXCLUDING the year and the extension)
  - fileExt: the extension of the file (xls included)
  - years must be a vec of all the years we want to include in the final .RDS file
  - SaveIn: if non NA, folder where to save the file. If NA the file is not saved
  - Return: what db I want to be returned (EntomoRaw, Nzanz, EpiZanz)
  **filename = Metadata_EtomoSurvRL_min(years)_max(years).RDS'
  )

  finalDb = c() # metadati siti entomo
  for (a in years) {
    message(a)
    tmp = read_excel(paste0(fileLoc,'/', fileName, a, '.', fileExt)) %>%
      mutate(CodiceTrappola = as.factor(`Codice Trappola`),
             ComunePrelievo = as.factor(ComunePrelievo),
             Provincia = as.factor(Provincia),
             Anno = as.factor(Anno),
             dataPrelievo = as.Date(dataPrelievo),
             numeroRegistro = as.factor(numeroRegistro),
             CodicePool = as.factor(`Codice pool`),
             PCR = as.factor(PCR),
             DimensionePool = as.numeric(`Dimensione pool`),
             IdentificazioneInsetti = as.factor(IdentificazioneInsetti)) %>%
      select(CodiceTrappola, ComunePrelievo, Provincia, Anno,
             dataPrelievo, PCR, DimensionePool, IdentificazioneInsetti)


    finalDb = rbind(finalDb, tmp)
  }

  EntomoRaw = finalDb

  if(Species != 'All') {
    message(paste('Only', Species))
    EntomoRaw <- EntomoRaw %>%
      filter(IdentificazioneInsetti == Species)
  }

  message('the rows without PCR results are removed from Nzanz and EpiZanz')
  NZanz = EntomoRaw %>%
    filter(!is.na(PCR)) %>% # removal of rows without PCR res
    group_by(CodiceTrappola, dataPrelievo, Anno) %>%
    summarise(n_mosq = sum(DimensionePool))

  EpiZanz = EntomoRaw %>%
    filter(!is.na(PCR)) %>% # removal of rows without PCR res
    group_by(CodiceTrappola, dataPrelievo, Anno) %>%
    mutate(pospooltmp = ifelse(PCR == 'Negativo', 0, 1)) %>%
    summarise(n_mosq = sum(DimensionePool),
              n_pool =n(),
              n_pool_pos = sum(pospooltmp),
              prev = n_pool_pos/n_pool) %>%
    select(CodiceTrappola, dataPrelievo, Anno, n_mosq,
           n_pool, n_pool_pos, prev)

  if(!is.na(SaveIn)) {
    saveRDS(EntomoRaw, file = paste0(SaveIn,
                                   '/EntomoSurvRL_',min(years),
                                   '_', max(years),'.RDS'))
    saveRDS(NZanz, file = paste0(SaveIn,
                                   '/NZanzSurvRL_',min(years),
                                   '_', max(years),'.RDS'))
    saveRDS(EpiZanz, file = paste0(SaveIn,
                                   '/EpiZanzSurvRL_',min(years),
                                   '_', max(years),'.RDS'))

  }
  if(Return == 'EntomoRaw')
    return(as.data.frame(EntomoRaw))
  if(Return == 'NZanz')
    return(as.data.frame(NZanz))
  if(Return == 'EpiZanz')
    return(as.data.frame(EpiZanz))
}

prepare_ARPAtemp_db = function(fileLoc = 'C:/Users/elisa/OneDrive - Università degli Studi di Milano/Documenti/Lavoro/Progetto_WNV/Preparatory_analysis_db_WN/Database_temperature_ARPA',
                               fileName = 'Stazioni_Meteorologiche',
                               fileExt = 'csv',
                               SaveIn = 'Processed data' #NA
) {
  message('
  Arg:
  - fileLoc: folder where the db is (NB= the file needs to be one sheet only with the metadata)
  - fileName: must be the name of the file (NB= EXCLUDING the extension)
  - fileExt: the extension of the file (xls included)
  - SaveIn: if non NA, folder where to save the file. If NA the file is not saved
  **filename = Metadata_EtomoSurvRL_min(years)_max(years).RDS'
  )

  name_file_stazioni_meteo <- paste0(fileLoc, '/',
                                     fileName, '.', fileExt)
  staz_meteo_coordinate <- read.csv2(name_file_stazioni_meteo, header = T) %>%
    mutate('IdSensore' = as.factor(IdSensore),
           'Tipologia' = as.factor(Tipologia),
           'Provincia' = as.factor(Provincia),
           'DataStart' = as.Date(DataStart,"%m/%d/%y"),
           # 'DataStop' = as.Date(DataStop),
           'UTM_Nord' = as.numeric(UTM_Nord),
           'UTM_Est' = as.numeric(UTM_Est),
           'lng' = as.numeric(lng),
           'lat' = as.numeric(lat)) %>%
    filter(Tipologia == 'Temperatura') %>%
    select(IdSensore, Tipologia, Provincia, lng, lat)


    if(!is.na(SaveIn)) {
      saveRDS(staz_meteo_coordinate,
              file = paste0(SaveIn, '/Metadata_StazMeteo.RDS'))
      }

  return(as.data.frame(staz_meteo_coordinate))
}





