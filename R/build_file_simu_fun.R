################################################################################################
################### ESTIMATE THE MEAN DAILY TEMPERATURE FROM ARPA DATABASE ###################
################################################################################################
# libraries ----
library(dplyr)
library(lubridate)
library(tidyverse)
library(rlang)
FileNames = c('RW_20230223113813_535209_5864_1.csv',
                     'RW_20230223113816_535209_8136_1.csv',
                     'RW_20230223113820_535209_14766_1.csv')
year = 2020

fileTemp <- function(FileLoc = 'C:/Users/elisa/OneDrive - Università degli Studi di Milano/Documenti/Lavoro/Progetto_WNV/Preparatory_analysis_db_WN/Database_temperature_ARPA',
                     FileNames,
                     year,
                     FileClus = NA,
                     OutLoc = 'Processed data/to_mcmc',
                     OutName = 'temperatura_media_Lomb') {

  OutName = paste0(OutName, '_', year)
  message(paste('
  Arg:
  - fileLoc: folder where the db is (NB= the file needs to be one sheet only with the metadata)
  - fileNames: vector containing the FULL name of all temperature files I want to merge
  - FileClus: a file, that can be imported as data.frame, where I have the col=IdSensore and
  a col Clus with the cluster associated to the sensor. Default = NA, meaning that the daily temperature
  is estimated as the mean over all the sensors.
  - year must be a vec of all the years we want to include in the final .RDS file
  - OutLoc: if non NA, folder where to save the file. If NA the file is not saved
  - OutName: name of the created file
  **filename = ', OutName)
  )
  names_file_temp_to_import <- as.array(paste0(FileLoc, '/',
                                               FileNames))

  temperature_list <- lapply(names_file_temp_to_import, FUN = function(a) {
    read.csv2(a, header = T) %>%
      mutate('IdSensore' = as.factor(Id.Sensore),
             'Date' = as.Date(Data.Ora,"%d/%m/%y"),
             'year' = (year(as.Date(Data.Ora,"%d/%m/%y"))),
             'month' = (month(as.Date(Data.Ora,"%d/%m/%y"))),
             'yday' = (yday(as.Date(Data.Ora,"%d/%m/%y"))),
             'Daily_mean' = as.numeric(Valore.Medio.Giornaliero),
             'Daily_min' = as.numeric(Minimo.Valore.Medio.Orario ),
             'Daily_max' = as.numeric(Massimo.Valore.Medio.Orario )
      ) %>%
      select(IdSensore, year, month, yday, Date, Daily_mean, Daily_min, Daily_max)
  })
  temperature_mat = c()
  for (i in (1:length(temperature_list))) {
    temperature_mat = rbind(temperature_mat,
                            temperature_list[[i]])

  }

  if(is.na(FileClus)) {
    temperature_mat <- as.data.frame(temperature_mat) %>%
      mutate(Clus = as.factor(1))
  }
  if(!is.na(FileClus)) {
    Clus = read.csv2(Clus, header = T) %>%
      mutate('IdSensore' = as.factor(Id.Sensore),
             'Clus' = as.factor(Clus))
    temperature_mat <- merge(temperature_mat, Clus)
  }

  DailyMeanT = temperature_mat %>%
    mutate(Clus = as.factor(Clus)) %>%
    group_by(Clus, Date) %>%
    summarise(meanT = mean(Daily_mean, na.rm = TRUE)) %>%
    filter(year(Date) == year) %>%
    mutate(yday = yday(Date)) %>%
    arrange(yday)

  temps = c()
  for (C in levels(DailyMeanT$Clus)) {
    ttmp = DailyMeanT %>% subset(Clus == C)
    temps = rbind(temps, as.vector(ttmp$meanT))
  }

  #plot(temps[1,], type = 'l', lwd = 2)
  #lines(temps[2,], col = 'red')

  write(as.vector(temps),
        file = paste0(OutLoc, '/', OutName),
        ncolumns = length(as.vector(temps))/length(levels(DailyMeanT$Clus)))
}

fileZanz <- function(FileLoc = 'Processed data',
                     FileName = 'EntomoSurvRL_2016_2018.RDS',
                     year,
                     FileClus = NA,
                     TemporalAggr = T, #n di giorni
                     Specie =  'Culex pipiens: femmine',
                     OutLoc = 'Processed data/to_mcmc') {
  OutNameNPool = paste0('totale_pool_cluster',year)
  OutNamePoolSize = paste0('mean_pool_size_cluster',year)
  OutNameOpenTraps = paste0('Giorni_cattura',year)

  message(paste('
  Arg:
  - fileLoc: folder where the db is (NB= the file needs to be one sheet only with the metadata)
  - fileNames: vector containing the FULL name of all temperature files I want to merge
  - FileClus: a file, that can be imported as data.frame, where I have the col=IdSensore and
  a col Clus with the cluster associated to the sensor. Default = NA, meaning that the daily temperature
  is estimated as the mean over all the sensors.
  - year must be a vec of all the years we want to include in the final .RDS file
  - OutLoc: if non NA, folder where to save the file. If NA the file is not saved
  - OutName: name of the created file
  - TemporalAggr: if data are aggregated. If T data are aggregated following the WEEK of the year
  \n (NB) I have the same number of days for yeach cluster!! (CHECK)
  \n**filenames = ', OutNameNPool, ',', OutNamePoolSize,
                'and ', OutNameOpenTraps)
  )

  OutNameNPool = paste0(OutLoc,'/',OutNameNPool)
  OutNamePoolSize = paste0(OutLoc,'/',OutNamePoolSize)
  OutNameOpenTraps = paste0(OutLoc,'/',OutNameOpenTraps)

  Zanz = readRDS(paste0(FileLoc, '/', FileName)) %>%
    subset(IdentificazioneInsetti == Specie ) %>%
    mutate(weekPrelievo = week(dataPrelievo)) %>%
    subset(!is.na(PCR))

  if(is.na(FileClus)) {
    Zanz <- as.data.frame(Zanz) %>%
      mutate(Clus = as.factor(1))
  }
  if(!is.na(FileClus)) {
    Clus = read.csv2(FileClus, header = T) %>%
      mutate('CodiceTrappola ' = as.factor(CodiceTrappola),
             'Clus' = as.factor(Clus))
    Zanz <- merge(Zanz, Clus)
  }

  if(TemporalAggr)
    Zanz2 = merge(Zanz, Zanz %>%
                    mutate(weekPrelievo = week(dataPrelievo)) %>%
                    group_by(weekPrelievo) %>%
                    summarise(dataPrelievoGroup = mean(dataPrelievo))) %>%
      group_by(Clus, dataPrelievoGroup) %>%
      summarise( NPool = n(),
                 MeanDimPool = mean(DimensionePool)) %>%
      mutate(Clus = as.factor(Clus),
             dataPrelievo = dataPrelievoGroup) else
        Zanz2 = Zanz %>%
      group_by(Clus, dataPrelievo) %>%
      summarise( NPool = n(),
                 MeanDimPool = mean(DimensionePool)) %>%
      mutate(Clus = as.factor(Clus))

  days = seq(1,365)
  NPools = c()
  MeanDimPools = c()
  OpenTrap = c()
  for (C in levels(Zanz2$Clus)) {
    ttmp = Zanz2 %>% subset(Clus == C)
    NPools = rbind(NPools, paste(ttmp$NPool, sep = " ", collapse = ' '))
    MeanDimPools = rbind(MeanDimPools, paste(ttmp$MeanDimPool, sep = " ", collapse = ' ')) #  rbind(MeanDimPools, as.vector(ttmp$MeanDimPool))
    OpenTrap = rbind(OpenTrap, paste(ifelse(days %in% yday(ttmp$dataPrelievo), 1, 0),
                                     sep = " ", collapse = ' '))
  }

  write(NPools,
        file = paste0(OutLoc, '/', OutNameNPool),
        ncolumns = 1)
  write(MeanDimPools,
        file = paste0(OutLoc, '/', OutNamePoolSize),
        ncolumns = 1)
  write(OpenTrap,
        file = paste0(OutLoc, '/', OutNameOpenTraps),
        ncolumns = 1)

}
