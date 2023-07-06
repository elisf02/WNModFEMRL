what_meteo_sensor_function <- function(SitiEntomo,
                                       StazMeteo){
  df = as.data.frame(t(apply(SitiEntomo, MARGIN = 1, function(a) {
    tmp = as.data.frame(t(apply(StazMeteo, MARGIN = 1, function(b) {
      c(b['IdSensore'], 
        b['Latitudine'], b['Longitudine'],
        'dist' = as.numeric(dist(rbind(c(a['Latitudine'], a['Longitudine']),
                                       c(b['Latitudine'], b['Longitudine']))))
      ) }))) %>% mutate(IdSensore = as.factor(IdSensore), 
                        Latitudine = as.numeric(Latitudine),
                        Longitudine = as.numeric(Longitudine),
                        dist = as.numeric(dist))
    
    c(a['CodiceTrappola'],
      'Latitudine.trap' = as.numeric(a['Latitudine']),
      'Longitudine.trap' = as.numeric(a['Longitudine']),
      'IdSensore.meteo' = tmp[which(tmp[,'dist'] == min(tmp[,'dist'])),'IdSensore'],
      'Latitudine.meteo' = tmp[which(tmp[,'dist'] == min(tmp[,'dist'])),'Latitudine'],
      'Longitudine.meteo' = tmp[which(tmp[,'dist'] == min(tmp[,'dist'])),'Longitudine'])
    
  })))  %>% mutate(CodiceTrappola   = as.factor(CodiceTrappola), 
                   Latitudine.trap  = as.numeric(Latitudine.trap ),
                   Longitudine.trap = as.numeric(Longitudine.trap),
                   IdSensore.meteo = as.factor(IdSensore.meteo),
                   Latitudine.meteo = as.numeric(Latitudine.meteo),
                   Longitudine.meteo = as.numeric(Longitudine.meteo))
  
  return(df)
  }
