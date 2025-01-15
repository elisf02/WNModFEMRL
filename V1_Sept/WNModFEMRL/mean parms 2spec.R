#Merli ----
UnAnno = T
anno = 2018
if(UnAnno) {
  nomi_parametri = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca", 
                     "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca", "niB_ca", 
                     "recB_ca")
  
  nome_file_parametri = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/Merli/MCMC/',
                               'parametri_2spec_Merli_M_', anno, '_1.txt')
  output_mcmc = read.table(nome_file_parametri)
  burnin = nrow(output_mcmc) * 0.1
  output_mcmc = output_mcmc[-c(1:burnin),-(length(nomi_parametri)+1)]
  
  colnames(output_mcmc) = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca", 
                            "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca", "niB_ca", 
                            "recB_ca")
  mean_parms = apply(output_mcmc, MARGIN = 2, mean)
  
  nome_file_parametri_save = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/Merli/MCMC/',
                                    'parametri_medi_2spec_Merli_', anno, '.txt')
  write.table(t(mean_parms), nome_file_parametri_save,
              row.names = F, col.names = F)
} else
{
  nomi_parametri = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca", 
                     "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca", "niB_ca", 
                     "recB_ca")
  
  nome_file_parametri1 = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/Merli/MCMC/',
                                'parametri_2spec_Merli_M_2016_1.txt')
  nome_file_parametri2 = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/Merli/MCMC/',
                                'parametri_2spec_Merli_M_2017_1.txt')
  nome_file_parametri3 = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/Merli/MCMC/',
                                'parametri_2spec_Merli_M_2018_1.txt')
  
  output_mcmc1 = read.table(nome_file_parametri1)
  output_mcmc2 = read.table(nome_file_parametri2)
  output_mcmc3 = read.table(nome_file_parametri3)
  
  burnin1 = nrow(output_mcmc1) * 0.1
  burnin2 = nrow(output_mcmc2) * 0.1
  burnin3 = nrow(output_mcmc3) * 0.1
  
  output_mcmc = rbind(output_mcmc1[-c(1:burnin1),-(length(nomi_parametri)+1)],
                      output_mcmc2[-c(1:burnin2),-(length(nomi_parametri)+1)],
                      output_mcmc3[-c(1:burnin3),-(length(nomi_parametri)+1)])
  
  colnames(output_mcmc) = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca", 
                            "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca", "niB_ca", 
                            "recB_ca")
  mean_parms = apply(output_mcmc, MARGIN = 2, mean)
  
  nome_file_parametri_save = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/Merli/MCMC/',
                                    'parametri_2spec_Merli_MediaAllYears.txt')
  write.table(t(mean_parms), nome_file_parametri_save,
              row.names = F, col.names = F)
} 

#Gazze ----
UnAnno = T
anno = 2018
if(UnAnno) {
  nomi_parametri = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca", 
                     "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca", "niB_ca", 
                     "recB_ca")
  
  nome_file_parametri = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/Gazze/MCMC/',
                               'parametri_2spec_Gazze_M_', anno, '_1.txt')
  output_mcmc = read.table(nome_file_parametri)
  burnin = nrow(output_mcmc) * 0.1
  output_mcmc = output_mcmc[-c(1:burnin),-(length(nomi_parametri)+1)]
  
  colnames(output_mcmc) = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca", 
                            "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca", "niB_ca", 
                            "recB_ca")
  mean_parms = apply(output_mcmc, MARGIN = 2, mean)
  
  nome_file_parametri_save = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/Gazze/MCMC/',
                                    'parametri_medi_2spec_Gazze_', anno, '.txt')
  write.table(t(mean_parms), nome_file_parametri_save,
              row.names = F, col.names = F)
} else
{
  nomi_parametri = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca", 
                     "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca", "niB_ca", 
                     "recB_ca")
  
  nome_file_parametri1 = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/Gazze/MCMC/',
                                'parametri_2spec_Gazze_M_2016_1.txt')
  nome_file_parametri2 = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/Gazze/MCMC/',
                                'parametri_2spec_Gazze_M_2017_1.txt')
  nome_file_parametri3 = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/Gazze/MCMC/',
                                'parametri_2spec_Gazze_M_2018_1.txt')
  
  output_mcmc1 = read.table(nome_file_parametri1)
  output_mcmc2 = read.table(nome_file_parametri2)
  output_mcmc3 = read.table(nome_file_parametri3)
  
  burnin1 = nrow(output_mcmc1) * 0.1
  burnin2 = nrow(output_mcmc2) * 0.1
  burnin3 = nrow(output_mcmc3) * 0.1
  
  output_mcmc = rbind(output_mcmc1[-c(1:burnin1),-(length(nomi_parametri)+1)],
                      output_mcmc2[-c(1:burnin2),-(length(nomi_parametri)+1)],
                      output_mcmc3[-c(1:burnin3),-(length(nomi_parametri)+1)])
  
  colnames(output_mcmc) = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca", 
                            "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca", "niB_ca", 
                            "recB_ca")
  mean_parms = apply(output_mcmc, MARGIN = 2, mean)
  
  nome_file_parametri_save = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/Gazze/MCMC/',
                                    'parametri_2spec_Gazze_MediaAllYears.txt')
  write.table(t(mean_parms), nome_file_parametri_save,
              row.names = F, col.names = F)
} 

#Germani ----
UnAnno = T
anno = 2018
if(UnAnno) {
  nomi_parametri = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca", 
                     "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca", "niB_ca", 
                     "recB_ca")
  
  nome_file_parametri = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/Germani/MCMC/',
                               'parametri_2spec_Germani_M_', anno, '_1.txt')
  output_mcmc = read.table(nome_file_parametri)
  burnin = nrow(output_mcmc) * 0.1
  output_mcmc = output_mcmc[-c(1:burnin),-(length(nomi_parametri)+1)]
  
  colnames(output_mcmc) = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca", 
                            "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca", "niB_ca", 
                            "recB_ca")
  mean_parms = apply(output_mcmc, MARGIN = 2, mean)
  
  nome_file_parametri_save = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/Germani/MCMC/',
                                    'parametri_medi_2spec_Germani_', anno, '.txt')
  write.table(t(mean_parms), nome_file_parametri_save,
              row.names = F, col.names = F)
} else
{
  nomi_parametri = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca", 
                     "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca", "niB_ca", 
                     "recB_ca")
  
  nome_file_parametri1 = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/Germani/MCMC/',
                                'parametri_2spec_Germani_M_2016_1.txt')
  nome_file_parametri2 = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/Germani/MCMC/',
                                'parametri_2spec_Germani_M_2017_1.txt')
  nome_file_parametri3 = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/Germani/MCMC/',
                                'parametri_2spec_Germani_M_2018_1.txt')
  
  output_mcmc1 = read.table(nome_file_parametri1)
  output_mcmc2 = read.table(nome_file_parametri2)
  output_mcmc3 = read.table(nome_file_parametri3)
  
  burnin1 = nrow(output_mcmc1) * 0.1
  burnin2 = nrow(output_mcmc2) * 0.1
  burnin3 = nrow(output_mcmc3) * 0.1
  
  output_mcmc = rbind(output_mcmc1[-c(1:burnin1),-(length(nomi_parametri)+1)],
                      output_mcmc2[-c(1:burnin2),-(length(nomi_parametri)+1)],
                      output_mcmc3[-c(1:burnin3),-(length(nomi_parametri)+1)])
  
  colnames(output_mcmc) = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca", 
                            "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca", "niB_ca", 
                            "recB_ca")
  mean_parms = apply(output_mcmc, MARGIN = 2, mean)
  
  nome_file_parametri_save = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/Germani/MCMC/',
                                    'parametri_2spec_Germani_MediaAllYears.txt')
  write.table(t(mean_parms), nome_file_parametri_save,
              row.names = F, col.names = F)
} 

#Cornacchie ----
UnAnno = T
anno = 2018
if(UnAnno) {
  nomi_parametri = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca", 
                     "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca", "niB_ca", 
                     "recB_ca")
  
  nome_file_parametri = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/Cornacchie/MCMC/',
                               'parametri_2spec_Cornacchie_M_', anno, '_1.txt')
  output_mcmc = read.table(nome_file_parametri)
  burnin = nrow(output_mcmc) * 0.1
  output_mcmc = output_mcmc[-c(1:burnin),-(length(nomi_parametri)+1)]
  
  colnames(output_mcmc) = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca", 
                            "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca", "niB_ca", 
                            "recB_ca")
  mean_parms = apply(output_mcmc, MARGIN = 2, mean)
  
  nome_file_parametri_save = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/Cornacchie/MCMC/',
                                    'parametri_medi_2spec_Cornacchie_', anno, '.txt')
  write.table(t(mean_parms), nome_file_parametri_save,
              row.names = F, col.names = F)
} else
{
  nomi_parametri = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca", 
                     "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca", "niB_ca", 
                     "recB_ca")
  
  nome_file_parametri1 = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/Cornacchie/MCMC/',
                                'parametri_2spec_Cornacchie_M_2016_1.txt')
  nome_file_parametri2 = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/Cornacchie/MCMC/',
                                'parametri_2spec_Cornacchie_M_2017_1.txt')
  nome_file_parametri3 = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/Cornacchie/MCMC/',
                                'parametri_2spec_Cornacchie_M_2018_1.txt')
  
  output_mcmc1 = read.table(nome_file_parametri1)
  output_mcmc2 = read.table(nome_file_parametri2)
  output_mcmc3 = read.table(nome_file_parametri3)
  
  burnin1 = nrow(output_mcmc1) * 0.1
  burnin2 = nrow(output_mcmc2) * 0.1
  burnin3 = nrow(output_mcmc3) * 0.1
  
  output_mcmc = rbind(output_mcmc1[-c(1:burnin1),-(length(nomi_parametri)+1)],
                      output_mcmc2[-c(1:burnin2),-(length(nomi_parametri)+1)],
                      output_mcmc3[-c(1:burnin3),-(length(nomi_parametri)+1)])
  
  colnames(output_mcmc) = c("p_ca", "p_sn", "B0_ca", "B0_sn", "pR_ca", 
                            "pR_sn", "b1_ca", "muB_ca", "s_ca", "phi_ca", "niB_ca", 
                            "recB_ca")
  mean_parms = apply(output_mcmc, MARGIN = 2, mean)
  
  nome_file_parametri_save = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/Cornacchie/MCMC/',
                                    'parametri_2spec_Cornacchie_MediaAllYears.txt')
  write.table(t(mean_parms), nome_file_parametri_save,
              row.names = F, col.names = F)
} 
