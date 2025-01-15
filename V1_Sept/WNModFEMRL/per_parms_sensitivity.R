#creo vettore con i nomi dei parametri (TUTTI QUELLI STIMATI)
nomi_parametri = c("p", "B0", "pR", "b1", "muB", 
                   "s", "phi", "niB", "recB")

nome_file_parametri = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/MCMC/',
                             'parametri_MediaAllYears.txt')#'parametri_', anno, '_1.txt')
output_mcmc = read.table(nome_file_parametri)

colnames(output_mcmc) = c("p", "B0", "pR", "b1", "muB", 
                          "s", "phi", "niB", "recB")
# cambiando b0 (muB) ----
min = 0/365
max = 8/365

tmp1 =  seq(min, max, length.out = 100)
input_sens = c()
for(i in 1:100) {
  tmp = output_mcmc
  tmp['muB'] = tmp1[i]
  
  input_sens = rbind(input_sens, tmp)
}
nome_file_parametri_save = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/MCMC/',
                                  'parametri_sens_MeadiaAllYears_muB.txt')
write.table((input_sens), nome_file_parametri_save,
            row.names = F, col.names = F)
# cambiando s ----
min = 5
max = 20

tmp1 =  seq(min, max, length.out = 100)
input_sens = c()
for(i in 1:100) {
  tmp = output_mcmc
  tmp['s'] = tmp1[i]
  
  input_sens = rbind(input_sens, tmp)
}
nome_file_parametri_save = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/MCMC/',
                                  'parametri_sens_MeadiaAllYears_s.txt')
write.table((input_sens), nome_file_parametri_save,
            row.names = F, col.names = F)
# cambiando phi ----
min = 0.33
max = 0.66

tmp1 =  seq(min, max, length.out = 100)
input_sens = c()
for(i in 1:100) {
  tmp = output_mcmc
  tmp['phi'] = tmp1[i]
  
  input_sens = rbind(input_sens, tmp)
}
nome_file_parametri_save = paste0('/home/nicolaferrari/Scrivania/GitHub/WestNile_project/Output_WNV/MCMC/',
                                  'parametri_sens_MeadiaAllYears_phi.txt')
write.table((input_sens), nome_file_parametri_save,
            row.names = F, col.names = F)
