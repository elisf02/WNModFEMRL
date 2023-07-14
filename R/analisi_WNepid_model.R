#### codice per analizzare modello con 2 carrying capacities
#### beta=4.61, alpha=0.108 da Li et al. 2016
#### uso le 70 trappole sempre attive tra 2013 e 2018

# graphics.off()
# rm(list = ls())
WN_MCMC_and_Simu <- function(seed=0,
                             max_iter_MCMC=10000,
                             numero_simulazioni = 100,
                             n_iter_preliminary_lik = 1000,

                             numero_cluster=1,
                             anno_inizio=2016,
                             anno_fine=2016,

                             con_cosa_inizio = 1,

                             prior_pR=0,
                             prior_B0=0,

                             min_niB=0.1,
                             max_niB=1,
                             min_recB=0.1,
                             max_recB=1,
                             min_muB=2/365,
                             max_muB=20/365,
                             min_s=5,
                             max_s=10,
                             min_phi=0,
                             max_phi=1,

                             file_temperatura_name= "per_mcmc/temperatura_media_",
                             file_numero_pool_name = "per_mcmc/totale_pool_cluster_",
                             file_numero_pool_positivi_name = "per_mcmc/totale_pool_WNVpos_cluster_",
                             file_pool_size_name = "per_mcmc/mean_pool_size_cluster_",
                             file_zanzare_name = "per_mcmc/zanzare/adulti_medi_",
                             vettore_date_catture="per_mcmc/giorni_cattura.txt",

                             sigma_big=3, #2 sigma per B0
                             sigma_small=0.01, #0.0001 sigma per tutti i parametri
                             sigma_medio = 1, # sigma per s

                             LocOutMCMC = "Output_WNV/MCMC",
                             LocOutSimu = "Output_WNV/Simulazioni",

                             NameOutParms = "parametri_",
                             NameOutParmsPerSimu = "per_simulazione_",
                             NameOutDyn = "dynamics_"

){
  nomi_parametri=c("p","B0","pR","b1",# "b2",
                   "muB",
                   "s", "phi", "niB", "recB")
  numero_catture=length(scan(vettore_date_catture,quiet=T))

  cluster_simulati=numero_cluster

  for(quale_cluster in 1:numero_cluster){
    for(anno in anno_inizio:anno_fine){

      file_temperatura= paste0(file_temperatura_name,anno)
      file_numero_pool= paste0(file_numero_pool_name,anno)
      file_numero_pool_positivi= paste0(file_numero_pool_positivi_name,anno)
      file_pool_size= paste0(file_pool_size_name,anno)
      file_zanzare= paste0(file_zanzare_name,anno)


      if(con_cosa_inizio==0)
        nome_file_parametri=paste0(LocOutMCMC, "/", NameOutParms,
                                   "B_",anno,"_",quale_cluster,".txt")
      if(con_cosa_inizio==1)
        nome_file_parametri=paste0(LocOutMCMC, "/", NameOutParms,
                                   "M_",anno,"_",quale_cluster,".txt")

      beta=4.61

      to_terminal=paste("time",
                        paste0(system.file(package = "WNModFEMRL"),'/templates/uccelli1Clus_WNV_mcmc'),
                        seed,file_temperatura,file_numero_pool,file_numero_pool_positivi,
                        file_pool_size,numero_catture,vettore_date_catture,nome_file_parametri,
                        max_iter_MCMC,sigma_small,sigma_big,con_cosa_inizio,file_zanzare,
                        beta,quale_cluster,numero_cluster,prior_pR,prior_B0,
                        min_muB,max_muB,
                        min_s,max_s,min_phi,max_phi,min_niB,max_niB,min_recB,max_recB,
                        n_iter_preliminary_lik, sigma_medio)
      system(to_terminal)

      output_mcmc=read.table(nome_file_parametri)
      if(ncol(output_mcmc)<=1)
        output_mcmc=matrix(0,ncol=length(nomi_parametri)+1,nrow=max_iter_MCMC)
      nomi_parametri_plot=c(nomi_parametri,"LogLik")
      par(mfrow=c(2,2))
      for(j in 1:length(nomi_parametri_plot))
        plot(output_mcmc[,j],type="l",main=nomi_parametri_plot[j])
      burnin=nrow(output_mcmc)*0.1

      if(con_cosa_inizio==0)
        parametri_per_simulazione=paste0(LocOutMCMC, "/",
                                         NameOutParmsPerSimu, "B_",
                                         anno,"_",quale_cluster,".txt")
      if(con_cosa_inizio==1)
        parametri_per_simulazione=paste0(LocOutMCMC, "/",
                                         NameOutParmsPerSimu, "M_",
                                         anno,"_",quale_cluster,".txt")

      write.table(output_mcmc[-c(1:burnin),-ncol(output_mcmc)],row.names=F,col.names=F,file=parametri_per_simulazione)

      if(con_cosa_inizio==0)
        nome_file_output_dynamics=paste0(LocOutSimu,
                                         "/", NameOutDyn, "B_",
                                         anno,"_",quale_cluster,".txt")
      if(con_cosa_inizio==1)
        nome_file_output_dynamics=paste0(LocOutSimu,
                                         "/", NameOutDyn, "M_",
                                         anno,"_",quale_cluster,".txt")

      ncol_file_parametri=ncol(read.table(parametri_per_simulazione))
      nrow_file_parametri=nrow(read.table(parametri_per_simulazione))

      to_terminal = paste("time",
                          paste0(system.file(package = "WNModFEMRL"),'/templates/uccelli1Clus_WNV_simulation'),
                          seed,file_temperatura,file_numero_pool,file_numero_pool_positivi,
                          file_pool_size,numero_catture,vettore_date_catture,nome_file_output_dynamics,
                          numero_simulazioni,parametri_per_simulazione,ncol_file_parametri,nrow_file_parametri,
                          con_cosa_inizio,file_zanzare,beta,quale_cluster, numero_cluster)

      system(to_terminal)
    }
  }
}


WNV_ParPlotFunc <- function(anno_inizio,
                     anno_fine,
                     numero_cluster,
                     con_cosa_inizio = 1, # cioè con zanzare, che è quello che abbiamo sempre fatto

                     OutLoc = 'Output_WNV/MCMC',
                     FileAllParmsName="parametri_",
                     FileParmsPerSimuName="per_simulazione_"
) {
  nomi_parametri=c("p",
                   "B0",
                   "pR",
                   "b1",
                   'niB',
                   'recoveryB',
                   'b0',
                   's',
                   'phi')

  for(anno in anno_inizio:anno_fine){

    parametri_stimati=vector("list", length(nomi_parametri))

    for(quale_cluster in 1:numero_cluster){
      #nome_file_parametri=paste0("Output/MCMC/parametri_2K_",anno,"_WNV5reg_alphaFix_constPerLik_70trappole_",quale_cluster,".txt")
      if(con_cosa_inizio == 0)
        nome_file_parametri=paste0(OutLoc, "/",
                                   FileAllParmsName,"B_", anno, "_" ,quale_cluster,
                                   ".txt")
      if(con_cosa_inizio == 1)
        nome_file_parametri=paste0(OutLoc, "/",
                                   FileAllParmsName,"M_", anno, "_" ,quale_cluster,
                                   ".txt")
      output_mcmc=read.table(nome_file_parametri)
      burnin=nrow(output_mcmc)*0.1

      if(ncol(output_mcmc)<=1)
        output_mcmc=matrix(0,ncol=length(nomi_parametri)+1,nrow=max_iter_MCMC)
      for(j in 1:(ncol(output_mcmc)-1))
        parametri_stimati[[j]]=cbind(parametri_stimati[[j]],output_mcmc[-c(1:burnin),j])
    }
    if(con_cosa_inizio == 0)
      nome_file=paste0(OutLoc, "/Plots/",
                       FileAllParmsName,"B_",  anno, "_" ,quale_cluster,
                       ".jpg")
    if(con_cosa_inizio == 1)
      nome_file=paste0(OutLoc, "/Plots/",
                       FileAllParmsName,"M_",  anno, "_" ,quale_cluster,
                       ".jpg")
    jpeg(nome_file,width=length(nomi_parametri)*1200,height=1000,res=200)
    par(mfrow=c(1,length(nomi_parametri)),cex.axis=1.5,mar=c(4,6,2,2),cex.lab=1.5,cex.main=1.5)

    for(i in 1:length(parametri_stimati)){
      tab=parametri_stimati[[i]]
      ymax=0
      for(j in 1:ncol(tab))
        if(quantile(tab[,j],probs=0.975)>ymax)
          ymax=quantile(tab[,j],probs=0.975)
      ymin=ymax
      for(j in 1:ncol(tab))
        if(quantile(tab[,j],probs=0.025)<ymin)
          ymin=quantile(tab[,j],probs=0.025)
      plot(0,col="white",xlim=c(1,ncol(tab)),ylim=c(ymin,ymax),main=nomi_parametri[i],
           axes=F,xlab="Cluster",ylab="")
      for(j in 1:ncol(tab))
        plotbox(w=j,X=tab[,j],fun=median)
      axis(2,at=seq(ymin,ymax,length.out=4),las=2,
           labels=format(seq(ymin,ymax,length.out=4),scientific=T,digits=2))
      axis(1,at=c(1:ncol(tab)))
    }
    dev.off()
  }


}



WN_PlotModelFit = function(anno_inizio = 2016,
                           anno_fine = 2016,
                           numero_cluster = 1,

                           con_cosa_inizio = 1,

                           max_iter_MCMC = 10000,
                           tmax= 180, #210,

                           FolderSimu="Output_WNV/Simulazioni/",
                           FileDynName = "dynamics_",
                           FolderPlotOut = "Output_WNV/Simulazioni/Plots/",
                           PlotName = "ModelFit_",

                           file_numero_pool_name = "per_mcmc/totale_pool_cluster_",
                           file_numero_pool_positivi_name = "per_mcmc/totale_pool_WNVpos_cluster_",
                           file_pool_size = "per_mcmc/mean_pool_size_cluster_",
                           vettore_date_catture="per_mcmc/giorni_cattura.txt",

                           cex_main = 2,
                           cex_axes = 1.2,
                           cex_lab_axes = 1.2,
                           margins_plot = c(7,7,4,2),
                           ymax_plot = 10) {

  settimane=scan(vettore_date_catture)

  colori=rainbow(n=numero_cluster)
  quanti_dentro95=0
  quante_osservazioni=0
  for(anno in anno_inizio:anno_fine){
    print(anno)

    nome_file_plot=paste0(FolderPlotOut, PlotName ,anno,".jpg")
    jpeg(nome_file_plot,height=1*1000,width=1*1400,res=200)
    par(mfrow=c(1,1),mar=margins_plot,cex.main=cex_main,cex.axis=cex_axes)

    message(paste('plot ModelFit',anno))

    file_numero_pool=paste0(file_numero_pool_name, anno)
    file_numero_pool_positivi=paste0(file_numero_pool_positivi_name ,anno)
    file_pool_size=paste0(file_pool_size ,anno)

    numero_pool=read.table(file_numero_pool)
    numero_pool_positivi=read.table(file_numero_pool_positivi)
    pool_size=read.table(file_pool_size)

    numero_classi=7

    for(quale_cluster in 1:numero_cluster){
      quando_ho_pool=which(pool_size[quale_cluster,]>0)

      pool_positivi_simulati=c()
      con_cosa_inizio=1
      nome_file_output_dynamics=paste0(FolderSimu,
                                       FileDynName,
                                       "M_",
                                       anno,"_",quale_cluster,".txt")

      output_dynamics=read.table(nome_file_output_dynamics)
      # output_dynamics=output_dynamics[,-1]
      mean_MS_allcluster=c()
      mean_ME_allcluster=c()
      mean_MI_allcluster=c()
      sel_MS=seq(1,nrow(output_dynamics),numero_classi)
      sel_ME=seq(2,nrow(output_dynamics),numero_classi)
      sel_MI=seq(3,nrow(output_dynamics),numero_classi)

      MS=output_dynamics[sel_MS,]
      ME=output_dynamics[sel_ME,]
      MI=output_dynamics[sel_MI,]

      mosquito_prevalence=MI/(MS+ME+MI)
      qmax_mosquito_prevalence=c()
      qmin_mosquito_prevalence=c()
      mean_mosquito_prevalence=c()
      for(j in 1:(ncol(mosquito_prevalence))){
        qmax_mosquito_prevalence=c(qmax_mosquito_prevalence,
                                   quantile(mosquito_prevalence[,j],probs=0.975,na.rm=T))
        qmin_mosquito_prevalence=c(qmin_mosquito_prevalence,
                                   quantile(mosquito_prevalence[,j],probs=0.025,na.rm=T))
        mean_mosquito_prevalence=c(mean_mosquito_prevalence,
                                   mean(mosquito_prevalence[,j],na.rm=T))
      }

      qmax_mosquito_prevalence[which(is.na(qmax_mosquito_prevalence))]=0
      qmin_mosquito_prevalence[which(is.na(qmin_mosquito_prevalence))]=0
      mean_mosquito_prevalence[which(is.na(mean_mosquito_prevalence))]=0

      for(index_giorno_pool in quando_ho_pool){
        #ora devi sottrarre al giorno di cattura un numero in modo tale che le date coincidano.
        #120=il modello parte da 1 maggio
        giorno_pool=settimane[index_giorno_pool]-120
        if(giorno_pool<tmax){
          prob_pool_positivo=1-(1-mosquito_prevalence[,giorno_pool])^as.numeric(pool_size[quale_cluster,index_giorno_pool])
          pool_analizzati=numero_pool[quale_cluster, index_giorno_pool]
          pool_positivi=numero_pool_positivi[quale_cluster, index_giorno_pool]
          tmp=c()
          tmp_sel=sample(1:length(prob_pool_positivo),size=100)
          for(quale in tmp_sel)
            tmp=c(tmp,rbinom(n=1,size=pool_analizzati,prob=prob_pool_positivo[quale]))
          pool_positivi_simulati=cbind(pool_positivi_simulati,tmp)
        }
      }

      # ymax=max(pool_positivi_simulati,as.numeric(catture[5, quando_ho_pool]))
      ymax=ymax_plot
      xmax=ncol(pool_positivi_simulati)
      plot(0,col="white",xlim=c(0,xmax),ylim=c(0,ymax),xlab="",ylab="",axes=F,main=quale_cluster)
      for(i in 1:(ncol(pool_positivi_simulati)))
        plotbox(w=i,X=pool_positivi_simulati[,i],fun=median,wd=0.2,col=colori[quale_cluster])
      points(1:xmax, numero_pool_positivi[quale_cluster, quando_ho_pool][1:xmax],
             pch=19,
             col="darkorange",cex=2)
      # mesi=c("May","Jun","Jul","Aug","Sep","Oct","Nov")
      date=format(as.Date(as.numeric(settimane[quando_ho_pool[1:xmax]]),origin=paste("01-01-",anno,sep=""),format="%d-%m-%Y"),"%d-%m")
      # quando_inizio_mesi=which(duplicated(format(as.Date(date,format="%d-%m"),"%m"))==F)
      # quando_inizio_mesi=c(quando_inizio_mesi,xmax)
      axis(1,at=c(1:xmax),labels=date,las=2)
      axis(2,las=2,at=seq(0,ymax,5))
      mtext("WNV positive pools",side=2,line=5,adj=0.5,cex=1.7)


      quante_osservazioni=quante_osservazioni+length(quando_ho_pool)
      for(i in 1:(ncol(pool_positivi_simulati))){
        qmin=quantile(pool_positivi_simulati[,i],probs=0.025)
        qmax=quantile(pool_positivi_simulati[,i],probs=0.975)
        if(numero_pool_positivi[quale_cluster, quando_ho_pool][i]>=qmin
           & numero_pool_positivi[quale_cluster, quando_ho_pool][i]<=qmax)
          quanti_dentro95=quanti_dentro95+1
      }

    }
    dev.off()
  }
  return(list('quanti_dentro'=round(quanti_dentro95/quante_osservazioni,2)))
}

WN_PlotDynamics <- function(anno_inizio,
                            anno_fine,
                            numero_cluster,

                            con_cosa_inizio = 1,

                            FolderSimu="Output_WNV/Simulazioni/",
                            FileDynName = "dynamics_",
                            FolderPlotOut = "Output_WNV/Simulazioni/Plots/",
                            PlotName = "Dynamics_MosqPrev_",

                            vettore_date_catture="per_mcmc/giorni_cattura_cluster.txt",

                            cex_main = 2,
                            cex_axes = 1.2,
                            cex_lab_axes = 1.2,
                            margins_plot = c(7,7,4,2),
                            colore = 'darkgreen',
                            what_plot = 'mosquito_prevalence' # or:'moquito population' 'bird_prevalence', 'bird_population'
) {
  settimane=scan(vettore_date_catture)

  for(anno in anno_inizio:anno_fine){
    print(anno)

    nome_file_plot=paste0(FolderPlotOut, PlotName ,anno,".jpg")
    jpeg(nome_file_plot,height=1*1000,width=1*1400,res=200)

    par(mfrow=c(1,1),mar=margins_plot,cex.main=cex_main,cex.axis=cex_axes)

    numero_classi = 7
    for(quale_cluster in 1:numero_cluster){
      if(con_cosa_inizio==0)
        nome_file_output_dynamics=paste0(FolderSimu,
                                         FileDynName,"B_",anno,"_",quale_cluster,".txt")
      if(con_cosa_inizio==1)
        nome_file_output_dynamics=paste0(FolderSimu,
                                         FileDynName,"M_",anno,"_",quale_cluster,".txt")

      output_dynamics=read.table(nome_file_output_dynamics)
      # output_dynamics=output_dynamics[,-1]
      sel_MS=seq(1,nrow(output_dynamics),numero_classi)
      sel_ME=seq(2,nrow(output_dynamics),numero_classi)
      sel_MI=seq(3,nrow(output_dynamics),numero_classi)
      sel_BS=seq(4,nrow(output_dynamics),numero_classi)
      sel_BE=seq(5,nrow(output_dynamics),numero_classi)
      sel_BI=seq(6,nrow(output_dynamics),numero_classi)
      sel_BR=seq(7,nrow(output_dynamics),numero_classi)
      MS=output_dynamics[sel_MS,]
      ME=output_dynamics[sel_ME,]
      MI=output_dynamics[sel_MI,]
      BS=output_dynamics[sel_BS,]
      BE=output_dynamics[sel_BE,]
      BI=output_dynamics[sel_BI,]
      BR=output_dynamics[sel_BR,]

      mosquito_population = MS+ME+MI
      mosquito_prevalence=MI/mosquito_population
      bird_population = BS+BE+BI+BR
      bird_prevalence=BI/bird_population

      if(what_plot == 'mosquito_prevalence')
        pop = mosquito_prevalence
      if(what_plot == 'moquito_population')
        pop = mosquito_population
      if(what_plot == 'bird_prevalence')
        pop = bird_prevalence
      if(what_plot == 'bird_population')
        pop = bird_population

      qmax_pop=c()
      qmin_pop=c()
      mean_pop=c()
      for(j in 1:(ncol(pop))){
        qmax_pop=c(qmax_pop,
                   quantile(pop[,j],probs=0.975,na.rm=T))
        qmin_pop=c(qmin_pop,
                   quantile(pop[,j],probs=0.025,na.rm=T))
        mean_pop=c(mean_pop,
                   mean(pop[,j],na.rm=T))
      }

      qmax_pop[which(is.na(qmax_pop))]=0
      qmin_pop[which(is.na(qmin_pop))]=0
      mean_pop[which(is.na(mean_pop))]=0

      ymax=max(qmax_pop)
      xmax=length(qmax_pop)
      plot(0,col="white",xlim=c(0,xmax),ylim=c(0,ymax),xlab="",ylab="",axes=F,main=quale_cluster)
      lines(mean_pop,lwd=3,col=colore)
      poligono=cbind(x=c(1:length(mean_pop),length(mean_pop):1),
                     y=c(qmin_pop,rev(qmax_pop)))
      polygon(poligono,col=adjustcolor(colore,alpha=0.2),border=NA)
      axis(1,at=c(1,1+31,1+31+30,1+31+30+31,1+31+30+31+31,1+31+30+31+31+30),
           labels= c('Apr', 'May', 'Jun', 'Jul', 'Aug','Sept'),cex.axis=1.5,las=2)
      axis(2,at=seq(0,ymax,length.out=4),las=2,
           labels=format(seq(0,ymax,length.out=4),digits=2,scientific=T),cex.axis=1.5)
      if(what_plot == 'mosquito_prevalence')
        text = 'Prev in mosquitoes'
      if(what_plot == 'moquito_population')
        text = 'Mosquito pop'
      if(what_plot == 'bird_prevalence')
        text = 'Prev in birds'
      if(what_plot == 'bird_population')
        text = 'Bird pop'
      mtext(text,side=2,line=6,adj=0.5)
    }
    dev.off()
  }
}



CheckMCMC = function(InLoc = "Output_WNV/MCMC/",
                     FileName = "parametri_",
                     anno,
                     quale_cluster,

                     OnFile = T,
                     OutLoc = "Output_WNV/MCMC/Plots/",
                     OutFileName = "Check_Lik_",
                     con_cosa_inizio = 1){
  nomi_parametri=c("p","B0","pR","b1",# "b2",
                   "muB",
                   "s", "phi", "niB", "recB")


  if(con_cosa_inizio==0)
    nome_file_parametri=paste0(InLoc, FileName, "B_",
                               anno,"_",quale_cluster,".txt")
  if(con_cosa_inizio==1)
    nome_file_parametri=paste0(InLoc, FileName, "M_",
                               anno,"_",quale_cluster,".txt")

  output_mcmc=read.table(nome_file_parametri)
  if(ncol(output_mcmc)<=1)
    output_mcmc=matrix(0,ncol=length(nomi_parametri)+1,nrow=max_iter_MCMC)
  nomi_parametri_plot=c(nomi_parametri,"LogLik")

  nome_file = paste0(OutLoc, OutFileName, "_",
                     anno,"_",quale_cluster,".jpg")
  if(OnFile) {
    jpeg(nome_file,width=4*1200,height=1000*3,res=200)
    par(mfrow=c(3,4),cex.axis=1.5,mar=c(4,6,2,2),cex.lab=1.5,cex.main=1.5)

    for(j in 1:length(nomi_parametri_plot))
      plot(output_mcmc[,j],type="l",main=nomi_parametri_plot[j])
    dev.off()
  }

  par(mfrow=c(2,2))
  for(j in 1:length(nomi_parametri_plot))
    plot(output_mcmc[,j],type="l",main=nomi_parametri_plot[j])
}

