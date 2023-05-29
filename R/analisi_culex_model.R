#### codice per analizzare modello con 2 carrying capacities
#### beta=4.61, alpha=0.108 da Li et al. 2016
#### uso le 70 trappole sempre attive tra 2013 e 2018

# graphics.off()
# rm(list = ls())

MCMCFunc <- function(numero_cluster,
                     anno_inizio,
                     anno_fine,

                     nomi_parametri=c("Carrying capacity1",
                                      "Carrying capacity2",
                                      "Initial adults"),


                     max_iter_MCMC=10000,
                     sigma_big=5,
                     sigma_small=0.01,

                     seed=0,
                     file_temperatura="per_mcmc/temperatura_media_",
                     file_catture="per_mcmc/catture_medie_cluster_",
                     file_trappole="per_mcmc/trappole_attive_cluster_",
                     vettore_date_catture="per_mcmc/giorni_cattura.txt",
                       file_luce="per_mcmc/light_minutes.txt",

                     OutLoc = 'Output/MCMC',
                     FileAllParmsName="parametri_",
                     FileParmsPerSimuName="per_simulazione_",

                     to_plot=T,
                     to_mcmc=T,
                     to_simulate=T,
                     to_write=T
) {
  length(scan(vettore_date_catture,quiet=T))
  cluster_simulati=numero_cluster

  if(to_mcmc)
    system("gcc -o culex_mcmc codice/V1/2K_alphafix_betafix_constPerLik/mcmc_culex_main.c codice/V1/2K_alphafix_betafix_constPerLik/culex_func.c -lgsl -lgslcblas -lm")
  if(to_simulate)
    system("gcc -o culex_simulation codice/V1/2K_alphafix_betafix_constPerLik/culex_simulation.c codice/V1/2K_alphafix_betafix_constPerLik/culex_func.c -lgsl -lgslcblas -lm")

  #### MCMC #####
  for(anno in anno_inizio:anno_fine){
    print(paste("Simulo anno",anno))
    for(quale_cluster in 1:numero_cluster){
      print(paste("Cluster",quale_cluster))
      nome_file_parametri=paste0(OutLoc, "/",
                                 FileAllParmsName, anno, "_" ,quale_cluster,
                                 ".txt")

      if(to_mcmc){
        to_terminal=paste("time ./culex_mcmc",seed,file_temperatura,file_catture,file_trappole,numero_cluster,
                          anno,anno,vettore_date_catture,numero_catture,nome_file_parametri,
                          max_iter_MCMC,sigma_big,sigma_small,file_luce,solo_un_cluster,quale_cluster-1)
        system(to_terminal)
      }

      output_mcmc=read.table(nome_file_parametri)
      if(ncol(output_mcmc)<=1)
        output_mcmc=matrix(0,ncol=length(nomi_parametri)+1,nrow=max_iter_MCMC)
      # plot(output_mcmc[,1],type="l",main="Capture rate")
      # plot(output_mcmc[,2],type="l",main="beta")
      # for(i in 3:(3+cluster_simulati-1))
      #   plot(output_mcmc[,i],type="l",main=paste("K",i-2))
      # for(i in (cluster_simulati+3):(ncol(output_mcmc)-1))
      #   plot(output_mcmc[,i],type="l",main=paste("A0",i-2))
      # plot(output_mcmc[,ncol(output_mcmc)],type="l",main="LogLik")
      burnin=nrow(output_mcmc)*0.1
      # plot(output_mcmc[-c(1:burnin),ncol(output_mcmc)],type="l",main="LogLik")

      # parametri_per_simulazione=paste0("Output/MCMC/per_simulazione_",anno,"_WNV5reg_alphaFix_constPerLik_70trappole_",quale_cluster,".txt")
      parametri_per_simulazione=paste0(OutLoc, "/",
                                       FileParmsPerSimuName, anno, "_" , quale_cluster,
                                 ".txt")

      if(to_write)
        write.table(output_mcmc[-c(1:burnin),-ncol(output_mcmc)],
                    row.names=F,col.names=F,file=parametri_per_simulazione)

      }
  }
  }

PlotFunc <- function(anno_inizio,
                     anno_fine,
                     numero_cluster,
                     nomi_parametri=c("Carrying capacity1",
                                      "Carrying capacity2",
                                      "Initial adults"),
                     OutLoc = 'Output/MCMC/Plots',
                     FileAllParmsName="parametri_",
                     FileParmsPerSimuName="per_simulazione_"

) {
  # source("myR.R")

  for(anno in anno_inizio:anno_fine){

    parametri_stimati=vector("list", length(nomi_parametri))

    for(quale_cluster in 1:numero_cluster){
      #nome_file_parametri=paste0("Output/MCMC/parametri_2K_",anno,"_WNV5reg_alphaFix_constPerLik_70trappole_",quale_cluster,".txt")
      nome_file_parametri=paste0(OutLoc, "/",
                                 FileAllParmsName, anno, "_" ,quale_cluster,
                                 ".txt")
      output_mcmc=read.table(nome_file_parametri)
      if(ncol(output_mcmc)<=1)
        output_mcmc=matrix(0,ncol=length(nomi_parametri)+1,nrow=max_iter_MCMC)
      for(j in 1:(ncol(output_mcmc)-1))
        parametri_stimati[[j]]=cbind(parametri_stimati[[j]],output_mcmc[-c(1:burnin),j])
    }

    #nome_file=paste0("Output/MCMC/Plots/5reg/parametri_2K_WNV5reg_alphabetaFix_constPerLik_70trappole_",anno,".jpg")
    nome_file=paste0(OutLoc, "/",
                     FileAllParmsName, anno, "_" ,quale_cluster,
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

PlotCheckK = function(anno_inizio,
                      anno_fine,
                      numero_cluster,
                      OutLoc = 'Output/MCMC/Plots',
                      FilePlotName="confronto_carrying_capacity_70trappole",
                      nomi_parametri=c("Carrying capacity1",
                                       "Carrying capacity2",
                                       "Initial adults"),
                      FileAllParmsName="parametri_"



) {
  ### check carrying capacity
  # source("../myR.R")
  library(RColorBrewer)
  colori=brewer.pal(numero_cluster,"Set1")

  nome_file=paste0(OutLoc, "/", FilePlotName, ".jpg")
  jpeg(nome_file,width=3*1200,height=2*1000,res=200)
  par(mfrow=c(2,3),cex.axis=2,mar=c(4,8,4,2),cex.lab=2,cex.main=2)

  for(anno in anno_inizio:anno_fine){

    parametri_stimati=vector("list", length(nomi_parametri))

    for(quale_cluster in 1:numero_cluster){
      # nome_file_parametri=paste0("Output/MCMC/parametri_2K_",anno,"_WNV5reg_alphaFix_constPerLik_70trappole_",quale_cluster,".txt")
      nome_file_parametri=paste0(OutLoc, "/",
                                 FileAllParmsName, anno, "_" ,quale_cluster,
                                 ".txt")
      output_mcmc=read.table(nome_file_parametri)
      burnin=nrow(output_mcmc)*0.1
      if(ncol(output_mcmc)<=1)
        output_mcmc=matrix(0,ncol=length(nomi_parametri)+1,nrow=max_iter_MCMC)
      for(j in 1:(ncol(output_mcmc)-1))
        parametri_stimati[[j]]=cbind(parametri_stimati[[j]],output_mcmc[-c(1:burnin),j])
    }

    i=1
    tab=parametri_stimati[[i]]
    i=2
    tab2=parametri_stimati[[i]]
    ymax=0
    for(j in 1:ncol(tab))
      if(quantile(tab[,j],probs=0.975)>ymax)
        ymax=quantile(tab[,j],probs=0.975)
    ymin=ymax
    for(j in 1:ncol(tab2))
      if(quantile(tab2[,j],probs=0.025)<ymin)
        ymin=quantile(tab2[,j],probs=0.025)
    plot(0,col="white",xlim=c(0,ncol(tab)+1),ylim=c(ymin,ymax),main=anno,
         axes=F,xlab="Cluster",ylab="")
    for(j in 1:ncol(tab)){
      plotbox(w=j-0.1,X=tab[,j],fun=median,col=colori[j])
      plotbox(w=j+0.1,X=tab2[,j],fun=median,col=colori[j])
      points(x=j+0.1,y=max(tab2[,j]),pch="*",cex=3)
    }
    axis(2,at=seq(ymin,ymax,length.out=4),las=2,
         labels=format(seq(ymin,ymax,length.out=4),scientific=T,digits=2))
    axis(1,at=c(1:ncol(tab)),labels=LETTERS[1:numero_cluster])
  }

}


SimuFunc = function(anno_inizio,
                    anno_fine,
                    numero_cluster,

                    numero_simulazioni=200,

                    OutLoc = 'Output/Simulazioni',
                    PlotLoc = 'Output/Simulazioni/Plots',
                    FileParmsLoc = 'Output/MCMC',
                    Filename = 'catture_simulate_',
                    FileParmsPerSimuName="per_simulazione_",

                    FileDynName = "dynamics_",
                    FileCaptName = "catture_",

                    seed=0,
                    file_temperatura="per_mcmc/temperatura_media_",
                    file_catture="per_mcmc/catture_medie_cluster_",
                    file_trappole="per_mcmc/trappole_attive_cluster_",
                    vettore_date_catture="per_mcmc/giorni_cattura_cluster.txt",
                    file_luce="per_mcmc/light_minutes.txt"
                    ){
  #### Simulazioni ####
  settimane=scan(vettore_date_catture)
  # settimane=settimane[-c(20:22)]

  cor_test_tab=c()
  pvalue_cor_test_tab=c()

  for(anno in anno_inizio:anno_fine){
    print(paste("Simulo anno",anno))
    if(to_plot){
      nome_file=paste0(OutLoc, "/", Filename, "_", anno, ".jpg")
      jpeg(nome_file,width=numero_cluster*1200,height=1000,res=200)
      # par(mfrow=c(1,numero_cluster),mar=c(5,8,2,2),cex.lab=1.5,cex.main=2,cex.axis=2.5)
      par(mfrow=c(1,numero_cluster),mar=c(5,10,2,2),cex.lab=1.5,cex.main=2,cex.axis=2.5)
    }
    cor_test_tmp=c()
    for(quale_cluster in 1:numero_cluster){
      parametri_per_simulazione=paste0(FileParmsLoc, "/",
                                       FileParmsPerSimuName, anno,"_",quale_cluster,".txt")
      nome_file_output_dynamics=paste0(OutLoc, "/",
                                       FileDynName, anno, "_", quale_cluster, ".txt")
      nome_file_output_catture=paste0(OutLoc, "/",
                                      FileCaptName, anno, "_", quale_cluster, ".txt")
      ncol_file_parametri=ncol(read.table(parametri_per_simulazione))
      nrow_file_parametri=nrow(read.table(parametri_per_simulazione))

      to_terminal=paste("time ./culex_simulation",seed,file_temperatura,file_catture,file_trappole,numero_cluster,
                          anno,anno,vettore_date_catture,numero_catture,nome_file_output_dynamics,
                          nome_file_output_catture,numero_simulazioni,
                          parametri_per_simulazione,ncol_file_parametri,nrow_file_parametri,file_luce,solo_un_cluster,
                          quale_cluster-1)
      system(to_terminal)

      catture_simulate=read.table(nome_file_output_catture)

      anni_totali=1
      catture_osservate=read.table(paste0(file_catture,anno))
      trappole_attive=read.table(paste0(file_trappole,anno))

      simu_sel=catture_simulate
      qmin=rep(NA,length(settimane))
      qmax=rep(NA,length(settimane))
      media=rep(NA,length(settimane))
      for(j in 1:ncol(simu_sel)){
        if(trappole_attive[quale_cluster,j]==1){
          qmin[j]=quantile(simu_sel[,j],probs=0.025)
          qmax[j]=quantile(simu_sel[,j],probs=0.975)
          media[j]=mean(simu_sel[,j])
        }
      }
      ymax=max(unlist(c(qmax,catture_osservate[quale_cluster,])),na.rm=T)
      ymin=0
      xmin=90
      xmax=90+180
      plot(0,axes=F,col="white",ylim=c(ymin,ymax),xlim=c(xmin,xmax),ylab="",xlab="",
           main=quale_cluster)
      quando_trappolo=which(trappole_attive[quale_cluster,]==1)
      poligono=cbind(x=c(settimane[quando_trappolo],rev(settimane[quando_trappolo])),
                     y=c(qmin[quando_trappolo],rev(qmax[quando_trappolo])))
      polygon(poligono,col=adjustcolor("darkorange",alpha=0.5),border=NA)
      lines(settimane[quando_trappolo],media[quando_trappolo],lwd=3,col="darkorange")
      test=cor.test(as.numeric(catture_osservate[quale_cluster,quando_trappolo]),media[quando_trappolo])
      cor_test_tmp=c(cor_test_tmp,test$estimate)
      for(j in 1:ncol(simu_sel)){
        if(trappole_attive[quale_cluster,j]==1){
          # plotbox(w=settimane[j],X=simu_sel[,j],fun=median,wd=2)
          points(settimane[j],catture_osservate[quale_cluster,j],pch=19,col="darkgreen",cex=2)
        }
      }
      # axis(1,at=seq(xmin,xmax,28),labels=seq(xmin,xmax,28)/7)
      axis(1,at=seq(xmin,xmax,60),labels=month.abb[seq(4,10,2)])
      axis(2,at=round(seq(ymin,ymax,length.out=4)),las=2)
      mtext(side=2,text="Captured Cx. pipiens",line=7.5,cex=1.75)
      mtext(side=1,text="Week",line=3.5,cex=1.75)
    }
    cor_test_tab=rbind(cor_test_tab,cor_test_tmp)
    if(to_plot)
      dev.off()
  }
}


DynFunc <- function(anno_inizio,
                    anno_fine,
                    numero_cluster,

                    numero_classi_popolazione = 4,
                    classe_adulti=4,

                    nome_classi = c("E","L","P","A"),
                    OutLoc = "Output/Simulazioni",
                    OutZanzMedieLoc = "../Modello WNV/per_mcmc/zanzare",
                    FileDynName = "dynamics_",
                    FileZanzMedieName = " adulti_medi_",
                    PlotOutLoc = "Output/Simulazioni/Plots",
                    PlotDynName = "dinamica_adulti_") {
  message('adults dynamics')
  ##### dinamica solo adulti
  for(anno in anno_inizio:anno_fine){
    print(paste("Anno",anno))

    nome_file=paste0(PlotOutLoc, "/",
                     PlotDynName, anno, ".jpg")
    jpeg(nome_file,width=numero_cluster*1200,height=1000,res=200)
    par(mfrow=c(1,numero_cluster),mar=c(5,10,2,2),cex.lab=1.5,cex.main=2,cex.axis=2.5)

    for(quale_cluster in 1:numero_cluster){
      nome_file_output_dynamics=paste0(OutLoc, "/",
                                       FileDynName, anno, "_", quale_cluster, ".txt")

      dinamica_simulata=read.table(nome_file_output_dynamics)

      classe=4

      simu_sel=dinamica_simulata[seq(classe,nrow(dinamica_simulata),
                                     numero_classi_popolazione),1:180]
      qmin=rep(NA,ncol(simu_sel))
      qmax=rep(NA,ncol(simu_sel))
      media=rep(NA,ncol(simu_sel))
      for(j in 1:ncol(simu_sel)){
        qmin[j]=quantile(simu_sel[,j],probs=0.025)
        qmax[j]=quantile(simu_sel[,j],probs=0.975)
        media[j]=mean(simu_sel[,j])
      }
      ymax=max(qmax,na.rm=T)

      ymin=0
      xmin=0
      xmax=length(media)
      plot(0,axes=F,col="white",ylim=c(ymin,ymax),xlim=c(xmin,xmax),ylab="",xlab="",
           main=quale_cluster)
      poligono=cbind(x=c(1:length(qmin),length(qmin):1),
                     y=c(qmin,rev(qmax)))
      polygon(poligono,col=adjustcolor("gray",alpha=0.5),border=NA)
      lines(media,lwd=3)
      axis(1,at=seq(xmin,xmax,60),labels=month.abb[seq(4,10,2)])
      axis(2,at=round(seq(ymin,ymax,length.out=4)),las=2,
           labels=format(round(seq(ymin,ymax,length.out=4)),scientific=T,digits=1))
      mtext(side=2,text="Cx. pipiens females",line=7.5,cex=2)
    }
  }

  ##### salvo adulti per modello WNV
  for(anno in anno_inizio:anno_fine){
    print(paste("Salvo adulti per modello epi", anno))
    adulti_medi=c()
    for(quale_cluster in 1:numero_cluster){
      nome_file_output_dynamics=paste0(OutLoc, "/",
                                       FileDynName, anno, "_", quale_cluster, ".txt")

      dinamica_simulata=read.table(nome_file_output_dynamics)

      simu_sel=dinamica_simulata[seq(classe_adulti,
                                     nrow(dinamica_simulata),
                                     numero_classi_popolazione),]
      media=rep(NA,ncol(simu_sel))
      for(j in 1:ncol(simu_sel))
        media[j]=round(mean(simu_sel[,j]))
      adulti_medi=rbind(adulti_medi,media)
    }

    nome_file=paste0(OutZanzMedieLoc, "/",
                     FileZanzMedieName,
                     "_", anno)
    if(to_write)
      write.table(adulti_medi,file=nome_file,row.names=F,col.names=F)
  }

}




