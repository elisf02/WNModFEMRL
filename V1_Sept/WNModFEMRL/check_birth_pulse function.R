#funzione per sistemare check_birth_pulse function
check_birth_pulse2 = function (parms, tmin = 90, tmax = 210, quante_specie = 2) 
{
  if (is.numeric(parms)) {
    with(as.list(parms), plot(muB * exp(-s * sin(pi * (seq(0, 
                                                           365)/365 - phi[1]))^2)/besselI(s/2, 0), type = "l", 
                              lwd = 3, col = "black"), xlab = "time", ylab = "BIRTH_PULSE")
    abline(v = c(tmin, tmax), lwd = 2, col = "yellow")
  }
  if (is.character(parms)) {
    if (quante_specie == 1) 
      nomi_parametri = c("p", "B0", "pR", "b1", "muB", 
                         "s", "phi", "niB", "recB")
    if (quante_specie == 2) 
      nomi_parametri = c("p", "p1", "B0", "B01", "pR", 
                         "pR1", "b1", "muB", "s", "phi", "niB", "recB")
    parametri_stimati = vector("list", length(nomi_parametri))
    matrix_birth_pulse = c()
    output_mcmc = read.table(parms)
    if (ncol(output_mcmc) <= 1) 
      output_mcmc = matrix(0, ncol = length(nomi_parametri) + 
                             1, nrow = max_iter_MCMC)
    burnin = nrow(output_mcmc) * 0.1
    for (j in 1:(ncol(output_mcmc) - 1)) parametri_stimati[[j]] = cbind(parametri_stimati[[j]], 
                                                                        output_mcmc[-c(1:burnin), j])
    for (j in which(nomi_parametri %in% c("muB", "phi", "s"))) matrix_birth_pulse = cbind(matrix_birth_pulse, 
                                                                                          parametri_stimati[[j]])
    colnames(matrix_birth_pulse) = c("muB", "s", "phi")
    birth_pulse = apply(matrix_birth_pulse, MARGIN = 1, function(a) {
      a["muB"] * exp(-a["s"] * sin(pi * (seq(0, 365)/365 - 
                                           a["phi"]))^2)/besselI(a["s"]/2, 0)
    })
    #matrix_birth_pulse[100, ]
    #plot(birth_pulse[, 100], type = "l")
    mean = apply(birth_pulse, MARGIN = 1, function(a) {
      mean(a, na.rm = T)
    })
    qmin = apply(birth_pulse, MARGIN = 1, function(a) {
      quantile(a, probs = 0.025, na.rm = T)
    })
    qmax = apply(birth_pulse, MARGIN = 1, function(a) {
      quantile(a, probs = 0.975, na.rm = T)
    })
    ymax = max(qmax, qmin, mean)
    plot(0, col = "white", xlim = c(0, 366), ylim = c(0, 
                                                      ymax), xlab = "time", ylab = "birth_pulse", axes = T)
    abline(v = c(tmin, tmax), lwd = 2, col = "yellow")
    lines(mean, lwd = 3, col = "black")
    poligono = cbind(x = c(1:length(mean), length(mean):1), 
                     y = c(qmin, rev(qmax)))
    polygon(poligono, col = adjustcolor("grey", alpha = 0.2), 
            border = NA)
  }
}
