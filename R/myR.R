plotbox <- function(w, X, fun=mean, wd=0.1, col="gray", border="black",vertical=T,lwd=1){
  if(vertical){
    q25 <- quantile(X, 0.25, na.rm=TRUE)
    q75 <- quantile(X, 0.75, na.rm=TRUE)
    q025 <- quantile(X, 0.025, na.rm=TRUE)
    q975 <- quantile(X, 0.975, na.rm=TRUE)
    m <- fun(X, na.rm=TRUE)
    rect(w-wd,q25,w+wd,q75,col=col, border=border, lwd=lwd)
    lines(c(w,w),c(q75,q975), col=border, lwd=lwd) 
    lines(c(w,w),c(q25,q025), col=border, lwd=lwd)
    lines(c(w-wd,w+wd),c(q025,q025), col=border, lwd=lwd)
    lines(c(w-wd,w+wd),c(q975,q975), col=border, lwd=lwd)
    lines(c(w-wd,w+wd),c(m,m), col=border, lwd=lwd+1)
  }  
  else{
    q25 <- quantile(X, 0.25, na.rm=TRUE)
    q75 <- quantile(X, 0.75, na.rm=TRUE)
    q025 <- quantile(X, 0.025, na.rm=TRUE)
    q975 <- quantile(X, 0.975, na.rm=TRUE)
    m <- fun(X, na.rm=TRUE)
    rect(q25,w-wd,q75,w+wd,col=col, border=border, lwd=lwd)
    lines(c(q75,q975),c(w,w), col=border, lwd=lwd) 
    lines(c(q25,q025),c(w,w), col=border, lwd=lwd)
    lines(c(q025,q025),c(w-wd,w+wd), col=border, lwd=lwd)
    lines(c(q975,q975),c(w-wd,w+wd), col=border, lwd=lwd)
    lines(c(m,m),c(w-wd,w+wd), col=border, lwd=lwd+1)
  }
}

AIC_function=function(log10lik, numero_parametri){
  D=-2*log10lik/log10(exp(1))
  
  pD=2*numero_parametri
  
  return(pD+mean(D))
}