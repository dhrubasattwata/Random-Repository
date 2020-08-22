# Durbin-Wu-Hausman Test
# Edited from ivreg2 function publicly available over the internet
# Reject null hypothesis means b1 is not consistent
wuhausman <- function(form,endog,iv,data,digits=3){
  # model setup
  r1 <- lm(form,data)
  y <- r1$fitted.values+r1$resid
  x <- model.matrix(r1)
  aa <- rbind(endog == colnames(x),1:dim(x)[2])  
  z <- cbind(x[,aa[2,aa[1,]==0]],data[,iv]) 
  
  
  # iv coefficients and standard errors
  z <- as.matrix(z)
  pz <- z %*% (solve(crossprod(z))) %*% t(z)
  biv <- solve(crossprod(x,pz) %*% x) %*% (crossprod(x,pz) %*% y)
  sigiv <- crossprod((y - x %*% biv),(y - x %*% biv))/(length(y)-length(biv))
  vbiv <- as.numeric(sigiv)*solve(crossprod(x,pz) %*% x)
  res <- cbind(biv,sqrt(diag(vbiv)),biv/sqrt(diag(vbiv)),(1-pnorm(biv/sqrt(diag(vbiv))))*2)
  res <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),res)),nrow=dim(res)[1])
  rownames(res) <- colnames(x)
  colnames(res) <- c("Coef","S.E.","t-stat","p-val")
  
  # First-stage F-test
  y1 <- data[,endog]
  z1 <- x[,aa[2,aa[1,]==0]]
  bet1 <- solve(crossprod(z)) %*% crossprod(z,y1)
  bet2 <- solve(crossprod(z1)) %*% crossprod(z1,y1)
  rss1 <- sum((y1 - z %*% bet1)^2)
  rss2 <- sum((y1 - z1 %*% bet2)^2)
  p1 <- length(bet1)
  p2 <- length(bet2)
  n1 <- length(y)
  fs <- abs((rss2-rss1)/(p2-p1))/(rss1/(n1-p1))
  firststage <- c(fs)
  firststage <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),firststage)),ncol=length(firststage))
  colnames(firststage) <- c("First Stage F-test")
  
  # Hausman tests
  bols <- solve(crossprod(x)) %*% crossprod(x,y) 
  sigols <- crossprod((y - x %*% bols),(y - x %*% bols))/(length(y)-length(bols))
  vbols <- as.numeric(sigols)*solve(crossprod(x))
  sigml <- crossprod((y - x %*% bols),(y - x %*% bols))/(length(y))
  x1 <- x[,!(colnames(x) %in% "(Intercept)")]
  z1 <- z[,!(colnames(z) %in% "(Intercept)")]
  pz1 <- z1 %*% (solve(crossprod(z1))) %*% t(z1)
  biv1 <- biv[!(rownames(biv) %in% "(Intercept)"),]
  bols1 <- bols[!(rownames(bols) %in% "(Intercept)"),]
  
  # Durbin-Wu-Hausman chi-sq test:
  # haus <- t(biv1-bols1) %*% ginv(as.numeric(sigml)*(solve(crossprod(x1,pz1) %*% x1)-solve(crossprod(x1)))) %*% (biv1-bols1)
  # hpvl <- 1-pchisq(haus,df=1)
  # Wu-Hausman F test
  resids <- NULL
  resids <- cbind(resids,y1 - z %*% solve(crossprod(z)) %*% crossprod(z,y1))
  x2 <- cbind(x,resids)
  bet1 <- solve(crossprod(x2)) %*% crossprod(x2,y)
  bet2 <- solve(crossprod(x)) %*% crossprod(x,y)
  rss1 <- sum((y - x2 %*% bet1)^2)
  rss2 <- sum((y - x %*% bet2)^2)
  p1 <- length(bet1)
  p2 <- length(bet2)
  n1 <- length(y)
  fs <- abs((rss2-rss1)/(p2-p1))/(rss1/(n1-p1))
  fpval <- 1-pf(fs, p1-p2, n1-p1)
  #hawu <- c(haus,hpvl,fs,fpval)
  hawu <- c(fs,fpval)
  hawu <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),hawu)),ncol=length(hawu))
  #colnames(hawu) <- c("Durbin-Wu-Hausman chi-sq test","p-val","Wu-Hausman F-test","p-val")
  colnames(hawu) <- c("Wu-Hausman F-test","p-val")
  
  full <- list(results=res, endogeneity=hawu)
  return(full)
}
