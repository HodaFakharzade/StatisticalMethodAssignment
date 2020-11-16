#4.84
library(dplyr)
library(ggplot2)

ex.4.83 <- function(){
  str <- c("\U03B1","\U03B2","\U03B6","\U03B4","\U03C0","\U03A8")
  alpha <-c(4,40,80)
  beta <- c(1,1,1)
  ave <- alpha*beta
  std.dv <- sqrt(alpha*beta^2)
  x <- seq(0,ave[3] + 5*std.dv[3], 0.01)
  y = dgamma(x, alpha[1], rate = 1/beta)

  plot(x, y, type = "l",ylim = c(0,max(y)+0.1),col="#009999",ylab="Density",main="Gamma Densities: alpha = 4, 40, 80")
  lines(x, dgamma(x,  alpha[2], 1), col="blue")
  lines(x, dgamma(x,  alpha[3], 1), col="red")
  legend("topright",
         c("alpha = 4","alpha = 40", "alpha = 80"),
         fill=c("#009999","blue","red")
  )
  grid()

}

#4.117
ex.4.117 <- function(){
  library(tidyr)
  library(ggplot2)
  str <- c("\U03B1","\U03B2","\U03B6","\U03B4","\U03C0","\U03A8")


  set.seed(1492) # reproducible

  x <- seq(0, 1, .01)

  alpha_1 <- dbeta(x, 9, 7)
  alpha_2 <- dbeta(x, 10, 7)
  alpha_3 <- dbeta(x, 12, 7)

  df <- data.frame(x, alpha_1, alpha_2, alpha_3)

  df <- gather(df, func, val, -x)
  gg <- ggplot(df, aes(x=x, y=val, group=func))
  gg <- gg + geom_line(aes(color=func))
  gg <- gg + scale_y_continuous(expand=c(0, 0))
  gg <- gg + scale_color_manual(name="Beta params",
                                values=c("green", "blue","red"),
                                labels=c("\U03B1=9, \U03B2=7", "\U03B1=10,\U03B2=7", "\U03B1=12, \U03B2=7"))
  gg <- gg + labs(x="x value", y="PDF",
                  title="Beta Probability Distribution Functions")
  gg <- gg + theme_bw()
  gg <- gg + theme(panel.border=element_blank())
  gg <- gg + theme(axis.line=element_line(size=0.15, color="#2b2b2b"))
  gg
}

#4.118
ex.4.118 <- function(){
  t <- c("alpha = 0.3, beta = 4"=dbeta(0.2,0.3,4),"alpha = 0.3, beta = 7"=dbeta(0.2,0.3,7),"alpha = 0.3, beta = 12"=dbeta(0.2,0.3,12))
  res<- names(which.max(t))
  paste("at x = 0.2 the highest probability belongs to ",res)

  library(tidyr)
  library(ggplot2)

  set.seed(1492) # reproducible

  x <- seq(0, 1, .01)

  alpha_1 <- dbeta(x, 0.3, 4)
  alpha_2 <- dbeta(x, 0.3, 7)
  alpha_3 <- dbeta(x, 0.3, 12)

  df <- data.frame(x, alpha_1, alpha_2, alpha_3)

  df <- gather(df, func, val, -x)
  gg <- ggplot(df, aes(x=x, y=val, group=func))
  gg <- gg + geom_line(aes(color=func))
  gg <- gg + scale_y_continuous(expand=c(0, 0))
  gg <- gg + scale_color_manual(name="Beta params",
                                values=c("green", "blue","red"),
                                labels=c("\U03B1=0.3, \U03B2=4", "\U03B1=0.3, \U03B2=7", "\U03B1=0.3,\U03B2=12"))
  gg <- gg + labs(x="x value", y="PDF",
                  title="Beta Probability Distribution Functions")
  gg <- gg + theme_bw()
  gg <- gg + theme(panel.border=element_blank())
  gg <- gg + theme(axis.line=element_line(size=0.15, color="#2b2b2b"))
  gg


}

#10.19
ex.10.19 <- function(){
  mu <- 130
  n <-  40
  Y_bar <- 128.6
  sd <- 2.1

  z.test <- function(Y_bar, mu, sd,n){
    zeta <- (Y_bar - mu) / (sd/sqrt(n))
    return(zeta)

  }

  # alpha = 0.05 (two-sided test)
  alpha <- 0.05
  # z_0.05 =1.645
  z_0.05 <- 1.645
  if (abs(z.test(Y_bar,mu,sd,n)) > z_0.05){
    print(paste0(abs(z.test(Y_bar,mu,sd,n))," " ,"exceeds than z_0.05 =1.645"))
    print("there fore the null hypothesis is rejected and the mean output voltage is less than 130")

  } else{
    print(" the mean output voltage might be higer than 130")
  }

}

ex.10.21 <- function(){
  soil_type_1 <- c(n =30,Y_s= 1.65, sd = 0.26)
  soil_type_2 <- c(n=35,Y_s = 1.43, sd = 0.22)

  #We are testing H0 : ??1 ??? ??2 = 0 vs. H0 : ??1 ??? ??2 != 0
  mu <- 0
  Y_bar <- as.numeric(soil_type_1[2] - soil_type_2[2])
  d <- sqrt((soil_type_1["sd"]^2 /soil_type_1["n"]) + (soil_type_2["sd"]^2 /soil_type_2["n"]))
  Z <- as.numeric((Y_bar - mu)/d)
  z.005 <- 2.575 #  Reject Region = {| z |> z.005 = 2.575}
  if (abs(Z) > z.005 ){
    print(paste0(" the test statistic = ", Z, " ", "is greater than ", " ", z.005))
    print("The soils differ with respect to average shear strength at the 1% confidence level!")
  } else {
    print("the soils are the same!")
  }

}


#11.31
ex.11.31 <- function(){
  x <- c(19.1, 38.2, 57.3, 76.2, 95, 114, 131, 150, 170)
  y <- c(.095, .174, .256, .348, .429, .500, .580, .651, .722)
  # plot(lm(y~x))
  summary(lm(y~x))
  ### Plot the regression line

  with(lm(y~x), plot(x = x, y = y))
  abline(lm(y~x),col = "blue")

}

#11.69
ex.11.69 <- function(){
  x <- c(-7, -5, -3, -1, 1, 3, 5, 7)
  y <- c(18.5,22.6,27.2,31.2,33.0,44.9,49.4,35.0)
  summary(lm(y~x))
  lm(formula = y ~ x)
  with(lm(y~x), plot(x = x, y = y))
  abline(lm(y~x),col = "blue")
  summary(lm(y~x+I(x^2)))


}

