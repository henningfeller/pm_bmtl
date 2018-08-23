build_population_table <- function(year = "2018", variant = "1") {
  # general demographic data for Germany from
  # https://www.destatis.de/DE/ZahlenFakten/GesellschaftStaat/Bevoelkerung/Bevoelkerung.html 
  bev <- read.csv("./_data/bev.csv", sep = ";")
  bev <- bev[bev$Simulationsjahr == year,]
  bev <- bev[bev$Variante == variant,]
  
  bev <- subset(bev, select = c(-Variante, -Simulationsjahr, -Median, -Bev))
  
  bev <- cbind(rep("population", times = 2), bev)
  colnames(bev) <- c("indicator", "mw", 0:99)
  # bev
  # 
  # plot(0:99, bev[1,3:102], type = "l", col = "blue")
  # points(0:99, bev[2,3:102], type = "l", col = "red")
  
  # some information on diabetes prevalance from
  # https://www.rki.de/SharedDocs/Bilder/GBE/Gesundheitsthemen/Infografik_Diabetes_2016.gif?__blob=normal&v=5
  # (these information are tough to get and nail down, so I'm happy I found something...
  # ... after all we should show algorithm knowledge, so the numbers don't really matter)
  levels(bev$indicator) <- c("population", "diabetesShare", "diabetesTotal")
  bev[3,] <-  c("diabetesShare", "m",
                rnorm(40,0.01,0.001),
                rnorm(10,0.02,0.002),
                rnorm(10,0.075,0.007),
                rnorm(10,0.17,0.01),
                rnorm(30,0.22,0.01))
  
  bev[4,] <-  c("diabetesShare", "w",
                rnorm(20,0.02,0.002),
                rnorm(20,0.035,0.0035),
                rnorm(10,0.045,0.0045),
                rnorm(10,0.04,0.004),
                rnorm(10,0.11,0.01),
                rnorm(30,0.22,0.01))
  
  bev[5,1:2] <- c("diabetesTotal", "m")
  bev[6,1:2] <- c("diabetesTotal", "w")
  
  for (i in 3:102) {
    bev[,i] <- as.numeric(bev[,i])
    bev[5,i] <- bev[1,i]*bev[3,i]	
    bev[6,i] <- bev[2,i]*bev[4,i]	
  }
  
  return(bev)
}

# rejection sampling function
r_ages <- function(n, 
                   age.distr,
                   beta.shape1 = 1,
                   beta.shape2 = 1,
                   c = max(age.distr)*1.1) {
  
  if (!(is.vector(age.distr) & is.numeric(age.distr))) {
    stop("Parameter for Age Distribution is not a numeric vector")
  }
  
  if (is.null(names(age.distr))) {
    stop("Names of Age Distribution vector must be ages")
  }
  
  age.rand <- c()
  age.max <- max( as.numeric( names(age.distr) ) )
  while (length(age.rand) < n) {
    beta <- rbeta(1, beta.shape1, beta.shape2)
    upper_border <- c*dbeta(beta, beta.shape1, beta.shape2)
    
    unif <- runif(1, 0, upper_border)
    age <- round(beta*100)
    age <- ifelse(age > age.max, age.max, age)
    if (unif < age.distr[paste(age)]) {
      # accept
      age.rand <- c(age.rand, age)
    }
  }
  return(age.rand)
}

r_gender <- function(n, p1 = 0.5, values = c("m","w")) {
  g.rand <- sample(values, n, replace=T, prob = c(p1,1-p1)) # Changed this because the package with the rbern function was not available for latest versions of R for 
  return(g.rand)
}

r_body <- function(gender, age) {
  # https://www.destatis.de/DE/ZahlenFakten/GesellschaftStaat/Gesundheit/GesundheitszustandRelevantesVerhalten/Tabellen/Koerpermasse.html
  if (!((gender == "m") | (gender == "w")| (gender == 1)| (gender == -1))) {
    stop("gender must be 'm','w','1' or '-1'")
  }
  
  if ((gender == "m") | (gender == 1)) {
    # Height
    height.mean <- ifelse(age <= 19,
                          0.5+0.0689*age,
                          1.81-0.00145*age)
    
    height <- round(rnorm(1, height.mean, 0.075), 2)
    
    
    #Body Mass Index
    # (please never ask, how I came up with shape numbers)
    beta1 = 0.05454*age+2.9090
    beta2 = -0.0727*age+30.454
    
    bmi <- round(rbeta(1, beta1, beta2)*50+16, 1)
    
    weight <- round(bmi*height^2, 1)
  } else {
    height.mean <- ifelse(age <= 19,
                          0.5+0.0568*age,
                          1.70-0.00109*age)
    
    height <- round(rnorm(1, height.mean, 0.075),2)
    
    beta1 <- 0.05818*age+2.3363
    beta2 <- -0.0727*age+31.454
    
    bmi <- round(rbeta(1, beta1, beta2)*50+16, 1)
    weight <- round(bmi*height^2, 1)
  }
  return(c(height, weight, bmi))
}

r_smoking <- function(gender, age) {
  # https://www.rauchfrei-info.de/informieren/verbreitung-des-rauchens/raucherquote-bei-erwachsenen/ 
  values <- c("Yes","No")
  # set probability
  if ((gender == 1) | (gender == "m")) {
    prob <- ifelse(age < 12, 0,
                   ifelse(age < 18, 0.075,
                          ifelse(age <= 20, 0.261,
                                 ifelse(age <= 24, 0.362,
                                        ifelse(age <=29, 0.319,
                                               ifelse(age <=39, 0.35,
                                                      ifelse(age <= 49, 0.321,
                                                             ifelse(age <= 59, 0.303,
                                                                    ifelse(age <= 64, 0.22,
                                                                           0.125))))))))) # kids, don't do this at home
  } else if ((gender == -1) | (gender == "w")) {
    prob <- ifelse(age < 12, 0,
                   ifelse(age < 18, 0.05,
                          ifelse(age <= 20, 0.187,
                                 ifelse(age <= 24, 0.234,
                                        ifelse(age <=29, 0.341,
                                               ifelse(age <=39, 0.274,
                                                      ifelse(age <= 49, 0.282,
                                                             ifelse(age <= 59, 0.248,
                                                                    ifelse(age <= 64, 0.194,
                                                                           0.115)))))))))
  } else {
    warning("specify gender as 1/'m' or -1/''w'")
  }	
  
  return(sample(values, 1, replace=T, prob = c(prob, 1-prob)))
}