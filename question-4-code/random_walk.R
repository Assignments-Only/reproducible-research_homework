#install.packages("ggplot2")
#install.packages("gridExtra")

#Packages installed via the Packages panel

library(ggplot2)
library(gridExtra)

random_walk  <- function (n_steps, seed = NULL) {
  
  if (!is.null(seed)) {
    set.seed(seed)
  } #set the random seed in the function for reproducible Brownian motion
  
  df <- data.frame(x = rep(NA, n_steps), y = rep(NA, n_steps), time = 1:n_steps)
  
  df[1,] <- c(0,0,1)
  
  for (i in 2:n_steps) {
    
    h <- 0.25
    
    angle <- runif(1, min = 0, max = 2*pi)
    
    df[i,1] <- df[i-1,1] + cos(angle)*h
    
    df[i,2] <- df[i-1,2] + sin(angle)*h
    
    df[i,3] <- i
    
  }
  
  return(df)
  
}


##enter the value of seed
##Here I chose 30
data1 <- random_walk(500, seed = 30)

plot1 <- ggplot(aes(x = x, y = y), data = data1) +
  
  geom_path(aes(colour = time)) +
  
  theme_bw() +
  
  xlab("x-coordinate") +
  
  ylab("y-coordinate")

data2 <- random_walk(500, seed = 30)

plot2 <- ggplot(aes(x = x, y = y), data = data2) +
  
  geom_path(aes(colour = time)) +
  
  theme_bw() +
  
  xlab("x-coordinate") +
  
  ylab("y-coordinate")

grid.arrange(plot1, plot2, ncol=2)


##save the package used in the code
sink(file = "Q4-package-versions.txt")
sessionInfo()
sink()
