##Code for question 5

##import the dataset
virus <- read.csv("question-5-data/Cui_etal2014.csv")
nrow(virus) #give the number of rows of the virus dataset
ncol(virus) #give the number of columns of the virus dataset

##According to Cui et al., the equations describes the relationship between viral genome size and virion volume is:
## V = αL^β 
##V=volume (nm^3), L=genome size (kb)

#5b: The proper transformation should be log-transformation of both virion volumn and genome size.
#fit the linear model
virus_lm <- lm(log(Virion.volume..nm.nm.nm.)~log(Genome.length..kb.), virus)
summary(virus_lm)

##Calculate the value of α and β
###From the summary we know, log(α)=7.0748, β=1.5152###
###α=e^7.0748=1181.807###

##plot the model
ggplot(virus, aes(x = log(Genome.length..kb.), y = log(Virion.volume..nm.nm.nm.))) +
  geom_point() +  # Scatter plot
  geom_smooth(method = "lm", col = "blue") +  # Add regression line
  labs(title = "The relationship between virion genome length and viron volume", x = "log[Genome length(kb)", y = "log[Virion volumn (nm3)]")


##Estimated volume of given genome size (300kb)
V <- 1187.807*(300^1.5152)
V
