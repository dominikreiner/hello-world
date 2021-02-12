library(tidyverse)
library(Rmisc)
library(nlrx)

results <- readRDS("resultschronicrange") 
repetitions <- 2
numberofvariablesarea <- 5
numberofruns <- nrow(results)/251 


####################################################################################################################################
### total population and foragers ##################################################################################################
####################################################################################################################################

pop <- results %>% select(ThresholdChronicDeath,step,TotalIHBees,TotalForagers)
pop$totalpopulation <- pop$TotalIHBees + pop$TotalForagers
pop <- gather(pop, PopulationType, numberbees, c("totalpopulation","TotalForagers"), factor_key=TRUE)
pop <- summarySE(pop, measurevar="numberbees", groupvars=c("ThresholdChronicDeath","PopulationType","step"))
pop$ThresholdChronicDeath <- as.factor(pop$ThresholdChronicDeath)
pop$PopulationType <- mapvalues(pop$PopulationType, from = c("totalpopulation","TotalForagers"), to = c("colony population","forager population"))
#pop$step <- rep(seq.Date(as.Date("2000/4/23"),as.Date("2000/12/29"),"day"),nrow(pop)/251)

ggplot(pop) + geom_point(aes(x=step,y=numberbees)) + scale_shape_manual(values=c(15,16,17,1))+
  xlab("Date") + ylab("Number of bees") + labs(color="Pesticide scenario",shape="Pesticide scenario") +
  geom_errorbar(aes(x = step,ymin=numberbees-se, ymax=numberbees+se), width=.1) +
  #scale_x_date(breaks = as.Date(c("2000-04-24", "2000-05-01","2000-05-08","2000-05-15")), labels = c("Apr 24", "May 01", "May 08", "May 15")) +
  facet_wrap(vars(ThresholdChronicDeath,PopulationType),nrow = 5, scales = "free_y")


