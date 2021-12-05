library(ggplot2)
library(ggthemes)
library(tidyverse)
library(dplyr)
library(lubridate)



pop1 <- read_csv("/Users/StephanieMeltzer/Desktop/pop.csv")
glimpse(pop1)
summary(pop1)

#changing to factors
pop1$breedid <- as.factor(pop1$breedid)
summary(pop1)
pop1$newgroup <- as.factor(pop1$newgroup)
pop1$sex <- as.factor(pop1$sex)
pop1$isneutered <- as.factor(pop1$isneutered)
pop1$whereacquired <- as.factor(pop1$whereacquired)
pop1$country <- as.factor(pop1$country)
pop1$aschild <- as.factor(pop1$aschild)
pop1$otherdogs <- as.factor(pop1$otherdogs)
summary(pop1)
pop1$healthproblems <- as.factor(pop1$healthproblems)
pop1$aschild <- as.factor(pop1$aschild)
pop1$top5 <- as.factor(pop1$top5)


#viz 1
qplot(factor(whereacquired), data=pop1, geom="bar",
      fill=factor(whereacquired), xlab="Where pet was acquired")


pop1 %>%
  mutate(name = fct_reorder(name, val)) %>%
  ggplot( aes(x=name, y=val)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

#VIZ1 barplot
bar <- pop1 %>% ggplot(aes(y=train, x=breedid, fill=breedid)) + geom_boxplot()
bar + theme(
  plot.title = element_text(color="black", size=14, face="bold.italic"),
  axis.title.x = element_text(color="black", size=14, face="bold"),
  axis.title.y = element_text(color="black", size=14, face="bold")
) + labs(title="Level of trainability by breed type",
         x ="Breed", y = "Trainability")

box <- pop1 %>% ggplot(aes(y=energy, x=breedid, fill=breedid)) + geom_boxplot()
box + theme(
  plot.title = element_text(color="black", size=14, face="bold.italic"),
  axis.title.x = element_text(color="black", size=14, face="bold"),
  axis.title.y = element_text(color="black", size=14, face="bold")
) + labs(title="Energy levels by breedtype",
         x ="Breed", y = "Energy")

by(pop1$energy, pop1$breedid, mean, na.rm=TRUE)
#germanshephard highest energy, bulldog is lowest

#correlation?
cor(pop1[,unlist(lapply(pop1, is.numeric))], use="complete.obs")
#.505 for strdiragg and dogdiragg
corr_check <- function(pop1, threshold){
  matriz_cor <- cor(pop1)
  matriz_cor
  
  for (i in 1:nrow(matriz_cor)){
    correlations <-  which((abs(matriz_cor[i,i:ncol(matriz_cor)]) > threshold) & (matriz_cor[i,i:ncol(matriz_cor)] != 1))
    
    if(length(correlations)> 0){
      lapply(correlations,FUN =  function(x) (cat(paste(colnames(pop1)[i], "with",colnames(pop1)[x]), "\n")))
      
    }
  }
}

corr_check(iris_set, 0.85)

#install.packages("psych")
library(psych)

ct <- corr.test(pop1)
corr.test(pop1, y = NULL, use = "pairwise",method="pearson",adjust="holm", 
          alpha=.05,ci=TRUE,minlength=5,normal=TRUE)
corr.p(r,n,adjust="holm",alpha=.05,minlength=5,ci=TRUE)

mycorrelations <- psych::corr.test(pop1)



model=lm(pop1$train ~ pop1$breedid)
ANOVA=aov(model)

#anova
av1 <- aov(energy ~ breedid, data = pop1)
summary(av1)
#tukey
tukey.two.way <- TukeyHSD(av1)
tukey.two.way



#exploring tukey results
TK_data<-as.data.frame(tukey.two.way[1]) # the [1] locates the part of the output to be exported
write.csv(TK_data, 'TK_data.csv')
#only labrador retriever and german shepherd different

#increased 15 to make it fit
psig=as.numeric(apply(tukey.two.way$breedid[,2:3],1,prod)>=0)+1
op=par(mar=c(4.2,15,3.8,2))
plot(tukey.two.way,col=psig,yaxt="n")
for (j in 1:length(psig)){
  axis(2,at=j,labels=rownames(tukey.two.way$breedid)[length(psig)-j+1],
       las=1,cex.axis=.8,col.axis=psig[length(psig)-j+1])
}
par(op)



#ifrange doesn't include 0, indicates the difference is statistically significant
#This range does not include zero, which indicates that the difference is statistically significant.
#The confidence intervals for the remaining pairs of means all include zero, which indicates that the differences are not statistically significant.
#The 95% simultaneous confidence level indicates that we can be 95% confident that all of these confidence intervals contain the true differences.

plot(tukey.two.way , las=1 , col="brown")


