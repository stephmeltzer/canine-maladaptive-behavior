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



#VIZ1 barplot
bar <- pop1 %>% ggplot(aes(y=train, x=breedid, fill=breedid)) + geom_boxplot()
bar + theme(
  plot.title = element_text(color="black", size=14, face="bold.italic"),
  axis.title.x = element_text(color="black", size=14, face="bold"),
  axis.title.y = element_text(color="black", size=14, face="bold")
) + labs(title="Level of trainability by breed type",
         x ="Breed", y = "Trainability")

box2 <- pop1 %>% ggplot(aes(y=energy, x=breedid, fill=breedid)) + geom_boxplot()
box2 + theme(
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
#significant differences in GR-GS and LR-GS

#increased to 15 to adjust width
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


avtrain <- aov(train ~ breedid, data = pop1)
summary(avtrain)
#tukey
tukey.two.waytrain <- TukeyHSD(avtrain)
tukey.two.waytrain


#TRAIN
#exploring tukey results
TK_data<-as.data.frame(tukey.two.waytrain[1]) # the [1] locates the part of the output to be exported
write.csv(TK_data, 'TK_data.csv')
#significant differences in GR-GS and LR-GS

#increased to 15 to adjust width
psig=as.numeric(apply(tukey.two.waytrain$breedid[,2:3],1,prod)>=0)+1
op=par(mar=c(4.2,15,3.8,2))
plot(tukey.two.waytrain,col=psig,yaxt="n")
for (j in 1:length(psig)){
  axis(2,at=j,labels=rownames(tukey.two.waytrain$breedid)[length(psig)-j+1],
       las=1,cex.axis=.8,col.axis=psig[length(psig)-j+1])
}
par(op)



#train
pop1 %>% group_by(breedid) %>% summarize(train = mean(train, na.rm=TRUE)) %>% arrange(desc(train))





# load package
install.packages('devtools')
devtools::install_github("laresbernardo/lares")
install.packages("lares")
lares::on()
library(lares)
corr_cross(dat, pop1,
           max_pvalue = 0.05, # display only significant correlations (at 5% level)
           top = 10 # display top 10 couples of variables (by correlation coefficient)
)
corr_cross(pop1, plot = TRUE, max = 1, top = 25, ignore = NA,
           contains = NA, rm.na = FALSE, dummy = TRUE)
corr_var(pop1)
#install.packages("ggstatsplot")
library(ggstatsplot)
install.packages("ggcorrplot")
library(ggcorrplot)
# correlogram
ggstatsplot::ggcorrmat(data = pop1,
  type = "parametric", # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("darkred", "white", "steelblue") # change default colors
)
ggcorrmat(
  data = gapminder_2007, ## data from which variable is to be taken
  cor.vars = lifeExp:gdpPercap ## specifying correlation matrix variables
)



qplot(factor(newgroup), data=pop1, geom="bar",
      fill=factor(newgroup), xlab="Group type")


library(treemap)

treepop1<- pop1 %>% group_by(breedid) %>% summarize(weight = mean(weight, na.rm=TRUE)) %>% arrange(desc(weight))
tree2<- groups5 %>% group_by(breedid, newgroup) %>% summarize(weight = mean(weight, na.rm=TRUE)) %>% arrange(desc(weight))

# treemap WEIGHT NOT WORKING
treemap(treepop1,
        index=c("newgroup","breedid"),
        vSize="weight",
        type="index",
        fun.aggregate = "mean",
        palette = "Set2",
        bg.labels=c("white"),
        align.labels=list(
          c("center", "center"),
          c("right", "bottom")
        )
)


pop1 %>% group_by(newgroup)
by(pop1$breedid, pop1$newgroup, na.rm=TRUE, summary)



#lm
p1 <- ggplot(data = pop1, aes(y = train, x = dogage)) 
p1 + geom_point(aes(color = breedid)) + geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) + facet_wrap(~breedid) + ggtitle("Trainability over time of Top 5 breeds") + xlab("Dog age") + ylab("Level of trainability") + theme(
  plot.title = element_text(color="black", size=14, face="bold.italic"),
  axis.title.x = element_text(color="black", size=14, face="bold"),
  axis.title.y = element_text(color="black", size=14, face="bold")
)  


#boxplot of where acquired / sex
box3 <- pop1 %>% ggplot(aes(y=train, x=whereacquired, fill=sex)) + geom_boxplot()
box3 + theme(
  plot.title = element_text(color="black", size=14, face="bold.italic"),
  axis.title.x = element_text(color="black", size=14, face="bold"),
  axis.title.y = element_text(color="black", size=14, face="bold")
) + labs(title="Trainability levels by sex",
         x ="Sex", y = "Trainability")

pop1$sex <- factor(pop1$sex, 
                 levels=c(0,1), 
                 labels=c("male","female"))

#update