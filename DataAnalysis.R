#Analysis of otolith Sr of walleye larvae collected from Sandusky and Maumee rivers 
#MS: Corey et al. Otolith microchemistry as a stock discrimination tool for river-spawning populations of Walleye (Sander vitreus) in Lake Erie.

#ANOVA for water chemistry
df_water <- read.csv("Data/water Sr Ca data.csv", header = T);summary(df_water);names(df_water)
df_water <- df_water[,c("year","site","Ca..ppm.","Sr..ppb.","Sr.Ca")];summary(df_water)
df_water1 <- subset(df_water, site=="Maumee River");summary(df_water1)
df_water2 <- subset(df_water, site=="Sandusky River");summary(df_water2)
df_water3 <- rbind(df_water1, df_water2);summary(df_water3)
df_water3$site <- factor(df_water3$site);summary(df_water3)
df_water3$year <- factor(df_water3$year);summary(df_water3)
#Run summarySE
dfn_water <- summarySE(df_water3, measurevar = "Sr.Ca", groupvars = c("year","site"));dfn_water
tdf <- data.frame("1995","Sandusky River",NA,NA,NA,NA,NA)
names(tdf) <- names(dfn_water)
dfn_water1 <- rbind(dfn_water[1:5,], tdf, dfn_water[6:13,]) 
write.csv(dfn_water1, "Data/water_summarySE.csv", row.names=F, quote = F )

#Make water Sr:Ca bar plot w/ SD
dfn_water <- read.csv("Data/water_summarySE.csv", header = T);head(dfn_water)
dfn_water$year <- factor(dfn_water$year);summary(dfn_water);str(dfn_water)

library(ggplot2)
ggplot(dfn_water, aes(x=year, y=Sr.Ca, fill=site, width=0.85))+
  geom_bar(stat="identity", position="dodge", colour="black")+
  geom_errorbar(aes(ymin=Sr.Ca-sd, ymax=Sr.Ca+sd),width=.3, position=position_dodge(0.9))+
  xlab("Year")+ylab("Mean water Sr:Ca (ppb:ppm) ± SD")+
  scale_fill_manual(values = c("white","grey"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank(),
        axis.title.x=element_text(size=16, colour = "black", margin = unit(c(5, 0, 0, 0), "mm")),
        axis.title.y=element_text(size=16, colour = "black", margin = unit(c(0, 5, 0, 0), "mm")),
        axis.text=element_text(size=14, colour = "black"),legend.title=element_blank(), legend.position="none" )+
  annotate("text", x=4:7-0.2, y=c(14,12,15,15), label=c("a","a","a","a"), size=6)+
  annotate("text", x=4:7+0.2, y=c(26,22,35,31), label=c("bc","c","b","bc"), size=6)

#Subset water chem data for 2001, 2011, 2012, 2013
df_water4 <- subset(df_water3, year=="2001"|year=="2011"|year=="2012"|year=="2013");summary(df_water4)
df_water4$year <- factor(df_water4$year)

anova_w <- aov(Sr.Ca ~ year*site, data=df_water4);summary(anova_w)
TukeyHSD(anova_w)

#ANOVA for otolith Sr
df_oto <- read.csv("Data/larval oto chem age.csv", header = T);summary(df_oto)

df_oto1 <- df_oto[,c("year","location","otoSr.ppm")];summary(df_oto1);head(df_oto1)

#convert year to categorical variable
df_oto1$year <- as.factor(df_oto1$year);summary(df_oto1)

anova1 <- aov(otoSr.ppm ~ location*year, data=df_oto1); summary(anova1)

'''
               Df   Sum Sq  Mean Sq F value   Pr(>F)    
location        1 61854142 61854142 420.500  < 2e-16 ***
year            6 10393452  1732242  11.776 1.36e-11 ***
location:year   6  5785234   964206   6.555 1.96e-06 ***
Residuals     244 35891562   147097                     
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

'''
#Tukey HSD test
TukeyHSD(anova1)

#Boxplot (basic)
boxplot(otoSr.ppm ~ year, data=df_oto1, boxfill=rgb(1, 1, 1, alpha=1), border=rgb(1, 1, 1, alpha=1), xlab="Year",ylab="Otolith [Sr](ppm)", cex.lab=1.5)
boxplot(otoSr.ppm ~ year, data=df_oto1[which(df_oto1$location=="Maumee"),], xaxt = "n", add = TRUE, boxfill="white",boxwex=0.3, at=1:7 - 0.15)
boxplot(otoSr.ppm ~ year, data=df_oto1[which(df_oto1$location=="Sandusky"),], xaxt = "n", add = TRUE, boxfill="grey",boxwex=0.3, at=1:7 + 0.15)

#Boxplot (ggplot2)
library(ggplot2)

ggplot(data=df_oto1, aes(x=year, y=otoSr.ppm, fill=location))+
  geom_boxplot()+
  scale_fill_manual(values = c("white","grey"))+
  xlab("Year")+ylab("Otolith [Sr](ppm)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank(),
        axis.title.x=element_text(size=16, colour = "black", margin = unit(c(6, 0, 0, 0), "mm")),
        axis.title.y=element_text(size=16, colour = "black", margin = unit(c(0, 5, 0, 0), "mm")),
        axis.text=element_text(size=14, colour = "black"),
        legend.position="none")+
  annotate("text",x=1:7-0.2, y=c(1600,1200,1200,1600,1200,1500,1400), label=c("a","a","a","a","a","a","a"), size=6)+
  annotate("text",x=1:7+0.2, y=c(2000,3200,2400,3200,1500,2900,2800), label=c("b","bc","bd","cd","ae","cd","bce"), size=6)

## ANOVA of regressions of otolith Sr and age between two rivers
df_oto2 <- df_oto[,c("year","location","otoSr.ppm","age.d")];summary(df_oto2);head(df_oto2)
df_oto2$year <- as.factor(df_oto2$year);summary(df_oto2)
# Drop data have no age 
df_oto2 <- na.omit(df_oto2);summary(df_oto2)
# Drop unused factor (year 2001)
df_oto2$year <- factor(df_oto2$year);summary(df_oto2)

#ANOVA with interaction between age and river
mod1 <- aov(otoSr.ppm ~ age.d*location, data=df_oto2);summary(mod1)

#ANOVA without interaction between age and river
mod2 <- aov(otoSr.ppm ~ age.d + location, data=df_oto2);summary(mod2)

#ANOVA of two models
anova(mod1,mod2)

#Regression plot
library(ggplot2)

ggplot(df_oto2, aes(x=age.d, y=otoSr.ppm, group=location, colour=location, shape=year))+
  geom_point(size=2)+
  geom_smooth(method = lm, se=F, fullrange=T)+
  xlab("Age (d)")+ylab("Otolith [Sr](ppm)")+
  scale_y_continuous(breaks=seq(0, 2500, 500))+scale_x_continuous(breaks=seq(0, 24, 2))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank(),
        axis.title.x=element_text(size=16, colour = "black", margin = unit(c(5, 0, 0, 0), "mm")),
        axis.title.y=element_text(size=16, colour = "black", margin = unit(c(0, 5, 0, 0), "mm")),
        axis.text=element_text(size=14, colour = "black"),legend.title=element_blank() )



##Help function
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

