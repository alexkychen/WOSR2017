#Analysis of otolith Sr of walleye larvae collected from Sandusky and Maumee rivers 
#MS: Corey et al. Otolith microchemistry as a stock discrimination tool for river-spawning populations of Walleye (Sander vitreus) in Lake Erie.

#ANOVA for water chemistry
df_water <- read.csv("Data/")

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

