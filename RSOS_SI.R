# Code to reproduce analyses in 'large scale cooperation driven by reputation,not fear of divine punishment' by [Ge et al. (2018)]
# The required files that accompany the code are not included here, but it is available on request to the authors
# Code by Erhao Ge & Yuan Chen 

############################
##########  Figure S1
############################

library(raster)
library(ggplot2)
library(ggthemes)
site17 <- read_excel("C:/Users/Geeh/Desktop/site17.xlsx")
site17$MainReligiousSect <- factor(site17$MainReligiousSect,levels = c("None","Mix","Muslim","Buddhism"))
mydata <- getData("GADM", country = "china", level = 1)
mymap <- fortify(mydata)
g1 <-  ggplot() +
  geom_blank(data = mymap, aes(x = long, y = lat)) +
  geom_map(data = mymap, map = mymap,
           aes(group = group, map_id = id),
           fill = "#b2b2b2", color = "black", size = 0.3) +
  scale_x_continuous(limits = c(101, 107), expand = c(0, 0)) +
  scale_y_continuous(limits = c(33, 39), expand = c(0, 0)) +
  geom_point(data=site17,aes(x=longitude,y=latitude,colour = MainReligiousSect ,shape=CommunitySize),alpha=0.8,size=3)+
  scale_colour_manual(values=c("None" = "white", "Mix"="yellow","Muslim" = "green","Buddhism"="red"))+
  scale_shape_manual(values = c("big"=16,"small"=17))+                      
  labs(x='Longitude',y='Latitude',colour="Main Religious Sect",shape="Community Size")+
  ggtitle("Survey Area")+theme(plot.title = element_text(hjust = 0.5))

temp <- data.frame(long = c(101, 101, 107, 107, 101),
                   lat = c(33, 39, 39, 33, 33))
g2 <- ggplotGrob(
  ggplot() +
    geom_blank(data = mymap, aes(x = long, y = lat)) +
    geom_map(data = mymap, map = mymap,
             aes(group = group, map_id = id),
             fill = "#b2b2b2", color = "black", size = 0.3) +
    geom_path(data = temp, aes(x = long, y = lat), size = 1,colour="yellow") +
    scale_x_continuous(limits = c(70, 135), expand = c(0, 0)) +
    scale_y_continuous(limits = c(10, 60), expand = c(0, 0)) +
    coord_map("polyconic") +
    theme_map() +
    theme(panel.background = element_rect(fill = NULL))
)
g3 <- g1 +annotation_custom(grob = g2, xmin = 104.5, xmax = 107,ymin = 33, ymax = 35.5) 
ggsave("map.png",width = 23,height = 20,units = "cm",dpi = 300)




############################
##########  Figure S2
############################

ggplot(data7,aes(x=DiceKept,group=Loc))+geom_line(aes(y=cumulative,colour="Locations"),show.legend = T)+
  geom_line(aes(y=Justified_dishonesty,colour="Justified dishonesty"),linetype="dashed",size=1,show.legend = T)+
  geom_hline(aes(yintercept=0),linetype=5)+ geom_vline(aes(xintercept=6),linetype=5)+
  scale_y_continuous(breaks = c(0,1/6,2/6,3/6,4/6,5/6,1),
                     labels=c("0","1/6","2/6","3/6","4/6","5/6","1"))+
  xlab("Money left for themselves in private game")+ylab("Frequency")+
  theme(panel.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))+
  theme(axis.line = element_line(colour = "black"))+
  scale_color_manual(name = "colour",
                     values = c("Locations" = 'red', "Justified dishonesty" = 'blue'))+
  theme(legend.title=element_blank())+theme(legend.key = element_blank())+
  theme(legend.justification=c(1,0),legend.position=c(0.85,0.1))+
  guides(colour = guide_legend(override.aes = list(lty = c(2,1))))

############################
##########  Figure S3
############################
#multicollinearity test   
shapiro.test(final2$DiceKept)
control.m<- lm(Choice ~ gender +economicinstability+education,data=final2)
attach(final2)
plot(Choice~economicinstability,data=final2,xlab="Proportion of economic instability",
     ylab="Proportion of choosing religious institution")
abline(lm(Choice ~ economicinstability,data=final2),col="lightgray")
abline(lm(Choice ~ economicinstability + gender + education,coef=TRUE,data=final2))
text(economicinstability,Choice,rownames(final2),cex=0.6,pos=4,col="red")
detach(final2)

summary(lm(Choice ~ economicinstability,data=final2))

############################
##########  Figure S4
############################

library(factoextra)
library(FactoMineR)

pca <- prcomp(Religiosity_PCA, center = TRUE,scale. = T,retx=T)
xtable::xtable(summary(pca))
xtable::xtable(print(pca))

pca_plot<-fviz_pca_biplot(pca, repel = TRUE,
                          col.var = "#2E9FDF",
                          col.ind = "#696969",
                          geom="point",
                          title="PCA_Religiosity")
pca_plot


############################
##########  Table S4
############################
library(tidyr)
library(dplyr)
library(xtable)
difference_community<- groupx %>% group_by(communitysize) %>% summarise_each(funs(mean,sd)) 
groupx %>% group_by(communitysize) %>% summarise(mean(as.numeric(gender)))
groupx %>% group_by(communitysize) %>% summarise(mean(as.numeric(Choice)))
write.table(difference_community,file="difference_community2.csv",col.names =T,row.names=F,sep=",")



############################
##########  Table S6
############################
###### Importting  religious-related data
library(readr)
Religiosity_PCA <- read_csv("Religiosity_PCA.csv")
###### changing the religious distance to " how close to the survey location"
Religiosity_PCA$religiondis<-0-Religiosity_PCA$religiondis

###### scree plot
library(psych)
library(magrittr)
scale(Religiosity_PCA[1:9]) %>% fa.parallel(.,fa="pc",n.iter = 100,
                                            show.legend = F,main="scree plot with parallel analysis")

pca3 <- scale(Religiosity_PCA[1:9]) %>% principal(.,nfactors = 3,score = T,rotate = "varimax")
pca3
pca3$scores
xtable::xtable(print(pca3))

########## Make a LATEX table and Visulization

fa2latex(pca3,digits=2,rowlabels=TRUE,apa=TRUE,short.names=FALSE,cumvar=FALSE,
         cut=0,big=.3,alpha=.05,font.size ="scriptsize",
         heading="A factor analysis table from the psych package in R",
         caption="fa2latex",label="default",silent=FALSE,file=NULL,append=FALSE)


############################
##########  Table S7
############################
library(stargazer)
############################### control model and fitted model parameters
stargazer(Pub.models[[2]],Pub.models[[3]],title = "model results",align = T, type = "html",single.row = T,
          no.space = TRUE,keep.stat="n", out = "free_donation_full model results.html")


############################
##########  Table S8
############################

# binairy part
hur1 <- glmer(pubbinarydonation ~ gender+age+offspring+onlookers+education+economicinstability+gameorder+
                communitysize+Choice+RC1+RC2+RC3+(1|Loc), 
              data = groupx, family = "binomial")
summary(hur1)
# truncated part
hur2 <- glmer(publicdonationamount ~ gender+age+offspring+onlookers+education+economicinstability+
                communitysize+Choice+RC1+RC2+RC3+(1|Loc),data = subset(groupx, publicdonationamount < 10), family = "poisson")
summary(hur2)
#######
stargazer(hur1,hur2,
          title = "free_donation_amount",align = T, type = "html",single.row = T,
          no.space = TRUE,keep.stat="n", column.labels = c("Zero","Count"),
          out = "free_donation_amount_model results.html")
summary(publicdonation_hurdle)


############################
##########  Table S9
############################
######################################### data preparation
bothchoice <- groupx[,c("ID","Choice","whichreligion")]
bothchoice <- bothchoice[1:501,]
bothchoice$godpunishment <- xxtotal$godpunishment[1:501]
bothchoice$godaward <- xxtotal$godaward[1:501]
bothchoice$whichreligion <- factor(bothchoice$whichreligion,levels = c("atheist","Buddhism","Muslim"))

########################  import "wmc" function #### In this function, alternative hypothesis is: not equal to 0
source("http://www.statmethods.net/RiA/wmc.txt")
wmc( godpunishment~ whichreligion, data=bothchoice, method="holm")
wmc( godaward~ whichreligion, data=bothchoice, method="holm")

######################## Diffrent ref-level cause diffrent W value;Different alternative hypothesis cause different P value

########### Comparing buddhist with muslim for exploring which religion has stronger belief in divine punishment. We set ref-level first.
bothchoice3 <- subset(bothchoice,whichreligion=="Buddhism"|whichreligion=="Muslim")
bothchoice3$whichreligion <- factor(bothchoice3$whichreligion,levels = c("Buddhism","Muslim"))

################### alternative="less" means "buddhism" has the weaker belief in divine punishment than "Muslim".
wilcox.test(godpunishment~whichreligion,alternative="less",data=bothchoice3)

################### Comparing God award
wilcox.test(godaward~whichreligion,alternative="less",data=bothchoice3)


############################
##########  Table S10
############################

library(lme4)
groupxsubset <- subset(groupx,whichreligion=="Buddhism"|whichreligion=="Muslim",select = whichchoice:religion)
summary(glmer(pubbinarydonation ~ whichreligion+education+economicinstability+(1|Loc),family=binomial,data=groupx))
summary(glmer(pubbinarydonation ~ whichreligion+education+economicinstability+(1|Loc),family=binomial,data=groupxsubset))



############################
##########  Table S12 & Table S13 
############################

##########################################################################################################
############################################## Permutation test ##########################################
##########################################################################################################
##########################################################################################################
#######Control model includes sex ratio(female:male) + average level of education + average economic instability.######
######################CS refers to community size, R refers to average religiosity.#######################
#Model selection for private dice game decisions (predicting the community-level mean payoffs form 17 communities).
######################################################################################################################
######################################################################################################################
library(MuMIn)
install.packages(file.choose(),repos=NULL,type = "source")
library(lmPerm) #### For Permutation test


# lists to store all models
d.m.s <- list()
# names of models
m_names = c("Control", "Control + R","Control + CS","Control + R+CS")
final2$communitysize <- factor(final2$communitysize)
d.m.s[[1]]=lmp(DiceKept ~ gender +economicinstability+education, data = final2)
d.m.s[[2]]=lmp(DiceKept ~ gender +economicinstability+education+RC1+RC2+RC3,data = final2)
d.m.s[[3]]=lmp(DiceKept ~ gender +economicinstability+education+communitysize,data = final2) 
d.m.s[[4]]=lmp(DiceKept ~ gender +economicinstability+education+communitysize+RC1+RC2+RC3,data = final2) 

best.ms<-model.sel(d.m.s)#model seletion
best.ms$Model = m_names[ as.integer(rownames(best.ms))]  # add model names in correct order

write.table(best.ms,file="Table1.csv",col.names =T,row.names=F,sep=",")

############################
##########  Table S14 
############################
library(lmPerm)

highclaim_model <- lmp(highclaim ~ gender +economicinstability+education+communitysize+RC1+RC2+RC3,data = final2)
second_highest_claim_model <- lmp(secondhighestclaim ~ gender +economicinstability+education+communitysize+RC1+RC2+RC3,data = final2) 
noclaim_model <- lmp(noclaim ~ gender +economicinstability+education+communitysize+RC1+RC2+RC3,data = final2)

summary(noclaim_model)
summary(second_highest_claim_model)
summary(highclaim_model)

