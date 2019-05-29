# Code to reproduce analyses in 'large scale cooperation driven by reputation,not fear of divine punishment' by [Ge et al. (2019)]
# The required files that accompany the code are not included here, but it is available on request to the authors
# Code by Erhao Ge & Yuan Chen 


####################################################
#################################    Text part
####################################################

########################################################
########################## Free donation game
########################################################

groupx$communitysize<- factor(groupx$communitysize,levels=c("small","big"))
levels(groupx$communitysize)

wilcox.test(publicdonationamount~pubbinarydonation,groupx,alternative="greater")


groupx$pubbinarydonation <- factor(groupx$pubbinarydonation,levels=c(0,1))
levels(groupx$pubbinarydonation)
wilcox.test(RC1~pubbinarydonation,groupx,alternative="less")

library("vcd","grid")
mytable <- xtabs(~communitysize+pubbinarydonation,groupx)
mytable
chisq.test(mytable)

x <- as.numeric(groupx$DiceKept)
x
########################################################
########################## Dice allocation game
########################################################

#######two sample KS test##########
set.seed(5)#set random seed to reproduce the result
library(purrr)
y <- rdunif(1002,0,5)#discrete uniform distribution
y <- 2*y
ks.test(x,y)

#########Binomial Test#########
######### ###### binom.test(x=,n=,p=) ############
with(groupx,table(DiceKept))

b.test.p <- function(x,y){
  m <- length(which(x==y))
  n <- length(x)
  binom.test(m,n,1/6)#PAYOFFS(RMB)
}
######## b.test.p(x,y) ###########binomal.test
b.test.p(groupx$DiceKept,0)
b.test.p(groupx$DiceKept,2)
b.test.p(groupx$DiceKept,4)
b.test.p(groupx$DiceKept,6) 
b.test.p(groupx$DiceKept,8)
b.test.p(groupx$DiceKept,10)


#########################
#### for each institution
#########################

################## Monastery ######################
Monastery <- groupx[which(groupx$whichchoice=="Buddhist temple"),]
x <- as.numeric(Monastery$DiceKept)
length(x)
y <- 2*rdunif(146,0,5)
ks.test(x,y)

with(Monastery,table(DiceKept))
b.test.p(Monastery$DiceKept,0)
b.test.p(Monastery$DiceKept,2)
b.test.p(Monastery$DiceKept,4)
b.test.p(Monastery$DiceKept,6)
b.test.p(Monastery$DiceKept,8)
b.test.p(Monastery$DiceKept,10)

################## Mosque ######################
mosque <- groupx[which(groupx$whichchoice=="mosque"),]
x <- as.numeric(mosque$DiceKept)
length(x)
y <- 2*rdunif(114,0,5)
ks.test(x,y)

with(mosque,table(DiceKept))
b.test.p(mosque$DiceKept,0)
b.test.p(mosque$DiceKept,2)
b.test.p(mosque$DiceKept,4)
b.test.p(mosque$DiceKept,6)
b.test.p(mosque$DiceKept,8)
b.test.p(mosque$DiceKept,10)

################## Hope Projects ######################
HopeProjects <- groupx[which(groupx$whichchoice=="Hope projects"),]
x <- as.numeric(HopeProjects$DiceKept)
length(x)
y <- 2*rdunif(437,0,5)
ks.test(x,y)

with(HopeProjects,table(DiceKept))
b.test.p(HopeProjects$DiceKept,0)
b.test.p(HopeProjects$DiceKept,2)
b.test.p(HopeProjects$DiceKept,4)
b.test.p(HopeProjects$DiceKept,6)
b.test.p(HopeProjects$DiceKept,8)
b.test.p(HopeProjects$DiceKept,10)

################## Mother watercellar ######################
watercellar <- groupx[which(groupx$whichchoice=="Mother's cellar"),]
x <- as.numeric(watercellar$DiceKept)
length(x)
y <- 2*rdunif(305,0,5)
ks.test(x,y)

with(watercellar,table(DiceKept))
b.test.p(watercellar$DiceKept,0)
b.test.p(watercellar$DiceKept,2)
b.test.p(watercellar$DiceKept,4)
b.test.p(watercellar$DiceKept,6)
b.test.p(watercellar$DiceKept,8)
b.test.p(watercellar$DiceKept,10)

###############Kruskal test###############
kruskal.test(groupx$DiceKept~groupx$whichchoice)


############################
##########  Figure 2
############################
library(ggplot2)
library(plyr)

ggplot(groupx,aes(x=dwellingtime,fill=communitysize))+geom_bar(aes(y = ..prop..),stat="count",position = "dodge")+
  scale_x_continuous(breaks = c(1,2,3,4,5),labels = c("1-3 years","3-5 years","5-10 years","10-20 years",">20 years"))+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(labels=c("Small Communites","Big Communities"),values = c("#D55E00","#009E73"))+
  ylab("Proportion")+xlab("Dwelling Time")+
  geom_text(aes(label = scales::percent(..prop..),y= ..prop.. ), stat= "count", hjust = .5,vjust=-.5,
            position = position_dodge(0.9))+
  theme(panel.background = element_rect(fill = "transparent",colour = NA), panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),plot.background = element_rect(fill = "transparent",colour = NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(legend.title=element_blank())+theme(legend.key = element_blank())+
  theme(legend.justification=c(1,0),legend.position=c(0.2,0.8))
ggsave("dwellingtime~CS.png",dpi = 400)


############################
##########  Figure 3
############################

ggplot(groupx,aes(x=factor(publicdonationamount),group=communitysize))+
  geom_bar(aes(y = ..prop.., fill=communitysize),stat="count")+
  scale_fill_manual(values = c("#CCEEFF","#FFDDDD"),guide=F)+xlab("donation amount(??)")+ylab("proportion ")+
  geom_text(aes(label = scales::percent(..prop..),y= ..prop.. ), stat= "count", vjust = -.4)+facet_grid(~communitysize) +
  scale_y_continuous(labels = scales::percent)+
  theme(plot.title = element_text(hjust = 0.5,vjust = 0))+
  theme(panel.background = element_rect(fill = "transparent",colour = NA), panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),plot.background = element_rect(fill = "transparent",colour = NA))+
  theme(axis.line = element_line(colour = "black"))



############################
##########  Figure 4
############################

ggplot(paper_fig1_ForPlotting,aes(x=Variable,y=LogOR1))+
  geom_bar(stat="identity",position="identity",width = 0.5)+
  scale_y_continuous(trans="log",breaks = c(0.129,1.0,1.9),labels = c(0.1,1.0,1.9))+
  scale_x_discrete(limits=rev(c("Religious practice","Divine reward/punishment","Community size(ref:small)",
                                "Choice(ref:nonreligious)","Education","Economic instability")))+
  coord_flip()+geom_errorbar(aes(ymin=CI1,ymax=CI2),width=.2)+
  annotate("text",x="Community size(ref:small)",y=0.145790145,label="*")+
  annotate("text",x="Religious practice",y=1.867065039,label="**")+
  annotate("text",x="Education",y=1.936917284,label="***")+
  annotate("text",x="Economic instability",y=0.475414744,label="**")+
  ylab("Odds ratio of donating all the money to institution")+guides(fill=FALSE)+
  theme(panel.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "transparent",colour = NA))+
  theme(axis.line = element_line(colour = "black"))+
  theme(axis.title.y = element_blank())


############################
##########  Figure 5
############################
groupx$whichchoice <- factor(groupx$whichchoice)
levels(groupx$whichchoice)[levels(groupx$whichchoice)=="Hope projects"] <-" Hope projects"
levels(groupx$whichchoice)[levels(groupx$whichchoice)=="Buddhist temple"] <-" Monastery"
levels(groupx$whichchoice)[levels(groupx$whichchoice)=="mosque"] <-" Mosque"
levels(groupx$whichchoice)[levels(groupx$whichchoice)=="Mother's cellar"] <-" Mother's cellar"

library(ggplot2)
p <- ggplot(groupx, aes(x=DiceKept))+
  facet_wrap(~whichchoice,nrow=2)+
  geom_histogram(aes(y=2*(..density..)),binwidth=2,fill="red",colour=I("black"))+
  theme(panel.background=element_blank(),axis.line=element_line(colour = "black"))+
  geom_hline(yintercept = 1/6,colour="blue")+
  xlab("Amount kept by the participants")+ylab("Proportion")+scale_x_continuous(breaks=seq(0,10,2))

f_labels <- data.frame(whichchoice=" Monastery",label="**")
ft_labels <- data.frame(whichchoice=" Mosque",label="**")

p+geom_text(x=8,y=0.278,aes(label=label),data=f_labels)+geom_text(x=8,y=0.275,aes(label=label),data=ft_labels)


############################
##########  Table 1
############################
library(lme4)
library(AICcmodavg)

groupx$communitysize <- factor(groupx$communitysize) %>% relevel(.,ref="small")
groupx$Choice<- factor(groupx$Choice) %>% relevel(.,ref="0")
groupx$gender<- factor(groupx$gender) %>% relevel(.,ref="1")

Pub.models = list()
Pub.models[[1]]=glmer(pubbinarydonation~(1|Loc),family=binomial,data=groupx)
Pub.models[[2]]=glmer(pubbinarydonation ~ gender+age+offspring+onlookers+education+economicinstability+(1|Loc),
                      family=binomial,data=groupx)
Pub.models[[3]]=glmer(pubbinarydonation ~ gender+age+offspring+onlookers+education+economicinstability+(1|Loc)+
                        communitysize+Choice+RC1+RC2+RC3,family=binomial,data=groupx)
Pub.models[[4]]=glmer(pubbinarydonation ~ gender+age+offspring+onlookers+education+economicinstability+(1|Loc)+
                        communitysize,family=binomial,data=groupx)
Pub.models[[5]]=glmer(pubbinarydonation ~ gender+age+offspring+onlookers+education+economicinstability+(1|Loc)+
                        RC1+RC2+RC3+Choice,family=binomial,data=groupx)
Pub.names=c("Null", "control","CS+R+C","CS","R+C")
final.aics = aictab(Pub.models, Pub.names,second.ord = F)


############################
##########  Table 2
############################
####### model averaging
library(MuMIn)
average_Pub_model<-model.avg(Pub.models[[1]],Pub.models[[2]],Pub.models[[3]],Pub.models[[4]],Pub.models[[5]])
summary(average_Pub_model)

