#####################################################################
## Replication file for                                           
## When Do Citizens Consider Political Parties Legitimate?  
##                                   
## Author: Ann-Kristin Kölln (ann-kristin.kolln@gu.se)                                                     
## 26th May 2023                                                 
## 
## Extension: Valeria Babayan                                              
## 31st March 2024                                               
## 


#Setting the working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


## load data European-wide survey Party Legitimacy
data_long2<-read.csv("PartyLeg_crosssec.csv", header=TRUE)


data_long2$Edu_o<- as.factor(data_long2$Edu_o)
data_long2 $Edu_o <- factor(data_long2 $Edu_o, levels = c("low", "medium","high"))

data_long2$Extremity_SelfLRN <- as.factor(data_long2$Extremity_SelfLRN)
data_long2 $Extremity_SelfLRN <- factor(data_long2 $Extremity_SelfLRN, levels = c("centre", "moderate left/right","left/right wing"))

data_long2$gov_exp <- as.factor(data_long2$gov_exp)
data_long2$newer_party <- as.factor(data_long2$newer_party)


library(radiant.data)
library(plyr)
library(ggplot2)
library(Rmisc)
library(tidyr)
library(ggpubr)
library(lme4)
library(lmerTest)
library(sjPlot)
library(effects)
library(psych)
library(ltm)
library(lavaan)



### Hypothesis tests ###

# evaluating the hierarchical model
Model0<-lmer(Leg_01~ 1 + (1|Party) + (1|COUNTRY), data=data_long2, weights = wFactor)
Model0a<-lmer(Leg_01~ 1 + (1|Party), data=data_long2, weights = wFactor)

anova(Model0, Model0a)

tab_model(Model0, Model0a)

## input for Table 1

# H1: gov -> Leg
Model2<-lmer(Leg_01~ female + Age + Edu_o + Extremity_SelfLRN + gov_exp + (1|Party) + (1|COUNTRY), data=data_long2, weights = wFactor)

# H2: inst -> Leg
Model3<-lmer(Leg_01~ female + Age + Edu_o + Extremity_SelfLRN + newer_party + (1|Party) + (1|COUNTRY), data=data_long2, weights = wFactor)

# H3: extr -> Leg
Model4<-lmer(Leg_01~ female + Age + Edu_o + Extremity_SelfLRN + IdeolModeration_LR  +  (1|Party) + (1|COUNTRY), data=data_long2, weights = wFactor)


# H4: DemBeh -> Leg
Model5<-lmer(Leg_01~ female + Age + Edu_o + Extremity_SelfLRN + DemBeh_01 + (1|Party) + (1|COUNTRY), data=data_long2, weights = wFactor)

# Full model
Model6<-lmer(Leg_01~ female + Age + Edu_o + Extremity_SelfLRN + gov_exp + newer_party + IdeolModeration_LR + DemBeh_01 + (1|Party) + (1|COUNTRY), data= data_long2, weights = wFactor)

## Producing Table 1
tab_model(Model2, Model3, Model4, Model5, Model6, show.ci = FALSE, show.se = TRUE, emph.p =FALSE, digits.p = 2, file ="Table1.doc")




## creating input for Figure 1
data<-read.csv("PartyLeg_exp.csv", header=TRUE)

data$treat_mod <-as.factor(data$treat_mod)
data$treat_mod <- factor(data$treat_mod, levels = c("control", "centre", "moderate left/right","left/right wing"))


#model results
fit1b<-lm(Leg_SameRights ~ treat_mod,data=data) #Q4_1a = item 1 of PartyLeg index, scaled 0-1
fit2b<-lm(Leg_DiscussProposals ~ treat_mod,data=data)
fit3b<-lm(Leg_AcceptDecisions ~ treat_mod,data=data)
fit4b<-lm(Leg_01~ treat_mod,data=data) ##Leg_01 = final index

#store predicted values
simdat2<-expand.grid("treat_mod"=c("control","centre", "moderate left/right","left/right wing"))
p1b<-predict(fit1b,simdat2,se.fit=T)
p2b<-predict(fit2b,simdat2,se.fit=T)
p3b<-predict(fit3b,simdat2,se.fit=T)
p4b<-predict(fit4b,simdat2,se.fit=T)

namedat2<-expand.grid("treat_mod"=c("control","centre", "moderate left/right","left/right wing"))

#put all into the same dataframe
plot.dat2<-gather(data.frame("SameRights"=p1b$fit,"DiscussProposals"=p2b$fit,"AcceptDecisions"=p3b$fit,"Index"=p4b$fit, namedat2),key="variable",value="fit",-treat_mod)
plot.dat2<-cbind(plot.dat2,"se"=c(p1b$se.fit, p2b$se.fit, p3b$se.fit, p4b$se.fit))
plot.dat2$variable<-factor(plot.dat2$variable,levels=c("SameRights","DiscussProposals","AcceptDecisions","Index"))

#function to rotate axis
rotatedAxisElementText = function(angle,position='x'){
  angle     = angle[1]; 
  position  = position[1]
  positions = list(x=0,y=90,top=180,right=270)
  if(!position %in% names(positions))
    stop(sprintf("'position' must be one of [%s]",paste(names(positions),collapse=", ")),call.=FALSE)
  if(!is.numeric(angle))
    stop("'angle' must be numeric",call.=FALSE)
  rads  = (angle - positions[[ position ]])*pi/180
  hjust = 3.75*(1 - sin(rads))
  vjust = 0.65*(1 + cos(rads))
  element_text(angle=angle,vjust=vjust,hjust=hjust)
}

#pd <- position_jitter(width = 0.1, height = 0)
pd <- position_dodge(width =.1)



## creating Figure 1
p2<-ggplot(plot.dat2,aes(treat_mod, fit, group=variable, colour=variable,label=fit)) +
  geom_point(aes(shape=variable),position=pd,size=2) + 
  geom_text(aes(y=fit-1.96*se,label=format(round(fit,2),nsmall = 2)),hjust="middle", vjust=2,size=1.75) +
  facet_grid(~variable)+
  geom_linerange(aes(ymin=fit-1.96*se, ymax=fit+1.96*se)) + 
  xlab("") + 
  ylab("Party legitimacy perceptions (0-1)") +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border=element_rect(linetype = "solid", fill = NA),
        strip.background = element_rect(color="black",linetype="solid"),legend.position = "none",text = element_text(size=rel(3.5)),
        strip.text.x=element_text(size=10),axis.text.x = rotatedAxisElementText(45,'x'))+
  scale_color_manual(values=c("grey60","grey60","grey60","black")) +
  scale_shape_manual(values=rep(19,5))

jpeg("Figure2.jpeg", width=8, height=5, units='in', res=300)   
p2
dev.off()


## creating input for Figure 2

data$treat_demo <-as.factor(data$treat_demo)
data$treat_demo <- factor(data$treat_demo, levels = c("control", "pro-democratic", "anti-democratic"))

#model results
fit1<-lm(Leg_SameRights ~treat_demo,data=data) #Q4_1a = item 1 of PartyLeg index, scaled 0-1
fit2<-lm(Leg_DiscussProposals ~ treat_demo,data=data)
fit3<-lm(Leg_AcceptDecisions ~ treat_demo,data=data)
fit4<-lm(Leg_01~ treat_demo,data=data) ##Leg_01 = final index

#store predicted values
simdat<-expand.grid("treat_demo"=c("control","pro-democratic","anti-democratic"))
p1<-predict(fit1,simdat,se.fit=T)
p2<-predict(fit2,simdat,se.fit=T)
p3<-predict(fit3,simdat,se.fit=T)
p4<-predict(fit4,simdat,se.fit=T)

namedat<-expand.grid("treat_demo"=c("control","pro-democratic","anti-democratic"))

#put all into the same dataframe
plot.dat<-gather(data.frame("SameRights"=p1$fit,"DiscussProposals"=p2$fit,"AcceptDecisions"=p3$fit,"Index"=p4$fit,namedat),key="variable",value="fit",-treat_demo)
plot.dat<-cbind(plot.dat,"se"=c(p1$se.fit,p2$se.fit,p3$se.fit,p4$se.fit))
plot.dat$variable<-factor(plot.dat$variable,levels=c("SameRights","DiscussProposals","AcceptDecisions","Index"))

#pd <- position_jitter(width = 0.1, height = 0)
pd <- position_dodge(width =.1)


## creating Figure 2
p<-ggplot(plot.dat,aes(treat_demo, fit, group=variable, colour=variable,label=fit)) +
  geom_point(aes(shape=variable),position=pd,size=2) + 
  geom_text(aes(y=fit-1.96*se,label=format(round(fit,2),nsmall = 2)),hjust="middle", vjust=2,size=1.75) +
  facet_grid(~variable)+
  geom_linerange(aes(ymin=fit-1.96*se, ymax=fit+1.96*se)) + 
  xlab("") + 
  ylab("Party legitimacy perceptions (0-1)") +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border=element_rect(linetype = "solid", fill = NA),
        strip.background = element_rect(color="black",linetype="solid"),legend.position = "none",text = element_text(size=rel(3.5)),
        strip.text.x=element_text(size=10),axis.text.x = rotatedAxisElementText(45,'x'))+
  scale_color_manual(values=c("grey60","grey60","grey60","black")) +
  scale_shape_manual(values=rep(19,5))

jpeg("Figure3.jpeg", width=8, height=5, units='in', res=300)   
p
dev.off()


# EXTENSIONS

### logit:

# DV across countries:
detach(package:plyr)

summary(data_long2$Leg_01)

# simple logit:
data_long2$Leg_bin <- ifelse(data_long3$Leg_01 > 0.8, 1, 0) # above the mean

Model7_1 <- glm(Leg_bin ~ female + Age + Edu_o + Extremity_SelfLRN + gov_exp + 
                  newer_party + IdeolModeration_LR + DemBeh_01, 
                family = "binomial", data = data_long2)
summary(Model7_1)
stargazer::stargazer(Model7_1, out = "logit.tex")

### hierarchical logit:

Model8_1 <- glmer(Leg_bin ~ female + Age + Edu_o + Extremity_SelfLRN + gov_exp + 
                    newer_party + IdeolModeration_LR + DemBeh_01 
                  + (1 | COUNTRY/Party), weights = wFactor, data = data_long2, family = binomial(link = "logit"))
summary(Model8_1)
stargazer::stargazer(Model8_1, out = "3.tex")

# Remove highest achievers:
data_long3 <- data_long2 %>% filter (CountryName != "Denmark" | CountryName != "Portugal")
summary(data_long3$Leg_01)

data_long3$Leg_bin <- ifelse(data_long3$Leg_01 > 0.8, 1, 0)
ftable(data_long3$Leg_bin)

Model9_1<-lmer(Leg_01~ female + Age + Edu_o + Extremity_SelfLRN + gov_exp + newer_party + IdeolModeration_LR + DemBeh_01 + (1|Party) + (1|COUNTRY), data= data_long3, weights = wFactor)

tab_model(Model9_1, show.ci = FALSE, show.se = TRUE, emph.p =FALSE, digits.p = 2, file ="Table3.doc")
