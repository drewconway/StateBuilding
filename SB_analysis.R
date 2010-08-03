# File-Name:       SB_analysis                 
# Date:            2010-07-28                                
# Author:          Drew Conway
# Email:           drew.conway@nyu.edu                                      
# Purpose:         Statistical analysis in support of ""
# Data Used:       
# Packages Used:          
# Output File:    
# Data Output:     
# Machine:         Drew Conway's MacBook Pro

# Copyright (c) 2010, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php
# All rights reserved.

# To replicate data you must first unzip the ABM_data.zip file
# untar("ABM_data.zip",compressed="gzip")

# Load libraries and data
library(ggplot2)
library(plyr)
library(Zelig)

# Add ID variable to df
binom<-read.csv("ABM_data/binomial/FULL_binomial.csv")
binom<-transform(binom,id=1:nrow(binom))
pareto<-read.csv("ABM_data/pareto/FULL_pareto.csv")
pareto<-transform(pareto,id=1:nrow(pareto))
power<-read.csv("ABM_data/power_law/FULL_power_law.csv")
power<-transform(power,id=1:nrow(power))
pref<-read.csv("ABM_data/pref_attach/FULL_pref_attach.csv")
pref<-transform(pref,id=1:nrow(pref))
uniform<-read.csv("ABM_data/uniform/FULL_uniform.csv")
uniform<-transform(uniform,id=1:nrow(uniform))

# Model parameters
num_agents<-150
num_runs<-500
agent.types<-c("Altruistic","Community","Min-match","Max-match","Miserly")
dummy.cols<-as.vector(c(sapply(agent.types,function(s) {paste("Is",sub("-","",s),sep=".")}),"id"))

# Create dummy variables for all types in all data sets
# binom dummies
binom.dummy.list<-list()
for(i in 0:4){
    binom.dummy.list[[i+1]]<-ifelse(binom$type==i,1,0)
}
binom.dummy.df<-as.data.frame(do.call("cbind",binom.dummy.list))
binom.dummy.df<-transform(binom.dummy.df,id=1:nrow(binom))
colnames(binom.dummy.df)<-dummy.cols
binom<-merge(binom,binom.dummy.df)

# Pareto dummies
pareto.dummy.list<-list()
for(i in 0:4){
    pareto.dummy.list[[i+1]]<-ifelse(pareto$type==i,1,0)
}
pareto.dummy.df<-as.data.frame(do.call("cbind",pareto.dummy.list))
pareto.dummy.df<-transform(pareto.dummy.df,id=1:nrow(pareto))
colnames(pareto.dummy.df)<-dummy.cols
pareto<-merge(pareto,pareto.dummy.df)

# Power-law dummies
power.dummy.list<-list()
for(i in 0:4){
    power.dummy.list[[i+1]]<-ifelse(power$type==i,1,0)
}
power.dummy.df<-as.data.frame(do.call("cbind",power.dummy.list))
power.dummy.df<-transform(power.dummy.df,id=1:nrow(power))
colnames(power.dummy.df)<-dummy.cols
power<-merge(power,power.dummy.df)

# Preferential attachment dummies
pref.dummy.list<-list()
for(i in 0:4){
    pref.dummy.list[[i+1]]<-ifelse(pref$type==i,1,0)
}
pref.dummy.df<-as.data.frame(do.call("cbind",pref.dummy.list))
pref.dummy.df<-transform(pref.dummy.df,id=1:nrow(pref))
colnames(pref.dummy.df)<-dummy.cols
pref<-merge(pref,pref.dummy.df)

# Uniform dummies
uniform.dummy.list<-list()
for(i in 0:4){
    uniform.dummy.list[[i+1]]<-ifelse(uniform$type==i,1,0)
}
uniform.dummy.df<-as.data.frame(do.call("cbind",uniform.dummy.list))
uniform.dummy.df<-transform(uniform.dummy.df,id=1:nrow(uniform))
colnames(uniform.dummy.df)<-dummy.cols
uniform<-merge(uniform,uniform.dummy.df)

### SUMMARY VISUALIZATIONS ###
img.dir<-"images"
dir.create(img.dir)

# Degree distribution plots #
dd.dir<-"degree_dist"
dir.create(paste(img.dir,"/",dd.dir,sep=""))
setwd(paste(img.dir,"/",dd.dir,sep=""))

png("binom_dd.png",height=600,width=600,res=100)
binom.dd<-ggplot(binom,aes(x=num_neighbors))+stat_bin(aes(fill="black",colour="gainsboro"),binwidth=1)+
    scale_fill_manual(values=c("black"),legend=FALSE)+scale_colour_manual(values=c("gainsboro"),legend=FALSE)+
    opts(title="Degree Distribution for Binomial Networks")+xlab("Degree")+ylab("Frequency")+theme_bw()+
    scale_x_continuous(limits=c(0,100))
print(binom.dd)
dev.off()

png("pareto_dd.png",height=600,width=600,res=100)
pareto.dd<-ggplot(pareto,aes(x=num_neighbors))+geom_histogram(aes(fill="black",colour="gainsboro"),binwidth=1)+
    scale_fill_manual(values=c("black"),legend=FALSE)+scale_colour_manual(values=c("gainsboro"),legend=FALSE)+
    opts(title="Degree Distribution for Pareto Networks")+xlab("Degree")+ylab("Frequency")+theme_bw()+
     scale_x_continuous(limits=c(0,100))
print(pareto.dd)
dev.off()

png("power_dd.png",height=600,width=600,res=100)
power.dd<-ggplot(power,aes(x=num_neighbors))+geom_histogram(aes(fill="black",colour="gainsboro"),binwidth=1)+
    scale_fill_manual(values=c("black"),legend=FALSE)+scale_colour_manual(values=c("gainsboro"),legend=FALSE)+
    opts(title="Degree Distribution for Power-law Networks")+xlab("Degree")+ylab("Frequency")+theme_bw()+
     scale_x_continuous(limits=c(0,100))
print(power.dd)
dev.off()

png("pref_dd.png",height=600,width=600,res=100)
pref.dd<-ggplot(pref,aes(x=num_neighbors))+geom_histogram(aes(fill="black",colour="gainsboro"),binwidth=1)+
    scale_fill_manual(values=c("black"),legend=FALSE)+scale_colour_manual(values=c("gainsboro"),legend=FALSE)+
    opts(title="Degree Distribution for Preferntial Attachment Networks")+xlab("Degree")+ylab("Frequency")+theme_bw()+
     scale_x_continuous(limits=c(0,100))
print(pref.dd)
dev.off()

png("uniform_dd.png",height=600,width=600,res=100)
uniform.dd<-ggplot(uniform,aes(x=num_neighbors))+geom_histogram(aes(fill="black",colour="gainsboro"),binwidth=1)+
    scale_fill_manual(values=c("black"),legend=FALSE)+scale_colour_manual(values=c("gainsboro"),legend=FALSE)+
    opts(title="Degree Distribution for Uniform Networks")+xlab("Degree")+ylab("Frequency")+theme_bw()+
     scale_x_continuous(limits=c(0,100))
print(uniform.dd)
dev.off()

# Return to root directory
setwd("../..")

# Bin plots for agent contribution levels by agent type and provision point
tp.dir<-"contrib_bins"
dir.create(paste(img.dir,"/",tp.dir,sep=""))
setwd(paste(img.dir,"/",tp.dir,sep=""))

# First create labeler for ggplot2
grid.lab<-function(variable, value) {
    agent.types<-c("Altruistic","Community","Min-match","Max-match","Miserly")
    prov.labels<-c("Good Not Provided","Good Provided")
    if(variable=="threshold_met") {
        prov.labels[value+1]
    }
    else {
         agent.types[value+1]
    }
}

png("uniform_tp.png",height=450,width=1000,res=100)
uniform.tp<-ggplot(uniform,aes(x=contrib))+stat_bin(aes(y=log(..count..),colour="gray"),binwidth=.1)+
    scale_fill_manual(values=c("black"))+scale_colour_manual(values=c("gainsboro"),legend=FALSE)+
    facet_grid(threshold_met~type,labeller=grid.lab)+xlab("Agent Contribution Level")+ylab("log(Frequency)")+
    opts(title="Histogram of Agent Contribution Level by Agent Type and\n Provision Point for Uniform Networks")+theme_bw()
print(uniform.tp)
dev.off()

png("pareto_tp.png",height=450,width=1000,res=100)
pareto.tp<-ggplot(pareto,aes(x=contrib))+stat_bin(aes(y=log(..count..),colour="gray"),binwidth=.1)+
    scale_fill_manual(values=c("black"))+scale_colour_manual(values=c("gainsboro"),legend=FALSE)+
    facet_grid(threshold_met~type,labeller=grid.lab)+xlab("Agent Contribution Level")+ylab("log(Frequency)")+
    opts(title="Histogram of Agent Contribution Level by Agent Type and\n Provision Point for Pareto Networks")+theme_bw()
print(pareto.tp)
dev.off()

png("binom_tp.png",height=450,width=1000,res=100)
binom.tp<-ggplot(binom,aes(x=contrib))+stat_bin(aes(y=log(..count..),colour="gray"),binwidth=.1)+
    scale_fill_manual(values=c("black"))+scale_colour_manual(values=c("gainsboro"),legend=FALSE)+
    facet_grid(threshold_met~type,labeller=grid.lab)+xlab("Agent Contribution Level")+ylab("log(Frequency)")+
    opts(title="Histogram of Agent Contribution Level by Agent Type and\n Provision Point for Binomial Networks")+theme_bw()
print(binom.tp)
dev.off()

png("pref_tp.png",height=450,width=1000,res=100)
pref.tp<-ggplot(pref,aes(x=contrib))+stat_bin(aes(y=log(..count..),colour="gray"),binwidth=.1)+
    scale_fill_manual(values=c("black"))+scale_colour_manual(values=c("gainsboro"),legend=FALSE)+
    facet_grid(threshold_met~type,labeller=grid.lab)+xlab("Agent Contribution Level")+ylab("log(Frequency)")+
    opts(title="Histogram of Agent Contribution Level by Agent Type and\n Provision Point for Preferential Attachment Networks")+theme_bw()
print(pref.tp)
dev.off()

png("power_tp.png",height=450,width=1000,res=100)
power.tp<-ggplot(power,aes(x=contrib))+stat_bin(aes(y=log(..count..),colour="gray"),binwidth=.1)+
    scale_fill_manual(values=c("black"))+scale_colour_manual(values=c("gainsboro"),legend=FALSE)+
    facet_grid(threshold_met~type,labeller=grid.lab)+xlab("Agent Contribution Level")+ylab("log(Frequency)")+
    opts(title="Histogram of Agent Contribution Level by Agent Type and\n Provision Point for Power-law Networks")+theme_bw()
print(power.tp)
dev.off()

# Return to root directory
setwd("../..")

# 2D bin plots of agent types and wealth
bn.dir<-"type_bins"
dir.create(paste(img.dir,"/",bn.dir,sep=""))
setwd(paste(img.dir,"/",bn.dir,sep=""))

png("uniform_bn.png",height=400,width=1000,res=100)
uniform.bn<-ggplot(uniform,aes(x=type,y=wealth))+stat_bin2d(aes(binwidth=20,colour="black"))+coord_flip()+
    scale_x_continuous(breaks=0:4,labels=agent.types)+ylab("Agent Wealth Frequency")+xlab("Agent Type Frequency")+
    opts(title="Two-Dimensional Histogram of Agent Types and Wealth for Uniform Networks")+
    scale_fill_gradient(low="gainsboro",high="black",name="Agent Counts",breaks=seq(2000,14000,by=2000))+
    scale_y_continuous(breaks=seq(0,120,20))+scale_colour_grey(legend=FALSE)+theme_bw()
print(uniform.bn)
dev.off()

png("pareto_bn.png",height=400,width=1000,res=100)
pareto.bn<-ggplot(pareto,aes(x=type,y=wealth))+stat_bin2d(aes(binwidth=20,colour="black"))+coord_flip()+
    scale_x_continuous(breaks=0:4,labels=agent.types)+ylab("Agent Wealth Frequency")+xlab("Agent Type Frequency")+
    opts(title="Two-Dimensional Histogram of Agent Types and Wealth for Uniform Networks")+
    scale_fill_gradient(low="gainsboro",high="black",name="Agent Counts",breaks=seq(2000,14000,by=2000))+
    scale_y_continuous(breaks=seq(0,120,20))+scale_colour_grey(legend=FALSE)+theme_bw()
print(pareto.bn)
dev.off()

png("binom_bn.png",height=400,width=1000,res=100)
binom.bn<-ggplot(binom,aes(x=type,y=wealth))+stat_bin2d(aes(binwidth=20,colour="black"))+coord_flip()+
    scale_x_continuous(breaks=0:4,labels=agent.types)+ylab("Agent Wealth Frequency")+xlab("Agent Type Frequency")+
    opts(title="Two-Dimensional Histogram of Agent Types and Wealth for Uniform Networks")+
    scale_fill_gradient(low="gainsboro",high="black",name="Agent Counts",breaks=seq(2000,14000,by=2000))+
    scale_y_continuous(breaks=seq(0,120,20))+scale_colour_grey(legend=FALSE)+theme_bw()
print(binom.bn)
dev.off()

png("pref_bn.png",height=400,width=1000,res=100)
pref.bn<-ggplot(pref,aes(x=type,y=wealth))+stat_bin2d(aes(binwidth=20,colour="black"))+coord_flip()+
    scale_x_continuous(breaks=0:4,labels=agent.types)+ylab("Agent Wealth Frequency")+xlab("Agent Type Frequency")+
    opts(title="Two-Dimensional Histogram of Agent Types and Wealth for Uniform Networks")+
    scale_fill_gradient(low="gainsboro",high="black",name="Agent Counts",breaks=seq(2000,14000,by=2000))+
    scale_y_continuous(breaks=seq(0,120,20))+scale_colour_grey(legend=FALSE)+theme_bw()
print(pref.bn)
dev.off()

png("power_bn.png",height=400,width=1000,res=100)
power.bn<-ggplot(power,aes(x=type,y=wealth))+stat_bin2d(aes(binwidth=20,colour="black"))+coord_flip()+
    scale_x_continuous(breaks=0:4,labels=agent.types)+ylab("Agent Wealth Frequency")+xlab("Agent Type Frequency")+
    opts(title="Two-Dimensional Histogram of Agent Types and Wealth for Uniform Networks")+
    scale_fill_gradient(low="gainsboro",high="black",name="Agent Counts",breaks=seq(2000,14000,by=2000))+
    scale_y_continuous(breaks=seq(0,120,20))+scale_colour_grey(legend=FALSE)+theme_bw()
print(power.bn)
dev.off()

# Return to root directory
setwd("../..")


# Gridded plots of agent wealtha and contributions levels, by agent type and provision point
wc.dir<-"wealth_contrib"
dir.create(paste(img.dir,"/",wc.dir,sep=""))
setwd(paste(img.dir,"/",wc.dir,sep=""))


png("uniform_wc.png",width=1000,height=700,res=100)
uniform.wc<-ggplot(subset(uniform,uniform$disposition>0),aes(x=wealth,y=contrib))+
    geom_jitter(position=position_jitter(height=0,width=4),aes(colour=as.factor(threshold_met),alpha=.5))+
    scale_colour_manual(values=c("gray","gray"),legend=FALSE)+scale_alpha(legend=FALSE)+
    ylab("Agent Contribition Level")+xlab("Agent Wealth")+
    opts(title="Agent Contribution Level and Wealth for Pareto Networks by\n Agent Types and Provision Point for Uniform Networks")+
    facet_grid(threshold_met~type,labeller=grid.lab)+theme_bw()
print(uniform.wc)
dev.off()

png("pareto_wc.png",width=1000,height=700,res=100)
pareto.wc<-ggplot(subset(pareto,pareto$disposition>0),aes(x=wealth,y=contrib))+
    geom_jitter(position=position_jitter(height=0,width=4),aes(colour=as.factor(threshold_met),alpha=.5))+
    scale_colour_manual(values=c("gray","gray"),legend=FALSE)+scale_alpha(legend=FALSE)+
    ylab("Agent Contribition Level")+xlab("Agent Wealth")+
    opts(title="Agent Contribution Level and Wealth for Pareto Networks by\n Agent Types and Provision Point for Pareto Networks")+
    facet_grid(threshold_met~type,labeller=grid.lab)+theme_bw()
print(pareto.wc)
dev.off()

png("binom_wc.png",width=1000,height=700,res=100)
binom.wc<-ggplot(subset(binom,binom$disposition>0),aes(x=wealth,y=contrib))+
    geom_jitter(position=position_jitter(height=0,width=4),aes(colour=as.factor(threshold_met),alpha=.5))+
    scale_colour_manual(values=c("gray","gray"),legend=FALSE)+scale_alpha(legend=FALSE)+
    ylab("Agent Contribition Level")+xlab("Agent Wealth")+
    opts(title="Agent Contribution Level and Wealth for Pareto Networks by\n Agent Types and Provision Point for Binomial Networks")+
    facet_grid(threshold_met~type,labeller=grid.lab)+theme_bw()
print(binom.wc)
dev.off()

png("pref_wc.png",width=1000,height=700,res=100)
pref.wc<-ggplot(subset(pref,pref$disposition>0),aes(x=wealth,y=contrib))+
    geom_jitter(position=position_jitter(height=0,width=4),aes(colour=as.factor(threshold_met),alpha=.5))+
    scale_colour_manual(values=c("gray","gray"),legend=FALSE)+scale_alpha(legend=FALSE)+
    ylab("Agent Contribition Level")+xlab("Agent Wealth")+
    opts(title="Agent Contribution Level and Wealth for Pareto Networks by\n Agent Types and Provision Point for Prefential Attachment Networks")+
    facet_grid(threshold_met~type,labeller=grid.lab)+theme_bw()
print(pref.wc)
dev.off()

png("power_wc.png",width=1000,height=700,res=100)
power.wc<-ggplot(subset(power,power$disposition>0),aes(x=wealth,y=contrib))+
    geom_jitter(position=position_jitter(height=0,width=4),aes(colour=as.factor(threshold_met),alpha=.5))+
    scale_colour_manual(values=c("gray","gray"),legend=FALSE)+scale_alpha(legend=FALSE)+
    ylab("Agent Contribition Level")+xlab("Agent Wealth")+
    opts(title="Agent Contribution Level and Wealth for Pareto Networks by\n Agent Types and Provision Point for Power-law Networks")+
    facet_grid(threshold_met~type,labeller=grid.lab)+theme_bw()
print(power.wc)
dev.off()

# Return to root
setwd("../..")

#### MODEL ESTIMATES ####

# Basic probit on provision point
probit.uniform<-zelig(threshold_met~contrib+wealth+num_neighbors+threshold+Is.Community+Is.Minmatch+Is.Maxmatch+Is.Miserly+Is.Altruistic,
    model="probit",data=uniform)
summary(probit.uniform)

probit.pareto<-zelig(threshold_met~contrib+wealth+num_neighbors+threshold+Is.Community+Is.Minmatch+Is.Maxmatch+Is.Miserly+Is.Altruistic,
    model="probit",data=pareto)
summary(probit.pareto)

probit.power<-zelig(threshold_met~contrib+wealth+num_neighbors+threshold+Is.Community+Is.Minmatch+Is.Maxmatch+Is.Miserly+Is.Altruistic,
    model="probit",data=power)
summary(probit.power)

probit.pref<-zelig(threshold_met~contrib+wealth+num_neighbors+threshold+Is.Community+Is.Minmatch+Is.Maxmatch+Is.Miserly+Is.Altruistic,
    model="probit",data=pref)
summary(probit.pref)

probit.binom<-zelig(threshold_met~contrib+wealth+num_neighbors+threshold+Is.Community+Is.Minmatch+Is.Maxmatch+Is.Miserly+Is.Altruistic,
    model="probit",data=binom)
summary(probit.binom)

# GLM in binomial family with logit link on level of contribution (contrib \in [0,1])
glm.uniform<-glm(contrib~wealth+num_neighbors+threshold+threshold_met+Is.Community+Is.Minmatch+Is.Maxmatch+Is.Miserly+Is.Altruistic,
    family=binomial(link="logit"),data=uniform)
summary(glm.uniform)

glm.pareto<-glm(contrib~wealth+num_neighbors+threshold+threshold_met+Is.Community+Is.Minmatch+Is.Maxmatch+Is.Miserly+Is.Altruistic,
    family=binomial(link="logit"),data=pareto)
summary(glm.pareto)

glm.power<-glm(contrib~wealth+num_neighbors+threshold+threshold_met+Is.Community+Is.Minmatch+Is.Maxmatch+Is.Miserly+Is.Altruistic,
    family=binomial(link="logit"),data=power)
summary(glm.power)

glm.pref<-glm(contrib~wealth+num_neighbors+threshold+threshold_met+Is.Community+Is.Minmatch+Is.Maxmatch+Is.Miserly+Is.Altruistic,
    family=binomial(link="logit"),data=pref)
summary(glm.pref)

glm.binom<-glm(contrib~wealth+num_neighbors+threshold+threshold_met+Is.Community+Is.Minmatch+Is.Maxmatch+Is.Miserly+Is.Altruistic,
    family=binomial(link="logit"),data=binom)
summary(glm.binom)

