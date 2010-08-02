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
    binom.dummy.list[[i+1]]<-sapply(binom$type,function(t) ifelse(t==i,1,0))
}
binom.dummy.df<-as.data.frame(do.call("cbind",binom.dummy.list))
binom.dummy.df<-transform(binom.dummy.df,id=1:nrow(binom))
colnames(binom.dummy.df)<-dummy.cols
binom<-merge(binom,binom.dummy.df)

# Pareto dummies
pareto.dummy.list<-list()
for(i in 0:4){
    pareto.dummy.list[[i+1]]<-sapply(pareto$type,function(t) ifelse(t==i,1,0))
}
pareto.dummy.df<-as.data.frame(do.call("cbind",pareto.dummy.list))
pareto.dummy.df<-transform(pareto.dummy.df,id=1:nrow(pareto))
colnames(pareto.dummy.df)<-dummy.cols
pareto<-merge(pareto,pareto.dummy.df)

# Power-law dummies
power.dummy.list<-list()
for(i in 0:4){
    power.dummy.list[[i+1]]<-sapply(power$type,function(t) ifelse(t==i,1,0))
}
power.dummy.df<-as.data.frame(do.call("cbind",power.dummy.list))
power.dummy.df<-transform(power.dummy.df,id=1:nrow(power))
colnames(power.dummy.df)<-dummy.cols
power<-merge(power,power.dummy.df)

# Preferential attachment dummies
pref.dummy.list<-list()
for(i in 0:4){
    pref.dummy.list[[i+1]]<-sapply(pref$type,function(t) ifelse(t==i,1,0))
}
pref.dummy.df<-as.data.frame(do.call("cbind",pref.dummy.list))
pref.dummy.df<-transform(pref.dummy.df,id=1:nrow(pref))
colnames(pref.dummy.df)<-dummy.cols
pref<-merge(pref,pref.dummy.df)

# Uniform dummies
uniform.dummy.list<-list()
for(i in 0:4){
    uniform.dummy.list[[i+1]]<-sapply(uniform$type,function(t) ifelse(t==i,1,0))
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
binom.dd<-ggplot(binom,aes(x=num_neighbors))+geom_histogram(aes(fill="Degree Distribution"),binwidth=1)+
    scale_fill_manual(values=c("Degree Distribution"="darkred"),name="")+
    opts(title="Degree Distribution for Binomial Networks")+
    xlab("Degree")+ylab("Frequency")
print(binom.dd)
dev.off()

png("pareto_dd.png",height=600,width=600,res=100)
pareto.dd<-ggplot(pareto,aes(x=num_neighbors))+geom_histogram(aes(fill="Degree Distribution"),binwidth=1)+
    scale_fill_manual(values=c("Degree Distribution"="darkred"),name="")+
    opts(title="Degree Distribution for Pareto Networks")+
    xlab("Degree")+ylab("Frequency")
print(pareto.dd)
dev.off()

png("power_dd.png",height=600,width=600,res=100)
power.dd<-ggplot(power,aes(x=num_neighbors))+geom_histogram(aes(fill="Degree Distribution"),binwidth=1)+
    scale_fill_manual(values=c("Degree Distribution"="darkred"),name="")+
    opts(title="Degree Distribution for Power-law Networks")+
    xlab("Degree")+ylab("Frequency")
print(power.dd)
dev.off()

png("pref_dd.png",height=600,width=600,res=100)
pref.dd<-ggplot(pref,aes(x=num_neighbors))+geom_histogram(aes(fill="Degree Distribution"),binwidth=1)+
    scale_fill_manual(values=c("Degree Distribution"="darkred"),name="")+
    opts(title="Degree Distribution for Wealth-based\nPreferential Attachment Networks")+
    xlab("Degree")+ylab("Frequency")
print(pref.dd)
dev.off()

png("uniform_dd.png",height=600,width=600,res=100)
uniform.dd<-ggplot(uniform,aes(x=num_neighbors))+geom_histogram(aes(fill="Degree Distribution"),binwidth=1)+
    scale_fill_manual(values=c("Degree Distribution"="darkred"),name="")+
    opts(title="Degree Distribution for Uniform Networks")+
    xlab("Degree")+ylab("Frequency")
print(uniform.dd)
dev.off()

# Return to root directory
setwd("../..")

# Gridded plots of agent wealth by contributions levels, disaggregated by agent
# type and provision point
wc.dir<-"wealth_contrib"
dir.create(paste(img.dir,"/",wc.dir,sep=""))
setwd(paste(img.dir,"/",wc.dir,sep=""))


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


png("uniform_wc.png",width=1000,height=700,res=100)
uniform.wc<-ggplot(subset(uniform,uniform$disposition>0),aes(x=wealth,y=contrib))+
    geom_jitter(position=position_jitter(height=0,width=4),aes(colour=as.factor(threshold_met),alpha=.5))+
    scale_colour_manual(values=c("darkred","darkblue"),legend=FALSE)+scale_alpha(legend=FALSE)+
    ylab("Agent Contribition Level")+xlab("Agent Wealth")+
    opts(title="Agent Contribution Level by Wealth for Uniform Networks\n[Agent Types][Provision Point]")+
    facet_grid(threshold_met~type,labeller=grid.lab)
print(uniform.wc)
dev.off()

png("pareto_wc.png",width=1000,height=700,res=100)
pareto.wc<-ggplot(subset(pareto,pareto$disposition>0),aes(x=wealth,y=contrib))+
    geom_jitter(position=position_jitter(height=0,width=4),aes(colour=as.factor(threshold_met),alpha=.5))+
    scale_colour_manual(values=c("darkred","darkblue"),legend=FALSE)+scale_alpha(legend=FALSE)+
    ylab("Agent Contribition Level")+xlab("Agent Wealth")+
    opts(title="Agent Contribution Level by Wealth for Pareto Networks\n[Agent Types][Provision Point]")+
    facet_grid(threshold_met~type,labeller=grid.lab)
print(pareto.wc)
dev.off()

png("binom_wc.png",width=1000,height=700,res=100)
binom.wc<-ggplot(subset(binom,binom$disposition>0),aes(x=wealth,y=contrib))+
    geom_jitter(position=position_jitter(height=0,width=4),aes(colour=as.factor(threshold_met),alpha=.5))+
    scale_colour_manual(values=c("darkred","darkblue"),legend=FALSE)+scale_alpha(legend=FALSE)+
    ylab("Agent Contribition Level")+xlab("Agent Wealth")+
    opts(title="Agent Contribution Level by Wealth for Binomial Networks\n[Agent Types][Provision Point]")+
    facet_grid(threshold_met~type,labeller=grid.lab)
print(binom.wc)
dev.off()

png("pref_wc.png",width=1000,height=700,res=100)
pref.wc<-ggplot(subset(pref,pref$disposition>0),aes(x=wealth,y=contrib))+
    geom_jitter(position=position_jitter(height=0,width=4),aes(colour=as.factor(threshold_met),alpha=.5))+
    scale_colour_manual(values=c("darkred","darkblue"),legend=FALSE)+scale_alpha(legend=FALSE)+
    ylab("Agent Contribition Level")+xlab("Agent Wealth")+
    opts(title="Agent Contribution Level by Wealth for Preferential Attachment Networks\n[Agent Types][Provision Point]")+
    facet_grid(threshold_met~type,labeller=grid.lab)
print(pref.wc)
dev.off()

png("power_wc.png",width=1000,height=700,res=100)
power.wc<-ggplot(subset(power,power$disposition>0),aes(x=wealth,y=contrib))+
    geom_jitter(position=position_jitter(height=0,width=4),aes(colour=as.factor(threshold_met),alpha=.5))+
    scale_colour_manual(values=c("darkred","darkblue"),legend=FALSE)+scale_alpha(legend=FALSE)+
    ylab("Agent Contribition Level")+xlab("Agent Wealth")+
    opts(title="Agent Contribution Level by Wealth for Uniform Networks\n[Agent Types][Provision Point]")+
    facet_grid(threshold_met~type,labeller=grid.lab)
print(power.wc)
dev.off()

#### MODEL ESTIMATES ####

# Probit on provision point
# Uniform networks
probit.uniform<-zelig(threshold_met~contrib+wealth+num_neighbors+threshold+Is.Altruistic+Is.Community+Is.Minmatch+Is.Maxmatch+Is.Miserly,
    model="probit",data=uniform)
summary(probit.uniform)

# Pareto networks
probit.pareto<-zelig(threshold_met~contrib+wealth+num_neighbors+threshold+Is.Altruistic+Is.Community+Is.Minmatch+Is.Maxmatch+Is.Miserly,
    model="probit",data=pareto)
summary(probit.pareto)

# Power-law networks
probit.power<-zelig(threshold_met~contrib+wealth+num_neighbors+threshold+Is.Altruistic+Is.Community+Is.Minmatch+Is.Maxmatch+Is.Miserly,
    model="probit",data=power)
summary(probit.power)

# Preferential networks
probit.pref<-zelig(threshold_met~contrib+wealth+num_neighbors+threshold+Is.Altruistic+Is.Community+Is.Minmatch+Is.Maxmatch+Is.Miserly,
    model="probit",data=pref)
summary(probit.pref)

# Uniform networks
probit.uniform<-zelig(threshold_met~contrib+wealth+num_neighbors+threshold+Is.Altruistic+Is.Community+Is.Minmatch+Is.Maxmatch+Is.Miserly,
    model="probit",data=uniform)
summary(probit.uniform)

