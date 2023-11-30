# Conversion of mycotoxin-contaminated maize by black soldier fly larvae into feed and fertilizer
# Journal of Insects as Food and Feed
# All analyses related to frass temperature and larval growth, as well as substrate, frass and larvae composition


#--------# 

rm(list=ls()) 

library(extrafont)
library(openxlsx)
library(tidyverse)
library(stringr)
library(gridExtra)
library(grid)
library(ggpubr)
library(corrplot)
library(car)
library(cowplot)
library(gridExtra)
library(ungeviz)
library(multcomp)
library(emmeans)
library(conflicted)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

# import data -----

  # larval weight

  larval_weight_messy <- openxlsx::read.xlsx(xlsxFile = "data/larval_mass.xlsx",sheet = 1) %>% 
    
    filter(!afla_level=="MEDIUM") %>% 
  
    mutate(afla_level=factor(afla_level)) %>% 
    
    mutate(maize_inclusion_perc=factor(maize_inclusion_perc)) %>% 
    
    mutate(day=factor(day)) %>% 
    
    mutate(replicate=factor(replicate)) 
  
  # crate temperature
  
  crate_temp_messy <-  openxlsx::read.xlsx(xlsxFile = "data/crate_temp.xlsx",sheet = 1) %>% 
    
    mutate(afla_level=factor(afla_level)) %>% 
    
    mutate(maize_inclusion_perc=factor(maize_inclusion_perc)) %>% 
    
    mutate(day=factor(day)) 
  
  # ambient temperature and relative humidity
  
  ambient_temp_messy <-  openxlsx::read.xlsx(xlsxFile = "data/environmental_conditions.xlsx",sheet = 1) %>% 
    
    mutate(Day=factor(Day),
           Position=factor(Position))

  # substrate, frass and larval chemical composition
  
  chem_ana_messy <-  openxlsx::read.xlsx(xlsxFile =  "data/substrate_frass_larvae.xlsx",sheet = 1) %>% 
    
    filter(!afla_level=="MEDIUM") %>% 
  
    mutate(value=as.numeric(as.character(value))) %>% 
    
    mutate(afla_level=factor(afla_level)) %>% 
    
    mutate(maize_inclusion_perc=factor(maize_inclusion_perc)) %>% 
    
    mutate(parameter=as.factor(parameter))
  
  
  # set order 
  
  larval_weight_messy$afla_level <- factor(larval_weight_messy$afla_level, levels = c("CONTROL","LOW","HIGH"))
  chem_ana_messy$afla_level <- factor(chem_ana_messy$afla_level, levels = c("CONTROL","LOW","HIGH"))
  
# analyze environmental data
  
  # gather data
  
  ambient_temp_gather <-
  
  ambient_temp_messy %>% 
    
    gather(4:7,key="parameter",value="value") %>% 
    
    mutate(parameter=factor(parameter)) %>% 
    
    filter(!value=="no") %>% 
    
    na.omit(value) %>% 
    
    mutate(value=as.numeric(value))
  
  # mean and range daytime temperature
  
  ambient_temp_gather %>% 
    
    group_by(Day,parameter) %>% 
    
    filter(!parameter=="maxT_C",
           !parameter=="minT_C") %>% 
    
    summarise(mean=mean(value,na.rm = T),
              n=n(),
              max=max(value,na.rm = T),
              min=min(value,na.rm = T)) 
  
  
  # max & min temperature per day
  
  ambient_temp_gather %>% 

    group_by(Day,parameter) %>% 
    
    filter(parameter=="maxT_C" |
           parameter=="minT_C") %>% 
    
    summarise(mean=mean(value)) %>% 
    
    spread(key="parameter",value="mean")
  
  # max & min temperature over all days
  
  ambient_temp_gather %>% 
    
    group_by(Day,parameter) %>% 
    
    filter(parameter=="maxT_C" |
             parameter=="minT_C") %>% 
    
    summarise(mean=mean(value)) %>% 
    
    ungroup(Day) %>% 
    
    group_by(parameter) %>% 
    
    summarise(n=n(),
              mean=mean(mean))
    
  # mean and range daytime temperature over all days
  
  ambient_temp_gather %>% 
    
    group_by(Day,parameter) %>% 
    
    filter(!parameter=="maxT_C",
           !parameter=="minT_C") %>% 
    
    summarise(mean=mean(value,na.rm = T),
              max=max(value,na.rm = T),
              min=min(value,na.rm = T)) %>% 
    
    gather(3:5,key="parameter_new",value="value") %>% 
    
    group_by(parameter,parameter_new) %>% 
    
    summarise(n=n(),
              mean=mean(value))
  
  
# calculate larval weight -----

  # select relevant columns

  larval_weight <- 

  larval_weight_messy %>% 
    
    select(-time,-container_weight_g)
  
  # calculate larval weight
  
  larval_weight <-
    
    larval_weight %>% 
    
    mutate(larval_weight_mg = (weight_larvae_g/number_larvae)*1000)

  # calculate mean of the same replicate
  
  larval_weight_rep_mean <-

  larval_weight %>%

    group_by(afla_level,maize_inclusion_perc,day, replicate) %>%

    summarise(larval_weight_mg=mean(larval_weight_mg))
  
  # summarise repliace mean data

  larval_weight %>% 
    
    summarise(n=n())
  
  # calculate mean and sd per treatment
  
  larval_weight_mean <- 
    
    larval_weight_rep_mean %>% 
    
    ungroup(replicate) %>% 
    
    summarise(n=n(),
              mean=mean(larval_weight_mg),
              sd=sd(larval_weight_mg))

  # plot influence of aflatoxin level on larval weight ------
  
    # day 12 - as function of mycotoxin contamination
    
    # 50 %DM maize
  
    larval_weight_rep_mean_12_50 <-
      
      larval_weight_rep_mean %>% 
    
      filter(afla_level == "CONTROL" | afla_level == "LOW" | afla_level == "HIGH") %>%
    
      filter(maize_inclusion_perc == 50 | maize_inclusion_perc ==80) %>% 
    
      filter(day==12) %>% 
    
      mutate(maize_inclusion_perc=case_when(maize_inclusion_perc == 50 ~"50 %DM maize",
                                            maize_inclusion_perc == 80 ~"80 %DM maize",
                               TRUE~"error")) %>% 
    
      mutate(afla_level=case_when(afla_level == "CONTROL" ~"CM",
                                  afla_level == "LOW" ~"LM",
                                  afla_level == "HIGH" ~"HM",
                                          TRUE~"error")) %>% 
    
      filter(maize_inclusion_perc =="50 %DM maize")
    
  
  
      larval_weight_mean %>% 
        
      filter(afla_level == "CONTROL" | afla_level == "LOW" | afla_level == "HIGH") %>%
        
      filter(maize_inclusion_perc == 50 | maize_inclusion_perc ==80) %>%
      
      filter(day==12) %>% 
      
      mutate(maize_inclusion_perc=case_when(maize_inclusion_perc == 50 ~"50 %DM maize",
                                              maize_inclusion_perc == 80 ~"80 %DM maize",
                                              TRUE~"error"))   %>% 
        
      filter(maize_inclusion_perc =="50 %DM maize") %>% 
        
      mutate(afla_level=case_when(afla_level == "CONTROL" ~"CM",
                                   afla_level == "LOW" ~"LM",
                                   afla_level == "HIGH" ~"HM",
                                   TRUE~"error")) %>% 
        
      ungroup() %>% 
      
      ggplot(aes(afla_level,mean)) +
      
      geom_point() +
      
      geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),width = 0.3)+
      
      geom_jitter(data=larval_weight_rep_mean_12_50,
                  aes(afla_level,larval_weight_mg),
                  size=2,
                  position = position_jitter(0.05),
                  alpha=.4) +
    
      theme_bw(base_size = 12) +
      
      ylab("mass per larva (mg") +
      
      xlab("") +

      ggtitle("(a) 50%DM maize") +
      
      # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      
      ylim(0,140) +
        
      geom_text(aes(label=c("a","a","a")),nudge_x = .2)
    
    ggsave("output/larval_weight/larval_weight_day12_myco_50.jpeg", dpi = 1200, width = 6,height = 14 , units = "cm")
    
    # 80 %DM maize
    
    larval_weight_rep_mean_12_80 <-
      
      larval_weight_rep_mean %>% 
      
      filter(afla_level == "CONTROL" | afla_level == "LOW" | afla_level == "HIGH") %>%
      
      filter(maize_inclusion_perc == 50 | maize_inclusion_perc ==80) %>% 
      
      filter(day==12) %>% 
      
      mutate(maize_inclusion_perc=case_when(maize_inclusion_perc == 50 ~"50 %DM maize",
                                            maize_inclusion_perc == 80 ~"80 %DM maize",
                                            TRUE~"error")) %>% 
      
      mutate(afla_level=case_when(afla_level == "CONTROL" ~"CM",
                                  afla_level == "LOW" ~"LM",
                                  afla_level == "HIGH" ~"HM",
                                  TRUE~"error")) %>% 
      
      filter(maize_inclusion_perc =="80 %DM maize")
    
    
    
    larval_weight_mean %>% 
      
      filter(afla_level == "CONTROL" | afla_level == "LOW" | afla_level == "HIGH") %>%
      
      filter(maize_inclusion_perc == 50 | maize_inclusion_perc ==80) %>%
      
      filter(day==12) %>% 
      
      mutate(maize_inclusion_perc=case_when(maize_inclusion_perc == 50 ~"50 %DM maize",
                                            maize_inclusion_perc == 80 ~"80 %DM maize",
                                            TRUE~"error"))   %>% 
      
      filter(maize_inclusion_perc =="80 %DM maize") %>% 
      
      mutate(afla_level=case_when(afla_level == "CONTROL" ~"CM",
                                  afla_level == "LOW" ~"LM",
                                  afla_level == "HIGH" ~"HM",
                                  TRUE~"error")) %>% 
      
      ungroup() %>% 
      
      ggplot(aes(afla_level,mean)) +
      
      geom_point() +
      
      geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),width = 0.3)+
      
      geom_jitter(data=larval_weight_rep_mean_12_80,
                  aes(afla_level,larval_weight_mg),
                  size=2,
                  position = position_jitter(0.05),
                  alpha=.4) +
      
      theme_bw(base_size = 12) +
      
      ylab("mass per larva (mg") +
      
      xlab("") +
      
      ggtitle("(b) 80%DM maize") +
      
      # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      
      ylim(0,140) +
      
      geom_text(aes(label=c("a","a","a")),nudge_x = .2)
    
    ggsave("output/larval_weight/larval_weight_day12_myco_80.jpeg", dpi = 1200, width = 6,height = 14 , units = "cm")
    
    
    # annova
    
      #50% DM maize
    
      larval_weight_rep_mean_12_50maize <-
    
      larval_weight_rep_mean_12 %>% 
        
        filter(maize_inclusion_perc=="50 %DM maize")
      
      
      # set up model
      model <- lm(larval_weight_mg ~ afla_level, data = larval_weight_rep_mean_12_50maize)
      
      # get (adjusted) weight means per group
      model_means <- emmeans(object = model,
                             specs = "afla_level")
      
      # add letters to each mean
      model_means_cld <- cld(object = model_means,
                             adjust = "tukey",
                             Letters = letters,
                             alpha = 0.05)
      
      # show output
      model_means_cld
      
      
      #80% DM maize
    
      larval_weight_rep_mean_12_80maize <-
        
        larval_weight_rep_mean_12 %>% 
        
        filter(maize_inclusion_perc=="80 %DM maize")
      
      
      # set up model
      model <- lm(larval_weight_mg ~ afla_level, data = larval_weight_rep_mean_12_80maize)
      
      # get (adjusted) weight means per group
      model_means <- emmeans(object = model,
                             specs = "afla_level")
      
      # add letters to each mean
      model_means_cld <- cld(object = model_means,
                             adjust = "tukey",
                             Letters = letters,
                             alpha = 0.05)
      
      # show output
      model_means_cld
    
    
    # day 12 - as function of maize inclusion
    
    larval_weight_mean_maize_inclusion <-
      
      larval_weight_rep_mean %>% 
      
      ungroup() %>% 
      
      group_by(maize_inclusion_perc) %>% 
      
      filter(afla_level == "CONTROL" | afla_level == "LOW" | afla_level == "HIGH") %>%
      
      filter(maize_inclusion_perc == 50 | maize_inclusion_perc ==80) %>% 
      
      filter(day==12) %>% 
      
      mutate(maize_inclusion_perc=case_when(maize_inclusion_perc == 50 ~"50 %DM maize",
                                            maize_inclusion_perc == 80 ~"80 %DM maize",
                                            TRUE~"error")) %>% 
      
      mutate(afla_level=case_when(afla_level == "CONTROL" ~"CM",
                                  afla_level == "LOW" ~"LM",
                                  afla_level == "HIGH" ~"HM",
                                  TRUE~"error")) %>% 
      
      summarise(n=n(),
                mean=mean(larval_weight_mg),
                sd=sd(larval_weight_mg)) 
    
    
    larval_weight_mean_maize_inclusion %>% 
      
      ungroup() %>% 
      
      ggplot(aes(maize_inclusion_perc,mean)) +
      
      geom_point() +
      
      geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),width = 0.3)+
      
      geom_jitter(data=larval_weight_rep_mean_12,
                  aes(maize_inclusion_perc,larval_weight_mg, shape=afla_level),
                  size=2,
                  position = position_jitter(0.05),
                  alpha=.4) +
      
      theme_bw(base_size = 12) +
      
      ylab("mass per larva (mg)") +
      
      xlab("") +

      ggtitle("(c) All samples") +
      
      theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust=0.5)) + 
      
      ylim(0,140) +
      
      scale_shape_discrete(name = "Mycotoxin level") +
      
      geom_text(aes(label=c("b","a")),nudge_x = .15)
      

    
    ggsave("output/larval_weight/larval_weight_day12_maize.jpeg", dpi = 1200, width = 14,height = 14 , units = "cm")
    
    
    # anova 
    
    # set up model
    model <- lm(larval_weight_mg ~ maize_inclusion_perc, data = larval_weight_rep_mean_12)
    
    # get (adjusted) weight means per group
    model_means <- emmeans(object = model,
                           specs = "maize_inclusion_perc")
    
    # add letters to each mean
    model_means_cld <- cld(object = model_means,
                           adjust = "tukey",
                           Letters = letters,
                           alpha = 0.05)
    
    # show output
    model_means_cld
    
    
  
  # analyze crate temperature ------
  
    # transform data in clean form
  
    crate_temp <- 
    
    crate_temp_messy %>% 
      
      gather(5:7,key = "time",value = "temperature_C") %>% 
      
      mutate(time=factor(time))
    
  
    # calculate mean crate temp between substrates for each rearing day
    
    crate_temp_descriptive <- 
    
    crate_temp %>% 
      
      group_by(afla_level, maize_inclusion_perc,day) %>% # calculate mean over all replicates of same treatment
      
      summarise(mean_T=mean(temperature_C)) %>% 
      
      filter(!afla_level=="MEDIUM",
             !maize_inclusion_perc=="65") %>% 
      
      unite(treatment,afla_level,maize_inclusion_perc,sep="") %>% 
      
      spread(3,key="treatment",value="mean_T")

    
    # write to excel
    
    write.xlsx(as.data.frame(crate_temp_descriptive), file = "output/bugpic_crate_temp.xlsx",sheetName = "data", append = FALSE)
    
    
    # calculate mean crate temp between substrates over entire experiment
    
    crate_temp_descriptive  %>% 
      
      gather(2:7,key="treatment",value="value") %>% 
      
      group_by(treatment) %>% 
      
      summarise(n=n(),
                mean=mean(value),
                sd=sd(value))
      
    
    
      # substrate composition -----
    
      chem_ana_messy %>% 
      
      filter(sample_type == "substrate") %>% 
      
      group_by(parameter,maize_inclusion_perc) %>% 
      
      filter(afla_level == "CONTROL" | afla_level == "LOW" | afla_level == "HIGH") %>%
      
      filter(maize_inclusion_perc == 40 | maize_inclusion_perc ==80) %>% 
      
      summarise(n=n(),
                mean=round(mean(value),1),
                sd=round(sd(value),1))
      
      
      # substrate DM
    
      substrate_DM <- 
        
        chem_ana_messy %>% 
        
        filter(sample_type == "substrate") %>% 
        
        filter(parameter=="DM_perc") %>% 
        
        mutate(DM=value) %>% 
        
        select(afla_level,maize_inclusion_perc,DM)

      
    # raw maize composition -----
    
    
    chem_ana_messy %>% 
      
      filter(sample_type =="Raw_maize")
    
    
    # larvae composition -----
    
    # correct N:P ratio from 6.25 to 4.7
    
      chem_ana_larvae <-
      
      chem_ana_messy %>% 
      
      filter(sample_type == "larvae" | sample_type =="fishmeal"|sample_type=="soybean") %>% 
      
      filter(!crop_name=="Amino acids") %>%   
      
      spread(key = parameter,value = value) %>% 
      
      mutate(Protein_perc=as.numeric(Protein_perc)) %>% 
      
      mutate(Protein_perc=case_when(sample_type=="larvae" ~ ((Protein_perc/6.25)*4.7),
             sample_type=="fishmeal" ~ ((Protein_perc/6.25)*6.25),
             sample_type=="soybean"~ ((Protein_perc/6.25)*5.7) , TRUE~2)) %>% 
      
      gather(9:14,key = parameter,value = value,na.rm = T)
    
    
    # mean of all larvae results

    larvae_mean_save <- 
    
    chem_ana_larvae %>% 
      
      filter(sample_type=="larvae" | sample_type =="fishmeal" | sample_type=="soybean") %>% 

      filter(!crop_name=="Amino acids") %>%   
      
      ungroup() %>% 
      
      group_by(maize_inclusion_perc,sample_type,parameter) %>% 
      
      filter(is.na(maize_inclusion_perc) | maize_inclusion_perc==40 | maize_inclusion_perc ==80) %>% 

      filter(is.na(afla_level)|(afla_level=="CONTROL" | afla_level =="LOW" | afla_level =="HIGH")) %>% 

      summarize(n=n(),
                mean=round(mean(value),1),
                sd=round(sd(value),1)) %>% 
      
      ungroup() %>% 
      
      select(-n) %>% 
      
      unite(label,mean,sd,sep=" (") %>%
      
      mutate(help=")") %>%
      
      unite(mean_sd,label,help,sep="") %>% 
      
      spread(parameter,mean_sd)
    
    # write to excel
    
    write.xlsx(as.data.frame(larvae_mean_save ), file = "output/bugpic_larvae_nutrients.xlsx",sheetName = "data", append = FALSE)
    
    
    # larvae DM
    
    larvae_DM <- 
      
      chem_ana_larvae %>% 
      
      filter(sample_type == "larvae") %>% 
      
      filter(parameter=="DM_perc") %>% 
      
      mutate(DM=value) %>% 
      
      select(afla_level,maize_inclusion_perc,DM)
    
    
    # frass composition -----
    
    frass_param <-
    
    chem_ana_messy %>% 
      
      filter(sample_type == "frass") %>% 
      
      drop_na(value) %>% 
      
      select(parameter)
    
    levels(frass_param$parameter)
    
    # mean of all results
    
    chem_frass_sum <- 
    
    chem_ana_messy %>% 
      
      filter(sample_type == "frass") %>% 
      
      filter(!maize_inclusion_perc==60) %>% 
      
      filter(!afla_level=="MEDIUM") %>% 
      
      group_by(parameter,maize_inclusion_perc) %>% 
      
      filter(!crop_name=="Amino acids") %>%   
      
      separate(parameter,into = c("parameter","unit")) %>% 
      
      mutate(value=case_when(unit == "mScm" ~ value*1000,
                             unit == "perc" ~ value*10,
                             unit == "ppm" ~ value*1,
                                            TRUE~value)) %>% 
      
      mutate(unit=case_when(unit == "mScm" ~ "uScm",
                             unit == "perc" ~ "g_kg",
                             unit == "ppm" ~ "mg_kg",
                             TRUE~unit)) %>% 
      
      unite(parameter,parameter,unit) %>% 
      
      group_by(parameter,maize_inclusion_perc) %>% 
      
      summarize(n=n(),
                mean=round(mean(value),1),
                sd=round(sd(value),1)) %>% 
      
      unite(label,mean,sd,sep=" (") %>%
      
      mutate(help=")") %>%
      
      unite(mean_sd,label,help,sep="") %>% 
      
      select(-n) %>% 
      
      spread(key = maize_inclusion_perc,value=mean_sd)
    
    # write to excel
    
    write.xlsx(as.data.frame(chem_frass_sum), file = "output/bugpic_frass_sum.xlsx",sheetName = "data", append = FALSE)
    
    
    # amino acid analyses -----
    
    # true protein 
    
       # correct for dry mass
    
      chem_ana_AA_DM <- 
    
      chem_ana_messy %>% 
      
      filter(sample_type == "soybean" | sample_type == "fishmeal" |
               
               sample_type == "larvae") %>% 
      
      filter(parameter=="DM_perc") %>% 
      
      filter(afla_level =="CONTROL" | sample_type == "soybean" | sample_type == "fishmeal" ) 
    
    
    chem_ana_messy %>% 
      
      filter(crop_name=="Amino acids") %>% 
      
      group_by(sample_type,afla_level,maize_inclusion_perc) %>% 
                 
      summarise(n=n(),
                sum=sum(value,na.rm = T)) %>% 
      
      left_join(chem_ana_AA_DM,by = c("sample_type","afla_level","maize_inclusion_perc")) %>% 
      
      select(sample_type,afla_level, maize_inclusion_perc,n,sum,parameter,value,sample_type) %>% 
      
      mutate(sum_tprotein_percDM = ((sum/value)*100))
      