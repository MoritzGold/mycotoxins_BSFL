# Conversion of mycotoxin-contaminated maize by black soldier fly larvae into feed and fertilizer
# Journal of Insects as Food and Feed
# All analyses related to mycotoxins in substrate, larvae and frass

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
library(xlsx)
library(broom)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(emmeans)
library(multcomp)
library(multcompView)
library(conflicted)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("write.xlsx", "xlsx")

parameter_levels = c("AFB1", "AFB2", "AFG1","AFG2","AFL","AFM1","AFP1","AFQ1","15AcDON","3AcDON","DAS","DON","DON3G","HT2","NIV","T2",
                     "a-ZEL","ZEN","b-ZEL","FB1", "FB2","FB3","ENNA","ENNA1","ENNB","ENNB1","BEA", "MON","NPA","STER","CIT", "MPA","OTA","PA","ROQ",
                     "AME","AOH","AC")

# import mycotoxin data -----

myco_messy <- openxlsx::read.xlsx(xlsxFile = "data/myco_results.xlsx",sheet = 1) %>% 
  
  mutate(afla_level=factor(afla_level)) %>% 
  
  mutate(maize_inclusion_perc=factor(maize_inclusion_perc)) %>% 
  
  mutate(sample_type=factor(sample_type)) %>% 
  
  mutate(replicate=factor(replicate)) %>% 
  
  select(-unit)


# import dry matter data -----

dry_mass <- xlsx::read.xlsx(file = "data/substrate_frass_larvae.xlsx",sheetIndex = 1) %>% 
  
  select(0:10) %>% 
  
  mutate(afla_level=factor(afla_level)) %>% 
  
  mutate(maize_inclusion_perc=factor(maize_inclusion_perc)) %>% 
  
  mutate(value=as.numeric(as.character(value))) %>% 
  
  mutate(parameter=as.factor(parameter)) %>% 
  
  filter(parameter=="DM_perc") %>% 
  
  select(-sample_code,-client_name,-farm_name,-crop_name,-Sample_Condition,-parameter)
  

colnames(dry_mass) <- c("afla_level","maize_inclusion_perc","sample_type","DM_perc")

# import LOD and LOQ levels -----

  #prepare data
  
  LOD_LOQ <- xlsx::read.xlsx(file = "data/LOD_LOQ.xlsx",sheetIndex = 1) %>% 
  
    group_by(sample_type,cutoff) %>% 
    
    gather(3:40,key = "parameter",value = "value") %>% 
    
    mutate(value=round(value,2)) %>% 
    
    ungroup() %>% 
    
    group_by(sample_type,parameter) %>% 
    
    spread(key = cutoff,value = value) %>% 
    
    mutate(parameter=case_when(parameter == "X15AcDON" ~"15AcDON",
                               parameter == "X3AcDON" ~"3AcDON",
                               parameter == "b.ZEL" ~"b-ZEL",
                               parameter == "a.ZEL" ~"a-ZEL",
                                TRUE~parameter)) %>% 
  
    mutate(parameter=as.factor(parameter))
  
    # sort mycotoxins-----
  
  LOD_LOQ$parameter <- factor(LOD_LOQ$parameter, levels = c("AFB1", "AFB2", "AFG1","AFG2","AFL","AFM1","AFP1","AFQ1","15AcDON","3AcDON","DAS","DON","DON3G","HT2","NIV","T2",
    "a-ZEL","ZEN","b-ZEL","FB1", "FB2","FB3","ENNA","ENNA1","ENNB","ENNB1","BEA", "MON","NPA","STER","CIT", "MPA","OTA","PA","ROQ",
    "AME","AOH","AC"))
  
  # sort sample type
  
  LOD_LOQ$sample_type <- factor(LOD_LOQ$sample_type, levels = c("Raw_maize","substrate","larvae","frass"))

  # save LOD and LOQ levels
  
    # LOQ save in Excel
  
    LOQ_save <-
  
    LOD_LOQ %>% 
      
      select(-LOD) %>% 
      
      spread(key = sample_type,value = LOQ)

    # LOQ save in Excel
    
    LOD_save <-
      
      LOD_LOQ %>% 
      
      select(-LOQ) %>% 
      
      spread(key = sample_type,value = LOD)
    
    # save both in one sheet
  
    write.xlsx(as.data.frame(LOQ_save), file = "output/bugpic_myco_LOD_LOQ.xlsx",sheetName = "LOQ", append = FALSE)
    write.xlsx(as.data.frame(LOD_save), file = "output/bugpic_myco_LOD_LOQ.xlsx",sheetName = "LOD", append = TRUE)
    
  
# sort factors

myco_messy$afla_level <- factor(myco_messy$afla_level, levels = c("clean","contam","CONTROL","LOW","HIGH"))
myco_messy$afla_level <- factor(myco_messy$afla_level, levels = c("clean","contam","CONTROL","LOW","HIGH"))
myco_messy$sample_type <- factor(myco_messy$sample_type, levels = c("Raw_maize","substrate","larvae","frass"))


# gather

myco_messy_narrow <-

myco_messy %>% 
  
  group_by(afla_level,sample_type,replicate) %>% 
  
  gather(6:43,key = "parameter",value = "value") %>% 
  
  mutate(parameter=factor(parameter))


# correct for dry mass

myco_messy_narrow_DM <-
  
  myco_messy_narrow %>% 
  
  left_join(dry_mass, by=c("sample_type","maize_inclusion_perc","afla_level")) %>% 
  
  mutate(value=as.numeric(value)) %>% 

  # filter(afla_level=="CONTROL" & sample_type =="frass")
  
  # filter(sample_type=="Raw_maize") %>% 
  
  mutate(value_DM=(value/DM_perc)*100)
  


# assign individual mycotoxins to mycotoxin groups ------

levels(myco$sample_type)

myco <- 

myco_messy_narrow_DM %>% 
  
  mutate(myco_group=case_when(parameter %in% c("AFB1", "AFB2", "AFG1","AFG2","AFL","AFM1","AFP1","AFQ1")~"Aflatoxins",
                              parameter %in% c("NPA","STER")~"Other_aflatoxins",
                              parameter %in% c("15AcDON","3AcDON","DAS","DON","DON3G","HT2","NIV","T2")~"Trichothecenes",
                              parameter %in% c("α-ZEL","ZEN","b-ZEL")~"Zearalenones",
                              parameter %in% c("FB1", "FB2","FB3") ~"Fumonisins",
                              parameter %in% c("ENNA","ENNA1","ENNB","ENNB1") ~"Enniatins",
                              parameter %in% c("BEA", "MON") ~"Other_fusarium_metabolites",
                              parameter %in% c("CIT", "MPA","OTA","PA","ROQ") ~"Penicillium_metabolites",
                              parameter %in% c("AME","AOH") ~"Alternaria_metabolites",
                              parameter %in% c("AC") ~"Ergot_alkaloids",
                              TRUE~"error")) %>% 
  
  mutate(myco_group1=case_when(myco_group == "Aflatoxins" | myco_group == "Other_aflatoxins"~"Aspergillus_metabolites",
                               myco_group == "Trichothecenes" | myco_group == "Zearalenones" | 
                               myco_group == "Fumonisins" | myco_group == "Enniatins" | myco_group == "Other_fusarium_metabolites" ~"Fusarium_metabolites",
                               myco_group == "Penicillium_metabolites" ~"Penicillium_metabolites",
                               myco_group == "Alternaria_metabolites"~"Alternaria_metabolites",
                               myco_group == "Ergot_alkaloids" ~"Ergot_alkaloids",
                               TRUE~"error")) %>% 
  
  mutate(myco_group2=case_when(parameter %in% c("AFB1", "AFB2", "AFG1","AFG2","AFL","AFM1","AFP1","AFQ1") ~"Aflatoxin",
                              parameter %in% c("15AcDON","3AcDON","DAS","DON","DON3G","HT2","NIV","T2","α-ZEL","ZEN","b-ZEL") ~"Fusarium_metabolites1",
                              parameter %in% c("FB1", "FB2","FB3","ENNA","ENNA1","ENNB","ENNB1","BEA", "MON") ~"Fusarium_metabolites2",
                              parameter %in% c("NPA","STER","CIT", "MPA","OTA","PA","ROQ","AME","AOH","AC") ~"Other_fungal_metabolites",
                              TRUE~"error")) %>% 
  
  mutate(value=as.numeric(value)) 


# add LOD and LOQ levels------

myco <-

myco %>% 
  
  left_join(LOD_LOQ,by=c("sample_type","parameter"))

# remove < LOQ ----

myco <-

myco %>% 
  
  mutate(value_DM=case_when(value < LOQ ~0, TRUE~value_DM)) %>% 
  
  mutate(maize_inclusion_perc=case_when(maize_inclusion_perc=="40"~"40%DM maize",
                                        maize_inclusion_perc=="80"~"80%DM maize",
                                        TRUE~"100")) 

                              
# calculate mean and sd----

myco_stats <- 

myco %>% 
  
  group_by(maize_inclusion_perc,afla_level, parameter,sample_type,myco_group,myco_group1,myco_group2) %>% 
  
  summarise(n=n(),
            mean=round(mean(value_DM),2),
            sd=round(sd(value_DM),2)) %>%
  
  mutate(myco_group=factor(myco_group)) %>% 
  
  mutate(myco_group1=factor(myco_group1)) %>% 
  
  mutate(myco_group2=factor(myco_group2))


# order parameters

myco$parameter <- factor(myco$parameter, levels = c("AFB1", "AFB2", "AFG1","AFG2","AFL","AFM1","AFP1","AFQ1","15AcDON","3AcDON","DAS","DON","DON3G","HT2","NIV","T2",
                                                             "a-ZEL","ZEN","b-ZEL","FB1", "FB2","FB3","ENNA","ENNA1","ENNB","ENNB1","BEA", "MON","NPA","STER","CIT", "MPA","OTA","PA","ROQ",
                                                             "AME","AOH","AC"))

myco_stats$parameter <- factor(myco_stats$parameter, levels = c("AFB1", "AFB2", "AFG1","AFG2","AFL","AFM1","AFP1","AFQ1","15AcDON","3AcDON","DAS","DON","DON3G","HT2","NIV","T2",
                                                    "a-ZEL","ZEN","b-ZEL","FB1", "FB2","FB3","ENNA","ENNA1","ENNB","ENNB1","BEA", "MON","NPA","STER","CIT", "MPA","OTA","PA","ROQ",
                                                    "AME","AOH","AC"))

myco$sample_type <- factor(myco$sample_type, levels = c("Raw_maize","substrate","larvae","frass"))
myco_stats$sample_type <-   factor(myco_stats$sample_type, levels = c("Raw_maize","substrate","larvae","frass"))

#save data to excel ------

write.xlsx(as.data.frame(myco), file = "output/bugpic_myco_data_MG.xlsx",sheetName = "data", append = FALSE)
write.xlsx(as.data.frame(myco_stats), file = "output/bugpic_myco_data_MG.xlsx",sheetName = "descriptive", append = TRUE)


# raw maize -------

  # complete profile

  myco_stats_maize <- 

  myco_stats %>% 
  
  filter(sample_type=="Raw_maize") %>% 
  
  group_by(parameter, afla_level) %>% 
  
  select(afla_level,parameter,mean, sd) %>% 
  
  arrange(factor(parameter,levels=parameter_levels)) %>% 
  
  unite(label,mean,sd,sep=" (") %>%
  
  mutate(help=")") %>%
  
  unite(mean_sd,label,help,sep="") %>% 
  
  spread(afla_level,mean_sd)

  # write to excel

  write.xlsx(as.data.frame(myco_stats_maize), file = "output/bugpic_maize_myco.xlsx",sheetName = "data", append = FALSE)

  
  # anova and post-hoc test
  
    # prepare data
  
    myco_maize_stat <- 
  
    myco %>% 
      
      filter(sample_type=="Raw_maize") %>% 
      
      ungroup() %>% 
      
      select(afla_level,replicate,parameter,value_DM) %>% 
      
      filter(value_DM>0) %>% 
      
      spread(key = parameter,value = value_DM,fill=0) %>% 
      
      mutate(total_aflatoxins=AFB1+AFB2+AFG1+AFG2) %>% 
      
      mutate(total_fumonisins=FB1+FB2+FB3) %>% 
      
      select(-replicate)
    
    
    # run anova and post-hoc test
  
    maize_aov <- aov(`a-ZEL`~afla_level, data=myco_maize_stat)
    
    summary(maize_aov)
    
    myco_maize_stat$`a-ZEL`

    myco_maize_stat
    
    myco_maize_stat.df <- data.frame(myco_maize_stat)
    
    names <- list()
    aov.models <- lapply(setdiff(names(myco_maize_stat.df), "afla_level"), function(s) {
      names <<- append(names, s) # Note the global assignment
      aov(as.formula(paste(s, " ~ afla_level")),myco_maize_stat.df)
    })
    names(aov.models) <- names
    
    out <- capture.output(lapply(aov.models, summary))
    cat(out, file="output/maize_aov.txt", sep="\n")
    
  # for comparison with substrate
  
  myco_stats_maize_comp <- 
  
  myco_stats %>% 
    
    filter(sample_type=="Raw_maize") %>% 
    
    group_by(parameter, afla_level) %>% 
    
    select(afla_level,parameter,mean)
  
  
  #AFB1

  #mean

  myco_stats %>% 
    
    filter(sample_type=="Raw_maize") %>% 
    
    filter(parameter=="AFB1")

  #analyses replicates

  myco %>% 
    
    filter(sample_type=="Raw_maize") %>% 
    
    filter(parameter=="AFB1")
  
  
  #Total aflatoxins

  #mean
  
  myco_stats %>% 
    
    filter(sample_type=="Raw_maize") %>% 
    
    filter(myco_group=="Aflatoxins") %>% 
    
    group_by(afla_level) %>% 
    
    select(afla_level,parameter,mean) %>% 
    
    spread(key = parameter,value = mean) %>% 
    
    mutate(total_aflatoxins=AFB1+AFB2+AFG1+AFG2)
  
  #analysis replicates
  
  myco %>% 
    
    filter(sample_type=="Raw_maize") %>% 
    
    filter(myco_group=="Aflatoxins") %>% 
    
    group_by(afla_level) %>% 
    
    select(afla_level,replicate, parameter,value_DM) %>% 
    
    spread(key = parameter,value = value_DM) %>% 
    
    mutate(total_aflatoxins=AFB1+AFB2+AFG1+AFG2) %>% 
    
    ungroup() %>% 
    
    group_by(afla_level) %>% 
    
    summarise(n=n(),
              mean=mean(total_aflatoxins),
              sd=sd(total_aflatoxins))
  
  #Fumonisins
  
  #mean
  
  myco_stats %>% 
    
    filter(sample_type=="Raw_maize") %>% 
    
    filter(myco_group=="Fumonisins") %>% 
    
    group_by(afla_level) %>% 
    
    select(afla_level,parameter,mean) %>% 
    
    spread(key = parameter,value = mean) %>% 
    
    mutate(total_aflatoxins=FB1+FB2+FB3)
    
  
  #analysis replicates
  
  myco %>% 
    
    filter(sample_type=="Raw_maize") %>% 
    
    filter(myco_group=="Fumonisins") %>% 
    
    group_by(afla_level) %>% 
    
    select(afla_level,replicate, parameter,value_DM) %>% 
    
    spread(key = parameter,value = value_DM) %>% 
    
    mutate(total_aflatoxins=FB1+FB2+FB3) %>% 
    
    ungroup() %>% 
    
    group_by(afla_level) %>% 
    
    summarise(n=n(),
              mean=mean(total_aflatoxins),
              sd=sd(total_aflatoxins))
  
  
  #DON
  
  #mean
  
  myco_stats %>% 
    
    filter(sample_type=="Raw_maize") %>% 
    
    filter(parameter=="DON") 

  
  # plot mycotoxin profile
  
  myco_stats %>% 
    
    filter(sample_type=="Raw_maize") %>% 
    
    filter(mean>0) %>% 
    
    ggplot(aes(afla_level,mean)) + 
    
    geom_bar(stat="identity") +
    
    geom_errorbar(aes(x=afla_level,ymin=mean-sd, ymax=mean+sd), width=0.4) +
    
facet_wrap(~parameter)
    
    theme_bw(base_size = 12) +
    
    ylab("ug/kg DM") +
    
    xlab("Maize contamination level") +
    
    ggtitle("substrate - Aflatoxins") +
    
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
# substrate -------


    # complete profile
    
    # 40% DM maize
    
    myco_stats_substrate_40maize <-

    myco_stats %>%

      filter(sample_type=="substrate") %>%

      filter(afla_level == "CONTROL" | afla_level == "LOW" | afla_level == "HIGH") %>%

      filter(maize_inclusion_perc =="40%DM maize") %>%

      ungroup() %>% 
      
      select(afla_level,parameter, mean,sd) %>%

      unite(label,mean,sd,sep=" (") %>%

      mutate(help=")") %>%

      unite(mean_sd,label,help,sep="") %>% 
      
      arrange(factor(parameter,levels=parameter_levels)) %>% 
      
      spread(key = afla_level,value = mean_sd)
    
    # 80% DM maize
    
    myco_stats_substrate_80maize <-
      
      myco_stats %>%
      
      filter(sample_type=="substrate") %>%
      
      filter(afla_level == "CONTROL" | afla_level == "LOW" | afla_level == "HIGH") %>%
      
      filter(maize_inclusion_perc =="80%DM maize") %>%
      
      ungroup() %>% 
      
      select(afla_level,parameter, mean,sd) %>%
      
      unite(label,mean,sd,sep=" (") %>%
      
      mutate(help=")") %>%
      
      unite(mean_sd,label,help,sep="") %>% 
      
      arrange(factor(parameter,levels=parameter_levels)) %>% 
      
      spread(key = afla_level,value = mean_sd)
      

    # write to excel
    
    write.xlsx(as.data.frame(myco_stats_substrate_40maize), file = "output/bugpic_substrate_myco.xlsx",sheetName = "40% maize", append = FALSE)
    write.xlsx(as.data.frame(myco_stats_substrate_80maize), file = "output/bugpic_substrate_myco.xlsx",sheetName = "80% maize", append = TRUE)
    
    # statistical analayses
    
     # prepare data
    
      myco_substrate_stat <-
      
      myco %>% 
      
      filter(sample_type=="substrate" | sample_type=="Raw_maize") %>% 
      
      select(maize_inclusion_perc,afla_level,parameter,value_DM) %>% 
      
      ungroup()%>% 
      
      select(-sample_type) %>% 
      
      # filter between two different maize inclusion levels  
        
      # filter(maize_inclusion_perc=="40%DM maize") %>%

      # only consider controls
        
      # filter(afla_level=="CONTROL") %>%
        
      spread(key = parameter,value = value_DM) %>% 
      
      mutate(total_aflatoxins=AFB1+AFB2+AFG1+AFG2) %>% 
      
      mutate(total_fumonisins=FB1+FB2+FB3) %>% 
      
      select(-replicate) %>% 
      
      unite(inclusion_afla_level, maize_inclusion_perc,afla_level) %>% 
      
      mutate(inclusion_afla_level=factor(inclusion_afla_level))
    
      # set up model
      model <- lm(AOH ~ inclusion_afla_level, data = myco_substrate_stat)
      
      # get (adjusted) weight means per group
      model_means <- emmeans(object = model,
                             specs = "inclusion_afla_level")
      
      # add letters to each mean
      model_means_cld <- cld(object = model_means,
                             adjust = "tukey",
                             Letters = letters,
                             alpha = 0.05)
      
      # show output
      model_means_cld
      

      
    # Does maize inclusion level change with maize mycotoxin concentrations for the same maize inclusion?
      
    # 40% maize
    
    myco_substrate_40_stat <- 
      
      myco %>% 
      
      filter(sample_type=="substrate") %>% 
      
      select(maize_inclusion_perc,afla_level,parameter,value_DM) %>% 
      
      filter(maize_inclusion_perc=="40%DM maize") %>% 
      
      ungroup() %>% 
      
      select(-maize_inclusion_perc,-sample_type) %>% 
      
      filter(parameter=="AFB1" | parameter =="AFB2" | parameter == "AFG1"| parameter =="AFG2" |
               parameter=="DON" | parameter == "FB1" | parameter == "FB2" | parameter == "FB3") %>% 
      
      spread(key = parameter,value = value_DM) %>% 
      
      mutate(total_aflatoxins=AFB1+AFB2+AFG1+AFG2) %>% 
      
      mutate(total_fumonisins=FB1+FB2+FB3)
    
    # mean concents of key mycotoxins
    
    myco_substrate_40_stat %>% 
      
      select(replicate,afla_level,total_aflatoxins,total_fumonisins,DON) %>% 
      
      gather(3:5,key = parameter,value = value) %>% 
      
      group_by(afla_level,parameter) %>% 
      
      summarise(n=n(),
                mean=round(mean(value),1),
                sd=round(sd(value),1)) 
    
    
    # anova and post-hoc test
    
    substrate_40_aov <- aov(AFG1~afla_level, data=myco_substrate_40_stat)
    
    summary(substrate_40_aov)
    
    Tukey_substrate_40_aov <- TukeyHSD(substrate_40_aov)
    
    Tukey_substrate_40_aov 
    
    
    
      # 80% maize
    
        myco_substrate_80_stat <- 
        
        myco %>% 
          
          filter(sample_type=="substrate") %>% 
          
          select(maize_inclusion_perc,afla_level,parameter,value_DM) %>% 
          
          filter(maize_inclusion_perc=="80%DM maize") %>% 
          
          ungroup() %>% 
          
          select(-maize_inclusion_perc,-sample_type) %>% 
          
          filter(parameter=="AFB1" | parameter =="AFB2" | parameter == "AFG1"| parameter =="AFG2" |
                   parameter=="DON" | parameter == "FB1" | parameter == "FB2" | parameter == "FB3") %>% 
          
          spread(key = parameter,value = value_DM) %>% 
          
          mutate(total_aflatoxins=AFB1+AFB2+AFG1+AFG2) %>% 
          
          mutate(total_fumonisins=FB1+FB2+FB3)
          
        # mean concents of key mycotoxins
        
        myco_substrate_80_stat %>% 
          
          select(replicate,afla_level,total_aflatoxins,total_fumonisins,DON) %>% 
          
          gather(3:5,key = parameter,value = value) %>% 
          
          group_by(afla_level,parameter) %>% 
          
          summarise(n=n(),
                    mean=round(mean(value),1),
                    sd=round(sd(value),1))
        
        
          # anova and post-hoc test
          
          substrate_80_aov <- aov(AFG1~afla_level, data=myco_substrate_80_stat)
          
          summary(substrate_80_aov)
          
          Tukey_substrate_80_aov <- TukeyHSD(substrate_80_aov)
          
          Tukey_substrate_80_aov 
        
     
    
    # compare with raw maize

    myco_compare_
    
    myco_stats %>%
      
      filter(sample_type=="substrate") %>%
      
      filter(afla_level == "CONTROL" | afla_level == "LOW" | afla_level == "HIGH") %>%
      
      filter(maize_inclusion_perc =="40%DM maize") %>%
      
      ungroup() %>% 
      
      select(afla_level,parameter, mean) %>% 
      
      bind_rows( myco_stats_maize_comp) %>% 
      
      spread(key = afla_level,value = mean) 
    
    
    

    
    # plot
    
    myco_stats %>% 
      
      filter(sample_type=="substrate") %>% 
    
      # filter(myco_group2=="Aflatoxin") %>% 
      
      filter(mean>0) %>% 
      
      ggplot(aes(afla_level,mean)) + 
      
      geom_bar(stat="identity") +
      
      geom_errorbar(aes(x=afla_level,ymin=mean-sd, ymax=mean+sd), width=0.4) +
      
      facet_grid(vars(maize_inclusion_perc),vars(parameter)) +
      
      theme_bw(base_size = 12) +
      
      ylab("ug/kg DM") +
      
      xlab("Maize contamination level") +
      
      ggtitle("substrate - Aflatoxins") +
      
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
#Aspergillus metabolites

myco_stats %>% 
  
  filter(sample_type=="substrate") %>% 
  
  filter(myco_group2=="Aflatoxin") %>% 
  
  ggplot(aes(afla_level,mean)) + 
  
  geom_bar(stat="identity") +
  
  geom_errorbar(aes(x=afla_level,ymin=mean-sd, ymax=mean+sd), width=0.4) +
  
  facet_grid(vars(maize_inclusion_perc),vars(parameter)) +
  
  theme_bw(base_size = 12) +
  
  ylab("ug/kg DM") +
  
  xlab("Maize contamination level") +
  
  ggtitle("substrate - Aflatoxins") +
  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("output/myco/substrate_aflatoxins.jpeg", dpi = "print", width = 18,height = 14 , units = "cm")

# Fusarium_metabolites 1

myco_stats %>% 
  
  filter(sample_type=="substrate") %>% 
  
  filter(myco_group2=="Fusarium_metabolites1") %>% 
  
  ggplot(aes(afla_level,mean)) + 
  
  geom_bar(stat="identity") +
  
  geom_errorbar(aes(x=afla_level,ymin=mean-sd, ymax=mean+sd), width=0.4) +
  
  facet_grid(vars(maize_inclusion_perc),vars(parameter)) +
  
  theme_bw(base_size = 12) +
  
  ylab("ug/kg DM") +
  
  xlab("Maize contamination level") +
  
  ggtitle("substrate - Fusarium metabolites (1)") +
  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("output/myco/substrate_fusarium1.jpeg", dpi = "print", width = 24,height = 14 , units = "cm")


# Fusarium_metabolites 2

myco_stats$maize_inclusion_perc

myco_stats %>% View()
  
  filter(sample_type=="substrate") %>% 
  
  filter(myco_group2=="Fusarium_metabolites2") %>% 
  
  ggplot(aes(afla_level,mean)) + 
  
  geom_bar(stat="identity") +
  
  geom_errorbar(aes(x=afla_level,ymin=mean-sd, ymax=mean+sd), width=0.4) +
  
  facet_grid(vars(maize_inclusion_perc),vars(parameter)) +
  
  theme_bw(base_size = 12) +
  
  ylab("ug/kg DM") +
  
  xlab("Maize contamination level") +
  
  ggtitle("substrate - Fusarium metabolites (2)") +
  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  
  ylim(0,1200)

ggsave("output/myco/substrate_fusarium2.jpeg", dpi = "print", width = 18,height = 14 , units = "cm")


# Other fungal metabolites

myco_stats %>% 
  
  filter(sample_type=="substrate") %>% 
  
  filter(myco_group2=="Other_fungal_metabolites") %>% 
  
  ggplot(aes(afla_level,mean)) + 
  
  geom_bar(stat="identity") +
  
  geom_errorbar(aes(x=afla_level,ymin=mean-sd, ymax=mean+sd), width=0.4) +
  
  facet_grid(vars(maize_inclusion_perc),vars(parameter)) +
  
  theme_bw(base_size = 12) +
  
  ylab("ug/kg DM") +
  
  xlab("Maize contamination level") +
  
  ggtitle("substrate - Other fungal metabolites") +
  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("output/myco/substrate_other_fungal.jpeg", dpi = "print", width = 18,height = 14 , units = "cm")


# larvae -------

  # stat comparison

  
# 80% maize

myco_larvae_80_stat <- 
  
  myco %>% 
  
  filter(sample_type=="larvae") %>% 
  
  select(maize_inclusion_perc,afla_level,parameter,value_DM) %>% 
  
  filter(maize_inclusion_perc=="80%DM maize") %>% 
  
  ungroup() %>% 
  
  select(-maize_inclusion_perc,-sample_type) %>% 
  
  filter(parameter=="AFB1" | parameter =="AFB2" | parameter == "AFG1"| parameter =="AFG2" |
           parameter=="DON" | parameter == "FB1" | parameter == "FB2" | parameter == "FB3") %>% 
  
  spread(key = parameter,value = value_DM) %>% 
  
  mutate(total_aflatoxins=AFB1+AFB2+AFG1+AFG2) %>% 
  
  mutate(total_fumonisins=FB1+FB2+FB3)

# mean concents of key mycotoxins

myco_larvae_80_stat %>% 
  
  select(replicate,afla_level,total_aflatoxins,total_fumonisins,DON, AFB1) %>% 
  
  gather(3:6,key = parameter,value = value) %>% 
  
  group_by(afla_level,parameter) %>% 
  
  summarise(n=n(),
            mean=round(mean(value),1),
            sd=round(sd(value),1))


# anova and post-hoc test

myco_larvae_all_stat <-

myco %>% 
  
  filter(sample_type=="larvae") %>% 
  
  select(maize_inclusion_perc,afla_level,parameter,value_DM) %>% 
  
  ungroup() %>% 
  
  select(-sample_type) %>% 
  
  unite(afla_maize_inclusion,maize_inclusion_perc,afla_level) %>% 

  spread(key = parameter,value = value_DM) %>% 
  
  select(-replicate)


# set up model
model <- lm(BEA ~ afla_maize_inclusion, data = myco_larvae_all_stat)

# get (adjusted) weight means per group
model_means <- emmeans(object = model,
                       specs = "afla_maize_inclusion")

# add letters to each mean
model_means_cld <- cld(object = model_means,
                       adjust = "tukey",
                       Letters = letters,
                       alpha = 0.05)

# show output
model_means_cld




# 40% maize

myco_larvae_40_stat <- 
  
  myco %>% 
  
  filter(sample_type=="larvae") %>% 
  
  select(maize_inclusion_perc,afla_level,parameter,value_DM) %>% 
  
  filter(maize_inclusion_perc=="40%DM maize") %>% 
  
  ungroup() %>% 
  
  select(-maize_inclusion_perc,-sample_type) %>% 
  
  filter(parameter=="AFB1" | parameter =="AFB2" | parameter == "AFG1"| parameter =="AFG2" |
           parameter=="DON" | parameter == "FB1" | parameter == "FB2" | parameter == "FB3") %>% 
  
  spread(key = parameter,value = value_DM) %>% 
  
  mutate(total_aflatoxins=AFB1+AFB2+AFG1+AFG2) %>% 
  
  mutate(total_fumonisins=FB1+FB2+FB3)

# mean concents of key mycotoxins

myco_larvae_40_stat %>% 
  
  select(replicate,afla_level,total_aflatoxins,total_fumonisins,DON, AFB1) %>% 
  
  gather(3:6,key = parameter,value = value) %>% 
  
  group_by(afla_level,parameter) %>% 
  
  summarise(n=n(),
            mean=round(mean(value),1),
            sd=round(sd(value),1))


# anova and post-hoc test

larvae_40_aov <- aov(AFG1~afla_level, data=myco_larvae_40_stat)

summary(larvae_40_aov)

Tukey_larvae_40_aov <- TukeyHSD(larvae_40_aov)

Tukey_larvae_40_aov 


  #Aspergillus metabolites

  myco_stats %>% 
    
    filter(sample_type=="larvae") %>% 
    
    filter(myco_group2=="Aflatoxin") %>% 
    
    ggplot(aes(afla_level,mean)) + 
    
    geom_bar(stat="identity") +
    
    geom_errorbar(aes(x=afla_level,ymin=mean-sd, ymax=mean+sd), width=0.4) +
    
    facet_grid(vars(maize_inclusion_perc),vars(parameter)) +
    
    theme_bw(base_size = 12) +
    
    ylab("ug/kg DM") +
    
    xlab("Maize contamination level") +
    
    ggtitle("Larvae - Aflatoxins") +
    
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  
    ylim(0,150)
  
    ggsave("output/myco/larvae_aflatoxins.jpeg", dpi = "print", width = 18,height = 14 , units = "cm")
  
    # Fusarium_metabolites 1
    
    myco_stats %>% 
      
      filter(sample_type=="larvae") %>% 
      
      filter(myco_group2=="Fusarium_metabolites1") %>% 
      
      ggplot(aes(afla_level,mean)) + 
      
      geom_bar(stat="identity") +
      
      geom_errorbar(aes(x=afla_level,ymin=mean-sd, ymax=mean+sd), width=0.4) +
      
      facet_grid(vars(maize_inclusion_perc),vars(parameter)) +
      
      theme_bw(base_size = 12) +
      
      ylab("ug/kg DM") +
      
      xlab("Maize contamination level") +
      
      ggtitle("Larvae - Fusarium metabolites (1)") +
      
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      
      ylim(0,150)
    
    ggsave("output/myco/larvae_fusarium1.jpeg", dpi = "print", width = 24,height = 14 , units = "cm")
     
  
    # Fusarium_metabolites 2
    
    myco_stats %>% 
      
      filter(sample_type=="larvae") %>% 
      
      filter(myco_group2=="Fusarium_metabolites2") %>% 
      
      ggplot(aes(afla_level,mean)) + 
      
      geom_bar(stat="identity") +
      
      geom_errorbar(aes(x=afla_level,ymin=mean-sd, ymax=mean+sd), width=0.4) +
      
      facet_grid(vars(maize_inclusion_perc),vars(parameter)) +
      
      theme_bw(base_size = 12) +
      
      ylab("ug/kg DM") +
      
      xlab("Maize contamination level") +
      
      ggtitle("Larvae - Fusarium metabolites (2)") +
      
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      
      ylim(0,1200)
    
    ggsave("output/myco/larvae_fusarium2.jpeg", dpi = "print", width = 18,height = 14 , units = "cm")
    
    
    # Other fungal metabolites
    
    myco_stats %>% 
      
      filter(sample_type=="larvae") %>% 
      
      filter(myco_group2=="Other_fungal_metabolites") %>% 
      
      ggplot(aes(afla_level,mean)) + 
      
      geom_bar(stat="identity") +
      
      geom_errorbar(aes(x=afla_level,ymin=mean-sd, ymax=mean+sd), width=0.4) +
      
      facet_grid(vars(maize_inclusion_perc),vars(parameter)) +
      
      theme_bw(base_size = 12) +
      
      ylab("ug/kg DM") +
      
      xlab("Maize contamination level") +
      
      ggtitle("Larvae - Other fungal metabolites") +
      
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      
      ylim(0,80)
    
    ggsave("output/myco/larvae_other_fungal.jpeg", dpi = "print", width = 18,height = 14 , units = "cm")

# frass -------
    

    
    # write to excel
    
    write.xlsx(as.data.frame(myco_stats_substrate_40maize), file = "output/bugpic_substrate_myco.xlsx",sheetName = "40% maize", append = FALSE)
    write.xlsx(as.data.frame(myco_stats_substrate_80maize), file = "output/bugpic_substrate_myco.xlsx",sheetName = "80% maize", append = TRUE)
    
    
    
    
    # frass vs substrate
    
    # 40% maize
    
    myco_frass_substrate_40_stat <- 
      
      myco %>% 
      
      filter(sample_type=="substrate" | sample_type =="frass") %>% 
      
      select(maize_inclusion_perc,afla_level,parameter,value_DM) %>% 
      
      filter(maize_inclusion_perc=="40%DM maize") %>% 
      
      filter(afla_level=="HIGH") %>% 
      
      ungroup() %>% 
      
      select(-maize_inclusion_perc,-afla_level) %>% 
      
      filter(parameter=="AFB1" | parameter =="AFB2" | parameter == "AFG1"| parameter =="AFG2" |
               parameter=="DON" | parameter == "FB1" | parameter == "FB2" | parameter == "FB3") %>%
      
      spread(key = parameter,value = value_DM) %>% 
      
      mutate(total_aflatoxins=AFB1+AFB2+AFG1+AFG2) %>% 
      
      mutate(total_fumonisins=FB1+FB2+FB3)
    
    # mean concents of key mycotoxins
    
    myco_frass_substrate_40_stat %>% 
      
      select(replicate,sample_type,total_aflatoxins,total_fumonisins,DON) %>% 
      
      gather(3:5,key = parameter,value = value) %>% 
      
      group_by(sample_type,parameter) %>% 
      
      summarise(n=n(),
                mean=round(mean(value),1),
                sd=round(sd(value),1)) 
    
    
    # anova and post-hoc test
    
    substrate_frass_40_aov <- aov(DON~sample_type, data=myco_frass_substrate_40_stat)
    
    summary(substrate_frass_40_aov)
    
    
    # 80% maize
    
    myco_frass_substrate_80_stat <- 
      
      myco %>% 
      
      filter(sample_type=="substrate" | sample_type =="frass") %>% 
      
      select(maize_inclusion_perc,afla_level,parameter,value_DM) %>% 
      
      filter(maize_inclusion_perc=="80%DM maize") %>% 
      
      filter(afla_level=="HIGH") %>% 
      
      ungroup() %>% 
      
      select(-maize_inclusion_perc,-afla_level) %>% 
      
      filter(parameter=="AFB1" | parameter =="AFB2" | parameter == "AFG1"| parameter =="AFG2" |
               parameter=="DON" | parameter == "FB1" | parameter == "FB2" | parameter == "FB3") %>%
      
      spread(key = parameter,value = value_DM) %>% 
      
      mutate(total_aflatoxins=AFB1+AFB2+AFG1+AFG2) %>% 
      
      mutate(total_fumonisins=FB1+FB2+FB3)
    
    # mean concents of key mycotoxins
    
    myco_frass_substrate_80_stat %>% 
      
      select(replicate,sample_type,total_aflatoxins,total_fumonisins,DON) %>% 
      
      gather(3:5,key = parameter,value = value) %>% 
      
      group_by(sample_type,parameter) %>% 
      
      summarise(n=n(),
                mean=round(mean(value),1),
                sd=round(sd(value),1)) 
    
    
    # anova and post-hoc test
      
      substrate_frass_80_aov <- aov(total_aflatoxins~sample_type, data=myco_frass_substrate_80_stat)
      
      summary(substrate_frass_80_aov)
    
 
    
    #Aspergillus metabolites
    
    myco_stats %>% 
      
      filter(sample_type=="frass") %>% 
      
      filter(myco_group2=="Aflatoxin") %>% 
      
      ggplot(aes(afla_level,mean)) + 
      
      geom_bar(stat="identity") +
      
      geom_errorbar(aes(x=afla_level,ymin=mean-sd, ymax=mean+sd), width=0.4) +
      
      facet_grid(vars(maize_inclusion_perc),vars(parameter)) +
      
      theme_bw(base_size = 12) +
      
      ylab("ug/kg DM") +
      
      xlab("Maize contamination level") +
      
      ggtitle("frass - Aflatoxins") +
      
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    ggsave("output/myco/frass_aflatoxins.jpeg", dpi = "print", width = 18,height = 14 , units = "cm")
    
    # Fusarium_metabolites 1
    
    myco_stats %>% 
      
      filter(sample_type=="frass") %>% 
      
      filter(myco_group2=="Fusarium_metabolites1") %>% 
      
      ggplot(aes(afla_level,mean)) + 
      
      geom_bar(stat="identity") +
      
      geom_errorbar(aes(x=afla_level,ymin=mean-sd, ymax=mean+sd), width=0.4) +
      
      facet_grid(vars(maize_inclusion_perc),vars(parameter)) +
      
      theme_bw(base_size = 12) +
      
      ylab("ug/kg DM") +
      
      xlab("Maize contamination level") +
      
      ggtitle("frass - Fusarium metabolites (1)") +
      
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    ggsave("output/myco/frass_fusarium1.jpeg", dpi = "print", width = 24,height = 14 , units = "cm")
    
    
    # Fusarium_metabolites 2
    
    myco_stats %>% 
      
      filter(sample_type=="frass") %>% 
      
      filter(myco_group2=="Fusarium_metabolites2") %>% 
      
      ggplot(aes(afla_level,mean)) + 
      
      geom_bar(stat="identity") +
      
      geom_errorbar(aes(x=afla_level,ymin=mean-sd, ymax=mean+sd), width=0.4) +
      
      facet_grid(vars(maize_inclusion_perc),vars(parameter)) +
      
      theme_bw(base_size = 12) +
      
      ylab("ug/kg DM") +
      
      xlab("Maize contamination level") +
      
      ggtitle("frass - Fusarium metabolites (2)") +
      
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    ggsave("output/myco/frass_fusarium2.jpeg", dpi = "print", width = 18,height = 14 , units = "cm")
    
    
    # Other fungal metabolites
    
    myco_stats %>% 
      
      filter(sample_type=="frass") %>% 
      
      filter(myco_group2=="Other_fungal_metabolites") %>% 
      
      ggplot(aes(afla_level,mean)) + 
      
      geom_bar(stat="identity") +
      
      geom_errorbar(aes(x=afla_level,ymin=mean-sd, ymax=mean+sd), width=0.4) +
      
      facet_grid(vars(maize_inclusion_perc),vars(parameter)) +
      
      theme_bw(base_size = 12) +
      
      ylab("ug/kg DM") +
      
      xlab("Maize contamination level") +
      
      ggtitle("frass - Other fungal metabolites") +
      
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    ggsave("output/myco/frass_other_fungal.jpeg", dpi = "print", width = 18,height = 14 , units = "cm")
    
  # correlation between % contam maize and mycotoxins
    
    myco_perc <- 
    
    myco_stats %>% 
      
      filter(!afla_level=="CONTROL") %>% 
      
      ungroup() %>% 
      
      mutate(myco_level=1) %>% 
      
      mutate(maize_inclusion_perc=factor(maize_inclusion_perc)) %>% 
      
      mutate(myco_level=case_when((maize_inclusion_perc=="40%DM maize" & afla_level =="LOW") ~"12.5", 
                                            maize_inclusion_perc=="40%DM maize" & afla_level =="HIGH" ~"50.0",
                                            maize_inclusion_perc=="80%DM maize" & afla_level =="LOW" ~"20.0",
                                            maize_inclusion_perc=="80%DM maize" & afla_level =="HIGH" ~"80.0",
                                            TRUE~"error")) %>% 
      
    mutate(myco_level=as.numeric(myco_level)) 

  
  # evolution -----
    
    # data summary
    
    # CONTROL, 50% maize
    
    myco_evo_control_50 <-
      
      myco_stats %>% 
      
      filter(afla_level=="CONTROL") %>% 
      
      filter(maize_inclusion_perc=="40%DM maize") %>% 
      
      filter(mean > 0) %>% 
      
      ungroup() %>% 
      
      select(parameter,sample_type,mean,sd) %>% 
      
      unite(label,mean,sd,sep=" (") %>%
      
      mutate(help=")") %>%
      
      unite(mean_sd,label,help,sep="") %>% 
      
      spread(sample_type,mean_sd)
    
    # CONTROL, 80% maize
    
    myco_evo_control_80 <-
      
      myco_stats %>% 
      
      filter(afla_level=="CONTROL") %>% 
      
      filter(maize_inclusion_perc=="80%DM maize") %>% 
      
      filter(mean > 0) %>% 
      
      ungroup() %>% 
      
      select(parameter,sample_type,mean,sd) %>% 
      
      unite(label,mean,sd,sep=" (") %>%
      
      mutate(help=")") %>%
      
      unite(mean_sd,label,help,sep="") %>% 
      
      spread(sample_type,mean_sd)
    
    
    # high, 50% maize
    
    myco_evo_high_50 <-
    
    myco_stats %>% 
      
      filter(afla_level=="HIGH") %>% 
      
      filter(maize_inclusion_perc=="40%DM maize") %>% 
      
      filter(mean > 0) %>% 
      
      ungroup() %>% 
      
      select(parameter,sample_type,mean,sd) %>% 
      
      unite(label,mean,sd,sep=" (") %>%
      
      mutate(help=")") %>%
      
      unite(mean_sd,label,help,sep="") %>% 
      
      spread(sample_type,mean_sd)
      
    # high, 80% maize
    
    myco_evo_high_80 <-
      
      myco_stats %>% 
      
      filter(afla_level=="HIGH") %>% 
      
      filter(maize_inclusion_perc=="80%DM maize") %>% 
      
      filter(mean > 0) %>% 
      
      ungroup() %>% 
      
      select(parameter,sample_type,mean,sd) %>% 
      
      unite(label,mean,sd,sep=" (") %>%
      
      mutate(help=")") %>%
      
      unite(mean_sd,label,help,sep="") %>% 
      
      spread(sample_type,mean_sd)
    
    # low, 50% maize
    
    myco_evo_low_50 <-
      
      myco_stats %>% 
      
      filter(afla_level=="LOW") %>% 
      
      filter(maize_inclusion_perc=="40%DM maize") %>% 
      
      filter(mean > 0) %>% 
      
      ungroup() %>% 
      
      select(parameter,sample_type,mean,sd) %>% 
      
      unite(label,mean,sd,sep=" (") %>%
      
      mutate(help=")") %>%
      
      unite(mean_sd,label,help,sep="") %>% 
      
      spread(sample_type,mean_sd)
    
    # low, 80% maize
    
    myco_evo_low_80 <-
      
      myco_stats %>% 
      
      filter(afla_level=="LOW") %>% 
      
      filter(maize_inclusion_perc=="80%DM maize") %>% 
      
      filter(mean > 0) %>% 
      
      ungroup() %>% 
      
      select(parameter,sample_type,mean,sd) %>% 
      
      unite(label,mean,sd,sep=" (") %>%
      
      mutate(help=")") %>%
      
      unite(mean_sd,label,help,sep="") %>% 
      
      spread(sample_type,mean_sd)
    
    # evo high
    
    myco_evo_high <- 
    
    myco_evo_high_50 %>% 
      
      full_join(myco_evo_high_80,by = "parameter")
    
    # evo low
    
    myco_evo_low <- 
      
      myco_evo_low_50 %>% 
      
      full_join(myco_evo_low_80,by = "parameter")
    
    # evo control
    
    myco_evo_control <- 
      
      myco_evo_control_50 %>% 
      
      full_join(myco_evo_control_80,by = "parameter")
    

    # save both in one sheet
    
    write.xlsx(as.data.frame(myco_evo_high), file = "output/myco_evo.xlsx",sheetName = "high", append = FALSE)
    write.xlsx(as.data.frame(myco_evo_low), file = "output/myco_evo.xlsx",sheetName = "low", append = TRUE)
    write.xlsx(as.data.frame(myco_evo_control), file = "output/myco_evo.xlsx",sheetName = "control", append = TRUE)
    
    
    
  
    
    # most conservative case - aflatoxins
    
    myco_evo_high_afla <- 
    
    myco %>% 
      
      filter(afla_level=="HIGH") %>% 
      
      filter(value_DM > 0) %>%
      
      filter(myco_group=="Aflatoxins") %>% 
      
      mutate(maize_inclusion_perc=case_when(maize_inclusion_perc == "40%DM maize" ~"50%DM maize",
                                            TRUE~maize_inclusion_perc))
      

      myco_stats %>% 
      
      filter(afla_level=="HIGH") %>% 
        
      filter(mean > 0) %>%
        
      filter(myco_group=="Aflatoxins") %>% 
        
        mutate(maize_inclusion_perc=case_when(maize_inclusion_perc == "40%DM maize" ~"50%DM maize",
                                              TRUE~maize_inclusion_perc)) %>% 

      ggplot(aes(sample_type,mean)) +
      
      geom_point(size=3) +
      
      geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),width = 0.3)+
      
      geom_jitter(data=myco_evo_high_afla,
                  aes(sample_type,value_DM),
                  size=1.5,
                  position = position_jitter(0.2)) +
      
      facet_grid(vars(maize_inclusion_perc),vars(parameter)) +
      
      theme_bw(base_size = 15) +
      
      ylab("μg/kg DM") +
      
      xlab("") +
      
      ggtitle("(a)") +
      
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      
      ylim(-2,150)
      
     ggsave("output/myco/high_afla.jpeg", dpi = "print", width = 14,height = 14 , units = "cm")
    
    
     # most conservative case - fuomonisins
     
     myco_evo_high_afla <- 
       
       myco %>% 
       
       filter(afla_level=="HIGH") %>% 
       
       filter(value_DM > 0) %>%
       
       filter(myco_group=="Fumonisins") %>% 
       
       mutate(maize_inclusion_perc=case_when(maize_inclusion_perc == "40%DM maize" ~"50%DM maize",
                                             TRUE~maize_inclusion_perc))
     

     myco_stats %>% 
       
       filter(afla_level=="HIGH") %>% 
       
       filter(mean > 0) %>%
       
       filter(myco_group=="Fumonisins") %>% 
       
       mutate(maize_inclusion_perc=case_when(maize_inclusion_perc == "40%DM maize" ~"50%DM maize",
                                             TRUE~maize_inclusion_perc)) %>% 
       
       ggplot(aes(sample_type,mean)) +
       
       geom_point(size=3) +
       
       geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),width = 0.3)+
       
       geom_jitter(data=myco_evo_high_afla,
                   aes(sample_type,value_DM),
                   size=1.5,
                   position = position_jitter(0.2)) +
       
       facet_grid(vars(maize_inclusion_perc),vars(parameter)) +
       
       theme_bw(base_size = 15) +
       
       ylab("μg/kg DM") +
       
       xlab("")+
       
       ggtitle("(b)") +
       
       theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
       
       ylim(-2,1500)
     
     ggsave("output/myco/high_fuom.jpeg", dpi = "print", width = 10,height = 14 , units = "cm")
     
     # most conservative case - all except for aflatoxins and fuomonisins
     
     myco_evo_high_afla <- 
       
       myco %>% 
       
       filter(afla_level=="HIGH") %>% 
       
       filter(value_DM > 0) %>%
       
       filter(!myco_group=="Fumonisins" & !myco_group =="Aflatoxins") %>% 
       
       mutate(maize_inclusion_perc=case_when(maize_inclusion_perc == "40%DM maize" ~"50%DM maize",
                                             TRUE~maize_inclusion_perc))
     

     myco_stats %>% 
       
       filter(afla_level=="HIGH") %>% 
       
       filter(mean > 0) %>%
       
       filter(!myco_group=="Fumonisins" & !myco_group =="Aflatoxins") %>% 
       
       mutate(maize_inclusion_perc=case_when(maize_inclusion_perc == "40%DM maize" ~"50%DM maize",
                                             TRUE~maize_inclusion_perc)) %>% 
       
       ggplot(aes(sample_type,mean)) +
       
       geom_point(size=3) +
       
       geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),width = 0.3)+
       
       geom_jitter(data=myco_evo_high_afla,
                   aes(sample_type,value_DM),
                   size=1.5,
                   position = position_jitter(0.2)) +
       
       facet_grid(vars(maize_inclusion_perc),vars(parameter)) +
       
       theme_bw(base_size = 15) +
       
       ylab("μg/kg DM") +
       
       xlab("")+
       
       ggtitle("(c)") +
       
       theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
       
       ylim(-20,250)
     
     ggsave("output/myco/high_all_others.jpeg", dpi = "print", width = 27,height = 14 , units = "cm")
     
     
     # BEA
     
     myco %>% 
       
       filter(parameter=="BEA") %>% 
       
       filter(!sample_type=="frass") %>% 
       
       filter(!sample_type=="Raw_maize") %>% 
       
       filter(afla_level=="HIGH") %>% 
       
       ungroup() %>% 
       
       select(sample_type,parameter,maize_inclusion_perc, value_DM,replicate) %>% 
       
       mutate(sample_type=factor(sample_type)) %>% 
      
       group_by(maize_inclusion_perc,sample_type) %>% 
       
       summarise(n=n(),
                 mean=mean(value_DM),
                 sd=sd(value_DM)) %>% 
       
       select(-sd) %>% 
       
       spread(key = sample_type,value=mean) %>% 
       
       mutate(BEA=larvae/substrate)

         
     # statical evolution
     
     myco_eco_stat <-
     
     myco %>% 
       
       select(maize_inclusion_perc,afla_level,sample_type,replicate,parameter, value_DM) %>% 
       
       filter(!sample_type=="Raw_maize") %>% 
       
       filter(afla_level=="CONTROL") %>% 
       
       filter(maize_inclusion_perc=="80%DM maize") %>% 
       
       ungroup() %>% 
       
       select(-afla_level,-maize_inclusion_perc) %>% 
       
       spread(key = parameter,value=value_DM)
       
     
  # compare frass, larvae and substrate (individually for each mycotoxin)
     
     # set up model
     model <- lm(FB1 ~ sample_type, data = myco_eco_stat)
     
     # get (adjusted) weight means per group
     model_means <- emmeans(object = model,
                            specs = "sample_type")
     
     # add letters to each mean
     model_means_cld <- cld(object = model_means,
                            adjust = "tukey",
                            Letters = letters,
                            alpha = 0.05)
     
     # show output
     model_means_cld
            
     
  # loop over all columns/mycotoxins that are not all 0
     
     # Set up model for each column starting from column 3
     results <- list()
     for (col in 3:ncol(myco_eco_stat)) {
       column_name <- colnames(myco_eco_stat)[col]
       
       # Check if all values in the column are 0
       if (all(myco_eco_stat[, col] == 0)) {
         # Skip the column if all values are 0
         next
       }
       
       # Set up model
       model <- lm(paste0(column_name, " ~ sample_type"), data = myco_eco_stat)
       
       # Get (adjusted) weight means per group
       model_means <- emmeans(object = model, specs = "sample_type")
       
       # Add letters to each mean
       model_means_cld <- cld(object = model_means, adjust = "tukey", Letters = letters, alpha = 0.05)
       
       # Save results for the column
       results[[column_name]] <- model_means_cld
     }
     

     # ratio in mycotoxins between substrate and frass
     
     myco_stats %>% 
       
       filter(!sample_type=="Raw_maize",
              !sample_type=="larvae") %>% 
       
       select(-n,-sd) %>% 
       
       spread(key="sample_type",value="mean") %>% 
       
       na.omit(substrate) %>% 
       
       na.omit(frass) %>% 
       
       mutate(sort=case_when(frass > 0 ~"in",TRUE~"out")) %>% # only filter results were frass is > 0
       
       filter(sort=="in") %>%
       
       mutate(sort=case_when(substrate > 0 ~"in",TRUE~"out")) %>% # only filter results were frass is > 0
       
       filter(sort=="in") %>%
       
       mutate(ratio=frass/substrate) %>% 
  
       ungroup() %>% 
       
       summarise(max=max(ratio),
                 min=min(ratio))
       
       
     