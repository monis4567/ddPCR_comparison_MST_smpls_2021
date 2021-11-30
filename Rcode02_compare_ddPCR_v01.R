#!/usr/bin/env Rscript
# -*- coding: utf-8 -*-



#____________________________________________________________________________#
# R-code provided for the project:

#remove everything in the working environment, without a warning!!
rm(list=ls())

#wd00 <- "/home/hal9000/ddPCR_comparison_MST_smpls_2021"
wd00 <- "/home/hal9000/Documents/Documents/NIVA_Ansaettelse_2021/ddPCR_comparison_MST_smpls_2021"
setwd(wd00)

getwd()
#grep for the '.csv' files in the directory, and place in a list
lst_fcsv <- list.files(wd00)[grep("\\.csv",list.files(wd00))]
lst_fcsv <- lst_fcsv[grep("QIAcuity",lst_fcsv)]

# make a variable to add count to
i <- 1
#make an empty list to add data frames to
lst_df1 <- list()
#iterate over elements in list
for (f in lst_fcsv)
{
  print(f)
  #read in the table, skip the first row
  tbltmp <- read.table(f,sep=",",header=F, skip = 1, stringsAsFactors = F)
  #read in the table again, and skip the first row, and then only retain the 
  # first row
  tblhead <- read.table(f,sep=",",header=F, skip = 1, nrows = 1, stringsAsFactors = F)
  #rename the columns using the headers read in above
  colnames(tbltmp) <- unlist(tblhead)
  #make it a data frame
  dftmp <- as.data.frame(tbltmp, header=T, stringsAsFactors = F)
  #remove the first row
  dftmp <- dftmp[-1,]
  #substitute in the file name
  f1 <- gsub("\\.csv","",f)
  f1 <- gsub("-","_",f1)
  #add the substituted filename as a column
  dftmp$filenm <- f1
  #add the data frame to the list of data frames
  lst_df1[[i]] <- dftmp
  # add an increment to the number keeping count of iterations
  i <- i+1
  #end the iteration over elements
}
#make the list of data frames a single data frame
df_dd01 <- as.data.frame(do.call(rbind,lst_df1))
#substitute in column names
colnames(df_dd01) <- gsub("\\(","", colnames(df_dd01))
colnames(df_dd01) <- gsub("\\)","", colnames(df_dd01))
colnames(df_dd01) <- gsub(" ","_", colnames(df_dd01))
colnames(df_dd01) <- gsub("\\%","", colnames(df_dd01))
colnames(df_dd01) <- gsub("\\/","_", colnames(df_dd01))
colnames(df_dd01) <- gsub("µL","uL", colnames(df_dd01))
#Recalculate percentage values
df_dd01$CI_95 <- gsub("\\%","",df_dd01$CI_95)
df_dd01$CI_95 <- gsub("-","0",df_dd01$CI_95)
df_dd01$CI_95 <- as.numeric(df_dd01$CI_95)/100
df_dd01$Sample_NTC_Control <- gsub("Mnelei","std",df_dd01$Sample_NTC_Control)
#rename column header
colnames(df_dd01)[1] <- "wellNm"
#split string in variable by delimiter, and make it a dataframe
filenmVl <- data.frame(do.call('rbind', strsplit(as.character(df_dd01$filenm),'_',fixed=TRUE)))
#use the columns in this dataframe to append to the original main data frame
df_dd01$ddPCRNo   <- filenmVl$X1
df_dd01$MONISprj  <- filenmVl$X2
df_dd01$spcAbr    <- filenmVl$X4
df_dd01$pltNo     <- filenmVl$X5
df_dd01$ddPCRmch  <- filenmVl$X6
df_dd01$rundate   <- as.character(gsub("rundate","",filenmVl$X7))
#the ddPCR QIAcuity mix comprised a total volume of 40 uL
# I added 2 uL template
df_dd01$Concentration_copies_uL <- df_dd01$Concentration_copies_uL*40/2
#make a column numeric
df_dd01$Concentration_copies_uL <-  as.numeric(df_dd01$Concentration_copies_uL)
# take log10 to a column plus one
df_dd01$l10_conc_cp_uL_p1 <- log10(df_dd01$Concentration_copies_uL+1)

CI_ccp <- df_dd01$Concentration_copies_uL*df_dd01$CI_95
df_dd01$sd <- log10(CI_ccp+1)

#exclude sample MST0147 as it was excluded in the ddPCR BioRad setup
# and is better to exclude to be able to compare plots
df_dd01 <- df_dd01[!df_dd01$Sample_NTC_Control=="MST0147",]
#head(df_dd01,4)
library(ggplot2)
# see it in a plot
plt01 <- ggplot2::ggplot(data=df_dd01, 
                         aes(y=Sample_NTC_Control,
                             x=l10_conc_cp_uL_p1,
                             group= spcAbr,
                             color= spcAbr)) +
                  geom_point() +
# See this website for adding error bars:
  #http://sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization
                  geom_errorbar(aes(xmin=l10_conc_cp_uL_p1-sd,
                                    xmax=l10_conc_cp_uL_p1+sd), width=0.2)#,
                      #position=position_dodge(.9))
#modify the axis labels
plt01 <- plt01 + xlab("log10 to (conc in copies per uL plus 1)") + ylab("sample")

# you will have to change the legend for all legends
plt01 <- plt01 + labs(color='species')
plt01 <- plt01 + labs(fill='species')
plt01 <- plt01 + labs(shape='species')
#change the title of the plot
plt01 <- plt01 + labs(title = "ddPCR w QIAcuity")#,
# see the plot
plt01


library(ggplot2)
# see it in a plot
plt01 <- ggplot2::ggplot(data=df_dd01, 
                         aes(y=Sample_NTC_Control,
                             x=l10_conc_cp_uL_p1,
                             group= spcAbr,
                             color= spcAbr)) +
  
  geom_point() +
  #scale_x_continuous(limits = c(0, 8)) + 
  coord_cartesian(xlim = c(0, 8)) +
  facet_wrap(~spcAbr, nrow = 2) + #'facet_wrap' subsets by column value in dataframe
  # See this website for adding error bars:
  #http://sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization
  geom_errorbar(aes(xmin=l10_conc_cp_uL_p1-sd,
                    xmax=l10_conc_cp_uL_p1+sd), width=0.2)#,

#position=position_dodge(.9))
#modify the axis labels
plt01 <- plt01 + xlab("log10 to (conc in copies per uL plus 1)") + ylab("sample")

# you will have to change the legend for all legends
plt01 <- plt01 + labs(color='species')
plt01 <- plt01 + labs(fill='species')
plt01 <- plt01 + labs(shape='species')
#change the title of the plot
plt01 <- plt01 + labs(title = "ddPCR QIAcuity")#,
# see the plot
plt01


#define path for file with MST collection data for NOVANA smpls from 2019-2020
pth_MSTcol <- "/home/hal9000/Documents/Documents/NIVA_Ansaettelse_2020/NOVANA_proever_2018_2019/out03_match_csv_MST_w_extractions"
#define path for file with qPCR data for NOVANA smpls from 2019-2020
pth_qPCRdt <- "/home/hal9000/Documents/Documents/NIVA_Ansaettelse_2020/NOVANA_proever_2018_2019/out02_merged_txtfiles_from_BioRad_for_MONIS5"
pth_qPCRdt <- "/home/hal9000/Documents/Documents/NIVA_Ansaettelse_2020/NOVANA_proever_2018_2019/out05_stdcrv_plots_and_tables_from_Rcode"
#define file name with MST collection data for NOVANA smpls from 2019-2020
file_MSTcol <- "smpl_locaMST_2019_2020_v01.csv"
#define file name with qPCR data for NOVANA smpls from 2019-2020
file_qPCRdt <- "out05_MONIS5_eDNA_smpls04.csv"
# paste path and file name together
pthfl_MSTcol <- paste0(pth_MSTcol,"/",file_MSTcol)
pthfl_qPCRdt <- paste0(pth_qPCRdt,"/",file_qPCRdt)

# copy the files to the new folder
# mainly because a different code elsewhere produces these files
file.copy(pthfl_MSTcol, wd00)
file.copy(pthfl_qPCRdt, wd00)
#read in tables
df_qP01 <- read.table(file_qPCRdt,header=T,sep=",",stringsAsFactors = F)
df_MST01 <- read.table(file_MSTcol,header=T,sep=",",stringsAsFactors = F)
# get only the number from the MST samples
df_MST01$MSTno <- gsub("MST","",df_MST01$Vandprvenummer_unikt_nummer_U_Pr_Nr)
#pad with zeros to two characters
#see this website: https://stackoverflow.com/questions/5812493/adding-leading-zeros-using-r
df_MST01$MSTno <-stringr::str_pad(df_MST01$MSTno, 4, pad = "0")
#paste 'MST' back in front of the sample number
df_MST01$MSTno <- paste("MST",df_MST01$MSTno,sep="")
#get longitude for MST sample
df_dd01$dc_lon <-df_MST01$position_lngdegrad_lok_pos[match(df_dd01$Sample_NTC_Control,df_MST01$MSTno)]
#get latitude for MST sample
df_dd01$dc_lat <- df_MST01$position_breddegrad_lok_pos[match(df_dd01$Sample_NTC_Control,df_MST01$MSTno)]
#get volume water filtered for the sample in mL
df_dd01$Vwf_mL <- df_MST01$Volumen_vand_filtreret_Vwf[match(df_dd01$Sample_NTC_Control,df_MST01$MSTno)]
#get collection date
df_dd01$cll.date <- df_MST01$dato_dato_inds[match(df_dd01$Sample_NTC_Control,df_MST01$MSTno)]
# paste species abbreviation together with sample name
df_dd01$Sample.spcAbr <- paste(df_dd01$Sample_NTC_Control,".",df_dd01$spcAbr,sep="")


#pad with zeroes in the MST samples 
df_qP01$smpltp[grepl("^MST",df_qP01$smpltp)] <- paste("MST",stringr::str_pad(gsub("MST","",df_qP01$smpltp[grepl("^MST",df_qP01$smpltp)]), 4, pad = "0"),sep="")

# paste species abbreviatoin and MST sample name together
df_qP01$smpltp.speciesabbr <- paste(df_qP01$smpltp,".",df_qP01$speciesabbr,sep="")
#make NAs zero
df_qP01$Quantitycopies[is.na(df_qP01$Quantitycopies)] <- 0
#make numeric
df_qP01$Quantitycopies <- as.numeric(df_qP01$Quantitycopies)
# I used 3 uL template, this means the original sample contains 1/3
df_qP01$Quantitycopies <- df_qP01$Quantitycopies*1/3

df_qP01$Qcp_l10 <- log10(df_qP01$Quantitycopies+1)
#deinfe columns to keep
keeps <- c("Qcp_l10","smpltp.speciesabbr")
#subset data frame to columns to keep
df_qP02 <-  df_qP01[keeps]

# use dplyr to get average and sd of triplicates in qPCR
library(dplyr)
tbl_qP01 <- df_qP02 %>% group_by(smpltp.speciesabbr) %>% summarise_each(funs(mean, sd))

#match qPCR mean and sd for copy count back to ddPCR data frame
df_dd01$qPCRcopies_mean <- tbl_qP01$mean[match(df_dd01$Sample.spcAbr,tbl_qP01$smpltp.speciesabbr)]
df_dd01$qPCRcopies_sd <- tbl_qP01$sd[match(df_dd01$Sample.spcAbr,tbl_qP01$smpltp.speciesabbr)]

#df_dd01$Concentration_copies_uL/3
#df_dd01$qPCRcopies_mean
#df_dd01$qPCRcopies_sd
#head(df_MST01)
#head(df_dd01,4)
#make a column numeric
df_dd01$qPCRcopies_mean <-  as.numeric(df_dd01$qPCRcopies_mean)
# take log10 to a column plus one
df_dd01$l10_qPCRcopies_mean_p1 <- log10(df_dd01$qPCRcopies_mean+1)
df_dd01$l10_qPCRcopies_mean_p1 <- df_dd01$qPCRcopies_mean

df_dd01$qP_sd <- log10(df_dd01$qPCRcopies_sd+1)
df_dd01$qP_sd  <- df_dd01$qPCRcopies_sd

df_dd01$qPCRcopies_mean[is.na(df_dd01$qPCRcopies_mean)] <- 0
df_dd01$qPCRcopies_mean <- as.numeric(df_dd01$qPCRcopies_mean) 

df_dd01$qPCRcopies_sd[is.na(df_dd01$qPCRcopies_sd)] <- 0
df_dd01$qPCRcopies_sd <- as.numeric(df_dd01$qPCRcopies_sd) 

df_dd01$l10_qPCRcopies_mean_p1 <- df_dd01$qPCRcopies_mean
df_dd01$qP_sd <- df_dd01$qPCRcopies_sd

library(ggplot2)
# see it in a plot
plt02 <- ggplot2::ggplot(data=df_dd01, 
                         aes(y=Sample_NTC_Control,
                             x=l10_qPCRcopies_mean_p1,
                             group= spcAbr,
                             color= spcAbr)) +
  geom_point() +
  #scale_x_continuous(limits = c(0, 8)) + 
  coord_cartesian(xlim = c(0, 8)) +
  facet_wrap(~spcAbr, nrow = 2) + #'facet_wrap' subsets by column value in dataframe
  # See this website for adding error bars:
  #http://sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization
  geom_errorbar(aes(xmin=l10_qPCRcopies_mean_p1-qP_sd,
                    xmax=l10_qPCRcopies_mean_p1+qP_sd), width=0.2)#,
#position=position_dodge(.9))
#modify the axis labels
plt02 <- plt02 + xlab("log10 to (conc in copies per uL plus 1)") + ylab("sample")

# you will have to change the legend for all legends
plt02 <- plt02 + labs(color='species')
plt02 <- plt02 + labs(fill='species')
plt02 <- plt02 + labs(shape='species')
#change the title of the plot
plt02 <- plt02 + labs(title = "qPCR BioRad")#,
# see the plot
plt02


df_dd01$l10_qPCRcopies_mean_p1[is.na(df_dd01$l10_qPCRcopies_mean_p1)] <- 0
df_dd01$qP_sd[is.na(df_dd01$qP_sd)] <- 0
df_dd01$l10_qPCRcopies_mean_p1 <- as.numeric(df_dd01$l10_qPCRcopies_mean_p1)
df_dd01$qP_sd  <- as.numeric(df_dd01$qP_sd)
# copy columns, just to give them new names
df_dd01$dd_sd <- df_dd01$sd
df_dd01$l10_ddPCRcopies_mean_p1 <- df_dd01$l10_conc_cp_uL_p1

#c(df_dd01$l10_ddPCRcopies_mean_p1)
lddP <- length(df_dd01$l10_ddPCRcopies_mean_p1)
let1 <- c("A","B")
#rbind(let1,df_dd01$l10_ddPCRcopies_mean_p1)
#
keeps <- c("Sample.spcAbr",
           #"qPCRcopies_mean",
           "l10_qPCRcopies_mean_p1",
           "qP_sd",
           "l10_qddcopies_mean_p1" ,
           "dd_sd")
# see column names
#colnames(df_dd01)
# only keep columns defined above
#df_dd02 <- df_dd01[keeps]

#dplyr::mutate(df_dd02)
# see it in a plot
plt03 <- ggplot2::ggplot(data=df_dd01, 
                         aes(y=Sample_NTC_Control,
                             x=l10_qPCRcopies_mean_p1,
                             group= spcAbr,
                             color= spcAbr)) +
  geom_point() +
  #scale_x_continuous(limits = c(0, 8)) + 
  coord_cartesian(xlim = c(0, 8)) +
  facet_wrap(~spcAbr, nrow = 2) + #'facet_wrap' subsets by column value in dataframe
  # See this website for adding error bars:
  #http://sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization
  geom_errorbar(aes(xmin=l10_qPCRcopies_mean_p1-qP_sd,
                    xmax=l10_qPCRcopies_mean_p1+qP_sd), width=0.2)#,
#position=position_dodge(.9))
#modify the axis labels
plt03 <- plt03 + xlab("log10 to (conc in copies per uL plus 1)") + ylab("sample")

# you will have to change the legend for all legends
plt03 <- plt03 + labs(color='species')
plt03 <- plt03 + labs(fill='species')
plt03 <- plt03 + labs(shape='species')
#change the title of the plot
plt03 <- plt03 + labs(title = "qPCR BioRad")#,
# see the plot
plt03


##

#3

#_______________________________________________________________________________
#Read in excel file with results from BioRad ddPCR
BR_test09 <- "Test09 Carsten_20211118_133632_844.xlsx"
BR_test10 <- "Test 10 start Carsten.xlsx"
tbl_BR09 <- readxl::read_xlsx(BR_test09)
tbl_BR10 <- readxl::read_xlsx(BR_test10)
#make it a dataframe
df_BR09 <- as.data.frame(tbl_BR09)
df_BR10 <- as.data.frame(tbl_BR10)
#head(df_BR09,5)
#Read in well names for ddPCR plate
tbl_wlnm <- readxl::read_xlsx("wellnames_test009_010_ddPCR_BioRad.xlsx")
#make it a dataframe
df_wlnm <- as.data.frame(tbl_wlnm, header=F)
#substitute in wellpositions
welllet <- gsub("^([A-Za-z]+)([0-9]+)$","\\1",df_wlnm$Wellpos)
wellnmb <- gsub("^([A-Za-z]+)([0-9]+)$","\\2",df_wlnm$Wellpos)
#pad with zeros to two characters
#see this website: https://stackoverflow.com/questions/5812493/adding-leading-zeros-using-r
wellnmb <-stringr::str_pad(wellnmb, 2, pad = "0")
# paste together and replace well postion
df_wlnm$Wellpos <- paste(welllet,wellnmb, sep="")
#add wellname to ddPCR df
df_BR09$wllnm <- df_wlnm$wllnm[match(df_BR09$Well, df_wlnm$Wellpos)]
#
df_BR10$wllnm <- df_wlnm$wllnm[match(df_BR10$Well, df_wlnm$Wellpos)]
#substitute to retain species abbreviation or MST sample no
df_BR09$spcAbr <- gsub("^(.*)_(.*)$","\\1",df_BR09$wllnm)
#
df_BR10$spcAbr <- gsub("^(.*)_(.*)$","\\1",df_BR10$wllnm)
#
df_BR09$MSTsmpl <- gsub("^(.*)_(.*)$","\\2",df_BR09$wllnm)
#
df_BR10$MSTsmpl <- gsub("^(.*)_(.*)$","\\2",df_BR10$wllnm)
#
df_BR09$spcAbr <- gsub("_std","",df_BR09$spcAbr)
#
df_BR10$spcAbr <- gsub("_std","",df_BR10$spcAbr)
# add column to a new column
df_BR09$Conc_copies_uL <- df_BR09$'Conc(copies/µL)'
#
df_BR10$Conc_copies_uL <- df_BR10$'Conc(copies/µL)'
# multiply w 3/2 because 2 uL extract was used instead
# 3 uL as was used in the other setups
#df_BR09$Conc_copies_uL <- df_BR09$Conc_copies_uL*(3/2)

# the total volume of ddPCR mix was 25 uL
# I used 2 uL of original template

df_BR09$Conc_copies_uL <- df_BR09$Conc_copies_uL*25/2
#
df_BR10$Conc_copies_uL <- df_BR10$Conc_copies_uL*25/2
#df_BR09$Conc_copies_uL <- df_BR09$`Copies/20µLWell`
#make numeric
df_BR09$Conc_copies_uL <- as.numeric(df_BR09$Conc_copies_uL)
df_BR10$Conc_copies_uL <- as.numeric(df_BR10$Conc_copies_uL)

#take log10 plus to all eDNA concenctrations
df_BR09$l10_copies_mean_p1 <- log10(df_BR09$Conc_copies_uL +1)
df_BR09$l10_copies_mean_p1 <- log10(df_BR09$Conc_copies_uL +1)
#
df_BR10$l10_copies_mean_p1 <- log10(df_BR10$Conc_copies_uL +1)
df_BR10$l10_copies_mean_p1 <- log10(df_BR10$Conc_copies_uL +1)
# if any are NAs then replace them with zeros
df_BR09$l10_copies_mean_p1[is.na(df_BR09$l10_copies_mean_p1)] <- 0
#
df_BR10$l10_copies_mean_p1[is.na(df_BR10$l10_copies_mean_p1)] <- 0


#make numeric
df_BR09$PoissonConfMin <- as.numeric(df_BR09$PoissonConfMin)
df_BR09$PoissonConfMax <- as.numeric(df_BR09$PoissonConfMax)
#
df_BR10$PoissonConfMin <- as.numeric(df_BR10$PoissonConfMin)
df_BR10$PoissonConfMax <- as.numeric(df_BR10$PoissonConfMax)

df_BR09$PoissonConfidenceMax68 <- as.numeric(df_BR09$PoissonConfidenceMax68)
df_BR09$PoissonConfidenceMin68 <- as.numeric(df_BR09$PoissonConfidenceMin68)
#
df_BR10$PoissonConfidenceMax68 <- as.numeric(df_BR10$PoissonConfidenceMax68)
df_BR10$PoissonConfidenceMin68 <- as.numeric(df_BR10$PoissonConfidenceMin68)

# multiply w 3/2 because 2 uL extract was used instead
# 3 uL as was used in the other setups
# df_BR09$PoissonConfMax <- df_BR09$PoissonConfMax*(3/2)
# df_BR09$PoissonConfMin <- df_BR09$PoissonConfMin*(3/2)
#df_BR09$Conc_copies_uL[1:3]
#df_BR09$PoissonConfidenceMax68[1:3]

df_BR09$PoissonConfMax <- df_BR09$PoissonConfMax*(25/2)
df_BR09$PoissonConfMin <- df_BR09$PoissonConfMin*(25/2)

df_BR10$PoissonConfMax <- df_BR10$PoissonConfMax*(25/2)
df_BR10$PoissonConfMin <- df_BR10$PoissonConfMin*(25/2)

df_BR09$PoisConfMx68 <- df_BR09$PoissonConfidenceMax68*(25/2)
df_BR09$PoisConfMi68 <- df_BR09$PoissonConfidenceMin68*(25/2)

df_BR10$PoisConfMx68 <- df_BR10$PoissonConfidenceMax68*(25/2)
df_BR10$PoisConfMi68 <- df_BR10$PoissonConfidenceMin68*(25/2)
# if any are NAs then replace them with zeros
df_BR09$PoissonConfMin[is.na(df_BR09$PoissonConfMin)] <- 0
df_BR09$PoissonConfMax[is.na(df_BR09$PoissonConfMax)] <- 0

df_BR10$PoissonConfMin[is.na(df_BR10$PoissonConfMin)] <- 0
df_BR10$PoissonConfMax[is.na(df_BR10$PoissonConfMax)] <- 0

df_BR09$PoisConfMx68[is.na(df_BR09$PoisConfMx68)] <- 0
df_BR09$PoisConfMi68[is.na(df_BR09$PoisConfMi68)] <- 0

df_BR10$PoisConfMx68[is.na(df_BR10$PoisConfMx68)] <- 0
df_BR10$PoisConfMi68[is.na(df_BR10$PoisConfMi68)] <- 0

# take log10 plus 1 to sd
df_BR09$ddPBR_sdmn_l10 <- log10(df_BR09$PoissonConfMin+1)
df_BR09$ddPBR_sdmx_l10 <- log10(df_BR09$PoissonConfMax+1)

df_BR10$ddPBR_sdmn_l10 <- log10(df_BR10$PoissonConfMin+1)
df_BR10$ddPBR_sdmx_l10 <- log10(df_BR10$PoissonConfMax+1)

#df_BR09$PoisConfMx68
df_BR09$ddPBR_sdmn_l10 <- log10(df_BR09$PoisConfMi68+1)
df_BR09$ddPBR_sdmx_l10 <- log10(df_BR09$PoisConfMx68+1)

df_BR10$ddPBR_sdmn_l10 <- log10(df_BR10$PoisConfMi68+1)
df_BR10$ddPBR_sdmx_l10 <- log10(df_BR10$PoisConfMx68+1)


# (df_BR09$l10_copies_mean_p1)[1:3]
# (df_BR09$ddPBR_sdmn_l10)[1:3]
# (df_BR09$ddPBR_sdmx_l10)[1:3]


df_BR09$MSTsmpl[grepl("E",df_BR09$MSTsmpl)] <- paste("std",df_BR09$MSTsmpl[grepl("E",df_BR09$MSTsmpl)],sep="")
df_BR10$MSTsmpl[grepl("E",df_BR10$MSTsmpl)] <- paste("std",df_BR10$MSTsmpl[grepl("E",df_BR10$MSTsmpl)],sep="")
#head(df_BR09,6)

#dplyr::mutate(df_dd02)
# see it in a plot
plt04 <- ggplot2::ggplot(data=df_BR09, 
                         aes(y=MSTsmpl,
                             x=l10_copies_mean_p1,
                             group= spcAbr,
                             color= spcAbr)) +
  geom_point() +
  #scale_x_continuous(limits = c(0, 8)) + 
  coord_cartesian(xlim = c(0, 8)) +
  facet_wrap(~spcAbr, nrow = 2) + #'facet_wrap' subsets by column value in dataframe
  # See this website for adding error bars:
  #http://sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization
  geom_errorbar(aes(xmin=ddPBR_sdmn_l10, 
                    xmax=ddPBR_sdmx_l10), width=.2,
              position=position_dodge(.9))
#position=position_dodge(.9))
#modify the axis labels
plt04 <- plt04 + xlab("log10 to (conc in copies per uL plus 1)") + ylab("sample")
plt04
# you will have to change the legend for all legends
plt04 <- plt04 + labs(color='species')
plt04 <- plt04 + labs(fill='species')
plt04 <- plt04 + labs(shape='species')
#change the title of the plot
plt04 <- plt04 + labs(title = "BioRad test ddPCR009")#,
# see the plot
plt04



# see it in a plot
plt05 <- ggplot2::ggplot(data=df_BR10, 
                         aes(y=MSTsmpl,
                             x=l10_copies_mean_p1,
                             group= spcAbr,
                             color= spcAbr)) +
  geom_point() +
  #scale_x_continuous(limits = c(0, 8)) + 
  coord_cartesian(xlim = c(0, 8)) +
  facet_wrap(~spcAbr, nrow = 2) + #'facet_wrap' subsets by column value in dataframe
  # See this website for adding error bars:
  #http://sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization
  geom_errorbar(aes(xmin=ddPBR_sdmn_l10, 
                    xmax=ddPBR_sdmx_l10), width=.2,
                position=position_dodge(.9))
#position=position_dodge(.9))
#modify the axis labels
plt05 <- plt05 + xlab("log10 to (conc in copies per uL plus 1)") + ylab("sample")
plt05
# you will have to change the legend for all legends
plt05 <- plt05 + labs(color='species')
plt05 <- plt05 + labs(fill='species')
plt05 <- plt05 + labs(shape='species')
#change the title of the plot
plt05 <- plt05 + labs(title = "BioRad test ddPCR010")#,
# see the plot
plt05



#_______________________________________________________________________________
#_______________________________________________________________________________
# Now combine all plots in to one diagram
    plt001 <-  plt01 # ddPCR QIAcuity
    plt002 <-  plt02 # qPCR BioRad
    plt003 <-  plt04 # ddPCR BioRad
    plt004 <-  plt05 # ddPCR BioRad
# Add titles
# see this example: https://www.datanovia.com/en/blog/ggplot-title-subtitle-and-caption/
# p01t <- p01 + labs(title = "Amphibians detected by eDNA",
#               subtitle = "approv controls and 1 or 2 pos repl")#,
#caption = "Data source: ToothGrowth")
p01t <- plt001 + labs(title = "A - ddPCR QIAcuity")#,
# Add titles
# p02t <- p02 + labs(title = "eDNA samples attempted",
#                    subtitle = "at least approv controls and 1 or 2 pos repl")#,
p02t <- plt002 + labs(title = "B - qPCR BioRad")#,
# Add titles
# p03t <- p03 + labs(title = "eDNA samples attempted",
#                    subtitle = "both unapprov controls and approv contrl")#,
p03t <- plt003 + labs(title = "C - ddPCR_009 BioRad")#,

p04t <- plt004 + labs(title = "D - ddPCR_010 BioRad")#,to H10 and A11 to H11 and A12 to H12.

#see the plot
# p01t
# p02t
# p03t

# ------------- plot Combined figure -------------
library(patchwork)
# set a variable to TRUE to determine whether to save figures
bSaveFigures <- T
#getwd()
#define a filename to save to
fnm03 <- "compare_ddPCR_w_qPCR_on_eDNA_samples_02"

#see this website: https://www.rdocumentation.org/packages/patchwork/versions/1.0.0
# on how to arrange plots in patchwork
p <-  p01t +
      p02t +
      p03t +
      p04t +
  
  plot_layout(nrow=1,ncol=4,byrow=T) + #xlab(xlabel) +
  plot_layout(guides = "collect") +
  plot_annotation(caption=fnm03) #& theme(legend.position = "bottom")
#p
#make filename to save plot to
figname02 <- paste0(fnm03,".png")


if(bSaveFigures==T){
  ggsave(p,file=figname02,
         #width=210,height=297,
         width=297,height=210,
         units="mm",dpi=300)
}

#
fnm04 <- "compare_ddPCR_w_qPCR_on_eDNA_samples_03"
fnm05 <- "compare_ddPCR_w_qPCR_on_eDNA_samples_04"
#see this website: https://www.rdocumentation.org/packages/patchwork/versions/1.0.0
# on how to arrange plots in patchwork
#p <-  p01b +
p +
    #
  
  plot_layout(nrow=1,ncol=4,byrow=T) + #xlab(xlabel) +
  plot_layout(guides = "collect") +
  plot_annotation(caption=fnm04) #& theme(legend.position = "bottom")
#p
#make filename to save plot to
figname03 <- paste0(fnm04,".png")
figname04 <- paste0(fnm05,".pdf")


if(bSaveFigures==T){
  ggsave(p,file=figname03,
         #width=210,height=297,
         width=297,height=210,
         units="mm",dpi=300)
}

if(bSaveFigures==T){
  ggsave(p,file=figname04,
         #width=210,height=297,
         width=297,height=210,
         units="mm",dpi=300)
}


#make minor extraction from grand ddPCR file
c <- df_BR09$Conc_copies_uL[1:3]
w <- df_BR09$Well[1:3]

pmx<- df_BR09$PoissonConfidenceMax68[1:3]
pmi <- df_BR09$PoissonConfidenceMin68[1:3]

df_A1toA3 <- as.data.frame(cbind(w,c,pmx,pmi))
colnames(df_A1toA3) <- c("WellName",
                         "conc copies/uL",
                         "PoissonConfidenceMax68",
                         "PoissonConfidenceMin68")

pth_fl02 <-  paste(wd00,"/","Table01_with_well_A1_to_A3_from_ddPCR_BioRad_test009_2021nov17.html",sep="")
# get package
if(!require("kableExtra")){
  # see https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html#Table_Styles
  #For dev version
  install.packages("devtools")
  devtools::install_github("haozhu233/kableExtra")
  
  library("kableExtra")
}
Tbltxt01 <- "Table 1. Extract from excel file for ddPCR BioRad test 009"
df_A1toA3_v2 <- df_A1toA3 %>%
  kableExtra::kbl(caption = Tbltxt01) %>%
  kableExtra::kable_classic(full_width = F,html_font = "Cambria") %>%
  kableExtra::kable_styling(latex_options = c("striped")) # %>%
#column_spec(1, italic = T) 
#and to export in a file a html file
kableExtra::save_kable(df_A1toA3_v2,file=pth_fl02)


#