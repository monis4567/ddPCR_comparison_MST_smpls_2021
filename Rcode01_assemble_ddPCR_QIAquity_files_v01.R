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
#make a column numeric
df_dd01$Concentration_copies_uL <-  as.numeric(df_dd01$Concentration_copies_uL)
# take log10 to a column plus one
df_dd01$l10_conc_cp_uL_p1 <- log10(df_dd01$Concentration_copies_uL+1)

CI_ccp <- df_dd01$Concentration_copies_uL*df_dd01$CI_95
df_dd01$sd <- log10(CI_ccp+1)
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

library(ggplot2)
# see it in a plot
plt02 <- ggplot2::ggplot(data=df_dd01, 
                         aes(y=Sample_NTC_Control,
                             x=l10_qPCRcopies_mean_p1,
                             group= spcAbr,
                             color= spcAbr)) +
  geom_point() +
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

c(df_dd01$l10_ddPCRcopies_mean_p1)
lddP <- length(df_dd01$l10_ddPCRcopies_mean_p1)
let1 <- c("A","B")
rbind(let1,df_dd01$l10_ddPCRcopies_mean_p1)

keeps <- c("Sample.spcAbr",
           #"qPCRcopies_mean",
           "l10_qPCRcopies_mean_p1",
           "qP_sd",
           "l10_qddcopies_mean_p1" ,
           "dd_sd")
# see column names
#colnames(df_dd01)
# only keep columns defined above
df_dd02 <- df_dd01[keeps]

dplyr::mutate(df_dd02)
# see it in a plot
plt03 <- ggplot2::ggplot(data=df_dd01, 
                         aes(y=Sample_NTC_Control,
                             x=l10_qPCRcopies_mean_p1,
                             group= spcAbr,
                             color= spcAbr)) +
  geom_point() +
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
tbl_BR09 <- readxl::read_xlsx(BR_test09)
#make it a dataframe
df_BR09 <- as.data.frame(tbl_BR09)
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
# add column to a new column
df_BR09$Conc_copies_uL <- df_BR09$'Conc(copies/µL)'


head(df_BR09,6)
