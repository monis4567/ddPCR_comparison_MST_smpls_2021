
wd00 <- "/home/hal9000/ddPCR_comparison_MST_smpls_2021"
setwd(wd00)
getwd()
#grep for the '.csv' files in the directory, and place in a list
lst_fcsv <- list.files(wd00)[grep("\\.csv",list.files(wd00))]
# make a variable to add count to
i <- 1
#make an empty list to add data frames to
lst_df1 <- list()
#iterate over elements in list
for (f in lst_fcsv)
{
  print(f)
  tbltmp <- read.table(f,sep=",",header=F, skip = 1, stringsAsFactors = F)
  tblhead <- read.table(f,sep=",",header=F, skip = 1, nrows = 1, stringsAsFactors = F)
  colnames(tbltmp) <- unlist(tblhead)
  dftmp <- as.data.frame(tbltmp, header=T, stringsAsFactors = F)
  dftmp <- dftmp[-1,]
  
  f1 <- gsub("\\.csv","",f)
  f1 <- gsub("-","_",f1)
  dftmp$filenm <- f1
  lst_df1[[i]] <- dftmp
  i <- i+1
}

df_dd01 <- as.data.frame(do.call(rbind,lst_df1))

head(df_dd01,3)
