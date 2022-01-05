#packages
library(devtools)
source("Functions.R")
packages<-c("tidyverse","readxl","haven","rio","here","nombre")
package_fn(packages)

#read data
text_files<-list.files(pattern = "*.TXT",path = paste0(here(),"/Data/Visit-1/"))
raw_data_files<-paste0(here(),"/Data/Visit-1/",text_files)
raw_data<-map(raw_data_files,read.delim,header=F,sep="\t") %>% 
        set_names(paste(nom_ord(1:17),"data",sep = "_"))
raw_data<-raw_data[-1]


#layout specifications sheet
lay_out_excel<-read_excel(path =paste0(here(),
                                       "/Supporting documents/data_layout33v1.xls" ))

#wrangling- layout sheet
n1<-which(lay_out_excel$`VISIT NUMBER 1 AND 2`=="Item") # identify column name start
n2<-which(lay_out_excel$`VISIT NUMBER 1 AND 2`=="Blank") #identify the end of column names
n2<-n2-1 #adjust the blank column
n3<-n1+1 # skip header row
slice_num<-map2(n3,n2,seq) %>% 
        set_names(paste0("slice_cond",1:17))
slice_num<-slice_num[-1]

info_tbl<-map(slice_num,~lay_out_excel %>% 
                       slice(.x) %>% 
                       select("into"=2,
                              "sep"=9)) 
sep_vec_list<-map(info_tbl,~.x %>% select(2) %>% pull)
into_vec_list<-map(info_tbl,~.x %>% select(1) %>% pull)

#Final data sheets
data_list<-tibble(data=raw_data,sep=sep_vec_list,
                  into=into_vec_list) %>% 
        pmap(separate,col="V1")


#Write into excel sheets
openxlsx::write.xlsx(data_list,"test.xlsx")
