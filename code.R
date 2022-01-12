#packages
source("Functions.R")
packages<-c("tidyverse","readxl","haven","rio","here","nombre","fs","openxlsx","writexl")
package_fn(packages)

#read data
text_files<-list.files(pattern = "*.TXT",path = paste0(here(),"/Data/Visit-1/"))
raw_data_files<-paste0(here(),"/Data/Visit-1/",text_files)
raw_data<-map(raw_data_files,read.delim,header=F,sep="\t") %>% 
        set_names(paste(nom_ord(1:17),"data",sep = "_"))
#raw_data<-raw_data[-1] #comment this line out if you want to use block 1 and block 2 info


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
#slice_num<-slice_num[-1] #simillarly here, comment out for using block 1 and 2 info

info_tbl<-map(slice_num,~lay_out_excel %>% 
                       slice(.x) %>% 
                       select("into"=2,
                              "sep"=9)) 
sep_vec_list<-map(info_tbl,~.x %>% select(2) %>% pull)
into_vec_list<-map(info_tbl,~.x %>% select(1) %>% pull)

#Final data sheets
data_list<-tibble(data=raw_data,sep=sep_vec_list,
                  into=into_vec_list) %>% 
        pmap(separate,col="V1") %>% 
        map(as_tibble)
data_list<-data_list %>% 
        map(~.x %>% mutate(across(.cols = -1,.fns = as.numeric)))

#Create replication of level 1 data

level_1_sep<-map(sep_vec_list[1],~replicate(n=length(raw_data)-1,
                                  .x,simplify = F)) %>% flatten() %>% 
        map(~.x[1:16])
        
all_levels_sep<-sep_vec_list[-1]
new_sep<-map2(level_1_sep,all_levels_sep,~c(.x,.y))

level_1_into<-map(into_vec_list[1],~replicate(n=length(raw_data)-1,
                                    .x,simplify = F)) %>% flatten()%>% 
        map(~.x[1:16])
all_levels_into<-into_vec_list[-1]
new_into<-map2(level_1_into,all_levels_into,~c(.x,.y))

new_data_list<-tibble(data=raw_data[-1],sep=new_sep,
                  into=new_into) %>% 
        pmap(separate,col="V1") %>% 
        map(as_tibble)

common_id_fn<-function(df,namevar,vars){
        
        df1<-df %>% unite(col = "common-id",vars,sep = "")
        df1
}

data_list_common<-map(new_data_list,common_id_fn,namevar="Common-ID",
        vars=c("LOT/FSU Serial No.","Hamlet group/Sub-block no.",
               "Second-stage-stratum no." ,"Sample hhld. No.")) %>% 
        map(arrange,"common-id")
data_list_common2<-map(data_list_common,~.x %>% select(-c(1,3:13))) %>% 
        map(~.x %>% mutate(across(!contains(c("common-id","Common-ID")),
                                                                 as.numeric)))


merge_data_common<-data_list_common2[-2]%>% reduce(inner_join,by="common-id") #not merging level 3

#Folder for Excel Sheets
output_folder<-"Excel-Sheets"
dir_create(output_folder)

#Write into one excel workbook all data
openxlsx::write.xlsx(data_list,paste0(here(),"/",output_folder,"/all_data.xlsx"))

#Write each data into separate excel workbooks- may be for processing in stata/spss
master_sheets<-"Master-Sheets"
dir_create(paste0(output_folder,"/",master_sheets))
walk2(data_list,paste0(here(),"/",output_folder,"/",master_sheets,
                       "/Level-",1:17,"_full",".xlsx"),
      ~write.xlsx(x=.x,file = .y))

#Write all data excluding level 1 into separate excel files
level_data<-paste0(output_folder,"/","Level-Data")
dir_create(level_data)
file_list<-paste0(here(),"/",level_data,"/Level-",2:17,".xlsx")
tibble(x=data_list_common2,file=file_list) %>% pwalk(write.xlsx)

#merging- before creating common-id - no more meaningful
merge_data<-data_list %>% reduce(full_join,by="Common-ID")

#write final merged data- not meaningful again
openxlsx::write.xlsx(merge_data,paste0(here(),"/",output_folder,"/merged.xlsx"))

#Merging Household data- after creation of common id
#level 3 is individual level data- so we leave that out
merge_raw_data<-data_list_common2[-2]
rm(raw_data,data_list,data_list_common,new_data_list,data_list_common2) #remove earlier created lists to save memory
gc() #clean unused memory
merged_data_final<-merge_raw_data %>% reduce(inner_join,by="common-id")


