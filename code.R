#packages
source("Functions.R")
packages<-c("tidyverse","readxl","haven","rio","here","nombre","fs","glue",
            "openxlsx","writexl","powerjoin","data.table","furrr")
package_fn(packages)

#Round and Visit Information
round<-"Round-70"
visit<-"Visit-1"

#read data
text_files<-list.files(pattern = "*.txt|*.TXT",path = paste0(here(),"/Data/",
                                                       round,"/",visit,"/"))
raw_data_files<- paste0(here(),"/Data/",
                        round,"/",visit,"/",text_files)
raw_data<-map(raw_data_files,read.delim,header=F,sep="\t") 
#raw_data<-raw_data[-1] #comment this line out if you want to use block 1 and block 2 info


#layout specifications sheet
lay_out_excel<-read_excel(path =paste0(here(),
                                       "/Supporting documents/",
                                       round,"/","data_layout33v1.xls" ))

#wrangling- layout sheet
n1<-which(lay_out_excel[2]=="Item")# identify column name start
if(round=="Round-77"){
n2<-which(lay_out_excel[2]=="Multiplier")
}else{
        n2<-which(lay_out_excel[2]=="Blank")   
        n2<-n2-1 #adjust the blank column
}        #identify the end of column names
 
n3<-n1+1 # skip header row

#layout information- based on availability of text-files
exclude_levels<-NULL

if(round=="Round-70" && visit=="Visit-2"){
        exclude_levels<-c(2,13)
}else if(round=="Round-70" && visit=="Visit-1" |round=="Round-77" && visit=="Visit-1" ){
        exclude_levels<-NULL
}else{
        exclude_levels<-c(3,9,15,18) # 
}

if(is.null(exclude_levels)){
        levels_label<-cardinal(seq_along(n1))
}else{
        levels_label<-cardinal(seq_along(n1))[-exclude_levels]  
}


#rename data files
raw_data<-raw_data %>% set_names(paste("level",levels_label,sep = "-"))

#Exclude some levels
slice_num<-map2(n3,n2,seq) %>% 
        set_names(paste0("slice_cond",seq_along(n2)))
#slice_num<-slice_num[-1] #similarly here, comment out for using block 1 and 2 info
if(is.null(exclude_levels)){
        slice_num<-slice_num
}else{
        slice_num<-slice_num[-exclude_levels]  
}

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
level_index<-which(into_vec_list$slice_cond1=="Level")-1

level_1_sep<-map(sep_vec_list[1],~replicate(n=length(raw_data)-1,
                                  .x,simplify = F)) %>% flatten() %>% 
        map(~.x[1:level_index])

#Exclude level one information        
all_levels_sep<-sep_vec_list[-1]
new_sep<-map2(level_1_sep,all_levels_sep,~c(.x,.y))

level_1_into<-map(into_vec_list[1],~replicate(n=length(raw_data)-1,
                                    .x,simplify = F)) %>% flatten()%>% 
        map(~.x[1:level_index])
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

if(round=="Round-70"){
        vars_common_id<-c("LOT/FSU Serial No.","Hamlet group/Sub-block no.",
                          "Second-stage-stratum no." ,"Sample hhld. No.","Visit number",
                          "Level","Filler")       
}else{
        vars_common_id<-c("FSU Serial No.","Second-stage-stratum no." ,"Sample hhld. No.",
                          "Visit number","Level","Filler")
}


data_list_common<-map(new_data_list,common_id_fn,namevar="Common-ID",
        vars=vars_common_id) %>% 
        map(arrange,"common-id")
data_list_common2<-map(data_list_common,~.x %>% select(-c(1,3:13))) %>% 
        map(~.x %>% mutate(across(!contains(c("common-id","Common-ID")),
                                                                 as.numeric)))

#Folder for Excel Sheets
output_folder<-paste0("Excel-Sheets/",round)
dir_create(output_folder)

#Write into one excel workbook all data
openxlsx::write.xlsx(data_list,paste0(here(),"/",output_folder,"/all_data-",round,
                                      visit,".xlsx"))

#Write each data into separate excel workbooks- may be for processing in stata/spss
master_sheets<-"Master-Sheets"
dir_create(paste0(output_folder,"/",master_sheets))
iwalk(data_list,
      ~write.xlsx(x=.x,file = glue(here(),"/",output_folder,"/",master_sheets,"/",
                                     .y,"-{round}","_{visit}",".xlsx")))

#Write all data excluding level 1 into separate excel files
level_data<-paste0(output_folder,"/","Level-Data")
dir_create(level_data)
file_list<-glue(here(),"/",level_data,"/{names(data_list_common2)}_{visit}",".xlsx")
tibble(x=data_list_common2,file=file_list) %>% pwalk(write.xlsx)

#merging- before creating common-id - no more meaningful
merge_data<-data_list %>% reduce(full_join,by="Common-ID")

#write final merged data- not meaningful again
openxlsx::write.xlsx(merge_data,paste0(here(),"/",output_folder,"/merged.xlsx"))

#Merging Household data- after creation of common id
if(round=="Round-70"){
        individual_level<-paste("level",cardinal(3),sep = "-")    
}else{
        individual_level<-paste("level",cardinal(2),sep = "-")
}

#level 3/2 is individual level data- so we leave that out
merge_raw_data<-data_list_common2 %>% discard(names(data_list_common2)==individual_level)
rm(raw_data,data_list,data_list_common,new_data_list,data_list_common2) #remove earlier created lists to save memory
gc() #clean unused memory

#Convert final list of data to data.table
list_DT<- map(merge_raw_data,setDT,key="common-id")

#merged_data_final<-merge_raw_data %>% reduce(power_inner_join,by="common-id")

merged_DT<-as.data.table(list_DT)

#Write Merged Data
write.xlsx(merged_DT,glue("merged_data","-{round}-","{visit}",".xlsx"))
fwrite(merged_DT,glue("merged_data","-{round}-","{visit}",".csv"))
