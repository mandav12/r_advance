#Table 14.1.2 Subject Disposition by Treatment (Safety Population
setwd("/Users/ramanireddy/Documents/SAS/work/myR/projects/R Project/ADAM Datasets")
wd<-getwd()
wd
if(!is.null(wd)) setwd(wd)
rm(list=ls(all.names=TRUE))
library(reporter)
library(magrittr)
library(gt)

#install.packages("gt")
pac <- c("tidyverse", "pharmaversesdtm", "sassy", "hms", "metacore", "readxl", "haven", "lubridate", "admiral","r2rtf","flextable")
lapply(pac, function(p) library(p, character.only=TRUE, warn.conflicts=TRUE))

#assigning file path for logfile

log_file<-"/Users/ramanireddy/Documents/SAS/work/myR/projects/R Project/Output/log"
#open the log file 
log_open(file.path(log_file,"Table_adsl_14.1.2.log"))

fpath<-"/Users/ramanireddy/Documents/SAS/work/myR/projects/R Project/Output/"

sep("calling ADSL dataset")

setwd("/Users/ramanireddy/Documents/SAS/work/myR/projects/R Project/ADAM Datasets")
adsl<-read_sas("adsl.sas7bdat")
#file path to save the output
fpath<-"/Users/ramanireddy/Documents/SAS/work/myR/projects/R Project/Output/"


adsl1<-adsl %>%
  mutate(ACTARM="Overal")
combine <-rbind(adsl,adsl1) %>%
  filter (SAFFL=="Y") %>%
  arrange (ACTARM) 


bign1<-combine %>%
  filter(SAFFL=="Y")%>%
  group_by(ACTARM) %>%
  summarise(bign=n()) 

sep(" Tabulate for the subjeccts planned")

sub_plan <-combine %>%
  filter (RANDFL =="Y") %>%
  group_by(ACTARM) %>%
  summarise(sub_cnt = n())%>%
  mutate(Response = "Subjects Planned",
         order =1)
sep(" Tabulate for the subjeccts enrolled")

sub_enrl <-adsl %>%
  filter (SCRNFL =="Y") %>%
 # group_by(ACTARM) %>%
  summarise(sub_cnt = n())%>%
  mutate(Response = "Subjects Enrolled",
         order =2,
         ACTARM ="Overal")

sep(" Tabulate for the subjeccts enrolled")
sub_dosed <-combine %>%
  filter (SAFFL =="Y") %>%
   group_by(ACTARM) %>%
  summarise(sub_cnt = n())%>%
  mutate(Response = "Subjects Dosed",
         order =3)


sep(" Tabulate for the subjeccts completed")
sub_compl <-combine %>%
  filter (SAFFL =="Y" & EOSSTT =="Completed") %>%
  group_by(ACTARM) %>%
  summarise(sub_cnt = n())%>%
  mutate(Response = "Subjects Completed",
         order =4)

sep(" Tabulate for the subjeccts Withdrawn")
sub_wdran <-combine %>%
  filter (SAFFL =="Y" & EOSSTT =="Discontinued") %>%
  group_by(ACTARM) %>%
  summarise(sub_cnt = n())%>%
  mutate(Response = "Subjects Withdrwan",
         order =5)

sep(" Tabulate for the  reasons for subjeccts Withdrawn")

sub_wdreas <-combine %>%
  filter (SAFFL =="Y" & EOSSTT =="Discontinued") %>%
  group_by(ACTARM,DCSREAS) %>%
  summarise(sub_cnt = n())%>%
  mutate(order =6,
         Response =case_when(DCSREAS == "ADVERSE EVENT"    ~  "  Adverse Event" ,
                             DCSREAS == "LOST TO FOLLOW-UP" ~ "  Lost to Follow-up",
                             DCSREAS == "WITHDRAWAL BY SUBJECT" ~ "  Subject Withdrew Consent")) %>%
  select(-DCSREAS)
sep("combine all the datasets")

final_<- rbind(sub_plan,sub_enrl,sub_dosed,sub_compl,sub_wdran,sub_wdreas)

sep(" mergeing fin and bign to pct")

final_pct <- merge(final_, bign1, by = "ACTARM") %>%
  mutate(
    pct = if_else(
      Response != "Subjects Enrolled",
      paste0(
        as.character(sub_cnt),
        " (",
        as.character(round((sub_cnt / bign1$bign) * 100, 2)),
        " )"
      ),
      paste0(
        as.character(sub_cnt),
        " (",
        as.character(round((sub_cnt / sub_cnt) * 100, 2)),
        " )"
      )
    )
  )
 sep ("transpose the data")
 final_tran<-final_pct %>%
   pivot_wider(id_cols=c("Response","order"),
             names_from = "ACTARM",
             values_from = "pct")  %>%
   mutate ( ord1 = case_when( Response == "  Adverse Event"  ~ 1,
                              Response == "  Lost to Follow-up"  ~ 3,
                              Response == "  Subject Withdrew Consent" ~5))
 
 sep ("create dummy data for the categories which are not avialable in data") 
 dumy_label<-tribble( 
   ~Response,  ~order ,    ~Overal,    ~Placebo ,  ~TQU ,  ~ord1,
   "  Protocol Violation",6,"0 (0.0)","0 (0.0)","0 (0.0)",2,
   "  Death",6,"0 (0.0)","0 (0.0)","0 (0.0)",4,
   "  Death",6,"0 (0.0)","0 (0.0)","0 (0.0)",6
)
final<-rbind(final_tran,dumy_label) %>%
  mutate(stat = "n (%)") %>%
  arrange(order,ord1) %>%
  select(Response,stat,TQU,Placebo,Overal)
  
  

TQU<-as.vector(bign1$bign[3])
Placebo<-as.vector(bign1$bign[2])
Overal<-as.vector(bign1$bign[1])


sep("create report")
output <- "/Users/ramanireddy/Documents/SAS/work/myR/projects/R Project/Output/"

final%>% 
  rtf_page(orientation = "landscape",
           col_width = 10) %>%
  rtf_title("Table 14.1.21 Subject Disposition by Treatment (Safety Population)",
            text_justification = "c") %>%
  rtf_colheader(paste0("Response| Statistic |Test \n (N=", TQU, ")","| Reference\n (N=", Placebo, ")","| Overal\n (N=", Overal, ")"),
    col_rel_width = c(4,rep(4,4)),
    text_justification = c("l",rep("c",4)),
    border_right = c("","","","",""),
    border_left = c("","","","","")
  )%>%
  rtf_body(col_rel_width =c(4,rep(4,4)),
           text_justification = c("l",rep("c",4)),
           border_right  = c("","","","",""),
           border_left =c("","","","","")
           ) %>%
  rtf_footnote("N: The number of subjects in the safety population; n: The number of subjects in the specific population;\n %: calculated using the number of subjects in the safety population as the denominator (n/N*100)",
               border_bottom = "",
               border_top = "single") %>%
  rtf_encode() %>%
  write_rtf(paste0(output,"table 14.1.21.rtf"))

log_close()


















