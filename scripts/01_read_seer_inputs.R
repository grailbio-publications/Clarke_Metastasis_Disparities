# get SEER data for disparities by race/ethnicity

library(readxl)

seer_excel_path<-"data/20210528_Incidence_CSS_For_Disparities.xlsx"

incidence_df<-read_xlsx(seer_excel_path,
                         sheet="incidence",
                         range="A2:G1141",
                         col_names=c("Race_origin","Sex","Cancer","Stage","Rate","Count","Pop"))


#fix myeloma errors (not staged with standard staging and in this seer draw)
parsed_incidence_data<-incidence_df %>%
  mutate(Rate=case_when(Cancer=="Myeloma" & Stage %in% c("I","II","III","IV") ~ 0.0,
                        TRUE ~ Rate))


raw_css_df<-read_xlsx(seer_excel_path,
                       sheet="CSS",
                       range="A3:G1370",
                       col_names=c("Race_origin","Sex","Cancer","Stage","Time","Count","CSS"))

#post_process to make sure consistent data
#myeloma is not staged in standard ways so any values are errors
#survival is at worst 0.0 if no-one survives to the end
parsed_css_data<-raw_css_df %>%
  filter(Stage!="All Values") %>%
  mutate(CSS=case_when(grepl("ERROR",CSS) ~ NA_character_,
                       TRUE ~ CSS)) %>%
  type_convert() %>% 
  mutate(CSS=case_when(Cancer=="Myeloma" & Stage %in% c("I","II","III","IV") ~ NA_real_,
                       Cancer=="Esophagus" ~ replace_na(CSS,replace=0.0),
                       Cancer=="Urinary Bladder" ~ replace_na(CSS,replace=0.0),
                       Cancer=="Pancreas" ~ replace_na(CSS,replace=0.0),
                       Cancer=="Breast" ~ replace_na(CSS,replace=0.0),
                       TRUE ~ CSS),
         Count=case_when(Cancer=="Myeloma" & Stage %in% c("I","II","III","IV") ~ 0,
                         TRUE ~ Count)) %>%
  rename(N=Count)



all_df<-parsed_incidence_data %>%
  left_join(parsed_css_data)

write_tsv(all_df,sprintf("generated_data/%s_all_data.tsv",date_code))
