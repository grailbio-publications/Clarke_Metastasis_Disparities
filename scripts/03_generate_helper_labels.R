#generate helper labels
#favored order
favored_order<-all_descriptor %>% 
  mutate(deaths=Rate*(1-CSS)) %>% 
  filter(Race_origin!="Other/unknown",Race_origin!="All") %>% 
  group_by(Race_origin,Sex,Cancer) %>% 
  summarize(total=sum(deaths,na.rm=TRUE)) %>% 
  ungroup() %>% 
  group_by(Race_origin,Sex) %>% 
  mutate(Prop=total/sum(total[Cancer!="All Together"],na.rm=TRUE)) %>% 
  ungroup() %>% 
  group_by(Cancer,Sex) %>% 
  summarize(frac=100*mean(Prop,na.rm=TRUE)) %>% 
  ungroup() %>% 
  arrange(Sex,desc(frac)) %>%
  mutate(alias=case_when(Cancer=="Lung and Bronchus" ~ "Lung",
                         Cancer=="Colon and Rectum" ~ "Colon/Rectum",
                         Cancer=="Kidney and Renal Pelvis" ~ "Renal",
                         Cancer=="Oral cavity and pharynx" ~ "Head/Neck",
                         Cancer=="Urinary Bladder" ~ "Bladder",
                         Cancer=="Melanoma of the Skin" ~ "Melanoma",
                         TRUE ~ Cancer),
         ShortSex=case_when(Sex=="Male" ~ "M",
                            TRUE ~ "F"),
         Name=sprintf("%s in %s (%s)",alias,Sex,round(frac)),
         FancyName=sprintf("%s (%2s)",alias,round(frac))) %>%
  mutate(FancyName=case_when(grepl("Together",FancyName) ~ "All Together(100)",
                             TRUE ~ FancyName)) %>%
  mutate(Name=factor(Name,levels=Name))

short_race_origin<-all_descriptor %>%
  select(Race_origin) %>%
  distinct() %>%
  mutate(Origin=case_when(grepl("White",Race_origin)~"NHWhite",
                          grepl("Black",Race_origin)~"NHBlack",
                          grepl("Asian",Race_origin)~"NHA/PI",
                          grepl("Other",Race_origin)~ "OTHER",
                          grepl("Hispanic", Race_origin) ~ "Hispanic",
                          TRUE ~ "All")) 
