#generate all-cancer together values

#get from the previously processed data sheet
all_df<-read_tsv(sprintf("generated_data/%s_all_data.tsv",input_date_code))

#now that we're using all cancers together
#generate "All Together" without residual
together_df<-all_df %>%
  group_by(Race_origin,Sex,Stage) %>%
  summarize(Total=sum(Rate,na.rm=TRUE),
            Count=sum(Count,na.rm=TRUE),
            Pop=Pop[1],
            Time=Time[1],
            N=sum(N,na.rm=TRUE),
            Death=sum(Rate*(1-CSS),na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(Ratio=1-Death/Total,
         Cancer="All Together") %>%
  select(Race_origin,Sex,Cancer,Stage,
         Rate=Total,
         Count,Pop,Time,N,CSS=Ratio)

sex_inconsistent_filter<-tribble(~Sex,~Cancer,
                                 "Male","Ovary",
                                 "Male","Uterus",
                                 "Male", "Cervix",
                                 "Female","Prostate") %>%
  mutate(inconsistent=TRUE)

all_descriptor<-all_df %>%
  bind_rows(together_df) %>%
  select(Race_origin,Sex,Cancer,Stage,Rate,CSS) %>%
  arrange(Race_origin,Sex,Cancer,Stage) %>%
  left_join(sex_inconsistent_filter) %>%
  mutate(inconsistent=replace_na(inconsistent,replace=FALSE)) %>%
  filter(inconsistent==FALSE) %>%
  select(-inconsistent)

#remove gender-specific cancers
