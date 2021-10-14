#implement stage-shift scenarios

just_survival<-
  all_descriptor %>%
  mutate(number_stage=match(Stage,c("I","II","III","IV","Unknown/missing"))) %>%
  select(Race_origin,Sex,Cancer,prequel=number_stage,CSS)

#pattern my scenarios
just_scenario<-
  all_descriptor %>%
  select(Stage) %>%
  distinct() %>%
  mutate(number_stage=match(Stage,c("I","II","III","IV","Unknown/missing")),
         tmp=case_when(number_stage==4 ~ 4,
                       TRUE ~ 1)) %>%
  uncount(tmp,.id="prequel") %>%
  mutate(CaseA=case_when(Stage=="IV" & prequel==3 ~ 1.0,
                         Stage!="IV" ~ 1.0,
                         TRUE ~ 0.0),
         CaseB=case_when(Stage=="IV" & prequel<4 ~ 1.0/3.0,
                         Stage!="IV"~ 1.0,
                         TRUE ~ 0.0),
         CaseZ=case_when(Stage=="IV" & prequel==4 ~ 1.0,
                         Stage!="IV"~ 1.0,
                         TRUE ~ 0.0)) %>%
  mutate(prequel=case_when(Stage=="IV" ~ prequel,
                           TRUE ~ number_stage)) %>%
  pivot_longer(starts_with("Case"),names_to="Scenario",values_to="Intercept") 


#join to data, make sure survival goes by stage at interception
case_select<-all_descriptor %>%
  left_join(just_scenario) %>%
  select(Race_origin,Sex,Cancer,Stage,Rate,number_stage,prequel,Scenario,Intercept) %>%
  left_join(just_survival) %>%
  mutate(Survival=Intercept*CSS*Rate)

#reaggregate all individual sites
case_results<-case_select %>%
  filter(Cancer!="All Together") %>%
  mutate(Cancer="All Aggregated") %>%
  group_by(Race_origin,Sex,Cancer,Stage,number_stage,prequel,Scenario) %>%
  summarize(Rate=sum(Rate,na.rm=TRUE),
            Survival=sum(Survival,na.rm=TRUE)) %>%
  ungroup() %>%
  bind_rows(case_select %>%
              select(Race_origin,Sex,Cancer,Stage,number_stage,prequel,Scenario,Rate,Survival))

final_values<-case_results %>%
  group_by(Race_origin,Sex,Cancer,Stage,Scenario) %>%
  summarize(Rate=Rate[1],
            XX=var(Rate),
            Survival=sum(Survival,na.rm=TRUE)) %>%
  mutate(Death=Rate-Survival) %>%
  ungroup()

summary_values<-final_values %>%
  group_by(Race_origin,Sex,Cancer,Scenario) %>%
  summarize(
    TotalIV=sum(Rate[Stage=="IV"]),
    Total=sum(Rate),
    FractionIV=Rate[Stage=="IV"]/Total,
    StageIVDeath=sum(Death[Stage=="IV"]),
    StageIVDeathRate=sum(Death[Stage=="IV"])/TotalIV,
    DeathIV=Death[Stage=="IV"]/sum(Death,na.rm=TRUE),
    Death=sum(Death,na.rm=TRUE),
    AllDeathRate=Death/Total) %>%
  ungroup() %>%
  arrange(Race_origin,Sex,Cancer,Scenario,Death)
