#make basic table for paper/poster
casez_summary<-summary_formatted_values %>%
  mutate(Scenario=case_when(Scenario=="CaseA" ~ "IV->III",
                            Scenario=="CaseB" ~ "IV->I,II,III",
                            TRUE ~ "IV->IV")) %>%
  mutate(Scenario=factor(Scenario,levels=c("IV->IV","IV->III","IV->I,II,III"))) %>%
  filter(Cancer=="All Together") %>%
  filter(Scenario=="IV->IV") %>%
  select(Sex,
         Origin,
         TotalIV,
         Total,
         FractionIV,
         StageIVDeath,
         StageIVDeathRate,
         Death,
         AllDeathRate,
         DeathIV) %>%
  arrange(Sex,Origin)

casea_summary<-summary_formatted_values %>%
  mutate(Scenario=case_when(Scenario=="CaseA" ~ "IV->III",
                            Scenario=="CaseB" ~ "IV->I,II,III",
                            TRUE ~ "IV->IV")) %>%
  mutate(Scenario=factor(Scenario,levels=c("IV->IV","IV->III","IV->I,II,III"))) %>%
  filter(Cancer=="All Together") %>%
  filter(Scenario=="IV->III") %>%
  select(Sex,
         Origin,
         AllIVtoIII_Death=Death) %>%
  arrange(Sex,Origin)

caseb_summary<-summary_formatted_values %>%
  mutate(Scenario=case_when(Scenario=="CaseA" ~ "IV->III",
                            Scenario=="CaseB" ~ "IV->I,II,III",
                            TRUE ~ "IV->IV")) %>%
  mutate(Scenario=factor(Scenario,levels=c("IV->IV","IV->III","IV->I,II,III"))) %>%
  filter(Cancer=="All Together") %>%
  filter(Scenario=="IV->I,II,III") %>%
  select(Sex,
         Origin,
         AllIVtoI_II_III_Death=Death) %>%
  arrange(Sex,Origin)

case_table_sketch<-casez_summary %>%
  left_join(casea_summary) %>%
  mutate(AllIVtoIII_Averted=Death-AllIVtoIII_Death,
         Pct_AllIVtoIII_Averted=100*AllIVtoIII_Averted/Death,
         Pct_AllIVtoIII_IVonly_Averted=100*AllIVtoIII_Averted/StageIVDeath) %>%
  left_join(caseb_summary) %>%
  mutate(AllIVtoI_II_III_Averted=Death-AllIVtoI_II_III_Death,
         Pct_AllIVtoI_II_III_Averted=100*AllIVtoI_II_III_Averted/Death,
         Pct_AllIVtoI_II_III_IVonly_Averted=100*AllIVtoI_II_III_Averted/StageIVDeath) %>%
  mutate(Sex=factor(Sex,levels=c("Male","Female")),
         Origin=factor(Origin,levels=c("NHBlack","NHWhite","Hispanic","NHA/PI"))) %>%
  arrange(Sex,Origin) %>%
  mutate_if(is.numeric,round,digits=2)

#data for table generation
write_tsv(case_table_sketch,sprintf("reports/%s_case_table.tsv",date_code))

