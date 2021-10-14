#make figures

summary_formatted_values<-summary_values %>%
  filter(Race_origin!="Other/unknown",Race_origin!="All") %>%
  filter(Cancer!="All Together") %>%
  mutate(Cancer=case_when(Cancer=="All Aggregated" ~ "All Together",
                          TRUE ~ Cancer)) %>%
  left_join(short_race_origin) %>%
  left_join(favored_order)

scenario_label_df<-tribble(~Scenario,~Fancy,
                           "IV->IV","No change",
                           "IV->III","Stage IV to III",
                           "IV->I,II,III", "Stage IV to III/II/I") %>%
  mutate(Fancy=factor(Fancy,levels=Fancy))


meta_frac_plot<-summary_formatted_values %>% 
  filter(Scenario=="CaseZ") %>%
  mutate(Sex=case_when(Sex=="Male" ~ "A. Male",
                       TRUE ~ "B. Female")) %>%
  mutate(Sex=factor(Sex,levels=c("A. Male","B. Female"))) %>%
  mutate(Name=fct_rev(Name)) %>%
  mutate(FancyName=fct_reorder(FancyName,frac,.desc=FALSE)) %>%
  mutate(PctIV=100*FractionIV) %>%
  ggplot(aes(x=FancyName,y=PctIV,group=Origin))+
  geom_point(aes(shape=Origin,color=Origin),size=4,stroke=1.5)+
  coord_flip(ylim=c(0,60))+
  scale_shape_manual(values=c("Hispanic"=21,"NHA/PI"=24,"NHWhite"=3,"NHBlack"=22))+
  theme_bw()+
  theme(
    strip.text= element_text(size=10),
    axis.text = element_text(size=14),
    legend.text = element_text(size=12),
    title=element_text(size=18),
    #panel.grid=element_blank(),
    panel.grid=element_line(linetype="dashed",color="grey50"),
    panel.grid.major.y=element_line(linetype="dotted",color="grey30"),
    panel.grid.minor.x=element_blank(),
    panel.grid.major.x=element_blank(),
    legend.position="bottom")+
  facet_wrap(~Sex,scale="free_y")+
  labs(y="Cancer Diagnosed at Stage IV (%)",x="Cancer Type (percent 5-year deaths)")

ggsave(sprintf("figs/%s_figure_2_SEER_metastatic_fraction.eps",date_code),
       meta_frac_plot,
       width=9,height=9)

ggsave(sprintf("figs/%s_figure_2_SEER_metastatic_fraction.pdf",date_code),
       meta_frac_plot,
       width=9,height=9)


death_all_plot_m<-summary_formatted_values %>% 
  filter(Cancer!="All Together") %>%
  mutate(Scenario=case_when(Scenario=="CaseA" ~ "IV->III",
                            Scenario=="CaseB" ~ "IV->I,II,III",
                            TRUE ~ "IV->IV")) %>%
  mutate(Scenario=factor(Scenario,levels=c("IV->IV","IV->III","IV->I,II,III"))) %>%
  filter(Sex=="Male") %>%
  ggplot(aes(x=Scenario,y=Death,group=Origin))+
  geom_point(aes(shape=Origin,color=Origin),size=2)+
  expand_limits(y=c(0,10))+
  facet_wrap(~Name,scale="free_y")+
  theme_bw()+
  theme(
    strip.text= element_text(size=10),
    axis.text = element_text(size=14),
    axis.text.x=element_text(angle=45,vjust=1.0,hjust=1.0),
    legend.text = element_text(size=12),
    title=element_text(size=18),
    legend.position="top")+
  ggtitle("Deaths Modified by Reduced Hazard")

death_all_plot_f<-summary_formatted_values %>%
  filter(Cancer!="All Together") %>%
  mutate(Scenario=case_when(Scenario=="CaseA" ~ "IV->III",
                            Scenario=="CaseB" ~ "IV->I,II,III",
                            TRUE ~ "IV->IV")) %>%
  mutate(Scenario=factor(Scenario,levels=c("IV->IV","IV->III","IV->I,II,III"))) %>%
  filter(Sex=="Female") %>%
  ggplot(aes(x=Scenario,y=Death,group=Origin))+
  geom_point(aes(shape=Origin,color=Origin),size=2)+
  expand_limits(y=c(0,10))+
  facet_wrap(~Name,scale="free_y")+
  theme_bw()+
  theme(
    strip.text= element_text(size=10),
    axis.text = element_text(size=14),
    axis.text.x=element_text(angle=45,vjust=1.0,hjust=1.0),
    legend.text = element_text(size=12),
    title=element_text(size=18),
    legend.position="top")+
  ggtitle("Deaths Modified by Reduced Hazard")


ggsave(sprintf("figs/%s_supplemental_figure_2A_SEER_death_all_male.eps",date_code),
       death_all_plot_m,
       width=12,height=9)

ggsave(sprintf("figs/%s_supplemental_figure_2A_SEER_death_all_male.pdf",date_code),
       death_all_plot_m,
       width=12,height=9)

ggsave(sprintf("figs/%s_supplemental_figure_2B_SEER_death_all_female.eps",date_code),
       death_all_plot_f,
       width=12,height=9)

ggsave(sprintf("figs/%s_supplemental_figure_2B_SEER_death_all_female.pdf",date_code),
       death_all_plot_f,
       width=12,height=9)


#manually move the labels to avoid overlap
death_one_plot<-summary_formatted_values %>% 
  mutate(Scenario=case_when(Scenario=="CaseA" ~ "IV->III",
                            Scenario=="CaseB" ~ "IV->I,II,III",
                            TRUE ~ "IV->IV")) %>%
  mutate(Scenario=factor(Scenario,levels=c("IV->IV","IV->III","IV->I,II,III"))) %>%
  filter(Cancer=="All Together") %>%
  group_by(Race_origin,Sex) %>%
  mutate(Proportion=100*(max(Death)-Death)/max(Death)) %>%
  ungroup() %>%
  mutate(manual_y=case_when(Sex=="Male" & Origin=="Hispanic" ~ 20.0,
                            Sex=="Male" & Origin=="NHA/PI" ~ -20.0,
                            Sex=="Male" & Origin=="NHWhite" ~ -20.0,
                            Sex=="Male" & Origin=="NHBlack" & Scenario=="IV->IV" ~ -20.0,
                            Sex=="Male" & Origin=="NHBlack" ~ 23.0,
                            Sex=="Female" & Origin=="Hispanic" ~ 20.0,
                            Sex=="Female" & Origin =="NHA/PI" ~ -20.0,
                            Sex=="Female" & Origin == "NHWhite" ~ -20.0,
                            Sex=="Female" & Origin == "NHBlack" ~ 20.0,
                            TRUE ~ 0.0)) %>%
  left_join(scenario_label_df) %>%
  mutate(Sex=case_when(Sex=="Male" ~ "A. Male",
                       TRUE ~ "B. Female")) %>%
  mutate(Sex=factor(Sex,levels=c("A. Male","B. Female"))) %>%
  ggplot(aes(x=Fancy,y=Death,group=Origin))+
  geom_line(aes(color=Origin))+
  geom_point(aes(color=Origin,shape=Origin),size=4,stroke=1.5,fill="white")+
  scale_shape_manual(values=c("Hispanic"=21,"NHA/PI"=24,"NHWhite"=3,"NHBlack"=22))+
  geom_label(aes(label=paste(round(-Proportion),"%",sep=""),color=Origin,y=Death+manual_y),size=4,label.size=NA,show.legend=FALSE)+
  expand_limits(y=c(0,500))+
  theme_bw()+
  theme(
    strip.text= element_text(size=10),
    axis.text = element_text(size=14),
    axis.text.x=element_text(angle=45,vjust=1.0,hjust=1.0),
    panel.grid=element_line(linetype="dashed",color="grey50"),
    panel.grid.major.x=element_line(linetype="dotted",color="grey30"),
    panel.grid.minor.y=element_blank(),
    legend.text = element_text(size=16),
    title=element_text(size=18),
    legend.position="bottom")+
  labs(y="Deaths (N)",x="")+
  facet_wrap(~Sex)

ggsave(sprintf("figs/%s_figure_3_SEER_death_one.eps",date_code),
       death_one_plot,
       width=9,height=9)

ggsave(sprintf("figs/%s_figure_3_SEER_death_one.pdf",date_code),
       death_one_plot,
       width=9,height=9)

