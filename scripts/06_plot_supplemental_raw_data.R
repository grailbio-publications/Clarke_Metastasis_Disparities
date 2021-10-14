library(patchwork)


raw_formatted_descriptor<-all_descriptor %>%
  filter(Race_origin!="Other/unknown",Race_origin!="All") %>%
  filter(Cancer!="All Together") %>%
  mutate(Stage=case_when(Stage=="Unknown/missing" ~ "U",
                         TRUE ~ Stage)) %>%
  left_join(short_race_origin) %>%
  left_join(favored_order) 


incidence_plot_m<-raw_formatted_descriptor %>%
  filter(Sex=="Male") %>%
  ggplot(aes(x=Stage,y=Rate,group=Origin))+
  geom_line(aes(color=Origin))+
  geom_point(aes(shape=Origin,color=Origin))+
  expand_limits(y=c(0,20))+
  facet_wrap(~Name,scale="free_y")+
  theme_bw()+
  #  theme(legend.position=c(0.8,0.1))+
  theme(
    strip.text= element_text(size=10),
    axis.text = element_text(size=14),
    legend.text = element_text(size=16),
    title=element_text(size=18))+
  ggtitle("By Site: Incidence Sex (Mean Percent of Deaths)")

incidence_plot_f<-raw_formatted_descriptor %>%
  filter(Sex=="Female") %>%
  ggplot(aes(x=Stage,y=Rate,group=Origin))+
  geom_line(aes(color=Origin))+
  geom_point(aes(shape=Origin,color=Origin))+
  expand_limits(y=c(0,20))+
  facet_wrap(~Name,scale="free_y")+
  theme_bw()+
  #  theme(legend.position=c(0.8,0.1))+
  theme(
    strip.text= element_text(size=10),
    axis.text = element_text(size=14),
    legend.text = element_text(size=16),
    title=element_text(size=18))+
  ggtitle("By Site: Incidence Sex (Mean Percent of Deaths)")


css_plot_m<-raw_formatted_descriptor %>%
  filter(Sex=="Male") %>%
  ggplot(aes(x=Stage,y=CSS,group=Origin))+
  geom_line(aes(color=Origin))+
  geom_point(aes(shape=Origin,color=Origin))+
  expand_limits(y=c(0,1))+
  facet_wrap(~Name,scale="free_y")+
  theme_bw()+
  #  theme(legend.position=c(0.8,0.1))+
  theme(
    strip.text= element_text(size=10),
    axis.text = element_text(size=14),
    legend.text = element_text(size=16),
    title=element_text(size=18))+
  ggtitle("Cause Specific Survival")

css_plot_f<-raw_formatted_descriptor %>%
  filter(Sex=="Female") %>%
  ggplot(aes(x=Stage,y=CSS,group=Origin))+
  geom_line(aes(color=Origin))+
  geom_point(aes(shape=Origin,color=Origin))+
  expand_limits(y=c(0,1))+
  facet_wrap(~Name,scale="free_y")+
  theme_bw()+
  #  theme(legend.position=c(0.8,0.1))+
  theme(
    strip.text= element_text(size=10),
    axis.text = element_text(size=14),
    legend.text = element_text(size=16),
    title=element_text(size=18))+
  ggtitle("Cause Specific Survival")



#one more
cumulative_deaths_m<-raw_formatted_descriptor %>%
  mutate(Death=Rate*(1-CSS)) %>%
  group_by(Origin,Sex,Cancer) %>%
  mutate(CDeath=cumsum(Death)) %>%
  ungroup() %>%
  filter(Sex=="Male") %>%
  ggplot(aes(x=Stage,y=CDeath,group=Origin))+
  geom_line(aes(color=Origin))+
  geom_point(aes(shape=Origin,color=Origin))+
  expand_limits(y=c(0,10))+
  facet_wrap(~Name,scale="free_y")+
  theme_bw()+
  #  theme(legend.position=c(0.8,0.1))+
  theme(
    strip.text= element_text(size=10),
    axis.text = element_text(size=14),
    legend.text = element_text(size=16),
    title=element_text(size=18),
    legend.position='bottom')+
  ggtitle("Cumulative Deaths")

cumulative_deaths_f<-raw_formatted_descriptor %>%
  mutate(Death=Rate*(1-CSS)) %>%
  group_by(Origin,Sex,Cancer) %>%
  mutate(CDeath=cumsum(Death)) %>%
  ungroup() %>%
  filter(Sex=="Female") %>%
  ggplot(aes(x=Stage,y=CDeath,group=Origin))+
  geom_line(aes(color=Origin))+
  geom_point(aes(shape=Origin,color=Origin))+
  expand_limits(y=c(0,10))+
  facet_wrap(~Name,scale="free_y")+
  theme_bw()+
  #  theme(legend.position=c(0.8,0.1))+
  theme(
    strip.text= element_text(size=10),
    axis.text = element_text(size=14),
    legend.text = element_text(size=16),
    title=element_text(size=18),
    legend.position='bottom')+
  ggtitle("Cumulative Deaths")


ggsave(sprintf("figs/%s_supplemental_figure_1A_SEER_summary_race_male.eps",date_code),
       incidence_plot_m+css_plot_m+plot_layout(guides='collect') & theme(legend.position='bottom'),
       width=22,height=9)

ggsave(sprintf("figs/%s_supplemental_figure_1A_SEER_summary_race_male.pdf",date_code),
       incidence_plot_m+css_plot_m+plot_layout(guides='collect') & theme(legend.position='bottom'),
       width=22,height=9)


ggsave(sprintf("figs/%s_supplemental_figure_1B_SEER_summary_race_female.eps",date_code),
       incidence_plot_f+css_plot_f+plot_layout(guides='collect') & theme(legend.position='bottom'),
       width=22,height=9)


ggsave(sprintf("figs/%s_supplemental_figure_1B_SEER_summary_race_female.pdf",date_code),
       incidence_plot_f+css_plot_f+plot_layout(guides='collect') & theme(legend.position='bottom'),
       width=22,height=9)

ggsave(sprintf("figs/%s_SEER_cumulative_deaths_male.eps",date_code),
       cumulative_deaths_m,
       width=12,height=9)

ggsave(sprintf("figs/%s_SEER_cumulative_deaths_male.pdf",date_code),
       cumulative_deaths_m,
       width=12,height=9)

ggsave(sprintf("figs/%s_SEER_cumulative_deaths_female.eps",date_code),
       cumulative_deaths_f,
       width=12,height=9)

ggsave(sprintf("figs/%s_SEER_cumulative_deaths_female.pdf",date_code),
       cumulative_deaths_f,
       width=12,height=9)

