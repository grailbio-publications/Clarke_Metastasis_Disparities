#coordination script to generate tables and figures for SEER Disparities
#https://github.com/grailbio-publications/Clarke_Metastasis_Disparities
library(tidyverse)

#edit this script to provide a unique code for a run output
source("scripts/current_date_code.R")

source("scripts/01_read_seer_inputs.R")

input_date_code<-date_code #can be altered to a previous version if skipping seer processing
source("scripts/02_generate_total_cancer.R")
source("scripts/03_generate_helper_labels.R") 
source("scripts/04_execute_stage_shift_scenarios.R")

source("scripts/05_plot_scenarios.R")
source("scripts/06_plot_supplemental_raw_data.R")
source("scripts/07_output_case_table.R")

source("scripts/08_diagram_stop_before_metastasis.R")