#' Targets plan
#' 

## Attach required packages ----

library(targets)
library(tarchetypes)
library(ggplot2)

tar_option_set(
  packages = c("dplyr","tidyr","ggplot2","piecewiseSEM")  # load dplyr in each environement
)

tar_source()

## Load Project R Functions ----

source(here::here("R", "functions.R"))

## Analyses pipeline ----

list(
  
  ## Manage data ----
  
  tar_target(data_resume_visits,get_resume_visits("data_raw/obs_ABPOLL.txt")),

  tar_target(data_id,load_data_id("data_raw/data_ABPOLL_ID_resume.txt", data_resume_visits, cols = "co10")),

  tar_target(data_genotypes,load_data("data_raw/fix10_paternities_ABPOLL.txt")),

  tar_target(data_previous_study,load_data("data_raw/all_data_long_NA_0AllFemFALSE_raw.txt")),

  tar_target(data_from_genotypes,get_data_from_genotypes(data_genotypes, data_id, "data_raw/data_ABPOLL_ID_level_detID.txt", cols="co10")),

  tar_target(data_sem_sampled_sessions,get_data_sem_sampled_sessions(data_id, data_previous_study, data_from_genotypes, cols="co10")),
  
  ## Effect on the measured variables ----
  
  tar_target(sr_all_mal,get_ttt_effect_id(data_sem_sampled_sessions,sex = "mal",variable = "sr_all")),
  
  tar_target(sr_all_fem,get_ttt_effect_id(data_sem_sampled_sessions,sex = "fem",variable = "sr_all")),
  
  tar_target(sr_out_mal,get_ttt_effect_id(data_sem_sampled_sessions,sex = "mal",variable = "sr_out")),
  
  tar_target(sr_out_fem,get_ttt_effect_id(data_sem_sampled_sessions,sex = "fem",variable = "sr_out")),
  
  tar_target(oms_mal,get_ttt_effect_id(data_sem_sampled_sessions,sex = "mal",variable = "oms")),
  
  tar_target(oms_fem,get_ttt_effect_id(data_sem_sampled_sessions,sex = "fem",variable = "oms")),
  
  tar_target(mean_ps_mal,get_ttt_effect_id(data_sem_sampled_sessions, sex = "mal", variable = "mean_ps")),
  
  tar_target(diff_q_fem,get_ttt_effect_id(data_sem_sampled_sessions, sex = "fem", variable = "diff_q")),
  
  tar_target(contid_id,get_ttt_effect_id(data_sem_sampled_sessions, sex = "id", variable = "contact_id")),
  
  tar_target(meanpos_id,get_ttt_effect_id(data_sem_sampled_sessions, sex = "id", variable = "mean_position", text_size = 18)),
  
  tar_target(visflo_id,get_ttt_effect_id(data_sem_sampled_sessions, sex = "id", variable = "nb_visits_per_flower", text_size = 18)),
  
  tar_target(flo_id,get_ttt_effect_id(data_sem_sampled_sessions, sex = "id", variable = "nb_flower_visited", text_size = 18)),
  

  ## Basic SEM models before dsep ----
  
  ## Males 
  
  tar_target(piecewise_males_low_combi1_wtot_basic,get_piecewise_males_visits(data_sem_sampled_sessions, target_ttt = "low", target_sex = "mal",
                                                                         target_sr = "W", target_ps = "PS",
                                                                         target_traits = c("F","H"))),
  
  tar_target(piecewise_males_medium_combi1_wtot_basic,get_piecewise_males_visits(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "mal",
                                                                            target_sr = "W", target_ps = "PS",
                                                                            target_traits = c("F","H"))),
  
  tar_target(piecewise_males_high_combi1_wtot_basic,get_piecewise_males_visits(data_sem_sampled_sessions, target_ttt = "high", target_sex = "mal",
                                                                          target_sr = "W", target_ps = "PS",
                                                                          target_traits = c("F","H"))),
  
  ## Females
  
  tar_target(piecewise_females_low_combi1_wtot_basic,get_piecewise_females_visits(data_sem_sampled_sessions, target_ttt = "low", target_sex = "fem",
                                                                             target_sr = "W", target_ps = "ME",
                                                                             target_traits = c("F","H"))),
  
  tar_target(piecewise_females_medium_combi1_wtot_basic,get_piecewise_females_visits(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "fem",
                                                                                target_sr = "W", target_ps = "ME",
                                                                                target_traits = c("F","H"))),
  
  tar_target(piecewise_females_high_combi1_wtot_basic,get_piecewise_females_visits(data_sem_sampled_sessions, target_ttt = "high", target_sex = "fem",
                                                                              target_sr = "W", target_ps = "ME",
                                                                              target_traits = c("F","H"))),
  
  ## Final SEM models with missing paths inclusion detected with dsep ----
  
  ## Males 
  
  tar_target(piecewise_males_low_combi1_wtot_final,get_piecewise_males_visits_low(data_sem_sampled_sessions, target_sr = "W")),
  
  tar_target(piecewise_males_medium_combi1_wtot_final,get_piecewise_males_visits_medium(data_sem_sampled_sessions, target_sr = "W")),
  
  tar_target(piecewise_males_high_combi1_wtot_final,get_piecewise_males_visits_high(data_sem_sampled_sessions, target_sr = "W")),
             
  ## Females
  
  tar_target(piecewise_females_low_combi1_wtot_final,get_piecewise_females_visits_low(data_sem_sampled_sessions, target_sr = "W")),
  
  tar_target(piecewise_females_medium_combi1_wtot_final,get_piecewise_females_visits_medium(data_sem_sampled_sessions, target_sr = "W")),
  
  tar_target(piecewise_females_high_combi1_wtot_final,get_piecewise_females_visits_high(data_sem_sampled_sessions, target_sr = "W")),
  
  
  ## Z-tests for SEM comparison ----
  
  tar_target(z_tests,get_z_tests(piecewise_males_low_combi1_wtot_final,piecewise_males_medium_combi1_wtot_final,piecewise_males_high_combi1_wtot_final,
                                 piecewise_females_low_combi1_wtot_final,piecewise_females_medium_combi1_wtot_final,piecewise_females_high_combi1_wtot_final)),
  
  ## Check result robustness with Wout ----
  
  ## Males 
  
  tar_target(piecewise_males_low_combi1_wout_final,get_piecewise_males_visits_low(data_sem_sampled_sessions, target_sr = "Wout")),
  
  tar_target(piecewise_males_medium_combi1_wout_final,get_piecewise_males_visits_medium(data_sem_sampled_sessions, target_sr = "Wout")),
  
  tar_target(piecewise_males_high_combi1_wout_final,get_piecewise_males_visits_high(data_sem_sampled_sessions, target_sr = "Wout")),
  
  ## Females
  
  tar_target(piecewise_females_low_combi1_wout_final,get_piecewise_females_visits_low(data_sem_sampled_sessions, target_sr = "Wout")),
  
  tar_target(piecewise_females_medium_combi1_wout_final,get_piecewise_females_visits_medium(data_sem_sampled_sessions, target_sr = "Wout")),
  
  tar_target(piecewise_females_high_combi1_wout_final,get_piecewise_females_visits_high(data_sem_sampled_sessions, target_sr = "Wout")),
  
  ## Quarto ----
  
  tarchetypes::tar_quarto(index, "index.qmd", quiet = FALSE)
  
)
