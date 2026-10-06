#' Get data that summarize pollinator visits
#'
#' @description 
#'
#' @param data 
#'
#' @return 
#' 
#' @import dplyr 
#' 
#' @export

get_resume_visits <- function(file_path) {
  
  data_arrival_with_bee <- read.table(file_path,head=T) |>
    rename(id=ID_full) |>
    arrange(session,time) |> # to calculate ID age (visits needed to be sequentially arrange, independently of the pollinators)
    pivot_wider(values_from=id,names_from=id,names_prefix="newid_",values_fn = list(id = ~ 1), values_fill = list(id = 0)) |>
    mutate(across(starts_with("newid"),~ifelse(duplicated(cumsum(.)),NA,cumsum(.)))) |>
    rowwise() |>
    mutate(id_age=sum(across(starts_with("newid")),na.rm=T)-1) |>
    select(!starts_with("newid")) |>
    mutate(id=paste0(session,".",ifelse(ID==10,10,paste0("0",ID)))) |>
    group_by(session,people) |>
    mutate(no_visit=1:n()) |>
    group_by(session,people) |>
    mutate(consecut=consecutive_id(id)) |>
    group_by(session,people,consecut) |>
    summarise(start=min(time),
              name_id=unique(id),
              length=n(),
              duration=sum(duration,na.rm=T)) |> 
    mutate(id=name_id) |>
    pivot_wider(values_from=name_id,names_from=name_id,names_prefix="newid_",values_fn = list(name_id = ~ 1), values_fill = list(name_id = 0)) |>
    mutate(across(starts_with("newid"),~ifelse(duplicated(cumsum(.)),NA,cumsum(.)))) |>
    rowwise() |>
    mutate(no_arrival=sum(across(starts_with("newid")),na.rm=T)) |>
    select(!starts_with("newid")) |>
    select(session,start,id,people,consecut,duration,no_arrival) |>
    arrange(session,start)
  
  data_position <- data_arrival_with_bee |>
    group_by(session, people, id) |>
    summarise(mean_position_focal_bee = mean(consecut)) |>
    left_join(data_arrival_with_bee |>
                group_by(session, people) |>
                summarise(max_boot = max(consecut))) |>
    mutate(rel_mean_position_focal_bee = mean_position_focal_bee / max_boot) |>
    group_by(session,id) |>
    summarise(mean_position = mean(rel_mean_position_focal_bee))
  
  data_first_visit <- data_arrival_with_bee |>
    group_by(session, people, id) |>
    summarise(first_visit_focal_bee = min(consecut)) |>
    # left_join(data_arrival_with_bee |>
    #             group_by(session, people) |>
    #             summarise(max_boot = max(consecut))) |>
    # mutate(rel_mean_first_visit = first_visit_focal_bee / max_boot) |>
    # group_by(session,id) |>
    # summarise(mean_first_visit = mean(rel_mean_first_visit))
    group_by(session,id) |>
    summarise(mean_first_visit = mean(first_visit_focal_bee))
  
  data_nb_visits <- read.table(file_path,head=T) |>
    rename(id=ID_full) |> 
    group_by(id) |>
    summarise(nb_visits = n(),
              nb_flower_visited = n_distinct(id_flow_full)) |>
    mutate(nb_visits_per_flower = nb_visits / nb_flower_visited)
  
  data_resume_visits <- data_arrival_with_bee |>
    group_by(session,id,people) |>
    summarize(contact_id_bee=max(no_arrival),
              dur_tot_bee=sum(duration,na.rm=T)) |>
    group_by(session,id) |>
    summarize(contact_id=sum(contact_id_bee),
              dur_tot=sum(dur_tot_bee,na.rm=T)) |>
    left_join(data_position) |>
    left_join(data_first_visit) |>
    left_join(data_nb_visits) |> 
    mutate(dur_per_visit = dur_tot / nb_visits) |>
    mutate(ttt=as.factor(case_when(grepl("FA",session)~"low",
                                   grepl("MO",session)~"medium",
                                   TRUE~"high"))) 
  
  return(data_resume_visits)
}


#' Read data about plant ids
#'
#' @description 
#' This function reads the data about plant ids and format the table.
#'
#' @param file a character of length 1. The path to the .txt file.
#'
#' @return A `table` containing data. 
#' 
#' @import dplyr
#' 
#' @export

load_data_id <- function(file_path, data_resume_visits, cols = c("co10")){
  # data_id <- read.table(file_path,head=T) |>
  #   mutate(poll_treat_factor=as.factor(case_when(poll_treat==1~"low",
  #                                                poll_treat==2~"medium",
  #                                                TRUE~"high"))) |>
  #   mutate(poll_treat_factor=forcats::fct_relevel(poll_treat_factor, c("low","medium","high"))) |>
  #   mutate(prop_self=sr_self/(sr_self+sr_fem_out+sr_mal_out),
  #          gam_ov_proxy=nb_flo*nbOv_mean,
  #          sr_out=sr_fem_out+sr_mal_out) |>
  #   mutate(mean_nb_visit_per_flower=nb_visit/nb_dist_vis)
  
  data_id <- read.table(file_path,head=T) |>
    rename(id = ID_full) |>
    select(session, id, nbGr_SR_sum,
           nb_flo, nb_flo_open, nb_flo_all, height_max, height_mean, nb_stem, nb_poll_focal, pl_mean,
           SR_fem_out, SR_self, SR_fem_out_share,
           !!!syms(paste0("import_nb_part_ID_all_", cols)),
           !!!syms(paste0("export_nb_part_ID_all_", cols)),
           !!!syms(paste0("import_nb_part_ID_out_", cols)),
           !!!syms(paste0("export_nb_part_ID_out_", cols)),
           !!!syms(paste0("import_nb_part_flo_out_", cols)),
           !!!syms(paste0("export_nb_part_flo_out_", cols))) |>
    rename(sr_fem_out = SR_fem_out,
           sr_fem_out_share = SR_fem_out_share,
           sr_self = SR_self)
  
  # for(c in cols){
  #   data_id <- data_id |>
  #     mutate(!!sym(paste0("mean_flower_per_mate_fem_", c)) := !!sym(paste0("import_nb_part_flo_out_", c)) / !!sym(paste0("import_nb_part_ID_out_", c)),
  #            !!sym(paste0("mean_flower_per_mate_mal_", c)) := !!sym(paste0("export_nb_part_flo_out_", c)) / !!sym(paste0("export_nb_part_ID_out_", c)))
  # }
  
  for (col in cols) {
    for (type in c("import", "export")) {
      all_col <- sym(paste0(type, "_nb_part_ID_all_", col))
      out_col <- sym(paste0(type, "_nb_part_ID_out_", col))
      
      data_id <- data_id |>
        mutate(!!out_col := if_else(!!all_col == 0, NA_integer_, !!out_col))
    }
  }

    data_id <- data_id |>
    rename(sr_fem_total=nbGr_SR_sum) |>
    rename_with(~ gsub("import_nb_part_ID_out_", "oms_fem_", .x), starts_with("import_")) |>
    rename_with(~ gsub("export_nb_part_ID_out_", "oms_mal_", .x), starts_with("export_")) |>
      mutate(ttt=as.factor(case_when(grepl("FA",session)~"low",
                                     grepl("MO",session)~"medium",
                                     TRUE~"high")),.before = 2) |>
      left_join(data_resume_visits) |>
      # replace NA by 0 only for contact_id 
      # for nb_flower_visited, nb_visits, duration and mean position, not any sense if not visited
      mutate(contact_id = replace_na(contact_id, 0)) |>
      # mutate(nb_flower_visited = replace_na(nb_flower_visited, 0)) |>
      group_by(session) |>
      mutate(across(where(is.numeric), ~ . / mean(., na.rm = TRUE), .names = "r_{.col}")) |>
      ungroup()

      
  
  return(data_id)
}


#' Generic function to just read dataset in .txt form
#'
#' @description 
#' This function allow to directly load the .txt dataset previously obtained.
#' Note that the sampling of genotypes, the script is available on dryad. We here 
#' directly provide our sampling dataset in a purpose of reproducible results.
#'
#' @param file a character of length 1. The path to the .txt file.
#'
#' @return A `table` containing data. 
#' 
#' @import dplyr
#' 
#' @export

load_data <- function(file_path){
  
  data <- read.table(file_path,head=T) 
  
  return(data)
}

#' #' Get data of paternity share for sem analyses
#' #'
#' #' @description Function to get paternity share data
#' #'
#' #' @param data table with genotypes
#' #'
#' #' @return table paternity share data
#' #'
#' #' @import dplyr
#' #'
#' #' @export
#' 
#' get_data_parent_share <- function(data_genotypes, data_id, file_path){
#'   
#'   # observation with observed visits only (carry-over 10)
#'   all_obs <- read.table(file_path,head=T) %>%
#'     filter(!session %in% c("5.FA1","5.MO1"))  %>%
#'     select(session,ID_full,ID_full_part,import_nb_visit_co10) %>%
#'     filter(!(is.na(import_nb_visit_co10)|import_nb_visit_co10==0)) %>%
#'     mutate(couple_obs = paste0(ID_full,"_",ID_full_part))
#'   
#'   # filtering only observed contact
#'   data_genotypes_filter <- data_genotypes |>
#'     mutate(couple_gen = paste0(known_id,"_",candidate_id)) |>
#'     filter(couple_gen %in% all_obs$couple_obs)
#'   
#'   # data_genotypes_filter <- data_genotypes
#'   
#'   data_parent_share <- data_genotypes_filter |>
#'     filter(candidate_id != known_id) |>
#'     group_by(candidate_id,known_id) |>
#'     summarise(genot_couple = n()) |>
#'     left_join(data_genotypes |>
#'                 group_by(known_id) |>
#'                 summarise(genot_mother = n())) |>
#'     left_join(data_id |>
#'                  select(id, sr_fem_total), by = join_by(known_id == id)) |>
#'     mutate(paternity_share = genot_couple / genot_mother,
#'            total_couple = paternity_share * sr_fem_total) |>
#'     group_by(candidate_id) |>
#'     summarise(mean_ps = mean(paternity_share),
#'               sr_mal_out_share = sum(total_couple)) |>
#'     rename(id = candidate_id) |>
#'     left_join(data_genotypes_filter |>
#'                 group_by(candidate_id,known_id) |>
#'                 summarise(genot_couple = n()) |>
#'                 left_join(data_genotypes |>
#'                             group_by(known_id) |>
#'                             summarise(genot_mother = n())) |>
#'                 left_join(data_id |>
#'                             select(id, sr_fem_total), by = join_by(known_id == id)) |>
#'                 mutate(paternity_share = genot_couple / genot_mother,
#'                        total_couple = paternity_share * sr_fem_total) |>
#'                 group_by(candidate_id) |>
#'                 summarise(mean_ps_all = mean(paternity_share),
#'                           sr_mal_all_share = sum(total_couple)) |>
#'                 rename(id = candidate_id)) |>
#'     mutate(sex = "mal",.before = 2) 
#'   
#'   return(data_parent_share)
#' }

#' Estimate q index
#'
#' @description 
#'
#' @param data 
#'
#' @return 
#' 
#' @import dplyr
#' 
#' @export

q_index <- function(x) {
  n <- length(x)
  N <- sum(x)
  if (N <= 1) return(NA)  # to avoid dividing by zero
  sum_xi <- sum(x * (x - 1))
  I_d <- (sum_xi) / (N * (N - 1))
  return(I_d)
}

#' Get data of paternity share for sem analyses
#'
#' @description Function to get paternity share data
#'
#' @param data table with genotypes
#'
#' @return table paternity share data
#'
#' @import dplyr
#'
#' @export

get_data_from_genotypes <- function(data_genotypes, data_id, file_path, cols="co10"){
  
  # observation with observed visits only (carry-over 10)
  obs_only <- read.table(file_path,head=T) %>%
    filter(!session %in% c("5.FA1","5.MO1"))  %>%
    select(session,ID_full_foc,ID_full_part,!!sym(paste0("import_nb_visit_",cols))) %>%
    filter(!(is.na(!!sym(paste0("import_nb_visit_",cols)))|!!sym(paste0("import_nb_visit_",cols))==0)) %>%
    mutate(couple_obs = paste0(ID_full_foc,"_",ID_full_part))
  
  # filtering only observed contact
  data_genotypes_filter <- data_genotypes |>
    mutate(couple_gen = paste0(known_id,"_",candidate_id)) |>
    filter(couple_gen %in% obs_only$couple_obs)
  
  # data_genotypes_filter <- data_genotypes
  
  data_male <- data_genotypes_filter |>
    filter(candidate_id != known_id) |>
    group_by(candidate_id,known_id) |>
    summarise(genot_couple = n()) |>
    left_join(data_genotypes |>
                group_by(known_id) |>
                summarise(genot_mother = n())) |>
    left_join(data_id |>
                select(id, sr_fem_total), by = join_by(known_id == id)) |>
    mutate(paternity_share = genot_couple / genot_mother,
           total_couple = paternity_share * sr_fem_total) |>
    group_by(candidate_id) |>
    summarise(mean_ps = mean(paternity_share),
              sr_out = sum(total_couple)) |>
    rename(id = candidate_id) |>
    left_join(data_genotypes_filter |>
                group_by(candidate_id,known_id) |>
                summarise(genot_couple = n()) |>
                left_join(data_genotypes |>
                            group_by(known_id) |>
                            summarise(genot_mother = n())) |>
                left_join(data_id |>
                            select(id, sr_fem_total), by = join_by(known_id == id)) |>
                mutate(paternity_share = genot_couple / genot_mother,
                       total_couple = paternity_share * sr_fem_total) |>
                group_by(candidate_id) |>
                summarise(mean_ps_all = mean(paternity_share),
                          sr_all = sum(total_couple)) |>
                rename(id = candidate_id)) |>
    mutate(sex = "mal",.before = 2,
           sr_self = sr_all - sr_out) 
  
  q_obs_female <- obs_only |>
    group_by(ID_full_foc) %>%
    summarise(
      q_obs_all = q_index(!!sym(paste0("import_nb_visit_",cols))),
      .groups = "drop"
    ) |> left_join(obs_only |>
                     filter(ID_full_foc != ID_full_part) |>
                     group_by(ID_full_foc) %>%
                     summarise(
                       q_obs_out = q_index(!!sym(paste0("import_nb_visit_",cols))),
                       .groups = "drop"
                     )) |>
    rename(known_id = ID_full_foc)
  
  q_gen_female <- data_genotypes_filter |>
    group_by(known_id,candidate_id) |>
    summarise(genot_couple = n()) |>
    group_by(known_id) |>
    summarise(
      q_gen_all = q_index(genot_couple),
      .groups = "drop"
    ) |> left_join(data_genotypes_filter |>
                     filter(known_id != candidate_id) |>
                     group_by(known_id,candidate_id) |>
                     summarise(genot_couple = n()) |>
                     group_by(known_id) |>
                     summarise(
                       q_gen_out = q_index(genot_couple),
                       .groups = "drop"
                     ))
  
  data_female <- data_id |>
    select(id, sr_fem_total) |>
    rename(known_id = id) |>
    left_join(q_obs_female) |>
    left_join(q_gen_female) |>
    left_join(data_genotypes_filter |>
                filter(known_id != candidate_id) |>
                group_by(known_id) |>
                summarise(sr_fem_out = n()) |>
                left_join(data_genotypes_filter |>
                            group_by(known_id) |>
                            summarise(n_genot = n())) |>
                mutate(prop_out = sr_fem_out / n_genot)) |>
    mutate(sr_out = sr_fem_total * prop_out) |>
    mutate(ratio_q = q_gen_out / q_obs_out,
           diff_q = q_gen_out - q_obs_out,
           abs_diff_q = abs(diff_q)) |>
    select(-c(sr_fem_out, n_genot, prop_out, q_gen_out, q_gen_all, q_obs_out, q_obs_all)) |>
    rename(id = known_id,
           sr_all = sr_fem_total) |>
    mutate(sex = "fem",.before = 2,
           sr_self = sr_all - sr_out) 
  
  data_from_genotypes <- data_male |>
    bind_rows(data_female) |>
    mutate(session = substr(id, 1, 5))
  
  return(data_from_genotypes)
}

#' #' Get data for sem analyses - all sessions with sampled genotypes OLD VERSION
#' #'
#' #' @description Function to gett data for sem analyses
#' #'
#' #' @param data table with true rs ms and data on id
#' #'
#' #' @return table data for sem analyses
#' #' 
#' #' @import dplyr
#' #' 
#' #' @export
#' 
#' get_data_sem_sampled_sessions_old <- function(data_id_sampled_sessions, data_id, data_proxy, data_parent_share, data_q_by_female, cols="co10") {
#'   
#'   data_sem <- data_id_sampled_sessions |>
#'     select(session, ID_full, type, SR_out, SR_all, r_SR_out, r_SR_all, poll_treat_factor,
#'            !!sym(paste0("r_mean_nb_dist_flo_out_", cols))) |>
#'     rename(id = ID_full,
#'            sex = type,
#'            sr = SR_out,
#'            sr_all = SR_all,
#'            r_sr = r_SR_out,
#'            r_sr_all = r_SR_all,
#'            ttt = poll_treat_factor) |>
#'     left_join(data_parent_share) |>
#'     group_by(session,sex) |>
#'     mutate(r_mean_ps = mean_ps / mean(mean_ps,na.rm = T),
#'            r_mean_ps_all = mean_ps_all / mean(mean_ps_all,na.rm = T)) |>
#'     ungroup() |>
#'         left_join(data_id %>% select(id, contact_id, dur_tot, mean_position, mean_first_visit, nb_visits, nb_flower_visited, dur_per_visit, nb_visits_per_flower, r_contact_id, r_dur_tot, r_mean_position, r_mean_first_visit, r_nb_visits, r_nb_flower_visited, r_dur_per_visit, r_nb_visits_per_flower)) |>
#'     left_join(
#'       data_proxy |>
#'         select(id, session,
#'                nb_flo,nb_flo_open,nb_flo_all,height_max,height_mean,nb_stem,
#'                !!sym(paste0("oms_fem_", cols)),
#'                !!sym(paste0("oms_mal_", cols))) |>
#'         group_by(session) |>
#'         mutate(across(where(is.numeric), ~ . / mean(., na.rm = TRUE), .names = "r_{.col}")) |>
#'         ungroup() |>
#'         pivot_longer(
#'           cols = starts_with(c("r_oms","oms")),
#'           names_to = c(".value", "sex"),
#'           names_pattern = "(r_oms|oms)_(fem|mal)_co10"
#'         ) 
#'     ) |>
#'     left_join(data_q_by_female |>
#'                 mutate(sex = "fem") |>
#'                 rename(id = ID_full_foc) |>
#'                 mutate(ratio_q = q_gen / q_obs,
#'                        diff_q = q_gen - q_obs,
#'                        abs_diff_q = abs(diff_q)) |>
#'                 group_by(session) |>
#'                 mutate(r_ratio_q = ratio_q / mean(ratio_q,na.rm = T),
#'                        r_diff_q = diff_q / mean(diff_q,na.rm = T),
#'                        r_abs_diff_q = abs_diff_q / mean(abs_diff_q,na.rm = T)) |>
#'                 ungroup() |>
#'                 select(session, id, sex, q_obs, q_gen, nb_genot, r_ratio_q, diff_q, r_diff_q, r_abs_diff_q)) |>
#'     mutate(ttt_plot=as.factor(case_when(grepl("FA",session)~"Low",
#'                                  grepl("MO",session)~"Medium",
#'                                  TRUE~"High")),
#'     ttt_plot=forcats::fct_relevel(ttt_plot, c("Low","Medium","High")),
#'     .before = 2) |>
#'     rename(Wout = r_sr,
#'            W = r_sr_all,
#'            PS = r_mean_ps,
#'            FL = r_mean_nb_dist_flo_out_co10,
#'            ME = r_diff_q,
#'            MS = r_oms,
#'            F = r_nb_flo_open,
#'            H = r_height_mean,
#'            POS = r_mean_position,
#'            PLA = r_contact_id,
#'            DUR = r_dur_per_visit,
#'            VIS = r_nb_visits_per_flower,
#'            FLO = r_nb_flower_visited)
#'   
#'   return(data_sem)
#' }

#' Get data for sem analyses - all sessions with sampled genotypes OLD VERSION
#'
#' @description Function to gett data for sem analyses
#'
#' @param data table with true rs ms and data on id
#'
#' @return table data for sem analyses
#' 
#' @import dplyr
#' 
#' @export

get_data_sem_sampled_sessions <- function(data_id, data_previous_study, data_from_genotypes, cols="co10") {
  
  data_sem <- data_id |>
    select(id, ttt, contact_id, dur_tot, mean_position, mean_first_visit, 
           nb_visits, nb_flower_visited, dur_per_visit, 
           nb_visits_per_flower, r_contact_id, r_dur_tot, r_mean_position, 
           r_mean_first_visit, r_nb_visits, r_nb_flower_visited, r_dur_per_visit, r_nb_visits_per_flower) |>
    left_join(
      data_id |>
        select(id, session,
               nb_flo,nb_flo_open,nb_flo_all,height_max,height_mean,nb_stem,
               !!sym(paste0("oms_fem_", cols)),
               !!sym(paste0("oms_mal_", cols))) |>
        group_by(session) |>
        mutate(across(where(is.numeric), ~ . / mean(., na.rm = TRUE), .names = "r_{.col}")) |>
        ungroup()) |>
    pivot_longer(
      cols = starts_with(c("r_oms","oms")),
      names_to = c(".value", "sex"),
      names_pattern = paste0("(r_oms|oms)_(fem|mal)_",cols)
    ) |>
    relocate(session, id, sex) |>
    left_join(data_previous_study |> # just to get the nb of flower touched per mate from a previous study
                select(ID_full, type, !!sym(paste0("r_mean_nb_dist_flo_out_", cols))) |>
                rename(id = ID_full,
                       sex = type)) |>
    left_join(data_from_genotypes |>
      group_by(session,sex) |>
      mutate(r_mean_ps = mean_ps / mean(mean_ps,na.rm = T),
             r_mean_ps_all = mean_ps_all / mean(mean_ps_all,na.rm = T),
             r_diff_q = diff_q / mean(diff_q, na.rm = T),
             r_sr_out = sr_out / mean(sr_out, na.rm=T),
             r_sr_all = sr_all / mean(sr_all, na.rm=T),
             r_sr_self = sr_self / mean(sr_self, na.rm =T)) |>
      ungroup()) |>
    mutate(ttt_plot=as.factor(case_when(grepl("FA",session)~"Low",
                                        grepl("MO",session)~"Medium",
                                        TRUE~"High")),
           ttt_plot=forcats::fct_relevel(ttt_plot, c("Low","Medium","High")),
           .before = 2) |>
    rename(Wout = r_sr_out,
           Wself = r_sr_self,
           W = r_sr_all,
           PS = r_mean_ps,
           FL = !!sym(paste0("r_mean_nb_dist_flo_out_", cols)),
           ME = r_diff_q,
           MS = r_oms,
           F = r_nb_flo_open,
           H = r_height_mean,
           POS = r_mean_position,
           POSf = r_mean_first_visit,
           PLA = r_contact_id,
           DUR = r_dur_per_visit,
           VIS = r_nb_visits_per_flower,
           FLO = r_nb_flower_visited)
  
  return(data_sem)
}

#' Get ttt effect on variables at the id level
#'
#' @description 
#'
#' @param file 
#'
#' @return Results
#' 
#' @import dplyr
#' 
#' @export

get_ttt_effect_id <- function(data_sem_sampled_sessions, sex, variable, text_size = 18){
  
  clean_name <- case_when(variable == "sr_all" ~ "Total reproductive success",
                          variable == "sr_out" ~ "Outcrossed reproductive success",
                          variable == "oms" ~ "Observational number of mates", # Poisson
                          variable == "mean_ps" ~ "Paternity share",
                          variable == "diff_q" ~ "Paternity bias",
                          variable == "mean_position" ~ "Relative mean position of the plant\nin the visit sequence",
                          variable == "contact_id" ~ "Number of pollinator visits to the plant", # Poisson
                          variable == "dur_per_visit" ~ "Average visit duration",
                          variable == "nb_visits_per_flower" ~ "Average number of pollinator visits\nper visited flower",
                          variable == "nb_flower_visited" ~ "Number of distinct flowers visited\non the plant", # Poisson
                          )
  
  if(sex == "mal"){
    data <- data_sem_sampled_sessions |>
      filter(sex == "mal")
  }else if (sex == "fem"){
    data <- data_sem_sampled_sessions |>
      filter(sex == "fem")
  }else{
    data <- data_sem_sampled_sessions
  }
  
  if(variable %in% c("sr_out","sr_all")){
    
    # gaussian
    model <- lme4::lmer(data = data, get(variable) ~ ttt + (1|session))
    lrt <- anova(model, update(model, . ~ . - ttt))
    summary <- summary(model)
    tukey <- emmeans::contrast(emmeans::emmeans(model, "ttt"), "pairwise", adjust = "Tukey")
    
    # both sexes for plot
    data_sem_sampled_sessions <- data_sem_sampled_sessions |>
      mutate(plot_label = paste0(ttt, sex),
             sex = ifelse(sex == "mal","Males","Females"))

    plot <- ggplot(data = data_sem_sampled_sessions, aes(x = ttt_plot, y = get(variable), fill = plot_label)) +
      facet_wrap(. ~ sex) +
      theme_classic() +
      geom_violin() + 
      # geom_jitter(shape = 16, height = 0, width = 0.3, alpha = 0.05) +
      scale_fill_manual(values=c("#9A4D1D", "#14453D", "#E6AA68","#43C59E","#D36135","#3D7068")) +
      theme(legend.position = "none") +
      xlab("Pollinator abundance") +
      ylab(clean_name) +
      theme(axis.text = element_text(size = 15), 
            axis.title.x = element_text(size = 18, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            axis.title.y = element_text(size = text_size, margin = margin(t = 00, r = 10, b = 0, l = 0)),
            strip.background = element_blank(),
            strip.text.x = element_blank()) +
      stat_summary(
        fun.data = data_summary_plot,
        geom = "pointrange",
        shape = 21,            
        size = 1,             
        stroke = 1,           
        color = "black",       
        fill = "white"        
      )
  }else if(variable == "oms"){
    
    # poisson
    model <- lme4::glmer(data = data, get(variable) ~ ttt + (1|session), family = "poisson")
    lrt <- anova(model, update(model, . ~ . - ttt))
    summary <- summary(model)
    tukey <- emmeans::contrast(emmeans::emmeans(model, "ttt"), "pairwise", adjust = "Tukey")
    
    # both sexes for plot
    data_sem_sampled_sessions <- data_sem_sampled_sessions |>
      mutate(plot_label = paste0(ttt, sex),
             sex = ifelse(sex == "mal","Males","Females"))
    
    plot <- ggplot(data = data_sem_sampled_sessions, aes(x = ttt_plot, y = get(variable), fill = plot_label)) +
      facet_wrap(. ~ sex) +
      theme_classic() +
      geom_violin() + 
      # geom_jitter(shape = 16, height = 0, width = 0.3, alpha = 0.05) +
      scale_fill_manual(values=c("#9A4D1D", "#14453D", "#E6AA68","#43C59E","#D36135","#3D7068")) +
      theme(legend.position = "none") +
      xlab("Pollinator abundance") +
      ylab(clean_name) +
      theme(axis.text = element_text(size = 15), 
            axis.title.x = element_text(size = 18, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            axis.title.y = element_text(size = text_size, margin = margin(t = 00, r = 10, b = 0, l = 0)),
            strip.background = element_blank(),
            strip.text.x = element_blank()) +
      stat_summary(
        fun.data = data_summary_plot,
        geom = "pointrange",
        shape = 21,            
        size = 1,             
        stroke = 1,           
        color = "black",       
        fill = "white"        
      )
  }else if(variable == "mean_ps"){
    
    # gaussian male
    model <- lme4::lmer(data = data, get(variable) ~ ttt + (1|session))
    lrt <- anova(model, update(model, . ~ . - ttt))
    summary <- summary(model)
    tukey <- emmeans::contrast(emmeans::emmeans(model, "ttt"), "pairwise", adjust = "Tukey")
    
    plot <- ggplot(data = data, aes(x = ttt_plot, y = get(variable), fill = ttt_plot)) +
      theme_classic() +
      geom_violin() + 
      # geom_jitter(shape = 16, height = 0, width = 0.3, alpha = 0.05) +
      scale_fill_manual(values=c("#43C59E","#3D7068","#14453D")) +
      theme(legend.position = "none") +
      xlab("Pollinator abundance") +
      ylab(clean_name) +
      theme(axis.text = element_text(size = 15), 
            axis.title.x = element_text(size = 18, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            axis.title.y = element_text(size = text_size, margin = margin(t = 00, r = 10, b = 0, l = 0))) +
      stat_summary(
        fun.data = data_summary_plot,
        geom = "pointrange",
        shape = 21,            
        size = 1,             
        stroke = 1,           
        color = "black",       
        fill = "white"        
      )
  }else if(variable %in% c("contact_id","nb_flower_visited")){
    
    # same for male and female
    data_sem_sampled_sessions <- data_sem_sampled_sessions |>
      filter(sex == "fem")
    
    # poisson
    model <- lme4::glmer(data = data_sem_sampled_sessions, get(variable) ~ ttt + (1|session), family = "poisson")
    lrt <- anova(model, update(model, . ~ . - ttt))
    summary <- summary(model)
    tukey <- emmeans::contrast(emmeans::emmeans(model, "ttt"), "pairwise", adjust = "Tukey")
    
    plot <- ggplot(data = data_sem_sampled_sessions, aes(x = ttt_plot, y = get(variable), fill = ttt_plot)) +
      theme_classic() +
      geom_violin() + 
      # geom_jitter(shape = 16, height = 0, width = 0.3, alpha = 0.05) +
      scale_fill_manual(values=c("#E9C8CE","#B384A7","#81657C")) +
      theme(legend.position = "none") +
      theme(axis.text = element_text(size = 15), 
            axis.title.x = element_text(size = 18, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            axis.title.y = element_text(size = text_size, margin = margin(t = 00, r = 10, b = 0, l = 0))) +
      xlab("Pollinator abundance") +
      ylab(clean_name) +
      stat_summary(
        fun.data = data_summary_plot,
        geom = "pointrange",
        shape = 21,            
        size = 1,             
        stroke = 1,           
        color = "black",       
        fill = "white"        
      )
  }else if(variable == "diff_q"){
    
    #  gaussian female
    model <- lme4::lmer(data = data, get(variable) ~ ttt + (1|session))
    lrt <- anova(model, update(model, . ~ . - ttt))
    summary <- summary(model)
    tukey <- emmeans::contrast(emmeans::emmeans(model, "ttt"), "pairwise", adjust = "Tukey")
    
    plot <- ggplot(data = data, aes(x = ttt_plot, y = get(variable), fill = ttt_plot)) +
      theme_classic() +
      geom_violin() + 
      theme(axis.text = element_text(size = 15), 
            axis.title.x = element_text(size = 18, margin = margin(t = 10, r = 0, b = 0, l = 0)),
            axis.title.y = element_text(size = text_size, margin = margin(t = 00, r = 10, b = 0, l = 0))) +
      # geom_jitter(shape = 16, height = 0, width = 0.3, alpha = 0.05) +
      scale_fill_manual(values=c("#E6AA68","#D36135","#9A4D1D")) +
      theme(legend.position = "none") +
      xlab("Pollinator abundance") +
      ylab(clean_name) +
      stat_summary(
        fun.data = data_summary_plot,
        geom = "pointrange",
        shape = 21,            
        size = 1,             
        stroke = 1,           
        color = "black",       
        fill = "white"        
      )
  }else{
    
    # same for male and female
    data_sem_sampled_sessions <- data_sem_sampled_sessions |>
      filter(sex == "fem")
    
    # gaussian
    model <- lme4::lmer(data = data_sem_sampled_sessions, get(variable) ~ ttt + (1|session))
    lrt <- anova(model, update(model, . ~ . - ttt))
    summary <- summary(model)
    tukey <- emmeans::contrast(emmeans::emmeans(model, "ttt"), "pairwise", adjust = "Tukey")
    
    plot <- ggplot(data = data_sem_sampled_sessions, aes(x = ttt_plot, y = get(variable), fill = ttt_plot)) +
      theme_classic() +
      geom_violin() + 
      theme(axis.text = element_text(size = 15), 
            axis.title = element_text(size = text_size)) +
      # geom_jitter(shape = 16, height = 0, width = 0.3, alpha = 0.05) +
      scale_fill_manual(values=c("#E9C8CE","#B384A7","#81657C")) +
      theme(legend.position = "none") +
      xlab("Pollinator abundance") +
      ylab(clean_name) +
      stat_summary(
        fun.data = data_summary_plot,
        geom = "pointrange",
        shape = 21,            
        size = 1,             
        stroke = 1,           
        color = "black",       
        fill = "white"        
      )
  }
  
  return(list(model = model,
              lrt = lrt,
              summary = summary,
              tukey = tukey,
              plot = plot))
}

#' Get z tests to compare SEM
#'
#' @description 
#'
#' @param file 
#'
#' @return Results
#' 
#' @import dplyr
#' 
#' @export

get_z_tests <- function(piecewisemales_low_combi1_visits,
                        piecewisemales_medium_combi1_visits,
                        piecewisemales_high_combi1_visits,
                        piecewisefemales_low_combi1_visits,
                        piecewisefemales_medium_combi1_visits,
                        piecewisefemales_high_combi1_visits){
  
  cf_males_low <- piecewisemales_low_combi1_visits$coefs
  cf_males_medium <- piecewisemales_medium_combi1_visits$coefs
  cf_males_high <- piecewisemales_high_combi1_visits$coefs
  cf_females_low <- piecewisefemales_low_combi1_visits$coefs
  cf_females_medium <- piecewisefemales_medium_combi1_visits$coefs
  cf_females_high <- piecewisefemales_high_combi1_visits$coefs
  
  # Clean coefficient tables
  clean_cf <- function(df, sex, treatment) {
    df <- df[, colnames(df) != ""]
    df <- df |>
      dplyr::mutate(Sex = sex, Treatment = treatment)
    return(df)
  }
  
  all_data <- dplyr::bind_rows(
    clean_cf(cf_males_low, "Male", "Low"),
    clean_cf(cf_males_medium, "Male", "Medium"),
    clean_cf(cf_males_high, "Male", "High"),
    clean_cf(cf_females_low, "Female", "Low"),
    clean_cf(cf_females_medium, "Female", "Medium"),
    clean_cf(cf_females_high, "Female", "High")
  )
  
  
  # comparison within sex
  
  wide_intra <- all_data %>%
    dplyr::select(Sex, Response, Predictor, Treatment,
                  Estimate, Std.Error) %>%
    tidyr::pivot_wider(
      names_from = Treatment,
      values_from = c(Estimate, Std.Error)
    )
  
  intra_sex <- wide_intra %>%
    dplyr::mutate(
      
      # Z-tests
      Low_vs_Medium_z =
        (Estimate_Low - Estimate_Medium) /
        sqrt(Std.Error_Low^2 + Std.Error_Medium^2),
      
      Low_vs_High_z =
        (Estimate_Low - Estimate_High) /
        sqrt(Std.Error_Low^2 + Std.Error_High^2),
      
      Medium_vs_High_z =
        (Estimate_Medium - Estimate_High) /
        sqrt(Std.Error_Medium^2 + Std.Error_High^2),
      
      # Raw P-values
      Low_vs_Medium_p =
        2 * pnorm(-abs(Low_vs_Medium_z)),
      
      Low_vs_High_p =
        2 * pnorm(-abs(Low_vs_High_z)),
      
      Medium_vs_High_p =
        2 * pnorm(-abs(Medium_vs_High_z))
    ) %>%
    
    # Holm correction applied separately to the three
    # treatment comparisons for each pathway and sex
    dplyr::rowwise() %>%
    dplyr::mutate(
      p_holm = list(
        p.adjust(
          c(
            Low_vs_Medium_p,
            Low_vs_High_p,
            Medium_vs_High_p
          ),
          method = "holm"
        )
      )
    ) %>%
    dplyr::ungroup() %>%
    
    dplyr::mutate(
      Low_vs_Medium_p_holm = purrr::map_dbl(p_holm, 1),
      Low_vs_High_p_holm = purrr::map_dbl(p_holm, 2),
      Medium_vs_High_p_holm = purrr::map_dbl(p_holm, 3)
    ) %>%
    
    dplyr::select(
      Sex, Response, Predictor,
      Low_vs_Medium_z,
      Low_vs_Medium_p,
      Low_vs_Medium_p_holm,
      Low_vs_High_z,
      Low_vs_High_p,
      Low_vs_High_p_holm,
      Medium_vs_High_z,
      Medium_vs_High_p,
      Medium_vs_High_p_holm
    )
  
  
  # comparison between sex for common pathways
  
  wide_inter <- all_data %>%
    dplyr::select(Sex, Response, Predictor, Treatment,
                  Estimate, Std.Error) %>%
    tidyr::pivot_wider(
      names_from = Sex,
      values_from = c(Estimate, Std.Error)
    )
  
  inter_sex <- wide_inter %>%
    dplyr::mutate(
      
      Male_vs_Female_z =
        (Estimate_Male - Estimate_Female) /
        sqrt(Std.Error_Male^2 + Std.Error_Female^2),
      
      Male_vs_Female_p =
        2 * pnorm(-abs(Male_vs_Female_z))
    ) %>%
    
    dplyr::select(
      Treatment, Response, Predictor,
      Male_vs_Female_z,
      Male_vs_Female_p
    )
  
  # Round numerical columns to 3 decimal places for output
  intra_sex <- intra_sex %>%
    dplyr::mutate(
      dplyr::across(
        where(is.numeric),
        ~ round(.x, 3)
      )
    )
  
  inter_sex <- inter_sex %>%
    dplyr::mutate(
      dplyr::across(
        where(is.numeric),
        ~ round(.x, 3)
      )
    )
  
  # Return results
  return(
    list(
      intra_sex = intra_sex,
      inter_sex = inter_sex
    )
  )
}



# get_z_tests <- function(piecewisemales_low_combi1_visits,piecewisemales_medium_combi1_visits,piecewisemales_high_combi1_visits,
#                         piecewisefemales_low_combi1_visits,piecewisefemales_medium_combi1_visits,piecewisefemales_high_combi1_visits){
#   
#   cf_males_low <- piecewisemales_low_combi1_visits$coefs
#   cf_males_medium <- piecewisemales_medium_combi1_visits$coefs
#   cf_males_high <- piecewisemales_high_combi1_visits$coefs
#   cf_females_low <- piecewisefemales_low_combi1_visits$coefs
#   cf_females_medium <- piecewisefemales_medium_combi1_visits$coefs
#   cf_females_high <- piecewisefemales_high_combi1_visits$coefs
# 
#   # merging tables
#   clean_cf <- function(df, sex, treatment) {
#     df <- df[, colnames(df) != ""]  # enlève la colonne vide
#     df <- df |>
#       dplyr::mutate(Sex = sex, Treatment = treatment)
#     return(df)
#   }
#   
#   all_data <- bind_rows(
#     clean_cf(cf_males_low, "Male", "Low"),
#     clean_cf(cf_males_medium, "Male", "Medium"),
#     clean_cf(cf_males_high, "Male", "High"),
#     clean_cf(cf_females_low, "Female", "Low"),
#     clean_cf(cf_females_medium, "Female", "Medium"),
#     clean_cf(cf_females_high, "Female", "High")
#   )
#    
#   # generic comparison function for z-tests
#   comp_fun <- function(est1, se1, est2, se2) {
#     z <- (est1 - est2) / sqrt(se1^2 + se2^2)
#     p <- 2 * pnorm(-abs(z))
#     tibble(z = z, p = p)
#   }
#   
#   # intrasex comparison function between pollinator ttt
#   wide_intra <- all_data %>%
#     select(Sex, Response, Predictor, Treatment, Estimate, Std.Error) %>%
#     pivot_wider(
#       names_from = Treatment,
#       values_from = c(Estimate, Std.Error)
#     )
#   
#   intra_sex <- wide_intra %>%
#     mutate(
#       Low_vs_Medium_z = round((Estimate_Low - Estimate_Medium) /
#         sqrt(Std.Error_Low^2 + Std.Error_Medium^2),3),
#       Low_vs_Medium_p = round(2 * pnorm(-abs(Low_vs_Medium_z)),3),
#       
#       Low_vs_High_z = round((Estimate_Low - Estimate_High) /
#         sqrt(Std.Error_Low^2 + Std.Error_High^2),3),
#       Low_vs_High_p = round(2 * pnorm(-abs(Low_vs_High_z)),3),
#       
#       Medium_vs_High_z = round((Estimate_Medium - Estimate_High) /
#         sqrt(Std.Error_Medium^2 + Std.Error_High^2),3),
#       Medium_vs_High_p = round(2 * pnorm(-abs(Medium_vs_High_z)),3)
#     ) %>%
#     select(Sex, Response, Predictor,
#            Low_vs_Medium_z, Low_vs_Medium_p,
#            Low_vs_High_z, Low_vs_High_p,
#            Medium_vs_High_z, Medium_vs_High_p)
#   
#   # intersex comparison 
#   wide_inter <- all_data %>%
#     select(Sex, Response, Predictor, Treatment, Estimate, Std.Error) %>%
#     pivot_wider(
#       names_from = Sex,
#       values_from = c(Estimate, Std.Error)
#     )
#   
#   inter_sex <- wide_inter %>%
#     mutate(
#       Male_vs_Female_z = round((Estimate_Male - Estimate_Female) /
#         sqrt(Std.Error_Male^2 + Std.Error_Female^2),3),
#       Male_vs_Female_p = round(2 * pnorm(-abs(Male_vs_Female_z)),3)
#     ) %>%
#     select(Treatment, Response, Predictor,
#            Male_vs_Female_z, Male_vs_Female_p)
#   
#   list(intra_sex = intra_sex, inter_sex = inter_sex)
# }


#' SEM analyses with piecewise - males basic model
#'
#' @description piecewise adapted only to males
#'
#' @param data 
#'
#' @return 
#' 
#' @import dplyr, piecewiseSEM
#' 
#' @export

get_piecewise_males_visits <- function(data_sem_sampled_sessions, target_ttt = "low", target_sex = "mal",
                                       target_sr = "W", target_ps = "PS",
                                       target_traits = c("F","H"),
                                       x_coord = c(1,3,1.25,2.75,1.75,2.25,2),
                                       y_coord = c(1,1,2,2,3,3,4)){
  
  # Couleur des labels de noeuds selon ttt
  color <- dplyr::case_when(target_ttt == "low" ~ "#43C59E",
                            target_ttt == "medium" ~ "#3D7068",
                            target_ttt == "high" ~ "#14453D")
  
  # Filtrer les données
  data_target <- data_sem_sampled_sessions %>%
    dplyr::filter(ttt == target_ttt & sex == target_sex)
  
  data_target <- data_sem_sampled_sessions %>%
    dplyr::filter(sex == target_sex)
  
  # Créer les formules
  # formula1a <- stats::reformulate(c(target_traits,"PLA"), response = "POS") |> stats::update(. ~ . + (1|session))
  formula1b <- stats::reformulate(target_traits, response = "PLA") |> stats::update(. ~ . + (1|session))
  formula1c <- stats::reformulate(c(target_traits,"PLA"), response = "FLO") |> stats::update(. ~ . + (1|session))
  # formula1d <- stats::reformulate(target_traits, response = "VIS") |> stats::update(. ~ . + (1|session))
  formula2 <- stats::reformulate(c("PLA","FLO"), response = "MS") |> stats::update(. ~ . + (1|session))
  formula3 <- stats::reformulate(c("PLA","FLO"), response = target_ps) |> stats::update(. ~ . + (1|session))
  formula4 <- stats::reformulate(c("MS", target_ps), response = target_sr) |> stats::update(. ~ . + (1|session))
  
  model1b <- lme4::lmer(formula1b, data = data_target)
  model1c <- lme4::lmer(formula1c, data = data_target)
  model2 <- lme4::lmer(formula2,  data = data_target)
  model3 <-lme4::lmer(formula3,  data = data_target)
  model4 <-lme4::lmer(formula4,  data = data_target)
  
  plot(resid(model1b) ~ fitted(model1b)) # homogeneity
  qqnorm(resid(model1b)); qqline(resid(model1b)) # normality
  summary(model1b)
  
  plot(resid(model1c) ~ fitted(model1c)) # homogeneity
  qqnorm(resid(model1c)); qqline(resid(model1c)) # normality
  summary(model1c)
  
  plot(resid(model2) ~ fitted(model2)) # homogeneity
  qqnorm(resid(model2)); qqline(resid(model2)) # normality
  summary(model2)
  
  plot(resid(model3) ~ fitted(model3)) # homogeneity
  qqnorm(resid(model3)); qqline(resid(model3)) # normality
  summary(model3)
  
  plot(resid(model4) ~ fitted(model4)) # homogeneity
  qqnorm(resid(model4)); qqline(resid(model4)) # normality
  summary(model4)
  
  # Construire le psem
  psem_proxy <- piecewiseSEM::psem(
    model1b,
    model1c,
    model2,
    model3,
    model4
  )
  
  summary(psem_proxy, groups = "ttt")
  
  # Extraire coefficients
  cf <- piecewiseSEM::coefs(psem_proxy)
  
  # Créer le graphe
  g <- igraph::graph_from_data_frame(
    d = data.frame(from = cf$Predictor, to = cf$Response),
    directed = TRUE
  )
  
  # Ajouter couleur des arêtes et estimates
  edge_cols <- ifelse(cf$P.Value < 0.05, "black", "lightgrey")
  ggraph_edges <- data.frame(from = cf$Predictor, to = cf$Response,
                             estimate = round(cf$Std.Estimate, 2),
                             color = edge_cols) |>
    mutate(estimate = ifelse(color == "lightgrey","",estimate))
  
  # Coordonnées des noeuds
  coords <- data.frame(name = igraph::V(g)$name,
                       x = x_coord[1:length(igraph::V(g))],
                       y = y_coord[1:length(igraph::V(g))])
  
  # Plot avec ggraph
  p <- ggraph::ggraph(g, layout = 'manual', x = coords$x, y = coords$y) +
    # Arêtes avec couleur selon significativité
    ggraph::geom_edge_link(aes(color = I(ggraph_edges$color), label = ggraph_edges$estimate),hjust=-0.1) +
    # Noeuds
    ggraph::geom_node_point(color = color, size = 15, shape = 21, stroke = 1.5, fill = "white") +
    ggraph::geom_node_text(aes(label = name), color = color, size = 4) +
    ggplot2::theme_void()
  
  # Retourner résumé et plot
  summary <- summary(psem_proxy)
  
  # dSep
  dsep <- piecewiseSEM::dSep(psem_proxy)
  
  
  return(list(summary = summary,
              dsep = dsep,
              coefs = cf,
              plot = p))
}

#' SEM analyses with piecewise - males basic model
#'
#' @description piecewise adapted only to males
#'
#' @param data 
#'
#' @return 
#' 
#' @import dplyr, piecewiseSEM
#' 
#' @export

get_piecewise_males_visits_low <- function(data_sem_sampled_sessions, target_ttt = "low", target_sex = "mal",
                                       target_sr = "W", target_ps = "PS",
                                       target_traits = c("F","H"),
                                       x_coord = c(1,3,1.25,2.75,1.75,2.25,2),
                                       y_coord = c(1,1,2,2,3,3,4)) {
  
  # Couleur des labels de noeuds selon ttt
  color <- dplyr::case_when(target_ttt == "low" ~ "#43C59E",
                            target_ttt == "medium" ~ "#3D7068",
                            target_ttt == "high" ~ "#14453D")
  
  # Filtrer les données
  data_target <- data_sem_sampled_sessions %>%
    dplyr::filter(ttt == target_ttt & sex == target_sex) 
  
  # Créer les formules
  # formula1a <- stats::reformulate(c(target_traits,"PLA"), response = "POS") |> stats::update(. ~ . + (1|session))
  formula1b <- stats::reformulate(target_traits, response = "PLA") |> stats::update(. ~ . + (1|session))
  formula1c <- stats::reformulate(c(target_traits,"PLA"), response = "FLO") |> stats::update(. ~ . + (1|session))
  # formula1d <- stats::reformulate(target_traits, response = "VIS") |> stats::update(. ~ . + (1|session))
  formula2 <- stats::reformulate(c("PLA","FLO",target_traits[2]), response = "MS") |> stats::update(. ~ . + (1|session))
  formula3 <- stats::reformulate(c("PLA","FLO"), response = target_ps) |> stats::update(. ~ . + (1|session))
  formula4 <- stats::reformulate(c("MS", target_ps), response = target_sr) |> stats::update(. ~ . + (1|session))
  # Construire le psem
  psem_proxy <- piecewiseSEM::psem(
    # lme4::lmer(formula1a, data = data_target),
    lme4::lmer(formula1b, data = data_target),
    lme4::lmer(formula1c, data = data_target),
    # lme4::lmer(formula1d, data = data_target),
    lme4::lmer(formula2,  data = data_target),
    lme4::lmer(formula3,  data = data_target),
    lme4::lmer(formula4,  data = data_target)
    
  )
  
  # Extraire coefficients
  cf <- piecewiseSEM::coefs(psem_proxy)
  
  # Créer le graphe
  g <- igraph::graph_from_data_frame(
    d = data.frame(from = cf$Predictor, to = cf$Response),
    directed = TRUE
  )
  
  # Ajouter couleur des arêtes et estimates
  edge_cols <- ifelse(cf$P.Value < 0.05, "black", "lightgrey")
  ggraph_edges <- data.frame(from = cf$Predictor, to = cf$Response,
                             estimate = round(cf$Std.Estimate, 2),
                             color = edge_cols) |>
    mutate(estimate = ifelse(color == "lightgrey","",estimate))
  
  # Coordonnées des noeuds
  coords <- data.frame(name = igraph::V(g)$name,
                       x = x_coord[1:length(igraph::V(g))],
                       y = y_coord[1:length(igraph::V(g))])
  
  # Plot avec ggraph
  p <- ggraph::ggraph(g, layout = 'manual', x = coords$x, y = coords$y) +
    # Arêtes avec couleur selon significativité
    ggraph::geom_edge_link(aes(color = I(ggraph_edges$color), label = ggraph_edges$estimate),hjust=-0.1) +
    # Noeuds
    ggraph::geom_node_point(color = color, size = 15, shape = 21, stroke = 1.5, fill = "white") +
    ggraph::geom_node_text(aes(label = name), color = color, size = 4) +
    ggplot2::theme_void()
  
  # Retourner résumé et plot
  summary <- summary(psem_proxy)
  
  # dSep
  dsep <- piecewiseSEM::dSep(psem_proxy)
  
  names(cf) <- make.names(names(cf), unique = TRUE)
  
  cf_final <- cf |>
    mutate(across(where(is.numeric), ~ round(.x, 3))) |>
    mutate(clean_est = paste0(Estimate, " ± ", Std.Error))
  
  return(list(summary = summary,
              dsep = dsep,
              plot = p, 
              coefs = cf_final))
}

# get_piecewise_males_visits_low(data_sem_sampled_sessions)

#' SEM analyses with piecewise - males basic model
#'
#' @description piecewise adapted only to males
#'
#' @param data 
#'
#' @return 
#' 
#' @import dplyr, piecewiseSEM
#' 
#' @export

get_piecewise_males_visits_medium <- function(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "mal",
                                       target_sr = "W", target_ps = "PS",
                                       target_traits = c("F","H"),
                                       x_coord = c(1,3,1.25,2.75,1.75,2.25,2),
                                       y_coord = c(1,1,2,2,3,3,4)) {
  
  # Couleur des labels de noeuds selon ttt
  color <- dplyr::case_when(target_ttt == "low" ~ "#43C59E",
                            target_ttt == "medium" ~ "#3D7068",
                            target_ttt == "high" ~ "#14453D")
  
  # Filtrer les données
  data_target <- data_sem_sampled_sessions %>%
    dplyr::filter(ttt == target_ttt & sex == target_sex)
  
  # Créer les formules
  # formula1a <- stats::reformulate(c(target_traits,"PLA"), response = "POS") |> stats::update(. ~ . + (1|session))
  formula1b <- stats::reformulate(target_traits, response = "PLA") |> stats::update(. ~ . + (1|session))
  formula1c <- stats::reformulate(c(target_traits,"PLA"), response = "FLO") |> stats::update(. ~ . + (1|session))
  # formula1d <- stats::reformulate(target_traits, response = "VIS") |> stats::update(. ~ . + (1|session))
  formula2 <- stats::reformulate(c("PLA","FLO"), response = "MS") |> stats::update(. ~ . + (1|session))
  formula3 <- stats::reformulate(c("PLA","FLO"), response = target_ps) |> stats::update(. ~ . + (1|session))
  formula4 <- stats::reformulate(c("MS", target_ps), response = target_sr) |> stats::update(. ~ . + (1|session))
  
  model1b <- lme4::lmer(formula1b, data = data_target)
  model1c <- lme4::lmer(formula1c, data = data_target)
  model2 <- lme4::lmer(formula2,  data = data_target)
  model3 <-lme4::lmer(formula3,  data = data_target)
  model4 <-lme4::lmer(formula4,  data = data_target)
  
  plot(resid(model1b) ~ fitted(model1b)) # homogeneity
  qqnorm(resid(model1b)); qqline(resid(model1b)) # normality
  
  plot(resid(model1c) ~ fitted(model1c)) # homogeneity
  qqnorm(resid(model1c)); qqline(resid(model1c)) # normality
  
  plot(resid(model2) ~ fitted(model2)) # homogeneity
  qqnorm(resid(model2)); qqline(resid(model2)) # normality
  
  plot(resid(model3) ~ fitted(model3)) # homogeneity
  qqnorm(resid(model3)); qqline(resid(model3)) # normality
  
  plot(resid(model4) ~ fitted(model4)) # homogeneity
  qqnorm(resid(model4)); qqline(resid(model4)) # normality
  
  # Construire le psem
  psem_proxy <- piecewiseSEM::psem(
    model1b,
    model1c,
    model2,
    model3,
    model4
  )
  
  # Extraire coefficients
  cf <- piecewiseSEM::coefs(psem_proxy)
  
  # Créer le graphe
  g <- igraph::graph_from_data_frame(
    d = data.frame(from = cf$Predictor, to = cf$Response),
    directed = TRUE
  )
  
  # Ajouter couleur des arêtes et estimates
  edge_cols <- ifelse(cf$P.Value < 0.05, "black", "lightgrey")
  ggraph_edges <- data.frame(from = cf$Predictor, to = cf$Response,
                             estimate = round(cf$Std.Estimate, 2),
                             color = edge_cols) |>
    mutate(estimate = ifelse(color == "lightgrey","",estimate))
  
  # Coordonnées des noeuds
  coords <- data.frame(name = igraph::V(g)$name,
                       x = x_coord[1:length(igraph::V(g))],
                       y = y_coord[1:length(igraph::V(g))])
  
  # Plot avec ggraph
  p <- ggraph::ggraph(g, layout = 'manual', x = coords$x, y = coords$y) +
    # Arêtes avec couleur selon significativité
    ggraph::geom_edge_link(aes(color = I(ggraph_edges$color), label = ggraph_edges$estimate),hjust=-0.1) +
    # Noeuds
    ggraph::geom_node_point(color = color, size = 15, shape = 21, stroke = 1.5, fill = "white") +
    ggraph::geom_node_text(aes(label = name), color = color, size = 4) +
    ggplot2::theme_void()
  
  # Retourner résumé et plot
  summary <- summary(psem_proxy)
  
  # dSep
  dsep <- piecewiseSEM::dSep(psem_proxy)
  
  names(cf) <- make.names(names(cf), unique = TRUE)
  
  cf_final <- cf |>
    mutate(across(where(is.numeric), ~ round(.x, 3))) |>
    mutate(clean_est = paste0(Estimate, " ± ", Std.Error))
  
  return(list(summary = summary,
              dsep = dsep,
              plot = p, 
              coefs = cf_final))
}

# get_piecewise_males_visits_medium(data_sem_sampled_sessions)

#' SEM analyses with piecewise - males basic model
#'
#' @description piecewise adapted only to males
#'
#' @param data 
#'
#' @return 
#' 
#' @import dplyr, piecewiseSEM
#' 
#' @export

get_piecewise_males_visits_high <- function(data_sem_sampled_sessions, target_ttt = "high", target_sex = "mal",
                                           target_sr = "W", target_ps = "PS",
                                           target_traits = c("F","H"),
                                           x_coord = c(1,3,1.25,2.75,1.75,2.25,2),
                                           y_coord = c(1,1,2,2,3,3,4)) {
  
  # Couleur des labels de noeuds selon ttt
  color <- dplyr::case_when(target_ttt == "low" ~ "#43C59E",
                            target_ttt == "medium" ~ "#3D7068",
                            target_ttt == "high" ~ "#14453D")
  
  # Filtrer les données
  data_target <- data_sem_sampled_sessions %>%
    dplyr::filter(ttt == target_ttt & sex == target_sex) 
  
  # Créer les formules
  # formula1a <- stats::reformulate(c(target_traits,"PLA"), response = "POS") |> stats::update(. ~ . + (1|session))
  formula1b <- stats::reformulate(target_traits, response = "PLA") |> stats::update(. ~ . + (1|session))
  formula1c <- stats::reformulate(c(target_traits,"PLA"), response = "FLO") |> stats::update(. ~ . + (1|session))
  # formula1d <- stats::reformulate(target_traits, response = "VIS") |> stats::update(. ~ . + (1|session))
  formula2 <- stats::reformulate(c("PLA","FLO"), response = "MS") |> stats::update(. ~ . + (1|session))
  formula3 <- stats::reformulate(c("PLA","FLO",target_traits[1]), response = target_ps) |> stats::update(. ~ . + (1|session))
  formula4 <- stats::reformulate(c("MS", target_ps), response = target_sr) |> stats::update(. ~ . + (1|session))
  # Construire le psem
  psem_proxy <- piecewiseSEM::psem(
    # lme4::lmer(formula1a, data = data_target),
    lme4::lmer(formula1b, data = data_target),
    lme4::lmer(formula1c, data = data_target),
    # lme4::lmer(formula1d, data = data_target),
    lme4::lmer(formula2,  data = data_target),
    lme4::lmer(formula3,  data = data_target),
    lme4::lmer(formula4,  data = data_target)
    
  )
  
  # Extraire coefficients
  cf <- piecewiseSEM::coefs(psem_proxy)
  
  # Créer le graphe
  g <- igraph::graph_from_data_frame(
    d = data.frame(from = cf$Predictor, to = cf$Response),
    directed = TRUE
  )
  
  # Ajouter couleur des arêtes et estimates
  edge_cols <- ifelse(cf$P.Value < 0.05, "black", "lightgrey")
  ggraph_edges <- data.frame(from = cf$Predictor, to = cf$Response,
                             estimate = round(cf$Std.Estimate, 2),
                             color = edge_cols) |>
    mutate(estimate = ifelse(color == "lightgrey","",estimate))
  
  # Coordonnées des noeuds
  coords <- data.frame(name = igraph::V(g)$name,
                       x = x_coord[1:length(igraph::V(g))],
                       y = y_coord[1:length(igraph::V(g))])
  
  # Plot avec ggraph
  p <- ggraph::ggraph(g, layout = 'manual', x = coords$x, y = coords$y) +
    # Arêtes avec couleur selon significativité
    ggraph::geom_edge_link(aes(color = I(ggraph_edges$color), label = ggraph_edges$estimate),hjust=-0.1) +
    # Noeuds
    ggraph::geom_node_point(color = color, size = 15, shape = 21, stroke = 1.5, fill = "white") +
    ggraph::geom_node_text(aes(label = name), color = color, size = 4) +
    ggplot2::theme_void()
  
  # Retourner résumé et plot
  summary <- summary(psem_proxy)
  
  # dSep
  dsep <- piecewiseSEM::dSep(psem_proxy)
  
  names(cf) <- make.names(names(cf), unique = TRUE)
  
  cf_final <- cf |>
    mutate(across(where(is.numeric), ~ round(.x, 3))) |>
    mutate(clean_est = paste0(Estimate, " ± ", Std.Error))
  
  return(list(summary = summary,
              dsep = dsep,
              plot = p, 
              coefs = cf_final))
}

# get_piecewise_males_visits_high(data_sem_sampled_sessions)

#' SEM analyses with piecewise - females basic model
#'
#' @description piecewise adapted only to males
#'
#' @param data 
#'
#' @return 
#' 
#' @import dplyr 
#' 
#' @export

get_piecewise_females_visits <- function(data_sem_sampled_sessions, target_ttt = "low", target_sex = "fem",
                                         target_sr = "W", target_ps = "ME",
                                         target_traits = c("F","H"),
                                         x_coord = c(1,3,1.25,2.75,1.75,2.25,2),
                                         y_coord = c(1,1,2,2,3,3,4)) {
  
  color <- case_when(target_ttt == "low" ~ "#E6AA68",
                     target_ttt == "medium" ~ "#D36135",
                     target_ttt == "high" ~ "#9A4D1D")
  
  # Filtrer les données
  data_target <- data_sem_sampled_sessions %>%
    dplyr::filter(ttt == target_ttt & sex == target_sex)
  
  # Créer les formules
  # formula1a <- stats::reformulate(c(target_traits,"PLA"), response = "POS") |> stats::update(. ~ . + (1|session))
  formula1b <- stats::reformulate(target_traits, response = "PLA") |> stats::update(. ~ . + (1|session))
  formula1c <- stats::reformulate(c(target_traits,"PLA"), response = "FLO") |> stats::update(. ~ . + (1|session))
  # formula1d <- stats::reformulate(target_traits, response = "VIS") |> stats::update(. ~ . + (1|session))
  formula2 <- stats::reformulate(c("PLA","FLO"), response = "MS") |> stats::update(. ~ . + (1|session))
  formula3 <- stats::reformulate(c("PLA","FLO"), response = target_ps) |> stats::update(. ~ . + (1|session))
  formula4 <- stats::reformulate(c(target_traits,"MS", target_ps), response = target_sr) |> stats::update(. ~ . + (1|session))
  
  model1b <- lme4::lmer(formula1b, data = data_target)
  model1c <- lme4::lmer(formula1c, data = data_target)
  model2 <- lme4::lmer(formula2,  data = data_target)
  model3 <-lme4::lmer(formula3,  data = data_target)
  model4 <-lme4::lmer(formula4,  data = data_target)
  
  plot(resid(model1b) ~ fitted(model1b)) # homogeneity
  qqnorm(resid(model1b)); qqline(resid(model1b)) # normality
  summary(model1b)
  
  plot(resid(model1c) ~ fitted(model1c)) # homogeneity
  qqnorm(resid(model1c)); qqline(resid(model1c)) # normality
  summary(model1c)
  
  plot(resid(model2) ~ fitted(model2)) # homogeneity
  qqnorm(resid(model2)); qqline(resid(model2)) # normality
  summary(model2)
  
  plot(resid(model3) ~ fitted(model3)) # homogeneity
  qqnorm(resid(model3)); qqline(resid(model3)) # normality
  summary(model3)
  
  plot(resid(model4) ~ fitted(model4)) # homogeneity
  qqnorm(resid(model4)); qqline(resid(model4)) # normality
  summary(model4)
  
  # Construire le psem
  psem_proxy <- piecewiseSEM::psem(
    # lme4::lmer(formula1a, data = data_target),
      model1b,
      model1c,
      model2,
      model3,
      model4
  )
  
  # Extraire coefficients
  cf <- piecewiseSEM::coefs(psem_proxy)
  
  # Créer le graphe
  g <- igraph::graph_from_data_frame(
    d = data.frame(from = cf$Predictor, to = cf$Response),
    directed = TRUE
  )
  
  # Ajouter couleur des arêtes et estimates
  edge_cols <- ifelse(cf$P.Value < 0.05, "black", "lightgrey")
  ggraph_edges <- data.frame(from = cf$Predictor, to = cf$Response,
                             estimate = round(cf$Std.Estimate, 2),
                             color = edge_cols) |>
    mutate(estimate = ifelse(color == "lightgrey","",estimate))
  
  # Coordonnées des noeuds
  coords <- data.frame(name = igraph::V(g)$name,
                       x = x_coord[1:length(igraph::V(g))],
                       y = y_coord[1:length(igraph::V(g))])
  
  # Plot avec ggraph
  p <- ggraph::ggraph(g, layout = 'manual', x = coords$x, y = coords$y) +
    # Arêtes avec couleur selon significativité
    ggraph::geom_edge_link(aes(color = I(ggraph_edges$color), label = ggraph_edges$estimate),hjust=-0.1) +
    # Noeuds
    ggraph::geom_node_point(color = color, size = 15, shape = 21, stroke = 1.5, fill = "white") +
    ggraph::geom_node_text(aes(label = name), color = color, size = 4) +
    ggplot2::theme_void()
  
  # Retourner résumé et plot
  summary <- summary(psem_proxy)
  
  # dSep
  dsep <- piecewiseSEM::dSep(psem_proxy)
  
  return(list(summary = summary,
              dsep = dsep,
              plot = p, 
              coefs = cf))
}

#' SEM analyses with piecewise - females basic model
#'
#' @description piecewise adapted only to males
#'
#' @param data 
#'
#' @return 
#' 
#' @import dplyr 
#' 
#' @export

get_piecewise_females_visits_low <- function(data_sem_sampled_sessions, target_ttt = "low", target_sex = "fem",
                                         target_sr = "W", target_ps = "ME",
                                         target_traits = c("F","H"),
                                         x_coord = c(1,3,1.25,2.75,1.75,2.25,2),
                                         y_coord = c(1,1,2,2,3,3,4)) {
  
  color <- case_when(target_ttt == "low" ~ "#E6AA68",
                     target_ttt == "medium" ~ "#D36135",
                     target_ttt == "high" ~ "#9A4D1D")
  
  # Filtrer les données
  data_target <- data_sem_sampled_sessions %>%
    dplyr::filter(ttt == target_ttt & sex == target_sex)
  
  # Créer les formules
  # formula1a <- stats::reformulate(c(target_traits,"PLA"), response = "POS") |> stats::update(. ~ . + (1|session))
  formula1b <- stats::reformulate(target_traits, response = "PLA") |> stats::update(. ~ . + (1|session))
  formula1c <- stats::reformulate(c(target_traits,"PLA"), response = "FLO") |> stats::update(. ~ . + (1|session))
  # formula1d <- stats::reformulate(target_traits, response = "VIS") |> stats::update(. ~ . + (1|session))
  formula2 <- stats::reformulate(c("PLA","FLO"), response = "MS") |> stats::update(. ~ . + (1|session))
  formula3 <- stats::reformulate(c("PLA","FLO",target_traits[1]), response = target_ps) |> stats::update(. ~ . + (1|session))
  formula4 <- stats::reformulate(c(target_traits,"MS", target_ps, "FLO"), response = target_sr) |> stats::update(. ~ . + (1|session))
  
  # Construire le psem
  psem_proxy <- piecewiseSEM::psem(
    # lme4::lmer(formula1a, data = data_target),
    lme4::lmer(formula1b, data = data_target),
    lme4::lmer(formula1c, data = data_target),
    lme4::lmer(formula2,  data = data_target),
    lme4::lmer(formula3,  data = data_target),
    lme4::lmer(formula4,  data = data_target)
  )
  
  # Extraire coefficients
  cf <- piecewiseSEM::coefs(psem_proxy)
  
  # Créer le graphe
  g <- igraph::graph_from_data_frame(
    d = data.frame(from = cf$Predictor, to = cf$Response),
    directed = TRUE
  )
  
  # Ajouter couleur des arêtes et estimates
  edge_cols <- ifelse(cf$P.Value < 0.05, "black", "lightgrey")
  ggraph_edges <- data.frame(from = cf$Predictor, to = cf$Response,
                             estimate = round(cf$Std.Estimate, 2),
                             color = edge_cols) |>
    mutate(estimate = ifelse(color == "lightgrey","",estimate))
  
  # Coordonnées des noeuds
  coords <- data.frame(name = igraph::V(g)$name,
                       x = x_coord[1:length(igraph::V(g))],
                       y = y_coord[1:length(igraph::V(g))])
  
  # Plot avec ggraph
  p <- ggraph::ggraph(g, layout = 'manual', x = coords$x, y = coords$y) +
    # Arêtes avec couleur selon significativité
    ggraph::geom_edge_link(aes(color = I(ggraph_edges$color), label = ggraph_edges$estimate),hjust=-0.1,vjust =0.1) +
    # Noeuds
    ggraph::geom_node_point(color = color, size = 15, shape = 21, stroke = 1.5, fill = "white") +
    ggraph::geom_node_text(aes(label = name), color = color, size = 4) +
    ggplot2::theme_void()
  
  # Retourner résumé et plot
  summary <- summary(psem_proxy)
  
  # dSep
  dsep <- piecewiseSEM::dSep(psem_proxy)
  
  names(cf) <- make.names(names(cf), unique = TRUE)
  
  cf_final <- cf |>
    mutate(across(where(is.numeric), ~ round(.x, 3))) |>
    mutate(clean_est = paste0(Estimate, " ± ", Std.Error))
  
  return(list(summary = summary,
              dsep = dsep,
              plot = p, 
              coefs = cf_final))
}
# get_piecewise_females_visits_low(data_sem_sampled_sessions, target_sr = "W")
 
#' SEM analyses with piecewise - females basic model
#'
#' @description piecewise adapted only to males
#'
#' @param data 
#'
#' @return 
#' 
#' @import dplyr 
#' 
#' @export

get_piecewise_females_visits_medium <- function(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "fem",
                                             target_sr = "W", target_ps = "ME",
                                             target_traits = c("F","H"),
                                             x_coord = c(1,3,1.25,2.75,1.75,2.25,2),
                                             y_coord = c(1,1,2,2,3,3,4)) {
  
  color <- case_when(target_ttt == "low" ~ "#E6AA68",
                     target_ttt == "medium" ~ "#D36135",
                     target_ttt == "high" ~ "#9A4D1D")
  
  # Filtrer les données
  data_target <- data_sem_sampled_sessions %>%
    dplyr::filter(ttt == target_ttt & sex == target_sex)
  
  # Créer les formules
  # formula1a <- stats::reformulate(c(target_traits,"PLA"), response = "POS") |> stats::update(. ~ . + (1|session))
  formula1b <- stats::reformulate(target_traits, response = "PLA") |> stats::update(. ~ . + (1|session))
  formula1c <- stats::reformulate(c(target_traits,"PLA"), response = "FLO") |> stats::update(. ~ . + (1|session))
  # formula1d <- stats::reformulate(target_traits, response = "VIS") |> stats::update(. ~ . + (1|session))
  formula2 <- stats::reformulate(c("PLA","FLO",target_traits[2]), response = "MS") |> stats::update(. ~ . + (1|session))
  formula3 <- stats::reformulate(c("PLA","FLO",target_traits), response = target_ps) |> stats::update(. ~ . + (1|session))
  formula4 <- stats::reformulate(c(target_traits,"MS", target_ps), response = target_sr) |> stats::update(. ~ . + (1|session))
  
  # Construire le psem
  psem_proxy <- piecewiseSEM::psem(
    # lme4::lmer(formula1a, data = data_target),
    lme4::lmer(formula1b, data = data_target),
    lme4::lmer(formula1c, data = data_target),
    lme4::lmer(formula2,  data = data_target),
    lme4::lmer(formula3,  data = data_target),
    lme4::lmer(formula4,  data = data_target)
  )
  
  # Extraire coefficients
  cf <- piecewiseSEM::coefs(psem_proxy)
  
  # Créer le graphe
  g <- igraph::graph_from_data_frame(
    d = data.frame(from = cf$Predictor, to = cf$Response),
    directed = TRUE
  )
  
  # Ajouter couleur des arêtes et estimates
  edge_cols <- ifelse(cf$P.Value < 0.05, "black", "lightgrey")
  ggraph_edges <- data.frame(from = cf$Predictor, to = cf$Response,
                             estimate = round(cf$Std.Estimate, 2),
                             color = edge_cols) |>
    mutate(estimate = ifelse(color == "lightgrey","",estimate))
  
  # Coordonnées des noeuds
  coords <- data.frame(name = igraph::V(g)$name,
                       x = x_coord[1:length(igraph::V(g))],
                       y = y_coord[1:length(igraph::V(g))])
  
  # Plot avec ggraph
  p <- ggraph::ggraph(g, layout = 'manual', x = coords$x, y = coords$y) +
    # Arêtes avec couleur selon significativité
    ggraph::geom_edge_link(aes(color = I(ggraph_edges$color), label = ggraph_edges$estimate),hjust=-0.2,vjust = 0.1) +
    # Noeuds
    ggraph::geom_node_point(color = color, size = 15, shape = 21, stroke = 1.5, fill = "white") +
    ggraph::geom_node_text(aes(label = name), color = color, size = 4) +
    ggplot2::theme_void()
  
  # Retourner résumé et plot
  summary <- summary(psem_proxy)
  
  # dSep
  dsep <- piecewiseSEM::dSep(psem_proxy)
  
  names(cf) <- make.names(names(cf), unique = TRUE)
  
  cf_final <- cf |>
    mutate(across(where(is.numeric), ~ round(.x, 3))) |>
    mutate(clean_est = paste0(Estimate, " ± ", Std.Error))
  
  return(list(summary = summary,
              dsep = dsep,
              plot = p, 
              coefs = cf_final))
}
# get_piecewise_females_visits_medium(data_sem_sampled_sessions, target_sr = "W")

#' SEM analyses with piecewise - females basic model
#'
#' @description piecewise adapted only to males
#'
#' @param data 
#'
#' @return 
#' 
#' @import dplyr 
#' 
#' @export

get_piecewise_females_visits_high <- function(data_sem_sampled_sessions, target_ttt = "high", target_sex = "fem",
                                         target_sr = "W", target_ps = "ME",
                                         target_traits = c("F","H"),
                                         x_coord = c(1,3,1.25,2.75,1.75,2.25,2),
                                         y_coord = c(1,1,2,2,3,3,4)) {
  
  color <- case_when(target_ttt == "low" ~ "#E6AA68",
                     target_ttt == "medium" ~ "#D36135",
                     target_ttt == "high" ~ "#9A4D1D")
  
  # Filtrer les données
  data_target <- data_sem_sampled_sessions %>%
    dplyr::filter(ttt == target_ttt & sex == target_sex)
  
  # Créer les formules
  # formula1a <- stats::reformulate(c(target_traits,"PLA"), response = "POS") |> stats::update(. ~ . + (1|session))
  formula1b <- stats::reformulate(target_traits, response = "PLA") |> stats::update(. ~ . + (1|session))
  formula1c <- stats::reformulate(c(target_traits,"PLA"), response = "FLO") |> stats::update(. ~ . + (1|session))
  # formula1d <- stats::reformulate(target_traits, response = "VIS") |> stats::update(. ~ . + (1|session))
  formula2 <- stats::reformulate(c("PLA","FLO"), response = "MS") |> stats::update(. ~ . + (1|session))
  formula3 <- stats::reformulate(c("PLA","FLO"), response = target_ps) |> stats::update(. ~ . + (1|session))
  formula4 <- stats::reformulate(c(target_traits,"MS", target_ps), response = target_sr) |> stats::update(. ~ . + (1|session))
  
  model1b <- lme4::lmer(formula1b, data = data_target)
  model1c <- lme4::lmer(formula1c, data = data_target)
  model2 <- lme4::lmer(formula2,  data = data_target)
  model3 <-lme4::lmer(formula3,  data = data_target)
  model4 <-lme4::lmer(formula4,  data = data_target)
  
  plot(resid(model1b) ~ fitted(model1b)) # homogeneity
  qqnorm(resid(model1b)); qqline(resid(model1b)) # normality
  
  plot(resid(model1c) ~ fitted(model1c)) # homogeneity
  qqnorm(resid(model1c)); qqline(resid(model1c)) # normality
  
  plot(resid(model2) ~ fitted(model2)) # homogeneity
  qqnorm(resid(model2)); qqline(resid(model2)) # normality
  
  plot(resid(model3) ~ fitted(model3)) # homogeneity
  qqnorm(resid(model3)); qqline(resid(model3)) # normality
  
  plot(resid(model4) ~ fitted(model4)) # homogeneity
  qqnorm(resid(model4)); qqline(resid(model4)) # normality
  
  # Construire le psem
  psem_proxy <- piecewiseSEM::psem(
    # lme4::lmer(formula1a, data = data_target),
    model1b,
    model1c,
    model2,
    model3,
    model4
  )
  
  # Extraire coefficients
  cf <- piecewiseSEM::coefs(psem_proxy)
  
  # Créer le graphe
  g <- igraph::graph_from_data_frame(
    d = data.frame(from = cf$Predictor, to = cf$Response),
    directed = TRUE
  )
  
  # Ajouter couleur des arêtes et estimates
  edge_cols <- ifelse(cf$P.Value < 0.05, "black", "lightgrey")
  ggraph_edges <- data.frame(from = cf$Predictor, to = cf$Response,
                             estimate = round(cf$Std.Estimate, 2),
                             color = edge_cols) |>
    mutate(estimate = ifelse(color == "lightgrey","",estimate))
  
  # Coordonnées des noeuds
  coords <- data.frame(name = igraph::V(g)$name,
                       x = x_coord[1:length(igraph::V(g))],
                       y = y_coord[1:length(igraph::V(g))])
  
  # Plot avec ggraph
  p <- ggraph::ggraph(g, layout = 'manual', x = coords$x, y = coords$y) +
    # Arêtes avec couleur selon significativité
    ggraph::geom_edge_link(aes(color = I(ggraph_edges$color), label = ggraph_edges$estimate),hjust=-0.1) +
    # Noeuds
    ggraph::geom_node_point(color = color, size = 15, shape = 21, stroke = 1.5, fill = "white") +
    ggraph::geom_node_text(aes(label = name), color = color, size = 4) +
    ggplot2::theme_void()
  
  # Retourner résumé et plot
  summary <- summary(psem_proxy)
  
  # dSep
  dsep <- piecewiseSEM::dSep(psem_proxy)
  
  names(cf) <- make.names(names(cf), unique = TRUE)
  
  cf_final <- cf |>
    mutate(across(where(is.numeric), ~ round(.x, 3))) |>
    mutate(clean_est = paste0(Estimate, " ± ", Std.Error))
  
  return(list(summary = summary,
              dsep = dsep,
              plot = p, 
              coefs = cf_final))
}