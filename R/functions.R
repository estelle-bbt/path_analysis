#' Generic function to just read dataset in .txt form
#'
#' @description 
#' This function allow to directly load the .txt dataset previously obtained.
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

#' Data summary for plots - effect of ttt on variables
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

data_summary_plot <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
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
  
  # label colors according to ttt
  color <- dplyr::case_when(target_ttt == "low" ~ "#43C59E",
                            target_ttt == "medium" ~ "#3D7068",
                            target_ttt == "high" ~ "#14453D")
  
  # filter data
  data_target <- data_sem_sampled_sessions %>%
    dplyr::filter(ttt == target_ttt & sex == target_sex)
  
  data_target <- data_sem_sampled_sessions %>%
    dplyr::filter(sex == target_sex)
  
  # formula
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
  
  # build the psem
  psem_proxy <- piecewiseSEM::psem(
    model1b,
    model1c,
    model2,
    model3,
    model4
  )
  
  summary(psem_proxy, groups = "ttt")
  
  # extract coefficients
  cf <- piecewiseSEM::coefs(psem_proxy)
  
  # create graph
  g <- igraph::graph_from_data_frame(
    d = data.frame(from = cf$Predictor, to = cf$Response),
    directed = TRUE
  )
  
  # add colors and estimates
  edge_cols <- ifelse(cf$P.Value < 0.05, "black", "lightgrey")
  ggraph_edges <- data.frame(from = cf$Predictor, to = cf$Response,
                             estimate = round(cf$Std.Estimate, 2),
                             color = edge_cols) |>
    mutate(estimate = ifelse(color == "lightgrey","",estimate))
  
  # node coord
  coords <- data.frame(name = igraph::V(g)$name,
                       x = x_coord[1:length(igraph::V(g))],
                       y = y_coord[1:length(igraph::V(g))])
  
  # plot with ggraph
  p <- ggraph::ggraph(g, layout = 'manual', x = coords$x, y = coords$y) +
    ggraph::geom_edge_link(aes(color = I(ggraph_edges$color), label = ggraph_edges$estimate),hjust=-0.1) +
    ggraph::geom_node_point(color = color, size = 15, shape = 21, stroke = 1.5, fill = "white") +
    ggraph::geom_node_text(aes(label = name), color = color, size = 4) +
    ggplot2::theme_void()
  
  # return
  summary <- summary(psem_proxy)
  
  # dSep
  dsep <- piecewiseSEM::dSep(psem_proxy)
  
  
  return(list(summary = summary,
              dsep = dsep,
              coefs = cf,
              plot = p))
}

#' SEM analyses with piecewise - males final model low ttt
#'
#' @description piecewise adapted only to males for the low ttt after dsep
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
  
  # color label according to ttt
  color <- dplyr::case_when(target_ttt == "low" ~ "#43C59E",
                            target_ttt == "medium" ~ "#3D7068",
                            target_ttt == "high" ~ "#14453D")
  
  # filter data
  data_target <- data_sem_sampled_sessions %>%
    dplyr::filter(ttt == target_ttt & sex == target_sex) 
  
  # formula
  # formula1a <- stats::reformulate(c(target_traits,"PLA"), response = "POS") |> stats::update(. ~ . + (1|session))
  formula1b <- stats::reformulate(target_traits, response = "PLA") |> stats::update(. ~ . + (1|session))
  formula1c <- stats::reformulate(c(target_traits,"PLA"), response = "FLO") |> stats::update(. ~ . + (1|session))
  # formula1d <- stats::reformulate(target_traits, response = "VIS") |> stats::update(. ~ . + (1|session))
  formula2 <- stats::reformulate(c("PLA","FLO",target_traits[2]), response = "MS") |> stats::update(. ~ . + (1|session))
  formula3 <- stats::reformulate(c("PLA","FLO"), response = target_ps) |> stats::update(. ~ . + (1|session))
  formula4 <- stats::reformulate(c("MS", target_ps), response = target_sr) |> stats::update(. ~ . + (1|session))
  
  # build the psem
  psem_proxy <- piecewiseSEM::psem(
    # lme4::lmer(formula1a, data = data_target),
    lme4::lmer(formula1b, data = data_target),
    lme4::lmer(formula1c, data = data_target),
    # lme4::lmer(formula1d, data = data_target),
    lme4::lmer(formula2,  data = data_target),
    lme4::lmer(formula3,  data = data_target),
    lme4::lmer(formula4,  data = data_target)
    
  )
  
  # extract coefficients
  cf <- piecewiseSEM::coefs(psem_proxy)
  
  # create the graph
  g <- igraph::graph_from_data_frame(
    d = data.frame(from = cf$Predictor, to = cf$Response),
    directed = TRUE
  )
  
  # add colors and estimate
  edge_cols <- ifelse(cf$P.Value < 0.05, "black", "lightgrey")
  ggraph_edges <- data.frame(from = cf$Predictor, to = cf$Response,
                             estimate = round(cf$Std.Estimate, 2),
                             color = edge_cols) |>
    mutate(estimate = ifelse(color == "lightgrey","",estimate))
  
  # nodes coord
  coords <- data.frame(name = igraph::V(g)$name,
                       x = x_coord[1:length(igraph::V(g))],
                       y = y_coord[1:length(igraph::V(g))])
  
  # plot with ggraph
  p <- ggraph::ggraph(g, layout = 'manual', x = coords$x, y = coords$y) +
    ggraph::geom_edge_link(aes(color = I(ggraph_edges$color), label = ggraph_edges$estimate),hjust=-0.1) +
    ggraph::geom_node_point(color = color, size = 15, shape = 21, stroke = 1.5, fill = "white") +
    ggraph::geom_node_text(aes(label = name), color = color, size = 4) +
    ggplot2::theme_void()
  
  # return
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

#' SEM analyses with piecewise - males final model medium ttt
#'
#' @description piecewise adapted only to males for the low ttt after dsep
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
  
  # color label according to ttt
  color <- dplyr::case_when(target_ttt == "low" ~ "#43C59E",
                            target_ttt == "medium" ~ "#3D7068",
                            target_ttt == "high" ~ "#14453D")
  
  # filter data
  data_target <- data_sem_sampled_sessions %>%
    dplyr::filter(ttt == target_ttt & sex == target_sex)
  
  # formula
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
  
  # build the psem
  psem_proxy <- piecewiseSEM::psem(
    model1b,
    model1c,
    model2,
    model3,
    model4
  )
  
  # extract coefficients
  cf <- piecewiseSEM::coefs(psem_proxy)
  
  # create the graph
  g <- igraph::graph_from_data_frame(
    d = data.frame(from = cf$Predictor, to = cf$Response),
    directed = TRUE
  )
  
  # add colors and estimate
  edge_cols <- ifelse(cf$P.Value < 0.05, "black", "lightgrey")
  ggraph_edges <- data.frame(from = cf$Predictor, to = cf$Response,
                             estimate = round(cf$Std.Estimate, 2),
                             color = edge_cols) |>
    mutate(estimate = ifelse(color == "lightgrey","",estimate))
  
  # nodes coord
  coords <- data.frame(name = igraph::V(g)$name,
                       x = x_coord[1:length(igraph::V(g))],
                       y = y_coord[1:length(igraph::V(g))])
  
  # plot with ggraph
  p <- ggraph::ggraph(g, layout = 'manual', x = coords$x, y = coords$y) +
    ggraph::geom_edge_link(aes(color = I(ggraph_edges$color), label = ggraph_edges$estimate),hjust=-0.1) +
    ggraph::geom_node_point(color = color, size = 15, shape = 21, stroke = 1.5, fill = "white") +
    ggraph::geom_node_text(aes(label = name), color = color, size = 4) +
    ggplot2::theme_void()
  
  # return
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


#' SEM analyses with piecewise - males final model high ttt
#'
#' @description piecewise adapted only to males for the high ttt after dsep
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
  
  # color label according to ttt
  color <- dplyr::case_when(target_ttt == "low" ~ "#43C59E",
                            target_ttt == "medium" ~ "#3D7068",
                            target_ttt == "high" ~ "#14453D")
  
  # filter data
  data_target <- data_sem_sampled_sessions %>%
    dplyr::filter(ttt == target_ttt & sex == target_sex) 
  
  # formula
  # formula1a <- stats::reformulate(c(target_traits,"PLA"), response = "POS") |> stats::update(. ~ . + (1|session))
  formula1b <- stats::reformulate(target_traits, response = "PLA") |> stats::update(. ~ . + (1|session))
  formula1c <- stats::reformulate(c(target_traits,"PLA"), response = "FLO") |> stats::update(. ~ . + (1|session))
  # formula1d <- stats::reformulate(target_traits, response = "VIS") |> stats::update(. ~ . + (1|session))
  formula2 <- stats::reformulate(c("PLA","FLO"), response = "MS") |> stats::update(. ~ . + (1|session))
  formula3 <- stats::reformulate(c("PLA","FLO",target_traits[1]), response = target_ps) |> stats::update(. ~ . + (1|session))
  formula4 <- stats::reformulate(c("MS", target_ps), response = target_sr) |> stats::update(. ~ . + (1|session))
 
   # build the psem
  psem_proxy <- piecewiseSEM::psem(
    # lme4::lmer(formula1a, data = data_target),
    lme4::lmer(formula1b, data = data_target),
    lme4::lmer(formula1c, data = data_target),
    # lme4::lmer(formula1d, data = data_target),
    lme4::lmer(formula2,  data = data_target),
    lme4::lmer(formula3,  data = data_target),
    lme4::lmer(formula4,  data = data_target)
    
  )
  
  # extract coefficients
  cf <- piecewiseSEM::coefs(psem_proxy)
  
  # create the graph
  g <- igraph::graph_from_data_frame(
    d = data.frame(from = cf$Predictor, to = cf$Response),
    directed = TRUE
  )
  
  # add colors and estimate
  edge_cols <- ifelse(cf$P.Value < 0.05, "black", "lightgrey")
  ggraph_edges <- data.frame(from = cf$Predictor, to = cf$Response,
                             estimate = round(cf$Std.Estimate, 2),
                             color = edge_cols) |>
    mutate(estimate = ifelse(color == "lightgrey","",estimate))
  
  # nodes coord
  coords <- data.frame(name = igraph::V(g)$name,
                       x = x_coord[1:length(igraph::V(g))],
                       y = y_coord[1:length(igraph::V(g))])
  
  # plot with ggraph
  p <- ggraph::ggraph(g, layout = 'manual', x = coords$x, y = coords$y) +
    # Arêtes avec couleur selon significativité
    ggraph::geom_edge_link(aes(color = I(ggraph_edges$color), label = ggraph_edges$estimate),hjust=-0.1) +
    # Noeuds
    ggraph::geom_node_point(color = color, size = 15, shape = 21, stroke = 1.5, fill = "white") +
    ggraph::geom_node_text(aes(label = name), color = color, size = 4) +
    ggplot2::theme_void()
  
  # return
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


#' SEM analyses with piecewise - females basic model
#'
#' @description piecewise adapted only to females
#'
#' @param data 
#'
#' @return 
#' 
#' @import dplyr 
#' 
#' @export

get_piecewise_females_visits <- function(data_sem_sampled_sessions, target_ttt = "low", target_sex = "fem",
                                         target_sr = "W", target_ps = "PB",
                                         target_traits = c("F","H"),
                                         x_coord = c(1,3,1.25,2.75,1.75,2.25,2),
                                         y_coord = c(1,1,2,2,3,3,4)) {
  
  # color label according to ttt
  color <- case_when(target_ttt == "low" ~ "#E6AA68",
                     target_ttt == "medium" ~ "#D36135",
                     target_ttt == "high" ~ "#9A4D1D")
  
  # filter data
  data_target <- data_sem_sampled_sessions %>%
    dplyr::filter(ttt == target_ttt & sex == target_sex)
  
  # formula
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
  
  # build the psem
  psem_proxy <- piecewiseSEM::psem(
    # lme4::lmer(formula1a, data = data_target),
      model1b,
      model1c,
      model2,
      model3,
      model4
  )
  
  # extract coefficients
  cf <- piecewiseSEM::coefs(psem_proxy)
  
  # create the graph
  g <- igraph::graph_from_data_frame(
    d = data.frame(from = cf$Predictor, to = cf$Response),
    directed = TRUE
  )
  
  # add colors and estimate
  edge_cols <- ifelse(cf$P.Value < 0.05, "black", "lightgrey")
  ggraph_edges <- data.frame(from = cf$Predictor, to = cf$Response,
                             estimate = round(cf$Std.Estimate, 2),
                             color = edge_cols) |>
    mutate(estimate = ifelse(color == "lightgrey","",estimate))
  
  # nodes coord
  coords <- data.frame(name = igraph::V(g)$name,
                       x = x_coord[1:length(igraph::V(g))],
                       y = y_coord[1:length(igraph::V(g))])
  
  # plot with ggraph
  p <- ggraph::ggraph(g, layout = 'manual', x = coords$x, y = coords$y) +
    ggraph::geom_edge_link(aes(color = I(ggraph_edges$color), label = ggraph_edges$estimate),hjust=-0.1) +
    ggraph::geom_node_point(color = color, size = 15, shape = 21, stroke = 1.5, fill = "white") +
    ggraph::geom_node_text(aes(label = name), color = color, size = 4) +
    ggplot2::theme_void()
  
  # return
  summary <- summary(psem_proxy)
  
  # dSep
  dsep <- piecewiseSEM::dSep(psem_proxy)
  
  return(list(summary = summary,
              dsep = dsep,
              plot = p, 
              coefs = cf))
}

#' SEM analyses with piecewise - females final model low ttt
#'
#' @description piecewise adapted only to mfeales for the low ttt after dsep
#'
#' @param data 
#'
#' @return 
#' 
#' @import dplyr 
#' 
#' @export

get_piecewise_females_visits_low <- function(data_sem_sampled_sessions, target_ttt = "low", target_sex = "fem",
                                         target_sr = "W", target_ps = "PB",
                                         target_traits = c("F","H"),
                                         x_coord = c(1,3,1.25,2.75,1.75,2.25,2),
                                         y_coord = c(1,1,2,2,3,3,4)) {
  
  # color label according to ttt
  color <- case_when(target_ttt == "low" ~ "#E6AA68",
                     target_ttt == "medium" ~ "#D36135",
                     target_ttt == "high" ~ "#9A4D1D")
  
  # filter data
  data_target <- data_sem_sampled_sessions %>%
    dplyr::filter(ttt == target_ttt & sex == target_sex)
  
  # formula
  # formula1a <- stats::reformulate(c(target_traits,"PLA"), response = "POS") |> stats::update(. ~ . + (1|session))
  formula1b <- stats::reformulate(target_traits, response = "PLA") |> stats::update(. ~ . + (1|session))
  formula1c <- stats::reformulate(c(target_traits,"PLA"), response = "FLO") |> stats::update(. ~ . + (1|session))
  # formula1d <- stats::reformulate(target_traits, response = "VIS") |> stats::update(. ~ . + (1|session))
  formula2 <- stats::reformulate(c("PLA","FLO"), response = "MS") |> stats::update(. ~ . + (1|session))
  formula3 <- stats::reformulate(c("PLA","FLO",target_traits[1]), response = target_ps) |> stats::update(. ~ . + (1|session))
  formula4 <- stats::reformulate(c(target_traits,"MS", target_ps, "FLO"), response = target_sr) |> stats::update(. ~ . + (1|session))
  
  # build the psem
  psem_proxy <- piecewiseSEM::psem(
    # lme4::lmer(formula1a, data = data_target),
    lme4::lmer(formula1b, data = data_target),
    lme4::lmer(formula1c, data = data_target),
    lme4::lmer(formula2,  data = data_target),
    lme4::lmer(formula3,  data = data_target),
    lme4::lmer(formula4,  data = data_target)
  )
  
  # extract coefficients
  cf <- piecewiseSEM::coefs(psem_proxy)
  
  # create the graph
  g <- igraph::graph_from_data_frame(
    d = data.frame(from = cf$Predictor, to = cf$Response),
    directed = TRUE
  )
  
  # add colors and estimate
  edge_cols <- ifelse(cf$P.Value < 0.05, "black", "lightgrey")
  ggraph_edges <- data.frame(from = cf$Predictor, to = cf$Response,
                             estimate = round(cf$Std.Estimate, 2),
                             color = edge_cols) |>
    mutate(estimate = ifelse(color == "lightgrey","",estimate))
  
  # nodes coord
  coords <- data.frame(name = igraph::V(g)$name,
                       x = x_coord[1:length(igraph::V(g))],
                       y = y_coord[1:length(igraph::V(g))])
  
  # plot with ggraph
  p <- ggraph::ggraph(g, layout = 'manual', x = coords$x, y = coords$y) +
    # Arêtes avec couleur selon significativité
    ggraph::geom_edge_link(aes(color = I(ggraph_edges$color), label = ggraph_edges$estimate),hjust=-0.1,vjust =0.1) +
    # Noeuds
    ggraph::geom_node_point(color = color, size = 15, shape = 21, stroke = 1.5, fill = "white") +
    ggraph::geom_node_text(aes(label = name), color = color, size = 4) +
    ggplot2::theme_void()
  
  # return
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
 
#' SEM analyses with piecewise - females final model medium ttt
#'
#' @description piecewise adapted only to females for the medium ttt after dsep
#'
#' @param data 
#'
#' @return 
#' 
#' @import dplyr 
#' 
#' @export

get_piecewise_females_visits_medium <- function(data_sem_sampled_sessions, target_ttt = "medium", target_sex = "fem",
                                             target_sr = "W", target_ps = "PB",
                                             target_traits = c("F","H"),
                                             x_coord = c(1,3,1.25,2.75,1.75,2.25,2),
                                             y_coord = c(1,1,2,2,3,3,4)) {
  # color label according to ttt
  color <- case_when(target_ttt == "low" ~ "#E6AA68",
                     target_ttt == "medium" ~ "#D36135",
                     target_ttt == "high" ~ "#9A4D1D")
  
  # filter data
  data_target <- data_sem_sampled_sessions %>%
    dplyr::filter(ttt == target_ttt & sex == target_sex)
  
  # formula
  # formula1a <- stats::reformulate(c(target_traits,"PLA"), response = "POS") |> stats::update(. ~ . + (1|session))
  formula1b <- stats::reformulate(target_traits, response = "PLA") |> stats::update(. ~ . + (1|session))
  formula1c <- stats::reformulate(c(target_traits,"PLA"), response = "FLO") |> stats::update(. ~ . + (1|session))
  # formula1d <- stats::reformulate(target_traits, response = "VIS") |> stats::update(. ~ . + (1|session))
  formula2 <- stats::reformulate(c("PLA","FLO",target_traits[2]), response = "MS") |> stats::update(. ~ . + (1|session))
  formula3 <- stats::reformulate(c("PLA","FLO",target_traits), response = target_ps) |> stats::update(. ~ . + (1|session))
  formula4 <- stats::reformulate(c(target_traits,"MS", target_ps), response = target_sr) |> stats::update(. ~ . + (1|session))
  
  # build the psem
  psem_proxy <- piecewiseSEM::psem(
    # lme4::lmer(formula1a, data = data_target),
    lme4::lmer(formula1b, data = data_target),
    lme4::lmer(formula1c, data = data_target),
    lme4::lmer(formula2,  data = data_target),
    lme4::lmer(formula3,  data = data_target),
    lme4::lmer(formula4,  data = data_target)
  )
  
  # extract coefficients
  cf <- piecewiseSEM::coefs(psem_proxy)
  
  # create the graph
  g <- igraph::graph_from_data_frame(
    d = data.frame(from = cf$Predictor, to = cf$Response),
    directed = TRUE
  )
  
  # add colors and estimate
  edge_cols <- ifelse(cf$P.Value < 0.05, "black", "lightgrey")
  ggraph_edges <- data.frame(from = cf$Predictor, to = cf$Response,
                             estimate = round(cf$Std.Estimate, 2),
                             color = edge_cols) |>
    mutate(estimate = ifelse(color == "lightgrey","",estimate))
  
  # nodes coord
  coords <- data.frame(name = igraph::V(g)$name,
                       x = x_coord[1:length(igraph::V(g))],
                       y = y_coord[1:length(igraph::V(g))])
  
  # plot with ggraph
  p <- ggraph::ggraph(g, layout = 'manual', x = coords$x, y = coords$y) +
    # Arêtes avec couleur selon significativité
    ggraph::geom_edge_link(aes(color = I(ggraph_edges$color), label = ggraph_edges$estimate),hjust=-0.2,vjust = 0.1) +
    # Noeuds
    ggraph::geom_node_point(color = color, size = 15, shape = 21, stroke = 1.5, fill = "white") +
    ggraph::geom_node_text(aes(label = name), color = color, size = 4) +
    ggplot2::theme_void()
  
  # return
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

#' SEM analyses with piecewise - females final model high ttt
#'
#' @description piecewise adapted only to females for the high ttt after dsep
#'
#' @param data 
#'
#' @return 
#' 
#' @import dplyr 
#' 
#' @export

get_piecewise_females_visits_high <- function(data_sem_sampled_sessions, target_ttt = "high", target_sex = "fem",
                                         target_sr = "W", target_ps = "PB",
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