#' Helper functions

#' determine the correct base font family
basefont <- get_base_fontfamily()
model_dir <- "~/workspace/RISK/risk_analysis/model/multiple"

#' Given a domain id - scenario_id syntax, get the scenario id
#'
#' @param x combination of domain id - scenario id
#'
#' @return scenario id
get_scenario_id <- function(x) {
    str_split(x, " - ", simplify = TRUE)[2]
}

#' Get list of scenarios
get_scenarios <- function(scenario_summary) {
    scenario_list <- paste(scenario_summary$domain_id, "-", scenario_summary$scenario_id)
}

#' Get scenario description
#'
#' @param input_scenario combination of "domain id - scenario id"
#' @param scenarios list of scenarios
#'
#' @return the scenario description
get_scenario_description <- function(input_scenario, scenarios) {
    description <- scenarios[scenarios$scenario_id == get_scenario_id(input_scenario),
                             "scenario_description"][[1]]
}

#' Get threat actor for a threat scenario
#'
#' @param input_scenario combination of domain id - scenario id
#' @param scenarios list of scenarios
#'
#' @return the threat actor
get_threat_actor <- function(input_scenario, scenarios) {
    threat_actor <- scenarios[scenarios$scenario_id == get_scenario_id(input_scenario), "tcomm"]
}

#' Get the results dataframe
#' TODO check if this is used anywhere
#'
#' @param sid scenario id
#' @param simulation_results dataframe
#'
#' @return results dataframe
get_scenario_results <- function(sid, simulation_results) {
    simulation_results %>% filter(scenario_id == sid) %>%
        pull(results) %>% .[[1]]
}

#' Get the scenario summary table
#'
#' @param sid scenario id
#' @param scenario_summary
#'
#' @return summary table for display
get_summary_table <- function(sid, scenario_summary) {
    summary_data <- scenario_summary %>%
        filter(scenario_id == sid) %>%
        select(-c(results, control_descriptions))

    # add pretty formatting
    summary_data <- mutate_at(summary_data, .funs = dollar,
                              .vars = vars(ale_median, ale_max, ale_var, sle_mean,
                                           sle_median, sle_max, sle_min)) %>%
        mutate(mean_tc_exceedance = ifelse(is.nan(mean_tc_exceedance),
                                           NA,
                                           percent(mean_tc_exceedance))) %>%
        mutate(mean_vuln = ifelse(is.nan(mean_vuln),
                                  NA,
                                  percent(mean_vuln)))

    names(summary_data) <- stringi::stri_trans_totitle(gsub("_", " ", names(summary_data)))
    summary_data <- summary_data %>% mutate_all(as.character) %>%
        tidyr::gather(key = "Parameter", value = "Value")
    summary_data
}

#' Get loss table from simulation results
#'
#' @param sid scenario id
#' @param simulation_results dataframe
#'
#' @return loss table
get_loss_table <- function(sid, simulation_results) {
    scenario_data <- simulation_results[simulation_results$scenario_id == sid, ][[1, "results"]]
    loss_table <- tibble::tibble(Category = c("Loss Events / Year", "Loss Magnitude",
                                              "Total Loss Exposure"),
                                 Minimum = c(min(scenario_data$loss_events),
                                             min(scenario_data$sle_min) %>% dollar,
                                             min(scenario_data$ale) %>% dollar),
                                 Mean = c(mean(scenario_data$loss_events, na.rm = TRUE),
                                          mean(scenario_data$sle_mean, na.rm = TRUE) %>% dollar,
                                          mean(scenario_data$ale, na.rm = TRUE) %>% dollar),
                                 Mode = c(statip::mfv(scenario_data$loss_events)[1],
                                          statip::mfv(scenario_data$sle_median)[1] %>% as.numeric %>% dollar,
                                          statip::mfv(scenario_data$ale)[1] %>% as.numeric %>% dollar),
                                 Maximum = c(max(scenario_data$loss_events),
                                             max(scenario_data$sle_max) %>% dollar,
                                             max(scenario_data$ale) %>% dollar)
                                 )
    return(loss_table)
}

#' Get scenario data
#'
#' @param scenario scenario
#' @param scenarios dataframe
#'
#' @return scenario data
get_scenario_data <- function(scenario, scenarios) {
    scenario_data <- scenarios[scenarios$scenario_id == get_scenario_id(scenario), ][[1, "scenario"]]
}

#' Get threat summary table
#'
#' @param scenario
#' @param simulation_results dataframe
#'
#' @return threat summary table
get_threat_table <- function(scenario, scenarios) {
    scenario_data <- get_scenario_data(scenario, scenarios)

    threat_data <- as_tibble(scenario_data) %>%
        filter(openfair_factor %in% c("tef", "tc")) %>%
        tidyr::gather(param, value, c(min:shape)) %>%
        mutate(value = ifelse(openfair_factor == "tef" | param == "shape",
        (value), percent(value))) %>%
        tidyr::spread(param, value) %>%
        mutate(type = ifelse(openfair_factor == "tc", "Capability", "Frequency"))

    threat_data <- threat_data %>% select(Type = openfair_factor, Low = min,
                                          "Most Likely" = mode,
                                          "High" = max, Confidence = shape)
}

#' Populate the control summary table
#'
#' @param scenario scenario
#' @param scenarios dataframe
#' @param capabilities dataframe
#'
#' @return control summary table
get_control_table <- function(scenario, scenarios, capabilities) {
    scenario_data <- get_scenario_data(scenario, scenarios)

    # add control description
    control_data <- as_tibble(scenario_data) %>%
        filter(openfair_factor == "diff") %>%
        left_join(capabilities, by = c("id" = "capability_id"))

    # format percentages
    control_data <- mutate_at(control_data, vars(min, mode, max), list(percent))

    # display
    control_data <- control_data %>% select(Control = capability,
                                            Low = min,
                                            "Most Likely" = mode,
                                            High = max,
                                            Confidence = shape)
}

#' Get loss summary table
#'
#' @param scenario scenario
#' @param scenarios
#'
#' @return loss data
get_loss_distribution_table <- function(scenario, scenarios) {
    scenario_data <- get_scenario_data(scenario, scenarios)
    loss_data <- as_tibble(scenario_data) %>%
        filter(openfair_factor == "lm") %>%
        mutate_at(vars(min, mode, max), dollar) %>%
        select(Low = min, "Most Likely" = mode, "High" = max, Confidence = shape)
}

#' Roll up scenarios that are not in top n
#'
#' @param dat
#' @param top_n
#' @param sortby
#'
#' @return
sum_bottom_n <- function(dat, top_n=10, sortby="max_ale") {
    cluster_hack <- max(dat$cluster_id)
    full_label <- "full_label"
    sortby <- rlang::enquo(sortby)

    # assign a flag on outliers
    keep_nokeep <- dat %>% group_by_at(.vars = full_label) %>%
        summarise(foo = max(!!sortby)) %>%
        mutate(keep = ifelse(row_number() <= top_n, TRUE, FALSE)) %>%
        select(-foo)

    # sum up the outliers
    others <- filter(keep_nokeep, keep == FALSE) %>%
        left_join(dat, by = "full_label") %>%
        group_by(iteration) %>% summarize(ale = max(ale), scenario_count = n()) %>%
        mutate(full_label = paste0("others (", scenario_count, ")")) %>%
        mutate(cluster_id = cluster_hack)

    # combine the outlier with the top n items
    union_all(filter(keep_nokeep, keep == TRUE) %>%
              left_join(dat, by = "full_label"), others)
}

#' Cluster scenario losses
#'
#' @param simulation_results
#'
#' @return minimal clustered data
#'
cluster_scenario_loss <- function(simulation_results) {
    # assign the text label to scenarios and order
    simulation_results %>% unnest(results) %>%
        mutate(full_label = paste0(domain_id, " - ", scenario_id)) %>%
        arrange(domain_id, scenario_id) -> dat

    #identify the top 10 scenarios bu VaR
    top_n_scenarios <- group_by(dat, full_label) %>%
        summarize(ale_var = quantile(ale, 0.95)) %>%
        filter(row_number(desc(ale_var)) <= 10) %>%
        .[["full_label"]]

    # remove no loss scenarios, enhance and cluster
    dat <- group_by(dat, full_label) %>% filter(sum(ale) != 0) %>%
        mutate(var = quantile(ale, 0.95), max_ale = max(ale), ale_median = median(ale)) %>%
        ungroup %>%
        mutate(cluster_id = kmeans(max_ale, 3)$cluster)

    # assign labels to the clusters
    text_labels <- c("High", "Median", "Low")
    cluster_descriptions <- group_by(dat, cluster_id, full_label) %>%
        summarize(ale = max(max_ale)) %>%
        summarize(highest_ale = max(ale), n = n()) %>%
        arrange(desc(highest_ale)) %>%
        mutate(cluster_labels = factor(text_labels, levels = text_labels),
               top_n = min(last(n), n))

    # roll up on a per-cluster basis, adding descriptive text to the cluster
    summed_dat <- group_by(dat, cluster_id) %>%
        left_join(cluster_descriptions, by = "cluster_id") %>%
        do(sum_bottom_n(., top_n = max(.$top_n))) %>%
        select(-c(cluster_labels, n, top_n, highest_ale)) %>%
        left_join(cluster_descriptions, by = "cluster_id") %>%
        ungroup

    summed_dat <- group_by(summed_dat, full_label) %>% mutate(ale_median = median(ale))

    # slimmed down alternative view of the dataset
    minimal_dat <- filter(dat, full_label %in% top_n_scenarios, ale != 0) %>%
        group_by(full_label) %>%
        mutate(ale_median = median(ale)) %>% ungroup %>%
        mutate(full_label = forcats::fct_reorder(full_label, ale_median))

}
