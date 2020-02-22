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
    scenario_list <- paste(scenario_sumary$domain_id, "-", scenario_summary$scenario_id)
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
        filter(scenario_id == sid) %>% select(-c(results, control_descriptions))

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

    names(summary_data) <- stringi::stri_trans_tototle(gsub("_", " ", names(summary_data)))
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
    control_data <- as_tibble(scenario_data) %>% filter(openfair_factor == "diff") %>%
        left_join(capabilities, by = c("id" = "capability_id"))

    # format percentages
    control_data <- mutate_at(control_data, vars(min, mode, max), list(percent))

    # display
    control_data %>% select(Control = capability,
                            Low = min, "Most Likely" = mode, High = max,
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

#' plot histogram and density distribution of annual loss events
#'
#' @param scenario
#' @param simulation_results
#'
#' @return plot
plot_scenario <- function(scenario, simulation_results) {
    scenario_sim_results <- simulation_results %>% unnest(results) %>%
        filter(scenario_id == get_scenario_id(scenario))

    gg <- ggplot(scenario_sim_results, aes(x = ale))

    # make sure the binwidth is postive
    binwidth <- diff(range(scenario_sim_results$ale) / 50)
    if (binwidth > 0) {
        gg <- gg + geom_histogram(binwidth = binwidth,
                                  aes(y = ..density..),
                                  color = "black",
                                  fill = "white")
        }

    gg <- gg + geom_density(fill = "steelblue", alpha = 1/3)
    gg <- gg + scale_x_continuous(labels = comma)
    gg <- gg + labs(x = "Annual Expacted Losses")
    gg <- gg + theme_evaluator(base_family = basefont)
    print(gg)
}
