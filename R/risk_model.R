
#' Get the list of models that can be loaded
#' @return list of models
get_model_list <- function(model_directory) {
    model_list <- list.files(model_directory)
}

#' Get the directory paths for scenario inputs and results
#'
#' @param model Name of scenario
#'
#' @return A list of directory paths
get_model_dirs <- function(model) {
    list (
        "inputs" = paste(model_directory, model, "/inputs", sep=""),
        "results" = paste(model_directory, model, "/results", sep="")
    )
}

#' Load and verify all associated files for a model
#'
#' @importFrom evaluator import_spreadsheet
#' @importFrom evaluator read_qualitative_inputs
#' @importFrom evaluator validate_scenarios
#' @param model Name of the model
#'
#' @return A list of input elements
load_model <- function(model) {
    dirs <- get_model_dirs(model)

    domains <- read_csv(
        file.path(dirs$inputs, "domains.csv"),
        col_types = cols(.default = col_character()))

    import_spreadsheet(
        file.path(dirs$inputs, "survey.xlsx"),
        domains,
        dirs$inputs)

    qual_inputs <- read_qualitative_inputs(dirs$inputs)
    qualitative_scenarios <- qual_inputs$qualitative_scenarios
    mappings <- qual_inputs$mappings
    capabilities <- qual_inputs$capabilities
    validate_scenarios(qualitative_scenarios, capabilities, domains, mappings)

    model_data <- list("domains" = domains,
                       "qualitative_scenarios" = qualitative_scenarios,
                       "mappings" = mappings,
                       "capabilities" = capabilities,
                       "inputs_dir" = dirs$inputs,
                       "results_dir" = dirs$results)

    return(model_data)
}

#' Convert qualitative scenarios to quantitative scenarios
#' * Saves quantitative values to disk
#'
#' @importFrom evaluator encode_scenarios
#' @param scenario_data List containing qualitative scenarios, capabilities, and mappings
#'
#' @return quantitative scenarios
encode_model_data <- function(model_data) {
    quantitative_scenarios <- encode_scenarios(
        model_data$qualitative_scenarios,
        model_data$capabilities,
        model_data$mappings)

    saveRDS(quantitative_scenarios,
            file = file.path(model_data$inputs_dir, "quantitative_scenarios.rds"))

    return(quantitative_scenarios)
}

#' Run simulation for each quantitative scenario
#'
#' @importFrom evaluator run_simulation
#' @param quantitative_scenarios
#' @param iterations
#'
#' @return simulation results
run_multiple_simulation <- function(quantitative_scenarios, iterations = 10000L) {
    simulation_results <- quantitative_scenarios %>%
        mutate(results = furrr::future_map(scenario, run_simulation,
                                           iterations = iterations,
                                           .progress = TRUE)) %>%
        select(-c(scenario, tcomm, scenario_description), scenario_id, domain_id, results)

    return(simulation_results)
}

#' Save the simulation results including a summarization
#'
#' @importFrom evaluator summarize_to_disk
#' @param simulation_results
#' @param results_dir
save_simulation_results <- function(simulation_results, results_dir) {
    saveRDS(simulation_results, file = file.path(results_dir, "simulation_results.rds"))
    summarize_to_disk(simulation_results = simulation_results, results_dir)
}

#' Load previous simulation results
#'
#' @importFrom evaluator encode_scenario_data
#' @param model Name of the model
#'
#' @return results
load_simulation_model <- function(model) {
    dirs <- get_model_dirs(model)

    model_data <- load_model(model)
    quantitative_scenarios <- encode_model_data(model_data)
    simulation_results <- readRDS(file.path(dirs$results, "simulation_results.rds"))

    model_results <- list(
        "model_data" = model_data,
        "quantitative_scenarios" = quantitative_scenarios,
        "simulation_results" = simulation_results
    )

    results <- summarize_model_simulation(model_results)
    return(results)
}

#' Run all scenarios in the model and save the simulation results
#'
#' @param model name of the model to run
#' @param iterations number of iterations to run
#'
#' @return model_results
run_model_simulation <- function(model, iterations) {
    model_data <- load_model(model)
    quantitative_scenarios <- encode_model_data(model_data)
    simulation_results <- run_multiple_simulation(quantitative_scenarios, iterations=iterations)

    save_simulation_results(simulation_results, model_data$results_dir)

    model_results <- list(
        "model_data" = model_data,
        "quantitative_scenarios" = quantitative_scenarios,
        "simulation_results" = simulation_results
    )
    return(model_results)
}

#' Create summaries of the model simulation results
#'
#' @param model_results result from running model simulation
#'
#' @return results
summarize_model_simulation <-function(model_results) {
    simulation_results <- model_results$simulation_results
    domains <- model_results$model_data$domains

    scenario_summary <- summarize_scenarios(simulation_results)

    scenario_outliers <- identify_outliers(scenario_summary) %>%
        filter(outlier == TRUE) %>% pull(scenario_id)

    iteration_summary <- summarize_iterations(simulation_results$results)

    domain_summary <- summarize_domains(simulation_results)
    domain_summary <- domain_summary %>% left_join(domains, by = "domain_id")

    simulation_results_unnested <- unnest(simulation_results, results) %>%
        mutate(outlier = scenario_id %in% scenario_outliers)

    risk_tolerances <-
        readr::read_csv(
                   file.path(model_results$model_data$inputs_dir, "risk_tolerances.csv"),
                   col_types = readr::cols(level = readr::col_character(),
                                           amount = readr::col_integer()))

    summarise_results <- list(
        "iteration_summary" = iteration_summary,
        "risk_tolerances" = risk_tolerances,
        "domain_summary" = domain_summary,
        "scenario_summary" = scenario_summary,
        "scenarios" = model_results$quantitative_scenarios)

    results <- list(
        "model_results" = model_results,
        "summarise_results" = summarise_results)

    return(results)
}
