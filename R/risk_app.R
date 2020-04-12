#' A simple web application that demonstrates OpenFAIR modelling. This application
#' allows the user to enter beta PERT parameters and run simulations to see the
#' distribution of results, with high level summary statistics. As a demonstration
#' application, only TEF, TC, DIFF, and LM parameters may be entered.
single_scenario_app <- "single_scenario"

#' A simple web application that allows the user to load an evaluator
#' spreadsheet from a local repository and run a FAIR analysis across
#' multiple scenarios presenting a dashboard of results.
multiple_scenario_app <- "multiple_scenario"

explore_scenarios_app <- "explore_scenarios"

#' Replaces the multiple scenario app
fair_model_app <- "fair_model"

default_app_name <- fair_model_app
default_model_directory <- "~/workspace/risk-analysis-ui/model/"


#'
#' @param app_name Name of the application to run
#' @param input_directory
#' @param results_directory
#' @param intermediates_dir Location directory for intermediates knit files
#' @param quiet 'TRUE' to suppress printing of pandoc output
#' @param host Set this to 0.0.0.0 if you want the server to be public
#'
#' @return Invisible NULL
risk_model_app <- function(app_name = default_app_name,
                           model_directory = default_model_directory,
                           intermediates_dir = tempdir(),
                           quiet = TRUE,
                           host = "127.0.0.1") {

    styles <- system.file("rmd", "styles", "html-styles.css", package = "evaluator")
    icon <- system.file("rmd", "img", "evaluator_hex_48px.png", package = "evaluator")

    rmd_file <- paste("./rmd/", app_name, ".Rmd", sep="")

    rmarkdown::run(rmd_file,
                   render_args = list(
                       output_options = list(
                           css = styles,
                           favicon = icon,
                           logo = icon),
                       intermediates_dir = intermediates_dir,
                       params = list(model_directory = model_directory),
                       quiet = quiet
                   ),
                   shiny_args = list(host = host)
                   )
    invisible(NULL)
}

risk_model_app()
