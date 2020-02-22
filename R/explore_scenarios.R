library(extrafont)
library(purr)
library(tidyverse)

# source("R/explore_scenarios_helper.R")

#' main function to invoke UI
explore_scenarios <- function(model,
                              host = "127.0.0.1",
                              intermediates_dir = tempdir(),
                              quite = TRUE) {

    if (nchar(model) == 0) {
        stop("Model must no be empty")
    }

    rmarkdown::run("rmd/explore_scenarios.Rmd",
                   render_args = list(
                       intermediates_dir = intermediates_dir,
                       quiet = quiet,
                       params = list(
                           model = model
                       )
                   ),
                   shiny_args = list(host = host)
                   )
    invisible(NULL)
}
