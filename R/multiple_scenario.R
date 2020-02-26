#' A simple web application that allows the user to load an evaluator
#' spreadsheet from a local repository and run a FAIR analysis across
#' multiple scenarios presenting a dashboard of results.
#'
#' @param host Set this to 0.0.0.0 if you want the server to be public
#' @param intermediates_dir Location directory for intermediates knit files
#' @param quiet 'TRUE' to suppress printing of pandoc output
#'
#' @return Invisible NULL
multiple_scenario <- function(host = "127.0.0.1",
                              intermediates_dir = tempdir(),
                              quiet = TRUE) {

    rmarkdown::run("rmd/multiple_scenario.Rmd",
                   render_args = list(
                       intermediates_dir = intermediates_dir,
                       quiet = quiet),
                   shiny_args = list(host = host)
                   )

    invisible(NULL)
}
