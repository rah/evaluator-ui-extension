#' A simple web application that demonstrates OpenFAIR modelling. This application
#' allows the user to enter beta PERT parameters and run simulations to see the
#' distribution of results, with high level summary statistics. As a demonstration
#' application, only TEF, TC, DIFF, and LM parameters may be entered.
#'
#' @param host Set this to 0.0.0.0 if you want the server to be public
#' @param intermediates_dir Location directory for intermediates knit files
#' @param quiet 'TRUE' to suppress printing of pandoc output
#'
#' @return Invisible NULL
single_scenario <- function(host = "127.0.0.1",
                            intermediates_dir = tempdir(),
                            quiet = TRUE) {

    rmarkdown::run("rmd/single_scenario.Rmd",
                   render_args = list(
                       intermediates_dir = intermediates_dir,
                       quiet = quiet),
                   shiny_args = list(host = host)
                   )

    invisible(NULL)
}
