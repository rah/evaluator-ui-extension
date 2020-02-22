#' Simple web app to demonstrate FAIR analysis
#'
#' @param host set to 0.0.0.0 for public access
#' @param intermediates_dir location for intermediate knit files
#' @param quiet 'TRUE' to suppress printing of pandoc output
#'
#' @return Invisible NULL
#' @examples
#' \dontrun {
#' fair_model()
#' }
#'
fair_model <- function(host = "127.0.0.1",
                       intermediates_dir = tempdir(),
                       quite = TRUE) {

    rmarkdown::run("rmd/fair_model.Rmd",
                   render_args = list(
                       intermediates_dir = intermediates_dir,
                       quiet = quiet),
                   shiny_args = list(host = host)
                   )
    invisible(NULL)
}
