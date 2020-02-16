#'
#' @param input_directory
#' @param results_directory
#' @param output_file
#' @param intermediate_dir
#' @param quiet
#' @param ...
#'
#' @return Default values of the \code{rmarkdown::render} function
#' @export
#' @examples
#' \dontrun {
#' threat_model_dashboard("~/inputs", "~/simulations")
#' )

threat_model_simulation <- function(input_directory = "~/evaluator/inputs",
                                    results_directory = "~/evaluator/results",
                                    output_file = NULL,
                                    intermediates_dir = tempdir(),
                                    quiet = TRUE,
                                    ...) {
    styles <- system.file("rmd", "styles", "html-styles.css", package = "evaluator")
    icon <- system.file("rmd", "img", "evaluator_hex_48px.png", package = "evaluator")

    rmarkdown::run("rmd/threat_model_dashboard.Rmd",
                   render_args = list(
                       output_options = list(
                           css = styles,
                           favicon = icon,
                           logo = icon),
                       output_file = output_file,
                       intermediates_dir = intermediates_dir,
                       params = list(
                           input_directory = input_directory,
                           results_directory = result_directory),
                       quite = quiet,
                       ...
                   )
                   )

}
