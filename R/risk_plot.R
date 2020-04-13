
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

    return(gg)
}

#' Default Loss Exceedance Tolerance
default_tolerance <- data.frame(x = c(100000, 2000000, 10000000, 50000000),
                                y = c(0.99,   0.25,    0.10,     0.005   ))

#' Display the loss exceedance curve for a group of one or more scenarios
#' Modified from Evaluator toolkit \code{loss_exceedance_curve}
#'
#' @import ggplot2
#' @importFrom dplyr arrange mutate percent_rank
#' @importFrom scales percent dollar
#' @importFrom rlang .data
#'
#' @param iteration_results iteration level summary from \code{summarize_iterations}.
#'
#' @return A ggplot object
plot_loss_exceedance_curve <- function(iteration_results, loss_tolerance = default_tolerance) {
    gg <- arrange(iteration_results, .data$max_loss) %>%
        mutate(prob = 1 - percent_rank(.data$max_loss)) %>%
        ggplot(aes(.data$max_loss, .data$prob))
    gg <- gg + geom_path()

    # add loss tolerance
    gg <- gg + geom_line(data = loss_tolerance, aes(x, y),
                         linetype = "dashed", color = "red")

    #set 80% threshold line
    gg <- gg + geom_hline(yintercept = 0.8, color = "red", size = 1)
    gg <- gg + annotate("text", y = 0.83, x = max(iteration_results$max_loss),
                        label = percent(.8, accuracy = 1), hjust = "right")

    gg <- gg + scale_x_continuous(labels = scales::dollar)
    gg <- gg + scale_y_continuous(labels = scales::percent)
    gg <- gg + theme_evaluator(base_family = get_base_fontfamily())
    gg <- gg + theme(panel.grid.minor = element_blank())
    gg <- gg + theme(panel.grid.major = element_blank())
    gg <- gg + labs(y = "Chance of Equal or Greater Loss", x = "Loss",
                    title = "Loss Exceedance Curve",
                    caption = "Source: Derived from Evaluator toolkit")

    return(gg)
}

#' Plot loss across a set of scenarios
#'
#' @param dat the scenarios to plot
#' @return ggplot
plot_loss_across_scenarios <- function(dat) {
    gg <- ggplot(dat, aes(x = forcats::fct_reorder(full_label, desc(ale_median)), y = ale + 1))

    gg <- gg + stat_boxplot(geom = 'errorbar', width = 0.4)
    gg <- gg + geom_boxplot(fill = viridis::viridis(1), coef = 0, alpha = 1/3, outlier.shape =NA)
    gg <- gg + scale_y_log10(label = scales::dollar) + annotation_logticks(sides = "1")
    gg <- gg + guides(fill = FALSE)
    gg <- gg + labs(x = NULL, y = "Annual\nLoss")
    gg <- gg + theme_evaluator(base_family = basefont)
    gg <- gg + theme(panel.grid.major.x = element_line())
    gg <- gg + theme(panel.grid.minor.x = element_line(color = "grey92"))
    gg <- gg + theme(axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = 0))

    return(gg)
}
