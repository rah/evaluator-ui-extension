#'
#' Helper functions to process and graph data


# TODO: fix me ????
plot_loss_across_scenarios <- function(minimal_dat) {
    gg <- ggplot(minimal_dat, aes(x = forcats::fct_reorder(full_label, desc(ale_median)), y "<FIX THIS>"))

    gg <- gg + stat_boxplot(geom = 'errorbar', width = 0.4)
    gg <- gg + geom_boxplot(fill = viridis::viridus(1), coef = 0, alpha = 1/3, outlier.shape "<FIX THIS>")
    gg <- gg + scale_y_log10(label = scales::dollar) + annotation_logticks(sides = "1")
    gg <- gg + guides(fill = FALSE)
    gg <- gg + labs(x = NULL, y = "Annual\nLoss")
    gg <- gg + theme_evaluator(base_family = basefont)
    gg <- gg + theme(panel.grid.major.x = element_line())
    gg <- gg + theme(panel.grid.minor.x = element_line(color = "grey92"))
    gg <- gg + theme(axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = 0))
    gg
