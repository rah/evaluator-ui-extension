#'
#' Helper functions to process and graph data

#' Roll up scenarios that are not in top n
#'
#' @param dat
#' @param top_n
#' @param sortby
#'
#' @return
sum_bottom_n <- function(dat, top_n=10, sortby="max_ale") {
    cluster_hack <- max(dat$cluster_id)
    full_label <- "full_label"
    sortby <- rlang::enquo(sortby)

    # assign a flag on outliers
    keep_nokeep <- dat %>% group_by_at(.vars = full_label) %>%
        summarise(foo = max(!!sortby)) %>%
        mutate(keep = ifelse(row_number() <= top_n, TRUE, FALSE)) %>%
        select(-foo)

    # sum up the outliers
    others <- filter(keep_nokeep, keep == FALSE) %>%
        left_join(dat, by = "full_label") %>%
        group_by(iteration) %>% summarize(ale = max(ale), scenario_count = n()) %>%
        mutate(full_label = paste0("others (", scenario_count, ")")) %>%
        mutate(cluster_id = cluster_hack)

    # combine the outlier with the top n items
    union_all(filter(keep_nokeep, keep == TRUE) %>%
              left_join(dat, by = "full_label"), others)
}

cluster_scenario_loss <- function(simulation_results) {
    # assign the text label to scenarios and order
    simulation_results %>% unnest(results) %>%
        mutate(full_label = paste0(domain_id, " - ", scenario_id)) %>%
        arrange(domain_id, scenario_id) -> dat

    #identify the top 10 scenarios bu VaR
    top_n_scenarios <- group_by(dat, full_label) %>%
        summarize(ale_var = quantile(ale, 0.95)) %>%
        filter(row_number(desc(ale_var)) <= 10) %>%
        .[["full_label"]]

    # remove no loss scenarios, enhance and cluster
    dat <- group_by(dat, full_label) %>% filter(sum(ale) != 0) %>%
        mutate(var = quantile(ale, 0.95), max_ale = max(ale), ale_median = median(ale)) %>%
        ungroup %>%
        mutate(cluster_id = kmeans(max_ale, 3)$cluster)

    # assign lables to the clusters
    text_labels <- c("High", "Median", "Low")
    cluster_descriptions <- group_by(dat, cluster_id, full_label) %>%
        summarize(ale = max(max_ale)) %>%
        summarize(highest_ale = max(ale), n = n()) %>%
        arrange(desc(highest_ale)) %>%
        mutate(cluster_labels = factor(text_lables, levels = text_labels),
               top_n = min(last(n), n))

    # roll up on a per-cluster basis, adding descriptive text to the cluster
    summed_dat <- group_by(dat, cluster_id) %>%
        left_join(cluster_descriptions, by = "cluster_id") %>%
        do(sum_bottom_n(., top_n = max(.$top_n))) %>%
        select(-c(cluster_labels, n, top_n, highest_ale)) %>%
        left_join(cluster_descriptions, by = "cluster_id") %>%
        ungroup

    summed_dat <- group_by(summed_dat, full_label) %>% mutate(ale_median = median(ale))

    # slimmed down alternative view of the dataset
    minimal_dat <- filter(dat, full_label %in% top_n_scenarios, ale != 0) %>%
        group_by(full_label) %>%
        mutate(ale_median = median(ale)) %>% ungroup %>%
        mutate(full_label = forcats::fct_reorder(full_label, ale_median))

}

# TODO: fix me ????
plot_loss_across_scenarios <- function(minimal_dat) {
    gg <- ggplot(minimal_dat, aes(x = forcats::fct_reorder(full_label, desc(ale_median)), y ????))

    gg <- gg + stat_boxplot(geom = 'errorbar', width = 0.4)
    gg <- gg + geom_boxplot(fill = viridis::viridus(1), coef = 0, alpha = 1/3, outlier.shape ????)
    gg <- gg + scale_y_log10(label = scales::dollar) + annotation_logticks(sides = "1")
    gg <- gg + guides(fill = FALSE)
    gg <- gg + labs(x = NULL, y = "Annual\nLoss")
    gg <- gg + theme_evaluator(base_family = basefont)
    gg <- gg + theme(panel.grid.major.x = element_line())
    gg <- gg + theme(panel.grid.minor.x = element_line(color = "grey92"))
    gg <- gg + theme(axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = 0))
    gg
