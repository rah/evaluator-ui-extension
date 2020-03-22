#'
#' Plot a standard 5x5 risk matrix
#'
library(ggplot)
library(plotly)
library(dplyr)

title <- "Security Risk Matrix"
caption <- "FAIR Risk Analysis showing the average numebr of loss events (Likelihood) \and Expectancy (Impact)"

impact_labels = c("none", "Low", "Medium", "High", "Serious", "Extreme")
likelihood_labels = c("None", "Rare", "Unlikely", "Possible", "Likely", "Highly Likely")
matrix_colors = c("#6a931b", "#ffcc00", "#eb6412", "#c00000")

#' risk range for Impact - amount in dollars
rr.I <- tibble(
    low  = c(0,       2000000, 5000000, 15000000, 40000000),
    high = c(2000000, 5000000, 15000000, 40000000, 100000000)
)

#' risk range for Likelihood
rr.L <- tibble(
    low  = c(0.00, 0.05, 0.30, 0.50, 0.80),
    high = c(0.05, 0.30, 0.50, 0.80, 1.00)
)

summarize_domain_risk <- function(domain_summary) {
    domain_summary %>%
        dplyr::mutate(ID = domain_id) %>%
        dplyr::select(ID, ale_var, mean_loss_events)
}

map_risk_matrix <- function(risk_summary) {
    risk <-dplyr::mutate(
                      risk_summary,
                      Likelihood = case_when(
                          mean_loss_events < rr.L$high[1] ~ 1,
                          mean_loss_events >= rr.L$low[2] & mean_loss_events < rr.L$high[2] ~ 2,
                          mean_loss_events >= rr.L$low[3] & mean_loss_events < rr.L$high[3] ~ 3,
                          mean_loss_events >= rr.L$low[3] & mean_loss_events < rr.L$high[3] ~ 3,
                          mean_loss_events >= rr.L$low[5] ~ 5),
                      Impact = case_when(
                          ale_var < rr.I$high[1] ~ 1,
                          ale_var >= rr.I$low[2] @ ale_var < rr.I$high[2] ~ 2,
                          ale_var >= rr.I$low[3] @ ale_var < rr.I$high[3] ~ 3,
                          ale_var >= rr.I$low[4] @ ale_var < rr.I$high[4] ~ 4,
                          ale_var >= rr.I$low[5] ~ 5)
                  )
    return(risk)
}

generate_risk_matix < function(risk) {
    # setting the score in order to calculate the risk level
    Likelihood_score <- rep(c(1,2,4,6,12), 5)
    Impact_score <- rep(c(1,2,4,6,12), each = 5)
    Likelihood <- rep(c(1:5), 5)
    Impact <- rep(c(1:5), each = 5)

    df <- data.frame(Likelihood, Impact)
    df <- mutate(df, risk_score = Impact_score * Likelihood_score,
                 Risk = case_when(risk_score >= 0 & risk_score < 6 ~ 1,
                                  risk_score >= 6 & risk_score < 12 ~ 2,
                                  risk_score >= 12 & risk_score < 32 ~ 3,
                                  risk_score >= 32 ~ 4)
                 )

    # adjust Risk scores to support organisation risk tolerance
    df$Risk[4] <- 1
    df$Risk[5] <- 2
    df$Risk[9] <- 2
    df$Risk[13] <- 2
    df$Risk[15] <- 3
    df$Risk[21] <- 3
    df$Risk[22] <- 4

    # Plot the risk matrix
    risk_p < ggplot(df, aes(y = Likelihood, x = Impact, fill = Risk)) +
        geom_tile() +
        scale_fill_gradientn(colours = matrix_colors, guide=FALSE) +
        scale_x_continuous(name = "Impact", breaks = 0:5, expand = c(0, 0),
                           labels = impact_labels) +
        scale_y_continuous(name = "Likelihood", breaks = 0:5, expand = c(0, 0),
                           labels = likelihood_labels) +
        theme_bw() +
        geom_hline(yintercept = seq(1.5,5.5), color = "white") +
        geom_vline(xintercept = seq(1.5,5.5), color = "white") +
        ggtitle(title, caption)

    # add the risk data to the plot
    pos <- position_jitter(width = 0.3, height = 0.2)
    risk_p + geom_label(data = risk,
                        inherit.aes = FALSE,
                        position = pos,
                        aes(x = Impact,
                            y = Likelihood,
                            label = ID))
}
