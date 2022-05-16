library(tidyverse)
library(tidyquant)

x <- tibble(
    MD                = "A", 
    MONTH             = month.name, 
    BILLING_COMPL_PCT = runif(12, min = .5), 
    NOTE_COMPL_PCT    = runif(12, min = .5)
)

y <- x %>% 
    pivot_longer(
        cols = 3:4, 
        names_to = "DOCUMENTATION_TYPE", 
        values_to = "COMPL_PCT"
    )


z <- y %>% 
    # slice(1:2) %>% 
    mutate(MONTH = factor(MONTH, levels = month.name, ordered = TRUE)) %>% 
    group_by(MD, MONTH) %>% 
    summarize(COMPL_PCT = mean(COMPL_PCT)) %>% 
    ungroup() %>% 
    mutate(DOCUMENTATION_TYPE = "BOTH")

y <- y %>%
    mutate(MONTH = factor(MONTH, levels = month.name, ordered = TRUE))

y %>% 
    ggplot(mapping = aes(x = MONTH, y = COMPL_PCT, fill = DOCUMENTATION_TYPE)) +
    geom_hline(yintercept = 0.9, linetype = "dashed") +
    geom_col(position = "dodge") + 
    geom_text(
        mapping = aes(label = scales::percent(COMPL_PCT, accuracy = .01), angle = 90), 
        position = position_dodge(0.9), hjust = 1.25,
        color = "white", 
        size = 4.5
    ) + 
    scale_y_continuous(
        expand = c(0, 0),
        breaks = seq(from = 0, to = 1, by = 0.1), 
        limits = c(0, 1.0001), 
        labels = scales::percent_format(accuracy = 1)
    ) +
    theme_tq() +
    scale_fill_tq()  +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
    ) +
    labs(
        x = "Month", 
        y = "Completion Percentage", 
        title = "Nicholas Rivera MD",
        subtitle = "YTD Documentation"
    ) + 
    annotate(
        geom = "text", 
        x = -Inf, 
        y = Inf, 
        hjust = -0.15,
        vjust = 1.5,
        label = "Benchmark: 90%",
        color = "tomato", 
        size = 6
    )

