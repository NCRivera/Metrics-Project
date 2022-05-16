library(tidyquant)


# a <- letters
# b <- rnorm(26, mean = .85, sd = .05)
# c <- rnorm(26, mean = .75, sd = .05)
# 
# tibble(MD = a, BILLING = b, NOTE = c) %>% 
#     ggplot(mapping = aes(x = BILLING, y = MD)) + 
#     geom_segment(mapping = aes(yend = MD), xend = 0, colour = "grey50") + 
#     geom_point(color = "red", size = 3) + 
#     scale_y_discrete(label=rev) + 
#     scale_x_continuous(limits = c(0.1, 1)) +
#     # scale_x_continuous(limits = c(0.1, 1)) +
#     theme_bw()
# 

DATASET <- read_csv("DATASET.csv")
DATASET <- DATASET %>% 
    mutate(MONTH = factor(MONTH, levels = month.name, ordered = TRUE))

DATASET <- DATASET %>% 
    pivot_longer(
        cols = 4:6, 
        names_to = "COMPLETION_TYPE", 
        values_to = "COMPL_PCT"
    )
    
levls <- DATASET %>% select(COMPLETION_TYPE) %>% distinct() %>% pull()

DATASET <- DATASET %>% 
    mutate(COMPLETION_TYPE = factor(COMPLETION_TYPE, levels = levls, ordered = TRUE))

PROVS <- DATASET %>% distinct(PROVIDER_NAME) %>% slice(1:3) %>% pull()


# BAR PLOT OF COMPL
DATASET %>% 
    filter(COMPLETION_TYPE == "DOCUMENTATION_COMPL_PCT") %>%
    filter(PROVIDER_NAME %in% PROVS[1]) %>% 
    mutate(MONTH_NUM = as.integer(MONTH)) %>% 
    ggplot(mapping = aes(x = MONTH, y = COMPL_PCT, fill = COMPLETION_TYPE)) +
    geom_hline(yintercept = 0.9, linetype = "dashed", size = 0.5) +
    geom_col(position = "dodge") + 
    geom_text(
        mapping = aes(label = scales::percent(COMPL_PCT, accuracy = .1), angle = 90), 
        position = position_dodge(0.9),
        hjust = 1.25,
        color = "white", 
        size = 3
    ) + 
    scale_y_continuous(
        expand = c(0, 0),
        breaks = seq(from = 0, to = 1, by = 0.1),
        limits = c(0, 1.01),
        # breaks = seq(from = 0, to = 1, by = 0.1), 
        # limits = c(0, 1.0001), 
        labels = scales::percent_format(accuracy = 1)
    ) +
    theme_tq() +
    scale_fill_tq()  +
    # theme_minimal() +
    # scale_fill_brewer(palette = "Set1")  +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 90)
        # axis.text.x = element_text(angle = 45, vjust = 0.7)
    ) +
    
    facet_wrap(PROVIDER_NAME ~ . ) +
    # facet_wrap(. ~ PROVIDER_NAME) +
    labs(
        x = "Month", 
        y = "Completion Percentage", 
        title = "Physician Documentation",
        subtitle = "Monthly Averages"
    )


# PLOT OF NOTE, BILLING
DATASET %>% 
    filter(COMPLETION_TYPE != "DOCUMENTATION_COMPL_PCT") %>%
    filter(PROVIDER_NAME %in% PROVS[1]) %>%
    # filter(PROVIDER_NAME %in% PROVS[1:2]) %>%
    # mutate(MONTH_NUM = as.integer(MONTH)) %>% 
    ggplot(mapping = aes(x = MONTH, y = COMPL_PCT, fill = COMPLETION_TYPE)) +
    geom_hline(yintercept = 0.9, linetype = "solid", size = 0.5) +
    # geom_hline(yintercept = 0.9, linetype = "dashed", size = 0.5) +
    geom_col(position = "dodge", color = "black") + 
    # geom_hline(yintercept = 1, linetype = "solid", size = 0.5) +
    # geom_hline(yintercept = 0.9, linetype = "solid", size = 0.5) +
    # geom_hline(yintercept = 0.8, linetype = "solid", size = 0.5) +
    # geom_hline(yintercept = 0.7, linetype = "solid", size = 0.5) +
    # geom_hline(yintercept = 0.6, linetype = "solid", size = 0.5) +
    # geom_hline(yintercept = 0.5, linetype = "solid", size = 0.5) +
    # geom_hline(yintercept = 0.4, linetype = "dashed", size = 0.5) +
    # geom_hline(yintercept = 0.3, linetype = "dashed", size = 0.5) +
    # geom_hline(yintercept = 0.2, linetype = "dashed", size = 0.5) +
    # geom_hline(yintercept = 0.1, linetype = "dashed", size = 0.5) +
    geom_text(
        mapping = aes(label = scales::percent(COMPL_PCT, accuracy = 1), angle = 90), 
        position = position_dodge(0.9),
        hjust = 1.25,
        # color = "white", 
        color = "black", 
        size = 3
    ) + 
    # ylim(0, 1.1) + 
    scale_y_continuous(
        expand = c(0, 0),
        breaks = seq(from = 0, to = 1, by = 0.1),
        limits = c(0, 1.01),
        labels = scales::percent_format(accuracy = 1)
    ) +
    # theme_tq() +
    # scale_fill_tq()  +
    # theme_minimal() +
    theme_bw() +
    scale_fill_brewer(palette = "Set1")  +
    # scale_fill_brewer(palette = "Pastel1")  +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 90)
    ) +
    facet_wrap(PROVIDER_NAME ~ . ) +
    labs(
        x = "Month", 
        y = "Completion Percentage", 
        title = "Physician Documentation",
        subtitle = "Monthly Averages"
    )
