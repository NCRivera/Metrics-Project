library(tidyverse)
library(readr)

DATASET_2 <- read_csv("DATASET_2.csv")
DATASET_2 <- DATASET_2 %>% 
    mutate(MONTH = factor(MONTH, levels = month.name, ordered = TRUE))

# Scatter Plot - ANYONE NOT IN TOP RIGHT IS IN TROUBLE
DATASET_2 %>% 
    filter(MONTH == "January") %>% 
    ggplot(mapping = aes(x = BILLING_COMPL_PCT, y = NOTE_COMPL_PCT)) + 
    geom_point() + 
    geom_hline(yintercept = 0.9, linetype = "dashed") + 
    geom_vline(xintercept = 0.9, linetype = "dashed") + 
    theme_minimal() + 
    theme(legend.position = "none") + 
    annotate(
        "rect", xmin = 0.9, xmax = 1, ymin = 0.9, ymax = 1, fill = "green", alpha = 0.5
    ) + 
    annotate(
        "rect", xmin = 0.9, xmax = 1, ymin = 0, ymax = 0.9, fill = "red", alpha = 0.05
    ) + 
    annotate(
        "rect", ymin = 0.9, ymax = 1, xmin = 0, xmax = 0.9, fill = "red", alpha = 0.05
    ) + 
    annotate(
        "rect", xmin = 0, xmax = 0.9, ymin = 0, ymax = 0.9, fill = "red", alpha = 0.25
    )

DATASET_2 <- DATASET_2 %>% 
    pivot_longer(
        cols = 4:6, 
        names_to = "COMPLETION_TYPE", 
        values_to = "COMPL_PCT"
    )
levls <- DATASET_2 %>% select(COMPLETION_TYPE) %>% distinct() %>% pull()

DATASET_2 <- DATASET_2 %>% 
    mutate(COMPLETION_TYPE = factor(COMPLETION_TYPE, levels = levls, ordered = TRUE))

PROVS <- DATASET_2 %>% distinct(PROVIDER_NAME) %>% slice(1:25) %>% pull()

# NOTE COMPLETION, BILLING COMPLETION, FACET BY MONTH
DATASET_2 %>% 
    filter(COMPLETION_TYPE != "DOCUMENTATION_COMPL_PCT") %>%
    filter(PROVIDER_NAME %in% PROVS) %>%
    filter(MONTH %in% c("January", "February", "March")) %>%
    ggplot(mapping = aes(x = COMPLETION_TYPE, y = PROVIDER_NAME)) +
    geom_tile(mapping = aes(height = 0.85, width = 0.85, fill = COMPL_PCT)) +
    geom_text(mapping = aes(label = scales::percent(COMPL_PCT, accuracy = 1)), size = 2) +
    facet_wrap(~ MONTH) +
    scale_fill_gradientn(
        colours = c("firebrick1", "white", "darkgreen"), 
        values = c(0, 0.5, 1)
    ) + 
    scale_y_discrete(lim=rev) + 
    theme_minimal() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        # axis.text.x = element_text(angle = 90), 
        axis.text.x = element_blank(), 
        legend.position = "none"
    )





# COMPLETION ONLY: BY MONTH, JAN - DEC
DATASET_2 %>% 
    filter(PROVIDER_NAME %in% PROVS) %>% 
    filter(COMPLETION_TYPE == "DOCUMENTATION_COMPL_PCT") %>% 
    ggplot(mapping = aes(x = MONTH, y = PROVIDER_NAME)) +
    geom_tile(mapping = aes(fill = COMPL_PCT)) + 
    # geom_tile(mapping = aes(height = 0.9, width = 0.95, fill = COMPL_PCT)) + 
    geom_text(mapping = aes(label = scales::percent(COMPL_PCT, accuracy = 1)), size = 3) +
    scale_y_discrete(lab = rev) +
    scale_fill_gradientn(
        colours = c("firebrick1", "white", "darkgreen"),
        # colours = terrain.colors(10), 
        values = c(0, 0.5, 1)
    ) + 
    # theme_tq() + 
    theme_minimal() + 
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 90)
    )

