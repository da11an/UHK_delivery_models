# UHK graphing and predictions
# Dallan Prince
# 2022-06-10

library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(V8)

# read data and augment ----
logs_local <- readr::read_csv("data/delivery_status.csv")

# read hxv data
# https://github.com/hxv/uhk-shipping-progress
# https://blog.hxv.me/uhk-shipping-progress/
cx <- V8::v8()
cx$source("https://raw.githubusercontent.com/hxv/uhk-shipping-progress/master/data.js") # now the variable 'data' is defined in V8

logs_hxv <- cx$get("inputData") %>%
  lapply(function(x) {
    template <- rep(NA_character_, 4)
    template[1:length(x)] <- x
    template <- set_names(template, c("post_date", "shipping-next", "last-order", "shipping-next non-black"))
    return(template)
  }) %>%
  bind_rows() %>%
  pivot_longer(cols = 2:4, names_to = "type", values_to = "order") %>%
  mutate(
    order = as.numeric(order),
    `post_date` = as.Date(`post_date`),
    `ship_date` = if_else(type != "last-order", `post_date`, as.Date(NA)),
    source = "https://github.com/hxv/uhk-shipping-progress"
  )

# numeric of dates for fitting models
logs <- bind_rows(logs_local, logs_hxv) %>%
  mutate(
    post_num = as.numeric(post_date),
    ship_num = as.numeric(ship_date),
    type = factor(type, levels = c("shipping-next", "last-order", "shipping-next non-black"))
  ) %>%
  distinct(post_date, order, type, .keep_all = TRUE)

# plot
delivery_dates <- function(logs) {
  p <- ggplot(logs) +
    aes(x = order, y = ship_date, col = type) +
    geom_point(size = 3, alpha = .5) +
    geom_smooth(method = "lm", fullrange=TRUE) +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          legend.position = "bottom") +
    labs(title = "UHK Shipping Reports & Estimates",
         subtitle = "Next-to-ship & last-placed order estimates",
         caption = "Data source: https://ultimatehackingkeyboard.com/delivery-status") +
    xlab("Order Number") + ylab("Estimated Shipping Date")
  print(p)
}

delivery_dates_plus <- function(logs, order_num) {
  my_preds <- preds(order_num) %>% rename(ship_date = .fitted)
  
  p <- delivery_dates(logs) +
    geom_errorbar(data = my_preds,
                  aes(x = order, y = ship_date, col = type, ymin = .lower, ymax = .upper), alpha = 0.7, size = 2) +
    geom_point(data = my_preds, size = 2, shape = 21, fill = "black",
               aes(x = order, y = ship_date, col = type)) +
    ggrepel::geom_label_repel(data = my_preds,
                              aes(x = order, y = ship_date, col = type, label = order), show.legend = FALSE)
  print(p)  
}

# fit models by type
regressions <- logs %>%
  tidyr::nest(data = -type) %>% 
  mutate(
    fit = purrr::map(data, ~ lm(ship_num ~ order, data = .x)),
    tidied = purrr::map(fit, broom::tidy),
    glanced = purrr::map(fit, broom::glance),
    augmented = purrr::map(fit, broom::augment, interval = "prediction")
  )

preds <- function(order) {
  regressions %>%
    mutate(
      pred = purrr::map(
        fit,
        broom::augment,
        interval = "prediction",
        newdata = data.frame(order = order)
      )
    ) %>%
    unnest(pred) %>%
    mutate(across(starts_with("."), as.Date, origin = "1970-01-01")) %>%
    select(order, type, starts_with("."))
}

# ref table
ref_table <- function(logs) {
  pred_range <- c(
    logs %>% filter(type=="shipping-next") %>% pull(order) %>% max(),
    logs %>% filter(type=="last-order", !is.na(ship_date)) %>% pull(order) %>% max()
  )
  seq(pred_range[1], pred_range[2], by = 500) %>% round(-2) %>% unique() %>%
    c(pred_range[2]) %>%
    preds() %>%
    pivot_wider(names_from = type, values_from = starts_with(".")) %>%
    arrange(-order) %>%
    filter(!is.na(order))
}
