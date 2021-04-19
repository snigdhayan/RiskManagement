# Loads tidyquant, lubridate, xts, quantmod, TTR, and PerformanceAnalytics
library(tidyverse)
library(tidyquant)  

# Specify stock ticker and the start and end dates
ticker <- "TSLA" # Tesla
start_date <- "2010-01-01"
end_date <- "2020-12-31"

# Get stock data and compute annual log returns
stock <- tq_get(ticker, from = start_date, to = end_date)

get_annual_returns <- function(stock.returns) {
  stock.returns %>%
    tq_transmute(select     = adjusted, 
                 mutate_fun = periodReturn, 
                 type       = "log", 
                 period     = "yearly")
}

stock_annual_log_returns <- get_annual_returns(stock)

# Visualize the annual returns
stock_annual_log_returns %>%
  ggplot(aes(x = year(date), y = yearly.returns)) + 
  geom_hline(yintercept = 0, color = palette_light()[[1]]) +
  geom_point(size = 2, color = palette_light()[[3]]) +
  geom_line(size = 1, color = palette_light()[[3]]) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Visualizing the Trend in Annual Returns",
       x = "", y = "Annual Returns", color = "") +
  theme_tq()

# Fit linear model
mod <- lm(yearly.returns ~ year(date), data = stock_annual_log_returns)

library(broom)
tidy(mod)