dcovid <- readr::read_csv("dcovid.csv", col_types = cols(date = col_date(format = "%Y-%m-%d")))


library(fpp2)         # An older forecasting framework
library(fpp3)         # A newer tidy forecasting framework
library(timetk)       # An even newer tidy forecasting framework
library(tidyverse)    # Collection of data manipulation tools
library(tidyquant)    # Business Science ggplot theme
library(cowplot)      # A ggplot add-on for arranging plots





a1 <- dcovid %>% 
  plot_time_series(
    .date_var = date,
    .value    = dcases,
    .smooth   = F,
    .interactive = FALSE,
    .line_type = 1,
    .x_lab = "Date",
    .y_lab = "New COVID-19 cases",
    .title = "New COVID-19 cases in the World"
  )
a1


dcovid=dcovid%>%mutate(dcases_r7=runner::mean_run(dcases,7))


a2 <- dcovid %>% 
  plot_time_series(
    .date_var = date,
    .value    = dcases_r7,
    .smooth   = F,
    .interactive = FALSE,
    .line_type = 1,
    .x_lab = "Date",
    .y_lab = "New COVID-19 cases",
    .title = "New COVID-19 cases in the World"
  )
a2


ts.plot(dcovid%>%select(dcases,dcases_r7),col=c("black","red"))
