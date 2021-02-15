
#################################################
#### a10 data  ###################################
#################################################


library(fpp2)
library(forecast)
data(a10)
a10%>%head()
class(a10)
autoplot(a10)+ggcharts::theme_ggcharts(axis = "xy", grid = "X")+ylab("Sales")+ggtitle(label = "Monthly anti-diabetic drug subsidy",
                                                                                     subtitle = "Australia from 1991 to 2008")



#################################################
##  Importation des donnees de la BCT ###########
#################################################


library(readr)
avoir_devises <- read_csv("avoir_devises.csv", 
                          col_types = cols(Date = col_date(format = "%d/%m/%Y")))

colnames(avoir_devises)
class(avoir_devises$Date)
library(dplyr)
avoir_devises=avoir_devises%>%na.omit()
library(lubridate)
year(avoir_devises$Date)
quarter(avoir_devises$Date)
month(avoir_devises$Date)
month(avoir_devises$Date,label = T,abbr = T)
month(avoir_devises$Date,label = T,abbr = F)


### Calculer Avoir en devises par trimestre

Avoir_q=avoir_devises%>%mutate(quart=quarter(Date),year=year(Date),YQ=paste0(year,"_",quart))%>%
  group_by(YQ)%>%summarise(Avoir=sum(Avoir))
Avoir_q

### Creer un objet ts trimisteriel et representation graphique 

x=ts(Avoir_q$Avoir,start = c(1994,1),frequency = 4)
x
library(forecast)
library(ggplot2)
autoplot(x)+labs(title="Avoir en Devises en TND (trimestriel)",
                 subtitle="1994-2018",
                 caption="Source : Banque Centrale Tunisienne")+
  xlab("Date")+ylab("Avoir en TND")+ggcharts::theme_ggcharts(axis = "x", grid = "X")


#### Representation  des Avoirs mensuels

x=ts(avoir_devises$Avoir,start = c(1994,1),frequency = 12)
x

autoplot(x)+labs(title="Avoir en Devises en TND (mensuel)",
                 subtitle="1994-2018",
                 caption="Source : Banque Centrale Tunisienne")+
  xlab("Date")+ylab("Avoir en TND")+ggcharts::theme_ggcharts(axis = "x", grid = "X")


### Seasonal plot pour les donnees Avoir devises


x=ts(avoir_devises$Avoir,start = c(1994,1),frequency = 12)
x

ggseasonplot(x,year.labels = T,year.labels.left = T)+ylab("TND")+
  ggtitle("Seasonal plot: Avoir devices en Tunisie (TND)")+
  theme_bw()  





#################################################
#### a10 data  ###################################
#################################################


### Seasonal plot

library(fpp2)
library(forecast)
data(a10)
a10
ggseasonplot(a10,year.labels = T,year.labels.left = T)+ylab("$ million")+
 ggtitle("Seasonal plot: Anti-diabetic drug sales\n  in Australia from 1991 to 2008")+
theme_bw()  



### Using other TS/data packages 

# Load libraries
library(fpp2)         # An older forecasting framework
library(fpp3)         # A newer tidy forecasting framework
library(timetk)       # An even newer tidy forecasting framework
library(tidyverse)    # Collection of data manipulation tools
library(tidyquant)    # Business Science ggplot theme
library(cowplot)      # A ggplot add-on for arranging plots



# Convert ts to tibble
a10_tbl <- fpp2::a10 %>%
  tk_tbl()

a10_tbl

# Monthly plot of anti-diabetic scripts in Australia 
a1 <- a10_tbl %>% 
  plot_time_series(
    .date_var = index,
    .value    = value,
    .smooth   = TRUE,
    .interactive = FALSE,
    .line_type = 1,
    .smooth_color = "#ff0000",
    .smooth_alpha = .5,
    .x_lab = "Date",
    .y_lab = "Sales",
    .title = "Monthly anti-diabetic scripts in Australia"
  )

a1

# New time-based features to group by
a10_tbl_add <- a10_tbl %>% 
  mutate( 
    month = factor(month(index, label = TRUE)),  # Plot this
    year = factor(year(index))  # Grouped on y-axis
  )

a10_tbl_add 


# Seasonal plot
a2 <- a10_tbl_add %>%
  ggplot(aes(x = month, y = value, 
             group = year, color = year)) + 
  geom_line() + 
  geom_text(
    data = a10_tbl_add %>% filter(month == min(month)),
    aes(label = year, x = month, y = value),
    nudge_x = -0.3) + 
  geom_text(
    data = a10_tbl_add %>% filter(month == max(month)),
    aes(label = year, x = month, y = value),
    nudge_x = 0.3) + 
  guides(color = FALSE)+theme_bw()

a2
# Arrangement of plots
plot_grid(a1, a2, ncol=1, rel_heights = c(1, 1.5))


######################################
##  Beer data    ###################
#####################################


beer_fpp2 <- fpp2::ausbeer
beer_fpp2
class(beer_fpp2)
# Monthly beer production in Australia 1992 and after
beer_fpp2 <- fpp2::ausbeer %>%
  window(start = 1992)    
beer_fpp2

# Time series plot
b1 <- beer_fpp2 %>% 
  autoplot()+ggcharts::theme_ggcharts(axis = "xy", grid = "X")
b1

# Subseries plot
b2 <- beer_fpp2 %>% 
  ggsubseriesplot()+ggcharts::theme_ggcharts(axis = "xy", grid = "X")
b2

# Plot it
plot_grid(b1, b2, ncol=1, rel_heights = c(1, 1.5))

############################################################################
# Monthly beer production in Australia 1992 and after  #####################
############################################################################


class(fpp2::ausbeer)
ausbeer_tbl <- fpp2::ausbeer %>%
  tk_tbl() %>%
  filter(year(index) >= 1992) %>%
  mutate(index = as_date(index))

ausbeer_tbl

# Time series plot
b1 <- ausbeer_tbl %>%
  plot_time_series(
    .date_var = index,
    .value    = value,
    .interactive = FALSE
  )

b1

### I can make it interactive 

b1 <- ausbeer_tbl %>%
  plot_time_series(
    .date_var = index,
    .value    = value,
    .interactive = TRUE
  )

b1


# Subseries plot
b2 <- ausbeer_tbl %>%
  mutate(
    quarter = str_c("Quarter ", as.character(quarter(index)))
  ) %>%
  plot_time_series(
    .date_var = index,
    .value = value,
    .facet_vars = quarter,
    .facet_ncol = 4, 
    .color_var = quarter, 
    .facet_scales = "fixed",
    .interactive = FALSE,
    .legend_show = FALSE
  )

b2
# Plot it
plot_grid(b1, b2, ncol=1, rel_heights = c(1, 1.5))



##################################################
## Quarter graph avoir en devises ################
##################################################

avoir_devises <- read_csv("avoir_devises.csv", 
                          col_types = cols(Date = col_date(format = "%d/%m/%Y")))

avoir_devises%>%head()
avoir=ts(avoir_devises$Avoir)


avoir1<- avoir_devises %>%
  plot_time_series(
    .date_var = Date,
    .value    = Avoir,
    .interactive = FALSE
  )
avoir1


avoir2 <- avoir_devises %>%
  mutate(
    quarter = str_c("Quarter ", as.character(quarter(Date)))
  ) %>%
  plot_time_series(
    .date_var = Date,
    .value = Avoir,
    .facet_vars = quarter,
    .facet_ncol = 4, 
    .color_var = quarter, 
    .facet_scales = "fixed",
    .interactive = FALSE,
    .legend_show = FALSE
  )
avoir2


################################################################
######## COVID data   ##########################################
################################################################


dcovid <- readr::read_csv("dcovid.csv", col_types = cols(date = col_date(format = "%Y-%m-%d")))
dcovid
dcovid=dcovid[-1,]


dcovid=dcovid%>%
  mutate(
    days = lubridate::wday(date,label=T)
  )
dcovid


p1<-dcovid%>%plot_time_series(
  .date_var = date,
  .value = ddeaths,
  .facet_collapse=F,
  .facet_vars = days,
  .facet_ncol = 3,
  .facet_scales = "fixed",
  .interactive = FALSE,
  .legend_show = FALSE
)
p1
