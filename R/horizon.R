# Assignment 4 Putting it all Together
# Oregon Unemployment Horizon Plot
# June 2019
# Sean Pitman

#install.packages(c("tidyverse","zoo","lubridate","GGally","ggalt","scales","here","ggTimeSeries","viridis"))
library(tidyverse)
library(zoo)
library(lubridate)
library(GGally)
library(ggalt)
library(scales)
library(here)
library(ggTimeSeries)
library(viridis)
library(data.table)
# devtools::install_github("wilkelab/cowplot")
# install.packages("colorspace", repos = "http://R-Forge.R-project.org")
# devtools::install_github("clauswilke/colorblindr")
library(colorblindr)

# --Read in the Data--

# download csv file from https://www.qualityinfo.org/ed-dwnl/?at=1&t1=~unemprate~y~03~1990~2019~
# save into "data" folder in working directory
unemploy <- read.csv(here("data/lausdata2_06-03-2019.csv"), skip = 4) #superflous titles
# note that you will need to update the file name depending on when you download it

# do some tidying, variables should be place, time, rate
unemploy_tidy <- unemploy %>% 
  gather(key = "Date", value = "Rate", -Area, -Adjusted)
unemploy_tidy$Date <- as.Date(as.yearmon(unemploy_tidy$Date, "%b.%y"))

# these are all seasonally adjusted, the "adjusted" variable isn't actually helpful
unemploy_tidy <- unemploy_tidy %>% 
  select(-Adjusted)

# Might be worth looking at Year and Month levels separately though, split up "Time"
# also ensure variables are the correct type for analysis
unemploy_tidy <- unemploy_tidy %>% 
  mutate(Year = as.factor(lubridate::year(Date)),
         Month = as.factor(lubridate::month(Date)),
         Rate = as.numeric(Rate)) %>% 
  group_by(Month,Year,Area)

# Filter the empty rates
unemploy_tidy <- unemploy_tidy %>% 
  filter(!is.na(Rate))

# --I don't like the defaults of ggplot_horizon, so here's a 'better' one--
better_horizons <- function (dtData, cXColumnName, cYColumnName, bandwidth = NULL, 
                             vcGroupingColumnNames = NULL){
  nMinY <- ""
  HorizonBracket <- ""
  HorizonY <- ""
  HorizonBracketGroup <- ""
  dtData <- copy(data.table(dtData))
  setkeyv(dtData, cXColumnName)
  if (is.null(bandwidth)) {
    bandwidth <- diff(range(dtData[, cYColumnName, with = F]))/4
  }
  dtData[, `:=`(nMinY, min(get(cYColumnName), na.rm = T)), 
         vcGroupingColumnNames]
  dtData[, `:=`(HorizonBracket, ((get(cYColumnName) - 
                                    nMinY)%/%bandwidth))]
  dtData[, `:=`(HorizonY, get(cYColumnName) - (bandwidth * 
                                                 HorizonBracket) - nMinY)]
  dtData[, `:=`(HorizonBracketGroup, cumsum(c(0, diff(HorizonBracket) != 
                                                0)))]
  ggplot_horizon <- ggplot(dtData, aes_string(x = cXColumnName, 
                                              y = "HorizonY", fill = "HorizonBracket")) + 
    geom_bar(aes(y = (HorizonBracket > 0) * bandwidth, fill = HorizonBracket - 
                   1), stat = "identity") + geom_bar(stat = "identity")
  return(ggplot_horizon)
}

# --Begin constructing the plot--

# tidy names
unem_clean <- unemploy_tidy
unem_clean$Area <- gsub("\\(Seasonally Adjusted\\)","",unem_clean$Area)

# take out Oregon and United States; they don't belong with counties and MSAs
unem_general <- unem_clean %>% 
  filter(Area == "United States " || Area == "Oregon ")
# these data are not very interesting, and are unused but available

unem_or <- unem_clean %>% 
  filter(Area != "United States ", Area != "Oregon ")

# the plot

(horizon <- better_horizons(unem_or,'Date','Rate',
                vcGroupingColumnNames = 'Area',
                bandwidth = 4) +
  facet_grid(Area ~ .) +
  geom_vline(xintercept = as.Date("2008-01-01"),
             linetype = "solid",
             color = "red") +
  xlab(NULL) + ylab(NULL) +
  scale_x_date() +
  scale_fill_distiller(name = "Unemployment Rate Bracket",
                       breaks = c(0,1,2,3),
                       labels = c("4%-8%","8%-12%","12%-16%","16%+"),
                       palette = "RdYlBu",
                       limits = c(-1, 3),
                      guide = guide_legend(label.position = "bottom",
                                           title.position = "top",
                                           keywidth = 2,
                                           keyheight = 0.16,
                                           default.unit = "inch")) +
  scale_x_date(breaks = as.Date(c("1990-01-01","1995-01-01",
                                  "2000-01-01","2005-01-01",
                                  "2008-01-01","2010-01-01",
                                  "2015-01-01","2019-04-01")),
               labels = c(1990,1995,2000,2005,2008,2010,2015,2019)) +
  labs(title = "Oregon Unemployment Since 1990",
       subtitle = "The job crisis beginning in 2008 was tough on Oregon unemployment, and toughest on Crook County",
       caption = "Crook County suffered the highest unemployment rate in history in 2009 at 19.6% unemployment. 
       Unemployment was at its lowest in 1994 and 1995 in Corvallis, OR MSA and Benton County at 2%.
       
       Created in R using economic data from the State of Oregon Employment Department; www.qualityinfo.org.") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 14,
                                   face = "bold"),
        strip.background = element_blank(),
        legend.position = "bottom",
        legend.title.align = 0.5,
        legend.title = element_text(size = 16,
                                    vjust = 1),
        legend.text = element_text(size = 14),
        legend.direction = "horizontal",
        legend.spacing.x = unit(0, 'mm'),
        plot.background = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        strip.text.y = element_text(angle = 0,
                                    size = 10,
                                    hjust = 0),
        strip.text.x = element_text(size = 14,
                                    face = "bold"),
        plot.title = element_text(size = 20,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 16,
                                     face = "italic",
                                     hjust = 0.5),
        plot.caption = element_text(size = 14,
                                    hjust = 1))
)
ggsave("doc/or_unemploy_horizon.pdf", height = 10, width = 14)

# run this code to confirm the colorblind-friendly palette choice
# colorblindr::cvd_grid(horizon)
