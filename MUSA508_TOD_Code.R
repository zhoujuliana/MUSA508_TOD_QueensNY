# MUSA 508 TOD Assignment #1 Code - New York City (Queens County)
# 9/20/2020
# By: Juliana Zhou & Julian Hartwell

#---- Set Up ----

install.packages("viridis")

# Load Libraries

library(tidyverse)
library(tidycensus)
library(sf)
library(kableExtra)
library(viridis)

options(scipen=999)
options(tigris_class = "sf")

# ---- Load Styling options -----

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.text.x = element_text(size = 14))
}

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )
}

# Load Quantile break functions

qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

q5 <- function(variable) {as.factor(ntile(variable, 5))}

# Load hexadecimal color palette

palette5 <- c("#f0f9e8","#bae4bc","#7bccc4","#43a2ca","#0868ac")

# Load census API key

census_api_key("dc04d127e79099d0fa300464507544280121fc3b", overwrite = TRUE)


# ---- Part 1. Year 2009 tracts -----

# Get 2009 ACS 5-Year data

tracts09 <- 
  get_acs(geography = "tract", variables = c("B25026_001E","B02001_002E","B15001_050E",
                                             "B15001_009E","B19013_001E","B25058_001E",
                                             "B06012_002E"), 
          year=2009, state=36, county=081, geometry=T, output="wide") %>%
  st_transform('ESRI:102318') %>%
  rename(TotalPop = B25026_001E, 
         Whites = B02001_002E,
         FemaleBachelors = B15001_050E, 
         MaleBachelors = B15001_009E,
         MedHHInc = B19013_001E, 
         MedRent = B25058_001E,
         TotalPoverty = B06012_002E) %>%
  dplyr::select(-NAME, -starts_with("B")) %>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop,0), #JW: Since we only need 4 indcators, remove?
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop),0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         year = "2009") %>%
  dplyr::select(-Whites, -FemaleBachelors, -MaleBachelors, -TotalPoverty) 

view(tracts09)

# ---- Part 1. 2016 Census Data -----

tracts16 <- 
  get_acs(geography = "tract", variables = c("B25026_001","B02001_002","B15001_050",
                                             "B15001_009","B19013_001","B25058_001",
                                             "B06012_002"), 
          year=2016, state=36, county=081, geometry=T, output="wide") %>%
  st_transform('ESRI:102318') %>%
  rename(TotalPop = B25026_001E, 
         Whites = B02001_002E,
         FemaleBachelors = B15001_050E, 
         MaleBachelors = B15001_009E,
         MedHHInc = B19013_001E, 
         MedRent = B25058_001E,
         TotalPoverty = B06012_002E) %>%
  dplyr::select(-NAME, -starts_with("B")) %>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop,0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop),0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         year = "2016") %>%
  dplyr::select(-Whites, -FemaleBachelors, -MaleBachelors, -TotalPoverty) 

view(tracts16)


# --- Part 1. Combining 2009 and 2016 data ----

allTracts <- rbind(tracts09,tracts16)


# ---- Part 1. Wrangling Transit Open Data -----

# Read in NYC subway station data 

MTAStops <- 
  rbind(
    st_read("https://data.cityofnewyork.us/resource/kk4q-3rt2.geojson") %>% 
      select(name, line)) %>%
  st_transform('ESRI:102318')

# Subset/select only MTA stations in Queens

QnsMTA <- MTAStops[tracts09,]

# Check QnsMTA stations

ggplot() + 
  geom_sf(data=st_union(tracts09)) +
  geom_sf(data=QnsMTA, 
          #aes(colour = Line), 
          show.legend = "point", size= 1) +
  #scale_colour_manual(values = c("orange","blue")) +
  labs(title="MTA Stops", 
       subtitle="Queens, NY", 
       caption="Figure x.x") +
  mapTheme()

# --- Part 1. Relating MTA Stops and Queens County Tracts ----

# Create buffers (in feet) around Queens MTA stations, then create union buffer

QnsMTA_buffer <- 
  rbind(
    st_buffer(QnsMTA, 1320) %>%
      mutate(Legend = "Buffer") %>%
      dplyr::select(Legend),
    st_union(st_buffer(QnsMTA, 1320)) %>%
      st_sf() %>%
      mutate(Legend = "Unioned Buffer"))

# Visualize both buffers and union buffer using "facet_wrap" plot

ggplot() +
  geom_sf(data=QnsMTA_buffer) +
  geom_sf(data=QnsMTA, show.legend = "point") +
  facet_wrap(~Legend) + 
  labs(caption = "Figure 2.6") +
  mapTheme()

# Create an sf object with ONLY the unioned buffer
buffer <- filter(QnsMTA_buffer, Legend=="Unioned Buffer")

# ---- Part 2. Indicator Maps ----

# Create TOD & non-TOD areas using centroid join
# Includes 2009-2016 inflation rate for MedRent variable: 1.118

allTracts.group <- 
  rbind(
    st_centroid(allTracts)[buffer,] %>%
      st_drop_geometry() %>%
      left_join(allTracts) %>%
      st_sf() %>%
      mutate(TOD = "TOD"),
    st_centroid(allTracts)[buffer, op = st_disjoint] %>%
      st_drop_geometry() %>%
      left_join(allTracts) %>%
      st_sf() %>%
      mutate(TOD = "Non-TOD")) %>%
  mutate(MedRent.inf = ifelse(year == "2009", MedRent * 1.118, MedRent)) 

# Time/Space map of 2009 & 2016 TOD tracts
ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts09))+
  geom_sf(aes(fill = TOD)) +
  scale_fill_manual(values = c("#bae4bc","#43a2ca"),
                    name = "TOD") +
  labs(title = "Time/Space Groups") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))

# Indicator Map #1: Median rent in 2009 & 2016, showing TOD
ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts09))+
  geom_sf(aes(fill = q5(MedRent.inf)), lwd = 0) +
  geom_sf(data = buffer, fill = "transparent", color = "red")+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "MedRent.inf"),
                    name = "Rent\n(Quintile Breaks)") +
  labs(title = "Median Rent 2009-2016", subtitle = "Real Dollars") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))

# Indicator Map #2: Population in 2009 & 2016, showing TOD
ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts09))+
  geom_sf(aes(fill = q5(TotalPop)), lwd = 0) +
  geom_sf(data = buffer, fill = "transparent", color = "red")+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "TotalPop"),
                    name = "Population\n(Quintile Breaks)") +
  labs(title = "Population 2009-2016") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))

# Indicator Map #3: Poverty (% of TotalPop) in 2009 & 2016, showing TOD
ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts09))+
  geom_sf(aes(fill = q5(pctPoverty)), lwd = 0) +
  geom_sf(data = buffer, fill = "transparent", color = "red")+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "pctPoverty"), #JW: NEED TO SHOW 2 DECIMAL PTS
                    name = "% Poverty\n(Quintile Breaks)") +
  labs(title = "Poverty 2009-2016", subtitle="% of total population") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))

# Indicator Map #4: Bachelor Degree Holders in 2009 & 2016, showing TOD
ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts09))+
  geom_sf(aes(fill = q5(pctBachelors)), lwd = 0) +
  geom_sf(data = buffer, fill = "transparent", color = "red")+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "pctBachelors"),
                    name = "Population\n(Quintile Breaks)") +
  labs(title = "Bachelor Degree Holders 2009-2016", subtitle="% of total population") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))

# --- Part 3. TOD Grouped Bar Plots ------

# Created grouped bar plot comparing:
# % bachelor's degree holders, % poverty rate, % white, total population, and median rent

allTracts.Summary %>%
  gather(Variable, Value, -year, -TOD) %>%
  ggplot(aes(year, Value, fill = TOD)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Variable, scales = "free", ncol=5) +
  scale_fill_manual(values = c("#bae4bc", "#0868ac")) +
  labs(title = "Indicator differences across time and space") +
  plotTheme() + theme(legend.position="bottom")

# --- Part 4. TOD Indicator Tables ----

allTracts.Summary <- 
  st_drop_geometry(allTracts.group) %>%
  group_by(TOD, year) %>% # func to summarize by a number of factors
  summarize(Rent = mean(MedRent, na.rm = T),
            Population = mean(TotalPop, na.rm = T),
            Percent_White = mean(pctWhite, na.rm = T),
            Percent_Bach = mean(pctBachelors, na.rm = T),
            Percent_Poverty = mean(pctPoverty, na.rm = T))

kable(allTracts.Summary) %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Table 2.2")

# ---- Part 5. Graduated Symbol Maps ----


# ---- Part 6. Line Plot as Function of Distance ----


# ---- Part 7. Crime Data ----

QNScrimedat <-  
  rbind(
    st_read("https://data.cityofnewyork.us/resource/qgea-i56i.geojson") %>%
      dplyr::select(rpt_dt, boro_nm, ky_cd, ofns_desc, law_cat_cd, x_coord_cd, y_coord_cd, latitude, longitude, vic_sex) %>%
      dplyr::filter(boro_nm == "QUEENS") %>%
      st_transform('ESRI:102318') %>%
      st_sf())

QNScrimedat$year <- format(as.Date(QNScrimedat$rpt_dt, format="%Y/%m/%d"),"%Y")
View(QNScrimedat)

ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts09))+
  geom_sf(aes(fill = TOD)) +
  geom_sf(data=QNScrimedat, 
          show.legend = "point", size= 1.5) +
  mapTheme()


