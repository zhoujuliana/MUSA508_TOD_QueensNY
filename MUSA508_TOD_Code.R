# MUSA 508 TOD Assignment #1 Code - New York City (Queens County)
# 9/25/2020
# Collaborators: Juliana Zhou & Julian Hartwell

#---- Set Up ----

# Load Libraries

library(tidyverse)
library(tidycensus)
library(sf)
library(kableExtra)
library(viridis)
library(RSocrata)

options(scipen=999)
options(tigris_class = "sf")

# ---- Load styling options & custom functions -----

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

# Load multiple ring buffer function

multipleRingBuffer <- function(inputPolygon, maxDistance, interval) 
{
  #create a list of distances that we'll iterate through to create each ring
  distances <- seq(0, maxDistance, interval)
  #we'll start with the second value in that list - the first is '0'
  distancesCounter <- 2
  #total number of rings we're going to create
  numberOfRings <- floor(maxDistance / interval)
  #a counter of number of rings
  numberOfRingsCounter <- 1
  #initialize an otuput data frame (that is not an sf)
  allRings <- data.frame()
  
  #while number of rings  counteris less than the specified nubmer of rings
  while (numberOfRingsCounter <= numberOfRings) 
  {
    #if we're interested in a negative buffer and this is the first buffer
    #(ie. not distance = '0' in the distances list)
    if(distances[distancesCounter] < 0 & distancesCounter == 2)
    {
      #buffer the input by the first distance
      buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
      #different that buffer from the input polygon to get the first ring
      buffer1_ <- st_difference(inputPolygon, buffer1)
      #cast this sf as a polygon geometry type
      thisRing <- st_cast(buffer1_, "POLYGON")
      #take the last column which is 'geometry'
      thisRing <- as.data.frame(thisRing[,ncol(thisRing)])
      #add a new field, 'distance' so we know how far the distance is for a give ring
      thisRing$distance <- distances[distancesCounter]
    }
    
    
    #otherwise, if this is the second or more ring (and a negative buffer)
    else if(distances[distancesCounter] < 0 & distancesCounter > 2) 
    {
      #buffer by a specific distance
      buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
      #create the next smallest buffer
      buffer2 <- st_buffer(inputPolygon, distances[distancesCounter-1])
      #This can then be used to difference out a buffer running from 660 to 1320
      #This works because differencing 1320ft by 660ft = a buffer between 660 & 1320.
      #bc the area after 660ft in buffer2 = NA.
      thisRing <- st_difference(buffer2,buffer1)
      #cast as apolygon
      thisRing <- st_cast(thisRing, "POLYGON")
      #get the last field
      thisRing <- as.data.frame(thisRing$geometry)
      #create the distance field
      thisRing$distance <- distances[distancesCounter]
    }
    
    #Otherwise, if its a positive buffer
    else 
    {
      #Create a positive buffer
      buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
      #create a positive buffer that is one distance smaller. So if its the first buffer
      #distance, buffer1_ will = 0. 
      buffer1_ <- st_buffer(inputPolygon, distances[distancesCounter-1])
      #difference the two buffers
      thisRing <- st_difference(buffer1,buffer1_)
      #cast as a polygon
      thisRing <- st_cast(thisRing, "POLYGON")
      #geometry column as a data frame
      thisRing <- as.data.frame(thisRing[,ncol(thisRing)])
      #add teh distance
      thisRing$distance <- distances[distancesCounter]
    }  
    
    #rbind this ring to the rest of the rings
    allRings <- rbind(allRings, thisRing)
    #iterate the distance counter
    distancesCounter <- distancesCounter + 1
    #iterate the number of rings counter
    numberOfRingsCounter <- numberOfRingsCounter + 1
  }
  
  #convert the allRings data frame to an sf data frame
  allRings <- st_as_sf(allRings)
}

# Load census API key

census_api_key("dc04d127e79099d0fa300464507544280121fc3b", overwrite = TRUE)


# ---- Part 1. Years 2009 & 2016 tracts -----

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

# Get 2016 ACS 5-Year data

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

# ---- Part 2. Create TOD & non-TOD areas ----

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

# ---- Part 2. Indicator Maps ----

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
ggplot(allTracts.group) + 
  geom_sf(data = st_union(tracts09))+
  geom_sf(aes(fill = q5(MedRent.inf)), lwd = 0) +
  geom_sf(data = buffer, fill = "transparent", color = "red", size = 1)+
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
  geom_sf(data = buffer, fill = "transparent", color = "red", size = 1)+
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
  geom_sf(data = buffer, fill = "transparent", color = "red", size = 1)+
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
  geom_sf(data = buffer, fill = "transparent", color = "red", size = 1) +
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "pctBachelors"),
                    name = "Population\n(Quintile Breaks)") +
  labs(title = "Bachelor Degree Holders 2009-2016", subtitle="% of total population") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))

# --- Part 3. TOD Grouped Bar Plots ------

# Create mean averages of variables
allTracts.Summary <- 
  st_drop_geometry(allTracts.group) %>%
  group_by(TOD, year) %>% # func to summarize by a number of factors
  summarize(Rent = mean(MedRent, na.rm = T),
            Population = mean(TotalPop, na.rm = T),
            Percent_White = mean(pctWhite, na.rm = T),
            Percent_Bach = mean(pctBachelors, na.rm = T),
            Percent_Poverty = mean(pctPoverty, na.rm = T))

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

# Create comparison tables for indicator variables by TOD and year

kable(allTracts.Summary) %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Table 2.2")

# ---- Part 5. Graduated Symbol Maps ----

# Create new sf object with 2016 tract data with 1/4 mile of Queens MTA stations

grad_buffer <- 
    st_transform(QnsMTA, ('ESRI:102318')) %>% 
      st_buffer(1320) %>%
      dplyr::select(name)

QnsMTA_tracts16 <- 
  st_join(tracts16, grad_buffer) %>%
  filter(!is.na(name))

# Map mean average of MedRent variable within 1/4 miles of MTA stations

pop_station <- QnsMTA_tracts16 %>% 
  group_by(name) %>%
  summarize(sumTotalPop = sum(TotalPop)) %>%
  st_drop_geometry() %>%
  left_join(QnsMTA) %>%
  st_sf()

ggplot() +
  geom_sf(data = allTracts.group, fill = "white") +
  geom_sf(data = pop_station, aes(size = sumTotalPop), shape = 21,
          fill = "#0868ac", alpha=0.5) +
  labs(title = "Population and Transit", subtitle = "Population Within 1/4 Mile of Queens, NY MTA Stations", caption ="Figure 5.1") +
  scale_size_continuous(range = c(.5, 10))

# Map mean average of MedRent variable within 1/4 miles of MTA stations

rent_station <- QnsMTA_tracts16 %>% 
  group_by(name) %>%
  summarize(avgRent = mean(MedRent, na.rm = TRUE)) %>%
  st_drop_geometry() %>%
  left_join(QnsMTA) %>%
  st_sf()

ggplot() +
  geom_sf(data = allTracts.group, fill = "white") +
  geom_sf(data = rent_station, aes(size = avgRent), shape = 21,
          fill = "#38885e", alpha=0.5) +
  labs(title = "Median Rent and Transit", subtitle = "Average Rent Within 1/4 Mile of Queens, NY MTA Stations", caption ="Figure 5.2") +
  scale_size_continuous(range = c(.5, 10))

# ---- Part 6. Line Plot as Function of Distance ----

QnsBoundary <- st_union(tracts16)

ring_buffer <- multipleRingBuffer(buffer, 15840, 1320)

# Plot ring buffers

ggplot() + 
  geom_sf(data = QnsBoundary, fill = "lightgray", lwd = 1) +
  geom_sf(data = ring_buffer, fill = "white", alpha = 0.3) +
  geom_sf(data=QnsMTA, color = "gold") +
  labs(title = "Distances from TOD Area", 
       subtitle = "1/4 Mile Ring Buffers", 
       caption ="Figure 6.0")

# Determine tracts that intersect with each ring buffer, 
# and calculate mean MedRent and distance from MTA station

allTracts.rings <- rbind(
  st_intersection(ring_buffer, allTracts.group) %>%
  st_drop_geometry() %>%
  group_by(distance, year) %>%
  mutate(Selection_Type = "Clip") %>%
  mutate(avgMedRent = mean(MedRent, na.rm = TRUE), 
         distance = distance / 5280, na.rm = TRUE))

# Graph Average Rent by Year and Distance from Subway Station

ggplot(data=allTracts.rings,
       aes(x = distance, y = avgMedRent, color = year)) +
  geom_point() +
  geom_line(size = 1.25) + 
  scale_x_continuous(breaks=seq(0, 2, by = 0.25)) +
  scale_y_continuous(labels = scales::dollar_format()) +
  xlab(label = "Distance from Subway (miles)") +
  ylab(label = "Avg. Median Income (by Tract)") +
  labs(title = "Average Rent by Distance from Subway",
       subtitle = "1/4 Mile Ring Buffers",
       caption ="Figure 6.1") +
  plotTheme()

# ---- Part 7. 2016 Crime Data ----

# Read crime data in from GitHub .csv file (manually filtered down from 6.9M rows to ~2,900 rows,
# showing only robberies in Queens in 2016)
QNSrobbery16 <- 
  rbind(
    read.csv("/Users/julianazhou/Documents/GitHub/MUSA508_TOD_QueensNY/NYPD_Complaint_Data_2016.csv") %>% 
    dplyr::select(OFNS_DESC, VIC_RACE, VIC_SEX, Y = Latitude, X = Longitude) %>%
    na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326, agr = "constant")) 

ggplot()+
  geom_sf(data = st_union(tracts16))+
  geom_sf(data = QNSrobbery16, show.legend = "point", size= .5, alpha = 0.5) +
  geom_sf(data = buffer, fill = "transparent", color = "#e43219", lwd = 1) +
  geom_sf(data = QnsMTA, color = "#e43219", size = 1.5)+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "MedRent"),
                    name = "Robberies") +
  labs(title = "Robberies in 2016", subtitle = "# of Complaints to NYPD") +
  coord_sf(crs = st_crs(2263)) +
  mapTheme() + 
  theme(plot.title = element_text(size=22))
