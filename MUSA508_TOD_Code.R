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
library(RSocrata)

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

QNSboundary <-
  rbind(
    st_read("https://data.cityofnewyork.us/resource/7t3b-ywvw.geojson") %>% 
      select(boro_name)) %>%
  dplyr::filter(boro_name == "Queens")

square <-  
  st_sfc(st_polygon(list(cbind(c(0,3,3,0,0),c(0,0,3,3,0))))) %>%

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

allTracts.rings <-
  st_join(st_centroid(dplyr::select(allTracts, GEOID, year)), 
          multipleRingBuffer(st_union(QnsMTA), 15840, 1320)) %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(allTracts, GEOID, MedRent, year), 
            by=c("GEOID"="GEOID", "year"="year")) %>%
  st_sf() %>%
  mutate(distance = distance / 5280) #convert to miles

ggplot() + 
  geom_sf(data = allTracts.rings, aes(fill = distance))+
  geom_sf(data=QnsMTA, 
          #aes(colour = Line), 
          show.legend = "point", size= 1)

# --------

allTracts.rings <-
  st_join(st_centroid(dplyr::select(allTracts, GEOID, year)),
          multipleRingBuffer(st_union(QnsMTA), 47520, 2640)) %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(allTracts, GEOID, MedRent, year),
            by=c("GEOID"="GEOID", "year"="year")) %>%
  st_sf() %>%
  mutate(distance = distance / 5280) #convert to miles

allTracts.rings_summary <-
  allTracts.rings %>%
  st_drop_geometry() %>%
  group_by(distance, year) %>%
  summarize(Rent = mean(MedRent, na.rm=T)) 

#graph
ggplot(data=allTracts.rings_summary,
       aes(x = distance, y = MedRent, colour = year)) +
  geom_point()


# ---- Part 7. Crime Data ----

crime_url = "https://data.cityofnewyork.us/Public-Safety/NYPD-Complaint-Data-Historic/qgea-i56i"
token = "jegityh6EZNecHwu90EIPMnUe"

QNScrimedat <-   
  read.socrata(
    "https://data.cityofnewyork.us/Public-Safety/NYPD-Complaint-Data-Historic/qgea-i56i?boro_nm=QUEENS", 
    app_token = token)

QNScrimedat <- 
  read.socrata("https://data.cityofnewyork.us/Public-Safety/NYPD-Complaint-Data-Historic/qgea-i56i") %>%
  filter(boro_name == "Queens") %>%
  dplyr::select(Y = latitude, X = longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("X", "Y"), crs = 2263, agr = "constant") %>%
  st_transform('ESRI:102318') %>%
  distinct()

ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts09))+
  geom_sf(aes(fill = TOD)) +
  geom_sf(data=QNScrimedat, 
          show.legend = "point", size= 1.5) +
  mapTheme()


