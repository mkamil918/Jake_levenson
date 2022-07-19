
library(readxl)
library(tidyverse)
library(data.table)
library(lubridate)

#################################### LOAD DATA ###################
whale_main <- fread("Whale Alert.csv")

whale_users <- read_excel("whale_stats.xlsx", sheet = 
                              "Download Summary")

whale_sightings <- read_excel("whale_stats.xlsx", sheet = "Sightings")

android <- read_excel("whale_stats.xlsx", sheet = "Android Daily Downloads")

## Make graph for whale sightings over time by android and iOS users, split by month
# data = Download summary tab from the excel file 


whale_users$Month <- ifelse(nchar(whale_users$Month) == 2, whale_users$Month,
                            paste("0", whale_users$Month, sep = ""))

whale_users$date <- as.Date(paste(whale_users$Year, whale_users$Month, "01",sep = "-"))

whale_users <- 
  whale_users %>% pivot_longer(names_to = "Device", cols = c("iOS", "Android"))

ggplot(data = whale_users, aes(x = date, y = value, color = Device,
                               group = Device)) + 
  geom_line() +
  xlab("Year") + ylab("Sightings") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 90), axis.line = element_line(colour = "black")) + 
  ggtitle("Plot 1: Sightings by Device Type") + 
  scale_x_date(date_breaks = "6 months")

# rm(whale_users)
#########################



whale_sightings$Month <- ifelse(nchar(whale_sightings$Month) == 2, whale_sightings$Month,
                            paste("0", whale_sightings$Month, sep = ""))

whale_sightings$date <- 
  as.Date(paste(whale_sightings$Year, whale_sightings$Month, "01",sep = "-"))

whale_sightings <- 
  whale_sightings %>% 
  pivot_longer(names_to = "Sighting", cols = c("All Sightings", 
                                             "Opportunistic Sightings",
                                             "Trusted Sightings"))

## Opportunistic and trusted sightings
whale_sightings %>% 
  filter(Sighting != "All Sightings") %>% 
  ggplot(data = ., aes(x = date, y = value, color = Sighting,
                               group = Sighting)) + 
  geom_line() +
  xlab("Year") + ylab("Sightings") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.text.x = element_text(angle = 90), 
        axis.line = element_line(colour = "black")) + 
  ggtitle("Plot 2: Opportunistic and Trusted Sightings") + 
  scale_x_date(date_breaks = "6 months")


## All sightings 
whale_sightings %>% 
  filter(Sighting == "All Sightings") %>% 
  ggplot(data = ., aes(x = date, y = value, color = Sighting,
                       group = Sighting)) + 
  geom_line() +
  xlab("Year") + 
  ylab("Sightings") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90)) + 
  ggtitle("Plot 3: All Sightings") + 
  scale_x_date(date_breaks = "6 months")
  
########################################  


# Objectives:

## assess the variety of users
## how data were submitted
## the species of interest
## Geographic location 


# Additional Observations 

## population statistics
## Site fidelity practices
## Shifts in distribution
## Environmental Factors


## Show that location is in USA and Iceland (plot it out)
## Show user distributions (scientists, eco tourists, researchers)
## Ship strikes 

##### From here on, only the "whale_main" data is used (from CSV)



## Plot the species of interest

whale_main <- fread("Whale Alert.csv")

whale_main$species <- gsub('Sighting', "", whale_main$species, fixed = T)
whale_main$species <- gsub(' :', "", whale_main$species, fixed = T)
whale_main$species <- gsub('(Orca)', "", whale_main$species, fixed = T)
whale_main$species <- gsub(' Whale', "", whale_main$species, fixed = T)
whale_main$species <- gsub('Grey', "Gray", whale_main$species, fixed = T)
whale_main$species <- gsub('(Specify in comments)', "", whale_main$species, fixed = T)
whale_main$species <- str_trim(whale_main$species, side = "right")


# whale_main$species <- gsub('Whale', "", whale_main$species)
# whale_main$species <- gsub('(Orca)', "", whale_main$species)

species_dist <- as.data.frame(table(whale_main$species))
species_dist <- subset(species_dist, Freq > 30)
species_dist$Var1 <- as.character(species_dist$Var1)

species_dist$Var1 <- gsub("[\r\n]", "", species_dist$Var1)

names(species_dist)[names(species_dist) == "Var1"] <- "Species"
names(species_dist)[names(species_dist) == "Freq"] <- "Frequency"

write.csv(species_dist, "species_dist.csv")

##########################

# Observations over time 

whale_over_time <- whale_main %>%
  select(created, species, number) %>%
  mutate(date = as.Date(created)) %>% 
  filter(year(date) > 2013) %>% 
  mutate(year2 = year(date)) %>% 
  filter(species != "") %>% 
  filter(species == "Sperm" | species == "Killer" | species == "Humpback" | 
           species == "Orca" | species == "Blue") %>% 
  arrange(year2, species) %>% 
  group_by(year2, species) %>% 
  summarize(number_species = abs(sum(number)))


ggplot(data = whale_over_time, aes(x = year2, y = number_species, color = species,
                                   group = species)) + 
  geom_line() + 
  xlab("Year") +
  ylab("Number of Observations") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



##################

# Sources 

ggplot(data = whale_main, aes(x = source)) + 
  geom_bar() + 
  ggtitle("How was the Sighting Reported?") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                            panel.background = element_blank(), 
                                         axis.line = element_line(colour = "black"))


########
whale_main$trusted_observer <- as.character(whale_main$trusted_observer)
ggplot(data = whale_main, aes(x = trusted_observer)) + 
  geom_bar() + 
  ggtitle("Trusted Observer") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))





## Geo location

library(sf)
library(rnaturalearth)

whale_main <- fread("Whale Alert.csv")

df <- whale_main %>% 
  select(longitude, latitude) %>% 
  setNames(c("long", "lat")) %>% 
  filter(lat >= -90 & lat <= 90) %>% 
  filter(long >= -180 & long <= 180)
  

class(df$long)

df <-  sf::sf_project("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", 
                      "+proj=moll", df) %>% 
  as.data.frame() %>% 
  setNames(c("long", "lat"))


range(df$lat)

range(df$long)



world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf")

world %>% 
  st_transform(crs = "+proj=moll") %>%
  ggplot() + 
  geom_sf() + 
  theme_minimal() +
  geom_point(data = df, aes(long, lat)) 

