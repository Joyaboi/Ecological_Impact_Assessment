####Load ggplot####
library(ggplot2)

####Organize Data####

#####Read all CSVs#####
plant_events <- read.csv("Plants/SE_Plants.csv", stringsAsFactors = FALSE)
plant_occurances <- read.csv("Plants/AO_Plants.csv", stringsAsFactors = FALSE)

bird_events <- read.csv("Birds/SE_Birds.csv", stringsAsFactors = FALSE)
bird_occurances <- read.csv("Birds/AO_Birds.csv", stringsAsFactors = FALSE)

aquatic_events <- read.csv("Aquatic_Inverts/SE_Aquatic.csv", stringsAsFactors = FALSE)
aquatic_occurances <- read.csv("Aquatic_Inverts/AO_Aquatic.csv", stringsAsFactors = FALSE)

bat_events <- read.csv("Bats/SE_Bats.csv", stringsAsFactors = FALSE)
bat_occurances <- read.csv("Bats/AO_Bats.csv", stringsAsFactors = FALSE)

insect_events <- read.csv("Insects/SE_Insects.csv", stringsAsFactors = FALSE)
insect_occurances <- read.csv("Insects/AO_Insects.csv", stringsAsFactors = FALSE)
moth_events <- read.csv("Insects/SE_Moths.csv", stringsAsFactors = FALSE)
moth_occurances <- read.csv("Insects/AO_Moths.csv", stringsAsFactors = FALSE)

mammal_events <- read.csv("Mammals/SE_Mammals.csv", stringsAsFactors = FALSE)
mammal_occurances <- read.csv("Mammals/AO_Mammals.csv", stringsAsFactors = FALSE)

#####Create a function that organizes data by North and South Plots#####
get_site <- function(event_id) {
  ifelse(grepl("^N", event_id), "North",
         ifelse(grepl("^S", event_id), "South", NA))
}

#####Prepare Data#####

#Combine plant events & occurrences and add Site column
plants <- merge(plant_occurances, plant_events, by = "eventID", all.x = TRUE)
plants$Site <- get_site(plants$eventID)
plants <- plants[!is.na(plants$Site), ]

#Combine insect & moth data and add Site column
inverts <- rbind(
  cbind(insect_occurances, Group = "Terrestrial inverts"),
  cbind(moth_occurances, Group = "Moths")
)
inverts$Site <- get_site(inverts$eventID)
inverts <- inverts[!is.na(inverts$Site), ]

#Add Site column to Birds
birds <- bird_occurances
birds$Site <- get_site(birds$eventID)
birds <- birds[!is.na(birds$Site), ]

#Add Site column to Bats
bats <- bat_occurances
bats$Site <- get_site(bats$eventID)
bats <- bats[!is.na(bats$Site), ]

#Add Site column to Mammals and remove "No Records"
mammals <- mammal_occurances
mammals$Site <- get_site(mammals$eventID)
mammals <- mammals[!is.na(mammals$Site), ]
mammals <- mammals[mammals$scientificName != "No records", ]

#Remove empty/non-numeric BMWP scores from Aquatic inverts and add Site column
aquatic <- aquatic_occurances
aquatic$Site <- get_site(aquatic$eventID)
aquatic$BMWPscore_clean <- as.numeric(gsub("[^0-9]", "", aquatic$BMWPscore))
aquatic <- aquatic[!is.na(aquatic$Site), ]

####Plants####

#####Plant Richness#####
plant_richness <- aggregate(scientificName ~ Site, data = unique(plants[, c("Site", "scientificName")]), FUN = length)
names(plant_richness)[2] <- "Species_richness"

plant_richness
#North: 25
#South: 18

#Plot Richness
ggplot(plant_richness, aes(x = Site, y = Species_richness)) +
  geom_col(fill = "forestgreen") +
  labs(title = "Plant species richness by site", y = "Number of species") +
  theme_minimal()

#####Plant Coverage#####

#Create a data frame for plant % coverage
plant_cover <- aggregate(organismQuantity ~ Site + scientificName, data = plants, FUN = mean, na.rm = TRUE)

#Top dominant species
top_plants <- do.call(rbind, lapply(split(plant_cover, plant_cover$Site), function(df) df[order(-df$organismQuantity), ][1:5, ]))

top_plants

#Site                  scientificName         organismQuantity
#North.8  North     Deschampsia_cespitosa         85.00000
#North.5  North          Calluna_vulgaris         75.62500
#North.34 North Rhytidadelphus_triquetrus         70.55556
#North.31 North       Pteridium_aquilinum         61.76471
#North.14 North            Galium_mollugo         61.47059
#South.25 South            Nardus_stricta         86.00000
#South.39 South          Trifolium_repens         52.50000
#South.21 South           Juncus_inflexus         40.00000
#South.20 South            Juncus_effusus         35.00000
#South.27 South       Polytrichum_commune         27.50000


#Plot dominant species (mean cover > 10)
ggplot(subset(plant_cover, organismQuantity > 10), aes(x = reorder(scientificName, organismQuantity), y = organismQuantity)) +
  geom_col(fill = "darkolivegreen") +
  coord_flip() +
  facet_wrap(~Site, scales = "free_y") +
  labs(title = "                                 Dominant plant species by site", y = "Mean percent cover", x = "") +
  theme_minimal()

####Terrestrial Invertebrates####

#####Invert Abundance#####
invert_abundance <- aggregate(individualCount ~ Site, data = inverts, FUN = sum, na.rm = TRUE)

invert_abundance
#North: 70
#South: 323

#Plot Abundance
ggplot(invert_abundance, aes(x = Site, y = individualCount)) +
  geom_col(fill = "orange") +
  labs(title = "Total invertebrate abundance by site", y = "Individuals recorded") +
  theme_minimal()

#####Invert Richness#####
invert_richness <- aggregate(scientificName ~ Site, data = unique(inverts[, c("Site", "scientificName")]), FUN = length)
names(invert_richness)[2] <- "Species_richness"

invert_richness
#North: 31
#South: 38

#Plot Richness
ggplot(invert_richness, aes(x = Site, y = Species_richness)) +
  geom_col(fill = "orange") +
  labs(title = "Terrestrial Invertebrate species richness by site", y = "Number of species") +
  theme_minimal()

####Birds####

#####Bird Abundance#####
bird_abundance <- aggregate(individualCount ~ Site, data = birds, FUN = sum, na.rm = TRUE)

bird_abundance
#North: 57
#South: 33

#Plot Abundance
ggplot(bird_abundance, aes(x = Site, y = individualCount)) +
  geom_col(fill = "skyblue") +
  labs(title = "Total Bird abundance by site", y = "Individuals recorded") +
  theme_minimal()

#####Bird Richness#####
bird_richness <- aggregate(scientificName ~ Site, data = unique(birds[, c("Site", "scientificName")]), FUN = length)
names(bird_richness)[2] <- "Species_richness"

bird_richness
#North: 14
#South: 11

#Plot Richness
ggplot(bird_richness, aes(x = Site, y = Species_richness)) +
  geom_col(fill = "skyblue") +
  labs(title = "Total Bird species richness by site", y = "Number of species") +
  theme_minimal()

####Bats####

#####Bat Richness#####
bat_richness <- aggregate(scientificName ~ Site, data = unique(bats[, c("Site", "scientificName")]), FUN = length)
names(bat_richness)[2] <- "Species_richness"

bat_richness
#North: 3
#South: 5

#Plot Richness
ggplot(bat_richness, aes(x = Site, y = Species_richness)) +
  geom_col(fill = "grey27") +
  labs(title = "Total bat species richness by site", y = "Number of species") +
  theme_minimal()

####Mammals####

#####Mammal Richness#####
mammal_richness <- aggregate(scientificName ~ Site, data = unique(mammals[, c("Site", "scientificName")]), FUN = length)
names(mammal_richness)[2] <- "Species_richness"

mammal_richness
#North: 5
#South: 4

#Plot Richness
ggplot(mammal_richness, aes(x = Site, y = Species_richness)) +
  geom_col(fill = "orange") +
  labs(title = "Total mammal species richness by site", y = "Number of species") +
  theme_minimal()

####Aquatic Invertebrates####

#####Aquatic Richness#####
aquatic_richness <- aggregate(Order ~ Site, data = unique(aquatic[, c("Site", "Order")]), FUN = length)
names(aquatic_richness)[2] <- "Orders_detected"

aquatic_richness
#North: 6
#South: 9

#Plot Richness
ggplot(aquatic_richness, aes(x = Site, y = Orders_detected)) +
  geom_col(fill = "darkcyan") +
  labs(title = "Total aquatic invertebrate order richness by site", y = "Number of orders") +
  theme_minimal()

#####Aquatic BMWP#####
aquatic_bmwp <- aggregate(BMWPscore_clean ~ Site, data = aquatic, FUN = mean, na.rm = TRUE)
names(aquatic_bmwp)[2] <- "Mean_BMWP"

aquatic_bmwp
#North: 94.41176
#South: 89.66667

#Plot BMWP
ggplot(aquatic_bmwp, aes(x = Site, y = Mean_BMWP)) +
  geom_col(fill = "darkcyan") +
  labs(title = "Mean BMWP by site", y = "BMWP Score") +
  theme_minimal()

####Combined Data####

#Define colors for each organism group
group_colours <- c(
  "Plants" = "forestgreen",
  "Terrestrial inverts" = "orange",
  "Birds" = "skyblue",
  "Bats" = "grey27",
  "Mammals" = "saddlebrown",
  "Aquatic inverts (orders)" = "darkcyan"
)


#####Richness Comparison#####
comparison_richness <- rbind(
  data.frame(Group = "Plants", plant_richness),
  data.frame(Group = "Terrestrial inverts", invert_richness),
  data.frame(Group = "Birds", bird_richness),
  data.frame(Group = "Bats", bat_richness),
  data.frame(Group = "Mammals", mammal_richness),
  data.frame(Group = "Aquatic inverts (orders)",
             Site = aquatic_richness$Site,
             Species_richness = aquatic_richness$Orders_detected)
)

#Plot Richness Comparison
ggplot(comparison_richness,
       aes(x = Group, y = Species_richness, fill = Group)) +
  geom_col(position = "dodge") +
  facet_wrap(~Site) +
  scale_fill_manual(values = group_colours) +
  labs(
    title = "Species richness comparison between hillsides",
    y = "Number of taxa detected",
    x = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "none"
  )

#####Abundance Comparison#####
abundance_compare <- rbind(
  data.frame(Group = "Terrestrial inverts", invert_abundance),
  data.frame(Group = "Birds", bird_abundance)
)

#Plot Abundance Comparison
ggplot(abundance_compare,
       aes(x = Group, y = individualCount, fill = Group)) +
  geom_col() +
  facet_wrap(~Site) +
  scale_fill_manual(values = group_colours) +
  labs(
    title = "Abundance comparison between hillsides",
    y = "Individuals recorded",
    x = ""
  ) +
  theme_minimal() +
  theme(legend.position = "none")
