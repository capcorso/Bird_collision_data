# title: Corso Bird Collision Exploration
# author: Julianna 
# output: html_document

#' Load Data 
#' 
library(readr)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)

url <- "https://raw.githubusercontent.com/capcorso/Bird-Collision/main/Old.ACC.dat.csv"
data <- read.csv(url)


# How many entries are there 
n_rows <- nrow(data)
cat("Number of data entries:", n_rows)

#  9080 data entries 

# List Species 
species_list = data$Species_Abv
list(species_list)

#' cleaning data 
#' 
# species count 
count_species = table(species_list)
sort(count_species)

species_list
# Look at distribution 
pie(count_species)



# Making a chart of the top 10 species with the most collisions 


# sp_count <- table(data$Species)

# sp_percent <- round(prop.table(sp_count) * 100, 2)

# top_sp <- names(sort(sp_count, decreasing = TRUE)[1:10])

# data_sub <- data[data$Species %in% top_sp,]

# sp_df <- as.data.frame(sp_count)

# colnames(sp_df) <- c("how_obtained_code", "frequency")

# sp_df$percentage <- sp_percent

# top_sp_df <- sp_df[sp_df$how_obtained_code %in% top_sp,]

# PLOTTTTT
ggplot(top_sp_df, aes(x = reorder(how_obtained_code, -frequency), y = percentage)) +
  geom_bar(stat = "identity") +
  xlab(" ") +
  ylab("%") +
  ggtitle("Top 10 Species with most collisions")

#Okay that looks good but change it from scientific name to species code/ abreviation 
# instead of species column Species_Abv 

sp_count <- table(data$Species_Abv)

sp_percent <- round(prop.table(sp_count) * 100, 2)

top_sp <- names(sort(sp_count, decreasing = TRUE)[1:10])
data_sub <- data[data$Species_Abv %in% top_sp,]
sp_df <- as.data.frame(sp_count)
colnames(sp_df) <- c("how_obtained_code", "frequency")
sp_df$percentage <- sp_percent
top_sp_df <- sp_df[sp_df$how_obtained_code %in% top_sp,]

# PLOTTTTT
ggplot(top_sp_df, aes(x = reorder(how_obtained_code, -frequency), y = percentage)) +
  geom_bar(stat = "identity")
xlab(" ") +
  ylab("%") +
  ggtitle("Top 10 Species with most collisions")


# Lookkkk at that! 

library(RColorBrewer)
nb.cols <- 10
mycolors <- colorRampPalette(brewer.pal(9, "Pastel1"))(nb.cols)
mycolors

# Plot with colors 
ggplot(top_sp_df, aes(x = reorder(how_obtained_code, -frequency), y = percentage)) +
  geom_bar(stat = "identity", fill= mycolors) +
  xlab(" ") +
  ylab("%") +
  ggtitle("Top 10 Species with most collisions 1992-2017 ") 


# Next renaming to be common name instead of species code
# RTHA Red Tailed Hawk, BDOW Barred Owl, EASO Eastern Screech Owl, GHOW Great Horned Owl,OSPR  Osprey 
# RSHA Red-Shouldered Hawk, COHA Cooper's Hawk, MIKI Mississippi Kite, TUVU Turkey Vulture, BAEA Bald Eagle 

cleaned.species <- with(data, factor(Species_Abv, 
                                     levels = c('RTHA', 'BDOW', 'EASO', 'GHOW', 'OSPR', 'RSHA', 'COHA', 'MIKI', 'TUVU', 'BEA'), 
                                     labels = c("Red Tailed Hawk", "Barred Owl", "Eastern Screech Owl", "Great Horned Owl", "Osprey", "Red-Shouldered Hawk ", "Cooper's Hawk",
                                                "Mississippi Kite", "Turkey Vulture", "Bald Eagle")))


re.count <- table(cleaned.species)

re_percent <- round(prop.table(re.count) * 100, 2)

re.top_sp <- names(sort(re.count, decreasing = TRUE)[1:10])

re.data_sub <- data[cleaned.species %in% re.top_sp,]

re.sp_df <- as.data.frame(re.count)

colnames(re.sp_df) <- c("how_obtained_code", "frequency")

re.sp_df$percentage <- re_percent

re.top_sp_df <- re.sp_df[re.sp_df$how_obtained_code %in% re.top_sp,]

ggplot(re.top_sp_df, aes(x = reorder(how_obtained_code, -frequency), y = percentage)) +
  geom_bar(stat = "identity", fill= mycolors) +
  xlab(" ") +
  ylab("%") +
  ggtitle("Top 10 Species with most collisions 1992-2017 ") 

#Yurrrhhhhh 




cleaned.outcome <- with(data, factor(Disposition, 
                                        levels = c('RELEASED', 'NR/EUTHANIZED', 'TRANSFERRED', 'EUTHANIZED', 'NR/DIED', 'DIED', 'TRANSFERRED OUT'), 
                                        labels = c("Survived", "Dead", "Transferred", "Dead", "Dead", "Dead", "Transferred")))
cleaned.outcome
Dispo <- table(data$Disposition)
list(data$Disposition)

outcome.list <-list(cleaned.outcome)
table(outcome.list)

barplot(table(outcome.list))



-------------------------------------------------------------------------------------------------------------

#' Recieved new updated collision data from the ACC
# Read in data 

# Old Data 
url.3 <- "https://raw.githubusercontent.com/capcorso/Bird-Collision/main/Old.ACC.dat.csv"
old.data <- read_csv(url.3)

#New Data 
url.2 <- "https://raw.githubusercontent.com/capcorso/Bird-Collision/main/ACC.data%20-%20Sheet1.csv"
new.data <- read_csv(url.2)

# New data Species List 
new.sp.list <- new.data$Species
list(new.sp.list)

ct.new.sp <- table(new.sp.list)
sort(ct.new.sp)

# look at outcome 

outcome <- new.data$Status
list(outcome)
outcome.ct <- table(outcome)
sort(outcome.ct)

# Grouping euthanized and dead with - dead. And Released with -Survive, Still in rehab is NA
with(new.data, factor(Status, 
                      levels = c('DOA', 'R', 'D', 'EOA', 'D24', 'E24', 'E'), 
                      labels = c("Dead", "Survive", "Dead", "Dead", "Dead", "Dead", "Dead")))

cleaned.status <- with(new.data, factor(Status, 
                      levels = c('DOA', 'R', 'D', 'EOA', 'D24', 'E24', 'E'), 
                      labels = c("Dead", "Survive", "Dead", "Dead", "Dead", "Dead", "Dead")))

cleaned.status
Survivorship <- table(cleaned.status)
Survivorship
barplot.default(Survivorship)

# Same species colsion bar graph for new data 

new.sp_count <- table(new.data$Species)

new.sp_percent <- round(prop.table(new.sp_count) * 100, 2)

new.top_sp <- names(sort(new.sp_count, decreasing = TRUE)[1:10])

new.data_sub <- new.data[new.data$Species %in% new.top_sp,]

new.sp_df <- as.data.frame(new.sp_count)

colnames(new.sp_df) <- c("how_obtained_code", "frequency")

new.sp_df$percentage <- new.sp_percent

new.top_sp_df <- new.sp_df[new.sp_df$how_obtained_code %in% new.top_sp,]

# PLOTTTTT
ggplot(new.top_sp_df, aes(x = reorder(how_obtained_code, -frequency), y = percentage)) +
  geom_bar(stat = "identity") +
  xlab(" ") +
  ylab("%") +
  ggtitle("Top 10 Species with most collisions 2019-2023") 

# Plot with colors 
mycolors.2 <- colorRampPalette(brewer.pal(8, "Pastel2"))(nb.cols)
mycolors
ggplot(new.top_sp_df, aes(x = reorder(how_obtained_code, -frequency), y = percentage)) +
  geom_bar(stat = "identity", fill= mycolors.2) +
  xlab(" ") +
  ylab("%") +
  ggtitle("Top 10 Species with most collisions 2019-2023") 

-----------------------------------------------------------------------------------------------
#' Merging two data sets together 
#' 
#' Renaming columns so the column names match 

-- 
  --------------------------------------------
#' Binding work 
  
# Making columns similar names prior to binding 
  
colnames(new.data)
colnames(data)

names(new.data)[names(new.data) == "Species"] <- "Species_Name"
names(data)[names(data) == "Species_Abv"] <- "Species_Name"

names(new.data)[names(new.data) == "Status"] <- "Disposition"

colnames(data)
colnames(new.data)

# Binding 
library(plyr)
rbind.fill(data, new.data)

bind.data <- rbind.fill(data, new.data)

# This doesn't allow for my renaming of species ABV to species name's, our the outcomes to be dead or survived 

names(data)

-------------------------------------------------------------
# the right way to rename abreviations with species name 
sort(table(data$Species_Abv))

  for(i in seq(1, nrow(bind.data)))  {
    if(bind.data$Species_Name[i] == "RTHA")
      bind.data$Species_Name[i] = "Red-Tailed Hawk"
    else if(bind.data$Species_Name[i] == "BDOW")
      bind.data$Species_Name[i] = "Barred Owl"
    else if(bind.data$Species_Name[i] == "EASO")
      bind.data$Species_Name[i] = "Eastern Screech Owl"
    else if(bind.data$Species_Name[i] == "GHOW")
      bind.data$Species_Name[i] = "Great Horned Owl"
    else if(bind.data$Species_Name[i] == "OSPR")
      bind.data$Species_Name[i] = "Osprey"
    else if(bind.data$Species_Name[i] == "RSHA")
      bind.data$Species_Name[i] = "Red-Shouldered Hawk"
    else if(bind.data$Species_Name[i] == "COHA")
      bind.data$Species_Name[i] = "Coopers Hawk"
    else if(bind.data$Species_Name[i] == "MIKI")
      bind.data$Species_Name[i] = "Mississippi Kite"
    else if(bind.data$Species_Name[i] == "TUVU")
      bind.data$Species_Name[i] = "Turkey Vulture"
    else if(bind.data$Species_Name[i] == "BAEA")
      bind.data$Species_Name[i] = "Bald Eagle"
    else if(bind.data$Species_Name[i] == "BLVU")
      bind.data$Species_Name[i] = "Black Vulture"
    else if(bind.data$Species_Name[i] == "BRPE")
      bind.data$Species_Name[i] = "Brown Pelican"
    else if(bind.data$Species_Name[i] == "AMKE")
      bind.data$Species_Name[i] = "American Kestrel"
    else if(bind.data$Species_Name[i] == "SSHA")
      bind.data$Species_Name[i] = "Sharp-shinned Hawk"
    else if(bind.data$Species_Name[i] == "GBHE")
      bind.data$Species_Name[i] = "Great Blue Heron"
    else if(bind.data$Species_Name[i] == "COLO")
      bind.data$Species_Name[i] = "Common Loon"
    else if(bind.data$Species_Name[i] == "LAGU")
      bind.data$Species_Name[i] = "Laughing Gull"
    else if(bind.data$Species_Name[i] == "BWHA")
      bind.data$Species_Name[i] = "Broad-winged Hawk"
    else if(bind.data$Species_Name[i] == "BCNH")
      bind.data$Species_Name[i] = "Black-crowned Night-Heron"
    else if(bind.data$Species_Name[i] == "BNOW")
      bind.data$Species_Name[i] = "Barn Owl"
    else if(bind.data$Species_Name[i] == "BLSC")
      bind.data$Species_Name[i] = "Black Scoter"
    else if(bind.data$Species_Name[i] == "DCCO")
      bind.data$Species_Name[i] = "Double-crested Cormorant"
    else if(bind.data$Species_Name[i] == "GREG")
      bind.data$Species_Name[i] = "Great Egret"
    else if(bind.data$Species_Name[i] == "CLRA")
      bind.data$Species_Name[i] = "Clapper Rail"
  }

# Cleaning Binded disposition 
for(i in seq(1, nrow(bind.data))){
  if(bind.data$Disposition[i]== "Died"| 
     bind.data$Disposition[i]== "DIED"| 
     bind.data$Disposition[i]=="ESCAPED"|
     bind.data$Disposition[i]== "EUTHANIZED"|
     bind.data$Disposition[i]== "NR/DIED"| 
     bind.data$Disposition[i]== "NR/ESCAPED"| 
     bind.data$Disposition[i]== "NR/EUTHANIZED"| 
     bind.data$Disposition[i]== "TRANSFERRED/DIED"|
     bind.data$Disposition[i]== "TRANSFERRED/EUTHANIZED"| 
     bind.data$Disposition[i]=="D"| 
     bind.data$Disposition[i]=="D24"| 
     bind.data$Disposition[i]=="DOA"| 
     bind.data$Disposition[i]=="E"| 
     bind.data$Disposition[i]== "Euthanized"|
     bind.data$Disposition[i]=="E24"| 
     bind.data$Disposition[i]=="EOA")
  bind.data$Disposition[i] = "Died"
  else if(bind.data$Disposition[i] == "Active"| 
          bind.data$Disposition[i]=="ESCAPED"| 
          bind.data$Disposition[i]=="NR/ESCAPED"| 
          bind.data$Disposition[i]== "NR/RELEASED"| 
          bind.data$Disposition[i]=="Released"| 
          bind.data$Disposition[i]=="RELEASED"| 
          bind.data$Disposition[i]=="Self-Release"| 
          bind.data$Disposition[i]=="R")
    bind.data$Disposition[i] = "Survived"
  else if(bind.data$Disposition[i]== "TRANSFER OUT"| 
          bind.data$Disposition[i]=="Transferred"|
          bind.data$Disposition[i]=="TRANSFERRED"| 
          bind.data$Disposition[i]=="TRANSFERRED OUT"| 
          bind.data$Disposition[i]=="Reh"|
          bind.data$Disposition[i]== "PENDING"|
          bind.data$Disposition[i]== "")
    bind.data$Disposition[i] = "Transfer"
}
-------------------------------------------------------------------------------
# Now binding that with the new data 
library(plyr)
rbind.fill(data, new.data)

bind.data <- rbind.fill(data, new.data)

---------------------------------------------------------------------------------
# Now what to do with binded work ? 

--------------------------------------------------------------------------------
# Table work 
  
  Bind_species_table <- table(bind.data$Species_Name)
  Bind_species_percent_collision <- round(prop.table(Bind_species_table) * 100, 2)
  Bind_top10_spec <- names(sort(Bind_species_percent_collision, decreasing = TRUE)[1:10])
  Bind.data.sub <- bind.data[bind.data$Species_Name %in% Bind_top10_spec,]
  
  Bind_Species_table_DF <- as.data.frame(Bind_species_table)
  
  colnames(Bind_Species_table_DF) <- c("how_obtained_code", "frequency")
  Bind_Species_table_DF$percentage <- Bind_species_percent_collision
  
  Bind_top10_percent <- Bind_Species_table_DF[Bind_Species_table_DF$how_obtained_code %in% Bind_top10_spec,]
  
  bind_dat_subset <- subset(bind.data, Species_Name %in% c('Bald Eagle','Barred Owl','Coopers Hawk',
                                                           'Eastern Screech Owl', 'Great Horned Owl', 'Mississippi Kite ', 'Osprey', 
                                                           'Red-Shouldered Hawk', 'Red-Tailed Hawk', 'Turkey Vulture'))
  
  ggplot(Bind_top10_percent, aes(x = reorder(how_obtained_code, -frequency), y = percentage)) +
    geom_bar(stat = "identity") + 
    xlab(" ") +
    ylab("%") +
    ggtitle("Bind Data Trial")
  
  disposition_table <- bind.data$Disposition
  ggplot(bind.data, aes(x = Species_Name, fill=disposition_table)) +
    geom_bar() +
    xlab(" ") +
    ylab("%") +
    ggtitle("Bind Data Trial 2")
  
  Top10_with_Dispo <-ggplot(bind_dat_subset, aes(x = Species_Name, fill= Collision_Disposition)) +
    geom_bar() +
    xlab(" ") +
    ylab("%") +
    ggtitle("Top 10 Species Collisions with Disposition")
  
  Top10_with_Dispo + coord_flip()
  
  --------------------------------------------------------------------------------  

  Bind_dispo_table <- table(bind.data$Disposition)
  Bind_dispo_percent_collision <- round(prop.table(Bind_dispo_table) * 100, 2)
  Bind_top10_dis <- names(sort(Bind_dispo_percent_collision, decreasing = TRUE)[1:10])
  bind.disdata_sub <- bind.data[bind.data$Disposition %in% Bind_top10_dis,]
  
  Bind_dispo_table_DF <- as.data.frame(Bind_dispo_table)
  
  colnames(Bind_dispo_table_DF) <- c("how_obtained_code", "frequency")
  Bind_dispo_table_DF$percentage <- Bind_dispo_percent_collision
  
  Bind_top10dispo_percent <- Bind_dispo_table_DF[Bind_dispo_table_DF$how_obtained_code %in% Bind_top10_dis,]
  
  
  
  
  dispo_sp_table <- table(bind.data$Species_Name, bind.data$Disposition)
  
  dispo_sp_percent_collision <- round(prop.column(dispo_sp_table) * 100, 2)
  Bind_top10_spec <- names(sort(Bind_species_percent_collision, decreasing = TRUE)[1:10])
  Bind.data.sub <- bind.data[bind.data$Species_Name %in% Bind_top10_spec,]
  
  Bind_Species_table_DF <- as.data.frame(Bind_species_table)
  
  colnames(Bind_Species_table_DF) <- c("how_obtained_code", "frequency")
  Bind_Species_table_DF$percentage <- Bind_species_percent_collision
  
  Bind_top10_percent <- Bind_Species_table_DF[Bind_Species_table_DF$how_obtained_code %in% Bind_top10_spec,]
  
bind_dat_subset <- subset(bind.data, Species_Name %in% c('Bald Eagle','Barred Owl','Coopers Hawk',
        'Eastern Screech Owl', 'Great Horned Owl', 'Mississippi Kite ', 'Osprey', 
        'Red-Shouldered Hawk', 'Red-Tailed Hawk', 'Turkey Vulture'))
 

  -----------------------------------------------------------------------------

      

  
    
    
    
-------------------------------------------------------------------------

