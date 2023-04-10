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

url.2 <- "https://raw.githubusercontent.com/capcorso/Bird-Collision/main/ACC.data%20-%20Sheet1.csv"
new.data <- read_csv(url.2)

--------------------------------------------------------------------------------
#' Old Data Exploration

n_rows <- nrow(data)
cat("Number of data entries:", n_rows)
#  9080 data entries 

# List Species 
species_list = data$Species_Abv
list(species_list)

# species count 
count_species = table(species_list)
sort(count_species)

# Top 10 species with the most collisions by percent of collisions 

sp_count <- table(data$Species_Abv)
sp_percent <- round(prop.table(sp_count) * 100, 2)
top_sp <- names(sort(sp_count, decreasing = TRUE)[1:10])
data_sub <- data[data$Species_Abv %in% top_sp,]
sp_df <- as.data.frame(sp_count)
colnames(sp_df) <- c("how_obtained_code", "frequency")
sp_df$percentage <- sp_percent
top_sp_df <- sp_df[sp_df$how_obtained_code %in% top_sp,]

nb.cols <- 10
mycolors <- colorRampPalette(brewer.pal(9, "Pastel1"))(nb.cols)
mycolors

ggplot(top_sp_df, aes(x = reorder(how_obtained_code, -frequency), y = percentage)) +
  geom_bar(stat = "identity", fill= mycolors) +
  xlab(" ") +
  ylab("%") +
  ggtitle("Top 10 Species with most collisions 1992-2017 ") 

# Replacing species code with common name, 're' 
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

# Looking at Outcome
cleaned.outcome <- with(data, factor(Disposition, 
                                     levels = c('RELEASED', 'NR/EUTHANIZED', 'TRANSFERRED', 'EUTHANIZED', 'NR/DIED', 'DIED', 'TRANSFERRED OUT'), 
                                     labels = c("Survived", "Dead", "Transferred", "Dead", "Dead", "Dead", "Transferred")))
cleaned.outcome
Dispo <- table(data$Disposition)
list(data$Disposition)

outcome.list <-list(cleaned.outcome)
table(outcome.list)

barplot(table(outcome.list))

--------------------------------------------------------------------------------
#' New Data Exploration 'new'

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

mycolors.2 <- colorRampPalette(brewer.pal(8, "Pastel2"))(nb.cols)
mycolors

ggplot(new.top_sp_df, aes(x = reorder(how_obtained_code, -frequency), y = percentage)) +
  geom_bar(stat = "identity", fill= mycolors.2) +
  xlab(" ") +
  ylab("%") +
  ggtitle("Top 10 Species with most collisions") 





