# title: Corso Bird Collision Exploration
# author: Julianna 
# output: html_document

#' Load Data 
#' 
library(readr)
library(ggplot2)
library(RColorBrewer)

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
  ggtitle("Top 10 Species with most collisions") 


# Next renaming to be common name instead of species code


# RTHA red tailed Hawk 

# BDOW Barred Owl 

# EASO Eastern Screech Owl 

# GHOW Great Horned Owl 

# OSPR  Osprey 

# RSHA Res-Shouldered Hawk 

# COHA Cooper's Hawk

# MIKI Mississippi Kite 

# TUVU Turkey Vulture 

# BAEA Bald Eagle 



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


