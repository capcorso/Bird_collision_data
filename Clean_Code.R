# title: Corso Bird Collision Exploration
# author: Julianna 
# output: html_document

#' Load Data 
#' 
library(readr)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
library(plyr)

data <- read.csv('./data/Old.ACC.dat.csv')

url.2 <- "https://raw.githubusercontent.com/capcorso/Bird-Collision/main/ACC.data%20-%20Sheet1.csv"
new.data <- read_csv('./data/new.data.csv (1).csv')

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
mycolors <- colorRampPalette(brewer.pal(9, "Greens"))(nb.cols)
mycolors

ggplot(top_sp_df, aes(x = reorder(how_obtained_code, -frequency), y = percentage)) +
  geom_bar(stat = "identity", fill= mycolors) +
  xlab(" ") +
  ylab("%") +
  ggtitle("Top 10 Species with most collisions 1992-2017 ") 

# Replacing species code with common name, 're' 
cleaned.species <- with(data, factor(Species_Name, 
                                     levels = c('RTHA', 'BDOW', 'EASO', 'GHOW', 'OSPR', 'RSHA', 'COHA', 'MIKI', 'TUVU', 'BAEA'), 
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
new.sp.list <- new.data$Species_Name
list(new.sp.list)
ct.new.sp <- table(new.sp.list)
sort(ct.new.sp)

# look at outcome 
outcome <- new.data$Disposition
list(outcome)
outcome.ct <- table(outcome)
sort(outcome.ct)

# Grouping euthanized and dead with - dead. And Released with -Survive, Still in rehab is NA
cleaned.status <- with(new.data, factor(Disposition, 
                                        levels = c('DOA', 'R', 'D', 'EOA', 'D24', 'E24', 'E', 'Reh'), 
                                        labels = c("Dead", "Survive", "Dead", "Dead", "Dead", "Dead", "Dead", "Rehab")))

cleaned.status
Survivorship <- table(cleaned.status)
Survivorship
barplot.default(Survivorship)

# Same species colsion bar graph for new data 
new.sp_count <- table(new.data$Species_Name)
new.sp_percent <- round(prop.table(new.sp_count) * 100, 2)
new.top_sp <- names(sort(new.sp_count, decreasing = TRUE)[1:10])
new.data_sub <- new.data[new.data$Species_Name %in% new.top_sp,]
new.sp_df <- as.data.frame(new.sp_count)
colnames(new.sp_df) <- c("how_obtained_code", "frequency")
new.sp_df$percentage <- new.sp_percent
new.top_sp_df <- new.sp_df[new.sp_df$how_obtained_code %in% new.top_sp,]

mycolors.2 <- colorRampPalette(brewer.pal(8, "Blues"))(nb.cols)
mycolors

ggplot(new.top_sp_df, aes(x = reorder(how_obtained_code, -frequency), y = percentage)) +
  geom_bar(stat = "identity", fill= mycolors.2) +
  xlab(" ") +
  ylab("%") +
  ggtitle("Top 10 Species with most collisions 2017-2023") 

top_species_list <- c( )

top_sp_data <- subset(new.data, Species %in% top_sp_list)

__ This one doesnt work 
ggplot(new.data, aes(x = Species, fill=cleaned.status)) +
  geom_bar() +
  xlab(" ") +
  ylab("%") +
  ggtitle("Top 10 Species with most collisions") 

ggplot(new.data, aes(x = Species_Name, fill=cleaned.status)) +
  geom_bar() +
  xlab(" ") +
  ylab("%") +
  ggtitle("Species Collisions & Dispositions")

--------------------------------------------------------------------------------
# Making columns similar names prior to binding 

colnames(new.data)
colnames(data)

names(new.data)[names(new.data) == "Species"] <- "Species_Name"
names(data)[names(data) == "Species_Abv"] <- "Species_Name"

names(new.data)[names(new.data) == "Status"] <- "Disposition"

colnames(data)
colnames(new.data)
--------------------------------------------------------------------------------
# Binding 
  
library(plyr)
rbind.fill(data, new.data)

bind.data <- rbind.fill(data, new.data)
--------------------------------------------------------------------------------
# Filling species names in for species abreviations

  for(i in seq(1, nrow(data)))  {
    if(data$Species_Abv[i] == "RTHA")
      data$Species_Abv[i] = "Red-Tailed Hawk"
    else if(data$Species_Abv[i] == "BDOW")
      data$Species_Abv[i] = "Barred Owl"
    else if(data$Species_Abv[i] == "EASO")
      data$Species_Abv[i] = "Eastern Screech Owl"
    else if(data$Species_Abv[i] == "GHOW")
      data$Species_Abv[i] = "Great Horned Owl"
    else if(data$Species_Abv[i] == "OSPR")
      data$Species_Abv[i] = "Osprey"
    else if(data$Species_Abv[i] == "RSHA")
      data$Species_Abv[i] = "Red-Shouldered Hawk"
    else if(data$Species_Abv[i] == "COHA")
      data$Species_Abv[i] = "Coopers Hawk"
    else if(data$Species_Abv[i] == "MIKI")
      data$Species_Abv[i] = "Mississippi Kite"
    else if(data$Species_Abv[i] == "TUVU")
      data$Species_Abv[i] = "Turkey Vulture"
    else if(data$Species_Abv[i] == "BAEA")
      data$Species_Abv[i] = "Bald Eagle"
    else if(data$Species_Abv[i] == "BLVU")
      data$Species_Abv[i] = "Black Vulture"
    else if(data$Species_Abv[i] == "BRPE")
      data$Species_Abv[i] = "Brown Pelican"
    else if(data$Species_Abv[i] == "AMKE")
      data$Species_Abv[i] = "American Kestrel"
    else if(data$Species_Abv[i] == "SSHA")
      data$Species_Abv[i] = "Sharp-shinned Hawk"
    else if(data$Species_Abv[i] == "GBHE")
      data$Species_Abv[i] = "Great Blue Heron"
    else if(data$Species_Abv[i] == "COLO")
      data$Species_Abv[i] = "Common Loon"
    else if(data$Species_Abv[i] == "LAGU")
      data$Species_Abv[i] = "Laughing Gull"
    else if(data$Species_Abv[i] == "BWHA")
      data$Species_Abv[i] = "Broad-winged Hawk"
    else if(data$Species_Abv[i] == "BCNH")
      data$Species_Abv[i] = "Black-crowned Night-Heron"
    else if(data$Species_Abv[i] == "BNOW")
      data$Species_Abv[i] = "Barn Owl"
    else if(data$Species_Abv[i] == "BLSC")
      data$Species_Abv[i] = "Black Scoter"
    else if(data$Species_Abv[i] == "DCCO")
      data$Species_Abv[i] = "Double-crested Cormorant"
    else if(data$Species_Abv[i] == "GREG")
      data$Species_Abv[i] = "Great Egret"
    else if(data$Species_Abv[i] == "CLRA")
      data$Species_Abv[i] = "Clapper Rail"
  }

# Disposition cleaning 

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
          bind.data$Disposition[i]== "PENDING")
    bind.data$Disposition[i] = "Transfer"
}




-------------------------------------------------------------------------------
#' Model Building 
# 
Bind_species_table <- table(bind.data$Species_Name)
Bind_species_percent_collision <- round(prop.table(Bind_species_table) * 100, 2)
Bind_top10_spec <- names(sort(Bind_species_percent_collision, decreasing = TRUE)[1:10])
new.data_sub <- new.data[new.data$Species_Name %in% Bind_top10_spec,]

Bind_Species_table_DF <- as.data.frame(Bind_species_table)

colnames(Bind_Species_table_DF) <- c("how_obtained_code", "frequency")
Bind_Species_table_DF$percentage <- Bind_species_percent_collision
Bind_top10_percent <- Bind_Species_table_DF[Bind_Species_table_DF$how_obtained_code %in% Bind_top10_spec,]

ggplot(Bind_top10_percent, aes(x = reorder(how_obtained_code, -frequency), y = percentage)) +
  geom_bar(stat = "identity", fill= mycolors.2) +
  xlab(" ") +
  ylab("%") +
  ggtitle("Top 10 Species with most collisions combined data") 

# 


#Barred Owl Collision Graph 

Bar_Owl_subset <- subset(bind.data, Species_Name %in% c('Barred Owl'))
clean_Bar_Owlsub <- Bar_Owl_subset[ , c("Species_Name", "Year.Admitted", 
                                        "Disposition")]  

Disposition <- add_Bar_Owl$Group.2
add_Bar_Owl <- aggregate(clean_Bar_Owlsub$Species_Name, by=list(clean_Bar_Owlsub$Year.Admitted, 
                                                                clean_Bar_Owlsub$Disposition, clean_Bar_Owlsub$Species_Name), FUN=length)

ggplot(add_Bar_Owl, aes(x =Group.1, y = x, color = Disposition)) +
  geom_point(size=4) +
  xlab("Year Admitted") +
  ylab("Collisions") +
  ggtitle("Barred Owl Collisions since 1991")

Barred_Owl_Collision_Graph <- ggplot(add_Bar_Owl, aes(x =Group.1, y = x, color = Disposition)) +
  geom_point(size=4) +
  xlab("Year Admitted") +
  ylab("Collisions") +
  ggtitle("Barred Owl Collisions since 1991")



-------------------
  Lets get some stats 
Survivorship = c()
for(i in seq(1, nrow(bind.data))){
  if(bind.data$Disposition[i]=="Died") 
    Survivorship[i] = 0
  else if(bind.data$Disposition[i]== "Survived")
    Survivorship[i] = 1 
  else if(bind.data$Disposition[i]== "Transfer")
    Survivorship[i] = NA
}
bind.data = cbind(bind.data, Survivorship)
Survivorship = bind.data$Survivorship

glm_bind =glm(bind.data$Survivorship ~ bind.data$Species_Name + bind.data$Disposition)

summary(glm_bind)
plot(glm_bind)

glm_full = glm(full_set$Survive_full ~ full_set$Species_Abv + bind.data$Disposition)
summary(glm_full)
plot(glm_full)








