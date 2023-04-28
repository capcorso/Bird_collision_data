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

df_1 <- read.csv('./data/Old.ACC.dat.csv')

df_2 <- read_csv('./data/new.data.csv (1).csv')

--------------------------------------------------------------------------------
#' Old Data Exploration

nrow(df_1)
#  9080 data entries 

# List Species 
species_list_1 = df_1$Species_Abv
list(species_list_1)

# species count 
sort(table(species_list_1))
--------------------------------------------------------------------------------
#'' Cleaning Data Frames

# Making Species, and disposition columns the same name between data frame's, 
 #in order to bind later

names(df_2)[names(df_2) == "Species"] <- "Species_Name"
names(df_1)[names(df_1) == "Species_Abv"] <- "Species_Name"

names(df_2)[names(df_2) == "Status"] <- "Disposition"

colnames(df_1)
colnames(df_2) 

# Columns shared are Species_Name and Disposition 


#' Data Frame 1 Cleaning 
#'
# Species Name's

sort(table(df_1$Species_Name))

  for(i in seq(1, nrow(df_1)))  {
    if(df_1$Species_Name[i] == "RTHA")
      df_1$Species_Name[i] = "Red-Tailed Hawk"
    else if(df_1$Species_Name[i] == "BDOW")
      df_1$Species_Name[i] = "Barred Owl"
    else if(df_1$Species_Name[i] == "EASO")
      df_1$Species_Name[i] = "Eastern Screech Owl"
    else if(df_1$Species_Name[i] == "GHOW")
      df_1$Species_Name[i] = "Great Horned Owl"
    else if(df_1$Species_Name[i] == "OSPR")
      df_1$Species_Name[i] = "Osprey"
    else if(df_1$Species_Name[i] == "RSHA")
      df_1$Species_Name[i] = "Red-Shouldered Hawk"
    else if(df_1$Species_Name[i] == "COHA")
      df_1$Species_Name[i] = "Coopers Hawk"
    else if(df_1$Species_Name[i] == "MIKI")
      df_1$Species_Name[i] = "Mississippi Kite"
    else if(df_1$Species_Name[i] == "TUVU")
      df_1$Species_Name[i] = "Turkey Vulture"
    else if(df_1$Species_Name[i] == "BAEA")
      df_1$Species_Name[i] = "Bald Eagle"
    else if(df_1$Species_Name[i] == "BLVU")
      df_1$Species_Name[i] = "Black Vulture"
    else if(df_1$Species_Name[i] == "BRPE")
      df_1$Species_Name[i] = "Brown Pelican"
    else if(df_1$Species_Name[i] == "AMKE")
      df_1$Species_Name[i] = "American Kestrel"
    else if(df_1$Species_Name[i] == "SSHA")
      df_1$Species_Name[i] = "Sharp-shinned Hawk"
    else if(df_1$Species_Name[i] == "GBHE")
      df_1$Species_Name[i] = "Great Blue Heron"
    else if(df_1$Species_Name[i] == "COLO")
      df_1$Species_Name[i] = "Common Loon"
    else if(df_1$Species_Name[i] == "LAGU")
      df_1$Species_Name[i] = "Laughing Gull"
    else if(df_1$Species_Name[i] == "BWHA")
      df_1$Species_Name[i] = "Broad-winged Hawk"
    else if(df_1$Species_Name[i] == "BCNH")
      df_1$Species_Name[i] = "Black-crowned Night-Heron"
    else if(df_1$Species_Name[i] == "BNOW")
      df_1$Species_Name[i] = "Barn Owl"
    else if(df_1$Species_Name[i] == "BLSC")
      df_1$Species_Name[i] = "Black Scoter"
    else if(df_1$Species_Name[i] == "DCCO")
      df_1$Species_Name[i] = "Double-crested Cormorant"
    else if(df_1$Species_Name[i] == "GREG")
      df_1$Species_Name[i] = "Great Egret"
    else if(df_1$Species_Name[i] == "CLRA")
      df_1$Species_Name[i] = "Clapper Rail"
  }

# Disposition 

sort(table(df_1$Disposition))

for(i in seq(1, nrow(df_1))){
  if(df_1$Disposition[i]== "Died"| 
     df_1$Disposition[i]== "DIED"| 
     df_1$Disposition[i]=="ESCAPED"|
     df_1$Disposition[i]== "EUTHANIZED"|
     df_1$Disposition[i]== "NR/DIED"| 
     df_1$Disposition[i]== "NR/ESCAPED"| 
     df_1$Disposition[i]== "NR/EUTHANIZED"| 
     df_1$Disposition[i]== "TRANSFERRED/DIED"|
     df_1$Disposition[i]== "TRANSFERRED/EUTHANIZED"| 
     df_1$Disposition[i]=="D"| 
     df_1$Disposition[i]=="D24"| 
     df_1$Disposition[i]=="DOA"| 
     df_1$Disposition[i]=="E"| 
     df_1$Disposition[i]== "Euthanized"|
     df_1$Disposition[i]=="E24"| 
     df_1$Disposition[i]=="EOA")
  df_1$Disposition[i] = "Died"
  else if(df_1$Disposition[i] == "Active"| 
          df_1$Disposition[i]=="ESCAPED"| 
          df_1$Disposition[i]=="NR/ESCAPED"| 
          df_1$Disposition[i]== "NR/RELEASED"| 
          df_1$Disposition[i]=="Released"| 
          df_1$Disposition[i]=="RELEASED"| 
          df_1$Disposition[i]=="Self-Release"| 
          df_1$Disposition[i]=="R")
    df_1$Disposition[i] = "Survived"
  else if(df_1$Disposition[i]== "TRANSFER OUT"| 
          df_1$Disposition[i]=="Transferred"|
          df_1$Disposition[i]=="TRANSFERRED"| 
          df_1$Disposition[i]=="TRANSFERRED OUT"| 
          df_1$Disposition[i]=="Reh")
    df_1$Disposition[i] = "Transfer"
  else if(df_1$Disposition[i]== ""|
          df_1$Disposition[i]== "PENDING")
    df_1$Disposition[i] = "Unknown"
}

#' Data Frame 2

# Species Name's

sort(table(df_2$Species_Name))

for(i in seq(1, nrow(df_2)))  {
  if(df_2$Species_Name[i] == "RTHA")
    df_2$Species_Name[i] = "Red-Tailed Hawk"
  else if(df_2$Species_Name[i] == "BDOW")
    df_2$Species_Name[i] = "Barred Owl"
  else if(df_2$Species_Name[i] == "EASO")
    df_2$Species_Name[i] = "Eastern Screech Owl"
  else if(df_2$Species_Name[i] == "GHOW")
    df_2$Species_Name[i] = "Great Horned Owl"
  else if(df_2$Species_Name[i] == "OSPR")
    df_2$Species_Name[i] = "Osprey"
  else if(df_2$Species_Name[i] == "RSHA")
    df_2$Species_Name[i] = "Red-Shouldered Hawk"
  else if(df_2$Species_Name[i] == "COHA")
    df_2$Species_Name[i] = "Coopers Hawk"
  else if(df_2$Species_Name[i] == "MIKI")
    df_2$Species_Name[i] = "Mississippi Kite"
  else if(df_2$Species_Name[i] == "TUVU")
    df_2$Species_Name[i] = "Turkey Vulture"
  else if(df_2$Species_Name[i] == "BAEA")
    df_2$Species_Name[i] = "Bald Eagle"
  else if(df_2$Species_Name[i] == "BLVU")
    df_2$Species_Name[i] = "Black Vulture"
  else if(df_2$Species_Name[i] == "BRPE")
    df_2$Species_Name[i] = "Brown Pelican"
  else if(df_2$Species_Name[i] == "AMKE")
    df_2$Species_Name[i] = "American Kestrel"
  else if(df_2$Species_Name[i] == "SSHA")
    df_2$Species_Name[i] = "Sharp-shinned Hawk"
  else if(df_2$Species_Name[i] == "GBHE")
    df_2$Species_Name[i] = "Great Blue Heron"
  else if(df_2$Species_Name[i] == "COLO")
    df_2$Species_Name[i] = "Common Loon"
  else if(df_2$Species_Name[i] == "LAGU")
    df_2$Species_Name[i] = "Laughing Gull"
  else if(df_2$Species_Name[i] == "BWHA")
    df_2$Species_Name[i] = "Broad-winged Hawk"
  else if(df_2$Species_Name[i] == "BCNH")
    df_2$Species_Name[i] = "Black-crowned Night-Heron"
  else if(df_2$Species_Name[i] == "BNOW")
    df_2$Species_Name[i] = "Barn Owl"
  else if(df_2$Species_Name[i] == "BLSC")
    df_2$Species_Name[i] = "Black Scoter"
  else if(df_2$Species_Name[i] == "DCCO")
    df_2$Species_Name[i] = "Double-crested Cormorant"
  else if(df_2$Species_Name[i] == "GREG")
    df_2$Species_Name[i] = "Great Egret"
  else if(df_2$Species_Name[i] == "CLRA")
    df_2$Species_Name[i] = "Clapper Rail"
}

# Disposition 

table(df_2$Disposition)

for(i in seq(1, nrow(df_2))){
  if(df_2$Disposition[i]=="D"| 
     df_2$Disposition[i]=="D24"| 
     df_2$Disposition[i]=="DOA"| 
     df_2$Disposition[i]=="E"|
     df_2$Disposition[i]=="E24"| 
     df_2$Disposition[i]=="EOA")
  df_2$Disposition[i] = "Died"
  else if(df_2$Disposition[i]=="R")
    df_2$Disposition[i] = "Survived"
  else if(df_2$Disposition[i]== "Reh")
    df_2$Disposition[i] = "Transfer"
}

--------------------------------------------------------------------------------

#'Data Frame 1 Model 1  
# Top 10 species with the most collisions by percent of collisions df_1

sp_table_1 <- table(df_1$Species_Name)
sp_percent_collision_1 <- round(prop.table(sp_table_1) * 100, 2)
top10sp_names_1 <- names(sort(sp_table_1, decreasing = TRUE)[1:10])
df1_subset_top10sp <- df_1[df_1$Species_Name %in% top10sp_names_1,]
sp_table_1_df <- as.data.frame(sp_table_1)
colnames(sp_table_1_df ) <- c("Species_Name", "Frequency")
sp_table_1_df$percentage <- sp_percent_collision_1
df1_subset_top10sp <- sp_table_1_df [sp_table_1_df$Species_Name %in% top10sp_names_1,]

# In order to scale table 
df1_subset_top10sp$percentage <- as.numeric(df1_subset_top10sp$percentage)

# Model 1 
ggplot(df1_subset_top10sp, aes(x = reorder(Species_Name, -Frequency), y = percentage)) +
  geom_bar(stat = "identity") +
  xlab(" ") +
  ylab("%") +
  ggtitle("Top 10 Species with most collisions 1992-2017 ") 

# Lets make it green 
nb.cols <- 10
df1_model1_color <- colorRampPalette(brewer.pal(9, "Greens"))(10)

ggplot(df1_subset_top10sp, aes(x = reorder(Species_Name, -Frequency), y = percentage)) +
  geom_bar(stat = "identity", fill= df1_model1_color) +
  xlab(" ") +
  ylab("%") +
  ggtitle("Top 10 Species with most collisions 1992-2017")

df1_top10_graph <-
  ggplot(df1_subset_top10sp, aes(x = reorder(Species_Name, -Frequency), y = percentage)) +
  geom_bar(stat = "identity", fill= df1_model1_color) +
  xlab(" ") +
  ylab("%") +
  ggtitle("Top 10 Species with most collisions 1992-2017") 

# Data Frame 1 Model 1-- Top 10 Species with most collisions 1992-2017 

--------------------------------------------------------------------------------
#'Data Frame 2 Model 2  
# Top 10 species with the most collisions by percent of collisions df_2
  
sp_table_2 <- table(df_2$Species_Name)
sp_percent_collision_2 <- round(prop.table(sp_table_2) * 100, 2)
top10sp_names_2 <- names(sort(sp_table_2, decreasing = TRUE)[1:10])
df2_subset_top10sp <- df_2[df_2$Species_Name %in% top10sp_names_2,]
sp_table_2_df <- as.data.frame(sp_table_2)
colnames(sp_table_2_df ) <- c("Species_Name", "Frequency")
sp_table_2_df$percentage <- sp_percent_collision_2
df2_subset_top10sp <- sp_table_2_df [sp_table_2_df$Species_Name %in% top10sp_names_2,]

# In order to scale table 
df2_subset_top10sp$percentage <- as.numeric(df2_subset_top10sp$percentage)

# Lets make it Blue 
df2_model2_color <- colorRampPalette(brewer.pal(9, "Blues"))(10)


#Model 2 
ggplot(df1_subset_top10sp, aes(x = reorder(Species_Name, -Frequency), y = percentage)) +
  geom_bar(stat = "identity", fill= df2_model2_color) +
  xlab(" ") +
  ylab("%") +
  ggtitle("Top 10 Species with most collisions 2019-2023")

df2_top10_graph <-
  ggplot(df1_subset_top10sp, aes(x = reorder(Species_Name, -Frequency), y = percentage)) +
  geom_bar(stat = "identity", fill= df2_model2_color) +
  xlab(" ") +
  ylab("%") +
  ggtitle("Top 10 Species with most collisions 2019-2023")

# Data Frame 2 Model 2-- Top 10 Species with most collisions 2019-2023 

--------------------------------------------------------------------------------
  









  
  
  

# Same species colsion bar graph for new data 
new.sp_count <- table(new.data$Species_Name)
new.sp_percent <- round(prop.table(new.sp_count) * 100, 2)
new.top_sp <- names(sort(new.sp_count, decreasing = TRUE)[1:10])
new.data_sub <- new.data[new.data$Species_Name %in% new.top_sp,]
new.sp_df <- as.data.frame(new.sp_count)
colnames(new.sp_df) <- c("how_obtained_code", "frequency")
new.sp_df$percentage <- new.sp_percent
new.df1_subset_top10sp <- new.sp_df[new.sp_df$how_obtained_code %in% new.top_sp,]

mycolors.2 <- colorRampPalette(brewer.pal(8, "Blues"))(nb.cols)
mycolors

ggplot(new.df1_subset_top10sp, aes(x = reorder(how_obtained_code, -frequency), y = percentage)) +
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








