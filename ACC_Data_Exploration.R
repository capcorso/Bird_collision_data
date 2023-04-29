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
--------------------------------------------------------------------------------

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

sort(table(df_1$Species_Name))
sort(table(df_2$Species_Name))

table(df_1$Disposition)
table(df_2$Disposition)
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
df1_subset_top10sp <- sp_table_1_df [sp_table_1_df$Species_Name
                                     %in% top10sp_names_1,]

# In order to scale table 
df1_subset_top10sp$percentage <- as.numeric(df1_subset_top10sp$percentage)

# Model 1 
ggplot(df1_subset_top10sp, aes(x = reorder(Species_Name, -Frequency), 
                               y = percentage)) +
  geom_bar(stat = "identity") +
  xlab(" ") +
  ylab("%") +
  ggtitle("Top 10 Species with most collisions 1992-2017 ") 

# Lets make it green 
nb.cols <- 10
df1_model1_color <- colorRampPalette(brewer.pal(9, "Greens"))(10)

df1_top10_graph <-
  ggplot(df1_subset_top10sp, aes(x = reorder(Species_Name, -Frequency), 
                                 y = percentage)) +
  geom_bar(stat = "identity", fill= df1_model1_color) +
  xlab(" ") +
  ylab("%") +
  ggtitle("Top 10 Species with most collisions 1992-2017") 

# Data Frame 1 Model 1-- Top 10 Species with most collisions 1992-2017 
df1_top10_graph

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
df2_subset_top10sp <- sp_table_2_df [sp_table_2_df$Species_Name 
                                     %in% top10sp_names_2,]

# In order to scale table 
df2_subset_top10sp$percentage <- as.numeric(df2_subset_top10sp$percentage)

# Lets make it Blue 
df2_model2_color <- colorRampPalette(brewer.pal(9, "Blues"))(10)

#Model 2 

df2_top10_graph <-
  ggplot(df2_subset_top10sp, aes(x = reorder(Species_Name, -Frequency), y = percentage)) +
  geom_bar(stat = "identity", fill= df2_model2_color) +
  xlab(" ") +
  ylab("%") +
  ggtitle("Top 10 Species with most collisions 2019-2023")

# Data Frame 2 Model 2-- Top 10 Species with most collisions 2019-2023 

df2_top10_graph

--------------------------------------------------------------------------------
#' Binding Data frame 1 and 2 to make Data frame 3
  
library(plyr)
rbind.fill(df_1,df_2)

# name the binded data data frame 3 -- df_3
df_3 <- rbind.fill(df_1, df_2)

-------------------------------------------------------------------------------
#' Binded data Models 
#
#' Data Frame 3 Model 3
# Top 10 species with the most collisions by percent of collisions binded data

sp_table_3 <- table(df_3$Species_Name)
sp_percent_collision_3 <- round(prop.table(sp_table_3) * 100, 2)
top10sp_names_3 <- names(sort(sp_table_3, decreasing = TRUE)[1:10])
df3_subset_top10sp <- df_3[df_3$Species_Name %in% top10sp_names_3,]
sp_table_3_df <- as.data.frame(sp_table_3)
colnames(sp_table_3_df ) <- c("Species_Name", "Frequency")
sp_table_3_df$percentage <- sp_percent_collision_3
df3_subset_top10sp <- sp_table_3_df [sp_table_3_df$Species_Name %in% top10sp_names_3,]

# In order to scale table 
df3_subset_top10sp$percentage <- as.numeric(df3_subset_top10sp$percentage)

# Lets make it Purple  
df3_model3_color <- colorRampPalette(brewer.pal(9, "Purples"))(10)

#Model 3 

df3_top10_graph <-
  ggplot(df3_subset_top10sp, aes(x = reorder(Species_Name, -Frequency), y = percentage)) +
  geom_bar(stat = "identity", fill= df3_model3_color) +
  xlab(" ") +
  ylab("%") +
  ggtitle("Top 10 Species with most collisions 1992 - 2023")

df3_top10_graph 

# Data Frame 3 Model 3-- Top 10 Species with most collisions 1992-2023 

-------------------------------------------------------------------------------
#' Creating a top 10 species bar graph with disposition stats 
#
# Creating a data frame 3 subset to separate the top 10 collided species

df3_subset <- subset(df_3, Species_Name %in% c('Red-Tailed Hawk',
                                 'Barred Owl',
                                 'Eastern Screech Owl',
                                 'Eastern Screech Owl', 
                                 'Great Horned Owl', 
                                 'Mississippi Kite ', 
                                 'Osprey', 
                                 'Red-Shouldered Hawk', 
                                 'Turkey Vulture', 
                                 'Coopers Hawk', 
                                 'Bald Eagle'))

disposition_data <- df3_subset$Disposition


ggplot(df3_subset, aes(x = Species_Name, fill= disposition_data)) +
  geom_bar() +
  xlab(" ") +
  ylab("%") +
  ggtitle("Top 10 Species Collisions with disposition")

# sorting from most collisions to least 

ggplot(df3_subset, aes(x = factor(Species_Name, 
                  level=c('Red-Tailed Hawk', 'Barred Owl','Eastern Screech Owl', 
                  'Great Horned Owl', 'Osprey', 'Red-Shouldered Hawk', 
                  'Coopers Hawk', 'Mississippi Kite', 'Turkey Vulture',
                  'Bald Eagle')), fill= disposition_data)) +
  geom_bar() +
  xlab(" ") +
  ylab("Collisions") +
  labs(fill = "Disposition") +
  ggtitle("Top 10 species collisions with disposition")

model_4 <-
  ggplot(df3_subset, aes(x = 
      factor(Species_Name, 
      level=c('Red-Tailed Hawk', 'Barred Owl','Eastern Screech Owl', 
              'Great Horned Owl', 'Osprey', 'Red-Shouldered Hawk', 
              'Coopers Hawk', 'Mississippi Kite', 'Turkey Vulture',
              'Bald Eagle')), fill= disposition_data)) +
      geom_bar() +
      xlab(" ") +
      ylab("Collisions") +
      labs(fill = "Disposition") +
      ggtitle("Top 10 species collisions with disposition")

Disposition_Collision_Graph <-
  model_4 + coord_flip()

Disposition_Collision_Graph
-------------------------------------------------------------------------------
#' Red-Tailed Hawk Collision graph 
RTHW_subset <- subset(df_3, Species_Name %in% c('Red-Tailed Hawk'))
RTHW_sub_subset <- RTHW_subset[ , c("Species_Name", "Year.Admitted", 
                                        "Disposition")]  

agg_RTHW <- aggregate(RTHW_sub_subset$Species_Name, 
                         by=list(RTHW_sub_subset$Year.Admitted, 
                                 RTHW_sub_subset$Disposition, 
                                 RTHW_sub_subset$Species_Name), 
                         FUN=length)

disp_RTHW <- agg_RTHW$Group.2

model_5 <- 
  ggplot(agg_RTHW, aes(x =Group.1, y = x, color = disp_RTHW)) +
  geom_point(size=4) +
  xlab("Year Admitted") +
  ylab("Collisions") + 
  ggtitle("Red-Tailed Hawk Yearly Collisions")

RTHW_Collision_Graph <-
  model_5 + scale_color_discrete(name = "Disposition")

RTHW_Collision_Graph
-------------------------------------------------------------------------------
#Barred Owl Collision Graph 

Bar_Owl_subset <- subset(df_3, Species_Name %in% c('Barred Owl'))
Bar_Owl_sub_subset <- Bar_Owl_subset[ , c("Species_Name", "Year.Admitted", 
                                        "Disposition")]  

agg_Bar_Owl <- aggregate(Bar_Owl_sub_subset$Species_Name, 
                         by=list(Bar_Owl_sub_subset$Year.Admitted, 
                                 Bar_Owl_sub_subset$Disposition, 
                                 Bar_Owl_sub_subset$Species_Name), 
                         FUN=length)

disp_Bar_Owl <- agg_Bar_Owl$Group.2

model_6 <- 
  ggplot(agg_Bar_Owl, aes(x =Group.1, y = x, color = disp_Bar_Owl)) +
  geom_point(size=4) +
  xlab("Year Admitted") +
  ylab("Collisions") +
  labs(fill = "Disposition") +
  ggtitle("Barred Owl Yearly Collisions")

Barred_Owl_Collision_Graph <-
  model_6 + scale_color_discrete(name = "Disposition")

Barred_Owl_Collision_Graph
-------------------------------------------------------------------------------
#' Eastern Screech Owl
EASO_subset <- subset(df_3, Species_Name %in% c('Eastern Screech Owl'))
EASO_sub_subset <- EASO_subset[ , c("Species_Name", "Year.Admitted", 
                                    "Disposition")]  

agg_EASO<- aggregate(EASO_sub_subset$Species_Name, 
                      by=list(EASO_sub_subset$Year.Admitted, 
                              EASO_sub_subset$Disposition, 
                              EASO_sub_subset$Species_Name), 
                      FUN=length)

disp_EASO <- agg_EASO$Group.2

model_7 <- 
  ggplot(agg_EASO, aes(x =Group.1, y = x, color = disp_EASO)) +
  geom_point(size=4) +
  xlab("Year Admitted") +
  ylab("Collisions") + 
  ggtitle("Eastern Screech Owl Yearly Collisions")

EASO_Collision_Graph <-
  model_7 + scale_color_discrete(name = "Disposition")

EASO_Collision_Graph
-------------------------------------------------------------------------------
#' Great Horned Owl Collision graph 
GHOW_subset <- subset(df_3, Species_Name %in% c('Great Horned Owl'))
GHOW_sub_subset <- GHOW_subset[ , c("Species_Name", "Year.Admitted", 
                                    "Disposition")]  

agg_GHOW <- aggregate(GHOW_sub_subset$Species_Name, 
                      by=list(GHOW_sub_subset$Year.Admitted, 
                              GHOW_sub_subset$Disposition, 
                              GHOW_sub_subset$Species_Name), 
                      FUN=length)

disp_GHOW <- agg_GHOW$Group.2

model_8 <- 
  ggplot(agg_GHOW, aes(x =Group.1, y = x, color = Disp_GHOW)) +
  geom_point(size=4) +
  xlab("Year Admitted") +
  ylab("Collisions") + 
  ggtitle("Great Horned Owl Yearly Collisions")

GHOW_Collision_Graph <-
  model_5 + scale_color_discrete(name = "Disposition")

GHOW_Collision_Graph
-------------------------------------------------------------------------------
#' Graphs 

  df1_top10_graph

  df2_top10_graph
  
  df3_top10_graph
  
  Disposition_Collision_Graph
  
  Barred_Owl_Collision_Graph 
  
  RTHW_Collision_Graph
  
  EASO_Collision_Graph
  
  GHOW_Collision_Graph
  
  




