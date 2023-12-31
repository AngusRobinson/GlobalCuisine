# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(plotly)

# Load the data
df <- read.csv("TidiedGlobalCuisineData.csv")

# (1) Overall how much people like each national cuisine.
graph1 <- df %>%
  group_by(Cuisine) %>%
  summarise(Like_pc = mean(Like_pc, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(Cuisine, Like_pc), y = Like_pc)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(title = "Overall popularity of each national cuisine",
       x = "Cuisine",
       y = "Proportion of foreigners who like it. (%)")

# (2) Overall how tolerant of other countries' cooking each nation is.
graph2 <- df %>%
  group_by(Respondent_Nationality) %>%
  summarise(Like_pc=mean(Like_pc, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(Respondent_Nationality, Like_pc), y = Like_pc)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(title = "Popularity of foreign cooking",
       x = "Nation",
       y = "Average percentage of population that likes a given foreign cuisine")

# (3) How popular each countries' cooking is against how tolerant they are.
popularity <- df %>%
  group_by(Cuisine) %>%
  summarise(Popularity = mean(Like_pc, na.rm = TRUE))

tolerance <- df %>%
  group_by(Respondent_Nationality) %>%
  summarise(Tolerance=mean(Like_pc, na.rm = TRUE))

pop_vs_tol <- merge(popularity, tolerance, by.x = "Cuisine", by.y = "Respondent_Nationality")

graph3 <- ggplot(pop_vs_tol, aes(x = Popularity, y = Tolerance)) +
  geom_point() +
  labs(title = "Popularity vs Tolerance",
       x = "Popularity (%)",
       y = "Tolerance (%)")

# (4) How much a country likes their own cooking against how tolerant they are.
own_cooking <- df[df$Cuisine == df$Respondent_Nationality,]

own_cooking_vs_tol <- merge(own_cooking, tolerance, by.x = "Cuisine", by.y = "Respondent_Nationality")

graph4 <- ggplot(own_cooking_vs_tol, aes(x = Like_a_lot_pc, y = Tolerance)) +
  geom_point() +
  labs(title = "Liking of own cooking vs Tolerance",
       x = "Liking of own cooking (%)",
       y = "Tolerance (%)")

# (5) How likely each countries' cuisine is to have been tried.
graph5 <- df %>%
  group_by(Cuisine) %>%
  summarise(Tried = mean(100 - Never_tried_pc, na.rm = TRUE)) %>%
  ggplot(mapping=aes(x = reorder(Cuisine, Tried), y = Tried)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(title = "Likelihood of each cuisine being tried",
       x = "Cuisine",
       y = "Tried (%)")

# (6) Whether it's been tried against whether it's liked.
graph6 <- df[df$Cuisine!=df$Respondent_Nationality,] %>%
  mutate(Tried = 100 - Never_tried_pc) %>%
  ggplot(mapping=aes(x = Tried, y = Like_pc,colour=Respondent_Nationality)) +
  #geom_point(alpha=0.5) +
  geom_smooth(se=FALSE)
  labs(title = "Whether a cuisine has been tried vs whether it's liked",
       x = "Tried (%)",
       y = "Popularity (%)")

# (7) Overall who likes who

graph7 <- ggplot() +
geom_line(data=summarise(group_by(df[df$Cuisine!=df$Respondent_Nationality,],Cuisine),Like_pc=mean(Like_pc)),mapping=aes(x=reorder(Cuisine,Like_pc),y=Like_pc,group=1))+
geom_point(data=df,mapping=aes(x=reorder(Cuisine,Like_pc),y=Like_pc,group=Respondent_Nationality,colour=Respondent_Nationality),alpha=0.3)+
coord_flip()+
labs(title = "Who likes whose food?",
      x = "Cuisine",
      y = "Popularity (%)")

# (8) How well-eaten populations are
graph8 <- df %>%
  group_by(Respondent_Nationality) %>%
  summarise(Tried_pc=mean(100-Never_tried_pc,na.rm=TRUE)) %>%
  ggplot(mapping=aes(x=reorder(Respondent_Nationality,Tried_pc),y=Tried_pc))+
  geom_bar(stat="identity") +
  coord_flip() +
  labs(title = "Breadth of culinary experience",
       x = "Cuisine",
       y = "Proportion of respondents who have tried the average foreign cuisine (%)")

green_theme <- theme(
  plot.background = element_rect(fill = "#e6ffe6"), # Pale green background
  panel.background = element_rect(fill = "#e6ffe6"),
  panel.grid.major = element_line(color = "#cceccc"), # Slightly different shade of green for grid
  
  axis.line = element_line(size = 0.5, color = "#339933"), # Adding solid axis lines
  axis.ticks = element_line(color = "#339933"), # Dark green axis lines
  axis.title = element_text(size = 14, face = "bold", color = "black", family = "Georgia"), # Dark green axis titles
  axis.text = element_text(size = 12, color = "black", family = "Georgia"), # Dark green axis text
  
  legend.background = element_rect(fill = "#f0fff0"), # Slightly different green for legend
  legend.title = element_text(face = "italic", color = "black"), # Italic, dark green legend title
  
  plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "black", family = "Georgia"), # Dark green, bold, centered title
  plot.subtitle = element_text(size = 14, hjust = 0.5, color = "#339933"), # Dark green, centered subtitle
  plot.caption = element_text(size = 10, hjust = 1, color = "#339933") # Dark green, right-aligned caption
)

# Create a list of ggplot objects
graph_list <- list(graph1, graph2, graph3, graph4, graph5, graph6, graph7, graph8)

# Create a folder in your working directory
if (!dir.exists("PlottedGraphs")) { # Check if folder exists
  dir.create("PlottedGraphs")
}

# Loop through the list, apply the theme, and save each plot
for (i in 1:length(graph_list)) {
  graph_with_theme <- graph_list[[i]] + green_theme  # Apply the theme
  ggsave(filename = paste0("PlottedGraphs/graph", i, ".png"), plot = graph_with_theme, width = 8, height = 6)
}
