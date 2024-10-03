## R Project - Code

# Libraries used
library(readr)
library(readxl)
library(ggplot2)
library(waffle)
library(maps)
library(writexl)
library(dplyr)
library(ggrepel)
library(corrplot)
library(ggpmisc)
library(reshape2)

# Import Dataset
X2022_23_Players_Stats <- read_excel("Dataset/2022-23 Players Stats.xlsx")
View(X2022_23_Players_Stats) 

## Data Cleaning 
# Delete all rows that have at least one null value
X2022_23_Players_Stats_NoNA <- na.omit(X2022_23_Players_Stats_Complete)
# Delete all lines concerning players whose role is goalkeeper, defender and forward
X2022_23_Players_Stats_Mid <- subset(X2022_23_Players_Stats_NoNA, !Pos %in% c("GK", "DF", "FW"))
#Keep only the top 10 teams placed in each league
teams_to_be_removed <- c('Reims', 'Montpellier', 'Toulouse', 'Brest', 'Strasbourg','Nantes', 'Auxerre', 'Ajaccio', 'Troyes', 'Angers', 'Crystal Palace', 'Chelsea', 'Wolves', 'West Ham', 'Bournemouth', 'Nott\'ham Forest', 'Everton', 'Leicester City', 'Leeds United', 'Southampton', 'Monza', 'Udinese', 'Sassuolo', 'Empoli', 'Salernitana', 'Lecce', 'Hellas Verona', 'Spezia', 'Sampdoria', 'Rayo Vallecano', 'Sevilla', 'Celta Vigo', 'Cádiz', 'Getafe', 'Valencia', 'Almería', 'Valladolid', 'Espanyol', 'Elche', 'Köln', 'Hoffenheim', 'Werder Bremen', 'Bochum', 'Augsburg', 'Stuttgart', 'Schalke 04', 'Hertha BSC')
X2022_23_Players_Stats_FirstTeams <- subset(X2022_23_Players_Stats_Mid, !Squad %in% teams_to_be_removed)
# Keep only players who played more than 10 matches
X2022_23_Players_Stats_Games <- subset(X2022_23_Players_Stats_FirstTeams, MP > 10)
# Eliminate players with PPM = 0
X2022_23_Midfielders_Stats <- subset(X2022_23_Players_Stats_Games, PPM != 0)

# Export the new dataset 
write_xlsx(X2022_23_Midfielders_Stats, "X2022_23_Midfielders_Stats.xlsx")

## Analysis of data 

# Visualization of the map
leagues <- data.frame(
        Comp = c("Bundesliga", "La Liga", "Ligue 1", "Serie A", "Premier League"),
        State = c("Germany", "Spain", "France", "Italy", "England")
        )
leagues_color <- data.frame(
        Comp = c("Bundesliga", "La Liga", "Ligue 1", "Serie A", "Premier League"),
        Colour = c("red", "yellow", "purple", "green", "blue")
        )
ggplot() +
 geom_map(data = map_data("world"), map = map_data("world"),
         aes(x = long, y = lat, map_id = region),
         fill = "white", color = "black") +
 geom_map(data = leagues, map = map_data("world"),
         aes(fill = State, map_id = State), color = "black") +
 scale_fill_manual(values = leagues_color$Colour) +
 coord_cartesian(xlim = c(-10, 30), ylim = c(30, 70)) +
 theme_minimal()

# Distribution of midfielders per league
counts <- table(X2022_23_Midfielders_Stats$Comp)
data <- data.frame(Comp = names(counts), Count = as.numeric(counts))

histogram <- ggplot(data, aes(x = Comp, y = Count, fill = Comp)) +
        geom_bar(stat = "identity", color = "white") +
        geom_text(aes(label = Count), position = position_stack(vjust = 0.5), size = 3) +
        labs(title = "Distribution of midfielders per league", x = "League", y = "Count") +
        scale_fill_manual(values = c("Bundesliga" = "red", "Premier League" = "blue", "La Liga" = "yellow", "Serie A" = "green", "Ligue 1" = "purple")) +
        theme_minimal()
 
print(histogram)

# Comparison between the average age of the players in the different leagues with boxplots
ggplot(X2022_23_Midfielders_Stats, aes(x = Comp, y = Age, color = Comp)) +
        geom_boxplot(outlier.shape = NA) +
        geom_jitter(position = position_jitter(0.2), alpha = 0.5) +
        scale_color_manual(values = c("Bundesliga" = "RED", "Premier League" = "BLUE", "La Liga" = "YELLOW", "Serie A" = "GREEN", "Ligue 1" = "PURPLE")) +
        labs(title = "Age distribution of midfielders by league", x = "League", y = "Age") +
        theme_minimal()

# Waffle Chart on total goals and assists per league
num_gol_per_comp <- aggregate(Gls ~ Comp, data = X2022_23_Midfielders_Stats, FUN = sum)
print(num_gol_per_comp)

waffle(num_gol_per_comp$Gls,
         rows = 10,
         colors = c("Bundesliga" = "red", "Premier League" = "blue", "La Liga" = "yellow", "Serie A" = "green", "Ligue 1" = "purple"),
         title = list(
             label = "Total Goals by Competition",
             size = 0.8
         ))

percentage_goal_per_comp <- num_gol_per_comp$Gls / sum(num_gol_per_comp$Gls) * 100
print(percentage_goal_per_comp)

# Bubble Chart of Top 5 Scorers and Assistman per League 
top_players <- X2022_23_Midfielders_Stats %>%
        group_by(Comp) %>%
        arrange(desc(GA)) %>%
        mutate(rank = row_number()) %>%
        filter(rank <= 5) %>%
        ungroup()

ggplot(top_players, aes(x = Gls, y = Ast, size = PPM, label = Player, color = Comp)) +
        geom_point(alpha = 0.7) +
        geom_text_repel(box.padding = 0.5, point.padding = 0.1, size = 3, check_overlap = TRUE, 
                        nudge_y = 0.5, color = "black") +
        scale_size_continuous(range = c(3, 15)) +
        labs(title = "Bubble Chart of Top 5 Assist Scorers per League",
                x = "Gol",
                y = "Assist") +
        theme_minimal() +
        scale_color_manual(values = c("Premier League" = "blue", "Serie A" = "green", 
                                    "La Liga" = "yellow", "Bundesliga" = "red", 
                                    "Ligue 1" = "purple")) +
        theme(legend.position = "bottom")

# Bubble Chart of Top 5 Defensive Midfielders per League
top_def <- X2022_23_Midfielders_Stats %>%
        group_by(Comp) %>%
        arrange(desc(TkIn)) %>%
        mutate(rank = row_number()) %>%
        filter(rank <= 5) %>%
        ungroup()
 
ggplot(top_def, aes(x = Tkl_percent, y = Int, size = PPM, label = Player, color = Comp)) +
        geom_point(alpha = 0.7) +
        geom_text_repel(box.padding = 0.5, point.padding = 0.1, size = 3, check_overlap = TRUE, 
                        nudge_y = 0.5, color = "black") +
        scale_size_continuous(range = c(3, 15)) +
        labs(title = "Bubble Chart of Top 5 Defensive Midfielders per League",
                x = "Tackles (%)",
                y = "Interceptions") +
        theme_minimal() +
        scale_color_manual(values = c("Premier League" = "blue", "Serie A" = "green", 
                                    "La Liga" = "yellow", "Bundesliga" = "red", 
                                    "Ligue 1" = "purple")) +
      theme(legend.position = "bottom")


top_playmaker <- X2022_23_Midfielders_Stats %>%
      group_by(Comp) %>%
      arrange(desc(CmpTB)) %>%
      mutate(rank = row_number()) %>%
      filter(rank <= 5) %>%
      ungroup()
 
top_playmaker$PPM <- as.numeric(top_playmaker$PPM)

ggplot(top_playmaker, aes(x = Cmp_percent, y = TB, size = PPM, label = Player, color = Comp)) +
      geom_point(alpha = 0.7) +
      geom_text_repel(box.padding = 0.5, point.padding = 0.1, size = 3, 
                      nudge_y = 0.5, color = "black") +
      scale_size_continuous(range = c(3, 15)) +
      labs(title = "Bubble Chart of Top 5 Playmakers per League",
           x = "Completion Percentage (in %)",
           y = "Through Balls") +
      theme_minimal() +
      scale_color_manual(values = c("Premier League" = "blue", "Serie A" = "green", 
                                    "La Liga" = "yellow", "Bundesliga" = "red", 
                                    "Ligue 1" = "purple")) +
      theme(legend.position = "bottom")

## Pie Chart for goals and assists
# Top 20 Goal Scorers
top_scorers <- X2022_23_Midfielders_Stats %>%
     arrange(desc(Gls)) %>%
      head(20) 

ggplot(top_scorers, aes(x = "", y = Gls, fill = factor(1))) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y") +
      labs(title = "Top 20 Goal Scorers", fill = "") +
      scale_fill_manual(values = "sky blue", guide = "none") +
     theme_minimal() +
     theme(axis.text = element_blank(),
            axis.title = element_blank(),
            legend.position = "none") +
      geom_text(position = position_stack(vjust = 0.5), aes(label = Player), size = 3)

# Top 20 Assistmen
 top_assistants <- X2022_23_Midfielders_Stats %>%
      arrange(desc(Ast)) %>%
      head(20) %>%
      mutate(percentage = (Ast / sum(Ast)) * 100)
 
ggplot(top_assistants, aes(x = "", y = Ast, fill = factor(1))) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      geom_text(aes(label = paste0(Player, "\n", sprintf("%.1f%%", percentage))),
               position = position_stack(vjust = 0.5),
               size = 3) +
      coord_polar("y") +
      labs(title = "Top 20 Assistmen",
           fill = "") +
      scale_fill_manual(values = "sky blue", guide = "none") +
     theme_minimal() +
      theme(axis.text = element_blank(),
           axis.title = element_blank(),
            legend.position = "none")

# Top 20 Players with more Expected Goals
  top_ExpectedGoals <- X2022_23_Midfielders_Stats %>%
      arrange(desc(xG)) %>%
      head(20) %>%
     mutate(percentage = (xG / sum(xG)) * 100)
 
ggplot(top_ExpectedGoals, aes(x = "", y = xG, fill = factor(1))) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      geom_text(aes(label = paste0(Player, "\n", sprintf("%.1f%%", percentage))),
                position = position_stack(vjust = 0.5),
                size = 3) +
      coord_polar("y") +
      labs(title = "Top 20 Players with more Expected Goals",
           fill = "") +
      scale_fill_manual(values = "lightgreen", guide = "none") +
      theme_minimal() +
      theme(axis.text = element_blank(),
           axis.title = element_blank(),
            legend.position = "none")

# Top 20 Creative Players
  top_ShotCreatingActions <- X2022_23_Midfielders_Stats %>%
      arrange(desc(SCA90)) %>%
      head(20) %>%
      mutate(percentage = (SCA90 / sum(SCA90)) * 100)
  
ggplot(top_ShotCreatingActions, aes(x = "", y = SCA90, fill = factor(1))) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      geom_text(aes(label = paste0(Player, "\n", sprintf("%.1f%%", percentage))),
                position = position_stack(vjust = 0.5),
               size = 3) +
      coord_polar("y") +
      labs(title = "Top 20 Creative Players",
           fill = "") +
      scale_fill_manual(values = "slateblue2", guide = "none") +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            legend.position = "none")
 
# Top 20 Playmakers
  top_playmakers <- X2022_23_Midfielders_Stats %>%
      arrange(desc(Cmp_percent)) %>%
      head(20) %>%
      mutate(percentage = (Cmp_percent / sum(Cmp_percent)) * 100)
  
ggplot(top_playmakers, aes(x = "", y = Cmp_percent, fill = factor(1))) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      geom_text(aes(label = paste0(Player, "\n", sprintf("%.1f%%", percentage))),
                position = position_stack(vjust = 0.5),
                size = 3) +
      coord_polar("y") +
      labs(title = "Top 20 Playmakers",
           fill = "") +
      scale_fill_manual(values = "sienna3", guide = "none") +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            legend.position = "none")

# Top 20 Players with more Takles
top_tackles <- X2022_23_Midfielders_Stats %>%
      arrange(desc(Tkl_percent)) %>%
      head(20) %>%
      mutate(percentage = (Tkl_percent / sum(Tkl_percent)) * 100)
  
ggplot(top_tackles, aes(x = "", y = Tkl_percent, fill = factor(1))) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      geom_text(aes(label = paste0(Player, "\n", sprintf("%.1f%%", percentage))),
               position = position_stack(vjust = 0.5),
                size = 3) +
      coord_polar("y") +
      labs(title = "Top 20 Players with more Takles",
           fill = "") +
      scale_fill_manual(values = "wheat", guide = "none") +
     theme_minimal() +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            legend.position = "none")
  
# Top 20 Players with more Intercepts
top_intercept <- X2022_23_Midfielders_Stats %>%
     arrange(desc(Int)) %>%
     head(20) %>%
     mutate(percentage = (Int / sum(Int)) * 100)

ggplot(top_intercept, aes(x = "", y = Int, fill = factor(1))) +
     geom_bar(stat = "identity", width = 1, color = "white") +
     geom_text(aes(label = paste0(Player, "\n", sprintf("%.1f%%", percentage))),
                position = position_stack(vjust = 0.5),
                size = 3) +
      coord_polar("y") +
      labs(title = "Top 20 Players with more Intercepts",
           fill = "") +
      scale_fill_manual(values = "steelblue", guide = "none") +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
           legend.position = "none")


# Heatmap of Key Performance Metrics
X2022_23_Midfielders_Stats <- X2022_23_Midfielders_Stats %>%
     select(-CmpTB, -TkIn)
 selected_columns <- X2022_23_Midfielders_Stats[, 5:19]
 correlation_matrix <- cor(numeric_columns, use = "pairwise.complete.obs")
 color_palette <- colorRampPalette(c("blue", "white", "red"))(100)
 corrplot(correlation_matrix, method = "color", type = "upper", col = color_palette, addCoef.col = "black", title = "Heatmap of Key Performance Metrics")

# Multiple linear regression
linear_regression_model <- lm(PPM ~ Gls + Ast + xG + SCA90 + Cmp_percent + Tkl_percent + Int, data = X2022_23_Midfielders_Stats)
summary(linear_regression_model)

# Scatter Plot: PPM vs Cmp_percent
ggplot(X2022_23_Midfielders_Stats, aes(x = Cmp_percent, y = PPM)) +
     geom_point(color = "dodgerblue3") +
     geom_smooth(method = "lm", se = FALSE, color = "firebrick") +
     geom_text(aes(label = paste("Corr =", round(cor(Cmp_percent, PPM), 2))),
                     x = 2, y = 70, color = "black", size = 4, parse = TRUE) +
     labs(title = "Scatter Plot: PPM vs Cmp_percent",
                x = "Percentage of Completed Passes",
                y = "Points per Match") +
     theme_minimal()