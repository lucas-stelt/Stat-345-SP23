# Necessary Packages for Project 
library(devtools)
devtools::install_github("abresler/nbastatR")
library("nbastatR")
library(ggplot2)
library(nbastatR)
library(ggplot2)
library(tidyverse)
library(extrafont)
library(cowplot)
library(sportyR)
library(gapminder)
library(gganimate)
library(gifski)
library(png)
library(ggthemes)

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072*100)

# Vector of year interval that shot data is gathered from 
year_interval = c(1997:2020)

# Function to assemble data from each year
# Inputs: a vector of year interval
# Outputs: Toronto Raptors shot data for each year 
function_year = function(x) {
  teams_shots(teams = "Toronto Raptors", seasons = x)
}

# Apply the year function to each year in interval
years = lapply(year_interval, function_year)

# Bind the data from each year together 
raptors_23yearShots = list_rbind(years)

# Creating Toronto Raptors shot chart for 23 years 
raptors_23yearShots_chart = 
  
# Code for court from Github (https://github.com/DomSamangy/R_Tutorials/blob/main/1_Shot_Chart_Tutorial.Rmd)
  ggplot(data=data.frame(x=1,y=1),aes(x,y))+
  ###outside box:
  geom_path(data=data.frame(x=c(-25,-25,25,25,-25),y=c(-47,47,47,-47,-47)))+
  ###halfcourt line:
  geom_path(data=data.frame(x=c(-25,25),y=c(0,0)))+
  ###halfcourt semicircle:
  geom_path(data=data.frame(x=c(-6000:(-1)/1000,1:6000/1000),y=c(sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y))+
  geom_path(data=data.frame(x=c(-6000:(-1)/1000,1:6000/1000),y=-c(sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y))+
  ###solid FT semicircle above FT line:
  geom_path(data=data.frame(x=c(-6000:(-1)/1000,1:6000/1000),y=c(28-sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y))+
  geom_path(data=data.frame(x=c(-6000:(-1)/1000,1:6000/1000),y=-c(28-sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y))+
  ###dashed FT semicircle below FT line:
  geom_path(data=data.frame(x=c(-6000:(-1)/1000,1:6000/1000),y=c(28+sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y),linetype='dashed')+
  geom_path(data=data.frame(x=c(-6000:(-1)/1000,1:6000/1000),y=-c(28+sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y),linetype='dashed')+
  ###key:
  geom_path(data=data.frame(x=c(-8,-8,8,8,-8),y=c(47,28,28,47,47)))+
  geom_path(data=data.frame(x=-c(-8,-8,8,8,-8),y=-c(47,28,28,47,47)))+
  ###box inside the key:
  geom_path(data=data.frame(x=c(-6,-6,6,6,-6),y=c(47,28,28,47,47)))+
  geom_path(data=data.frame(x=c(-6,-6,6,6,-6),y=-c(47,28,28,47,47)))+
  ###restricted area semicircle:
  geom_path(data=data.frame(x=c(-4000:(-1)/1000,1:4000/1000),y=c(41.25-sqrt(4^2-c(-4000:(-1)/1000,1:4000/1000)^2))),aes(x=x,y=y))+
  geom_path(data=data.frame(x=c(-4000:(-1)/1000,1:4000/1000),y=-c(41.25-sqrt(4^2-c(-4000:(-1)/1000,1:4000/1000)^2))),aes(x=x,y=y))+
  ###rim:
  geom_path(data=data.frame(x=c(-750:(-1)/1000,1:750/1000,750:1/1000,-1:-750/1000),y=c(c(41.75+sqrt(0.75^2-c(-750:(-1)/1000,1:750/1000)^2)),c(41.75-sqrt(0.75^2-c(750:1/1000,-1:-750/1000)^2)))),aes(x=x,y=y))+
  geom_path(data=data.frame(x=c(-750:(-1)/1000,1:750/1000,750:1/1000,-1:-750/1000),y=-c(c(41.75+sqrt(0.75^2-c(-750:(-1)/1000,1:750/1000)^2)),c(41.75-sqrt(0.75^2-c(750:1/1000,-1:-750/1000)^2)))),aes(x=x,y=y))+
  ###backboard:
  geom_path(data=data.frame(x=c(-3,3),y=c(43,43)),lineend='butt')+
  geom_path(data=data.frame(x=c(-3,3),y=-c(43,43)),lineend='butt')+
  ###three-point line:
  geom_path(data=data.frame(x=c(-22,-22,-22000:(-1)/1000,1:22000/1000,22,22),y=c(47,47-169/12,41.75-sqrt(23.75^2-c(-22000:(-1)/1000,1:22000/1000)^2),47-169/12,47)),aes(x=x,y=y))+
  geom_path(data=data.frame(x=c(-22,-22,-22000:(-1)/1000,1:22000/1000,22,22),y=-c(47,47-169/12,41.75-sqrt(23.75^2-c(-22000:(-1)/1000,1:22000/1000)^2),47-169/12,47)),aes(x=x,y=y))+
  ###fix aspect ratio to 1:1
  coord_fixed() +
  
# Used geom point function to plot shot chart   
geom_point(aes(locationX/10, locationY/10-47, color = isShotMade), alpha = .01, data = raptors_23yearShots) + 
# Used scale color discrete function for aesthetics for chart 
scale_color_discrete(labels= c("Miss" , "Made"),type= c("red", "black"))+ labs(color="")+ 
# Used facet wrap function to separate shot chart data for each season
facet_wrap(vars(yearSeason), ncol = 4 , nrow = 6)+ylim(-50,0)
# Outputs individual graphic for each year 
raptors_23yearShots_chart

# Code for court from Github (https://github.com/DomSamangy/R_Tutorials/blob/main/1_Shot_Chart_Tutorial.Rmd)
chart23yr <- ggplot(data=data.frame(x=1,y=1),aes(x,y))+
  ###outside box:
  geom_path(data=data.frame(x=c(-25,-25,25,25,-25),y=c(-47,47,47,-47,-47)))+
  ###halfcourt line:
  geom_path(data=data.frame(x=c(-25,25),y=c(0,0)))+
  ###halfcourt semicircle:
  geom_path(data=data.frame(x=c(-6000:(-1)/1000,1:6000/1000),y=c(sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y))+
  geom_path(data=data.frame(x=c(-6000:(-1)/1000,1:6000/1000),y=-c(sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y))+
  ###solid FT semicircle above FT line:
  geom_path(data=data.frame(x=c(-6000:(-1)/1000,1:6000/1000),y=c(28-sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y))+
  geom_path(data=data.frame(x=c(-6000:(-1)/1000,1:6000/1000),y=-c(28-sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y))+
  ###dashed FT semicircle below FT line:
  geom_path(data=data.frame(x=c(-6000:(-1)/1000,1:6000/1000),y=c(28+sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y),linetype='dashed')+
  geom_path(data=data.frame(x=c(-6000:(-1)/1000,1:6000/1000),y=-c(28+sqrt(6^2-c(-6000:(-1)/1000,1:6000/1000)^2))),aes(x=x,y=y),linetype='dashed')+
  ###key:
  geom_path(data=data.frame(x=c(-8,-8,8,8,-8),y=c(47,28,28,47,47)))+
  geom_path(data=data.frame(x=-c(-8,-8,8,8,-8),y=-c(47,28,28,47,47)))+
  ###box inside the key:
  geom_path(data=data.frame(x=c(-6,-6,6,6,-6),y=c(47,28,28,47,47)))+
  geom_path(data=data.frame(x=c(-6,-6,6,6,-6),y=-c(47,28,28,47,47)))+
  ###restricted area semicircle:
  geom_path(data=data.frame(x=c(-4000:(-1)/1000,1:4000/1000),y=c(41.25-sqrt(4^2-c(-4000:(-1)/1000,1:4000/1000)^2))),aes(x=x,y=y))+
  geom_path(data=data.frame(x=c(-4000:(-1)/1000,1:4000/1000),y=-c(41.25-sqrt(4^2-c(-4000:(-1)/1000,1:4000/1000)^2))),aes(x=x,y=y))+
  ###rim:
  geom_path(data=data.frame(x=c(-750:(-1)/1000,1:750/1000,750:1/1000,-1:-750/1000),y=c(c(41.75+sqrt(0.75^2-c(-750:(-1)/1000,1:750/1000)^2)),c(41.75-sqrt(0.75^2-c(750:1/1000,-1:-750/1000)^2)))),aes(x=x,y=y))+
  geom_path(data=data.frame(x=c(-750:(-1)/1000,1:750/1000,750:1/1000,-1:-750/1000),y=-c(c(41.75+sqrt(0.75^2-c(-750:(-1)/1000,1:750/1000)^2)),c(41.75-sqrt(0.75^2-c(750:1/1000,-1:-750/1000)^2)))),aes(x=x,y=y))+
  ###backboard:
  geom_path(data=data.frame(x=c(-3,3),y=c(43,43)),lineend='butt')+
  geom_path(data=data.frame(x=c(-3,3),y=-c(43,43)),lineend='butt')+
  ###three-point line:
  geom_path(data=data.frame(x=c(-22,-22,-22000:(-1)/1000,1:22000/1000,22,22),y=c(47,47-169/12,41.75-sqrt(23.75^2-c(-22000:(-1)/1000,1:22000/1000)^2),47-169/12,47)),aes(x=x,y=y))+
  geom_path(data=data.frame(x=c(-22,-22,-22000:(-1)/1000,1:22000/1000,22,22),y=-c(47,47-169/12,41.75-sqrt(23.75^2-c(-22000:(-1)/1000,1:22000/1000)^2),47-169/12,47)),aes(x=x,y=y))+
  ###fix aspect ratio to 1:1
  coord_fixed() +
  
# Used geom point function to plot shot chart    
geom_point(aes(locationX/10, locationY/10-43, color = isShotMade), alpha = .15, data = raptors_23yearShots) + 
# Used scale color discrete function for aesthetics for chart
scale_color_discrete(labels= c("Miss" , "Made"),type= c("red", "black"))+ labs(color="")+
# Displays only half the court 
ylim(-50,0) + 
# Allows animation to change every year 
transition_time(yearSeason) + 
# Title on animation 
ggtitle("Raptors Shots By Year: {frame_time}")+
# Removes color and lines from court 
theme_clean()+
# Centers my title 
theme(plot.title= element_text(hjust=0.5))
# Creates animation of Toronto Raptors shot chart data from 1997-2020
chart23yr %>% animate(nframes = 24, fps= .5)

# Creating object to filter Toronto Raptors Field Goal Percentage by year 
field_goal_percentage = raptors_23yearShots %>% group_by(yearSeason) %>% mutate(isShotMade = 100*isShotMade) %>% summarize(mean(isShotMade))

# Creates graph for Toronto Raptors Field Goal percentage by each season
fieldGoalPercentage_graph = 
  ggplot(aes(x=yearSeason, y=`mean(isShotMade)`), data= field_goal_percentage)+
  # Changes color to red 
  geom_col(fill = "red")+
  # Changes title of graph 
  labs(title = "Raptors Field Goal Percentage By Season", x = "Year", y= "Percentage (%)")

# Creates object for total number of shots taken each season 
total_shots = raptors_23yearShots %>% group_by(yearSeason) %>% summarize(sum(isShotAttempted))

# Creates graph for Toronto Raptors total shot attempts by season 
shotAttempts_graph = 
  ggplot(aes(x=yearSeason, y=`sum(isShotAttempted)`), data= total_shots)+
# Changes color to red 
  geom_col(fill = "red")+
# Changes title of graph 
  labs(title = "Total Shots Per Year", x = "Year", y= "Number of Shots Taken")

# Creates object for total number of threes shot per year 
raptors_3_taken = raptors_23yearShots %>% 
  mutate(threePoint = ifelse(raptors_23yearShots$typeShot == "3PT Field Goal", 1, 0))
# Summing number of threes taken each season 
raptors_3_taken = raptors_3_taken %>%   
  group_by(yearSeason) %>%   
  summarize(sum(threePoint))

# Creates graph for total number of threes taken each season
threes_graph = 
  ggplot(aes(x=yearSeason, y=`sum(threePoint)`), data= raptors_3_taken)+
# Changes color to red 
  geom_col(fill = "red")+
# Changes title of graph 
  labs(title = "Total Threes Per Year", x = "Year", y= "Number of Threes Taken")

# Creates object for total number of mid-range shots taken per year 
raptors_midrange_taken = raptors_23yearShots %>%
  mutate(midRange = ifelse(raptors_23yearShots$zoneBasic == "Mid-Range", 1,0))
# Summing number of mid-range shots per season 
raptors_midrange_taken = raptors_midrange_taken %>%
  group_by(yearSeason) %>%
  summarize(sum(midRange))

# Creates graph for total number of mid-range shots per season
midRange_graph = 
  ggplot(aes(x=yearSeason, y=`sum(midRange)`), data= raptors_midrange_taken)+
# Changes color to red 
  geom_col(fill = "red")+
# Changes title of graph 
  labs(title = "Total Mid-Range Shots Per Year", x = "Year", y= "Number of Mid-Range Taken")

