library(dplyr)
library(scales)
library(ggplot2)
library(ggthemes)
library(lazyeval)


#Created by Greg Gunn 3-16-2015 for the Charlotte R User's Group Meetup
#This file demonstrates how to do basic data manipulation with dplyr

#We will be use the baseball data set created by Sean Lehman
#We can download it from his website directly

#Create a temporary file placeholder, and download the csv directly
temp = tempfile()
download.file("http://seanlahman.com/files/database/lahman-csv_2015-01-24.zip", destfile = temp)

#Let's take a look at the pipe operator "%>%".  You can use the pipe operator to simplify your code.
# F(g(x)) is equivalent to g(x) %>% F  Using this you can make your code read top to bottom instead instead of inside out

Player_data = read.csv((unz(temp, "Master.csv")))

#Is equivalent to:

Player_data = unz(temp, "Master.csv")%>%
  read.csv()

Batting_data = unz(temp, "Batting.csv")%>%
  read.csv()

Salary_data = unz(temp, "Salaries.csv")%>%
  read.csv()

unlink(temp)

#Player data contains biographical information such as height, weight, and birthday
glimpse(Player_data)

#Batting data contains offensive statistics such as home runs, rbis, etc
glimpse(Batting_data)

#Salary Data tells you how much each player made
glimpse(Salary_data)


#Before I jump into the tutorial, there are a couple links which I found very helpful:
#1) An overview of packages in the hadleyverse, including dplyr
#     http://barryrowlingson.github.io/hadleyverse/#1
#  2) A heplful cheatsheet on data wrangling from RRStuio
#     http://www.rstudio.com/resources/cheatsheets


#I won't spend much time going over syntax in detail.  There are many videos for that.  I am going to ask
# A series of data wrangling quesitons, and show you how to use dplyr to answer them.  That should help show
#how to string dplyr commands together into a sensible flow


#Question 1: What are the top 10 base stealing season since 1950?

Stolen_Base_Seasons = Batting_data%>%
  filter(yearID>=1950)%>%
  select(yearID, playerID, SB, CS)%>%
  arrange(desc(SB))%>%
  top_n(10, SB)


#Question 2: Which player has the most stolen bases of All time?

Career_Stolen_Bases = Batting_data%>%
  group_by(playerID)%>%
  summarise(Career_Stolen_Bases = sum(SB, na.rm = TRUE), 
            Career_Caught_Stealing = sum(CS, na.rm=TRUE),
            Seasons = n_distinct(yearID))%>%
  arrange(desc(Career_Stolen_Bases))


#Question 3: What are the top 10 Stolen Base leader's names and how tall are they?

Enhanced_Career_SB = Career_Stolen_Bases%>%
  top_n(10, Career_Stolen_Bases)%>%
  left_join(y = Player_data, by = c("playerID" = "playerID"))%>%
  mutate(Name = paste(nameLast, nameFirst, sep = ","), 
         Height = paste(height%/%12, "'", height%%12, "\"", sep = ""))%>%
  select(Name, Height, Seasons,
         `Stolen Bases` = Career_Stolen_Bases, `Caught Stealing`=Career_Caught_Stealing)


#Question 4: What percentage of their team's runs did each player account for since 2008? RBi's


Team_Contribution = Batting_data%>%
  filter(yearID == 2008)%>%
  group_by(yearID, teamID, playerID)%>%
  summarise(Runs = sum(R), 
            RBI = sum(RBI))%>%
  mutate(Team_Runs = sum(Runs), 
         Team_RBIS = sum(RBI), 
         Pct_of_Runs = Runs/Team_Runs, 
         Pct_of_RBI = RBI/Team_RBIS)%>%
  left_join(y = Player_data, by = c("playerID" = "playerID"))%>%
  mutate(Name = paste(nameLast, nameFirst, sep = ","), 
         Height = paste(height%/%12, "'", height%%12, "\"", sep = ""))%>%
  select(Team = teamID, Name, `Percent of Team's Runs` = Pct_of_Runs, `Percent of Team's RBIs` = Pct_of_RBI)%>%
  ungroup()%>%
  arrange(Team, desc(`Percent of Team's Runs`))%>%
  mutate(`Percent of Team's Runs` = percent(`Percent of Team's Runs`), 
         `Percent of Team's RBIs` = percent(`Percent of Team's RBIs`))

#Question 5: Can you make me a graph of HR by team since 1975?

HR_Time_Series = Batting_data%>%
  filter(yearID>=1975)%>%
  group_by(teamID, yearID)%>%
  summarise(HR = sum(HR, na.rm = TRUE))%>%
  ggplot()+
  geom_line(aes(x = yearID, y = HR, color = teamID))+
  theme_tufte()+
  xlab("Year")
HR_Time_Series


#Question 6:What if I want to see only some teams and choose my statistic?

#Ok so we'll need to creat a function.  This is where it gets a bit tricky..
#Check out this site for more information:
#http://blog.rstudio.org/2014/10/13/dplyr-0-3-2/

#Also make sure you read the vignette on non-standard evaluation linked in there
#You'll notice that


StatPlotter = function(teamlist, stat, Min_Year = 1950, Max_Year = 2014){
  summary_stat = setNames(list(interp(~sum(var1, na.rm = TRUE), var1 = as.name(stat))), c(stat))
  
  filters = list(~teamID %in% teamlist,
                 ~yearID >= Min_Year, 
                 ~yearID <= Max_Year)
  
  Batting_data%>%
    filter_(.dots = filters)%>%
    group_by_(~teamID, ~yearID)%>%
    summarise_(.dots = summary_stat)%>%
    ggplot()+
    geom_line(aes_string(x = "yearID", y = stat, color = "teamID"), stat = "identity")
  
}


StatPlotter(teamlist = c("PHI", "HOU"), stat = "SB")+
  expand_limits(y = 0)+
  theme_tufte()







