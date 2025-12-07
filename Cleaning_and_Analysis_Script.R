#Import the dataset (Match.csv)
library(readr)
Match <- read_csv("Match.csv") #This will observe the dataset i.e columns, rows and column names.
View(Match)
Match <- na.omit(Match) #This command handles missing match records and removes the incomplete outcomes.
#Creating a seperate column in the datset called as "Toss_Won_Match"(i.e Binary Outcome Variable). This operation produces output 1 if Toss_Winner ID equals to Match_Winner ID otherwise 0.This column gives us the clear information about Winner vs Lost
Match$Toss_Won_Match <- ifelse(Match$Toss_Winner == Match$Match_Winner, 1, 0)
#contigency table is created to get the draw counts of Toss Winner Wins i.e 1 or Losses i.e 0
contigency_table <- table(Match$Toss_Won_Match)
print(contigency_table)