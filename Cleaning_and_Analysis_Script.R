#Import the dataset (Match.csv)
library(readr)
Match <- read_csv("Match.csv") #This will observe the dataset i.e columns, rows and column names.
View(Match)
Match <- na.omit(Match) #This command handles missing match records and removes the incomplete outcomes.
#Creating a seperate column in the dataset called as "Toss_Won_Match"(i.e Binary Outcome Variable). This operation produces output 1 if Toss_Winner ID equals to Match_Winner ID otherwise 0.This column gives us the clear information about Winner vs Lost
Match$Toss_Won_Match <- ifelse(Match$Toss_Winner == Match$Match_Winner, 1, 0)
#contigency table is created to get the draw counts of Toss Winner Wins i.e 1 or Losses i.e 0
contigency_table <- table(Match$Toss_Won_Match)
print(contigency_table)
install.packages("ggplot2")
#installing package called ggplot2 to feature R graph
library(ggplot2)
#Creating data frame for plotting from contigency table
plot_data <- as.data.frame(contigency_table)
#Creating contigency table data frame to view as viewable file
contigency_DF <- as.data.frame(contigency_table)
<<<<<<< HEAD
#Creating a seperate column in the datset called as "Toss_Won_Match"(i.e Binary Outcome Variable). This operation produces output 1 if Toss_Winner ID equals to Match_Winner ID otherwise 0.This column gives us the clear information about Winner vs Lost
Match$Toss_Won_Match <- ifelse(Match$Toss_Winner == Match$Match_Winner, 1, 0)
#contigency table is created to get the draw counts of Toss Winner Wins i.e 1 or Losses i.e 0
contigency_table <- table(Match$Toss_Won_Match)
print(contigency_table)
#Creating Main Barplot 
names(plot_data) <- c("Outcome_ID", "Count")
plot_data$Outcome <- factor(plot_data$Outcome_ID, 
                            levels = c(0, 1), 
                            labels = c("Toss Winner Lost Match", "Toss Winner Won Match"))

#Calculating percentages and creating the final label for the plot (Required for geom_text)
total_matches <- sum(plot_data$Count) 
plot_data$Percentage <- (plot_data$Count / total_matches) * 100
# Formating the labels to show both count and percentage on the bar
plot_data$Label <- paste0(plot_data$Count, " (", round(plot_data$Percentage, 1), "%)")
toss_won_plot <- ggplot(plot_data, aes(x = Outcome, y = Count, fill = Outcome)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = Label), vjust = -0.5, size = 4) +
  scale_y_continuous(
    breaks = seq(0, 300, 15),
    limits = c(0, max(plot_data$Count) + 30)
  ) +
  scale_x_discrete(labels = c(
    "Toss Winner Lost\n(280 Matches)",
    "Toss Winner Won\n(288 Matches)"
  )) +
  labs(
    title = "Match Results Based on Winning the Toss (Total Matches = 568)",
    x = "Match Results Relative to Toss Winner",
    y = "Number of Matches (Frequency)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 12, face = "bold")
  )

print(toss_won_plot)
=======
names(plot_data) <- c("Outcome_ID", "Count")
plot_data$Outcome <- factor(plot_data$Outcome_ID, 
                              levels = c(0, 1), 
                              labels = c("Toss Winner Lost Match", "Toss Winner Won Match"))
#generating the Barplot with custom x and y labels.
 toss_won_plot <- ggplot(plot_data, aes(x = Outcome, y = Count, fill = Outcome)) +
       geom_bar(stat = "identity")+
  scale_y_continuous(breaks = seq(0, 300, 15)) +
  labs(
         title = "Match Results Based on Winning the Toss",
         x = "Do Toss Winner Also Won the Match?",
         y = "Number of Matches"
      ) +
        theme_minimal() + 
        theme(legend.position="none")
print(toss_won_plot)
#Input the raw counts from the contigency table
match_results <- c(280, 288)
#Run the Chi-Square Test (Tests against the 50/50 null hypothesis)
chi_sq_result <- chisq.test(match_results)
print(chi_sq_result)
