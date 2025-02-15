# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)

# Load the data
setwd("~/Desktop/759")
generating_hypothesis <- read_excel("generatinghypotheses.xls")
View(generating_hypothesis)

# Convert the 'Onset' column to datetime
generating_hypothesis$Onset <- as.POSIXct(generating_hypothesis$Onset, format = "%Y-%m-%d %H:%M:%S")

# Create epidemic curve with 1-day interval
epicurve_1day <- generating_hypothesis %>%
  mutate(Onset = as.Date(Onset)) %>%
  count(Onset)

# Create epidemic curve with 12-hour interval
epicurve_12hour <- generating_hypothesis %>%
  mutate(Onset = cut(Onset, breaks = "12 hours")) %>%
  count(Onset)

# Plotting the epidemic curves
# 1-day interval
ggplot(epicurve_1day, aes(x = Onset, y = n)) +
  geom_bar(stat = "identity") +
  labs(title = "Epidemic Curve (1-day interval)", x = "Date", y = "Number of Ill Cases") 

# 12-hour interval
ggplot(epicurve_12hour, aes(x = Onset, y = n)) +
  geom_bar(stat = "identity") +
  labs(title = "Epidemic Curve (12-hour interval)", x = "Date and Time", y = "Number of Ill Cases") 

print(epicurve_12hour)
print(epicurve_1day)

library(dplyr)
library(ggplot2)

# Create the data frame from the Idaho data
idaho_data <- data.frame(
  Date = as.Date(c(rep("2025-11-01", 3), rep("2025-11-02", 7), rep("2025-11-03", 8), rep("2025-11-04", 20), rep("2025-11-05", 43))),
  Sex = c(rep("Male", 3), rep("Female", 0), rep("Male", 5), rep("Female", 2), rep("Male", 3), rep("Female", 5), rep("Male", 9), rep("Female", 11), rep("Male", 21), rep("Female", 22)),
  AttendedUniversity = c(rep("Yes", 2), rep("No", 1), rep("Yes", 3), rep("No", 4), rep("Yes", 4), rep("No", 4), rep("Yes", 11), rep("No", 9), rep("Yes", 19), rep("No", 24))
)

# Create epidemic curve
epicurve_idaho <- idaho_data %>%
  count(Date)

# Plotting the epidemic curve
plot_idaho <- ggplot(epicurve_idaho, aes(x = Date, y = n)) +
  geom_bar(stat = "identity") +
  labs(title = "Epidemic Curve (Idaho Data)", x = "Date", y = "Number of Cases")
print(plot_idaho)

# Print the dataset as a CSV
write.csv(idaho_data, "idaho_data.csv", row.names = FALSE)

# To view the CSV content in the console
print(read.csv("idaho_data.csv"))