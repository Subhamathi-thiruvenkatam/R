## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(Accidentanalysis)

## -----------------------------------------------------------------------------
#Load the dataset
 accidents <- load_accident_dataset()

## -----------------------------------------------------------------------------
# Display the first few rows of the dataset 
 head(accidents) 
 # Summary statistics of the dataset 
 summary(accidents)

## -----------------------------------------------------------------------------
severity_analysis_plot <- function(data) {
  library(ggplot2)

  # Perform severity analysis (example calculation)
  severity_summary <- aggregate(
    cbind(Mild = `Mild injuries`, Serious = `Serious injuries`) ~ `District Name`,
    data = data,
    FUN = sum
  )

  # Plot severity analysis
  ggplot(severity_summary, aes(x = `District Name`, y = Mild + Serious, fill = `District Name`)) +
    geom_bar(stat = "identity") +
    labs(title = "Severity Analysis by District", x = "District Name", y = "Total Injuries") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.margin = margin(10, 10, 20, 10, "pt")) +
    theme(legend.position = "none") +
    theme(panel.grid.major.x = element_blank()) +
    theme(panel.grid.minor.x = element_blank())
}


## ----severity-analysis,fig.width=8, fig.height=5------------------------------
library(dplyr)
severity_analysis_plot(accidents)


## -----------------------------------------------------------------------------
plot_hourly_distribution <- function(data) {
  library(ggplot2)

  ggplot(data, aes(x = Hour)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
    labs(title = "Hourly Accident Distribution", x = "Hour of the Day", y = "Frequency") +
    theme_minimal()
}


## ----hourly-distribution, fig.width=8, fig.height=5---------------------------
accidents <- load_accident_dataset()
plot_hourly_distribution(accidents)

## -----------------------------------------------------------------------------
vehicle_involvement_analysis <- function(data) {
  result <- data %>%
    group_by(`Vehicles involved`) %>%
    summarise(
      Avg_Mild = mean(`Mild injuries`),
      Avg_Serious = mean(`Serious injuries`),
      Total_Injuries = sum(`Mild injuries` + `Serious injuries`)
    )

  # Plot the results
  plot <- ggplot(result, aes(x = `Vehicles involved`, y = Total_Injuries, fill = `Vehicles involved`)) +
    geom_bar(stat = "identity") +
    labs(title = "Vehicle Involvement Analysis", x = "Number of Vehicles Involved", y = "Total Injuries") +
    theme_minimal()

  return(list(summary = result, plot = plot))
}

## -----------------------------------------------------------------------------
library(magrittr)
library(dplyr)
vehicle_analysis_result <- vehicle_involvement_analysis(accidents)
vehicle_analysis_result$summary


## ----vehicle-involvement, fig.width=8, fig.height=5---------------------------
print(vehicle_analysis_result$plot)


## -----------------------------------------------------------------------------
day_of_week_analysis <- function(data) {
  result <- data %>%
    group_by(Weekday) %>%
    summarise(AccidentCount = n())

  # Plot the results using a scatter plot
  plot <- ggplot(result, aes(x = reorder(Weekday, -AccidentCount), y = AccidentCount, color = Weekday)) +
    geom_point(size = 4) +
    labs(title = "Day of the Week Analysis", x = "Day of the Week", y = "Accident Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  return(list(summary = result, plot = plot))
}


## -----------------------------------------------------------------------------
#Statistical Analysis of the result
day_of_week_result <- day_of_week_analysis(accidents)
day_of_week_result$summary


## ----day-of-week, fig.width=8, fig.height=5-----------------------------------
#The plot that shows the accidents trend over the days of a week
print(day_of_week_result$plot)


