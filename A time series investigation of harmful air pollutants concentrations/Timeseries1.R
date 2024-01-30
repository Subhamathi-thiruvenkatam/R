#A Time Series Investigation of Harmful Air Pollutants Concentrations in Urban Areas"


#Load the libraries
library(tidyverse)
library(lubridate)
library(readr)
library(ggplot2)
library(dplyr)
library(corrplot)
library(ggplot2)
library(reshape2)

#Read the data from the CSV file

#Data for NO2
data2010 <- read.csv("C:/Users/subhu/Desktop/AOSD/New folder/berlin no2/2010/b2010.csv")
d10 <- data2010[data2010$Air.Quality.Station.Name == "Berlin Neukolln",]
data2011 <- read.csv("C:/Users/subhu/Desktop/AOSD/New folder/berlin no2/2011/b2011.csv")
d11 <- data2011[data2011$Air.Quality.Station.Name == "Berlin Neukolln",]
data2012 <- read.csv("C:/Users/subhu/Desktop/AOSD/New folder/berlin no2/2012/b2012.csv")
d12 <- data2012[data2012$Air.Quality.Station.Name == "Berlin Neukolln",]
data2013 <- read.csv("C:/Users/subhu/Desktop/AOSD/New folder/berlin no2/2013/b2013.csv")
d13 <- data2013[data2013$Air.Quality.Station.Name == "Berlin Neukolln",]
data2014 <- read.csv("C:/Users/subhu/Desktop/AOSD/New folder/berlin no2/2014/b2014.csv")
d14 <- data2014[data2014$Air.Quality.Station.Name == "Berlin Neukolln",]
data2015 <- read.csv("C:/Users/subhu/Desktop/AOSD/New folder/berlin no2/2015/b2015.csv")
d15 <- data2015[data2015$Air.Quality.Station.Name == "Berlin Neukolln",]
data2016 <- read.csv("C:/Users/subhu/Desktop/AOSD/New folder/berlin no2/2016/b2016.csv")
d16 <- data2016[data2016$Air.Quality.Station.Name == "Berlin Neukolln",]
data2017 <- read.csv("C:/Users/subhu/Desktop/AOSD/New folder/berlin no2/2017/b2017.csv")
d17 <- data2017[data2017$Air.Quality.Station.Name == "Berlin Neukolln",]
data2018 <- read.csv("C:/Users/subhu/Desktop/AOSD/New folder/berlin no2/2018/b2018.csv")
d18 <- data2018[data2018$Air.Quality.Station.Name == "Berlin Neukolln",]
data2019 <- read.csv("C:/Users/subhu/Desktop/AOSD/New folder/berlin no2/2019/b2019.csv")
d19 <- data2019[data2019$Air.Quality.Station.Name == "Berlin Neukolln",]
data2020 <- read.csv("C:/Users/subhu/Desktop/AOSD/New folder/berlin no2/2020/b2020.csv")
d20 <- data2020[data2020$Air.Quality.Station.Name == "Berlin Neukolln",]



#Data for O3
O3_2010 <- read.csv("C:/Users/subhu/Desktop/AOSD/New folder/berlin no2/2010/O3_10.csv")
o10 <- O3_2010[O3_2010$Air.Quality.Station.Name == "Berlin Neukolln",]
O3_2011 <- read.csv("C:/Users/subhu/Desktop/AOSD/New folder/berlin no2/2011/O3_11.csv")
o11 <- O3_2011[O3_2011$Air.Quality.Station.Name == "Berlin Neukolln",]
O3_2012 <- read.csv("C:/Users/subhu/Desktop/AOSD/New folder/berlin no2/2012/O3_12.csv")
o12 <- O3_2012[O3_2012$Air.Quality.Station.Name == "Berlin Neukolln",]
O3_2013 <- read.csv("C:/Users/subhu/Desktop/AOSD/New folder/berlin no2/2013/O3_13.csv")
o13 <- O3_2013[O3_2012$Air.Quality.Station.Name == "Berlin Neukolln",]
O3_2014 <- read.csv("C:/Users/subhu/Desktop/AOSD/New folder/berlin no2/2014/O3_14.csv")
o14 <- O3_2014[O3_2014$Air.Quality.Station.Name == "Berlin Neukolln",]
O3_2015 <- read.csv("C:/Users/subhu/Desktop/AOSD/New folder/berlin no2/2015/O3_15.csv")
o15 <- O3_2015[O3_2015$Air.Quality.Station.Name == "Berlin Neukolln",]
O3_2016 <- read.csv("C:/Users/subhu/Desktop/AOSD/New folder/berlin no2/2016/O3_16.csv")
o16 <- O3_2016[O3_2016$Air.Quality.Station.Name == "Berlin Neukolln",]
O3_2017 <- read.csv("C:/Users/subhu/Desktop/AOSD/New folder/berlin no2/2017/O3_17.csv")
o17 <- O3_2017[O3_2017$Air.Quality.Station.Name == "Berlin Neukolln",]
O3_2018 <- read.csv("C:/Users/subhu/Desktop/AOSD/New folder/berlin no2/2018/O3_18.csv")
o18 <- O3_2018[O3_2018$Air.Quality.Station.Name == "Berlin Neukolln",]
O3_2019 <- read.csv("C:/Users/subhu/Desktop/AOSD/New folder/berlin no2/2019/O3_19.csv")
o19 <- O3_2019[O3_2019$Air.Quality.Station.Name == "Berlin Neukolln",]
O3_2020 <- read.csv("C:/Users/subhu/Desktop/AOSD/New folder/berlin no2/2020/O3_20.csv")
o20 <- O3_2020[O3_2020$Air.Quality.Station.Name == "Berlin Neukolln",]




#Data for NO
NO_2010 <- read.csv("C:/Users/subhu/Desktop/AOSD/New folder/berlin no2/2010/NO_10.csv")
NO10 <- NO_2010[NO_2010$Air.Quality.Station.Name == "Berlin Neukolln",]
NO_2011 <- read.csv("C:/Users/subhu/Desktop/AOSD/New folder/berlin no2/2011/NO_11.csv")
NO11 <- NO_2011[NO_2011$Air.Quality.Station.Name == "Berlin Neukolln",]
NO_2012 <- read.csv("C:/Users/subhu/Desktop/AOSD/New folder/berlin no2/2012/NO_12.csv")
NO12 <- NO_2012[NO_2012$Air.Quality.Station.Name == "Berlin Neukolln",]
NO_2013 <- read.csv("C:/Users/subhu/Desktop/AOSD/New folder/berlin no2/2013/NO_13.csv")
NO13 <- NO_2013[NO_2012$Air.Quality.Station.Name == "Berlin Neukolln",]
NO_2014 <- read.csv("C:/Users/subhu/Desktop/AOSD/New folder/berlin no2/2014/NO_14.csv")
NO14 <- NO_2014[NO_2014$Air.Quality.Station.Name == "Berlin Neukolln",]
NO_2015 <- read.csv("C:/Users/subhu/Desktop/AOSD/New folder/berlin no2/2015/NO_15.csv")
NO15 <- NO_2015[NO_2015$Air.Quality.Station.Name == "Berlin Neukolln",]
NO_2016 <- read.csv("C:/Users/subhu/Desktop/AOSD/New folder/berlin no2/2016/NO_16.csv")
NO16 <- NO_2016[NO_2016$Air.Quality.Station.Name == "Berlin Neukolln",]
NO_2017 <- read.csv("C:/Users/subhu/Desktop/AOSD/New folder/berlin no2/2017/NO_17.csv")
NO17 <- NO_2017[NO_2017$Air.Quality.Station.Name == "Berlin Neukolln",]
NO_2018 <- read.csv("C:/Users/subhu/Desktop/AOSD/New folder/berlin no2/2018/NO_18.csv")
NO18 <- NO_2018[NO_2018$Air.Quality.Station.Name == "Berlin Neukolln",]
NO_2019 <- read.csv("C:/Users/subhu/Desktop/AOSD/New folder/berlin no2/2019/NO_19.csv")
NO19 <- NO_2019[NO_2019$Air.Quality.Station.Name == "Berlin Neukolln",]
NO_2020 <- read.csv("C:/Users/subhu/Desktop/AOSD/New folder/berlin no2/2020/NO_20.csv")
NO20 <- NO_2020[NO_2020$Air.Quality.Station.Name == "Berlin Neukolln",]

#Plotting the station chosen 
library(tidyverse) |> suppressPackageStartupMessages()
library(sf)
# Linking to GEOS 3.10.2, GDAL 3.4.3, PROJ 8.2.1; sf_use_s2() is TRUE
crs <- st_crs("EPSG:32632")
st_as_sf(d12, crs = "OGC:CRS84", coords = 
           c("Longitude", "Latitude")) |>
  st_transform(crs) -> no2.sf

read_sf("C:/Users/subhu/Desktop/AOSD/New folder/Berlin/Berlin_city.shp") |> st_transform(crs) -> de


ggplot() + geom_sf(data = de) + 
  geom_sf(data = no2.sf)

#Binding all the NO2 data
df1 <- bind_rows(d10, d11, d12, d13, d14, d15, d16, d17, d18, d19, d20)
Nitrogen_dioxide <- df1$Air.Pollution.Level
df1

#Binding all the O3 data
O3 <- bind_rows(o10, o11, o12, o13, o14, o15, o16, o17, o18, o19, o20)
Ozone <- O3$Air.Pollution.Level

#Binding all the NO2 data
NO <- bind_rows(NO10, NO11, NO12, NO13, NO14, NO15, NO16, NO17, NO18, NO19, NO20)
Nitrogen_Monoxide <- NO$Air.Pollution.Level

#Combining all the Air pollutants data
All <- bind_rows(df1,O3,NO)


# Visualizing the plot for NO2
options(repr.plot.width = 16, repr.plot.height = 6)

ggplot(data = df1, aes(x = Year, y = Air.Pollution.Level, color = Air.Pollution.Level)) +
  geom_line() +
  scale_color_gradient(low = "green", high = "red") +
  labs(x = "Year", y = "Concentration (ug/m3)", title="Trends in NO2 Concentrations Over Time in Berlin ")



# Visualizing the plot for O3
ggplot(data = O3, aes(x = Year, y = Air.Pollution.Level)) +
  geom_line(color="Blue") +
  labs(x = "Year", y = "Concentration (ug/m3)",title="Trends in Ozone Concentrations Over Time in Berlin ")




# Visualizing the plot for NO
ggplot(data = NO, aes(x = Year, y = Air.Pollution.Level, color = Air.Pollution.Level)) +
  geom_line() +
  scale_color_gradient(low = "PINK", high = "VIOLET") +
  labs(x = "Year", y = "Concentration (ug/m3)", title="Trends in NO Concentrations Over Time in Berlin ")




# Combine the plots into one grid

df <- data.frame(x = All$Year,
                 y = All$Air.Pollution.Level,
                 Air.Pollutants = factor(All$Air.Pollutant.Description))


ggplot(df, aes(x = x, y = y, color = Air.Pollutants)) + 
  geom_line() + 
  labs(x = "Time (years) ", y = "Concentration (ug/m3)", title = "Decadal Changes in Harmful Air Pollutants Concentrations" )+
  scale_color_manual(values = c("red", "green", "blue"))



# Creating a data frame with the three pollutants 
pollutants_df <- data.frame(Nitrogen_dioxide, Ozone, Nitrogen_Monoxide)

# calculation of the correlation matrix
cor_matrix <- cor(pollutants_df)
cor_matrix



#Visualizing the correlation matrix
cor_df <- melt(cor_matrix)


# creation of a heatmap
ggplot(cor_df, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "white", mid = "grey", high = "black", 
                       midpoint = 0, limit = c(-3,3)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0))
