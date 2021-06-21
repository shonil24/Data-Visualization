defaultWarn <- getOption("warn")
options(warn = -1)

#----------------------Packages---------------------------------

library(readr) # to read data
library(dplyr) # to prepare the data
library(ggplot2) # to plot static graphs
library(tidyr) # to manipulate and shape data
library(plotly) # to make graphs interactive
library(leaflet) # to plot dynamic map
library(flexdashboard) # to display dashboard

#----------------------Data-------------------------------------

# Reading the data
covid <- read.csv("https://raw.githubusercontent.com/RamiKrispin/coronavirus/master/csv/coronavirus.csv", stringsAsFactors = FALSE) %>% 
  mutate(date = as.Date(date, "%Y-%m-%d"),
         country = factor(country, levels = unique(country)),
         lat = as.numeric(lat),
         long = as.numeric(long),
         type = factor(type, levels = unique(type)))

# dropping province column and setting data types
covid <- select(covid, -matches("province"))

# Checking the output
head(covid)

# New variable as month and year
covid$month <- format(covid$date, "%m")
covid$year <- format(covid$date, "%y")

#---- Wider format
covidW <- covid %>% 
  select(country, lat, long, type, cases) %>% 
  group_by(country, lat, long, type) %>% 
  summarise(cases = sum(cases, na.rm = TRUE)) %>% 
  pivot_wider(names_from = type,
              values_from = cases) %>% 
  mutate(active = confirmed - death - recovered) %>% 
  arrange(-active) 

#----Longer format
covidL <- 
  covidW %>% 
  pivot_longer(cols = confirmed:active, 
               names_to = "type", 
               values_to = "cases")

# Global type cases
coW <- sum(as.numeric(covidW$confirmed), na.rm = TRUE)
reW <- sum(as.numeric(covidW$death), na.rm = TRUE)
deW <- sum(as.numeric(covidW$recovered), na.rm = TRUE)
acW <- sum(as.numeric(covidW$active), na.rm = TRUE)

# France cases. 
covidFr <- covid %>% filter(country == "France")

# Daily new cases type for last 14 days in France. Also cumulative cases (sum of cases for each day)
frVis <- covidFr %>% 
  filter(date %in% tail(unique(as.Date(covidFr$date)), 14) ) %>% 
  select(date, type, cases) %>% 
  group_by(date, type) %>% 
  summarise(cases = sum(cases, na.rm = TRUE)) %>% 
  group_by(type) %>% 
  mutate(cumulative_cases = cumsum(cases))

# Daily new cases for Visualization
frDaily <- frVis %>% 
  select(date, type, cases) %>% 
  pivot_wider(names_from = type,
              values_from = cases)

# Cumulative cases for Visualization
frCum <- frVis %>% 
  select(date, type, cumulative_cases) %>% 
  pivot_wider(names_from = type,
              values_from = cumulative_cases) %>% 
  mutate(active = confirmed - death - recovered) %>% 
  mutate(active = cumsum(active))

# Growth rate for past 6 months
frGrowth <- covidFr %>% filter(year == "21") %>% 
  select(month, type, cases) %>% 
  group_by(month, type) %>% summarise(cases = sum(cases)) %>% 
  ungroup() %>% 
  group_by(type) %>% mutate(growth = ((cases - lag(cases))/lag(cases)) * 100) %>% 
  select(month, type, growth) %>% 
  pivot_wider(names_from = type,
              values_from = growth)

# Recovery rate for past 6 months. 
frRecovery <- covidFr %>% 
  filter(year == "21") %>% 
  select(month, type, cases) %>% 
  group_by(month, type) %>% summarise(cases = sum(cases)) %>% 
  ungroup() %>% 
  group_by(type) %>% 
  pivot_wider(names_from = type,
              values_from = cases) %>% 
  mutate(recovery_rate = (recovered / confirmed) * 100) %>% 
  mutate(recovery_rate = if_else(is.na(recovery_rate), 0, recovery_rate))


##-----------------------------Daily New Cases------------------

ggplot(frVis, aes(fill = type, y = cases, x = date)) + 
  geom_bar(width = 0.8, position = "dodge", 
           stat = "identity") +
  scale_color_manual(values = c("orange", "red", "green")) + 
  scale_fill_manual(values = c("orange", "red", "green")) + 
  scale_x_date(date_labels = "%b %d",
               breaks = frVis$date) +
  theme(axis.text.x = element_text(angle = 30)) +
  geom_text(
    aes(label = cases, group = type), 
    position = position_dodge(0.8),
    vjust = -0.3, size = 3.5
  ) + 
  ggtitle("Daily new cases in France for past 14 days")

plot_ly(frDaily, x = ~ date, y = ~ confirmed, name = 'Confirmed', color = 'rgba(243, 156, 18, 1)', 
        type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~ recovered, name = "Recovered", color = 'rgba(46, 204, 113, 1)') %>%
  add_trace(y = ~ death, name = "Death", color = 'rgba(240, 52, 52, 1)') %>% 
  layout(title = "Daily New Cases in France for past 14 days", yaxis = list(title = "Daily New Cases"), 
         xaxis = list(title = "Date", type = "date"), autosize = FALSE, legend = list(x = 0.1, y = 0.9), hovermode = "compare")

##-----------------------------Cumulative Cases------------------

plot_ly(frCum, x = ~ date, y = ~ confirmed, name = 'Confirmed', color = 'rgba(243, 156, 18, 1)', 
        type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~ recovered, name = "Recovered", color = 'rgba(46, 204, 113, 1)') %>%
  add_trace(y = ~ death, name = "Death", color = 'rgba(240, 52, 52, 1)') %>% 
  layout(title = "Cumulative Cases in France for past 14 days", yaxis = list(title = "Cumulative Cases"), 
         xaxis = list(title = "Date", type = "date"), autosize = FALSE, legend = list(x = 0.1, y = 0.9), hovermode = "compare")

##-----------------------------Growth rate------------------

plot_ly(frGrowth, x = ~month, y = ~ confirmed, name = 'Confirmed', color = 'rgba(46, 204, 113, 1)', 
        type = 'scatter', mode = 'lines+markers') %>% 
  add_trace(y = ~ recovered, name = "Recovered", color = 'rgba(240, 52, 52, 1)', type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~ death, name = "Death", color = 'rgba(243, 156, 18, 1)', type = 'scatter', mode = 'lines+markers') %>% 
  layout(title = "Growth rate in France for past 5 months", yaxis = list(title = "Growth rate"), 
         xaxis = list(
           ticktext = list("Feb", "March", "April", "May", "June"), 
           tickvals = list(2, 3, 4, 5, 6),
           tickmode = "array",
           title = "Months"
         ), autosize = FALSE, legend = list(x = 0.1, y = 0.9), hovermode = "compare")

##-----------------------------Recovery rate------------------

frRecovery$rec_cri <- ifelse(frRecovery$recovery_rate <= 30 , frRecovery$recovery_rate, NA)
frRecovery$rec_avg <- ifelse(frRecovery$recovery_rate >= 31 && frRecovery$recovery_rate <= 60 , frRecovery$recovery_rate, NA)
frRecovery$rec_bet <- ifelse(frRecovery$recovery_rate >= 61 , frRecovery$recovery_rate, NA)

plot_ly(frRecovery, x = ~month, y = ~ round(rec_bet, 2), name = 'Better recovery rate', marker = list(color = 'green', size = 10), 
        type = 'scatter', mode = 'markers') %>% 
  add_trace(y = ~ round(rec_cri, 2), name = "Critical recovery rate", marker = list(color = 'red', size = 10), 
            type = 'scatter', mode = 'markers') %>% 
  add_trace(y = ~ round(rec_avg, 2), name = "Average recovery rate", marker = list(color = 'blue', size = 10), 
            type = 'scatter', mode = 'markers') %>%
  layout(title = "Recovery rate in France for past 6 months", yaxis = list(title = "Recovery rate"), 
         xaxis = list(
           ticktext = list("Jan", "Feb", "March", "April", "May", "June"), 
           tickvals = list(1, 2, 3, 4, 5, 6),
           tickmode = "array",
           title = "Months"
         ), autosize = FALSE) 

#-----------------------------------------------------------------------
# Map

pal <- colorFactor(c("orange", "blue", "red", "green"), domain = c("confirmed", "active", "death", "recovered"))

# adding log cases for radius purpose
covidL <- covidL %>% 
  mutate(log_cases = 2 * log(cases)) 

# Split act as 3 data frames for 3 types
covidL.split <- covidL %>% 
  split(covidL$type)

# Map with different overlay
groupI <- names(covidL.split)
leaflet() %>% addTiles() %>% addProviderTiles(providers$CartoDB.Positron) %>% 
  addCircleMarkers(data = covidL.split[[1]], lat = ~lat, lng = ~long,
                   color = active_color,
                   stroke = FALSE,
                   fillOpacity = 0.5,
                   radius = ~log_cases,
                   popup = paste0("Country: ", covidL.split[[1]]$country,
                                  "</br>Type: ", covidL.split[[1]]$type,
                                  "</br>Cases: ", covidL.split[[1]]$cases),
                   group = groupI[1],
                   labelOptions = labelOptions(noHide = F,
                                               direction = 'auto')) %>%
  addCircleMarkers(data = covidL.split[[2]], lat = ~lat, lng = ~long,
                   color = confirmed_color,
                   stroke = FALSE,
                   fillOpacity = 0.5,
                   radius = ~log_cases,
                   popup = paste0("Country: ", covidL.split[[2]]$country,
                                  "</br>Type: ", covidL.split[[2]]$type,
                                  "</br>Cases: ", covidL.split[[2]]$cases),
                   group = groupI[2],
                   labelOptions = labelOptions(noHide = F,
                                               direction = 'auto')) %>%
  addCircleMarkers(data = covidL.split[[3]], lat = ~lat, lng = ~long,
                   color = death_color,
                   stroke = FALSE,
                   fillOpacity = 0.5,
                   radius = ~log_cases,
                   popup = paste0("Country: ", covidL.split[[3]]$country,
                                  "</br>Type: ", covidL.split[[3]]$type,
                                  "</br>Cases: ", covidL.split[[3]]$cases),
                   group = groupI[3],
                   labelOptions = labelOptions(noHide = F,
                                               direction = 'auto')) %>%
  addCircleMarkers(data = covidL.split[[4]], lat = ~lat, lng = ~long,
                   color = recovered_color,
                   stroke = FALSE,
                   fillOpacity = 0.5,
                   radius = ~log_cases,
                   popup = paste0("Country: ", covidL.split[[4]]$country,
                                  "</br>Type: ", covidL.split[[4]]$type,
                                  "</br>Cases: ", covidL.split[[4]]$cases),
                   group = groupI[4],
                   labelOptions = labelOptions(noHide = F,
                                               direction = 'auto')) %>%
  addLayersControl(overlayGroups = names(covidL.split),
                   options = layersControlOptions(collapsed = FALSE))

##-----------------------------Top 5 Countries------------------

# top 5 countries with most active cases
covCountry <- covidW %>% 
  ungroup() %>% 
  top_n(5, active)

# Extract those top 5 country names 
countnames <- covCountry$country

# Contains top 5 countries with their daily cases
coworld <- covid %>% 
  select(date, country, type, cases) %>% 
  group_by(date, country, type) %>% 
  summarise(cases = sum(cases)) %>% 
  pivot_wider(names_from = type,
              values_from = cases) %>% 
  mutate(active = confirmed - death - recovered) %>% 
  filter(country %in% countnames)  %>% 
  arrange(date) 

# Now considering top 5 countries with their daily cases for past 14 days
tailWorld <- coworld %>% 
  tail(70, date) %>% 
  select(date, country, confirmed) %>% 
  pivot_wider(names_from = country, 
              values_from = confirmed)

# Daily Confirmed Cases in Top 5 Countries for past 14 days (Log scale)
plot_ly(data = tailWorld, 
        x = ~ date,
        y = ~ US,
        name = 'US', 
        mode = 'lines+markers', 
        type = 'scatter',
        stackgroup = 'one') %>% 
  add_trace(y = ~ France,
            name = "France",
            mode = 'lines+markers') %>%
  add_trace(y = ~ `United Kingdom`,
            name = "United Kingdom",
            mode = 'lines+markers') %>% 
  add_trace(y = ~ Spain,
            name = "Spain",
            mode = 'lines+markers') %>%
  add_trace(y = ~ `Netherlands`,
            name = "Netherlands",
            mode = 'lines+markers') %>%
  layout(title = "Daily Confirmed Cases in Top 5 Countries for past 14 days (Log scale)",
         yaxis = list(title = "Daily Confirmed Cases",
                      type = "log"),
         xaxis = list(title = "Date",
                      type = "date"),
         legend = list(x = 0.1, y = 0.9),
         hovermode = "compare")

options(warn = defaultWarn)