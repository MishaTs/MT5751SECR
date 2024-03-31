library(tidyverse)
library(ggplot2)
library(viridisLite)

data <- data.frame(
  Design = factor(rep(c("Compact", "Diffuse"), each = 3), 
                  levels = c("Compact", "Diffuse")),
  Coefficient = rep(c("Sigma (km)", "Density (per 100 km2)", "Detection"), times = 2),
  Estimate = c(12.23, 0.12, 0.023, 3.27, 0.534, 0.023),
  CI_lower = c(8.78, 0.041, 0.013, 2.45, 0.256, 0.013),
  CI_upper = c(17.04, 0.316, 0.040, 4.35, 1.113, 0.040),
  Session = factor(rep(c("1", "2"), each = 3))
)

#data$Coefficient <- factor(data$Coefficient, levels=c("A", "B", "C"))

#lbs <- setNames(c("'Sigma (km)'", 
#                  "'Density (per 100' (km*2) ')", 
#                  "'Detection'"),
#               LETTERS[1:3]
#)[levels(data$Coefficient)]

plot <- ggplot(data, aes(x = Session, y = Estimate, fill = Design, colour = Design)) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), 
                width = 0.2, 
                position = position_dodge(width = 0.25), 
                size = 1) +  # thicker error bars
  scale_colour_viridis_d(begin = 0.2,
                         end = 0.8,
                         direction = -1) +
  geom_point(position = position_dodge(width = 0.25), 
             size = 3, 
             colour="black", 
             pch = 21) + # thicker points for estimates
  facet_wrap(~Coefficient, 
             #labeller=as_labeller(lbs, label_parsed),
             scales = "free_y") +
  theme_minimal() +
  labs(x = "Session", y = "Estimate") +
  scale_fill_viridis_d(begin = 0.2,
                        end = 0.8,
                        direction = -1) +
  scale_x_discrete(labels = c("0", "1")) +  # Set specific labels for the x axis.
  theme(
    axis.text = element_text(size = 12),  # Adjust axis text size
    axis.title = element_text(size = 14),  # Adjust axis label size
    strip.text = element_text(size = 14),  # Adjust facet label size
    axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')
  )

# Save the plot with transparent background
ggsave("CoefPlot.png", 
       plot, 
       bg = "transparent", 
       dpi = 300, 
       width = 30, 
       height = 10, 
       units = "cm")



library(openmeteo)
geocode("Gulmit")
geocode("Nagir")

#looking at Hispar Village which is in both surveys
lat <- 217/6
lon <- 75
dates1 <- seq(as.Date("2016-03-01"), as.Date("2016-05-31"), by="days")
dates2 <- seq(as.Date("2018-04-01"), as.Date("2018-06-30"), by="days")

weatherHistData1 <- weather_history(location = c(lat,lon),
                                    start = min(dates1),
                                    end = max(dates1),
                                    daily = c("temperature_2m_max",
                                              "temperature_2m_min",
                                              "temperature_2m_mean",
                                              "apparent_temperature_max",
                                              "apparent_temperature_min",
                                              "apparent_temperature_mean",
                                              "sunrise",
                                              "sunset",
                                              "daylight_duration",
                                              "sunshine_duration",
                                              "precipitation_sum",
                                              "rain_sum",
                                              "snowfall_sum",
                                              "precipitation_hours",
                                              "wind_speed_10m_max",
                                              "wind_gusts_10m_max",
                                              "wind_direction_10m_dominant",
                                              "shortwave_radiation_sum",
                                              "et0_fao_evapotranspiration"))

weatherHistData2 <- weather_history(location = c(lat,lon),
                                    start = min(dates2),
                                    end = max(dates2),
                                    daily = c("temperature_2m_max",
                                              "temperature_2m_min",
                                              "temperature_2m_mean",
                                              "apparent_temperature_max",
                                              "apparent_temperature_min",
                                              "apparent_temperature_mean",
                                              "sunrise",
                                              "sunset",
                                              "daylight_duration",
                                              "sunshine_duration",
                                              "precipitation_sum",
                                              "rain_sum",
                                              "snowfall_sum",
                                              "precipitation_hours",
                                              "wind_speed_10m_max",
                                              "wind_gusts_10m_max",
                                              "wind_direction_10m_dominant",
                                              "shortwave_radiation_sum",
                                              "et0_fao_evapotranspiration"))

weatherHistDat1 <- weatherHistData1 %>% rownames_to_column("i") %>% mutate(Session = 1,
                                                                            id = as.numeric(i),
                                                                            day = as.Date(format(date, format="%m-%d"),
                                                                                          format = "%m-%d"))
weatherHistDat2 <- weatherHistData2 %>% rownames_to_column("i") %>% mutate(Session = 2,
                                                                            id = as.numeric(i) + 31,
                                                                            day = as.Date(format(date, format="%m-%d"),
                                                                                          format = "%m-%d"))
weatherDat <- rbind(weatherHistData1, weatherHistData2)
weatherDatAlt <- left_join(weatherHistData1, weatherHistData2, by = "id")

ggplot() + 
  geom_ribbon(data = weatherHistDat1, 
              aes(x=day, y=daily_temperature_2m_mean, 
                  ymin=daily_temperature_2m_min, 
                  ymax=daily_temperature_2m_max),
              fill="#7ad151", alpha = 0.25) +
  geom_line(data = weatherHistDat1, 
            aes(x=day, y=daily_temperature_2m_mean), linewidth=1, color="#7ad151") + 
  geom_ribbon(data = weatherHistDat2, 
              aes(x=day, y=daily_temperature_2m_mean, 
                  ymin=daily_temperature_2m_min, 
                  ymax=daily_temperature_2m_max),
              fill="#440154", alpha = 0.25) +
  geom_line(data = weatherHistDat2, 
    aes(x=day, y=daily_temperature_2m_mean), linewidth=1, color="#440154") + 
  xlab("Survey Date") + 
  ylab("Temperature (C)") + 
  theme_bw()

#sunshine duration?
ggplot() + 
  geom_line(data = weatherHistDat1, 
            aes(x=day, y=daily_sunshine_duration), linewidth=1, color="blue") + 
  geom_line(data = weatherHistDat2, 
            aes(x=day, y=daily_sunshine_duration), linewidth=1, color="red") + 
  xlab("Survey Date") + 
  ylab("Daily Snowfall Sum (mm)") + 
  theme_bw()





# apparent temperature is better
tempPlot <- ggplot() + 
  geom_ribbon(data = weatherHistDat1, 
              aes(x=day, y=daily_apparent_temperature_mean, 
                  ymin=daily_apparent_temperature_min, 
                  ymax=daily_apparent_temperature_max),
              fill="#7ad151", alpha = 0.25) +
  geom_line(data = weatherHistDat1, 
            aes(x=day, y=daily_apparent_temperature_mean), linewidth=1, color="#7ad151") + 
  geom_ribbon(data = weatherHistDat2, 
              aes(x=day, y=daily_apparent_temperature_mean, 
                  ymin=daily_apparent_temperature_min, 
                  ymax=daily_apparent_temperature_max),
              fill="#440154", alpha = 0.25) +
  geom_line(data = weatherHistDat2, 
            aes(x=day, y=daily_apparent_temperature_mean), linewidth=1, color="#440154") + 
  scale_colour_manual("", 
                      values = c("#7ad151", "#440154")) +  
  ggtitle("Temperatures in Diffuse (Green) and Compact (Purple) Surveys") +
  xlab("Survey Date") + 
  ylab("Apparent Temperature (C)") + 
  theme_minimal() + 
  theme(axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA),
        plot.title = element_text(size = 16),
        axis.text = element_text(size = 12),  # Adjust axis text size
        axis.title = element_text(size = 14),  # Adjust axis label size
        strip.text = element_text(size = 14))  # Adjust facet label size

# Save the plot with transparent background
ggsave("TempPlot.png", 
       tempPlot, 
       bg = "transparent", 
       dpi = 300, 
       width = 20, 
       height = 10, 
       units = "cm")

# snowfall?
ggplot() + 
  geom_line(data = weatherHistDat1, 
            aes(x=day, y=daily_snowfall_sum), linewidth=1, color="#7ad151") + 
  geom_line(data = weatherHistDat2, 
            aes(x=day, y=daily_snowfall_sum), linewidth=1, color="#440154") + 
  xlab("Survey Date") + 
  ylab("Daily Snowfall Sum (mm)") + 
  theme_bw()

