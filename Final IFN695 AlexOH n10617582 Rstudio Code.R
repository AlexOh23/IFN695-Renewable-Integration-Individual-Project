library(lubridate)
library(tidyr)
library(lme4) 
library(tidyverse) 
library(RColorBrewer) 
library(lmerTest)
library(effectsize)
library(parameters)
library(dplyr)
library(corrplot)
library(GGally)
library(ggplot2)
library(scales)
library(forecast)
library(imputeTS)
library(caret) 
library(ggpubr)

#Temporal Correlation Analysis

#Load Temporal data (2018) for 12 months

temporal_2018_Data1 <- read.csv("WEB_AVERAGE_PRICE_MONTH_201801.csv") 
temporal_2018_Data2 <- read.csv("WEB_AVERAGE_PRICE_MONTH_201802.csv") 
temporal_2018_Data3 <- read.csv("WEB_AVERAGE_PRICE_MONTH_201803.csv") 
temporal_2018_Data4 <- read.csv("WEB_AVERAGE_PRICE_MONTH_201804.csv") 
temporal_2018_Data5 <- read.csv("WEB_AVERAGE_PRICE_MONTH_201805.csv") 
temporal_2018_Data6 <- read.csv("WEB_AVERAGE_PRICE_MONTH_201806.csv") 
temporal_2018_Data7 <- read.csv("WEB_AVERAGE_PRICE_MONTH_201807.csv") 
temporal_2018_Data8 <- read.csv("WEB_AVERAGE_PRICE_MONTH_201808.csv") 
temporal_2018_Data9 <- read.csv("WEB_AVERAGE_PRICE_MONTH_201809.csv") 
temporal_2018_Data10 <- read.csv("WEB_AVERAGE_PRICE_MONTH_201810.csv") 
temporal_2018_Data11 <- read.csv("WEB_AVERAGE_PRICE_MONTH_201811.csv") 
temporal_2018_Data12 <- read.csv("WEB_AVERAGE_PRICE_MONTH_201812.csv") 

#Load Temporal data (2019) for 12 months
temporal_2019_Data1 <- read.csv("WEB_AVERAGE_PRICE_MONTH_201901.csv") 
temporal_2019_Data2 <- read.csv("WEB_AVERAGE_PRICE_MONTH_201902.csv") 
temporal_2019_Data3 <- read.csv("WEB_AVERAGE_PRICE_MONTH_201903.csv") 
temporal_2019_Data4 <- read.csv("WEB_AVERAGE_PRICE_MONTH_201904.csv") 
temporal_2019_Data5 <- read.csv("WEB_AVERAGE_PRICE_MONTH_201905.csv") 
temporal_2019_Data6 <- read.csv("WEB_AVERAGE_PRICE_MONTH_201906.csv") 
temporal_2019_Data7 <- read.csv("WEB_AVERAGE_PRICE_MONTH_201907.csv") 
temporal_2019_Data8 <- read.csv("WEB_AVERAGE_PRICE_MONTH_201908.csv") 
temporal_2019_Data9 <- read.csv("WEB_AVERAGE_PRICE_MONTH_201909.csv") 
temporal_2019_Data10 <- read.csv("WEB_AVERAGE_PRICE_MONTH_201910.csv") 
temporal_2019_Data11 <- read.csv("WEB_AVERAGE_PRICE_MONTH_201911.csv") 
temporal_2019_Data12 <- read.csv("WEB_AVERAGE_PRICE_MONTH_201912.csv") 

#Load Temporal data (2020) for 12 months
temporal_2020_Data1 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202001.csv") 
temporal_2020_Data2 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202002.csv") 
temporal_2020_Data3 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202003.csv") 
temporal_2020_Data4 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202004.csv") 
temporal_2020_Data5 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202005.csv") 
temporal_2020_Data6 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202006.csv") 
temporal_2020_Data7 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202007.csv") 
temporal_2020_Data8 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202008.csv") 
temporal_2020_Data9 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202009.csv") 
temporal_2020_Data10 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202010.csv") 
temporal_2020_Data11 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202011.csv") 
temporal_2020_Data12 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202012.csv") 


#Load Temporal data (2021) for 12 months
temporal_2021_Data1 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202101.csv") 
temporal_2021_Data2 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202102.csv") 
temporal_2021_Data3 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202103.csv") 
temporal_2021_Data4 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202104.csv") 
temporal_2021_Data5 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202105.csv") 
temporal_2021_Data6 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202106.csv") 
temporal_2021_Data7 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202107.csv") 
temporal_2021_Data8 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202108.csv") 
temporal_2021_Data9 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202109.csv") 
temporal_2021_Data10 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202110.csv") 
temporal_2021_Data11 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202111.csv") 
temporal_2021_Data12 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202112.csv") 

#Load Temporal data (2022) for 12 months
temporal_2022_Data1 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202201.csv") 
temporal_2022_Data2 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202202.csv") 
temporal_2022_Data3 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202203.csv") 
temporal_2022_Data4 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202204.csv") 
temporal_2022_Data5 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202205.csv") 
temporal_2022_Data6 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202206.csv") 
temporal_2022_Data7 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202207.csv") 
temporal_2022_Data8 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202208.csv") 
temporal_2022_Data9 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202209.csv") 
temporal_2022_Data10 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202210.csv") 
temporal_2022_Data11 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202211.csv") 
temporal_2022_Data12 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202212.csv") 

#Load Temporal data (2023) for 12 months
temporal_2023_Data1 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202301.csv") 
temporal_2023_Data2 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202302.csv") 
temporal_2023_Data3 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202303.csv") 
temporal_2023_Data4 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202304.csv") 
temporal_2023_Data5 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202305.csv") 
temporal_2023_Data6 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202306.csv") 
temporal_2023_Data7 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202307.csv") 
temporal_2023_Data8 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202308.csv") 
temporal_2023_Data9 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202309.csv") 
temporal_2023_Data10 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202310.csv") 
temporal_2023_Data11 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202311.csv") 
temporal_2023_Data12 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202312.csv") 

#Load Temporal data (2024) for 12 months
temporal_2024_Data1 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202401.csv") 
temporal_2024_Data2 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202402.csv") 
temporal_2024_Data3 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202403.csv") 
temporal_2024_Data4 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202404.csv") 
temporal_2024_Data5 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202405.csv") 
temporal_2024_Data6 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202406.csv") 
temporal_2024_Data7 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202407.csv") 
temporal_2024_Data8 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202408.csv") 
temporal_2024_Data9 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202409.csv") 
temporal_2024_Data10 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202410.csv") 
temporal_2024_Data11 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202411.csv") 
temporal_2024_Data12 <- read.csv("WEB_AVERAGE_PRICE_MONTH_202412.csv") 




Temporal_2018 = bind_rows(temporal_2018_Data1, temporal_2018_Data2, temporal_2018_Data3, temporal_2018_Data4, temporal_2018_Data5, temporal_2018_Data6, temporal_2018_Data7, temporal_2018_Data8, temporal_2018_Data9, temporal_2018_Data10, temporal_2018_Data11, temporal_2018_Data12)
Temporal_2019 = bind_rows(temporal_2019_Data1, temporal_2019_Data2, temporal_2019_Data3, temporal_2019_Data4, temporal_2019_Data5, temporal_2019_Data6, temporal_2019_Data7, temporal_2019_Data8, temporal_2019_Data9, temporal_2019_Data10, temporal_2019_Data11, temporal_2019_Data12)
Temporal_2020 = bind_rows(temporal_2020_Data1, temporal_2020_Data2, temporal_2020_Data3, temporal_2020_Data4, temporal_2020_Data5, temporal_2020_Data6, temporal_2020_Data7, temporal_2020_Data8, temporal_2020_Data9, temporal_2020_Data10, temporal_2020_Data11, temporal_2020_Data12)
Temporal_2021 = bind_rows(temporal_2021_Data1, temporal_2021_Data2, temporal_2021_Data3, temporal_2021_Data4, temporal_2021_Data5, temporal_2021_Data6, temporal_2021_Data7, temporal_2021_Data8, temporal_2021_Data9, temporal_2021_Data10, temporal_2021_Data11, temporal_2021_Data12)
Temporal_2022 = bind_rows(temporal_2022_Data1, temporal_2022_Data2, temporal_2022_Data3, temporal_2022_Data4, temporal_2022_Data5, temporal_2022_Data6, temporal_2022_Data7, temporal_2022_Data8, temporal_2022_Data9, temporal_2022_Data10, temporal_2022_Data11, temporal_2022_Data12)
Temporal_2023 = bind_rows(temporal_2023_Data1, temporal_2023_Data2, temporal_2023_Data3, temporal_2023_Data4, temporal_2023_Data5, temporal_2023_Data6, temporal_2023_Data7, temporal_2023_Data8, temporal_2023_Data9, temporal_2023_Data10, temporal_2023_Data11, temporal_2023_Data12)
Temporal_2024 = bind_rows(temporal_2024_Data1, temporal_2024_Data2, temporal_2024_Data3, temporal_2024_Data4, temporal_2024_Data5, temporal_2024_Data6, temporal_2024_Data7, temporal_2024_Data8, temporal_2024_Data9, temporal_2024_Data10, temporal_2024_Data11, temporal_2024_Data12)



Temporal_Years_combined = bind_rows(Temporal_2018, Temporal_2019, Temporal_2020, Temporal_2021, Temporal_2022, Temporal_2023, Temporal_2024)

#Temporal_2024, select relevant columns for Temporal Analysis
Temporal_Years_combined <- Temporal_Years_combined[, c("MONTHNUMBER", "REGIONID", "AVGRRP")]
#Filter by Region [NSW]
Temporal_Years_combined_nsw <- Temporal_Years_combined[Temporal_Years_combined$REGIONID == "NSW1", ]
#Filter by Region [QLD]
Temporal_Years_combined_qld <- Temporal_Years_combined[Temporal_Years_combined$REGIONID == "QLD1", ]
#Filter by Region [SA]
Temporal_Years_combined_sa <- Temporal_Years_combined[Temporal_Years_combined$REGIONID == "SA1", ]
#Filter by Region [VIC]
Temporal_Years_combined_vic <- Temporal_Years_combined[Temporal_Years_combined$REGIONID == "VIC1", ]
#Filter by Region [Tas]
Temporal_Years_combined_tas <- Temporal_Years_combined[Temporal_Years_combined$REGIONID == "TAS1", ]


#New South Wales

# Convert the MONTHNUMBER into a date format
Temporal_Years_combined_nsw$MONTHNUMBER <- as.Date(Temporal_Years_combined_nsw$MONTHNUMBER, format = "%Y/%m/%d")



# Define an origin date (e.g., 2017/7/31) as the start date from the df for Temporal Analysis
origin_date <- as.Date("2017/07/31", format = "%Y/%m/%d")

# Calculate days since origin
Temporal_Years_combined_nsw$days_since_origin <- as.numeric(Temporal_Years_combined_nsw$MONTHNUMBER - origin_date)

# Set breaks every 180 days
breaks <- seq(min(Temporal_Years_combined_nsw$days_since_origin), max(Temporal_Years_combined_nsw$days_since_origin), by = 180)

# Generate labels
labels <- format(origin_date + breaks, "%b %Y")


correlation <- cor(Temporal_Years_combined_nsw$days_since_origin, Temporal_Years_combined_nsw$AVGRRP)
print(correlation)






  
  #Create rsquared and p value for Temporal analysis (NSW)
  stats <- Temporal_Years_combined_nsw %>%
    summarise(
      r_squared = summary(lm(AVGRRP ~ days_since_origin))$r.squared,
      p_value = summary(lm(AVGRRP ~ days_since_origin))$coefficients[2, 4]
    ) %>%
    mutate(
      label = paste0("R² = ", round(r_squared, 3), "\n",
                     "p value = ", signif(p_value, 3), "\n",
                     "correlation = ", round(correlation, 3))
    )
    
  
  #ggplot for temporal analysis for NSW with rsquared and p value
  ggplot(Temporal_Years_combined_nsw, aes(x = days_since_origin, y = AVGRRP)) +
    geom_point(alpha = 0.2, colour = 'red') +
    geom_smooth(method = 'lm', se = FALSE, colour = 'blue') +
    geom_text(
      data = stats,
      aes(x = min(Temporal_Years_combined_nsw$days_since_origin) + 0.05 * diff(range(Temporal_Years_combined_nsw$days_since_origin)),
          y = max(Temporal_Years_combined_nsw$AVGRRP) - 0.05 * diff(range(Temporal_Years_combined_nsw$AVGRRP)),
          label = label),
      inherit.aes = FALSE,
      size = 4
    ) +
    labs(title = paste("Average Monthly RRP vs Temporal years from July 2017 to Dec 2024 for New South Wales"),
         x = "Years", 
         y = "Average RRP") +
    scale_x_continuous(
      breaks = breaks,
      labels = labels
    )  +
    
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5)  # Horizontal labels
    ) +
    theme_minimal()
  
  
  
  
  
  

  

  
  #Queensland
  
  # Convert the MONTHNUMBER into a date format
  Temporal_Years_combined_qld$MONTHNUMBER <- as.Date(Temporal_Years_combined_qld$MONTHNUMBER, format = "%Y/%m/%d")
  
  
  
  # Define an origin date (e.g., 2017/7/31) as the start date from the df for Temporal Analysis
  origin_date <- as.Date("2017/07/31", format = "%Y/%m/%d")
  
  # Calculate days since origin
  Temporal_Years_combined_qld$days_since_origin <- as.numeric(Temporal_Years_combined_qld$MONTHNUMBER - origin_date)
  
  # Set breaks every 180 days
  breaks <- seq(min(Temporal_Years_combined_qld$days_since_origin), max(Temporal_Years_combined_qld$days_since_origin), by = 180)
  
  # Generate labels
  labels <- format(origin_date + breaks, "%b %Y")
  
  
  correlation <- cor(Temporal_Years_combined_qld$days_since_origin, Temporal_Years_combined_qld$AVGRRP)
  print(correlation)
  
  
  #Create rsquared and p value for Temporal analysis (qld)
  stats <- Temporal_Years_combined_qld %>%
    summarise(
      r_squared = summary(lm(AVGRRP ~ days_since_origin))$r.squared,
      p_value = summary(lm(AVGRRP ~ days_since_origin))$coefficients[2, 4]
    ) %>%
    mutate(
      label = paste0("R² = ", round(r_squared, 3), "\n",
                     "p value = ", signif(p_value, 3), "\n",
                     "correlation = ", round(correlation, 3))
    )
  
  
  #ggplot for temporal analysis for qld with rsquared and p value
  ggplot(Temporal_Years_combined_qld, aes(x = days_since_origin, y = AVGRRP)) +
    geom_point(alpha = 0.2, colour = 'red') +
    geom_smooth(method = 'lm', se = FALSE, colour = 'blue') +
    geom_text(
      data = stats,
      aes(x = min(Temporal_Years_combined_qld$days_since_origin) + 0.05 * diff(range(Temporal_Years_combined_qld$days_since_origin)),
          y = max(Temporal_Years_combined_qld$AVGRRP) - 0.05 * diff(range(Temporal_Years_combined_qld$AVGRRP)),
          label = label),
      inherit.aes = FALSE,
      size = 4
    ) +
    labs(title = paste("Average Monthly RRP vs Temporal years from July 2017 to Dec 2024 for Queensland"),
         x = "Years", 
         y = "Average RRP") +
    scale_x_continuous(
      breaks = breaks,
      labels = labels
    )  +
    
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5)  # Horizontal labels
    ) +
    theme_minimal()
  
  
  
  #South Australia
  
  # Convert the MONTHNUMBER into a date format
  Temporal_Years_combined_sa$MONTHNUMBER <- as.Date(Temporal_Years_combined_sa$MONTHNUMBER, format = "%Y/%m/%d")
  
  
  
  # Define an origin date (e.g., 2017/7/31) as the start date from the df for Temporal Analysis
  origin_date <- as.Date("2017/07/31", format = "%Y/%m/%d")
  
  # Calculate days since origin
  Temporal_Years_combined_sa$days_since_origin <- as.numeric(Temporal_Years_combined_sa$MONTHNUMBER - origin_date)
  
  # Set breaks every 180 days
  breaks <- seq(min(Temporal_Years_combined_sa$days_since_origin), max(Temporal_Years_combined_sa$days_since_origin), by = 180)
  
  # Generate labels
  labels <- format(origin_date + breaks, "%b %Y")
  
  
  correlation <- cor(Temporal_Years_combined_sa$days_since_origin, Temporal_Years_combined_sa$AVGRRP)
  print(correlation)
  
  
  #Create rsquared and p value for Temporal analysis (sa)
  stats <- Temporal_Years_combined_sa %>%
    summarise(
      r_squared = summary(lm(AVGRRP ~ days_since_origin))$r.squared,
      p_value = summary(lm(AVGRRP ~ days_since_origin))$coefficients[2, 4]
    ) %>%
    mutate(
      label = paste0("R² = ", round(r_squared, 3), "\n",
                     "p value = ", signif(p_value, 3), "\n",
                     "correlation = ", round(correlation, 3))
    )
  
  
  #ggplot for temporal analysis for sa with rsquared and p value
  ggplot(Temporal_Years_combined_sa, aes(x = days_since_origin, y = AVGRRP)) +
    geom_point(alpha = 0.2, colour = 'red') +
    geom_smooth(method = 'lm', se = FALSE, colour = 'blue') +
    geom_text(
      data = stats,
      aes(x = min(Temporal_Years_combined_sa$days_since_origin) + 0.05 * diff(range(Temporal_Years_combined_sa$days_since_origin)),
          y = max(Temporal_Years_combined_sa$AVGRRP) - 0.05 * diff(range(Temporal_Years_combined_sa$AVGRRP)),
          label = label),
      inherit.aes = FALSE,
      size = 4
    ) +
    labs(title = paste("Average Monthly RRP vs Temporal years from July 2017 to Dec 2024 for South Australia"),
         x = "Years", 
         y = "Average RRP") +
    scale_x_continuous(
      breaks = breaks,
      labels = labels
    )  +
    
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5)  # Horizontal labels
    ) +
    theme_minimal()
  
  
  #Tasmania
  
  # Convert the MONTHNUMBER into a date format
  Temporal_Years_combined_tas$MONTHNUMBER <- as.Date(Temporal_Years_combined_tas$MONTHNUMBER, format = "%Y/%m/%d")
  
  
  
  # Define an origin date (e.g., 2017/7/31) as the start date from the df for Temporal Analysis
  origin_date <- as.Date("2017/07/31", format = "%Y/%m/%d")
  
  # Calculate days since origin
  Temporal_Years_combined_tas$days_since_origin <- as.numeric(Temporal_Years_combined_tas$MONTHNUMBER - origin_date)
  
  # Set breaks every 180 days
  breaks <- seq(min(Temporal_Years_combined_tas$days_since_origin), max(Temporal_Years_combined_tas$days_since_origin), by = 180)
  
  # Generate labels
  labels <- format(origin_date + breaks, "%b %Y")
  
  
  correlation <- cor(Temporal_Years_combined_tas$days_since_origin, Temporal_Years_combined_tas$AVGRRP)
  print(correlation)
  
  
  #Create rsquared and p value for Temporal analysis (tas)
  stats <- Temporal_Years_combined_tas %>%
    summarise(
      r_squared = summary(lm(AVGRRP ~ days_since_origin))$r.squared,
      p_value = summary(lm(AVGRRP ~ days_since_origin))$coefficients[2, 4]
    ) %>%
    mutate(
      label = paste0("R² = ", round(r_squared, 3), "\n",
                     "p value = ", signif(p_value, 3), "\n",
                     "correlation = ", round(correlation, 3))
    )
  
  
  #ggplot for temporal analysis for tas with rsquared and p value
  ggplot(Temporal_Years_combined_tas, aes(x = days_since_origin, y = AVGRRP)) +
    geom_point(alpha = 0.2, colour = 'red') +
    geom_smooth(method = 'lm', se = FALSE, colour = 'blue') +
    geom_text(
      data = stats,
      aes(x = min(Temporal_Years_combined_tas$days_since_origin) + 0.05 * diff(range(Temporal_Years_combined_tas$days_since_origin)),
          y = max(Temporal_Years_combined_tas$AVGRRP) - 0.05 * diff(range(Temporal_Years_combined_tas$AVGRRP)),
          label = label),
      inherit.aes = FALSE,
      size = 4
    ) +
    labs(title = paste("Average Monthly RRP vs Temporal years from July 2017 to Dec 2024 for Tasmania"),
         x = "Years", 
         y = "Average RRP") +
    scale_x_continuous(
      breaks = breaks,
      labels = labels
    )  +
    
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5)  # Horizontal labels
    ) +
    theme_minimal()
  
  
  
  #Victoria
  
  # Convert the MONTHNUMBER into a date format
  Temporal_Years_combined_vic$MONTHNUMBER <- as.Date(Temporal_Years_combined_vic$MONTHNUMBER, format = "%Y/%m/%d")
  
  
  
  # Define an origin date (e.g., 2017/7/31) as the start date from the df for Temporal Analysis
  origin_date <- as.Date("2017/07/31", format = "%Y/%m/%d")
  
  # Calculate days since origin
  Temporal_Years_combined_vic$days_since_origin <- as.numeric(Temporal_Years_combined_vic$MONTHNUMBER - origin_date)
  
  # Set breaks every 180 days
  breaks <- seq(min(Temporal_Years_combined_vic$days_since_origin), max(Temporal_Years_combined_vic$days_since_origin), by = 180)
  
  # Generate labels
  labels <- format(origin_date + breaks, "%b %Y")
  
  
  correlation <- cor(Temporal_Years_combined_vic$days_since_origin, Temporal_Years_combined_vic$AVGRRP)
  print(correlation)
  
  
  #Create rsquared and p value for Temporal analysis (vic)
  stats <- Temporal_Years_combined_vic %>%
    summarise(
      r_squared = summary(lm(AVGRRP ~ days_since_origin))$r.squared,
      p_value = summary(lm(AVGRRP ~ days_since_origin))$coefficients[2, 4]
    ) %>%
    mutate(
      label = paste0("R² = ", round(r_squared, 3), "\n",
                     "p value = ", signif(p_value, 3), "\n",
                     "correlation = ", round(correlation, 3))
    )
  
  
  #ggplot for temporal analysis for vic with rsquared and p value
  ggplot(Temporal_Years_combined_vic, aes(x = days_since_origin, y = AVGRRP)) +
    geom_point(alpha = 0.2, colour = 'red') +
    geom_smooth(method = 'lm', se = FALSE, colour = 'blue') +
    geom_text(
      data = stats,
      aes(x = min(Temporal_Years_combined_vic$days_since_origin) + 0.05 * diff(range(Temporal_Years_combined_vic$days_since_origin)),
          y = max(Temporal_Years_combined_vic$AVGRRP) - 0.05 * diff(range(Temporal_Years_combined_vic$AVGRRP)),
          label = label),
      inherit.aes = FALSE,
      size = 4
    ) +
    labs(title = paste("Average Monthly RRP vs Temporal years from July 2017 to Dec 2024 for Victoria"),
         x = "Years", 
         y = "Average RRP") +
    scale_x_continuous(
      breaks = breaks,
      labels = labels
    )  +
    
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5)  # Horizontal labels
    ) +
    theme_minimal()
  
  
  
  
  
  
  # Using Predictive Modelling for Temporal_Years_combined_nsw 


  # Summary statistics (NSW)
  summary(Temporal_Years_combined_nsw) 
  
  
  Temporal_Years_combined_nsw <- na.omit(Temporal_Years_combined_nsw)
  
  set.seed(123)  # to ensure predictive model can be reproduced
  
  # Split the data into 80% training and 20% testing
  train_index <- createDataPartition(Temporal_Years_combined_nsw$AVGRRP, p = 0.8, list = FALSE)
  train_data <- Temporal_Years_combined_nsw[train_index, ]
  test_data <- Temporal_Years_combined_nsw[-train_index, ]
  
  # Train model_nsw
  Temporal_Years_combined_nsw_model <- train(
    AVGRRP ~ days_since_origin,
    data = train_data,
    method = "lm",  
    trControl = trainControl(method = "cv", number = 5)  
  )
  
  # View model_nsw summary
  print(Temporal_Years_combined_nsw_model)
  
  # Make the predictions
  predictions <- predict(Temporal_Years_combined_nsw_model, newdata = test_data)
  
  # Calculate the results
  results <- data.frame(
    Actual = test_data$AVGRRP,
    Predicted = predictions
  )
  
  # Calculate the RMSE and R-squared with the results generated
  rmse <- sqrt(mean((results$Actual - results$Predicted)^2))
  
  print(paste("RMSE:", rmse))
  
  # Plot Temporal_Years_combined_nsw_model for actual vs predicted
  plot(test_data$AVGRRP, predictions, main = "Spot Price (Daily RRP) projections in  New South Wales",
       xlab = "Years", ylab = "Daily RRP")
  abline(0, 1, col = "red")  # Add diagonal line for reference
  
  
  
  
  # Extract model_nsw for nsw coefficients
  coefficients <- coef(Temporal_Years_combined_nsw_model$finalModel)
  print(coefficients)
  
  
  # Extract coefficients from nsw model_nsw
  coefs <- coef(Temporal_Years_combined_nsw_model$finalmodelModel)
  intercept <- coefs[1]  
  SETTLEMENT_coef <- coefs["SETTLEMENTDATE"]
  
  
  # Create predicted value from model_nsw
  Temporal_Years_combined_nsw$AVGRRP_new <- 71.68090725 + 0.01679844   * Temporal_Years_combined_nsw$days_since_origin
  
  zero_day <- (71.68090725) / 0.01679844  
  
  # Extend the range of days 
  extended_days <- seq(min(Temporal_Years_combined_nsw$days_since_origin), zero_day + 4000, by = 1)  
  
  # Calculate predicted values for extended
  extended_predictions <- 71.68090725 + 0.01679844   * extended_days
  
  # Calculate extended y-axis limits
  y_min <- 0
  y_max <- max(c(Temporal_Years_combined_nsw$AVGRRP_new, extended_predictions)) + 300
  
  
  # Define x-axis limits by adding more space
  xlim_min <- min(extended_days) 
  xlim_max <- max(extended_days) + 100 
  
  # Convert days_from_origin to actual dates for the x-axis
  dates_since_origin <- origin_date + Temporal_Years_combined_nsw$days_since_origin
  extended_dates <- origin_date + extended_days
  
  Extend <- max(extended_dates) + 100
  min <- min(dates_since_origin)
  
  # Plot actual data (actual CO2 generation) in blue
  plot(dates_since_origin, Temporal_Years_combined_nsw$AVGRRP, type = "l", col = "blue", lwd = 2, 
       xlab = "Date", ylab = "Daily RRP", main = "Spot Price (Daily RRP) projections in New South Wales",
       xlim = c(min, Extend), ylim = c(y_min, y_max)) 
  
  
  # Add the predicted line in red for both the original data and the extended prediction
  lines(dates_since_origin, Temporal_Years_combined_nsw$AVGRRP_new, col = "red", lwd = 3)
  lines(extended_dates, extended_predictions, col = "red", lwd = 3, lty = 2)  
  
  points(dates_since_origin, Temporal_Years_combined_nsw$AVGRRP_new, col = "red", pch = 16)
  
  # Custom axis labels every 6 months
  axis_dates <- seq(from = min, to = Extend, by = "6 months")
  axis(1, at = axis_dates, labels = format(axis_dates, "%b %Y"), las = 2)
  
  # X axis with dates instead of Days from Origin`
  axis(1, at = seq(min(extended_dates), max(extended_dates), by = "6 months"), 
       labels = format(seq(min(extended_dates), max(extended_dates), by = "6 months"), "%b %Y"), 
       las = 2)  # las = 2 for vertical labels
  
  # Add a legend to the plot
  legend("topright", legend = c("Actual", "Predicted (Current)", "Predicted (Extended)"), 
         col = c("blue", "red", "red"), lwd = 2, lty = c(1, 1, 2), pch = c(NA, 16, NA))
  

  
  
  # Using Predictive Modelling for Temporal_Years_combined_qld 
  
  
  # Summary statistics (qld)
  
  summary(Temporal_Years_combined_qld) 
  
  
  Temporal_Years_combined_qld <- na.omit(Temporal_Years_combined_qld)
  
  set.seed(123)  # to ensure predictive model can be reproduced
  
  # Split the data into 80% training and 20% testing
  train_index <- createDataPartition(Temporal_Years_combined_qld$AVGRRP, p = 0.8, list = FALSE)
  train_data <- Temporal_Years_combined_qld[train_index, ]
  test_data <- Temporal_Years_combined_qld[-train_index, ]
  
  # Train model_qld
  Temporal_Years_combined_qld_model <- train(
    AVGRRP ~ days_since_origin,
    data = train_data,
    method = "lm",  
    trControl = trainControl(method = "cv", number = 5)  
  )
  
  # View model_qld summary
  print(Temporal_Years_combined_qld_model)
  
  # Make the predictions
  predictions <- predict(Temporal_Years_combined_qld_model, newdata = test_data)
  
  # Calculate the results
  results <- data.frame(
    Actual = test_data$AVGRRP,
    Predicted = predictions
  )
  
  # Calculate the RMSE and R-squared with the results generated
  rmse <- sqrt(mean((results$Actual - results$Predicted)^2))
  
  print(paste("RMSE:", rmse))
  
  # Plot Temporal_Years_combined_qld_model for actual vs predicted
  plot(test_data$AVGRRP, predictions, main = "Spot Price (Daily RRP) projections in  Queensland",
       xlab = "Years", ylab = "Daily RRP")
  abline(0, 1, col = "red")  # Add diagonal line for reference
  
  
  
  
  # Extract model_qld for qld coefficients
  coefficients <- coef(Temporal_Years_combined_qld_model$finalModel)
  print(coefficients)
  
  
  # Extract coefficients from qld model_qld
  coefs <- coef(Temporal_Years_combined_qld_model$finalmodelModel)
  intercept <- coefs[1]  
  SETTLEMENT_coef <- coefs["SETTLEMENTDATE"]
  
  
  # Create predicted value from model_qld
  Temporal_Years_combined_qld$AVGRRP_new <- 59.6167404 + 0.0245387  * Temporal_Years_combined_qld$days_since_origin
  
  zero_day <- (59.6167404) / 0.0245387 
  
  # Extend the range of days 
  extended_days <- seq(min(Temporal_Years_combined_qld$days_since_origin), zero_day + 5000, by = 1)  
  
  # Calculate predicted values for extended
  extended_predictions <- 59.6167404 + 0.0245387  * extended_days
  
  # Calculate extended y-axis limits
  y_min <- 0
  y_max <- max(c(Temporal_Years_combined_qld$AVGRRP_new, extended_predictions)) + 300
  

  # Define x-axis limits by adding more space
  xlim_min <- min(extended_days) 
  xlim_max <- max(extended_days) + 100 
  
  # Convert days_from_origin to actual dates for the x-axis
  dates_since_origin <- origin_date + Temporal_Years_combined_qld$days_since_origin
  extended_dates <- origin_date + extended_days
  
  
  Extend <- max(extended_dates) + 100
  min <- min(dates_since_origin)
  
  # Plot actual data (actual CO2 generation) in blue
  plot(dates_since_origin, Temporal_Years_combined_qld$AVGRRP, type = "l", col = "blue", lwd = 2, 
       xlab = "Date", ylab = "Daily RRP", main = "Spot Price (Daily RRP) projections in Queensland",
       xlim = c(min, Extend), ylim = c(y_min, y_max)) 
  
  
  # Add the predicted line in red for both the original data and the extended prediction
  lines(dates_since_origin, Temporal_Years_combined_qld$AVGRRP_new, col = "red", lwd = 3)
  lines(extended_dates, extended_predictions, col = "red", lwd = 3, lty = 2)  
  
  points(dates_since_origin, Temporal_Years_combined_qld$AVGRRP_new, col = "red", pch = 16)
  
  # Custom axis labels every 6 months
axis_dates <- seq(from = min, to = Extend, by = "6 months")
axis(1, at = axis_dates, labels = format(axis_dates, "%b %Y"), las = 2)
  
  # X axis with dates instead of Days from Origin`
  axis(1, at = seq(min(extended_dates), max(extended_dates), by = "6 months"), 
       labels = format(seq(min(extended_dates), max(extended_dates), by = "6 months"), "%b %Y"), 
       las = 2)  # las = 2 for vertical labels
  
  # Add a legend to the plot
  legend("topright", legend = c("Actual", "Predicted (Current)", "Predicted (Extended)"), 
         col = c("blue", "red", "red"), lwd = 2, lty = c(1, 1, 2), pch = c(NA, 16, NA))
  
  
  
  # Using Predictive Modelling for Temporal_Years_combined_sa 
  
  
  # Summary statistics (sa)
  
  summary(Temporal_Years_combined_sa) 
  
  
  Temporal_Years_combined_sa <- na.omit(Temporal_Years_combined_sa)
  
  set.seed(123)  # to ensure predictive model can be reproduced
  
  # Split the data into 80% training and 20% testing
  train_index <- createDataPartition(Temporal_Years_combined_sa$AVGRRP, p = 0.8, list = FALSE)
  train_data <- Temporal_Years_combined_sa[train_index, ]
  test_data <- Temporal_Years_combined_sa[-train_index, ]
  
  # Train model_sa
  Temporal_Years_combined_sa_model <- train(
    AVGRRP ~ days_since_origin,
    data = train_data,
    method = "lm",  
    trControl = trainControl(method = "cv", number = 5)  
  )
  
  # View model_sa summary
  print(Temporal_Years_combined_sa_model)
  
  # Make the predictions
  predictions <- predict(Temporal_Years_combined_sa_model, newdata = test_data)
  
  # Calculate the results
  results <- data.frame(
    Actual = test_data$AVGRRP,
    Predicted = predictions
  )
  
  # Calculate the RMSE and R-squared with the results generated
  rmse <- sqrt(mean((results$Actual - results$Predicted)^2))
  
  print(paste("RMSE:", rmse))
  
  # Plot Temporal_Years_combined_sa_model for actual vs predicted
  plot(test_data$AVGRRP, predictions, main = "Spot Price (Daily RRP) projections in  South Australia",
       xlab = "Years", ylab = "Daily RRP")
  abline(0, 1, col = "red")  # Add diagonal line for reference
  
  
  
  
  # Extract model_sa for sa coefficients
  coefficients <- coef(Temporal_Years_combined_sa_model$finalModel)
  print(coefficients)
  
  
  # Extract coefficients from sa model_sa
  coefs <- coef(Temporal_Years_combined_sa_model$finalmodelModel)
  intercept <- coefs[1]  
  SETTLEMENT_coef <- coefs["SETTLEMENTDATE"]
  
  
  # Create predicted value from model_sa
  Temporal_Years_combined_sa$AVGRRP_new <- 85.233312801 + 0.001159241   * Temporal_Years_combined_sa$days_since_origin
  
  zero_day <- (85.233312801) / 0.001159241  
  
  # Extend the range of days 
  extended_days <- seq(min(Temporal_Years_combined_sa$days_since_origin), zero_day + 2, by = 1)  
  
  # Calculate predicted values for extended
  extended_predictions <- 85.233312801 + 0.001159241   * extended_days
  
  # Calculate extended y-axis limits
  y_min <- 0
  y_max <- max(c(Temporal_Years_combined_sa$AVGRRP_new, extended_predictions)) + 300
  
  
  # Define x-axis limits by adding more space
  xlim_min <- min(extended_days) 
  xlim_max <- max(extended_days) + 100 
  
  # Convert days_from_origin to actual dates for the x-axis
  dates_since_origin <- origin_date + Temporal_Years_combined_sa$days_since_origin
  extended_dates <- origin_date + extended_days
  
  
  min <- min(dates_since_origin)
  
  # Plot actual data (actual CO2 generation) in blue
  plot(dates_since_origin, Temporal_Years_combined_sa$AVGRRP, type = "l", col = "blue", lwd = 2, 
       xlab = "Date", ylab = "Daily RRP", main = "Spot Price (Daily RRP) projections in South Australia",
       xlim = c(min, 25000), ylim = c(y_min, y_max)) 
  
  
  # Add the predicted line in red for both the original data and the extended prediction
  lines(dates_since_origin, Temporal_Years_combined_sa$AVGRRP_new, col = "red", lwd = 3)
  lines(extended_dates, extended_predictions, col = "red", lwd = 3, lty = 2)  
  
  points(dates_since_origin, Temporal_Years_combined_sa$AVGRRP_new, col = "red", pch = 16)
  
  # Custom axis labels every 6 months
  axis_dates <- seq(from = min, to = Extend, by = "6 months")
  axis(1, at = axis_dates, labels = format(axis_dates, "%b %Y"), las = 2)
  
  # X axis with dates instead of Days from Origin`
  axis(1, at = seq(min(extended_dates), max(extended_dates), by = "6 months"), 
       labels = format(seq(min(extended_dates), max(extended_dates), by = "6 months"), "%b %Y"), 
       las = 2)  # las = 2 for vertical labels
  
  # Add a legend to the plot
  legend("topright", legend = c("Actual", "Predicted (Current)", "Predicted (Extended)"), 
         col = c("blue", "red", "red"), lwd = 2, lty = c(1, 1, 2), pch = c(NA, 16, NA))
  
  
  
  # Using Predictive Modelling for Temporal_Years_combined_tas 
  
  
  # Summary statistics (tas)
  
  summary(Temporal_Years_combined_tas) 
  
  
  Temporal_Years_combined_tas <- na.omit(Temporal_Years_combined_tas)
  
  set.seed(123)  # to ensure predictive model can be reproduced
  
  # Split the data into 80% training and 20% testing
  train_index <- createDataPartition(Temporal_Years_combined_tas$AVGRRP, p = 0.8, list = FALSE)
  train_data <- Temporal_Years_combined_tas[train_index, ]
  test_data <- Temporal_Years_combined_tas[-train_index, ]
  
  # Train model_tas
  Temporal_Years_combined_tas_model <- train(
    AVGRRP ~ days_since_origin,
    data = train_data,
    method = "lm",  
    trControl = trainControl(method = "cv", number = 5)  
  )
  
  # View model_tas summary
  print(Temporal_Years_combined_tas_model)
  
  # Make the predictions
  predictions <- predict(Temporal_Years_combined_tas_model, newdata = test_data)
  
  # Calculate the results
  results <- data.frame(
    Actual = test_data$AVGRRP,
    Predicted = predictions
  )
  
  # Calculate the RMSE and R-squared with the results generated
  rmse <- sqrt(mean((results$Actual - results$Predicted)^2))
  
  print(paste("RMSE:", rmse))
  
  # Plot Temporal_Years_combined_tas_model for actual vs predicted
  plot(test_data$AVGRRP, predictions, main = "Spot Price (Daily RRP) projections in  Tasmania",
       xlab = "Years", ylab = "Daily RRP")
  abline(0, 1, col = "red")  # Add diagonal line for reference
  
  
  
  
  # Extract model_tas for tas coefficients
  coefficients <- coef(Temporal_Years_combined_tas_model$finalModel)
  print(coefficients)
  
  
  # Extract coefficients from tas model_tas
  coefs <- coef(Temporal_Years_combined_tas_model$finalmodelModel)
  intercept <- coefs[1]  
  SETTLEMENT_coef <- coefs["SETTLEMENTDATE"]
  
  
  # Create predicted value from model_tas
  Temporal_Years_combined_tas$AVGRRP_new <- 72.826273326  + 0.001714152   * Temporal_Years_combined_tas$days_since_origin
  
  zero_day <- (72.826273326 ) / 0.001714152 
  
  # Extend the range of days 
  extended_days <- seq(min(Temporal_Years_combined_tas$days_since_origin), zero_day + 20, by = 1)  
  
  # Calculate predicted values for extended
  extended_predictions <- 72.826273326  + 0.001714152   * extended_days
  
  # Calculate extended y-axis limits
  y_min <- 0
  y_max <- max(c(Temporal_Years_combined_tas$AVGRRP_new, extended_predictions)) + 300
  
  
  # Define x-axis limits by adding more space
  xlim_min <- min(extended_days) 
  xlim_max <- max(extended_days) + 100 
  
  # Convert days_from_origin to actual dates for the x-axis
  dates_since_origin <- origin_date + Temporal_Years_combined_tas$days_since_origin
  extended_dates <- origin_date + extended_days
  
  
  min <- min(dates_since_origin)
  
  # Plot actual data (actual CO2 generation) in blue
  plot(dates_since_origin, Temporal_Years_combined_tas$AVGRRP, type = "l", col = "blue", lwd = 2, 
       xlab = "Date", ylab = "Daily RRP", main = "Spot Price (Daily RRP) projections in Tasmania",
       xlim = c(min, 25000), ylim = c(y_min, y_max)) 
  
  
  # Add the predicted line in red for both the original data and the extended prediction
  lines(dates_since_origin, Temporal_Years_combined_tas$AVGRRP_new, col = "red", lwd = 3)
  lines(extended_dates, extended_predictions, col = "red", lwd = 3, lty = 2)  
  
  points(dates_since_origin, Temporal_Years_combined_tas$AVGRRP_new, col = "red", pch = 16)
  
  # Custom axis labels every 6 months
  axis_dates <- seq(from = min, to = Extend, by = "6 months")
  axis(1, at = axis_dates, labels = format(axis_dates, "%b %Y"), las = 2)
  
  # X axis with dates instead of Days from Origin`
  axis(1, at = seq(min(extended_dates), max(extended_dates), by = "6 months"), 
       labels = format(seq(min(extended_dates), max(extended_dates), by = "6 months"), "%b %Y"), 
       las = 2)  # las = 2 for vertical labels
  
  # Add a legend to the plot
  legend("topright", legend = c("Actual", "Predicted (Current)", "Predicted (Extended)"), 
         col = c("blue", "red", "red"), lwd = 2, lty = c(1, 1, 2), pch = c(NA, 16, NA))
  
  
  
  # Using Predictive Modelling for Temporal_Years_combined_vic 
  
  
  # Summary statistics (vic)
  
  summary(Temporal_Years_combined_vic) 
  
  
  Temporal_Years_combined_vic <- na.omit(Temporal_Years_combined_vic)
  
  set.seed(123)  # to ensure predictive model can be reproduced
  
  # Split the data into 80% training and 20% testing
  train_index <- createDataPartition(Temporal_Years_combined_vic$AVGRRP, p = 0.8, list = FALSE)
  train_data <- Temporal_Years_combined_vic[train_index, ]
  test_data <- Temporal_Years_combined_vic[-train_index, ]
  
  # Train model_vic
  Temporal_Years_combined_vic_model <- train(
    AVGRRP ~ days_since_origin,
    data = train_data,
    method = "lm",  
    trControl = trainControl(method = "cv", number = 5)  
  )
  
  # View model_vic summary
  print(Temporal_Years_combined_vic_model)
  
  # Make the predictions
  predictions <- predict(Temporal_Years_combined_vic_model, newdata = test_data)
  
  # Calculate the results
  results <- data.frame(
    Actual = test_data$AVGRRP,
    Predicted = predictions
  )
  
  # Calculate the RMSE and R-squared with the results generated
  rmse <- sqrt(mean((results$Actual - results$Predicted)^2))
  
  print(paste("RMSE:", rmse))
  
  # Plot Temporal_Years_combined_vic_model for actual vs predicted
  plot(test_data$AVGRRP, predictions, main = "Spot Price (Daily RRP) projections in  Victoria",
       xlab = "Years", ylab = "Daily RRP")
  abline(0, 1, col = "red")  # Add diagonal line for reference
  
  
  
  
  # Extract model_vic for vic coefficients
  coefficients <- coef(Temporal_Years_combined_vic_model$finalModel)
  print(coefficients)
  
  
  # Extract coefficients from vic model_vic
  coefs <- coef(Temporal_Years_combined_vic_model$finalmodelModel)
  intercept <- coefs[1]  
  SETTLEMENT_coef <- coefs["SETTLEMENTDATE"]
  
  
  # Create predicted value from model_vic
  Temporal_Years_combined_vic$AVGRRP_new <- 97.53375225 - 0.01408622  * Temporal_Years_combined_vic$days_since_origin
  
  zero_day <- (97.53375225) / 0.01408622 
  
  # Extend the range of days 
  extended_days <- seq(min(Temporal_Years_combined_vic$days_since_origin), zero_day + 5500, by = 1)  
  
  # Calculate predicted values for extended
  extended_predictions <- 97.53375225 - 0.01408622  * extended_days
  
  # Calculate extended y-axis limits
  y_min <- 0
  y_max <- max(c(Temporal_Years_combined_vic$AVGRRP_new, extended_predictions)) + 300
  
  
  # Define x-axis limits by adding more space
  xlim_min <- min(extended_days) 
  xlim_max <- max(extended_days) + 100 
  
  # Convert days_from_origin to actual dates for the x-axis
  dates_since_origin <- origin_date + Temporal_Years_combined_vic$days_since_origin
  extended_dates <- origin_date + extended_days
  
  
  min <- min(dates_since_origin)
  
  # Plot actual data (actual CO2 generation) in blue
  plot(dates_since_origin, Temporal_Years_combined_vic$AVGRRP, type = "l", col = "blue", lwd = 2, 
       xlab = "Date", ylab = "Daily RRP", main = "Spot Price (Daily RRP) projections in Victoria",
       xlim = c(min, 25000), ylim = c(y_min, y_max)) 
  
  
  # Add the predicted line in red for both the original data and the extended prediction
  lines(dates_since_origin, Temporal_Years_combined_vic$AVGRRP_new, col = "red", lwd = 3)
  lines(extended_dates, extended_predictions, col = "red", lwd = 3, lty = 2)  
  
  points(dates_since_origin, Temporal_Years_combined_vic$AVGRRP_new, col = "red", pch = 16)
  
  # Custom axis labels every 6 months
  axis_dates <- seq(from = min, to = Extend, by = "6 months")
  axis(1, at = axis_dates, labels = format(axis_dates, "%b %Y"), las = 2)
  
  # X axis with dates instead of Days from Origin`
  axis(1, at = seq(min(extended_dates), max(extended_dates), by = "6 months"), 
       labels = format(seq(min(extended_dates), max(extended_dates), by = "6 months"), "%b %Y"), 
       las = 2)  # las = 2 for vertical labels
  
  # Add a legend to the plot
  legend("topright", legend = c("Actual", "Predicted (Current)", "Predicted (Extended)"), 
         col = c("blue", "red", "red"), lwd = 2, lty = c(1, 1, 2), pch = c(NA, 16, NA))
  
  
  
  
  
  
  
  
  
  
  





#Load dataframe
data <- read.csv("NEM_RENEWABLE_PENETRATION_ALL_202503231117.csv") 
head(data)
str(data)

#Rename the data frame to an appropriate name
Renewable_Penetration_df <-data

colnames(Renewable_Penetration_df)


#Select on the Max in the Min.Max column for the Renewable_Penetration_df

Renewable_Penetration_filtered <- Renewable_Penetration_df[Renewable_Penetration_df$Max.Min == 'Max', ]
print(Renewable_Penetration_filtered)



# Use strptime() with time, and its format being "%d/%m/%Y %H:%M"
Renewable_Penetration_filtered$DateTime <- strptime(Renewable_Penetration_filtered$DateTime, format = "%d/%m/%Y %H:%M")

# Create a new column called "Year" for the df
Renewable_Penetration_filtered$Year <- format(Renewable_Penetration_filtered$DateTime, "%Y")

# Don't select 2025 as its incomplete for other years
Renewable_Penetration_filtered <- Renewable_Penetration_filtered %>% filter(Year != 2025)

# View the dateframe to see the changes with Year
print(Renewable_Penetration_filtered)



# Sum the Supply and group variables of choice
Sumarised_Renewable_Penetration <- Renewable_Penetration_filtered %>%
  group_by(Max.Min, State, Fuel.Type,Supply,Year) %>%
  summarise(Sum_Supply = sum(Supply))

# View the dataframe to see the updates
print(Sumarised_Renewable_Penetration)


fuel_types <- unique(Sumarised_Renewable_Penetration$Fuel.Type)

# Choose custom colours that are easily categorised and viewable for the ggplot
custom_colors <- c(
  "Battery" = "red",
  "Biomass" = "lavender",
  "Black coal" = "black",  
  "Brown coal"  = "brown",   
  "Distributed PV" = "orange",
  "Gas" = "grey",   
  "Hydro" = "blue", 
  "Liquid Fuel"  = "purple",
  "Other" = "pink", 
  "Utility-scale Solar" = "yellow", 
  "Wind" = "skyblue" 
) 

#Order from highest nonrenewable to least nonrenewable, then highest renewable to lowest renewable to highlight the significance of 
Order <- c("Black coal", "Brown coal", "Gas", "Liquid Fuel", "Other", "Distributed PV", "Utility-scale Solar", "Wind",  "Hydro", "Biomass", "Battery")


Sumarised_Renewable_Penetration$Fuel.Type <- factor(Sumarised_Renewable_Penetration$Fuel.Type, levels = Order)


#ggplot created with Sumarised_Renewable_Penetration for Maximum Renewable Penetration by Year and Fuel Type in Australia (NEM)

ggplot(Sumarised_Renewable_Penetration, aes(x = Year, y = Sum_Supply, fill = Fuel.Type)) +
  geom_bar(stat = "identity", position = "dodge") +  # Use fill from Fuel.Type
  labs(title = "Maximum Supply of generated Electricity by Year and Fuel Type in Australia (NEM)",
       x = "Year",
       y = "Total Generation (MWh)",
       fill = "Fuel Type") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) + 
  scale_fill_manual(values = custom_colors) +  # Assign manual colors
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )



















# Categorise Energy sources into their respective fuel types
Renewable_Penetration_filtered_grouping <- Renewable_Penetration_filtered %>%
  mutate(FuelCategory = case_when(
    Fuel.Type %in% c('Battery', 'Biomass', 'Distributed PV', 'Hydro', 'Utility-scale Solar', 'Wind') ~ 'Renewable',
    Fuel.Type %in% c('Black coal', 'Brown coal', 'Gas', 'Liquid Fuel') ~ 'Non-Renewable',
    TRUE ~ 'Other'
  ))

#view df to verify the updates for fuel type are correct
print(Renewable_Penetration_filtered_grouping)



# Sum the supply and grouped by year and fuel type for the data frame
Sumarised_Renewable_Penetration_grouping <- Renewable_Penetration_filtered_grouping %>%
  group_by(Year, FuelCategory) %>%
  summarise(Sum_Supply = sum(Supply))

# View df to check the updates for the the Sum(supply)
print(Sumarised_Renewable_Penetration_grouping)





# #ggplot created with Sumarised_Renewable_Penetration for Maximum Renewable Penetration by Year and Fuel Category in Australia (NEM)
ggplot(Sumarised_Renewable_Penetration_grouping, aes(x = Year, y = Sum_Supply, fill = FuelCategory)) +
  geom_bar(stat = "identity", position = "dodge") +  # Create a bar plot
  labs(title = "Maxmimum Supply of generated Electricity by Year and Fuel Category in Australia (NEM)",
       x = "Year",
       y = "Total Generation (MWh)",
       fill = "Fuel Type") +
  theme_minimal() +
  scale_fill_manual(values = c("Renewable" = "green", 
                               "Non-Renewable" = "brown", 
                               "Other" = "yellow")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        axis.title = element_text(size = 12),  
        legend.title = element_text(size = 12),  
        legend.text = element_text(size = 10),  
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) 


#Alternative, to including percentage to make implications with percentage for better interpretation


Sumarised_Renewable_Penetration_grouping_percent <- Sumarised_Renewable_Penetration_grouping %>%
  group_by(Year) %>%
  mutate(
    percent = Sum_Supply / sum(Sum_Supply) * 100,
    label = paste0(round(percent, 1), "%")
  )

ggplot(Sumarised_Renewable_Penetration_grouping_percent, aes(x = factor(Year), y = Sum_Supply, fill = FuelCategory)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 3.5) +
  labs(title = "Maxmimum Supply by Year and Fuel Category in Australia (NEM) with percentages", x = "Year", y = "Sum_Supply", fill = "FuelCategory") +
  scale_x_discrete(labels = levels(factor(Sumarised_Renewable_Penetration_grouping_percent$Year))) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  theme_minimal()







#Load data, delete first row (as it doesn't provide relevant data), rename columns that make sense for analysis
Scada_data1 <- read.csv("PUBLIC_DVD_DISPATCH_UNIT_SCADA_201801010000.CSV") 
Scada_data1 <- Scada_data1[, colSums(is.na(Scada_data1)) < nrow(Scada_data1)]
Scada_data1 <- Scada_data1[-1, ]  #Delete first row of df
colnames(Scada_data1)[6] <- "DUID"
colnames(Scada_data1)[7] <- "SCADAVALUE"

#Load data, delete first row (as it doesn't provide relevant data), rename columns that make sense for analysis
Scada_data2 <- read.csv("PUBLIC_DVD_DISPATCH_UNIT_SCADA_201802010000.CSV") 
Scada_data2 <- Scada_data2[, colSums(is.na(Scada_data2)) < nrow(Scada_data2)]
Scada_data2 <- Scada_data2[-1, ]  #Delete first row of df
colnames(Scada_data2)[6] <- "DUID"
colnames(Scada_data2)[7] <- "SCADAVALUE"

#Load data, delete first row (as it doesn't provide relevant data), rename columns that make sense for analysis
Scada_data3 <- read.csv("PUBLIC_DVD_DISPATCH_UNIT_SCADA_201803010000.CSV") 
Scada_data3 <- Scada_data3[, colSums(is.na(Scada_data3)) < nrow(Scada_data3)]
Scada_data3 <- Scada_data3[-1, ]  #Delete first row of df
colnames(Scada_data3)[6] <- "DUID"
colnames(Scada_data3)[7] <- "SCADAVALUE"

#Load data, delete first row (as it doesn't provide relevant data), rename columns that make sense for analysis
Scada_data4 <- read.csv("PUBLIC_DVD_DISPATCH_UNIT_SCADA_201804010000.CSV") 
Scada_data4 <- Scada_data4[, colSums(is.na(Scada_data4)) < nrow(Scada_data4)]
Scada_data4 <- Scada_data4[-1, ]  #Delete first row of df
colnames(Scada_data4)[6] <- "DUID"
colnames(Scada_data4)[7] <- "SCADAVALUE"

#Load data, delete first row (as it doesn't provide relevant data), rename columns that make sense for analysis
Scada_data5 <- read.csv("PUBLIC_DVD_DISPATCH_UNIT_SCADA_201805010000.CSV") 
Scada_data5 <- Scada_data5[, colSums(is.na(Scada_data5)) < nrow(Scada_data5)]
Scada_data5 <- Scada_data5[-1, ]  #Delete first row of df
colnames(Scada_data5)[6] <- "DUID"
colnames(Scada_data5)[7] <- "SCADAVALUE"

#Load data, delete first row (as it doesn't provide relevant data), rename columns that make sense for analysis
Scada_data6 <- read.csv("PUBLIC_DVD_DISPATCH_UNIT_SCADA_201806010000.CSV") 
Scada_data6 <- Scada_data6[, colSums(is.na(Scada_data6)) < nrow(Scada_data6)]
Scada_data6 <- Scada_data6[-1, ]  #Delete first row of df
colnames(Scada_data6)[6] <- "DUID"
colnames(Scada_data6)[7] <- "SCADAVALUE"

#Load data, delete first row (as it doesn't provide relevant data), rename columns that make sense for analysis
Scada_data7 <- read.csv("PUBLIC_DVD_DISPATCH_UNIT_SCADA_201807010000.CSV") 
Scada_data7 <- Scada_data7[, colSums(is.na(Scada_data7)) < nrow(Scada_data7)]
Scada_data7 <- Scada_data7[-1, ]  #Delete first row of df
colnames(Scada_data7)[6] <- "DUID"
colnames(Scada_data7)[7] <- "SCADAVALUE"

#Load data, delete first row (as it doesn't provide relevant data), rename columns that make sense for analysis
Scada_data8 <- read.csv("PUBLIC_DVD_DISPATCH_UNIT_SCADA_201808010000.CSV") 
Scada_data8 <- Scada_data8[, colSums(is.na(Scada_data8)) < nrow(Scada_data8)]
Scada_data8 <- Scada_data8[-1, ]  #Delete first row of df
colnames(Scada_data8)[6] <- "DUID"
colnames(Scada_data8)[7] <- "SCADAVALUE"

#Load data, delete first row (as it doesn't provide relevant data), rename columns that make sense for analysis
Scada_data9 <- read.csv("PUBLIC_DVD_DISPATCH_UNIT_SCADA_201809010000.CSV") 
Scada_data9 <- Scada_data9[, colSums(is.na(Scada_data9)) < nrow(Scada_data9)]
Scada_data9 <- Scada_data9[-1, ]  #Delete first row of df
colnames(Scada_data9)[6] <- "DUID"
colnames(Scada_data9)[7] <- "SCADAVALUE"


#Load data, delete first row (as it doesn't provide relevant data), rename columns that make sense for analysis
Scada_data10 <- read.csv("PUBLIC_DVD_DISPATCH_UNIT_SCADA_201810010000.CSV") 
Scada_data10 <- Scada_data10[, colSums(is.na(Scada_data10)) < nrow(Scada_data10)]
Scada_data10 <- Scada_data10[-1, ]  #Delete first row of df
colnames(Scada_data10)[6] <- "DUID"
colnames(Scada_data10)[7] <- "SCADAVALUE"

#Load data, delete first row (as it doesn't provide relevant data), rename columns that make sense for analysis
Scada_data11 <- read.csv("PUBLIC_DVD_DISPATCH_UNIT_SCADA_201811010000.CSV") 
Scada_data11 <- Scada_data11[, colSums(is.na(Scada_data11)) < nrow(Scada_data11)]
Scada_data11 <- Scada_data11[-1, ]  #Delete first row of df
colnames(Scada_data11)[6] <- "DUID"
colnames(Scada_data11)[7] <- "SCADAVALUE"

#Load data, delete first row (as it doesn't provide relevant data), rename columns that make sense for analysis
Scada_data12 <- read.csv("PUBLIC_DVD_DISPATCH_UNIT_SCADA_201812010000.CSV") 
Scada_data12 <- Scada_data12[, colSums(is.na(Scada_data12)) < nrow(Scada_data12)]
Scada_data12 <- Scada_data12[-1, ]  #Delete first row of df
colnames(Scada_data12)[6] <- "DUID"
colnames(Scada_data12)[7] <- "SCADAVALUE"

#Load data, delete first row (as it doesn't provide relevant data), rename columns that make sense for analysis
Scada_data13 <- read.csv("PUBLIC_DVD_DISPATCH_UNIT_SCADA_202401010000.CSV") 
Scada_data13 <- Scada_data13[, colSums(is.na(Scada_data13)) < nrow(Scada_data13)]
Scada_data13 <- Scada_data13[-1, ]  #Delete first row of df
colnames(Scada_data13)[6] <- "DUID"
colnames(Scada_data13)[7] <- "SCADAVALUE"

#Load data, delete first row (as it doesn't provide relevant data), rename columns that make sense for analysis
Scada_data14 <- read.csv("PUBLIC_DVD_DISPATCH_UNIT_SCADA_202402010000.CSV") 
Scada_data14 <- Scada_data14[, colSums(is.na(Scada_data14)) < nrow(Scada_data14)]
Scada_data14 <- Scada_data14[-1, ]  #Delete first row of df
colnames(Scada_data14)[6] <- "DUID"
colnames(Scada_data14)[7] <- "SCADAVALUE"

#Load data, delete first row (as it doesn't provide relevant data), rename columns that make sense for analysis
Scada_data15 <- read.csv("PUBLIC_DVD_DISPATCH_UNIT_SCADA_202403010000.CSV") 
Scada_data15 <- Scada_data15[, colSums(is.na(Scada_data15)) < nrow(Scada_data15)]
Scada_data15 <- Scada_data15[-1, ]  #Delete first row of df
colnames(Scada_data15)[6] <- "DUID"
colnames(Scada_data15)[7] <- "SCADAVALUE"

#Load data, delete first row (as it doesn't provide relevant data), rename columns that make sense for analysis
Scada_data16 <- read.csv("PUBLIC_DVD_DISPATCH_UNIT_SCADA_202404010000.CSV") 
Scada_data16 <- Scada_data16[, colSums(is.na(Scada_data16)) < nrow(Scada_data16)]
Scada_data16 <- Scada_data16[-1, ]  #Delete first row of df
colnames(Scada_data16)[6] <- "DUID"
colnames(Scada_data16)[7] <- "SCADAVALUE"

#Load data, delete first row (as it doesn't provide relevant data), rename columns that make sense for analysis
Scada_data17 <- read.csv("PUBLIC_DVD_DISPATCH_UNIT_SCADA_202405010000.CSV") 
Scada_data17 <- Scada_data17[, colSums(is.na(Scada_data17)) < nrow(Scada_data17)]
Scada_data17 <- Scada_data17[-1, ]  #Delete first row of df
colnames(Scada_data17)[6] <- "DUID"
colnames(Scada_data17)[7] <- "SCADAVALUE"

#Load data, delete first row (as it doesn't provide relevant data), rename columns that make sense for analysis
Scada_data18 <- read.csv("PUBLIC_DVD_DISPATCH_UNIT_SCADA_202406010000.CSV") 
Scada_data18 <- Scada_data18[, colSums(is.na(Scada_data18)) < nrow(Scada_data18)]
Scada_data18 <- Scada_data18[-1, ]  #Delete first row of df
colnames(Scada_data18)[6] <- "DUID"
colnames(Scada_data18)[7] <- "SCADAVALUE"

#Load data, delete first row (as it doesn't provide relevant data), rename columns that make sense for analysis
Scada_data19 <- read.csv("PUBLIC_DVD_DISPATCH_UNIT_SCADA_202407010000.CSV") 
Scada_data19 <- Scada_data19[, colSums(is.na(Scada_data19)) < nrow(Scada_data19)]
Scada_data19 <- Scada_data19[-1, ]  #Delete first row of df
colnames(Scada_data19)[6] <- "DUID"
colnames(Scada_data19)[7] <- "SCADAVALUE"


#Load data, delete first row (as it doesn't provide relevant data), rename columns that make sense for analysis
Scada_data20 <- read.csv("PUBLIC_DVD_DISPATCH_UNIT_SCADA_202408010000.CSV") 
Scada_data20 <- Scada_data20[, colSums(is.na(Scada_data20)) < nrow(Scada_data20)]
Scada_data20 <- Scada_data20[-1, ]  #Delete first row of df
colnames(Scada_data20)[6] <- "DUID"
colnames(Scada_data20)[7] <- "SCADAVALUE"

#Load data, delete first row (as it doesn't provide relevant data), rename columns that make sense for analysis
Scada_data21 <- read.csv("PUBLIC_DVD_DISPATCH_UNIT_SCADA_202409010000.CSV") 
Scada_data21 <- Scada_data21[, colSums(is.na(Scada_data21)) < nrow(Scada_data21)]
Scada_data21 <- Scada_data21[-1, ]  #Delete first row of df
colnames(Scada_data21)[6] <- "DUID"
colnames(Scada_data21)[7] <- "SCADAVALUE"

#Load data, delete first row (as it doesn't provide relevant data), rename columns that make sense for analysis
Scada_data22 <- read.csv("PUBLIC_DVD_DISPATCH_UNIT_SCADA_202410010000.CSV") 
Scada_data22 <- Scada_data22[, colSums(is.na(Scada_data22)) < nrow(Scada_data22)]
Scada_data22 <- Scada_data22[-1, ]  #Delete first row of df
Scada_data22 <- Scada_data22[, -8]
colnames(Scada_data22)[6] <- "DUID"
colnames(Scada_data22)[7] <- "SCADAVALUE"

#Load data, delete first row (as it doesn't provide relevant data), rename columns that make sense for analysis
Scada_data23 <- read.csv("PUBLIC_DVD_DISPATCH_UNIT_SCADA_202411010000.CSV") 
Scada_data23 <- Scada_data23[, colSums(is.na(Scada_data23)) < nrow(Scada_data23)]
Scada_data23 <- Scada_data23[-1, ]  #Delete first row of df
Scada_data23 <- Scada_data23[, -8]
colnames(Scada_data23)[6] <- "DUID"
colnames(Scada_data23)[7] <- "SCADAVALUE"

#Load data, delete first row (as it doesn't provide relevant data), rename columns that make sense for analysis
Scada_data24 <- read.csv("PUBLIC_DVD_DISPATCH_UNIT_SCADA_202412010000.CSV") 
Scada_data24 <- Scada_data24[, colSums(is.na(Scada_data24)) < nrow(Scada_data24)]
Scada_data24 <- Scada_data24[-1, ]  #Delete first row of df
Scada_data24 <- Scada_data24[, -8]
colnames(Scada_data24)[6] <- "DUID"
colnames(Scada_data24)[7] <- "SCADAVALUE"


#Create two df based on years(2018 and 2024) separately and merged the relevant dataframes together

df_2018 = bind_rows(Scada_data1, Scada_data2, Scada_data3, Scada_data4, Scada_data5, Scada_data6, Scada_data7, Scada_data8, Scada_data9, Scada_data10, Scada_data11, Scada_data12)
df_2024 = bind_rows(Scada_data13, Scada_data14, Scada_data15, Scada_data16, Scada_data17, Scada_data18, Scada_data19, Scada_data20, Scada_data21, Scada_data22, Scada_data23, Scada_data24)





#Create a dataframe,with NEM Registration and Exemption List
data2 <- read.csv("NEM Registration and Exemption List.csv")
df2 <- data2
colnames(df2) # Check column names



# Perform left join named 'merged_df_2018' with two dataframes (df_2018, df2), selecting df2 relevant variables (7 of them) to merge together
merged_df_2018 <- df_2018 %>%
    left_join(df2[, c("DUID", "Region", "Dispatch.Type", "Fuel.Source...Primary", "Fuel.Source...Descriptor", "Technology.Type...Primary", "Reg.Cap.generation..MW.")], by = "DUID")

# Perform left join named 'merged_df_2024' with two dataframes (df_2024, df2), selecting df2 relevant variables (7 of them) to merge together
merged_df_2024 <- df_2024 %>%
  left_join(df2[, c("DUID", "Region", "Dispatch.Type", "Fuel.Source...Primary", "Fuel.Source...Descriptor", "Technology.Type...Primary", "Reg.Cap.generation..MW.")], by = "DUID")


# Change merged_df_2018 column name "PUBLIC" to "SETTLEMENTDATE" for consistency towards analysis
colnames(merged_df_2018)[colnames(merged_df_2018) == "PUBLIC"] <- "SETTLEMENTDATE"
print(merged_df_2018)

#Identify the unique values in Fuel Source
unique_values <- data.frame(unique_values = unique(merged_df_2018$Fuel.Source...Primary))
print(unique_values)

#Arrange dateframe of merged_df_2018 based on Region and SETTLEMENTDATE
merged_df_2018 <- merged_df_2018 %>%
  arrange(Region, SETTLEMENTDATE)

# Change merged_df_2018 column name "PUBLIC" to "SETTLEMENTDATE" for consistency towards analysis
colnames(merged_df_2024)[colnames(merged_df_2024) == "PUBLIC"] <- "SETTLEMENTDATE"
print(merged_df_2024)

#Arrange dateframe of merged_df_2024 based on Region and SETTLEMENTDATE
merged_df_2024 <- merged_df_2024 %>%
  arrange(Region, SETTLEMENTDATE)









# merged_df_2018, changing SCADAVALUE to numerical and any missing NA values to be as '0'
merged_df_2018 <- merged_df_2018 %>%
  mutate(SCADAVALUE = as.numeric(gsub("[^0-9.]", "", SCADAVALUE))) %>%
  mutate(SCADAVALUE = ifelse(is.na(SCADAVALUE), 0, SCADAVALUE))

# merged_df_2024, changing SCADAVALUE to numerical and any missing NA values to be as '0'
merged_df_2024 <- merged_df_2024 %>%
  mutate(SCADAVALUE = as.numeric(gsub("[^0-9.]", "", SCADAVALUE))) %>%
  mutate(SCADAVALUE = ifelse(is.na(SCADAVALUE), 0, SCADAVALUE))



# Converting Merged_df_2018 Settlmentdate (with POSIXct) and create a new column 'DayMonthYear'


merged_df_2018 <- merged_df_2018 %>%
  mutate(

    SETTLEMENTDATE = as.POSIXct(SETTLEMENTDATE, format = "%Y/%m/%d %H:%M:%S"),
    

    `DayMonthYear` = format(SETTLEMENTDATE, "%d %B %Y")
  )

colnames(merged_df_2018)

# Converting Merged_df_2024 Settlmentdate (with POSIXct) and create a new column 'DayMonthYear''


merged_df_2024 <- merged_df_2024 %>%
  mutate(

    SETTLEMENTDATE = as.POSIXct(SETTLEMENTDATE, format = "%Y/%m/%d %H:%M:%S"),
    

    `DayMonthYear` = format(SETTLEMENTDATE, "%d %B %Y")
  )

colnames(merged_df_2024)


# Count number of unique values in 'SETTLEMENTDATE' for merged_df_2018
value_counts_merged_2018 <- table(merged_df_2018$Fuel.Source...Primary)

# Print number of unique value for merged_df_2018
print(value_counts_merged_2018)

# Count number of unique values in 'SETTLEMENTDATE' for merged_df_2024
value_counts_merged_2024 <- table(merged_df_2024$Fuel.Source...Primary)

# Print number of unique value for merged_df_2024
print(value_counts_merged_2024)



# Group by Region and sum of SCADAVALUE with a new df named summarized_merged_df_2018_withtech
summarized_merged_df_2018_withtech <- merged_df_2018 %>%
  group_by(Region, Technology.Type...Primary, DayMonthYear) %>%
  summarize(Total_Generation = sum(SCADAVALUE, na.rm = TRUE),
  )
summarized_merged_df_2018_withtech <- na.omit(summarized_merged_df_2018_withtech)


# Using the 'Technology' column and pivot it into multiple columns (Renewable, Combustion, storage)
df_renewable_vs_combustion_vs_storage_2018 <- summarized_merged_df_2018_withtech %>%
  pivot_wider(names_from = Technology.Type...Primary, values_from = Total_Generation)

# Replace NA with 0 in three columns - renewable, nonrenewable and storage
df_renewable_vs_combustion_vs_storage_2018_complete <- df_renewable_vs_combustion_vs_storage_2018 %>%
  mutate(across(c(Renewable, Combustion), ~replace(., is.na(.), 0)))

df_renewable_vs_combustion_vs_storage_2018_complete$Renewable <- as.numeric(df_renewable_vs_combustion_vs_storage_2018_complete$Renewable)
df_renewable_vs_combustion_vs_storage_2018_complete$Combustion <- as.numeric(df_renewable_vs_combustion_vs_storage_2018_complete $Combustion)



#Group by Region and sum of SCADAVALUE with a new df named summarized_merged_df_2024_withtech
summarized_merged_df_2024_withtech <- merged_df_2024 %>%
  group_by(Region, Technology.Type...Primary, DayMonthYear) %>%
  summarize(Total_Generation = sum(SCADAVALUE, na.rm = TRUE),
  )
summarized_merged_df_2024_withtech <- na.omit(summarized_merged_df_2024_withtech)


# using the'Technology' column to pivot into multiple columns (Renewable, Combustion, storage)
df_renewable_vs_combustion_vs_storage_2024 <- summarized_merged_df_2024_withtech %>%
  pivot_wider(names_from = Technology.Type...Primary, values_from = Total_Generation)

# Replace NA with 0 in three columns - renewable, nonrenewable and storage
df_renewable_vs_combustion_vs_storage_2024_complete <- df_renewable_vs_combustion_vs_storage_2024 %>%
  mutate(across(c(Renewable, Combustion), ~replace(., is.na(.), 0)))
# Delete column named '-'
df_renewable_vs_combustion_vs_storage_2024_complete <- df_renewable_vs_combustion_vs_storage_2024_complete %>% select(-`-`)
df_renewable_vs_combustion_vs_storage_2024_complete <- df_renewable_vs_combustion_vs_storage_2024_complete %>% select(-`Storage`)

df_renewable_vs_combustion_vs_storage_2024_complete$Renewable <- as.numeric(df_renewable_vs_combustion_vs_storage_2024_complete$Renewable)
df_renewable_vs_combustion_vs_storage_2024_complete$Renewable <- as.numeric(df_renewable_vs_combustion_vs_storage_2024_complete$Renewable)
df_renewable_vs_combustion_vs_storage_2024_complete$Combustion <- as.numeric(df_renewable_vs_combustion_vs_storage_2024_complete$Combustion)





















#Loaded data for PV individually, and remove first column and first row as not relevant for analysis

PV_data1 <- read.csv("PUBLIC_DVD_ROOFTOP_PV_ACTUAL_201801010000.csv") 
colnames(PV_data1) <- as.character(PV_data1[1, ])
PV_data1 <- PV_data1[-1, ]

#Loaded data for PV individually, and remove first column and first row as not relevant for analysis
PV_data2 <- read.csv("PUBLIC_DVD_ROOFTOP_PV_ACTUAL_201802010000.csv")
colnames(PV_data2) <- as.character(PV_data2[1, ])
PV_data2 <- PV_data2[-1, ]

#Loaded data for PV individually, and remove first column and first row as not relevant for analysis
PV_data3 <- read.csv("PUBLIC_DVD_ROOFTOP_PV_ACTUAL_201803010000.csv")
colnames(PV_data3) <- as.character(PV_data3[1, ])
PV_data3 <- PV_data3[-1, ]

#Loaded data for PV individually, and remove first column and first row as not relevant for analysis
PV_data4 <- read.csv("PUBLIC_DVD_ROOFTOP_PV_ACTUAL_201804010000.csv")
colnames(PV_data4) <- as.character(PV_data4[1, ])
PV_data4 <- PV_data4[-1, ]

#Loaded data for PV individually, and remove first column and first row as not relevant for analysis
PV_data5 <- read.csv("PUBLIC_DVD_ROOFTOP_PV_ACTUAL_201805010000.csv")
colnames(PV_data5) <- as.character(PV_data5[1, ])
PV_data5 <- PV_data5[-1, ]

#Loaded data for PV individually, and remove first column and first row as not relevant for analysis
PV_data6 <- read.csv("PUBLIC_DVD_ROOFTOP_PV_ACTUAL_201806010000.csv")
colnames(PV_data6) <- as.character(PV_data6[1, ])
PV_data6 <- PV_data6[-1, ]

#Loaded data for PV individually, and remove first column and first row as not relevant for analysis
PV_data7 <- read.csv("PUBLIC_DVD_ROOFTOP_PV_ACTUAL_201807010000.csv")
colnames(PV_data7) <- as.character(PV_data7[1, ])
PV_data7 <- PV_data7[-1, ]

#Loaded data for PV individually, and remove first column and first row as not relevant for analysis
PV_data8 <- read.csv("PUBLIC_DVD_ROOFTOP_PV_ACTUAL_201808010000.csv")
colnames(PV_data8) <- as.character(PV_data8[1, ])
PV_data8 <- PV_data8[-1, ]

#Loaded data for PV individually, and remove first column and first row as not relevant for analysis
PV_data9 <- read.csv("PUBLIC_DVD_ROOFTOP_PV_ACTUAL_201809010000.csv")
colnames(PV_data9) <- as.character(PV_data9[1, ])
PV_data9 <- PV_data9[-1, ]

#Loaded data for PV individually, and remove first column and first row as not relevant for analysis
PV_data10 <- read.csv("PUBLIC_DVD_ROOFTOP_PV_ACTUAL_201810010000.csv")
colnames(PV_data10) <- as.character(PV_data10[1, ])
PV_data10 <- PV_data10[-1, ]

#Loaded data for PV individually, and remove first column and first row as not relevant for analysis
PV_data11 <- read.csv("PUBLIC_DVD_ROOFTOP_PV_ACTUAL_201811010000.csv")
colnames(PV_data11) <- as.character(PV_data11[1, ])
PV_data11 <- PV_data11[-1, ]

#Loaded data for PV individually, and remove first column and first row as not relevant for analysis
PV_data12 <- read.csv("PUBLIC_DVD_ROOFTOP_PV_ACTUAL_201812010000.csv")
colnames(PV_data12) <- as.character(PV_data12[1, ])
PV_data12 <- PV_data12[-1, ]

#Loaded data for PV individually, and remove first column and first row as not relevant for analysis
PV_data13 <- read.csv("PUBLIC_DVD_ROOFTOP_PV_ACTUAL_202401010000.csv") 
colnames(PV_data13) <- as.character(PV_data13[1, ])
PV_data13 <- PV_data13[-1, ]

#Loaded data for PV individually, and remove first column and first row as not relevant for analysis
PV_data14 <- read.csv("PUBLIC_DVD_ROOFTOP_PV_ACTUAL_202402010000.csv")
colnames(PV_data14) <- as.character(PV_data14[1, ])
PV_data14 <- PV_data14[-1, ]

#Loaded data for PV individually, and remove first column and first row as not relevant for analysis
PV_data15 <- read.csv("PUBLIC_DVD_ROOFTOP_PV_ACTUAL_202403010000.csv")
colnames(PV_data15) <- as.character(PV_data15[1, ])
PV_data15 <- PV_data15[-1, ]

#Loaded data for PV individually, and remove first column and first row as not relevant for analysis
PV_data16 <- read.csv("PUBLIC_DVD_ROOFTOP_PV_ACTUAL_202404010000.csv")
colnames(PV_data16) <- as.character(PV_data16[1, ])
PV_data16 <- PV_data16[-1, ]

#Loaded data for PV individually, and remove first column and first row as not relevant for analysis
PV_data17 <- read.csv("PUBLIC_DVD_ROOFTOP_PV_ACTUAL_202405010000.csv")
colnames(PV_data17) <- as.character(PV_data17[1, ])
PV_data17 <- PV_data17[-1, ]

#Loaded data for PV individually, and remove first column and first row as not relevant for analysis
PV_data18 <- read.csv("PUBLIC_DVD_ROOFTOP_PV_ACTUAL_202406010000.csv")
colnames(PV_data18) <- as.character(PV_data18[1, ])
PV_data18 <- PV_data18[-1, ]

#Loaded data for PV individually, and remove first column and first row as not relevant for analysis
PV_data19 <- read.csv("PUBLIC_DVD_ROOFTOP_PV_ACTUAL_202407010000.csv")
colnames(PV_data19) <- as.character(PV_data19[1, ])
PV_data19 <- PV_data19[-1, ]

#Loaded data for PV individually, and remove first column and first row as not relevant for analysis
PV_data20 <- read.csv("PUBLIC_DVD_ROOFTOP_PV_ACTUAL_202408010000.csv")
colnames(PV_data20) <- as.character(PV_data20[1, ])
PV_data20 <- PV_data20[-1, ]

#Loaded data for PV individually, and remove first column and first row as not relevant for analysis
PV_data21 <- read.csv("PUBLIC_DVD_ROOFTOP_PV_ACTUAL_202409010000.csv")
colnames(PV_data21) <- as.character(PV_data21[1, ])
PV_data21 <- PV_data21[-1, ]

#Loaded data for PV individually, and remove first column and first row as not relevant for analysis
PV_data22 <- read.csv("PUBLIC_DVD_ROOFTOP_PV_ACTUAL_202410010000.csv")
colnames(PV_data22) <- as.character(PV_data22[1, ])
PV_data22 <- PV_data22[-1, ]

#Loaded data for PV individually, and remove first column and first row as not relevant for analysis
PV_data23 <- read.csv("PUBLIC_DVD_ROOFTOP_PV_ACTUAL_202411010000.csv")
colnames(PV_data23) <- as.character(PV_data23[1, ])
PV_data23 <- PV_data23[-1, ]

#Loaded data for PV individually, and remove first column and first row as not relevant for analysis
PV_data24 <- read.csv("PUBLIC_DVD_ROOFTOP_PV_ACTUAL_202412010000.csv")
colnames(PV_data24) <- as.character(PV_data24[1, ])
PV_data24 <- PV_data24[-1, ]


#Create a PV_df_2018 dataframe with all 12 df merged together, and rename one of the column "SCADAVALUE' for consistnecy
PV_df_2018 = bind_rows(list(PV_data1, PV_data2, PV_data3, PV_data4, PV_data5, PV_data6, PV_data7, PV_data8, PV_data9, PV_data10, PV_data11, PV_data12))
colnames(PV_df_2018)[7] <- "SCADAVALUE"

#Create a PV_df_2024 dataframe with all 12 df merged together, and rename one of the column "SCADAVALUE' for consistnecy
PV_df_2024 = bind_rows(list(PV_data13, PV_data14, PV_data15, PV_data16, PV_data17, PV_data18, PV_data19, PV_data20, PV_data21, PV_data22, PV_data23, PV_data24))
colnames(PV_df_2024)[7] <- "SCADAVALUE"


#Using mutate with the df to select only Dispatch.Type with "Generation unit"
PV_df_2018  <- PV_df_2018  %>%
  mutate(Dispatch.Type = "Generation unit")

#Using mutate with the df to select only Fuel.Source Primary with "Solar"
PV_df_2018  <- PV_df_2018  %>%
  mutate(Fuel.Source...Primary = "Solar")

#Using mutate with the df to select only Fuel.Source Description with "Pv_Rooftop_Solar"
PV_df_2018  <- PV_df_2018  %>%
  mutate(Fuel.Source...Descriptor = "PV_Rooftop_Solar")

#Using mutate with the df to select only Technology.Type with "Renewable PV Rooftop"
PV_df_2018  <- PV_df_2018  %>%
  mutate(Technology.Type...Primary = "Renewable PV Rooftop")


#Using mutate with the df to select only Dispatch.Type with "Generation unit"
PV_df_2024  <- PV_df_2024  %>%
  mutate(Dispatch.Type = "Generation unit")

#Using mutate with the df to select only Fuel.Source Primary with "Solar"
PV_df_2024  <- PV_df_2024  %>%
  mutate(Fuel.Source...Primary = "Solar")

#Using mutate with the df to select only Fuel.Source Description with "Pv_Rooftop_Solar"
PV_df_2024  <- PV_df_2024  %>%
  mutate(Fuel.Source...Descriptor = "PV_Rooftop_Solar")

#Using mutate with the df to select only Technology.Type with "Renewable PV Rooftop"
PV_df_2024  <- PV_df_2024  %>%
  mutate(Technology.Type...Primary = "Renewable PV Rooftop")





# Create Pv_df_2018_withdate that converts the INTERVAL_DATETIME format and create a new column "DayMonthYear
PV_df_2018_withdate <- PV_df_2018 %>%
  mutate(
        INTERVAL_DATETIME = as.POSIXct(INTERVAL_DATETIME, format = "%Y/%m/%d %H:%M:%S"),
    
    `DayMonthYear` = format(INTERVAL_DATETIME, "%d %B %Y")
  )


# For consistency, regions grouped based on five states for PV_regions_grouped_2018
Pv_regions_grouped_2018 <- PV_df_2018_withdate %>%
  mutate(Region = case_when(
    grepl("^QLD", REGIONID) ~ "QLD1",  # All regions starting with QLD are grouped as QLD1
    grepl("^NSW", REGIONID) ~ "NSW1",  # All regions starting with NSW are grouped as NSW1
    grepl("^VIC", REGIONID) ~ "VIC1",  # All regions starting with VIC are grouped as VIC1
    grepl("^SA", REGIONID) ~ "SA1",# All regions starting with SA are grouped as SA1
    grepl("^TAS", REGIONID) ~ "TAS1",# All regions starting with TAS are grouped as TAS1
  ))
#Check to confirm the updates of the df in Region
table(Pv_regions_grouped_2018$Region)


# Convert SCADAVALUE to numeric
Pv_regions_grouped_2018 <- Pv_regions_grouped_2018[!grepl("power", Pv_regions_grouped_2018$SCADAVALUE, ignore.case = TRUE), ]
Pv_regions_grouped_2018$SCADAVALUE <- as.numeric(gsub(",", "", Pv_regions_grouped_2018$SCADAVALUE))





# Create Pv_df_2018_withdate that converts the INTERVAL_DATETIME format and create a new column "DayMonthYear
PV_df_2024_withdate <- PV_df_2024 %>%
  mutate(
    INTERVAL_DATETIME = as.POSIXct(INTERVAL_DATETIME, format = "%Y/%m/%d %H:%M:%S"),
    
    `DayMonthYear` = format(INTERVAL_DATETIME, "%d %B %Y")
  )


# For consistency, regions grouped based on five states for PV_regions_grouped_2024
Pv_regions_grouped_2024 <- PV_df_2024_withdate %>%
  mutate(Region = case_when(
    grepl("^QLD", REGIONID) ~ "QLD1",  # All regions starting with QLD are grouped as QLD1
    grepl("^NSW", REGIONID) ~ "NSW1",  # All regions starting with NSW are grouped as NSW1
    grepl("^VIC", REGIONID) ~ "VIC1",  # All regions starting with VIC are grouped as VIC1
    grepl("^SA", REGIONID) ~ "SA1",# All regions starting with SA are grouped as SA1
    grepl("^TAS", REGIONID) ~ "TAS1",# All regions starting with TAS are grouped as TAS1
  ))

#Check to confirm the updates of the df in Region
table(Pv_regions_grouped_2024$Region)

# Convert SCADAVALUE to numeric
Pv_regions_grouped_2024 <- Pv_regions_grouped_2024[!grepl("power", Pv_regions_grouped_2024$SCADAVALUE, ignore.case = TRUE), ]
Pv_regions_grouped_2024$SCADAVALUE <- as.numeric(gsub(",", "", Pv_regions_grouped_2024$SCADAVALUE))








#Create a new df (Pv_regions_grouped_2018) to group relevant columns and do a sum of the generation of PV_Rooftop with SCADAVALUE
Pv_regions_grouped_2018 <- Pv_regions_grouped_2018 %>%
  group_by(Region, Dispatch.Type, Technology.Type...Primary, 
           Fuel.Source...Descriptor, Fuel.Source...Primary, DayMonthYear) %>%
  summarise(PV_Rooftop = sum(SCADAVALUE, na.rm = TRUE), .groups = "drop")

# After summarizing, remove duplicates based on Region and DayMonthYear by using the distinct feature and making all values unique
Pv_regions_grouped_2018_unique <- Pv_regions_grouped_2018 %>%
  distinct(Region, DayMonthYear, .keep_all = TRUE)

#Create a new df (Pv_regions_grouped_2018) to group relevant columns and do a sum of the generation of PV_Rooftop with SCADAVALUE
Pv_regions_grouped_2024 <- Pv_regions_grouped_2024 %>%
   group_by(Region, Dispatch.Type, Technology.Type...Primary, 
           Fuel.Source...Descriptor, Fuel.Source...Primary, DayMonthYear) %>%
  summarise(PV_Rooftop = sum(SCADAVALUE, na.rm = TRUE), .groups = "drop")

# After summarizing, remove duplicates based on Region and DayMonthYear by using the distinct feature and making all values unique
Pv_regions_grouped_2024_unique <- Pv_regions_grouped_2024 %>%
  distinct(Region, DayMonthYear, .keep_all = TRUE)



#Trading Price analysis (With PUBLIC_DVD_TRADINGPRICE data)



# Trim CSV file of Trading Price to be readable and executable as too many columns (to 9 columns)
lines <- readLines("PUBLIC_DVD_TRADINGPRICE_201801010000.CSV")

# Split each line retain only the first 9 columns for RQ1 Analysis
lines_trimmed <- sapply(lines, function(line) {
  paste(strsplit(line, ",")[[1]][1:9], collapse = ",")
})

# Using the line_trimmed to save it as a temporary file
writeLines(lines_trimmed, "PUBLIC_DVD_TRADINGPRICE_201801010000.CSV_trimmed.csv")

# Trading data file can now be read and executed as a df
Trading_data1 <- read.csv("PUBLIC_DVD_TRADINGPRICE_201801010000.CSV_trimmed.csv", header = TRUE)

Trading_data1  <- Trading_data1 [-1, ]
colnames(Trading_data1)[7] <- "Region"
colnames(Trading_data1)[9] <- "RRP"


Trading_data1 <- Trading_data1 %>%
  mutate(
    PUBLIC = as.POSIXct(PUBLIC, format = "%Y/%m/%d %H:%M:%S"),
    DayMonthYear = format(PUBLIC, "%d %B %Y")
  )


Trading_data1$RRP <- as.numeric(Trading_data1$RRP)

# Group by Region and MonthYear, then calculate Daily RRP
Trading_data1 <- Trading_data1 %>%
  group_by(Region, DayMonthYear) %>%
  summarize(
    Daily_RRP = mean(RRP, na.rm = TRUE),
    .groups = "drop" 
  )

Trading_data1 <- na.omit(Trading_data1)


# Trim another CSV file of Trading Price to be readable and executable as too many columns (to 9 columns)
lines <- readLines("PUBLIC_DVD_TRADINGPRICE_201802010000.CSV")

# Split each line retain only the first 9 columns for RQ1 Analysis
lines_trimmed <- sapply(lines, function(line) {
  paste(strsplit(line, ",")[[1]][1:9], collapse = ",")
})

# Using the line_trimmed to save it as a temporary file
writeLines(lines_trimmed, "PUBLIC_DVD_TRADINGPRICE_201802010000.CSV_trimmed.csv")

# Trading data file can now be read and executed as a df
Trading_data2 <- read.csv("PUBLIC_DVD_TRADINGPRICE_201802010000.CSV_trimmed.csv", header = TRUE)

Trading_data2  <- Trading_data2 [-1, ]
colnames(Trading_data2)[7] <- "Region"
colnames(Trading_data2)[9] <- "RRP"


Trading_data2 <- Trading_data2 %>%
  mutate(
    PUBLIC = as.POSIXct(PUBLIC, format = "%Y/%m/%d %H:%M:%S"),
    DayMonthYear = format(PUBLIC, "%d %B %Y")
  )

Trading_data2$RRP <- as.numeric(Trading_data2$RRP)

# Group by Region and MonthYear, then calculate Daily RRP
Trading_data2 <- Trading_data2 %>%
  group_by(Region, DayMonthYear) %>%
  summarize(
    Daily_RRP = mean(RRP, na.rm = TRUE),
    .groups = "drop" 
  )

Trading_data2 <- na.omit(Trading_data2)

# Trim another CSV file of Trading Price to be readable and executable as too many columns (to 9 columns)
lines <- readLines("PUBLIC_DVD_TRADINGPRICE_201803010000.CSV")

# Split each line retain only the first 9 columns for RQ1 Analysis
lines_trimmed <- sapply(lines, function(line) {
  paste(strsplit(line, ",")[[1]][1:9], collapse = ",")
})

# Using the line_trimmed to save it as a temporary file
writeLines(lines_trimmed, "PUBLIC_DVD_TRADINGPRICE_201803010000.CSV_trimmed.csv")

# Trading data file can now be read and executed as a df
Trading_data3 <- read.csv("PUBLIC_DVD_TRADINGPRICE_201803010000.CSV_trimmed.csv", header = TRUE)

Trading_data3  <- Trading_data3 [-1, ]
colnames(Trading_data3)[7] <- "Region"
colnames(Trading_data3)[9] <- "RRP"

Trading_data3 <- Trading_data3 %>%
  mutate(
    PUBLIC = as.POSIXct(PUBLIC, format = "%Y/%m/%d %H:%M:%S"),
    DayMonthYear = format(PUBLIC, "%d %B %Y")
  )

Trading_data3$RRP <- as.numeric(Trading_data3$RRP)

# Group by Region and MonthYear, then calculate Daily RRP
Trading_data3 <- Trading_data3 %>%
  group_by(Region, DayMonthYear) %>%
  summarize(
    Daily_RRP = mean(RRP, na.rm = TRUE),
    .groups = "drop" 
  )

Trading_data3 <- na.omit(Trading_data3)



# Trim another CSV file of Trading Price to be readable and executable as too many columns (to 9 columns)
lines <- readLines("PUBLIC_DVD_TRADINGPRICE_201804010000.CSV")

# Split each line retain only the first 9 columns for RQ1 Analysis
lines_trimmed <- sapply(lines, function(line) {
  paste(strsplit(line, ",")[[1]][1:9], collapse = ",")
})

# Using the line_trimmed to save it as a temporary file
writeLines(lines_trimmed, "PUBLIC_DVD_TRADINGPRICE_201804010000.CSV_trimmed.csv")

# Trading data file can now be read and executed as a df
Trading_data4 <- read.csv("PUBLIC_DVD_TRADINGPRICE_201804010000.CSV_trimmed.csv", header = TRUE)

Trading_data4  <- Trading_data4 [-1, ]
colnames(Trading_data4)[7] <- "Region"
colnames(Trading_data4)[9] <- "RRP"


Trading_data4 <- Trading_data4 %>%
  mutate(
    PUBLIC = as.POSIXct(PUBLIC, format = "%Y/%m/%d %H:%M:%S"),
    DayMonthYear = format(PUBLIC, "%d %B %Y")
  )

Trading_data4$RRP <- as.numeric(Trading_data4$RRP)

# Group by Region and MonthYear, then calculate Daily RRP
Trading_data4 <- Trading_data4 %>%
  group_by(Region, DayMonthYear) %>%
  summarize(
    Daily_RRP = mean(RRP, na.rm = TRUE),
    .groups = "drop" 
  )

Trading_data4 <- na.omit(Trading_data4)


# Trim another CSV file of Trading Price to be readable and executable as too many columns (to 9 columns)
lines <- readLines("PUBLIC_DVD_TRADINGPRICE_201805010000.CSV")

# Split each line retain only the first 9 columns for RQ1 Analysis
lines_trimmed <- sapply(lines, function(line) {
  paste(strsplit(line, ",")[[1]][1:9], collapse = ",")
})

# Using the line_trimmed to save it as a temporary file
writeLines(lines_trimmed, "PUBLIC_DVD_TRADINGPRICE_201805010000.CSV_trimmed.csv")

# Trading data file can now be read and executed as a df
Trading_data5 <- read.csv("PUBLIC_DVD_TRADINGPRICE_201805010000.CSV_trimmed.csv", header = TRUE)

Trading_data5  <- Trading_data5 [-1, ]
colnames(Trading_data5)[7] <- "Region"
colnames(Trading_data5)[9] <- "RRP"

Trading_data5 <- Trading_data5 %>%
  mutate(
    PUBLIC = as.POSIXct(PUBLIC, format = "%Y/%m/%d %H:%M:%S"),
    DayMonthYear = format(PUBLIC, "%d %B %Y")
  )

Trading_data5$RRP <- as.numeric(Trading_data5$RRP)

# Group by Region and MonthYear, then calculate Daily RRP
Trading_data5 <- Trading_data5 %>%
  group_by(Region, DayMonthYear) %>%
  summarize(
    Daily_RRP = mean(RRP, na.rm = TRUE),
    .groups = "drop" 
  )

Trading_data5 <- na.omit(Trading_data5)


# Trim another CSV file of Trading Price to be readable and executable as too many columns (to 9 columns)
lines <- readLines("PUBLIC_DVD_TRADINGPRICE_201806010000.CSV")

# Split each line retain only the first 9 columns for RQ1 Analysis
lines_trimmed <- sapply(lines, function(line) {
  paste(strsplit(line, ",")[[1]][1:9], collapse = ",")
})

# Using the line_trimmed to save it as a temporary file
writeLines(lines_trimmed, "PUBLIC_DVD_TRADINGPRICE_201806010000.CSV_trimmed.csv")

# Trading data file can now be read and executed as a df
Trading_data6 <- read.csv("PUBLIC_DVD_TRADINGPRICE_201806010000.CSV_trimmed.csv", header = TRUE)

Trading_data6  <- Trading_data6 [-1, ]
colnames(Trading_data6)[7] <- "Region"
colnames(Trading_data6)[9] <- "RRP"


Trading_data6 <- Trading_data6 %>%
  mutate(
    PUBLIC = as.POSIXct(PUBLIC, format = "%Y/%m/%d %H:%M:%S"),
    DayMonthYear = format(PUBLIC, "%d %B %Y")
  )

Trading_data6$RRP <- as.numeric(Trading_data6$RRP)

# Group by Region and MonthYear, then calculate Daily RRP
Trading_data6 <- Trading_data6 %>%
  group_by(Region, DayMonthYear) %>%
  summarize(
    Daily_RRP = mean(RRP, na.rm = TRUE),
    .groups = "drop" 
  )

Trading_data6 <- na.omit(Trading_data6)

# Trim another CSV file of Trading Price to be readable and executable as too many columns (to 9 columns)
lines <- readLines("PUBLIC_DVD_TRADINGPRICE_201807010000.CSV")

# Split each line retain only the first 9 columns for RQ1 Analysis
lines_trimmed <- sapply(lines, function(line) {
  paste(strsplit(line, ",")[[1]][1:9], collapse = ",")
})

# Using the line_trimmed to save it as a temporary file
writeLines(lines_trimmed, "PUBLIC_DVD_TRADINGPRICE_201807010000.CSV_trimmed.csv")

# Trading data file can now be read and executed as a df
Trading_data7 <- read.csv("PUBLIC_DVD_TRADINGPRICE_201807010000.CSV_trimmed.csv", header = TRUE)

Trading_data7  <- Trading_data7 [-1, ]
colnames(Trading_data7)[7] <- "Region"
colnames(Trading_data7)[9] <- "RRP"


Trading_data7 <- Trading_data7 %>%
  mutate(
    PUBLIC = as.POSIXct(PUBLIC, format = "%Y/%m/%d %H:%M:%S"),
    DayMonthYear = format(PUBLIC, "%d %B %Y")
  )

Trading_data7$RRP <- as.numeric(Trading_data7$RRP)

# Group by Region and MonthYear, then calculate Daily RRP
Trading_data7 <- Trading_data7 %>%
  group_by(Region, DayMonthYear) %>%
  summarize(
    Daily_RRP = mean(RRP, na.rm = TRUE),
    .groups = "drop" 
  )

Trading_data7 <- na.omit(Trading_data7)



# Trim another CSV file of Trading Price to be readable and executable as too many columns (to 9 columns)
lines <- readLines("PUBLIC_DVD_TRADINGPRICE_201808010000.CSV")

# Split each line retain only the first 9 columns for RQ1 Analysis
lines_trimmed <- sapply(lines, function(line) {
  paste(strsplit(line, ",")[[1]][1:9], collapse = ",")
})

# Using the line_trimmed to save it as a temporary file
writeLines(lines_trimmed, "PUBLIC_DVD_TRADINGPRICE_201808010000.CSV_trimmed.csv")

# Trading data file can now be read and executed as a df
Trading_data8 <- read.csv("PUBLIC_DVD_TRADINGPRICE_201808010000.CSV_trimmed.csv", header = TRUE)

Trading_data8  <- Trading_data8 [-1, ]
colnames(Trading_data8)[7] <- "Region"
colnames(Trading_data8)[9] <- "RRP"


Trading_data8 <- Trading_data8 %>%
  mutate(
    PUBLIC = as.POSIXct(PUBLIC, format = "%Y/%m/%d %H:%M:%S"),
    DayMonthYear = format(PUBLIC, "%d %B %Y")
  )

Trading_data8$RRP <- as.numeric(Trading_data8$RRP)

# Group by Region and MonthYear, then calculate Daily RRP
Trading_data8 <- Trading_data8 %>%
  group_by(Region, DayMonthYear) %>%
  summarize(
    Daily_RRP = mean(RRP, na.rm = TRUE),
    .groups = "drop" 
  )

Trading_data8 <- na.omit(Trading_data8)

# Trim another CSV file of Trading Price to be readable and executable as too many columns (to 9 columns)
lines <- readLines("PUBLIC_DVD_TRADINGPRICE_201809010000.CSV")

# Split each line retain only the first 9 columns for RQ1 Analysis
lines_trimmed <- sapply(lines, function(line) {
  paste(strsplit(line, ",")[[1]][1:9], collapse = ",")
})

# Using the line_trimmed to save it as a temporary file
writeLines(lines_trimmed, "PUBLIC_DVD_TRADINGPRICE_201809010000.CSV_trimmed.csv")

# Trading data file can now be read and executed as a df
Trading_data9 <- read.csv("PUBLIC_DVD_TRADINGPRICE_201809010000.CSV_trimmed.csv", header = TRUE)

Trading_data9  <- Trading_data9 [-1, ]
colnames(Trading_data9)[7] <- "Region"
colnames(Trading_data9)[9] <- "RRP"

Trading_data9 <- Trading_data9 %>%
  mutate(
    PUBLIC = as.POSIXct(PUBLIC, format = "%Y/%m/%d %H:%M:%S"),
    DayMonthYear = format(PUBLIC, "%d %B %Y")
  )

Trading_data9$RRP <- as.numeric(Trading_data9$RRP)

# Group by Region and MonthYear, then calculate Daily RRP
Trading_data9 <- Trading_data9 %>%
  group_by(Region, DayMonthYear) %>%
  summarize(
    Daily_RRP = mean(RRP, na.rm = TRUE),
    .groups = "drop" 
  )

Trading_data9 <- na.omit(Trading_data9)

# Trim another CSV file of Trading Price to be readable and executable as too many columns (to 9 columns)
lines <- readLines("PUBLIC_DVD_TRADINGPRICE_201810010000.CSV")

# Split each line retain only the first 9 columns for RQ1 Analysis
lines_trimmed <- sapply(lines, function(line) {
  paste(strsplit(line, ",")[[1]][1:9], collapse = ",")
})

# Using the line_trimmed to save it as a temporary file
writeLines(lines_trimmed, "PUBLIC_DVD_TRADINGPRICE_201810010000.CSV_trimmed.csv")

# Trading data file can now be read and executed as a df
Trading_data10 <- read.csv("PUBLIC_DVD_TRADINGPRICE_201810010000.CSV_trimmed.csv", header = TRUE)

Trading_data10  <- Trading_data10 [-1, ]
colnames(Trading_data10)[7] <- "Region"
colnames(Trading_data10)[9] <- "RRP"

Trading_data10 <- Trading_data10 %>%
  mutate(
    PUBLIC = as.POSIXct(PUBLIC, format = "%Y/%m/%d %H:%M:%S"),
    DayMonthYear = format(PUBLIC, "%d %B %Y")
  )

Trading_data10$RRP <- as.numeric(Trading_data10$RRP)

# Group by Region and MonthYear, then calculate Daily RRP
Trading_data10 <- Trading_data10 %>%
  group_by(Region, DayMonthYear) %>%
  summarize(
    Daily_RRP = mean(RRP, na.rm = TRUE),
    .groups = "drop" 
  )

Trading_data10 <- na.omit(Trading_data10)

# Trim another CSV file of Trading Price to be readable and executable as too many columns (to 9 columns)
lines <- readLines("PUBLIC_DVD_TRADINGPRICE_201811010000.CSV")

# Split each line retain only the first 9 columns for RQ1 Analysis
lines_trimmed <- sapply(lines, function(line) {
  paste(strsplit(line, ",")[[1]][1:9], collapse = ",")
})

# Using the line_trimmed to save it as a temporary file
writeLines(lines_trimmed, "PUBLIC_DVD_TRADINGPRICE_201811010000.CSV_trimmed.csv")

# Trading data file can now be read and executed as a df
Trading_data11 <- read.csv("PUBLIC_DVD_TRADINGPRICE_201811010000.CSV_trimmed.csv", header = TRUE)

Trading_data11  <- Trading_data11 [-1, ]
colnames(Trading_data11)[7] <- "Region"
colnames(Trading_data11)[9] <- "RRP"

Trading_data11 <- Trading_data11 %>%
  mutate(
    PUBLIC = as.POSIXct(PUBLIC, format = "%Y/%m/%d %H:%M:%S"),
    DayMonthYear = format(PUBLIC, "%d %B %Y")
  )

Trading_data11$RRP <- as.numeric(Trading_data11$RRP)

# Group by Region and MonthYear, then calculate Daily RRP
Trading_data11 <- Trading_data11 %>%
  group_by(Region, DayMonthYear) %>%
  summarize(
    Daily_RRP = mean(RRP, na.rm = TRUE),
    .groups = "drop" 
  )

Trading_data11 <- na.omit(Trading_data11)

# Trim CSV file of Trading Price to be readable and executable as too many columns (to 9 columns)
lines <- readLines("PUBLIC_DVD_TRADINGPRICE_201812010000.CSV")


# Split each line retain only the first 9 columns for RQ1 Analysis
lines_trimmed <- sapply(lines, function(line) {
  paste(strsplit(line, ",")[[1]][1:9], collapse = ",")
})

# Using the line_trimmed to save it as a temporary file
writeLines(lines_trimmed, "PUBLIC_DVD_TRADINGPRICE_201812010000.CSV_trimmed.csv")


# Trading data file can now be read and executed as a df
Trading_data12 <- read.csv("PUBLIC_DVD_TRADINGPRICE_201812010000.CSV_trimmed.csv", header = TRUE)

Trading_data12  <- Trading_data12 [-1, ]
colnames(Trading_data12)[7] <- "Region"
colnames(Trading_data12)[9] <- "RRP"

Trading_data12 <- Trading_data12 %>%
  mutate(
    PUBLIC = as.POSIXct(PUBLIC, format = "%Y/%m/%d %H:%M:%S"),
    DayMonthYear = format(PUBLIC, "%d %B %Y")
  )

Trading_data12$RRP <- as.numeric(Trading_data12$RRP)

# Group by Region and MonthYear, then calculate Daily RRP
Trading_data12 <- Trading_data12 %>%
  group_by(Region, DayMonthYear) %>%
  summarize(
    Daily_RRP = mean(RRP, na.rm = TRUE),
    .groups = "drop" 
  )

Trading_data12 <- na.omit(Trading_data12)


# Trim another CSV file of Trading Price to be readable and executable as too many columns (to 9 columns)
lines <- readLines("PUBLIC_DVD_TRADINGPRICE_202401010000.CSV")

if (length(lines) > 0 && nzchar(lines[length(lines)])) {
  lines <- c(lines, "")  # This adds a final blank line, ensuring a newline at the end
}

writeLines(lines, "PUBLIC_DVD_TRADINGPRICE_202401010000.CSV")

# Split each line retain only the first 9 columns for RQ1 Analysis
lines_trimmed <- sapply(lines, function(line) {
  paste(strsplit(line, ",")[[1]][1:9], collapse = ",")
})

# Using the line_trimmed to save it as a temporary file
writeLines(lines_trimmed, "PUBLIC_DVD_TRADINGPRICE_202401010000.CSV_trimmed.csv")

# Trading data file can now be read and executed as a df
Trading_data13 <- read.csv("PUBLIC_DVD_TRADINGPRICE_202401010000.CSV_trimmed.csv", header = TRUE)

Trading_data13  <- Trading_data13 [-1, ]
colnames(Trading_data13)[7] <- "Region"
colnames(Trading_data13)[9] <- "RRP"

Trading_data13 <- Trading_data13 %>%
  mutate(
    PUBLIC = as.POSIXct(PUBLIC, format = "%Y/%m/%d %H:%M:%S"),
    DayMonthYear = format(PUBLIC, "%d %B %Y")
  )

Trading_data13$RRP <- as.numeric(Trading_data13$RRP)

# Group by Region and MonthYear, then calculate Daily RRP
Trading_data13 <- Trading_data13 %>%
  group_by(Region, DayMonthYear) %>%
  summarize(
    Daily_RRP = mean(RRP, na.rm = TRUE),
    .groups = "drop" 
  )

Trading_data13 <- na.omit(Trading_data13)


# Trim another CSV file of Trading Price to be readable and executable as too many columns (to 9 columns)
lines <- readLines("PUBLIC_DVD_TRADINGPRICE_202402010000.CSV")

if (length(lines) > 0 && nzchar(lines[length(lines)])) {
  lines <- c(lines, "")  # This adds a final blank line, ensuring a newline at the end
}

writeLines(lines, "PUBLIC_DVD_TRADINGPRICE_202402010000.CSV")

# Split each line retain only the first 9 columns for RQ1 Analysis
lines_trimmed <- sapply(lines, function(line) {
  paste(strsplit(line, ",")[[1]][1:9], collapse = ",")
})

# Using the line_trimmed to save it as a temporary file
writeLines(lines_trimmed, "PUBLIC_DVD_TRADINGPRICE_202402010000.CSV_trimmed.csv")

# Trading data file can now be read and executed as a df
Trading_data14 <- read.csv("PUBLIC_DVD_TRADINGPRICE_202402010000.CSV_trimmed.csv", header = TRUE)

Trading_data14  <- Trading_data14 [-1, ]
colnames(Trading_data14)[7] <- "Region"
colnames(Trading_data14)[9] <- "RRP"

Trading_data14 <- Trading_data14 %>%
  mutate(
    PUBLIC = as.POSIXct(PUBLIC, format = "%Y/%m/%d %H:%M:%S"),
    DayMonthYear = format(PUBLIC, "%d %B %Y")
  )

Trading_data14$RRP <- as.numeric(Trading_data14$RRP)

# Group by Region and MonthYear, then calculate Daily RRP
Trading_data14 <- Trading_data14 %>%
  group_by(Region, DayMonthYear) %>%
  summarize(
    Daily_RRP = mean(RRP, na.rm = TRUE),
    .groups = "drop" 
  )

Trading_data14 <- na.omit(Trading_data14)


# Trim another CSV file of Trading Price to be readable and executable as too many columns (to 9 columns)
lines <- readLines("PUBLIC_DVD_TRADINGPRICE_202403010000.CSV")

if (length(lines) > 0 && nzchar(lines[length(lines)])) {
  lines <- c(lines, "")  # This adds a final blank line, ensuring a newline at the end
}

writeLines(lines, "PUBLIC_DVD_TRADINGPRICE_202403010000.CSV")

# Split each line retain only the first 9 columns for RQ1 Analysis
lines_trimmed <- sapply(lines, function(line) {
  paste(strsplit(line, ",")[[1]][1:9], collapse = ",")
})

# Using the line_trimmed to save it as a temporary file
writeLines(lines_trimmed, "PUBLIC_DVD_TRADINGPRICE_202403010000.CSV_trimmed.csv")

# Trading data file can now be read and executed as a df
Trading_data15 <- read.csv("PUBLIC_DVD_TRADINGPRICE_202403010000.CSV_trimmed.csv", header = TRUE)

Trading_data15  <- Trading_data15 [-1, ]
colnames(Trading_data15)[7] <- "Region"
colnames(Trading_data15)[9] <- "RRP"


Trading_data15 <- Trading_data15 %>%
  mutate(
    PUBLIC = as.POSIXct(PUBLIC, format = "%Y/%m/%d %H:%M:%S"),
    DayMonthYear = format(PUBLIC, "%d %B %Y")
  )

Trading_data15$RRP <- as.numeric(Trading_data15$RRP)

# Group by Region and MonthYear, then calculate Daily RRP
Trading_data15 <- Trading_data15 %>%
  group_by(Region, DayMonthYear) %>%
  summarize(
    Daily_RRP = mean(RRP, na.rm = TRUE),
    .groups = "drop" 
  )

Trading_data15 <- na.omit(Trading_data15)



# Trim another CSV file of Trading Price to be readable and executable as too many columns (to 9 columns)
lines <- readLines("PUBLIC_DVD_TRADINGPRICE_202404010000.CSV")

if (length(lines) > 0 && nzchar(lines[length(lines)])) {
  lines <- c(lines, "")  # This adds a final blank line, ensuring a newline at the end
}



writeLines(lines, "PUBLIC_DVD_TRADINGPRICE_202404010000.CSV")

# Split each line retain only the first 9 columns for RQ1 Analysis
lines_trimmed <- sapply(lines, function(line) {
  paste(strsplit(line, ",")[[1]][1:9], collapse = ",")
})

# Using the line_trimmed to save it as a temporary file
writeLines(lines_trimmed, "PUBLIC_DVD_TRADINGPRICE_202404010000.CSV_trimmed.csv")

# Trading data file can now be read and executed as a df
Trading_data16 <- read.csv("PUBLIC_DVD_TRADINGPRICE_202404010000.CSV_trimmed.csv", header = TRUE)

Trading_data16  <- Trading_data16 [-1, ]
colnames(Trading_data16)[7] <- "Region"
colnames(Trading_data16)[9] <- "RRP"

Trading_data16 <- Trading_data16 %>%
  mutate(
    PUBLIC = as.POSIXct(PUBLIC, format = "%Y/%m/%d %H:%M:%S"),
    DayMonthYear = format(PUBLIC, "%d %B %Y")
  )



Trading_data16$RRP <- as.numeric(Trading_data16$RRP)

# Group by Region and MonthYear, then calculate Daily RRP
Trading_data16 <- Trading_data16 %>%
  group_by(Region, DayMonthYear) %>%
  summarize(
    Daily_RRP = mean(RRP, na.rm = TRUE),
    .groups = "drop" 
  )

Trading_data16 <- na.omit(Trading_data16)

# Trim another CSV file of Trading Price to be readable and executable as too many columns (to 9 columns)
lines <- readLines("PUBLIC_DVD_TRADINGPRICE_202405010000.CSV")


if (length(lines) > 0 && nzchar(lines[length(lines)])) {
  lines <- c(lines, "")  # This adds a final blank line, ensuring a newline at the end
}


writeLines(lines, "PUBLIC_DVD_TRADINGPRICE_202405010000.CSV")

# Split each line retain only the first 9 columns for RQ1 Analysis
lines_trimmed <- sapply(lines, function(line) {
  paste(strsplit(line, ",")[[1]][1:9], collapse = ",")
})

# Using the line_trimmed to save it as a temporary file
writeLines(lines_trimmed, "PUBLIC_DVD_TRADINGPRICE_202405010000.CSV_trimmed.csv")

# Trading data file can now be read and executed as a df
Trading_data17 <- read.csv("PUBLIC_DVD_TRADINGPRICE_202405010000.CSV_trimmed.csv", header = TRUE)

Trading_data17  <- Trading_data17 [-1, ]
colnames(Trading_data17)[7] <- "Region"
colnames(Trading_data17)[9] <- "RRP"


Trading_data17 <- Trading_data17 %>%
  mutate(
    PUBLIC = as.POSIXct(PUBLIC, format = "%Y/%m/%d %H:%M:%S"),
    DayMonthYear = format(PUBLIC, "%d %B %Y")
  )



Trading_data17$RRP <- as.numeric(Trading_data17$RRP)

# Group by Region and MonthYear, then calculate Daily RRP
Trading_data17 <- Trading_data17 %>%
  group_by(Region, DayMonthYear) %>%
  summarize(
    Daily_RRP = mean(RRP, na.rm = TRUE),
    .groups = "drop" 
  )

Trading_data17 <- na.omit(Trading_data17)

# Trim Another CSV file of Trading Price to be readable and executable as too many columns (to 9 columns)
lines <- readLines("PUBLIC_DVD_TRADINGPRICE_202406010000.CSV")

if (length(lines) > 0 && nzchar(lines[length(lines)])) {
  lines <- c(lines, "")  # This adds a final blank line, ensuring a newline at the end
}


writeLines(lines, "PUBLIC_DVD_TRADINGPRICE_202406010000.CSV")

# Split each line retain only the first 9 columns for RQ1 Analysis
lines_trimmed <- sapply(lines, function(line) {
  paste(strsplit(line, ",")[[1]][1:9], collapse = ",")
})

# Using the line_trimmed to save it as a temporary file
writeLines(lines_trimmed, "PUBLIC_DVD_TRADINGPRICE_202406010000.CSV_trimmed.csv")

# Trading data file can now be read and executed as a df
Trading_data18 <- read.csv("PUBLIC_DVD_TRADINGPRICE_202406010000.CSV_trimmed.csv", header = TRUE)

Trading_data18  <- Trading_data18 [-1, ]
colnames(Trading_data18)[7] <- "Region"
colnames(Trading_data18)[9] <- "RRP"

Trading_data18 <- Trading_data18 %>%
  mutate(
    PUBLIC = as.POSIXct(PUBLIC, format = "%Y/%m/%d %H:%M:%S"),
    DayMonthYear = format(PUBLIC, "%d %B %Y")
  )



Trading_data18$RRP <- as.numeric(Trading_data18$RRP)

# Group by Region and MonthYear, then calculate Daily RRP
Trading_data18 <- Trading_data18 %>%
  group_by(Region, DayMonthYear) %>%
  summarize(
    Daily_RRP = mean(RRP, na.rm = TRUE),
    .groups = "drop" 
  )

Trading_data18 <- na.omit(Trading_data18)


# Trim Another CSV file of Trading Price to be readable and executable as too many columns (to 9 columns)
lines <- readLines("PUBLIC_DVD_TRADINGPRICE_202407010000.CSV")

if (length(lines) > 0 && nzchar(lines[length(lines)])) {
  lines <- c(lines, "")  # This adds a final blank line, ensuring a newline at the end
}


writeLines(lines, "PUBLIC_DVD_TRADINGPRICE_202407010000.CSV")

# Split each line retain only the first 9 columns for RQ1 Analysis
lines_trimmed <- sapply(lines, function(line) {
  paste(strsplit(line, ",")[[1]][1:9], collapse = ",")
})

# Using the line_trimmed to save it as a temporary file
writeLines(lines_trimmed, "PUBLIC_DVD_TRADINGPRICE_202407010000.CSV_trimmed.csv")

# Trading data file can now be read and executed as a df
Trading_data19 <- read.csv("PUBLIC_DVD_TRADINGPRICE_202407010000.CSV_trimmed.csv", header = TRUE)

Trading_data19  <- Trading_data19 [-1, ]
colnames(Trading_data19)[7] <- "Region"
colnames(Trading_data19)[9] <- "RRP"

Trading_data19 <- Trading_data19 %>%
  mutate(
    PUBLIC = as.POSIXct(PUBLIC, format = "%Y/%m/%d %H:%M:%S"),
    DayMonthYear = format(PUBLIC, "%d %B %Y")
  )

Trading_data19$RRP <- as.numeric(Trading_data19$RRP)

# Group by Region and MonthYear, then calculate Daily RRP
Trading_data19 <- Trading_data19 %>%
  group_by(Region, DayMonthYear) %>%
  summarize(
    Daily_RRP = mean(RRP, na.rm = TRUE),
    .groups = "drop" 
  )

Trading_data19 <- na.omit(Trading_data19)


# Trim Another CSV file of Trading Price to be readable and executable as too many columns (to 9 columns)
lines <- readLines("PUBLIC_DVD_TRADINGPRICE_202408010000.CSV")

if (length(lines) > 0 && nzchar(lines[length(lines)])) {
  lines <- c(lines, "")  # This adds a final blank line, ensuring a newline at the end
}


writeLines(lines, "PUBLIC_DVD_TRADINGPRICE_202408010000.CSV")

# Split each line retain only the first 9 columns for RQ1 Analysis
lines_trimmed <- sapply(lines, function(line) {
  paste(strsplit(line, ",")[[1]][1:9], collapse = ",")
})

# Using the line_trimmed to save it as a temporary file
writeLines(lines_trimmed, "PUBLIC_DVD_TRADINGPRICE_202408010000.CSV_trimmed.csv")

# Trading data file can now be read and executed as a df
Trading_data20 <- read.csv("PUBLIC_DVD_TRADINGPRICE_202408010000.CSV_trimmed.csv", header = TRUE)

Trading_data20  <- Trading_data20 [-1, ]
colnames(Trading_data20)[7] <- "Region"
colnames(Trading_data20)[9] <- "RRP"


Trading_data20 <- Trading_data20 %>%
  mutate(
    PUBLIC = as.POSIXct(PUBLIC, format = "%Y/%m/%d %H:%M:%S"),
    DayMonthYear = format(PUBLIC, "%d %B %Y")
  )

Trading_data20$RRP <- as.numeric(Trading_data20$RRP)

# Group by Region and MonthYear, then calculate Daily RRP
Trading_data20 <- Trading_data20 %>%
  group_by(Region, DayMonthYear) %>%
  summarize(
    Daily_RRP = mean(RRP, na.rm = TRUE),
    .groups = "drop" 
  )

Trading_data20 <- na.omit(Trading_data20)


# Trim Another CSV file of Trading Price to be readable and executable as too many columns (to 9 columns)
lines <- readLines("PUBLIC_DVD_TRADINGPRICE_202409010000.CSV")

if (length(lines) > 0 && nzchar(lines[length(lines)])) {
  lines <- c(lines, "")  # This adds a final blank line, ensuring a newline at the end
}



writeLines(lines, "PUBLIC_DVD_TRADINGPRICE_202409010000.CSV")

# Split each line retain only the first 9 columns for RQ1 Analysis
lines_trimmed <- sapply(lines, function(line) {
  paste(strsplit(line, ",")[[1]][1:9], collapse = ",")
})

# Using the line_trimmed to save it as a temporary file
writeLines(lines_trimmed, "PUBLIC_DVD_TRADINGPRICE_202409010000.CSV_trimmed.csv")

# Trading data file can now be read and executed as a df
Trading_data21 <- read.csv("PUBLIC_DVD_TRADINGPRICE_202409010000.CSV_trimmed.csv", header = TRUE)

Trading_data21  <- Trading_data21 [-1, ]
colnames(Trading_data21)[7] <- "Region"
colnames(Trading_data21)[9] <- "RRP"

Trading_data21 <- Trading_data21 %>%
  mutate(
    PUBLIC = as.POSIXct(PUBLIC, format = "%Y/%m/%d %H:%M:%S"),
    DayMonthYear = format(PUBLIC, "%d %B %Y")
  )

Trading_data21$RRP <- as.numeric(Trading_data21$RRP)

# Group by Region and MonthYear, then calculate Daily RRP
Trading_data21 <- Trading_data21 %>%
  group_by(Region, DayMonthYear) %>%
  summarize(
    Daily_RRP = mean(RRP, na.rm = TRUE),
    .groups = "drop" 
  )

Trading_data21 <- na.omit(Trading_data21)

# Trim Another CSV file of Trading Price to be readable and executable as too many columns (to 9 columns)
lines <- readLines("PUBLIC_DVD_TRADINGPRICE_202410010000.CSV")


if (length(lines) > 0 && nzchar(lines[length(lines)])) {
  lines <- c(lines, "")  # This adds a final blank line, ensuring a newline at the end
}

writeLines(lines, "PUBLIC_DVD_TRADINGPRICE_202410010000.CSV")

# Split each line retain only the first 9 columns for RQ1 Analysis
lines_trimmed <- sapply(lines, function(line) {
  paste(strsplit(line, ",")[[1]][1:9], collapse = ",")
})

# Using the line_trimmed to save it as a temporary file
writeLines(lines_trimmed, "PUBLIC_DVD_TRADINGPRICE_202410010000.CSV_trimmed.csv")

# Trading data file can now be read and executed as a df
Trading_data22 <- read.csv("PUBLIC_DVD_TRADINGPRICE_202410010000.CSV_trimmed.csv", header = TRUE)

Trading_data22  <- Trading_data22 [-1, ]
colnames(Trading_data22)[7] <- "Region"
colnames(Trading_data22)[9] <- "RRP"

Trading_data22 <- Trading_data22 %>%
  mutate(
    PUBLIC = as.POSIXct(PUBLIC, format = "%Y/%m/%d %H:%M:%S"),
    DayMonthYear = format(PUBLIC, "%d %B %Y")
  )

Trading_data22$RRP <- as.numeric(Trading_data22$RRP)

# Group by Region and MonthYear, then calculate Daily RRP
Trading_data22 <- Trading_data22 %>%
  group_by(Region, DayMonthYear) %>%
  summarize(
    Daily_RRP = mean(RRP, na.rm = TRUE),
    .groups = "drop" 
  )

Trading_data22 <- na.omit(Trading_data22)

# Trim Another CSV file of Trading Price to be readable and executable as too many columns (to 9 columns)
lines <- readLines("PUBLIC_DVD_TRADINGPRICE_202411010000.CSV")

if (length(lines) > 0 && nzchar(lines[length(lines)])) {
  lines <- c(lines, "")  # This adds a final blank line, ensuring a newline at the end
}



writeLines(lines, "PUBLIC_DVD_TRADINGPRICE_202411010000.CSV")

# Split each line retain only the first 9 columns for RQ1 Analysis
lines_trimmed <- sapply(lines, function(line) {
  paste(strsplit(line, ",")[[1]][1:9], collapse = ",")
})

# Using the line_trimmed to save it as a temporary file
writeLines(lines_trimmed, "PUBLIC_DVD_TRADINGPRICE_202411010000.CSV_trimmed.csv")

# Trading data file can now be read and executed as a df
Trading_data23 <- read.csv("PUBLIC_DVD_TRADINGPRICE_202411010000.CSV_trimmed.csv", header = TRUE)

Trading_data23  <- Trading_data23 [-1, ]
colnames(Trading_data23)[7] <- "Region"
colnames(Trading_data23)[9] <- "RRP"

Trading_data23 <- Trading_data23 %>%
  mutate(
    PUBLIC = as.POSIXct(PUBLIC, format = "%Y/%m/%d %H:%M:%S"),
    DayMonthYear = format(PUBLIC, "%d %B %Y")
  )

Trading_data23$RRP <- as.numeric(Trading_data23$RRP)

# Group by Region and MonthYear, then calculate Daily RRP
Trading_data23 <- Trading_data23 %>%
  group_by(Region, DayMonthYear) %>%
  summarize(
    Daily_RRP = mean(RRP, na.rm = TRUE),
    .groups = "drop" 
  )

Trading_data23 <- na.omit(Trading_data23)

# Trim Another CSV file of Trading Price to be readable and executable as too many columns (to 9 columns)
lines <- readLines("PUBLIC_DVD_TRADINGPRICE_202412010000.CSV")

if (length(lines) > 0 && nzchar(lines[length(lines)])) {
  lines <- c(lines, "")  # This adds a final blank line, ensuring a newline at the end
}


writeLines(lines, "PUBLIC_DVD_TRADINGPRICE_202412010000.CSV")

# Split each line retain only the first 9 columns for RQ1 Analysis
lines_trimmed <- sapply(lines, function(line) {
  paste(strsplit(line, ",")[[1]][1:9], collapse = ",")
})

# Using the line_trimmed to save it as a temporary file
writeLines(lines_trimmed, "PUBLIC_DVD_TRADINGPRICE_202412010000.CSV_trimmed.csv")

# Trading data file can now be read and executed as a df
Trading_data24 <- read.csv("PUBLIC_DVD_TRADINGPRICE_202412010000.CSV_trimmed.csv", header = TRUE)

Trading_data24  <- Trading_data24 [-1, ]
colnames(Trading_data24)[7] <- "Region"
colnames(Trading_data24)[9] <- "RRP"


Trading_data24 <- Trading_data24 %>%
  mutate(
    PUBLIC = as.POSIXct(PUBLIC, format = "%Y/%m/%d %H:%M:%S"),
    DayMonthYear = format(PUBLIC, "%d %B %Y")
  )

Trading_data24$RRP <- as.numeric(Trading_data24$RRP)

# Group by Region and MonthYear, then calculate Daily RRP
Trading_data24 <- Trading_data24 %>%
  group_by(Region, DayMonthYear) %>%
  summarize(
    Daily_RRP = mean(RRP, na.rm = TRUE),
    .groups = "drop" 
  )

Trading_data24 <- na.omit(Trading_data24)



#Create a combined df (Trading_df_2018) with all the trading data in the same year (2018)

Trading_df_2018 = bind_rows(Trading_data1, Trading_data2, Trading_data3, Trading_data4, Trading_data5, Trading_data6, Trading_data7, Trading_data8, Trading_data9, Trading_data10, Trading_data11, Trading_data12)


#Create a combined df (Trading_df_2024) with all the trading data in the same year (2024)
Trading_df_2024 = bind_rows(Trading_data13, Trading_data14, Trading_data15, Trading_data16, Trading_data17, Trading_data18, Trading_data19, Trading_data20, Trading_data21, Trading_data22, Trading_data23, Trading_data24)


#Do an Inner join to create a new dataframe from two existing dfs (df_renewable_vs_combustion_vs_storage_2018_complete and Pv_regions_grouped_2018_unique)
merged_df_2018_withPV_RQ1<- df_renewable_vs_combustion_vs_storage_2018_complete %>%
  inner_join(
    Pv_regions_grouped_2018_unique %>% select(DayMonthYear, Region, PV_Rooftop), 
    by = c("DayMonthYear", "Region")
  )


# Merge two dfs together to create merged_df_2018_withPV_Trading with left_join
merged_df_2018_withPV_Trading <- Trading_df_2018 %>%
  left_join(merged_df_2018_withPV_RQ1, by = c("Region", "DayMonthYear"))

#Remove duplicates and only unique values for merged_df_2018_withPV_Trading 
merged_df_2018_withPV_Trading_unique_RQ1 <- merged_df_2018_withPV_Trading %>%
  distinct(Region, DayMonthYear, .keep_all = TRUE)

#Do an Inner join to create a new dataframe from two existing dfs (df_renewable_vs_combustion_vs_storage_2018_complete and Pv_regions_grouped_2018_unique)
merged_df_2024_withPV_RQ1<- df_renewable_vs_combustion_vs_storage_2024_complete %>%
  inner_join(
    Pv_regions_grouped_2024_unique %>% select(DayMonthYear, Region, PV_Rooftop), 
    by = c("DayMonthYear", "Region")
  )

# Merge two dfs together for 2024
merged_df_2024_withPV_RQ1_Trading <- Trading_df_2024 %>%
  left_join(merged_df_2024_withPV_RQ1, by = c("Region", "DayMonthYear"))

#Remove duplicates and only unique values for merged_df_2024_withPV_RQ1_Trading
merged_df_2024_withPV_Trading_unique_RQ1 <- merged_df_2024_withPV_RQ1_Trading %>%
  distinct(Region, DayMonthYear, .keep_all = TRUE)



#Using IQR method for removing outlines for merged_df_2018_withPV_Trading_unique_RQ1

Q1 <- quantile(merged_df_2018_withPV_Trading_unique_RQ1$Daily_RRP, 0.25, na.rm = TRUE)
Q3 <- quantile(merged_df_2018_withPV_Trading_unique_RQ1$Daily_RRP, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val

#New df created with no outliers from IQR method
merged_df_2018_withPV_Trading_unique_RQ1_noOutliers <- merged_df_2018_withPV_Trading_unique_RQ1[merged_df_2018_withPV_Trading_unique_RQ1$Daily_RRP >= lower_bound & merged_df_2018_withPV_Trading_unique_RQ1$Daily_RRP <= upper_bound, ]


#New df created for Correlation Analysis
RQ1_2018_correlation<- merged_df_2018_withPV_Trading_unique_RQ1_noOutliers


#Run Correlation tests for RQ1_2018 with the following method:

# For Correlation, requires numerical variables for analysis
RQ1_2018_correlation<- RQ1_2018_correlation[, sapply(RQ1_2018_correlation, is.numeric)]

# Correlation Matrix to be computed 
cor_matrix_2018 <- cor(RQ1_2018_correlation)

# Correlation matrix to be rounded to 2 decimal places
print(round(cor_matrix_2018, 2))


# Heatmap for visualization purposes
corrplot(cor_matrix_2018, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")

#Another visualization for correlation
ggpairs(RQ1_2018_correlation,
        upper = list(continuous = wrap("cor", size = 4)),
        lower = list(continuous = wrap("smooth", alpha = 0.3, color = "blue")),
        diag = list(continuous = wrap("densityDiag")))  # or use "barDiag"

# Select numerical columns for dataframe
numeric_RQ1_2018 <- RQ1_2018_correlation[, sapply(RQ1_2018_correlation, is.numeric)]

# Inspect the structure of numerical columns for dataframe
str(numeric_RQ1_2018)
cor_matrix_2018_correlation <- cor(RQ1_2018_correlation)

# Print the correlation matrix
print(cor_matrix_2018_correlation)


#Add a new column where AllRenewable is the sum of Renewable and Pv Rooftop for 2018

merged_df_2018_withPV_Trading_unique_RQ1_noOutliers <- merged_df_2018_withPV_Trading_unique_RQ1_noOutliers %>%
  mutate(AllRenewable = Renewable + PV_Rooftop)


#Test linearity assumption with a scatterplot
ggplot(merged_df_2018_withPV_Trading_unique_RQ1_noOutliers, aes(x = AllRenewable, y = Daily_RRP)) +
  geom_point() +
  labs(title = "RQ1 Scatterplot for testing Linearity with model 2018")  +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed")  # Linear line

#Findings: 
#Scatter plot of model 2018 reflected a negative downward linear line


# Fit model with residuals and test assumptions of linear model with 2018 with no outliers 
model_2018_Original <- lm(Daily_RRP ~ AllRenewable + Region, data = merged_df_2018_withPV_Trading_unique_RQ1_noOutliers)

# Fortify 
model_2018_Original.fort <- fortify(model_2018_Original) 

head(model_2018_Original.fort)

#Assume mean of residuals as 0

round(mean(model_2018_Original.fort$.resid), 3)

#Create a scatter plot of the residuals 
ggplot(data = model_2018_Original.fort, aes(x = .fitted, y = .resid))+
  geom_point()+
  theme_bw()+
  geom_smooth()+
  labs(title = "RQ1 residuals vs fitted for Linearity assumption with model 2018")
    x = expression(paste("Fitted (",hat(y[i]), ")"))
       y = expression(paste("Residual (",epsilon[i],")"))

#Findings from the assumptions:
# The residuals appear to be homoscedastic.
#The mean is approximately 0 as we move from low to high fitted values.
# The residuals appear to be homogeneously distributed about a mean of zero.
# Evenly spread above and below 0, no visible patterns.


#Create a histogram of the residuals of the model_2018
ggplot(data = model_2018_Original.fort, aes(x = .resid))+
  geom_histogram(colour = "grey", fill = "coral", aes(y = ..density..))+
  theme_bw()+ labs(title = "RQ1 histogram of Linearity assumption with model 2018") +
  stat_function(fun = dnorm, args = list(mean = mean(model_2018_Original.fort$.resid), sd = sd(model_2018_Original.fort$.resid)))+
  labs(x = "Residual", y = "Density")

# Findings:
#The residuals seem to be normally distributed

#Create a histogram of the standardized residuals of the model_2018
ggplot(data = model_2018_Original.fort, aes(x = .stdresid))+
  geom_histogram(colour = "grey", fill = "coral", aes(y = ..density..))+
  theme_bw() + labs(title = "RQ1 histogram (standardised) of Linearity assumption with model 2018") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1))+
  labs(x = "Standardised Residual", y = "Density")

#Findings:
#Again the residuals seem to be normally distributed

#Create a quantile_quantile plot of the model_2018
ggplot(data=model_2018_Original.fort, aes(sample=.stdresid)) +
  stat_qq() + labs(title = "RQ1 quantile_quantile of Linearity assumption with model 2018") +
  geom_abline(intercept=0, slope=1) +
  coord_equal()+
  theme_bw()

#Findings:
#The values appear to lie quite close to the line


#In sum, the assumptions of linear tested that this explains linearity being met in this model Of 2018




#Using IQR method for removing outlines for merged_df_2024_withPV_Trading_unique_RQ1

Q1 <- quantile(merged_df_2024_withPV_Trading_unique_RQ1$Daily_RRP, 0.25, na.rm = TRUE)
Q3 <- quantile(merged_df_2024_withPV_Trading_unique_RQ1$Daily_RRP, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val

#New df created with no outliers from IQR method
merged_df_2024_withPV_Trading_unique_RQ1_noOutliers <- merged_df_2024_withPV_Trading_unique_RQ1[merged_df_2024_withPV_Trading_unique_RQ1$Daily_RRP >= lower_bound & merged_df_2024_withPV_Trading_unique_RQ1$Daily_RRP <= upper_bound, ]


#New df created for Correlation Analysis
RQ1_2024_correlation<- merged_df_2024_withPV_Trading_unique_RQ1_noOutliers


#Run Correlation tests for RQ1_2018 with the following method:
# For Correlation, requires numerical variables for analysis
RQ1_2024_correlation<- RQ1_2024_correlation[, sapply(RQ1_2024_correlation, is.numeric)]

# Correlation Matrix to be computed 
cor_matrix_2024 <- cor(RQ1_2024_correlation)

# Correlation matrix to be rounded to 2 decimal places
print(round(cor_matrix_2024, 2))


# Basic heatmap
corrplot(cor_matrix_2024, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")

#Another visualization for correlation
ggpairs(RQ1_2024_correlation,
        upper = list(continuous = wrap("cor", size = 4)),
        lower = list(continuous = wrap("smooth", alpha = 0.3, color = "blue")),
        diag = list(continuous = wrap("densityDiag")))  # or use "barDiag"

# Select numerical columns for dataframe
numeric_RQ1_2024 <- RQ1_2024_correlation[, sapply(RQ1_2024_correlation, is.numeric)]

# Inspect the structure of numerical columns for dataframe
str(numeric_RQ1_2024)
cor_matrix_2024_correlation <- cor(RQ1_2024_correlation)

# Print the correlation matrix
print(cor_matrix_2024_correlation)


#Add a new column where AllRenewable is the sum of Renewable and Pv Rooftop for 2024

merged_df_2024_withPV_Trading_unique_RQ1_noOutliers <- merged_df_2024_withPV_Trading_unique_RQ1_noOutliers %>%
  mutate(AllRenewable = Renewable + PV_Rooftop)


#Test linearity assumption with a scatterplot
ggplot(merged_df_2024_withPV_Trading_unique_RQ1_noOutliers, aes(x = AllRenewable, y = Daily_RRP)) +
  geom_point() +
  labs(title = "RQ1 Scatterplot for testing Linearity with model 2024") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed")  # Linear line

#Findings: 
#Scatter plot of model 2024 reflected a negative downward linear line


####Residuals and test for the assumptions of a linear model for 2024 

# Fit model with residuals and test assumptions of linear model with 2018 with no outliers 
model_2024_Original <- lm(Daily_RRP ~ AllRenewable + Region, data = merged_df_2024_withPV_Trading_unique_RQ1_noOutliers)

# Fortify (optional)
model_2024_Original.fort <- fortify(model_2024_Original)  # or use augment(personal_model)



head(model_2024_Original.fort)

#Assume the mean of the residuals is 0

round(mean(model_2024_Original.fort$.resid), 3)

#Create a scatter plot of the residuals,  εi , .resid, against the fitted values,  y^i , .fitted. 
ggplot(data = model_2024_Original.fort, aes(x = .fitted, y = .resid))+
  geom_point()+
  theme_bw()+
  geom_smooth()+
  labs(title = "RQ1 residuals vs fitted for Linearity assumption with model 2024")
    x = expression(paste("Fitted (",hat(y[i]), ")"))
       y = expression(paste("Residual (",epsilon[i],")"))

#Findings:
# The residuals appear to be homoscedastic.
#The mean is approximately 0 as we move from low to high fitted values.
# The residuals appear to be homogeneously distributed about a mean of zero.
# Evenly spread above and below 0, no visible patterns.

#Create a histogram of the residuals of the model_2018
ggplot(data = model_2024_Original.fort, aes(x = .resid))+
  geom_histogram(colour = "grey", fill = "coral", aes(y = ..density..))+
  theme_bw()+ labs(title = "RQ1 histogram of Linearity assumption with model 2024") +
  stat_function(fun = dnorm, args = list(mean = mean(model_2024_Original.fort$.resid), sd = sd(model_2024_Original.fort$.resid)))+
  labs(x = "Residual", y = "Density")

#Findings:
#The  histogram appear to be normally distributed

#Create a histogram of the standardised residuals of the model_2018
ggplot(data = model_2024_Original.fort, aes(x = .stdresid))+
  geom_histogram(colour = "grey", fill = "coral", aes(y = ..density..))+
  theme_bw()+ labs(title = "RQ1 histogram (standardised) of Linearity assumption with model 2024") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1))+
  labs(x = "Standardised Residual", y = "Density")

#Findings:
#Again histogram (standardised) seem to be normally distributed

#Create a quantile_quantile plot of the model_2018
ggplot(data=model_2024_Original.fort, aes(sample=.stdresid)) +
  stat_qq() + labs(title = "RQ1 quantile_quantile of Linearity assumption with model 2024") +
  geom_abline(intercept=0, slope=1) +
  coord_equal()+
  theme_bw()

#Findings:
#The values appear to lie quite close to the line
#In sum, the the assumptions of linear tested was met that explains linearity are met in this model Of 2024




#ggplot with Rsquared and pvalue for Allrenewable and Daily_RRP for 2018 model by Region
stats <- merged_df_2018_withPV_Trading_unique_RQ1_noOutliers %>%
  group_by(Region) %>%
  summarise(
    r_squared = summary(lm(Daily_RRP ~ AllRenewable))$r.squared,
    p_value = summary(lm(Daily_RRP ~ AllRenewable))$coefficients[2,4]
  ) %>%
  mutate(
    label = paste0("R² = ", round(r_squared, 3), "\n",
                   "p = ", signif(p_value, 3))
  )

ggplot(merged_df_2018_withPV_Trading_unique_RQ1_noOutliers) +
  geom_point(aes(x = AllRenewable, y = Daily_RRP), alpha = 0.2, colour = 'black') +
  geom_smooth(aes(x = AllRenewable, y = Daily_RRP), method = 'lm', se = FALSE) +
  facet_wrap(~Region, nrow = 4) +
  geom_text(
    data = stats,
    aes(x = min(merged_df_2018_withPV_Trading_unique_RQ1_noOutliers$AllRenewable), y = max(merged_df_2018_withPV_Trading_unique_RQ1_noOutliers$Daily_RRP), label = label),
    inherit.aes = FALSE,
    size = 4
  ) +
  labs(title = " Daily_RRP vs Allrenewable for 2018 model by Region with rsquared and pvalue") +
  theme_minimal() +
  coord_cartesian(clip = "off")




#ggplot with Rsquared and pvalue for Allrenewable and Daily_RRP for 2024 model by Region

stats <- merged_df_2024_withPV_Trading_unique_RQ1_noOutliers %>%
  group_by(Region) %>%
  summarise(
    r_squared = summary(lm(Daily_RRP ~ AllRenewable))$r.squared,
    p_value = summary(lm(Daily_RRP ~ AllRenewable))$coefficients[2,4]
  ) %>%
  mutate(
    label = paste0("R² = ", round(r_squared, 3), "\n",
                   "p = ", signif(p_value, 3))
  )

ggplot(merged_df_2024_withPV_Trading_unique_RQ1_noOutliers) +
  geom_point(aes(x = AllRenewable, y = Daily_RRP), alpha = 0.2, colour = 'red') +
  geom_smooth(aes(x = AllRenewable, y = Daily_RRP), method = 'lm', se = FALSE) +
  facet_wrap(~Region, nrow = 4) +
  geom_text(
    data = stats,
    aes(x = min(merged_df_2024_withPV_Trading_unique_RQ1_noOutliers$AllRenewable), y = max(merged_df_2024_withPV_Trading_unique_RQ1_noOutliers$Daily_RRP), label = label),
    inherit.aes = FALSE,
    size = 4
  ) +
  labs(title = " Daily_RRP vs Allrenewable for 2024 model by Region with rsquared and pvalue") +
  theme_minimal() +
  coord_cartesian(clip = "off")






#Using Stepwise to discover main effects and interactions for 2018 data with AllRenewable variable instead for interpretation
full_model_2018_investigation1 <- lm(Daily_RRP ~ (AllRenewable + Combustion + Region)^2, data = merged_df_2018_withPV_Trading_unique_RQ1_noOutliers)
stepwise_model_2018_investigation1 <- step(full_model_2018_investigation1, direction = "both")



# Based on AIC to determine the best model with main effects and interactions 
stepwise_full_model_2018_investigation1 <- lm(Daily_RRP ~ (AllRenewable + Combustion + Region)^2 , data = merged_df_2018_withPV_Trading_unique_RQ1_noOutliers)
summary (stepwise_full_model_2018_investigation1)
eta_squared(stepwise_full_model_2018_investigation1, partial=FALSE)
#Adjusted R-squared:  0.2847, p-value: < 2.2e-16


#RQ1: How does Allrenewable(Renewable +Rooftop) predict their Daily RRP and does this trend differ across states?
AllRenewableWith_2018_Investigation2.lm <- lm(Daily_RRP ~ AllRenewable + Region + AllRenewable:Region, data = merged_df_2018_withPV_Trading_unique_RQ1_noOutliers)
summary (AllRenewableWith_2018_Investigation2.lm)
eta_squared(AllRenewableWith_2018_Investigation2.lm, partial=FALSE)
#Adjusted R-squared:  0.2516, p-value: < 2.2e-16, 




# Use Stepwise analysis, this time with AllRenewable variable instead for interpretation
full_model_2024_investigation3 <- lm(Daily_RRP ~ (AllRenewable + Combustion + Region)^2, data = merged_df_2024_withPV_Trading_unique_RQ1_noOutliers)
full_model_2024_investigation3  <- step(full_model_2024_investigation3, direction = "both")

# Based on AIC to determine the best model with main effects and interactions
stepwise_full_model_2024_investigation3.lm <- lm(Daily_RRP ~ (AllRenewable + Combustion + Region)^2 , data = merged_df_2024_withPV_Trading_unique_RQ1_noOutliers)
summary (stepwise_full_model_2024_investigation3.lm)
eta_squared(stepwise_full_model_2024_investigation3.lm, partial=FALSE)
#Adjusted R-squared:  0.5286, p-value: < 2.2e-16

#RQ1: How does Allrenewable(Renewable +Rooftop) predict their Daily RRP and does this trend differ across states?
AllRenewableWith_2024_Investigation4.lm <- lm(Daily_RRP ~ AllRenewable + Region + AllRenewable:Region, data = merged_df_2024_withPV_Trading_unique_RQ1_noOutliers)
summary (AllRenewableWith_2024_Investigation4.lm)
eta_squared(AllRenewableWith_2024_Investigation4.lm, partial=FALSE)
#Adjusted R-squared:  0.1909, p-value: < 2.2e-16




#####RQ2##########
# Group by Region and sum of SCADAVALUE with a new df for fuel source 2018
summarized_merged_df_fuelsource_2018 <- merged_df_2018 %>%
  group_by(Region, Fuel.Source...Primary, DayMonthYear) %>%
  summarize(Total_Generation = sum(SCADAVALUE, na.rm = TRUE),
  )
summarized_merged_df_fuelsource_2018 <- na.omit(summarized_merged_df_fuelsource_2018)




# Pivot the 'Fuel Source' column into multiple columns that can be used to identify the energy source
df_fuel_source_2018 <- summarized_merged_df_fuelsource_2018 %>%
  pivot_wider(names_from = Fuel.Source...Primary, values_from = Total_Generation)

#Replace NA values with '0' for analysis purposes
df_fuel_source_2018_complete <- df_fuel_source_2018 %>%
  mutate(across(c(Fossil, Hydro, Solar, Wind), ~replace(., is.na(.), 0)))

#Delete columns that are not meaningful/insightful 
df_fuel_source_2018_complete$`Renewable/ Biomass / Waste` <- NULL
df_fuel_source_2018_complete$'-' <- NULL


#Make four new variables so that they are all numerical for analysis

df_fuel_source_2018_complete$Fossil <- as.numeric(df_fuel_source_2018_complete$Fossil)
df_fuel_source_2018_complete$Hydro <- as.numeric(df_fuel_source_2018_complete$Hydro)
df_fuel_source_2018_complete$Solar <- as.numeric(df_fuel_source_2018_complete$Solar)
df_fuel_source_2018_complete$Wind <- as.numeric(df_fuel_source_2018_complete$Wind)

# Group by Region and sum of SCADAVALUE with a new df for fuel source 2024
summarized_merged_df_fuelsource_2024<- merged_df_2024 %>%
  group_by(Region, Fuel.Source...Primary, DayMonthYear) %>%
  summarize(Total_Generation = sum(SCADAVALUE, na.rm = TRUE),
  )
summarized_merged_df_fuelsource_2024 <- na.omit(summarized_merged_df_fuelsource_2024)

# Pivot the 'Fuel Source' column into multiple columns that can be used to identify the energy source
df_fuel_source_2024 <- summarized_merged_df_fuelsource_2024 %>%
  pivot_wider(names_from = Fuel.Source...Primary, values_from = Total_Generation)

#Replace NA values with '0' for analysis purposes
df_fuel_source_2024_complete <- df_fuel_source_2024 %>%
  mutate(across(c(Fossil, Hydro, Solar, Wind), ~replace(., is.na(.), 0)))

#Delete columns that are not meaningful/insightful 
df_fuel_source_2024_complete$`Renewable/ Biomass / Waste and Fossil` <- NULL
df_fuel_source_2024_complete$'-' <- NULL


#Make the four new variables so that they are all numerical for analysis
df_fuel_source_2024_complete$Fossil <- as.numeric(df_fuel_source_2024_complete$Fossil)
df_fuel_source_2024_complete$Hydro <- as.numeric(df_fuel_source_2024_complete$Hydro)
df_fuel_source_2024_complete$Solar <- as.numeric(df_fuel_source_2024_complete$Solar)
df_fuel_source_2024_complete$Wind <- as.numeric(df_fuel_source_2024_complete$Wind)


#Merge two df together for 2018
merged_df_2018_withPV_RQ2<- df_fuel_source_2018_complete %>%
  inner_join(
    Pv_regions_grouped_2018_unique %>% select(DayMonthYear, Region, PV_Rooftop), 
    by = c("DayMonthYear", "Region")
  )

#Merge two df together for 2024
merged_df_2024_withPV_RQ2<- df_fuel_source_2024_complete %>%
  inner_join(
    Pv_regions_grouped_2024_unique %>% select(DayMonthYear, Region, PV_Rooftop), 
    by = c("DayMonthYear", "Region")
  )

#BOM DATA for RQ2

#Load data of weatherAUS.CSV as RQ2_BOM, rename second column to Location and check for Unique

RQ2_BOM <- read.csv("weatherAUS.CSV") 
summary(RQ2_BOM)
colnames(RQ2_BOM)[2] <- "Location"
unique(RQ2_BOM$Location)

#There are some locations that aren't part of NEM network, filtered out ones not relevant
RQ2_BOM <- RQ2_BOM %>%
  filter(!Location %in% c("AliceSprings", "Darwin", "Katherine", "Uluru", "GoldCoast", "Cairns", "Albany", "Witchcliffe", "PearceRAAF", "PerthAirport",
                          "Perth", "SalmonGums", "Walpole" , "NorfolkIsland"))


#Create a df that groups the locations into their respective regions (i.e, states) for analysis
RQ2_BOM <- RQ2_BOM %>%
  mutate(Region = case_when(
    Location %in% c("Albury", "BadgerysCreek", "Cobar", "CoffsHarbour", "Moree",
                    "Newcastle", "NorahHead", "Penrith", "Richmond",
                    "Sydney", "SydneyAirport", "WaggaWagga", "Williamtown", "Wollongong") ~ "NSW",
    
    Location %in% c("Canberra", "Tuggeranong", "MountGinini") ~ "ACT",
    
    Location %in% c("Ballarat", "Bendigo", "Sale", "MelbourneAirport", "Melbourne",
                    "Mildura", "Nhil", "Portland", "Watsonia", "Dartmoor") ~ "VIC",
    
    Location %in% c("Brisbane", "Cairns", "GoldCoast", "Townsville") ~ "QLD",
    
    Location %in% c("Adelaide", "MountGambier", "Nuriootpa", "Woomera") ~ "SA",
    
    Location %in% c("Hobart", "Launceston") ~ "TAS",
    
  ))
#Create a DayMonthYear column which is the same as previous df
RQ2_BOM <- RQ2_BOM %>%
  mutate(
    Date = as.Date(Date, format = "%d/%m/%Y"),      # Convert character to Date
    DayMonthYear = format(Date, "%d %B %Y")         # Format: "14 April 2025"
  )
#Create a new DF so that it contains only data with 2018
RQ2_BOM_2018 <- RQ2_BOM %>%
  filter(year(Date) %in% c(2018))

#Create a new DF so that it contains only data with 2024
RQ2_BOM_2024 <- RQ2_BOM %>%
  filter(year(Date) %in% c(2024))


#Group all Days and Regions together with median across numerical values for 2018
RQ2_BOM_2018 <- RQ2_BOM_2018 %>%
  group_by(DayMonthYear, Region) %>%
  summarise(
    across(where(is.numeric), \(x) median(x, na.rm = TRUE)),
    .groups = "drop"
  )

#Group all Days and Regions together with median across numerical values for 2024
RQ2_BOM_2024 <- RQ2_BOM_2024 %>%
  group_by(DayMonthYear, Region) %>%
  summarise(
    across(where(is.numeric), \(x) median(x, na.rm = TRUE)),
    .groups = "drop"
  )

#Replaces all Nan and NA values with '0' for RQ2_BOM_2018
RQ2_BOM_2018 <- RQ2_BOM_2018  %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.) | is.nan(.), 0, .)))



#Replaces all Nan and NA values with '0' for RQ2_BOM_2024
RQ2_BOM_2024 <- RQ2_BOM_2024  %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.) | is.nan(.), 0, .)))


#Rename column 'Region' so it is consistent with other dataframes 
RQ2_BOM_2018 <- RQ2_BOM_2018 %>%
  mutate(
    Region = case_when(
      Region == "NSW" ~ "NSW1",    
      Region == "QLD" ~ "QLD1",    
      Region == "VIC" ~ "VIC1", 
      Region == "SA" ~ "SA1",      
      Region == "TAS" ~ "TAS1",   
      TRUE ~ Region                
    )
  )
#Delete Rows containing ACT for RQ2_BOM_2018 using NSW Data Instead
RQ2_BOM_2018 <- RQ2_BOM_2018 %>%
  filter(Region != "ACT")


#Rename column 'Region' so it is consistent with other dataframes 
RQ2_BOM_2024 <- RQ2_BOM_2024 %>%
  mutate(
    Region = case_when(
      Region == "NSW" ~ "NSW1",    
      Region == "QLD" ~ "QLD1",    
      Region == "VIC" ~ "VIC1",    
      Region == "SA" ~ "SA1",     
      Region == "TAS" ~ "TAS1",   
      TRUE ~ Region                
    )
  )

#Delete Rows containing ACT for RQ2_BOM_2018 using NSW Data Instead
RQ2_BOM_2024 <- RQ2_BOM_2024 %>%
  filter(Region != "ACT")

#Cleans data with unique variables for RQ2_BOM_2018
RQ2_BOM_2018_unique <- RQ2_BOM_2018 %>%
  distinct()

#Group by Region and DayMonthYear for 2018
RQ2_BOM_2018_unique <- RQ2_BOM_2018_unique %>%
  group_by(Region, DayMonthYear)


#Cleans data with unique variables for RQ2_BOM_2018
RQ2_BOM_2024_unique <- RQ2_BOM_2024 %>%
  distinct()

#Group by Region and DayMonthYear for 2024
RQ2_BOM_2024_unique <- RQ2_BOM_2024_unique %>%
  group_by(Region, DayMonthYear)


# Merge two dfs together to create merged_df_2018_withPV_Trading with left_join
merged_df_2018_withPV_Trading <- Trading_df_2018 %>%
  left_join(merged_df_2018_withPV_RQ2, by = c("Region", "DayMonthYear"))

#Remove duplicates and only unique values for merged_df_2018_withPV_Trading 
merged_df_2018_withPV_Trading_unique_RQ2 <- merged_df_2018_withPV_Trading %>%
  distinct(Region, DayMonthYear, .keep_all = TRUE)











#Merge two dfs together for analysis 2018
merged_df_2018_WithBOM_RQ2 <- merge(merged_df_2018_withPV_RQ2, Trading_df_2018, by = c("Region", "DayMonthYear"), all.x = TRUE)
merged_df_2018_withBOM_RQ2 <- merge (merged_df_2018_WithBOM_RQ2, RQ2_BOM_2018_unique, by = c("Region", "DayMonthYear"), all.x = TRUE)

#Choose the relevant variables that will be used for analysis in RQ2
merged_df_2018_WithBOM_Selected_RQ2 <- merged_df_2018_withBOM_RQ2 %>% select(DayMonthYear, Region, Daily_RRP, PV_Rooftop, Fossil, Hydro, Wind, Solar, MinTemp, MaxTemp, Sunshine, WindGustSpeed, Rainfall)
merged_df_2018_WithBOM_Selected_RQ2$DayMonthYear <- as.Date(merged_df_2018_WithBOM_Selected_RQ2$DayMonthYear, format = "%d %b %Y")
merged_df_2018_WithBOM_Selected_RQ2 <- merged_df_2018_WithBOM_Selected_RQ2 %>%
  filter(format(DayMonthYear, "%Y") == "2018")

#Remove Na values in df
merged_df_2018_WithBOM_Selected_RQ2 <- na.omit(merged_df_2018_WithBOM_Selected_RQ2)

#Merge two dfs together for analysis 2024
merged_df_2024_WithBOM_RQ2 <- merge(merged_df_2024_withPV_RQ2, Trading_df_2024, by = c("Region", "DayMonthYear"), all.x = TRUE)
merged_df_2024_withBOM_RQ2 <- merge(merged_df_2024_WithBOM_RQ2, RQ2_BOM_2024_unique, by = c("Region", "DayMonthYear"), all.x = TRUE)

#Choose the relevant variables that will be used for analysis in RQ2
merged_df_2024_WithBOM_Selected_RQ2 <- merged_df_2024_withBOM_RQ2 %>% select(DayMonthYear, Region, Daily_RRP, PV_Rooftop, Fossil, Hydro, Wind, Solar, MinTemp, MaxTemp, Sunshine, WindGustSpeed, Rainfall)
merged_df_2024_WithBOM_Selected_RQ2$DayMonthYear <- as.Date(merged_df_2024_WithBOM_Selected_RQ2$DayMonthYear, format = "%d %b %Y")
merged_df_2024_WithBOM_Selected_RQ2 <- merged_df_2024_WithBOM_Selected_RQ2 %>%
  filter(format(DayMonthYear, "%Y") == "2024")

#Remove Na values in df
merged_df_2024_WithBOM_Selected_RQ2 <- na.omit(merged_df_2024_WithBOM_Selected_RQ2)

#TESTING HERE
#Using IQR method for removing outlines for merged_df_2018_WithBOM_Selected

Q1 <- quantile(merged_df_2018_WithBOM_Selected_RQ2$Daily_RRP, 0.25, na.rm = TRUE)
Q3 <- quantile(merged_df_2018_WithBOM_Selected_RQ2$Daily_RRP, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val

#New df created with no outliers from IQR method
RQ2_2018_noOutliers <- merged_df_2018_WithBOM_Selected_RQ2[merged_df_2018_WithBOM_Selected_RQ2$Daily_RRP >= lower_bound & merged_df_2018_WithBOM_Selected_RQ2$Daily_RRP <= upper_bound, ]

#New df created for Correlation Analysis
RQ2_2018_correlation<- RQ2_2018_noOutliers


#Run Correlation tests for RQ2_2018 for RQ2:


#Run Correlation tests for RQ2_2018 with the following method:

# For Correlation, requires numerical variables for analysis
RQ2_2018_correlation <- RQ2_2018_correlation %>% select(where(is.numeric))


# Correlation Matrix to be computed 
cor_matrix_2018 <- cor(RQ2_2018_correlation)

# Correlation matrix to be rounded to 2 decimal places
print(round(cor_matrix_2018, 2))


# Heatmap for visualization purposes
corrplot(cor_matrix_2018, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")

sapply(RQ2_2018_correlation, function(x) sum(is.na(x)))
sapply(RQ2_2018_correlation, function(x) length(unique(x)))

#Another visualization for correlation
ggpairs(RQ2_2018_correlation,
        upper = list(continuous = wrap("cor", size = 4)),
        lower = list(continuous = wrap("smooth", alpha = 0.3, color = "blue")),
        diag = list(continuous = wrap("densityDiag")))  # or use "barDiag"


# Select numerical columns for dataframe
numeric_RQ2_2018 <- RQ2_2018_correlation[, sapply(RQ2_2018_correlation, is.numeric)]

# Inspect the structure of numerical columns for dataframe
str(numeric_RQ2_2018)
cor_matrix_2018_correlation <- cor(RQ2_2018_correlation)

# Print the correlation matrix
print(cor_matrix_2018_correlation)


#Using IQR method for removing outlines for merged_df_2024_WithBOM_Selected

Q1 <- quantile(merged_df_2024_WithBOM_Selected_RQ2$Daily_RRP, 0.25, na.rm = TRUE)
Q3 <- quantile(merged_df_2024_WithBOM_Selected_RQ2$Daily_RRP, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val

#New df created with no outliers from IQR method
RQ2_2024_noOutliers <- merged_df_2024_WithBOM_Selected_RQ2[merged_df_2024_WithBOM_Selected_RQ2$Daily_RRP >= lower_bound & merged_df_2024_WithBOM_Selected_RQ2$Daily_RRP <= upper_bound, ]


#New df created for Correlation Analysis
RQ2_2024_correlation<- RQ2_2024_noOutliers


#Run Correlation tests for RQ2_2018 with the following method:



# For Correlation, requires numerical variables for analysis
RQ2_2024_correlation <- RQ2_2024_correlation %>% select(where(is.numeric))

# Correlation Matrix to be computed 
cor_matrix_2024 <- cor(RQ2_2024_correlation)

# Correlation matrix to be rounded to 2 decimal places
print(round(cor_matrix_2024, 2))


# Heatmap for visualization purposes
corrplot(cor_matrix_2024, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")


sapply(RQ2_2024_correlation, function(x) sum(is.na(x)))
sapply(RQ2_2024_correlation, function(x) length(unique(x)))


#Another visualization for correlation
ggpairs(RQ2_2024_correlation,
        upper = list(continuous = wrap("cor", size = 4)),
        lower = list(continuous = wrap("smooth", alpha = 0.3, color = "blue")),
        diag = list(continuous = wrap("densityDiag")))  # or use "barDiag"

# Select numerical columns for dataframe
numeric_RQ2_2024 <- RQ2_2024_correlation[, sapply(RQ2_2024_correlation, is.numeric)]

# Inspect the structure of numerical columns for dataframe
str(numeric_RQ2_2024)
cor_matrix_2024_correlation <- cor(RQ2_2024_correlation)

# Print the correlation matrix
print(cor_matrix_2024_correlation)


#Linear Model Analysis RQ2
#Full model of 2018, with Stepwise to consider the ideal model based on AIC
ful_model_2018_RQ2 <- lm(Daily_RRP ~ (Hydro + Wind + Solar + PV_Rooftop + Region + MinTemp + MaxTemp + Sunshine + WindGustSpeed + Rainfall)^2, data = RQ2_2018_noOutliers)


stepwise_model_2018_RQ2 <- step(ful_model_2018_RQ2, direction = "both")


#WITH THE AIC, Stepwise to consider the best model 
stepwise_full_model_2018_RQ2.lm <- lm(Daily_RRP ~ Hydro + Wind + Solar + PV_Rooftop + Region + MinTemp + 
                                        MaxTemp + Sunshine + WindGustSpeed + Rainfall + Hydro:Wind + 
                                        Hydro:Solar + Hydro:Region + Hydro:MinTemp + Hydro:WindGustSpeed + 
                                        Hydro:Rainfall + Wind:PV_Rooftop + Wind:Region + Solar:Region + 
                                        Solar:MinTemp + Solar:Rainfall + PV_Rooftop:Region + PV_Rooftop:MinTemp + 
                                        PV_Rooftop:MaxTemp + PV_Rooftop:Sunshine + Region:MinTemp + 
                                        Region:Sunshine + Region:WindGustSpeed + MinTemp:MaxTemp + 
                                        MinTemp:WindGustSpeed + MinTemp:Rainfall + MaxTemp:Sunshine + 
                                        MaxTemp:WindGustSpeed + Sunshine:WindGustSpeed, data = RQ2_2018_noOutliers)
summary (stepwise_full_model_2018_RQ2.lm)
eta_squared(stepwise_full_model_2018_RQ2.lm, partial=FALSE)
#Adjusted R-squared:  0.461, p-value: < 2.2e-16




#Model of 2018 consisted of Solar, PV_Rooftop and relevant variables, with Steps wise to consider the ideal model based on AIC
model_Solar_2018_RQ2 <- lm(Daily_RRP ~ ( Solar + PV_Rooftop + Region + MinTemp + MaxTemp + Sunshine +  Rainfall)^2, data = RQ2_2018_noOutliers)

stepwise_model_Solar_2018_RQ2 <- step(model_Solar_2018_RQ2, direction = "both")


#WITH THE AIC, stepwise to consider the best model
stepwise_model_Solar_2018_RQ2.lm <- lm(Daily_RRP ~ Solar + PV_Rooftop + Region + MinTemp + MaxTemp + 
                                     Sunshine + Rainfall + Solar:MaxTemp + PV_Rooftop:MaxTemp + 
                                     PV_Rooftop:Sunshine + Region:MinTemp + Region:MaxTemp + Region:Sunshine + 
                                     Region:Rainfall + MinTemp:MaxTemp + MinTemp:Sunshine + MaxTemp:Sunshine + 
                                     MaxTemp:Rainfall + PV_Rooftop:MinTemp, data = RQ2_2018_noOutliers)
summary (stepwise_model_Solar_2018_RQ2.lm)
eta_squared(stepwise_model_Solar_2018_RQ2.lm, partial=FALSE)

#Adjusted R-squared:  0.2236, p-value: < 2.2e-16



#Model of 2018 consisted of Wind and relevant variables, with Stepwise to consider the ideal model based on AIC


model_Wind_2018_RQ2 <- lm(Daily_RRP ~ (Wind + Region + WindGustSpeed + Rainfall)^2, data = RQ2_2018_noOutliers)

stepwise_model_Wind_2018_RQ2 <- step(model_Wind_2018_RQ2, direction = "both")


#WITH THE AIC, stepwise to consider the best model
Stepwise_model_Wind_2018_RQ2.lm <- lm(Daily_RRP ~ Wind + Region + WindGustSpeed + Wind:Region + Wind:WindGustSpeed + 
                                    Region:WindGustSpeed, data = RQ2_2018_noOutliers)
summary (Stepwise_model_Wind_2018_RQ2.lm)
eta_squared(Stepwise_model_Wind_2018_RQ2.lm, partial=FALSE)


#Model of 2018 consisted of Hydro and relevant variables, with Stepwise to consider the ideal model based on AIC


model_Hydro_2018_RQ2 <- lm(Daily_RRP ~ (Hydro + Region + MaxTemp + MinTemp + WindGustSpeed + Rainfall)^2, data = RQ2_2018_noOutliers)

stepwise_model_Hydro_2018_RQ2 <- step(model_Hydro_2018_RQ2, direction = "both")


#WITH THE AIC, stepwise to consider the best model
Stepwise_model_Hydro_2018_RQ2.lm <- lm(Daily_RRP ~ Hydro + Region + MaxTemp + MinTemp + WindGustSpeed + 
                                         Hydro:Region + Hydro:MinTemp + Region:MaxTemp + Region:MinTemp + 
                                         Region:WindGustSpeed + MaxTemp:MinTemp + MaxTemp:WindGustSpeed + 
                                         MinTemp:WindGustSpeed
                                       , data = RQ2_2018_noOutliers)
summary (Stepwise_model_Hydro_2018_RQ2.lm)
eta_squared(Stepwise_model_Hydro_2018_RQ2.lm, partial=FALSE) #Adjusted R-squared:  0.3028, p-value: < 2.2e-16






#Full model of 2024, with Steps wise to consider the ideal model based on AIC

ful_model_2024_RQ2 <- lm(Daily_RRP ~ (Hydro + Wind + Solar + PV_Rooftop + Region + MinTemp + MaxTemp + Sunshine + WindGustSpeed + Rainfall)^2, data = RQ2_2024_noOutliers)


stepwise_model_2024_RQ2 <- step(ful_model_2024_RQ2, direction = "both")



#WITH THE AIC, stepwise to consider the best model
Stepwise_full_model_2024_RQ2.lm <- lm(Daily_RRP ~ Hydro + Wind + Solar + PV_Rooftop + Region + MinTemp + 
                                        MaxTemp + Sunshine + WindGustSpeed + Rainfall + Hydro:Region + 
                                        Hydro:MinTemp + Hydro:Sunshine + Hydro:WindGustSpeed + Wind:Solar + 
                                        Wind:PV_Rooftop + Wind:Region + Wind:Rainfall + Solar:PV_Rooftop + 
                                        Solar:Region + Solar:WindGustSpeed + PV_Rooftop:Region + 
                                        PV_Rooftop:MinTemp + PV_Rooftop:MaxTemp + PV_Rooftop:Sunshine + 
                                        PV_Rooftop:WindGustSpeed + Region:MaxTemp + Region:WindGustSpeed + 
                                        MinTemp:MaxTemp + MinTemp:Sunshine + MaxTemp:WindGustSpeed + 
                                        MaxTemp:Rainfall, data = RQ2_2024_noOutliers)
summary (Stepwise_full_model_2024_RQ2.lm)
eta_squared(Stepwise_full_model_2024_RQ2.lm, partial=FALSE)
#Adjusted R-squared:  0.5923, p-value: < 2.2e-16





#Model of 2024 consisted of Solar, PV_Rooftop and relevant variables, with Steps wise to consider the ideal model based on AIC


model_Solar_2024_RQ2  <- lm(Daily_RRP ~ ( Solar + PV_Rooftop + Region + MinTemp + MaxTemp + Sunshine +  Rainfall)^2, data = RQ2_2024_noOutliers)

stepwise_model_Solar_2024_RQ2  <- step(model_Solar_2024_RQ2, direction = "both")


#WITH THE AIC, stepwise to consider the best model
Stepwise_model_Solar_2024_RQ2.lm <- lm(Daily_RRP ~ Solar + PV_Rooftop + Region + MinTemp + MaxTemp + 
                                     Sunshine + Rainfall + Solar:MaxTemp + PV_Rooftop:MaxTemp + 
                                     PV_Rooftop:Sunshine + Region:MinTemp + Region:MaxTemp + Region:Sunshine + 
                                     Region:Rainfall + MinTemp:MaxTemp + MinTemp:Sunshine + MaxTemp:Sunshine + 
                                     MaxTemp:Rainfall + PV_Rooftop:MinTemp, data = RQ2_2024_noOutliers)
summary (Stepwise_model_Solar_2024_RQ2.lm)
eta_squared(Stepwise_model_Solar_2024_RQ2.lm, partial=TRUE)

#Adjusted R-squared:  0.329, p-value: < 2.2e-16



#Model of 2024 consisted of Wind and relevant variables, with Steps wise to consider the ideal model based on AIC

model_Wind_2024_RQ2 <- lm(Daily_RRP ~ (Wind + Region + WindGustSpeed + Rainfall)^2, data = RQ2_2024_noOutliers)

stepwise_model_Wind_2024_RQ2 <- step(model_Solar_2024_RQ2, direction = "both")


#WITH THE AIC, stepwise to consider the best model 
Stepwise_model_Wind_2024_RQ2.lm <- lm(Daily_RRP ~ Wind + Region + WindGustSpeed + Rainfall + Wind:Region + 
                                    Wind:WindGustSpeed + Region:WindGustSpeed, data = RQ2_2024_noOutliers)
summary (Stepwise_model_Wind_2024_RQ2.lm)
eta_squared(Stepwise_model_Wind_2024_RQ2.lm, partial=TRUE)

#Adjusted R-squared:  0.2956, p-value: < 2.2e-16


model_Hydro_2024_RQ2 <- lm(Daily_RRP ~ (Hydro + Region + MaxTemp + MinTemp + WindGustSpeed + Rainfall)^2, data = RQ2_2024_noOutliers)

stepwise_model_Hydro_2024_RQ2 <- step(model_Hydro_2024_RQ2, direction = "both")


#WITH THE AIC, stepwise to consider the best model
Stepwise_model_Hydro_2024_RQ2.lm <- lm(Daily_RRP ~ Hydro + Region + MaxTemp + MinTemp + WindGustSpeed + 
                                         Hydro:Region + Hydro:MinTemp + Region:MaxTemp + Region:MinTemp + 
                                         Region:WindGustSpeed + MaxTemp:MinTemp + MaxTemp:WindGustSpeed + 
                                         MinTemp:WindGustSpeed
                                       , data = RQ2_2024_noOutliers)
summary (Stepwise_model_Hydro_2024_RQ2.lm)
eta_squared(Stepwise_model_Hydro_2024_RQ2.lm, partial=FALSE) #Adjusted R-squared:  0.4953, p-value: < 2.2e-16


#RQ3
# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data1 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_201801010000.CSV", header = FALSE)
CO2df1 <-CO2data1
colnames(CO2df1) <- c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df1 <- CO2df1[-c(1, 2), ]
CO2df1 <- CO2df1[-nrow(CO2df1), ]


# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data2 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_201802010000.CSV", header = FALSE)
CO2df2 <-CO2data2
colnames(CO2df2) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df2 <- CO2df2[-c(1, 2), ]
CO2df2 <- CO2df2[-nrow(CO2df2), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data3 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_201803010000.CSV", header = FALSE)
CO2df3 <-CO2data3
colnames(CO2df3) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df3 <- CO2df3[-c(1, 2), ]
CO2df3 <- CO2df3[-nrow(CO2df3), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data4 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_201804010000.CSV", header = FALSE)
CO2df4 <-CO2data4
colnames(CO2df4) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df4 <- CO2df4[-c(1, 2), ]
CO2df4 <- CO2df4[-nrow(CO2df4), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data5 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_201805010000.CSV", header = FALSE)
CO2df5 <-CO2data5
colnames(CO2df5) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df5 <- CO2df5[-c(1, 2), ]
CO2df5 <- CO2df5[-nrow(CO2df5), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data6 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_201806010000.CSV", header = FALSE)
CO2df6 <-CO2data6
colnames(CO2df6) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df6 <- CO2df6[-c(1, 2), ]
CO2df6 <- CO2df6[-nrow(CO2df6), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data7 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_201807010000.CSV", header = FALSE)
CO2df7 <-CO2data7
colnames(CO2df7) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df7 <- CO2df7[-c(1, 2), ]
CO2df7 <- CO2df7[-nrow(CO2df7), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data8 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_201808010000.CSV", header = FALSE)
CO2df8 <-CO2data8
colnames(CO2df8) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df8 <- CO2df8[-c(1, 2), ]
CO2df8 <- CO2df8[-nrow(CO2df8), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data9 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_201809010000.CSV", header = FALSE)
CO2df9 <-CO2data9
colnames(CO2df9) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df9 <- CO2df9[-c(1, 2), ]
CO2df9 <- CO2df9[-nrow(CO2df9), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data10 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_201810010000.CSV", header = FALSE)
CO2df10 <-CO2data10
colnames(CO2df10) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df10 <- CO2df10[-c(1, 2), ]
CO2df10 <- CO2df10[-nrow(CO2df10), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data11 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_201811010000.CSV", header = FALSE)
CO2df11 <-CO2data11
colnames(CO2df11) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df11 <- CO2df11[-c(1, 2), ]
CO2df11 <- CO2df11[-nrow(CO2df11), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data12 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_201812010000.CSV", header = FALSE)
CO2df12 <-CO2data12
colnames(CO2df12) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df12 <- CO2df12[-c(1, 2), ]
CO2df12 <- CO2df12[-nrow(CO2df12), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data13 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_201901010000.CSV", header = FALSE)
CO2df13 <-CO2data13
colnames(CO2df13) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df13 <- CO2df13[-c(1, 2), ]
CO2df13 <- CO2df13[-nrow(CO2df13), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data14 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_201902010000.CSV", header = FALSE)
CO2df14 <-CO2data14
colnames(CO2df14) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df14 <- CO2df14[-c(1, 2), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysisCO2df14 <- CO2df14[-nrow(CO2df14), ]
CO2data15 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_201903010000.CSV", header = FALSE)
CO2df15 <-CO2data15
colnames(CO2df15) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df15 <- CO2df15[-c(1, 2), ]
CO2df15 <- CO2df15[-nrow(CO2df15), ]


# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data16 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_201904010000.CSV", header = FALSE)
CO2df16 <-CO2data16
colnames(CO2df16) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df16 <- CO2df16[-c(1, 2), ]
CO2df16 <- CO2df16[-nrow(CO2df16), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data17 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_201905010000.CSV", header = FALSE)
CO2df17 <-CO2data17
colnames(CO2df17) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df17 <- CO2df17[-c(1, 2), ]
CO2df17 <- CO2df17[-nrow(CO2df17), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data18 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_201906010000.CSV", header = FALSE)
CO2df18 <-CO2data18
colnames(CO2df18) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df18 <- CO2df18[-c(1, 2), ]
CO2df18 <- CO2df18[-nrow(CO2df18), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data19 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_201907010000.CSV", header = FALSE)
CO2df19 <-CO2data19
colnames(CO2df19) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df19 <- CO2df19[-c(1, 2), ]
CO2df19 <- CO2df19[-nrow(CO2df19), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data20 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_201908010000.CSV", header = FALSE)
CO2df20 <-CO2data20
colnames(CO2df20) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df20 <- CO2df20[-c(1, 2), ]
CO2df20 <- CO2df20[-nrow(CO2df20), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data21 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_201909010000.CSV", header = FALSE)
CO2df21 <-CO2data21
colnames(CO2df21) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df21 <- CO2df21[-c(1, 2), ]
CO2df21 <- CO2df21[-nrow(CO2df21), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data22 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_201910010000.CSV", header = FALSE)
CO2df22 <-CO2data22
colnames(CO2df22) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df22 <- CO2df22[-c(1, 2), ]
CO2df22 <- CO2df22[-nrow(CO2df22), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data23 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_201911010000.CSV", header = FALSE)
CO2df23 <-CO2data23
colnames(CO2df23) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df23 <- CO2df23[-c(1, 2), ]
CO2df23 <- CO2df23[-nrow(CO2df23), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data24 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_201912010000.CSV", header = FALSE)
CO2df24 <-CO2data24
colnames(CO2df24) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df24 <- CO2df24[-c(1, 2), ]
CO2df24 <- CO2df24[-nrow(CO2df24), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data25 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_202001010000.CSV", header = FALSE)
CO2df25 <-CO2data25
colnames(CO2df25) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df25 <- CO2df25[-c(1, 2), ]
CO2df25 <- CO2df25[-nrow(CO2df25), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data26 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_202002010000.CSV", header = FALSE)
CO2df26 <-CO2data26
colnames(CO2df26) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df26 <- CO2df26[-c(1, 2), ]
CO2df26 <- CO2df26[-nrow(CO2df26), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data27 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_202003010000.CSV", header = FALSE)
CO2df27 <-CO2data27
colnames(CO2df27) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df27 <- CO2df27[-c(1, 2), ]
CO2df27 <- CO2df27[-nrow(CO2df27), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data28 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_202004010000.CSV", header = FALSE)
CO2df28 <-CO2data28
colnames(CO2df28) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df28 <- CO2df28[-c(1, 2), ]
CO2df28 <- CO2df28[-nrow(CO2df28), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data29 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_202005010000.CSV", header = FALSE)
CO2df29 <-CO2data29
colnames(CO2df29) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df29 <- CO2df29[-c(1, 2), ]
CO2df29 <- CO2df29[-nrow(CO2df29), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data30 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_202006010000.CSV", header = FALSE)
CO2df30 <-CO2data30
colnames(CO2df30) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df30 <- CO2df30[-c(1, 2), ]
CO2df30 <- CO2df30[-nrow(CO2df30), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data31 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_202007010000.CSV", header = FALSE)
CO2df31 <-CO2data31
colnames(CO2df31) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df31 <- CO2df31[-c(1, 2), ]
CO2df31 <- CO2df31[-nrow(CO2df31), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data32 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_202008010000.CSV", header = FALSE)
CO2df32 <-CO2data32
colnames(CO2df32) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df32 <- CO2df32[-c(1, 2), ]
CO2df32 <- CO2df32[-nrow(CO2df32), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data33 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_202009010000.CSV", header = FALSE)
CO2df33 <-CO2data33
colnames(CO2df33) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df33 <- CO2df33[-c(1, 2), ]
CO2df33 <- CO2df33[-nrow(CO2df33), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data34 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_202010010000.CSV", header = FALSE)
CO2df34 <-CO2data34
colnames(CO2df34) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df34 <- CO2df34[-c(1, 2), ]
CO2df34 <- CO2df34[-nrow(CO2df34), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data35 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_202011010000.CSV", header = FALSE)
CO2df35 <-CO2data35
colnames(CO2df35) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df35 <- CO2df35[-c(1, 2), ]
CO2df35 <- CO2df35[-nrow(CO2df35), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data36 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_202012010000.CSV", header = FALSE)
CO2df36 <-CO2data36
colnames(CO2df36) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df36 <- CO2df36[-c(1, 2), ]
CO2df36 <- CO2df36[-nrow(CO2df36), ]


# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data37 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_202106010000.CSV", header = FALSE)
CO2df37<-CO2data37
colnames(CO2df37) <- c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df37 <- CO2df37[-c(1, 2), ]
CO2df37 <- CO2df37[-nrow(CO2df37), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data38 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_202112010000.CSV", header = FALSE)
CO2df38 <-CO2data38
colnames(CO2df38) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df38 <- CO2df38[-c(1, 2), ]
CO2df38 <- CO2df38[-nrow(CO2df38), ]


# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data39 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_202207010000.CSV", header = FALSE)
CO2df39 <-CO2data39
colnames(CO2df39) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df39 <- CO2df39[-c(1, 2), ]
CO2df39 <- CO2df39[-nrow(CO2df39), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data40 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_202302010000.CSV", header = FALSE)
CO2df40 <-CO2data40
colnames(CO2df40) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df40 <- CO2df40[-c(1, 2), ]
CO2df40 <- CO2df40[-nrow(CO2df40), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data41 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_202309010000.CSV", header = FALSE)
CO2df41 <-CO2data41
colnames(CO2df41) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df41 <- CO2df41[-c(1, 2), ]
CO2df41 <- CO2df41[-nrow(CO2df41), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data42 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_202404010000.CSV", header = FALSE)
CO2df42 <-CO2data42
colnames(CO2df42) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df42 <- CO2df42[-c(1, 2), ]
CO2df42 <- CO2df42[-nrow(CO2df42), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data43 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_202407010000.CSV", header = FALSE)
CO2df43 <-CO2data43
colnames(CO2df43) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df43 <- CO2df43[-c(1, 2), ]
CO2df43 <- CO2df43[-nrow(CO2df43), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data44 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_202408010000.CSV", header = FALSE)
CO2df44 <-CO2data44
colnames(CO2df44) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df44 <- CO2df44[-c(1, 2), ]
CO2df44 <- CO2df44[-nrow(CO2df44), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data45 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_202409010000.CSV", header = FALSE)
CO2df45 <-CO2data45
colnames(CO2df45) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df45 <- CO2df45[-c(1, 2), ]
CO2df45 <- CO2df45[-nrow(CO2df45), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data46 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_202410010000.CSV", header = FALSE)
CO2df46 <-CO2data46
colnames(CO2df46) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df46 <- CO2df46[-c(1, 2), ]
CO2df46 <- CO2df46[-nrow(CO2df46), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data47 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_202411010000.CSV", header = FALSE)
CO2df47 <-CO2data47
colnames(CO2df47) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df47 <- CO2df47[-c(1, 2), ]
CO2df47 <- CO2df47[-nrow(CO2df47), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data48 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_202412010000.CSV", header = FALSE)
CO2df48 <-CO2data48
colnames(CO2df48) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df48 <- CO2df48[-c(1, 2), ]
CO2df48 <- CO2df48[-nrow(CO2df48), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data49 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_202501010000.CSV", header = FALSE)
CO2df49 <-CO2data49
colnames(CO2df49) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df49 <- CO2df49[-c(1, 2), ]
CO2df49 <- CO2df49[-nrow(CO2df49), ]

# Load CO2 Data separately, manually rename the columns with the right headings, delete rows that are not relevant for analysis
CO2data50 <- read.csv("PUBLIC_DVD_BILLING_CO2E_PUBLICATION_202502010000.CSV", header = FALSE)
CO2df50 <-CO2data50
colnames(CO2df50) <-  c("C", "SETP.WORLD", "DVD_BILLING_COE2_PUBLICATION", "AEMO", "CONTRACTYEAR", "WEEKNO", "BILLRUNNO","SETTLEMENTDATE", "REGION", "SENTOUTENERGY", "GENERATOREMISSIONS", "INTENSITYINDEX")
CO2df50 <- CO2df50[-c(1, 2), ]
CO2df50 <- CO2df50[-nrow(CO2df50), ]



# Combine all the dfs together in one CO2_df_combined from 2018 to 2025
CO2_df_combined = bind_rows(CO2df1, CO2df2, CO2df3, CO2df4, CO2df5, CO2df6, CO2df7, CO2df8, CO2df9, CO2df10, CO2df11, CO2df12,
                            CO2df13, CO2df14, CO2df15, CO2df16, CO2df17, CO2df18, CO2df19, CO2df20, CO2df21, CO2df22, CO2df23,
                            CO2df24, CO2df25, CO2df26, CO2df27, CO2df28, CO2df29, CO2df30, CO2df31, CO2df32, CO2df33, CO2df34, 
                            CO2df35, CO2df36, CO2df37, CO2df38, CO2df39, CO2df40, CO2df41, CO2df42, CO2df43, CO2df44, CO2df45,
                            CO2df46, CO2df47, CO2df48, CO2df49, CO2df50)



# Remove duplicate rows based on GENERATOREMISSIONS and SETTLMENTDATE
CO2_df_combined <- CO2_df_combined[!duplicated(CO2_df_combined[c("GENERATOREMISSIONS", "SETTLEMENTDATE")]), ]


#Convert SETTLEMENTDATE column as a POSIXct in the dataframe
CO2_df_combined <- CO2_df_combined %>%
  mutate(
    # Convert the datetime_column to Date or POSIXct if it's not already
    SETTLEMENTDATE = as.POSIXct(SETTLEMENTDATE, format = "%Y/%m/%d %H:%M:%S"),
    
  )

# remove rows that have na in the dataframe
CO2_df_combined<- na.omit(CO2_df_combined)

# Convert these three columns to numeric
CO2_df_combined$INTENSITYINDEX <- as.numeric(CO2_df_combined$INTENSITYINDEX)
CO2_df_combined$GENERATOREMISSIONS <- as.numeric(CO2_df_combined$GENERATOREMISSIONS)
CO2_df_combined$SENTOUTENERGY <- as.numeric(CO2_df_combined$SENTOUTENERGY)

# Convert SETTLEMENTDATE to Date format  
CO2_df_combined$SETTLEMENTDATE <- as.Date(CO2_df_combined$SETTLEMENTDATE)

#Check to confirm changes made in CO2_df_combined)
str(CO2_df_combined) 
summary(CO2_df_combined)


#  Create a new df that chooses only numeric columns, used for corelation
numeric_CO2_df_combined <- CO2_df_combined[, sapply(CO2_df_combined, is.numeric)]

# Compute the correlation matrix
cor_matrix <- cor(numeric_CO2_df_combined)

# View correlation matrix rounded to 2 decimal places
print(round(cor_matrix, 2))

#There are strong relationship between GENERATOREMISSIONS & SENTOUTENERGY
# and weaker with INTENSITYINDEX being(0.57,0.46) respectively


# Create a linear model with (GENERATOREMISSIONS ~ SENTOUTENERGY + INTENSITYINDEX + REGION) and eta_squared

CO2_region.lm <- lm(GENERATOREMISSIONS ~ SENTOUTENERGY + INTENSITYINDEX + REGION, data = CO2_df_combined)
summary (CO2_region.lm)
eta_squared(CO2_region.lm, partial=FALSE)



## When executing the code as above to get the Tabble for "Effect Siz for Anova" , our model attributes most of the generation of energy
#for predicting Average RRP to the variable 'Combustion' which improves the the models's R^2 by 23%. Renewabele gets average
#RRP for predicitng less than 0% of the variage in ____, and region with 18%. 

#region_renewable.lm <- lm(AVGRRP ~ SENTOUTENERGY + INTENSITYINDEX + REGIONegion, data = merged_df_df3_combined)
#summary (region_renewable.lm)
#eta_squared(region_renewable.lm, partial=FALSE)

## write comments for region_renewable here

#region_Combustion.lm <- lm(AVGRRP ~ Combustion + Region, data = merged_df_df3_combined)
#summary (region_Combustion.lm)
#eta_squared(region_Combustion.lm, partial=FALSE)
#model_parameters(region_Combustion.lm)   

## write comments for region_Combustion here



# Using ggplot to create a line plot with SETTLEMENTDATE AND GENERATOREMISSIONS
ggplot(CO2_df_combined, aes(x = SETTLEMENTDATE, y = GENERATOREMISSIONS, color = REGION, group = REGION)) +
  geom_line() +
  labs(title = "Generation Emissions by Region",
       x = "Date",
       y = "Generation Emissions") +
  geom_smooth(method = "lm", se = FALSE,        # Add regression line for linear model
              size = 2,                        # Mak regression line more visible 
              linetype = "solid",             # Make the line solid
              alpha = 0.8) +  
  scale_x_date(labels = scales::date_format("%d-%b-%Y"),  
               breaks = scales::date_breaks("1 month")) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))






#Create a new dataframe for NEM, check for missing values, change CONTRACTYEAR to YEAR, and select relevant columns
CO2_NEM <- CO2_df_combined %>% filter(REGION == "NEM")
sum(is.na(CO2_NEM)) #Check for Missing values for NEM 
names(CO2_NEM)[names(CO2_NEM) == "CONTRACTYEAR"] <- "YEAR"
CO2_NEM <- CO2_NEM %>% select(YEAR, SETTLEMENTDATE, GENERATOREMISSIONS)



#Create a new dataframe for NSW, check for missing values, change CONTRACTYEAR to YEAR, and select relevant columns
CO2_nsw <- CO2_df_combined %>% filter(REGION == "NSW1")
sum(is.na(CO2_nsw)) #Check for Missing values for NSW 
names(CO2_nsw)[names(CO2_nsw) == "CONTRACTYEAR"] <- "YEAR"
CO2_nsw <- CO2_nsw %>% select(YEAR, SETTLEMENTDATE, GENERATOREMISSIONS)

#Create a new dataframe for QLD, check for missing values, change CONTRACTYEAR to YEAR, and select relevant columns
CO2_qld <- CO2_df_combined %>% filter(REGION == "QLD1")
sum(is.na(CO2_qld)) #Check for Missing values for QLD
names(CO2_qld)[names(CO2_qld) == "CONTRACTYEAR"] <- "YEAR"
CO2_qld <- CO2_qld %>% select(YEAR, SETTLEMENTDATE, GENERATOREMISSIONS)


#Create a new dataframe for SA, check for missing values, change CONTRACTYEAR to YEAR, and select relevant columns
CO2_sa <- CO2_df_combined %>% filter(REGION == "SA1")
sum(is.na(CO2_sa)) #Check for Missing values for SA
names(CO2_sa)[names(CO2_sa) == "CONTRACTYEAR"] <- "YEAR"
CO2_sa <- CO2_sa %>% select(YEAR, SETTLEMENTDATE, GENERATOREMISSIONS)



#Create a new dataframe for TAS, check for missing values, change CONTRACTYEAR to YEAR, and select relevant columns
CO2_tas <- CO2_df_combined %>% filter(REGION == "TAS1")
sum(is.na(CO2_tas)) #Check for Missing values for TAS
names(CO2_tas)[names(CO2_tas) == "CONTRACTYEAR"] <- "YEAR"
CO2_tas <- CO2_tas %>% select(YEAR, SETTLEMENTDATE, GENERATOREMISSIONS)


#Create a new dataframe for VIC, check for missing values, change CONTRACTYEAR to YEAR, and select relevant columns
CO2_vic <- CO2_df_combined %>% filter(REGION == "VIC1")
sum(is.na(CO2_vic)) #Check for Missing values for Vic
names(CO2_vic)[names(CO2_vic) == "CONTRACTYEAR"] <- "YEAR"
CO2_vic <- CO2_vic %>% select(YEAR, SETTLEMENTDATE, GENERATOREMISSIONS)



# Predictive Modelling with Machine learning
# Using Predictive Modelling for CO2_NEM


# Ensure SETTLEMENTDATE is converted in a Date
CO2_NEM$SETTLEMENTDATE <- as.Date(CO2_NEM$SETTLEMENTDATE)

# Orign date where the first date started 15.12.2017
origin_date <- as.Date("2017-12-15")

# Calculate SETTLEMENDDATE - origin_date
CO2_NEM$days_from_origin <- as.numeric(CO2_NEM$SETTLEMENTDATE - origin_date)

# Summary statistics
summary(CO2_NEM)

CO2_NEM <- na.omit(CO2_NEM)

set.seed(123)  # to ensure predictive modelling can be reproduced

# Split the data into 80% training and 20% testing
train_index <- createDataPartition(CO2_NEM$GENERATOREMISSIONS, p = 0.8, list = FALSE)
train_data <- CO2_NEM[train_index, ]
test_data <- CO2_NEM[-train_index, ]

# Train model
model <- train(
  GENERATOREMISSIONS ~ days_from_origin,
  data = train_data,
  method = "lm",  
  trControl = trainControl(method = "cv", number = 5)  
)

# View model summary
print(model)

# Make the predictions
predictions <- predict(model, newdata = test_data)

# Calculate the results
results <- data.frame(
  Actual = test_data$GENERATOREMISSIONS,
  Predicted = predictions
)

# Calculate the RMSE and R-squared with the results generated
rmse <- sqrt(mean((results$Actual - results$Predicted)^2))

print(paste("RMSE:", rmse))

# Plot model for actual vs predicted
plot(test_data$GENERATOREMISSIONS, predictions, main = "CO2_Emissions of NEM",
     xlab = "Actual Generator Emissions", ylab = "Predicted Generator Emissions")
abline(0, 1, col = "red")  # Add diagonal line for reference




# Extract model for NEM coefficients
coefficients <- coef(model$finalModel)
print(coefficients)


# Extract coefficients from NEM model
coefs <- coef(model$finalModel)
intercept <- coefs[1]  
SETTLEMENT_coef <- coefs["SETTLEMENTDATE"]


# Convert the SETTLEMENTDATE column to Date
CO2_NEM$SETTLMENTDATE <- as.Date(CO2_NEM$SETTLEMENTDATE)

# Origin_date starting on 16.12.2017 from the data
origin_date <- as.Date("2017-12-16")

# Add a new column created by SETTLEMENTDATE - origin_date
CO2_NEM$days_from_origin <- as.numeric(CO2_NEM$SETTLEMENTDATE - origin_date)

# View dataframe
print(CO2_NEM)


# Create predicted value from model
CO2_NEM$GENERATOREMISSIONS_new <- 410776.07338 - 43.99975 * CO2_NEM$days_from_origin

zero_day <- (410776.07338) / 43.99975 

# Extend the range of days 
extended_days <- seq(min(CO2_NEM$days_from_origin), zero_day + 500, by = 1)  

# Calculate predicted values for extended
extended_predictions <- 410776.07338 - 43.99975 * extended_days

# Calculate extended y-axis limits
y_min <- 0  
y_max <- max(c(CO2_NEM$GENERATOREMISSIONS_new, extended_predictions)) + 100000 

# Define x-axis limits by adding more space
xlim_min <- min(extended_days) - 500 
xlim_max <- max(extended_days) + 500 

# Convert days_from_origin to actual dates for the x-axis
dates_from_origin <- origin_date + CO2_NEM$days_from_origin
extended_dates <- origin_date + extended_days

# Plot actual data (actual CO2 generation) in blue
plot(dates_from_origin, CO2_NEM$GENERATOREMISSIONS, type = "l", col = "blue", lwd = 2, 
     xlab = "Date", ylab = "CO2 Generation", main = "Australia's Emission projections in  NEM",
     xlim = c(min(extended_dates), max(extended_dates)), ylim = c(y_min, y_max)) 

# Add the predicted line in red for both the original data and the extended prediction
lines(dates_from_origin, CO2_NEM$GENERATOREMISSIONS_new, col = "red", lwd = 3)
lines(extended_dates, extended_predictions, col = "red", lwd = 3, lty = 2)  

points(dates_from_origin, CO2_NEM$GENERATOREMISSIONS_new, col = "red", pch = 16)

# X axis with dates instead of Days from Origin`
axis(1, at = seq(min(extended_dates), max(extended_dates), by = "6 months"), 
     labels = format(seq(min(extended_dates), max(extended_dates), by = "6 months"), "%b %Y"), 
     las = 2)  # las = 2 for vertical labels

# Horizontal line in green to determine when projected line hits zerio (i.e., Net Zero tarrget)
abline(h = 0, col = "green", lwd = 2, lty = 2)

# Add a legend to the plot
legend("topright", legend = c("Actual", "Predicted (Current)", "Predicted (Extended)"), 
       col = c("blue", "red", "red"), lwd = 2, lty = c(1, 1, 2), pch = c(NA, 16, NA))





# Using Predictive Modelling for CO2_NSW
# Ensure SETTLEMENTDATE is converted in a Date
CO2_nsw$SETTLEMENTDATE <- as.Date(CO2_nsw$SETTLEMENTDATE)

# Orign date where the first date started 15.12.2017
origin_date <- as.Date("2017-12-15")

# Calculate SETTLEMENDDATE - origin_date
CO2_nsw$days_from_origin <- as.numeric(CO2_nsw$SETTLEMENTDATE - origin_date)

# Summary statistics
summary(CO2_nsw)

CO2_nsw <- na.omit(CO2_nsw)

set.seed(123)  # to ensure predictive model can be reproduced

# Split the data into 80% training and 20% testing
train_index <- createDataPartition(CO2_nsw$GENERATOREMISSIONS, p = 0.8, list = FALSE)
train_data <- CO2_nsw[train_index, ]
test_data <- CO2_nsw[-train_index, ]

# Train model_nsw
model_nsw <- train(
  GENERATOREMISSIONS ~ days_from_origin,
  data = train_data,
  method = "lm",  
  trControl = trainControl(method = "cv", number = 5)  
)

# View model_nsw summary
print(model_nsw)

# Make the predictions
predictions <- predict(model_nsw, newdata = test_data)

# Calculate the results
results <- data.frame(
  Actual = test_data$GENERATOREMISSIONS,
  Predicted = predictions
)

# Calculate the RMSE and R-squared with the results generated
rmse <- sqrt(mean((results$Actual - results$Predicted)^2))

print(paste("RMSE:", rmse))

# Plot model_nsw for actual vs predicted
plot(test_data$GENERATOREMISSIONS, predictions, main = "Australia's Emission projections in  New South Wales",
     xlab = "Actual Generator Emissions", ylab = "Predicted Generator Emissions")
abline(0, 1, col = "red")  # Add diagonal line for reference




# Extract model_nsw for NSW coefficients
coefficients <- coef(model_nsw$finalModel)
print(coefficients)


# Extract coefficients from NSW model_nsw
coefs <- coef(model_nsw$finalmodelModel)
intercept <- coefs[1]  
SETTLEMENT_coef <- coefs["SETTLEMENTDATE"]


# Convert the SETTLEMENTDATE column to Date
CO2_nsw$SETTLMENTDATE <- as.Date(CO2_nsw$SETTLEMENTDATE)

# Origin_date starting on 16.12.2017 from the data
origin_date <- as.Date("2017-12-16")

# Add a new column created by SETTLEMENTDATE - origin_date
CO2_nsw$days_from_origin <- as.numeric(CO2_nsw$SETTLEMENTDATE - origin_date)

# View dataframe
print(CO2_nsw)


# Create predicted value from model_nsw
CO2_nsw$GENERATOREMISSIONS_new <- 147548.55248 - 17.01236 * CO2_nsw$days_from_origin

zero_day <- (147548.55248) / 17.01236 

# Extend the range of days 
extended_days <- seq(min(CO2_nsw$days_from_origin), zero_day + 500, by = 1)  

# Calculate predicted values for extended
extended_predictions <- 147548.55248 -17.01236 * extended_days

# Calculate extended y-axis limits
y_min <- 0  
y_max <- max(c(CO2_nsw$GENERATOREMISSIONS_new, extended_predictions)) + 100000 

# Define x-axis limits by adding more space
xlim_min <- min(extended_days) - 500 
xlim_max <- max(extended_days) + 500 

# Convert days_from_origin to actual dates for the x-axis
dates_from_origin <- origin_date + CO2_nsw$days_from_origin
extended_dates <- origin_date + extended_days

# Plot actual data (actual CO2 generation) in blue
plot(dates_from_origin, CO2_nsw$GENERATOREMISSIONS, type = "l", col = "blue", lwd = 2, 
     xlab = "Date", ylab = "CO2 Generation", main = "Australia's Emission projections in  NSW",
     xlim = c(min(extended_dates), max(extended_dates)), ylim = c(y_min, y_max)) 

# Add the predicted line in red for both the original data and the extended prediction
lines(dates_from_origin, CO2_nsw$GENERATOREMISSIONS_new, col = "red", lwd = 3)
lines(extended_dates, extended_predictions, col = "red", lwd = 3, lty = 2)  

points(dates_from_origin, CO2_nsw$GENERATOREMISSIONS_new, col = "red", pch = 16)

# X axis with dates instead of Days from Origin`
axis(1, at = seq(min(extended_dates), max(extended_dates), by = "6 months"), 
     labels = format(seq(min(extended_dates), max(extended_dates), by = "6 months"), "%b %Y"), 
     las = 2)  # las = 2 for vertical labels

# Horizontal line in green to determine when projected line hits zerio (i.e., Net Zero tarrget)
abline(h = 0, col = "green", lwd = 2, lty = 2)

# Add a legend to the plot
legend("topright", legend = c("Actual", "Predicted (Current)", "Predicted (Extended)"), 
       col = c("blue", "red", "red"), lwd = 2, lty = c(1, 1, 2), pch = c(NA, 16, NA))



# Using Predictive Modelling for CO2_qld


# Ensure SETTLEMENTDATE is converted in a Date
CO2_qld$SETTLEMENTDATE <- as.Date(CO2_qld$SETTLEMENTDATE)

# Orign date where the first date started 15.12.2017
origin_date <- as.Date("2017-12-15")

# Calculate SETTLEMENDDATE - origin_date
CO2_qld$days_from_origin <- as.numeric(CO2_qld$SETTLEMENTDATE - origin_date)

# Summary statistics
summary(CO2_qld)

CO2_qld <- na.omit(CO2_qld)

set.seed(123)  # to ensure predictive model can be reproduced

# Split the data into 80% training and 20% testing
train_index <- createDataPartition(CO2_qld$GENERATOREMISSIONS, p = 0.8, list = FALSE)
train_data <- CO2_qld[train_index, ]
test_data <- CO2_qld[-train_index, ]

# Train model_qld
model_qld <- train(
  GENERATOREMISSIONS ~ days_from_origin,
  data = train_data,
  method = "lm",  
  trControl = trainControl(method = "cv", number = 5)  
)

# View model_qld summary
print(model_qld)

# Make the predictions
predictions <- predict(model_qld, newdata = test_data)

# Calculate the results
results <- data.frame(
  Actual = test_data$GENERATOREMISSIONS,
  Predicted = predictions
)

# Calculate the RMSE and R-squared with the results generated
rmse <- sqrt(mean((results$Actual - results$Predicted)^2))

print(paste("RMSE:", rmse))

# Plot model_qld for actual vs predicted
plot(test_data$GENERATOREMISSIONS, predictions, main = "Actual vs Predicted in Queensland",
     xlab = "Actual Generator Emissions", ylab = "Predicted Generator Emissions")
abline(0, 1, col = "red")  # Add diagonal line for reference




# Extract model_qld for QLD coefficients
coefficients <- coef(model_qld$finalModel)
print(coefficients)


# Extract coefficients from Qld model_qld
coefs <- coef(model_qld$finalmodelModel)
intercept <- coefs[1]  
SETTLEMENT_coef <- coefs["SETTLEMENTDATE"]


# Convert the SETTLEMENTDATE column to Date
CO2_qld$SETTLMENTDATE <- as.Date(CO2_qld$SETTLEMENTDATE)

# Origin_date starting on 16.12.2017 from the data
origin_date <- as.Date("2017-12-16")

# Add a new column created by SETTLEMENTDATE - origin_date
CO2_qld$days_from_origin <- as.numeric(CO2_qld$SETTLEMENTDATE - origin_date)

# View dataframe
print(CO2_qld)


# Create predicted value from model_qld
CO2_qld$GENERATOREMISSIONS_new <- 135137.55523 - 14.61689* CO2_qld$days_from_origin

zero_day <- (135137.55523) / 14.61689

# Extend the range of days 
extended_days <- seq(min(CO2_qld$days_from_origin), zero_day + 500, by = 1)  

# Calculate predicted values for extended
extended_predictions <- 135137.55523 -14.61689* extended_days

# Calculate extended y-axis limits
y_min <- 0  
y_max <- max(c(CO2_qld$GENERATOREMISSIONS_new, extended_predictions)) + 100000 

# Define x-axis limits by adding more space
xlim_min <- min(extended_days) - 500 
xlim_max <- max(extended_days) + 500 

# Convert days_from_origin to actual dates for the x-axis
dates_from_origin <- origin_date + CO2_qld$days_from_origin
extended_dates <- origin_date + extended_days

# Plot actual data (actual CO2 generation) in blue
plot(dates_from_origin, CO2_qld$GENERATOREMISSIONS, type = "l", col = "blue", lwd = 2, 
     xlab = "Date", ylab = "CO2 Generation", main = "Australia's Emission projections in  Queensland",
     xlim = c(min(extended_dates), max(extended_dates)), ylim = c(y_min, y_max)) 

# Add the predicted line in red for both the original data and the extended prediction
lines(dates_from_origin, CO2_qld$GENERATOREMISSIONS_new, col = "red", lwd = 3)
lines(extended_dates, extended_predictions, col = "red", lwd = 3, lty = 2)  

points(dates_from_origin, CO2_qld$GENERATOREMISSIONS_new, col = "red", pch = 16)

# X axis with dates instead of Days from Origin`
axis(1, at = seq(min(extended_dates), max(extended_dates), by = "6 months"), 
     labels = format(seq(min(extended_dates), max(extended_dates), by = "6 months"), "%b %Y"), 
     las = 2)  # las = 2 for vertical labels

# Horizontal line in green to determine when projected line hits zerio (i.e., Net Zero tarrget)
abline(h = 0, col = "green", lwd = 2, lty = 2)

# Add a legend to the plot
legend("topright", legend = c("Actual", "Predicted (Current)", "Predicted (Extended)"), 
       col = c("blue", "red", "red"), lwd = 2, lty = c(1, 1, 2), pch = c(NA, 16, NA))



# Using Predictive Modelling for CO2_sa


# Ensure SETTLEMENTDATE is converted in a Date
CO2_sa$SETTLEMENTDATE <- as.Date(CO2_sa$SETTLEMENTDATE)

# Orign date where the first date started 15.12.2017
origin_date <- as.Date("2017-12-15")

# Calculate SETTLEMENDDATE - origin_date
CO2_sa$days_from_origin <- as.numeric(CO2_sa$SETTLEMENTDATE - origin_date)

# Summary statistics
summary(CO2_sa)

CO2_sa <- na.omit(CO2_sa)

set.seed(123)  # to ensure predictive model can be reproduced

# Split the data into 80% training and 20% testing
train_index <- createDataPartition(CO2_sa$GENERATOREMISSIONS, p = 0.8, list = FALSE)
train_data <- CO2_sa[train_index, ]
test_data <- CO2_sa[-train_index, ]

# Train model_sa
model_qld <- train(
  GENERATOREMISSIONS ~ days_from_origin,
  data = train_data,
  method = "lm",  
  trControl = trainControl(method = "cv", number = 5)  
)

# View model_qld summary
print(model_qld)

# Make the predictions
predictions <- predict(model_qld, newdata = test_data)

# Calculate the results
results <- data.frame(
  Actual = test_data$GENERATOREMISSIONS,
  Predicted = predictions
)

# Calculate the RMSE and R-squared with the results generated
rmse <- sqrt(mean((results$Actual - results$Predicted)^2))

print(paste("RMSE:", rmse))

# Plot model_qld for actual vs predicted
plot(test_data$GENERATOREMISSIONS, predictions, main = "Australia's Emission projections in  SA",
     xlab = "Actual Generator Emissions", ylab = "Predicted Generator Emissions")
abline(0, 1, col = "red")  # Add diagonal line for reference




# Extract model_qld for QLD coefficients
coefficients <- coef(model_qld$finalModel)
print(coefficients)


# Extract coefficients from QLD model_qld
coefs <- coef(model_qld$finalmodelModel)
intercept <- coefs[1]  
SETTLEMENT_coef <- coefs["SETTLEMENTDATE"]



# Using Predictive Modelling for CO2_sa


# Ensure SETTLEMENTDATE is converted in a Date
CO2_sa$SETTLEMENTDATE <- as.Date(CO2_sa$SETTLEMENTDATE)

# Orign date where the first date started 15.12.2017
origin_date <- as.Date("2017-12-15")

# Calculate SETTLEMENDDATE - origin_date
CO2_sa$days_from_origin <- as.numeric(CO2_sa$SETTLEMENTDATE - origin_date)

# Summary statistics
summary(CO2_sa)

CO2_sa <- na.omit(CO2_sa)

set.seed(123)  # to ensure predictive model can be reproduced

# Split the data into 80% training and 20% testing
train_index <- createDataPartition(CO2_sa$GENERATOREMISSIONS, p = 0.8, list = FALSE)
train_data <- CO2_sa[train_index, ]
test_data <- CO2_sa[-train_index, ]

# Train model_sa
model_sa <- train(
  GENERATOREMISSIONS ~ days_from_origin,
  data = train_data,
  method = "lm",  
  trControl = trainControl(method = "cv", number = 5)  
)

# View model_sa summary
print(model_sa)

# Make the predictions
predictions <- predict(model_sa, newdata = test_data)

# Calculate the results
results <- data.frame(
  Actual = test_data$GENERATOREMISSIONS,
  Predicted = predictions
)

# Calculate the RMSE and R-squared with the results generated
rmse <- sqrt(mean((results$Actual - results$Predicted)^2))

print(paste("RMSE:", rmse))

# Plot model_sa for actual vs predicted
plot(test_data$GENERATOREMISSIONS, predictions, main = "Australia's Emission projections in  SA",
     xlab = "Actual Generator Emissions", ylab = "Predicted Generator Emissions")
abline(0, 1, col = "red")  # Add diagonal line for reference




# Extract model_sa for SA coefficients
coefficients <- coef(model_sa$finalModel)
print(coefficients)


# Extract coefficients from SA model_sa
coefs <- coef(model_sa$finalmodelModel)
intercept <- coefs[1]  
SETTLEMENT_coef <- coefs["SETTLEMENTDATE"]


# Convert the SETTLEMENTDATE column to Date
CO2_sa$SETTLMENTDATE <- as.Date(CO2_sa$SETTLEMENTDATE)

# Origin_date starting on 16.12.2017 from the data
origin_date <- as.Date("2017-12-16")

# Add a new column created by SETTLEMENTDATE - origin_date
CO2_sa$days_from_origin <- as.numeric(CO2_sa$SETTLEMENTDATE - origin_date)

# View dataframe
print(CO2_sa)


# Create predicted value from model_sa
CO2_sa$GENERATOREMISSIONS_new <- 11730.74114 - 2.94485* CO2_sa$days_from_origin

zero_day <- (11730.74114) / 2.94485

# Extend the range of days 
extended_days <- seq(min(CO2_sa$days_from_origin), zero_day + 500, by = 1)  

# Calculate predicted values for extended
extended_predictions <- 11730.74114 -2.94485* extended_days

# Calculate extended y-axis limits
y_min <- -400  
y_max <- max(c(CO2_sa$GENERATOREMISSIONS_new, extended_predictions)) + 11000

# Define x-axis limits by adding more space
xlim_min <- min(extended_days) - 500 
xlim_max <- max(extended_days) + 500 

# Convert days_from_origin to actual dates for the x-axis
dates_from_origin <- origin_date + CO2_sa$days_from_origin
extended_dates <- origin_date + extended_days

# Plot actual data (actual CO2 generation) in blue
plot(dates_from_origin, CO2_sa$GENERATOREMISSIONS, type = "l", col = "blue", lwd = 2, 
     xlab = "Date", ylab = "CO2 Generation", main = "Australia's Emission projections in  South Australia",
     xlim = c(min(extended_dates), max(extended_dates)), ylim = c(y_min, y_max)) 

# Add the predicted line in red for both the original data and the extended prediction
lines(dates_from_origin, CO2_sa$GENERATOREMISSIONS_new, col = "red", lwd = 3)
lines(extended_dates, extended_predictions, col = "red", lwd = 3, lty = 2)  

points(dates_from_origin, CO2_sa$GENERATOREMISSIONS_new, col = "red", pch = 16)

# X axis with dates instead of Days from Origin`
axis(1, at = seq(min(extended_dates), max(extended_dates), by = "6 months"), 
     labels = format(seq(min(extended_dates), max(extended_dates), by = "6 months"), "%b %Y"), 
     las = 2)  # las = 2 for vertical labels

# Horizontal line in green to determine when projected line hits zerio (i.e., Net Zero tarrget)
abline(h = 0, col = "green", lwd = 2, lty = 2)

# Add a legend to the plot
legend("topright", legend = c("Actual", "Predicted (Current)", "Predicted (Extended)"), 
       col = c("blue", "red", "red"), lwd = 2, lty = c(1, 1, 2), pch = c(NA, 16, NA))


# Using Predictive Modelling for CO2_tas


# Ensure SETTLEMENTDATE is converted in a Date
CO2_tas$SETTLEMENTDATE <- as.Date(CO2_tas$SETTLEMENTDATE)

# Orign date where the first date started 15.12.2017
origin_date <- as.Date("2017-12-15")

# Calculate SETTLEMENDDATE - origin_date
CO2_tas$days_from_origin <- as.numeric(CO2_tas$SETTLEMENTDATE - origin_date)

# Summary statistics
summary(CO2_tas)

CO2_tas <- na.omit(CO2_tas)

set.seed(123)  # to ensure predictive model can be reproduced

# Split the data into 80% training and 20% testing
train_index <- createDataPartition(CO2_tas$GENERATOREMISSIONS, p = 0.8, list = FALSE)
train_data <- CO2_tas[train_index, ]
test_data <- CO2_tas[-train_index, ]

# Train model_tas
model_tas <- train(
  GENERATOREMISSIONS ~ days_from_origin,
  data = train_data,
  method = "lm",  
  trControl = trainControl(method = "cv", number = 5)  
)

# View model_tas summary
print(model_tas)

# Make the predictions
predictions <- predict(model_tas, newdata = test_data)

# Calculate the results
results <- data.frame(
  Actual = test_data$GENERATOREMISSIONS,
  Predicted = predictions
)

# Calculate the RMSE and R-squared with the results generated
rmse <- sqrt(mean((results$Actual - results$Predicted)^2))

print(paste("RMSE:", rmse))

# Plot model_tas for actual vs predicted
plot(test_data$GENERATOREMISSIONS, predictions, main = "Australia's Emission projections in  SA",
     xlab = "Actual Generator Emissions", ylab = "Predicted Generator Emissions")
abline(0, 1, col = "red")  # Add diagonal line for reference




# Extract model_tas for Tas coefficients
coefficients <- coef(model_tas$finalModel)
print(coefficients)


# Extract coefficients from Tas model_tas
coefs <- coef(model_tas$finalmodelModel)
intercept <- coefs[1]  
SETTLEMENT_coef <- coefs["SETTLEMENTDATE"]



# Using Predictive Modelling for CO2_tas


# Ensure SETTLEMENTDATE is converted in a Date
CO2_tas$SETTLEMENTDATE <- as.Date(CO2_tas$SETTLEMENTDATE)

# Orign date where the first date started 15.12.2017
origin_date <- as.Date("2017-12-15")

# Calculate SETTLEMENDDATE - origin_date
CO2_tas$days_from_origin <- as.numeric(CO2_tas$SETTLEMENTDATE - origin_date)

# Summary statistics
summary(CO2_tas)

CO2_tas <- na.omit(CO2_tas)

set.seed(123)  # to ensure predictive model can be reproduced

# Split the data into 80% training and 20% testing
train_index <- createDataPartition(CO2_tas$GENERATOREMISSIONS, p = 0.8, list = FALSE)
train_data <- CO2_tas[train_index, ]
test_data <- CO2_tas[-train_index, ]

# Train model_tas
model_tas <- train(
  GENERATOREMISSIONS ~ days_from_origin,
  data = train_data,
  method = "lm",  
  trControl = trainControl(method = "cv", number = 5)  
)

# View model_tas summary
print(model_tas)

# Make the predictions
predictions <- predict(model_tas, newdata = test_data)

# Calculate the results
results <- data.frame(
  Actual = test_data$GENERATOREMISSIONS,
  Predicted = predictions
)

# Calculate the RMSE and R-squared with the results generated
rmse <- sqrt(mean((results$Actual - results$Predicted)^2))

print(paste("RMSE:", rmse))

# Plot model_tas for actual vs predicted
plot(test_data$GENERATOREMISSIONS, predictions, main = "Australia's Emission projections in  SA",
     xlab = "Actual Generator Emissions", ylab = "Predicted Generator Emissions")
abline(0, 1, col = "red")  # Add diagonal line for reference




# Extract model_tas for Tas coefficients
coefficients <- coef(model_tas$finalModel)
print(coefficients)


# Extract coefficients from Tas model_tas
coefs <- coef(model_tas$finalmodelModel)
intercept <- coefs[1]  
SETTLEMENT_coef <- coefs["SETTLEMENTDATE"]


# Convert the SETTLEMENTDATE column to Date
CO2_tas$SETTLMENTDATE <- as.Date(CO2_tas$SETTLEMENTDATE)

# Origin_date starting on 16.12.2017 from the data
origin_date <- as.Date("2017-12-16")

# Add a new column created by SETTLEMENTDATE - origin_date
CO2_tas$days_from_origin <- as.numeric(CO2_tas$SETTLEMENTDATE - origin_date)

# View dataframe
print(CO2_tas)


# Create predicted value from model_tas
CO2_tas$GENERATOREMISSIONS_new <- 832.4988419 - 0.2690263 * CO2_tas$days_from_origin

zero_day <- (832.4988419) / 0.2690263 

# Extend the range of days 
extended_days <- seq(min(CO2_tas$days_from_origin), zero_day + 500, by = 1)  

# Calculate predicted values for extended
extended_predictions <- 832.4988419 -0.2690263 * extended_days

# Calculate extended y-axis limits
y_min <- -400  
y_max <- max(c(CO2_tas$GENERATOREMISSIONS_new, extended_predictions)) + 5000

# Define x-axis limits by adding more space
xlim_min <- min(extended_days) - 500 
xlim_max <- max(extended_days) + 500 

# Convert days_from_origin to actual dates for the x-axis
dates_from_origin <- origin_date + CO2_tas$days_from_origin
extended_dates <- origin_date + extended_days

# Plot actual data (actual CO2 generation) in blue
plot(dates_from_origin, CO2_tas$GENERATOREMISSIONS, type = "l", col = "blue", lwd = 2, 
     xlab = "Date", ylab = "CO2 Generation", main = "Australia's Emission projections in  Tasmania",
     xlim = c(min(extended_dates), max(extended_dates)), ylim = c(y_min, y_max)) 

# Add the predicted line in red for both the original data and the extended prediction
lines(dates_from_origin, CO2_tas$GENERATOREMISSIONS_new, col = "red", lwd = 3)
lines(extended_dates, extended_predictions, col = "red", lwd = 3, lty = 2)  

points(dates_from_origin, CO2_tas$GENERATOREMISSIONS_new, col = "red", pch = 16)

# X axis with dates instead of Days from Origin`
axis(1, at = seq(min(extended_dates), max(extended_dates), by = "6 months"), 
     labels = format(seq(min(extended_dates), max(extended_dates), by = "6 months"), "%b %Y"), 
     las = 2)  # las = 2 for vertical labels

# Horizontal line in green to determine when projected line hits zerio (i.e., Net Zero tarrget)
abline(h = 0, col = "green", lwd = 2, lty = 2)

# Add a legend to the plot
legend("topright", legend = c("Actual", "Predicted (Current)", "Predicted (Extended)"), 
       col = c("blue", "red", "red"), lwd = 2, lty = c(1, 1, 2), pch = c(NA, 16, NA))

# Using Predictive Modelling for CO2_vic


# Ensure SETTLEMENTDATE is converted in a Date
CO2_vic$SETTLEMENTDATE <- as.Date(CO2_vic$SETTLEMENTDATE)

# Orign date where the first date started 15.12.2017
origin_date <- as.Date("2017-12-15")

# Calculate SETTLEMENDDATE - origin_date
CO2_vic$days_from_origin <- as.numeric(CO2_vic$SETTLEMENTDATE - origin_date)

# Summary statistics
summary(CO2_vic)

CO2_vic <- na.omit(CO2_vic)

set.seed(123)  # to ensure predictive model can be reproduced

# Split the data into 80% training and 20% testing
train_index <- createDataPartition(CO2_vic$GENERATOREMISSIONS, p = 0.8, list = FALSE)
train_data <- CO2_vic[train_index, ]
test_data <- CO2_vic[-train_index, ]

# Train model_vic
model_vic <- train(
  GENERATOREMISSIONS ~ days_from_origin,
  data = train_data,
  method = "lm",  
  trControl = trainControl(method = "cv", number = 5)  
)

# View model_vic summary
print(model_vic)

# Make the predictions
predictions <- predict(model_vic, newdata = test_data)

# Calculate the results
results <- data.frame(
  Actual = test_data$GENERATOREMISSIONS,
  Predicted = predictions
)

# Calculate the RMSE and R-squared with the results generated
rmse <- sqrt(mean((results$Actual - results$Predicted)^2))

print(paste("RMSE:", rmse))

# Plot model_vic for actual vs predicted
plot(test_data$GENERATOREMISSIONS, predictions, main = "Australia's Emission projections in  SA",
     xlab = "Actual Generator Emissions", ylab = "Predicted Generator Emissions")
abline(0, 1, col = "red")  # Add diagonal line for reference




# Extract model_vic for Vic coefficients
coefficients <- coef(model_vic$finalModel)
print(coefficients)


# Extract coefficients from Vic model_vic
coefs <- coef(model_vic$finalmodelModel)
intercept <- coefs[1]  
SETTLEMENT_coef <- coefs["SETTLEMENTDATE"]

# Using Predictive Modelling for CO2_vic


# Ensure SETTLEMENTDATE is converted in a Date
CO2_vic$SETTLEMENTDATE <- as.Date(CO2_vic$SETTLEMENTDATE)

# Orign date where the first date started 15.12.2017
origin_date <- as.Date("2017-12-15")

# Calculate SETTLEMENDDATE - origin_date
CO2_vic$days_from_origin <- as.numeric(CO2_vic$SETTLEMENTDATE - origin_date)

# Summary statistics
summary(CO2_vic)

CO2_vic <- na.omit(CO2_vic)

set.seed(123)  # to ensure predictive model can be reproduced

# Split the data into 80% training and 20% testing
train_index <- createDataPartition(CO2_vic$GENERATOREMISSIONS, p = 0.8, list = FALSE)
train_data <- CO2_vic[train_index, ]
test_data <- CO2_vic[-train_index, ]

# Train model_vic
model_vic <- train(
  GENERATOREMISSIONS ~ days_from_origin,
  data = train_data,
  method = "lm",  
  trControl = trainControl(method = "cv", number = 5)  
)

# View model_vic summary
print(model_vic)

# Make the predictions
predictions <- predict(model_vic, newdata = test_data)

# Calculate the results
results <- data.frame(
  Actual = test_data$GENERATOREMISSIONS,
  Predicted = predictions
)

# Calculate the RMSE and R-squared with the results generated
rmse <- sqrt(mean((results$Actual - results$Predicted)^2))

print(paste("RMSE:", rmse))

# Plot model_vic for actual vs predicted
plot(test_data$GENERATOREMISSIONS, predictions, main = "Australia's Emission projections in  SA",
     xlab = "Actual Generator Emissions", ylab = "Predicted Generator Emissions")
abline(0, 1, col = "red")  # Add diagonal line for reference




# Extract model_vic for Vic coefficients
coefficients <- coef(model_vic$finalModel)
print(coefficients)


# Extract coefficients from Vic model_vic
coefs <- coef(model_vic$finalmodelModel)
intercept <- coefs[1]  
SETTLEMENT_coef <- coefs["SETTLEMENTDATE"]



# Using Predictive Modelling for CO2_vic


# Ensure SETTLEMENTDATE is converted in a Date
CO2_vic$SETTLEMENTDATE <- as.Date(CO2_vic$SETTLEMENTDATE)

# Orign date where the first date started 15.12.2017
origin_date <- as.Date("2017-12-15")

# Calculate SETTLEMENDDATE - origin_date
CO2_vic$days_from_origin <- as.numeric(CO2_vic$SETTLEMENTDATE - origin_date)

# Summary statistics
summary(CO2_vic)

CO2_vic <- na.omit(CO2_vic)

set.seed(123)  # to ensure predictive model can be reproduced

# Split the data into 80% training and 20% testing
train_index <- createDataPartition(CO2_vic$GENERATOREMISSIONS, p = 0.8, list = FALSE)
train_data <- CO2_vic[train_index, ]
test_data <- CO2_vic[-train_index, ]

# Train model_vic
model_vic <- train(
  GENERATOREMISSIONS ~ days_from_origin,
  data = train_data,
  method = "lm",  
  trControl = trainControl(method = "cv", number = 5)  
)

# View model_vic summary
print(model_vic)

# Make the predictions
predictions <- predict(model_vic, newdata = test_data)

# Calculate the results
results <- data.frame(
  Actual = test_data$GENERATOREMISSIONS,
  Predicted = predictions
)

# Calculate the RMSE and R-squared with the results generated
rmse <- sqrt(mean((results$Actual - results$Predicted)^2))

print(paste("RMSE:", rmse))

# Plot model_vic for actual vs predicted
plot(test_data$GENERATOREMISSIONS, predictions, main = "Australia's Emission projections in  Victoria",
     xlab = "Actual Generator Emissions", ylab = "Predicted Generator Emissions")
abline(0, 1, col = "red")  # Add diagonal line for reference




# Extract model_vic for Vic coefficients
coefficients <- coef(model_vic$finalModel)
print(coefficients)


# Extract coefficients from Vic model_vic
coefs <- coef(model_vic$finalmodelModel)
intercept <- coefs[1]  
SETTLEMENT_coef <- coefs["SETTLEMENTDATE"]


# Convert the SETTLEMENTDATE column to Date
CO2_vic$SETTLMENTDATE <- as.Date(CO2_vic$SETTLEMENTDATE)

# Origin_date starting on 16.12.2017 from the data
origin_date <- as.Date("2017-12-16")

# Add a new column created by SETTLEMENTDATE - origin_date
CO2_vic$days_from_origin <- as.numeric(CO2_vic$SETTLEMENTDATE - origin_date)

# View dataframe
print(CO2_vic)


# Create predicted value from model_vic
CO2_vic$GENERATOREMISSIONS_new <- 115407.415034 - 8.835049  * CO2_vic$days_from_origin

zero_day <- (115407.415034) / 8.835049  

# Extend the range of days 
extended_days <- seq(min(CO2_vic$days_from_origin), zero_day + 500, by = 1)  

# Calculate predicted values for extended
extended_predictions <- 115407.415034 -8.835049  * extended_days

# Calculate extended y-axis limits
y_min <- 0  
y_max <- max(c(CO2_vic$GENERATOREMISSIONS_new, extended_predictions)) + 100000 

# Define x-axis limits by adding more space
xlim_min <- min(extended_days) - 500 
xlim_max <- max(extended_days) + 500 

# Convert days_from_origin to actual dates for the x-axis
dates_from_origin <- origin_date + CO2_vic$days_from_origin
extended_dates <- origin_date + extended_days

# Plot actual data (actual CO2 generation) in blue
plot(dates_from_origin, CO2_vic$GENERATOREMISSIONS, type = "l", col = "blue", lwd = 2, 
     xlab = "Date", ylab = "CO2 Generation", main = "Australia's Emission projections in  Victoria",
     xlim = c(min(extended_dates), max(extended_dates)), ylim = c(y_min, y_max)) 

# Add the predicted line in red for both the original data and the extended prediction
lines(dates_from_origin, CO2_vic$GENERATOREMISSIONS_new, col = "red", lwd = 3)
lines(extended_dates, extended_predictions, col = "red", lwd = 3, lty = 2)  

points(dates_from_origin, CO2_vic$GENERATOREMISSIONS_new, col = "red", pch = 16)

# X axis with dates instead of Days from Origin`
axis(1, at = seq(min(extended_dates), max(extended_dates), by = "6 months"), 
     labels = format(seq(min(extended_dates), max(extended_dates), by = "6 months"), "%b %Y"), 
     las = 2)  # las = 2 for vertical labels


# Horizontal line in green to determine when projected line hits zerio (i.e., Net Zero tarrget)
abline(h = 0, col = "green", lwd = 2, lty = 2)


# Add a legend to the plot
legend("topright", legend = c("Actual", "Predicted (Current)", "Predicted (Extended)"), 
       col = c("blue", "red", "red"), lwd = 2, lty = c(1, 1, 2), pch = c(NA, 16, NA))



#Investigation for Renewable Proportion in 2024 for RQ3 to explain its distribution in renewable energy source


RQ3_renewableproportion_selected <- RQ2_2024_noOutliers[, c("Hydro", "Wind", "Solar", "PV_Rooftop", "Region")]



RQ3_renewableproportion_selected$Quantity <-RQ3_renewableproportion_selected$Hydro + RQ3_renewableproportion_selected$Wind + RQ3_renewableproportion_selected$Solar + RQ3_renewableproportion_selected$PV_Rooftop
RQ3_renewableproportion_selected_unique <- RQ3_renewableproportion_selected[!duplicated(RQ3_renewableproportion_selected$Quantity), ]




RQ3_renewableproportion_selected_unique_summary <- RQ3_renewableproportion_selected_unique %>%
  group_by(Region) %>%
  summarise("2024_UtilityScaleSolar"= sum(Solar), "2024_PV_Rooftop" = sum(PV_Rooftop),"2024_Wind" = sum(Wind), "2024_Hydro" = sum(Hydro) , .groups = "drop")

# Do a Pivot for the stacked Bar plot
RQ3_renewableproportion_selected_unique_pivot <- RQ3_renewableproportion_selected_unique_summary %>%
  pivot_longer(cols = "2024_UtilityScaleSolar":"2024_Hydro", 
               names_to = "Fuel_Renewable_Category", 
               values_to = "Quantity")

# Check to see if pivot was done correctly
head(RQ3_renewableproportion_selected_unique_pivot)




#Compute Percentage specifiically for the stacked bar plot

Sumarised_Renewable_Penetration_grouping_percent <- RQ3_renewableproportion_selected_unique_pivot %>%
  group_by(Region) %>%
  mutate(
    percent = Quantity / sum(Quantity) * 100,
    label = paste0(round(percent, 1), "%")
  )

#Stack bar plot created with the existing data 

ggplot(Sumarised_Renewable_Penetration_grouping_percent, aes(x = factor(Region), y = Quantity, fill = Fuel_Renewable_Category)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 3.5) +
  labs(title = "Quantity and Fuel Renewable Categories in Australia (NEM) (with percentages) in 2024", x = "Region", y = "Quantity in Electricity Generation", fill = "Fuel_Renewable_Category") +
  scale_x_discrete(labels = levels(factor(Sumarised_Renewable_Penetration_grouping_percent$Region))) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  theme_minimal()

###########
#Load UNFCCC Data

UNFCCCdata <- read.csv("Electricity Emissions in the baseline scenario 2005 to 2040.CSV", header = FALSE)

DCCEEW <- UNFCCCdata
colnames(DCCEEW) <-  c("Year", "Electricity Emissions at NEM from DCCEEW")
DCCEEW <- DCCEEW[-1, ]

# Convert 'Year' to numeric value
DCCEEW$Year <- as.numeric(DCCEEW$Year)
DCCEEW$'Electricity Emissions at NEM from DCCEEW' <- as.numeric(DCCEEW$'Electricity Emissions at NEM from DCCEEW')


# Order first before doing the ggplot
DCCEEW <- DCCEEW[order(DCCEEW$Year), ]
DCCEEW <- DCCEEW[order(DCCEEW$'Electricity Emissions at NEM from DCCEEW'), ]


ggplot(DCCEEW, aes(x = Year, y = `Electricity Emissions at NEM from DCCEEW`)) +
  geom_point(color = "blue", size = 3) +  
  geom_line(color = "red", size = 1) + 
  labs(title = "DCCEEW - UNFCCC over time to 2040 for Emission", 
       x = "Year", 
       y = "Electricity Emissions at NEM from DCCEEW") +  
  theme_minimal()





