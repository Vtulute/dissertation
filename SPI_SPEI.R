
library(dplyr)
library(lubridate)
library(ggplot2)
library(SPEI)
library(readxl)
library(dunn.test)

#data import
data = read_excel("C:/Users/climate_mega.xlsx", sheet = "Main")

#----SPI----
# date column for year and month
data = data %>%
  mutate(
    Date = as.Date(paste(Year, Month, "1"), format = "%Y %B %d")
  )
# defining spi function
calculate_spi = function(df) {
  df = df %>% arrange(Date)
  spi_vals = spi(as.numeric(df$Rainfall), scale = 1)$fitted
  df$SPI = spi_vals
  return(df)
}


#----SPEI----


# spi function with rainfall and temperature
calculate_spei = function(df) {
  df = df %>% arrange(Date)
  
  # estimating pet using Thornthwaite
  pet = thornthwaite(df$Temperature, lat = 52)
  
  #water balance
  balance = df$Rainfall - pet
  
  # 1-month spei
  spei_vals = spei(balance, scale = 1)$fitted
  
  df$SPEI = spei_vals
  return(df)
}

spei_data = data %>%
  group_by(Habitat) %>%
  group_modify(~ calculate_spei(.x)) %>%
  ungroup()

#export
#write_csv(spi_data, "SPI_results.csv")
#write_csv(spei_data, "SPEI_results.csv")

