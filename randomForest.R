#installations
install.packages("caret")

#libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(randomForest)
library(tidyr)
library(caret) 
set.seed(12)

#----------- DATA SET UP: No LAG-------------

#data import
data = read_excel("C:/Users/climate_mega.xlsx", sheet = "mega_month")


# replacing N/A with actual NAs in relevant columns
data = data %>%
mutate(across(where(is.character), ~na_if(., "N/A")))


# converting columns to numeric (numbers) and factor types (characters)
rf_data = data %>%
  mutate(
    VCI = as.numeric(VCI),
    NDMI = as.numeric(NDMI),
    VV = as.numeric(VV),
    VH = as.numeric(VH),
    SPI = as.numeric(SPI),
    SPEI = as.numeric(SPEI),
    Habitat = as.factor(Habitat),
    Year = as.factor(Year),
    monthNo = as.factor(monthNo),
    monthNo = as.factor(Location)
  )


#----------- DATA SET UP: WITH LAG-------------


# setting year as numeric just for lagged SPI and SPEI
rf_data_lagged = data %>%
  mutate(Year = as.numeric(as.character(Year)))


# applying lag of 5 (ie one warm-season-only year)
rf_data_lagged = rf_data %>%
  arrange(Location, Year, monthNo) %>%
  group_by(Location) %>%
  mutate(
    SPI_lag = lag(SPI, 5),
    SPEI_lag = lag(SPEI, 5)
  ) %>%
  ungroup() %>%
  drop_na(SPI_lag, SPEI_lag)

# dropping na values for OG data
rf_data = rf_data %>%
  drop_na(VCI, VV, VH, NDMI, SPI, SPEI, monthNo, Year, Habitat, Location)

# making sure variables are numeric (again)
rf_data$VCI = as.numeric(rf_data$VCI)
rf_data$NDMI = as.numeric(rf_data$NDMI)

# repeating the na drop
rf_data_lagged = rf_data_lagged %>%
  drop_na(VCI, VV, VH, NDMI, SPI_lag, SPEI_lag, monthNo, Year, Habitat, Location)


# and the variables to numeric
rf_data_lagged$VCI = as.numeric(rf_data_lagged$VCI)
rf_data_lagged$NDMI = as.numeric(rf_data_lagged$NDMI)

#to check if there's any leftover NAs
colSums(is.na(rf_data))

#----------- RANDOM FOREST SET UP AND RUN -------------


# predicting *VCI* with RF and *all variables*
rf_vci = randomForest(
  VCI ~ VV + VH + SPI + SPEI+ NDMI + monthNo + Year + Habitat + Location,
  data = rf_data,
  importance = TRUE
)
# variable importance
varImpPlot(rf_vci)
#summary
print(rf_vci)

#predicting *VCI* with RF and *just SAR*
rf_vci_sar = randomForest(
  VCI ~ VV + VH,
  data = rf_data,
  importance = TRUE
)
# variable importance
varImpPlot(rf_vci_sar)
#summary
print(rf_vci_sar)



# predicting *NDMI* with RF and all vars
rf_ndmi = randomForest(
  NDMI ~ VV + VH + SPI + SPEI + VCI + monthNo + Year + Habitat,
  data = rf_data,
  importance = TRUE,
  ntree = 500,
  mtry = 2
)
# variable importance
varImpPlot(rf_ndmi)
# summary
print(rf_ndmi)



#predicting *NDMI* with RF and *just SAR*
rf_ndmi_sar = randomForest(
  NDMI ~ VV + VH,
  data = rf_data,
  importance = TRUE
)
# variable importance
varImpPlot(rf_ndmi_sar)
#summary
print(rf_ndmi_sar)



#PREDICTED VS MEASURED VALUES 

#saving predicted values
rf_data$VCI_pred = predict(rf_vci)
rf_data$VCI_pred_sar = predict(rf_vci_sar)

rf_data$NDMI_pred = predict(rf_ndmi)
rf_data$NDMI_pred_sar = predict(rf_ndmi_sar)

rf_data$SPI_pred = predict(rf_SPI)
rf_data$SPEI_pred = predict(rf_SPEI)



#CHART FOR ALL VARS AND *VCI*

# calculating r squared and rmse for VCI
r_squared_VCI = summary(lm(VCI_pred ~ VCI, data = rf_data))$r.squared
r_squared_VCI_sar = summary(lm(VCI_pred_sar ~ VCI, data = rf_data))$r.squared

rmse_VCI = sqrt(mean((rf_data$VCI - rf_data$VCI_pred)^2))
rmse_VCI_sar = sqrt(mean((rf_data$VCI - rf_data$VCI_pred_sar)^2))


# text for chart
annotation_text = paste0("R² = ", round(r_squared_VCI, 2),
                          "\nRMSE = ", round(rmse_VCI, 2))

# chart
ggplot(rf_data, aes(x = VCI, y = VCI_pred)) +
  geom_point(alpha = 0.8, color = "azure4") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  annotate("text", x = min(rf_data$VCI), y = max(rf_data$VCI_pred),
           label = annotation_text, hjust = 0, vjust = 1,
           size = 5, fontface = "italic") +
  labs(x = "Observed VCI",
       y = "Predicted VCI") +
  theme_minimal()


#CHART FOR SAR ONLY AND *VCI*
#text for chart
annotation_text = paste0("R² = ", round(r_squared_VCI_sar, 2),
                          "\nRMSE = ", round(rmse_VCI_sar, 2))

# chart
ggplot(rf_data, aes(x = VCI, y = VCI_pred_sar)) +
  geom_point(alpha = 0.8, color = "azure4") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  annotate("text", x = min(rf_data$VCI), y = max(rf_data$VCI_pred_sar),
           label = annotation_text, hjust = 0, vjust = 1,
           size = 5, fontface = "italic") +
  labs(x = "Observed VCI",
       y = "Predicted VCI") +
  theme_minimal()


#NDMI CHARTS WITH ALL VARS

# calculating r^2 and rmse for charts
r_squared_NDMI_sar = summary(lm(NDMI_pred_sar ~ NDMI, data = rf_data))$r.squared
rmse_NDMI_sar = sqrt(mean((rf_data$NDMI - rf_data$NDMI_pred)^2))

r_squared_NDMI_sar = summary(lm(NDMI_pred_sar ~ NDMI, data = rf_data))$r.squared
rmse_NDMI_sar = sqrt(mean((rf_data$NDMI - rf_data$NDMI_pred_sar)^2))


# adding text
annotation_text = paste0("R² = ", round(r_squared_NDMI_sar, 2), 
                          "\nRMSE = ", round(rmse_NDMI_sar, 2))

# charts
ggplot(rf_data, aes(x = NDMI, y = NDMI_pred_sar)) +
  geom_point(alpha = 0.5, color = "azure4") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  annotate("text", x = min(rf_data$NDMI), y = max(rf_data$NDMI_pred_sar),
           label = annotation_text, hjust = 0, vjust = 1,
           size = 5, fontface = "italic") +
  labs(x = "Observed NDMI",
       y = "Predicted NDMI") +
  theme_minimal()


#VARIABLE IMPORTANCE FOR NDMI

importance_ndmi = data.frame(
  Variable = rownames(importance(rf_ndmi)),
  Importance = importance(rf_ndmi)[, "IncNodePurity"]
)

# descending order
importance_ndmi = importance_ndmi[order(importance_ndmi$Importance, decreasing = TRUE), ]
importance_ndmi$Variable = factor(importance_ndmi$Variable, levels = importance_ndmi$Variable)

# chart
ggplot(importance_ndmi, aes(x = Variable, y = Importance)) +
  geom_bar(stat = "identity", fill = "azure4", width = 0.5) +
  theme_minimal(base_size = 14) +
  labs(x = "Predictor",
       y = "Importance") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#VARIABLE IMPORTANCE FOR VCI

importance_vci = data.frame(
  Variable = rownames(importance(rf_vci)),
  Importance = importance(rf_vci)[, "IncNodePurity"]
)

# descending order
importance_vci = importance_vci[order(importance_vci$Importance, decreasing = TRUE), ]
importance_vci$Variable = factor(importance_vci$Variable, levels = importance_vci$Variable)

# chart
library(ggplot2)
ggplot(importance_vci, aes(x = Variable, y = Importance)) +
  geom_bar(stat = "identity", fill = "azure4", width = 0.5) +
  theme_minimal(base_size = 14) +
  labs(x = "Predictor",
       y = "Importance") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



