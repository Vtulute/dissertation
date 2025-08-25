#install.packages("patchwork")

library(tidyverse)
library(car)     
library(rstatix)
library(readxl)
library(ggplot2)
library(dplyr)
library(dunn.test)
library(lubridate)
library(purrr)
library(FSA)
library(ggpubr)
library(tidyr)
library(patchwork)
library(cowplot)
#reading the data set
data = read_excel("C:/Users/climate_mega.xlsx", sheet = "mega_month")

#----*********************set up**********************----

yearly_summary = data %>%
  group_by(Habitat, Year) %>%
  summarise(
    mean_VV = mean(VV, na.rm = TRUE),
    sd_VV = sd(VV, na.rm = TRUE),
    n = sum(!is.na(VV)),
    se_VV = sd_VV / sqrt(n)
  ) %>%
  ungroup()

habitat_colors = c(
  "Heathland" = "mediumorchid4",
  "Forest" = "forestgreen",
  "Grassland" = "lightgoldenrod2"
)

habitat_shapes = c(
  "Heathland" = 16,  # solid circle
  "Forest" = 17,     # triangle
  "Grassland" = 15   # square
)

# replacing N/A with NA, but only in character columns
data = data %>%
mutate(across(where(is.character), ~na_if(., "N/A")))

# converting those columns to numeric
data = data %>%
mutate(across(c(NDMI, VCI, SPEI, SPI), as.numeric))

#creating data with stacked indices
long_data = data %>%
  pivot_longer(
    cols = c(VV, VH, NDMI, VCI, SPEI, SPI),
    names_to = "Index",
    values_to = "Value"
  )


# custom labels for graphs
custom_labels = c(
  "VV" = "VV Backscatter (dB)",
  "VH" = "VH Backscatter (dB)",
  "NDMI" = "NDMI",
  "VCI" = "VCI",
  "SPEI" = "SPEI",
  "SPI" = "SPI"
)


#----*******************Patchwork plot********************----

# making one plot for each index/backscatter
p_vv = ggplot(filter(long_data, Index == "VV"), aes(x = factor(Year), y = Value, fill = Habitat)) +
  annotate("rect", xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf, fill="grey80", alpha=0.4) +
  annotate("rect", xmin=5.5, xmax=6.5, ymin=-Inf, ymax=Inf, fill="grey80", alpha=0.4) +
  geom_boxplot(position=position_dodge(0.8), alpha=0.6, color="black", outlier.shape=NA) +
  scale_fill_manual(values=habitat_colors) +
  labs(y = "VV Backscatter (dB)", x = "Year") +
  theme_minimal(base_size=14) +
  theme(
    axis.title.y = element_text(size=10),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position="right",
    legend.text=element_text(size=10),
    legend.title=element_text(size=11),
    legend.key.size=unit(0.5, "cm"),
    legend.spacing.y=unit(0.2, "cm")
    
    )

p_vh = ggplot(filter(long_data, Index == "VH"), aes(x = factor(Year), y = Value, fill = Habitat)) +
  annotate("rect", xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf, fill="grey80", alpha=0.4) +
  annotate("rect", xmin=5.5, xmax=6.5, ymin=-Inf, ymax=Inf, fill="grey80", alpha=0.4) +
  geom_boxplot(position=position_dodge(0.8), alpha=0.6, color="black", outlier.shape=NA) +
  scale_fill_manual(values=habitat_colors) +
  labs(y = "VH Backscatter (dB)", x = "Year") +
  theme_minimal(base_size=14) +
  theme(
    axis.title.y = element_text(size=10),
    axis.title.x = element_blank(),            
    axis.text.x = element_blank(),            
    axis.ticks.x = element_blank(),            
    legend.position="none"
    )

p_ndmi = ggplot(filter(long_data, Index == "NDMI"), aes(x = factor(Year), y = Value, fill = Habitat)) +
  annotate("rect", xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf, fill="grey80", alpha=0.4) +
  annotate("rect", xmin=5.5, xmax=6.5, ymin=-Inf, ymax=Inf, fill="grey80", alpha=0.4) +
  geom_boxplot(position=position_dodge(0.8), alpha=0.6, color="black", outlier.shape=NA) +
  scale_fill_manual(values=habitat_colors) +
  labs(y = "NDMI", x = "Year") +
  theme_minimal(base_size=14) +
  theme(
    axis.title.y = element_text(size=10),
    axis.title.x = element_blank(),            
    axis.text.x = element_blank(),             
    axis.ticks.x = element_blank(),           
    legend.position="none"
    )

p_vci = ggplot(filter(long_data, Index == "VCI"), aes(x = factor(Year), y = Value, fill = Habitat)) +
  annotate("rect", xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf, fill="grey80", alpha=0.4) +
  annotate("rect", xmin=5.5, xmax=6.5, ymin=-Inf, ymax=Inf, fill="grey80", alpha=0.4) +
  geom_boxplot(position=position_dodge(0.8), alpha=0.6, color="black", outlier.shape=NA) +
  scale_fill_manual(values=habitat_colors) +
  labs(y = "VCI", x = "Year") +
  theme_minimal(base_size=14) +
  theme(
    axis.title.y = element_text(size=10),
    axis.title.x = element_blank(),            
    axis.text.x = element_blank(),         
    axis.ticks.x = element_blank(),          
    legend.position="none"
    )

p_spei = ggplot(filter(long_data, Index == "SPEI"), aes(x = factor(Year), y = Value, fill = Habitat)) +
  annotate("rect", xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf, fill="grey80", alpha=0.4) +
  annotate("rect", xmin=5.5, xmax=6.5, ymin=-Inf, ymax=Inf, fill="grey80", alpha=0.4) +
  geom_boxplot(position=position_dodge(0.8), alpha=0.6, color="black", outlier.shape=NA) +
  scale_fill_manual(values=habitat_colors) +
  labs(y = "SPEI", x = "Year") +
  theme_minimal(base_size=14) +
  theme(
    axis.title.y = element_text(size=10),
    axis.title.x = element_text(size=10),     
    axis.text.x = element_text(angle=45, hjust=1, size=8),  
    legend.position="none"
    )

p_spi = ggplot(filter(long_data, Index == "SPI"), aes(x = factor(Year), y = Value, fill = Habitat)) +
  annotate("rect", xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf, fill="grey80", alpha=0.4) +
  annotate("rect", xmin=5.5, xmax=6.5, ymin=-Inf, ymax=Inf, fill="grey80", alpha=0.4) +
  geom_boxplot(position=position_dodge(0.8), alpha=0.6, color="black", outlier.shape=NA) +
  scale_fill_manual(values=habitat_colors) +
  labs(y = "SPI", x = "Year") +
  theme_minimal(base_size=14) +
  theme(
    axis.title.y = element_text(size=10),
    axis.title.x = element_text(size=10),     
    axis.text.x = element_text(angle=45, hjust=1, size=8),  
    legend.position="none"
        )

# making top and bottom rows without legends
top_row = plot_grid(
  p_vv + theme(legend.position="none"),
  p_vh + theme(legend.position="none"),
  p_ndmi + theme(legend.position="none"),
  p_vci + theme(legend.position="none"),
  ncol=2, labels=NULL
)

bottom_row = plot_grid(
  p_spi + theme(legend.position="none"),
  p_spei + theme(legend.position="none"),
  ncol=2, labels=NULL
)

# extracting the legend from vv plot with legend enabled
legend_plot = get_legend(p_vv)

# combining the rows
combined_plot = plot_grid(top_row, bottom_row, ncol=1, rel_heights=c(2,1))

# adding the legend
final_plot = plot_grid(combined_plot, legend_plot, ncol=2, rel_widths=c(0.9,0.1))

# printing the patch plot with habitat groups
print(final_plot)


#----one patchwork plot for years only----

# creating a base theme
base_theme = theme_minimal(base_size = 14) +
  theme(
    axis.title.y = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "none"
  )


# making a function for the plot
make_plot_by_year = function(index_name, y_label, box_color = "slategray2", box_width = 0.4, show_x_axis = TRUE) {
  p = ggplot(filter(long_data, Index == index_name), aes(x = factor(Year), y = Value)) +
    annotate("rect", xmin = 1.5, xmax = 2.5, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.4) +
    annotate("rect", xmin = 5.5, xmax = 6.5, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.4) +
    geom_boxplot(
      width = box_width,
      fill = box_color,
      color = "black",
      alpha = 0.6,
      outlier.shape = NA
    ) +
    labs(y = y_label, x = "Year") +
    base_theme
  
  if (!show_x_axis) {
    p = p + theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  }
  
  return(p)
}


#applying function

p_vv_year = make_plot_by_year("VV", "VV Backscatter (dB)", show_x_axis = FALSE)
p_vh_year = make_plot_by_year("VH", "VH Backscatter (dB)", show_x_axis = FALSE)
p_ndmi_year = make_plot_by_year("NDMI", "NDMI", show_x_axis = FALSE)
p_vci_year = make_plot_by_year("VCI", "VCI", show_x_axis = FALSE)

# choosing which axes are shown and where
p_spi_year   = make_plot_by_year("SPI", "SPI", show_x_axis = TRUE)
p_spei_year  = make_plot_by_year("SPEI", "SPEI", show_x_axis = TRUE)

# combining plots into rows
top_row_year = plot_grid(p_vv_year, p_vh_year, p_ndmi_year, p_vci_year, ncol = 2)
bottom_row_year = plot_grid(p_spi_year, p_spei_year, ncol = 2)

# final plot
final_year_plot = plot_grid(top_row_year, bottom_row_year, ncol = 1, rel_heights = c(2, 1))

# printing plot without habitat groupings
print(final_year_plot)







#----***************************actual tests**********************----
 # setting factor variables
 data = data %>%
  mutate(
    Year = as.factor(Year),
    DroughtStatus = as.factor(DroughtStatus),
    Habitat = as.factor(Habitat),
    Location = as.factor(Location)
  )

#establishing indices seperately
indices = c("VV", "VH", "NDMI", "VCI", "SPI", "SPEI")

#re-establishing numeric-ness and making N/As actual NAs
data = data %>%
mutate(across(
    all_of(indices),
    ~ as.numeric(na_if(as.character(.), "N/A"))
  ))

# seeing if there's other NA values
sapply(data[indices], function(x) sum(is.na(x)))

colnames(data)


#----NORMALITY----

# shapiro wilk test for each index
normality_results = map_df(indices, function(index) {
  if (!index %in% colnames(data)) {
    warning(glue("Column {index} not found in data. Skipping."))
    return(tibble(Index = index, statistic = NA, p.value = NA))
  }
  
  df_index = data %>%
    select(all_of(index)) %>%
    filter(!is.na(.data[[index]]))
  

  shapiro = shapiro_test(df_index[[index]])
  shapiro$Index = index
  return(shapiro)
})

print(normality_results)

# and groupings of year, index and habitat-group
normality_results = data %>%
  pivot_longer(cols = all_of(indices), names_to = "Index", values_to = "Value") %>%
  group_by(Habitat, Year, Index) %>%
  summarise(
    p.value = if (length(Value[!is.na(Value)]) >= 3) shapiro.test(Value)$p.value else NA,
    .groups = "drop"
  )

print(normality_results, n=120)


#all index only data is non-parametric
# groupings data also has non-parametric groups

# Kruskal-Wallis test for year vs year for all indices, 
#but no habitat groups

for (index in indices) {

  kruskal_year_all = kruskal.test(
    formula(paste(index, "~ as.factor(Year)")), 
    data = data
  )
  
  cat("\n============================\n")
  cat("Kruskal-Wallis Test (Year vs Year):", index, "\n")
  print(kruskal_year_all)
  cat("============================\n")
}

#significant differences only for SPI and SPEI

# now Kruskal for years separated by habitat groups
for (index in indices) {
  habitats = unique(data$Habitat)
  
  for (habitat in habitats) {
    subset_data = data %>% filter(Habitat == habitat)
    
    kruskal_year_habitat = kruskal.test(
      formula(paste(index, "~ as.factor(Year)")), 
      data = subset_data
    )
    
    cat("\n============================\n")
    cat("Kruskal-Wallis Test (Year vs Year) -", habitat, "-", index, "\n")
    print(kruskal_year_habitat)
    cat("============================\n")
  }
}

# significant differences found between:
# SPI (all habitats), SPEI (grass and forest), 
# VCI (grasslands), NDMI (grass, forest)
# VH (grassland), VV (grassland)


# dunns test for SPI and SPEI all years without habitat groups
posthoc_indices = c("SPI", "SPEI")

for (index in posthoc_indices) {
  cat("\n=====================================\n")
  cat("Post hoc Dunn's Test for", index, "(Year vs Year)\n")
  cat("=====================================\n")
  
  # removing NAs
  df = data %>%
    select(Year, all_of(index)) %>%
    filter(!is.na(.data[[index]]))
  
  # running the test
  dunn_result = dunn.test(
    x = df[[index]],
    g = df$Year,
    method = "none"  #kruskal and dunn's didn't match with other settings
  )
  
  print(dunn_result)
}


#testing significant combos further
#defining index habitat combos for larger dunns test
posthoc_combos = list(
  list(index = "VV", habitat = "Grassland"),
  list(index = "VH", habitat = "Grassland"),
  list(index = "NDMI", habitat = "Grassland"),
  list(index = "VCI", habitat = "Grassland"),
  list(index = "SPI", habitat = "Grassland"),
  list(index = "SPEI", habitat = "Grassland"),
  list(index = "NDMI", habitat = "Forest"),
  list(index = "SPI", habitat = "Forest"),
  list(index = "SPEI", habitat = "Forest"),
  list(index = "SPI", habitat = "Heathland")
  
)

# dunns test for all habitat group, index and year combos
for (combo in posthoc_combos) {
  index = combo$index
  habitat = combo$habitat
  
  cat("\n=====================================\n")
  cat("Dunn's Test for", index, "in", habitat, "(Year vs Year)\n")
  cat("=====================================\n")
  
  # filtering and dropping NAs
  df = data %>%
    filter(Habitat == habitat) %>%
    select(Year, all_of(index)) %>%
    filter(!is.na(.data[[index]]))
  
  # Run Dunn's test
  dunn_result = dunn.test(
    x = df[[index]],
    g = df$Year,
    method = "none" 
  )
  
  print(dunn_result)
}










