library(stargazer)
library(tidyr)
library(dplyr)
library(ggplot2)

#get df
final_df <- read.csv("/Users/chiara/Documents/GitHub/Thesis-2025-Hertie/final_df.csv")
final_df$date <- as.Date(paste0(final_df$date, "-01"))

##GENERAL
stargazer(final_df, type = "html", title="Descriptive statistics", digits=1, out="table1.htm")


##PIRACY
#distribution over time
agg_monthly_df <- final_df %>%
  mutate(YearMonth = format(date, "%Y-%m")) %>%  # Create a Year-Month column
  group_by(YearMonth) %>%
  summarise(MonthlyEventsCount = sum(EventsCount, na.rm = TRUE)) %>%
  ungroup()

dist_plot_density <- ggplot(agg_monthly_df, aes(x = MonthlyEventsCount)) +
  geom_density(fill = "gray", alpha = 0.6) +
  labs(title = "Monthly Density Distribution of Total Piracy Event Counts", 
       x = "Monthly Piracy Events Count", 
       y = "Density") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", family = "Times New Roman"),
    axis.title = element_text(face = "bold", family = "Times New Roman"),
    axis.text = element_text(size = 12, family = "Times New Roman"),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels by 45°
  )
ggsave("m_eez.agg_piracy_events_density_distribution.png", dist_plot_density, width = 12, height = 8)


#logged monthly
agg_monthly_df$log_MonthlyEventsCount <- log(agg_monthly_df$MonthlyEventsCount+1)

log_dist_plot_density <- ggplot(agg_monthly_df, aes(x = log_MonthlyEventsCount)) +
  geom_density(fill = "gray", alpha = 0.6) +
  labs(title = "Logged Monthly Density Distribution of Total Event Counts", 
       x = "Logged Piracy Events Count", 
       y = "Density") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", family = "Times New Roman"),
    axis.title = element_text(face = "bold", family = "Times New Roman"),
    axis.text = element_text(size = 12, family = "Times New Roman"),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels by 45°
  )
ggsave("log_m_eez.agg_piracy_events_density_distribution.png", log_dist_plot_density, width = 12, height = 8)

#yearly-> not to be logged
#final_df$year <- format(final_df$date, "%Y")
#yearly_counts <- final_df %>%
  #group_by(year) %>%
  #summarise(total_events = sum(EventsCount, na.rm = TRUE)) %>%
  #ungroup()

#y_piracy_dens_plot<- ggplot(yearly_counts, aes(x = total_events)) +
  #geom_density(fill = "gray", alpha = 0.6, color = "white") +
  #theme_minimal(base_size = 14, base_family = "Times") +
  #labs(
    #title = "Distribution of Total Events per Year",
    #x = "Total Events",
    #y = "Density"
  #) +
  #theme(
  #  plot.title = element_text(hjust = 0.5, face = "bold")
  #)
#ggsave("yearly_event_distribution.png", log_dist_plot_density, width = 12, height = 8)



##IUU Fishing
m_IUU_dist_plot<- ggplot(final_df, aes(x = IUU_EventsCount)) +
  geom_density(fill = "gray", alpha = 0.6, color = "white", na.rm = TRUE) +
  theme_minimal(base_size = 14, base_family = "Times") +
  labs(
    title = "Monthly Distribution of IUU Events Count",
    x = "IUU Events Count",
    y = "Density"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
ggsave("m_IUU_dist_plot.png", m_IUU_dist_plot, width = 12, height = 8)

#logged
final_df$log_IUU_EventsCount <- log(final_df$IUU_EventsCount+1)

log_m_IUU_dist_plot<- ggplot(final_df, aes(x = log_IUU_EventsCount)) +
  geom_density(fill = "gray", alpha = 0.6, color = "white", na.rm = TRUE) +
  theme_minimal(base_size = 14, base_family = "Times") +
  labs(
    title = "Logged Monthly Distribution of IUU Events Count",
    x = "Logged IUU Events Count",
    y = "Density"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
ggsave("log_m_IUU_dist_plot.png", log_m_IUU_dist_plot, width = 12, height = 8)

##AVG_ANOM
avg_anom_plot<- ggplot(final_df, aes(x = date, y = avg_anom)) +
  geom_area(aes(fill = avg_anom > 0), alpha = 0.4, show.legend = FALSE) +
  geom_line(color = "black", linewidth = 0.8) +
  geom_smooth(method = "loess", se = FALSE, color = "darkred", size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_fill_manual(values = c("TRUE" = "tomato", "FALSE" = "skyblue")) +
  labs(
    title = "Temperature Anomalies Over Time",
    x = "Date",
    y = "Temperature Anomaly (°C)"
  ) +
  theme_minimal(base_family = "Times") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )

ggsave("avg_anom_plot.png", avg_anom_plot, width = 12, height = 8)



#COLOMBIA
colombia_df <- final_df %>% 
  filter(eez_country=="COL")

#piracy and IUU, density
colombia_density_data <- final_df %>%
  filter(eez_country == "COL") %>%
  pivot_longer(cols = c(EventsCount, IUU_EventsCount),
               names_to = "variable",
               values_to = "value") %>%
  filter(!is.na(value))  # Remove NA values

colombia_density_plot <- ggplot(colombia_density_data, aes(x = value)) +
  geom_density(fill = "gray", alpha = 0.6) +
  facet_wrap(~ variable, scales = "free", ncol = 1) +
  labs(
    title = "Colombia: Density plot Piracy and IUU",
    x = "Value",
    y = "Density"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", family = "Times New Roman"),
    axis.title = element_text(face = "bold", family = "Times New Roman"),
    axis.text = element_text(size = 12, family = "Times New Roman"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("col_density_p_IUU_facet.png", colombia_density_plot, width = 12, height = 8)

#piracy and IUU, time series
col_p_IUU_time <- ggplot(colombia_density_data, aes(x = date, y = value)) +
  geom_line(color = "black", size = 0.8) +
  geom_smooth(method = "loess", se = FALSE, color = "darkred", size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~ variable, scales = "free", ncol = 1) +
  labs(
    title = "Colombia: Piracy and IUU Fishing Over Time",
    x = "Date",
    y = "Events Count"
  ) +
  theme_minimal(base_family = "Times") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")  # Title in bold
  )

#compared timelines
colombia_p_eez_compared <- colombia_density_data %>% 
  filter(date >= as.Date("2017-01-01") & date <= as.Date("2019-12-31"))
  
col_p_IUU_comp_time <- ggplot(colombia_p_eez_compared, aes(x = date, y = value)) +
  geom_line(color = "black", size = 0.8) +
  geom_smooth(method = "loess", se = FALSE, color = "darkred", size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~ variable, scales = "free", ncol = 1) +
  labs(
    title = "Colombia: Piracy and IUU Fishing Over Time",
    x = "Date",
    y = "Events Count"
  ) +
  theme_minimal(base_family = "Times") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")  # Title in bold
  )

#avg_anom: time series
col_avg_anom_plot <- ggplot(colombia_df, aes(x = date, y = avg_anom)) +
  geom_area(aes(fill = avg_anom > 0), alpha = 0.4, show.legend = FALSE) +
  geom_line(color = "black", size = 0.8) +
  geom_smooth(method = "loess", se = FALSE, color = "darkred", size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_fill_manual(values = c("TRUE" = "tomato", "FALSE" = "skyblue")) +
  labs(
    title = "Colombia: Temperature Anomalies Over Time",
    x = "Date",
    y = "Temperature Anomaly (°C)"
  ) +
  theme_minimal(base_family = "Times") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")  # Title in bold
  )

ggsave("col_avg_anom_plot.png", avg_anom_plot, width = 12, height = 8)


#