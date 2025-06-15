# Install required packages if not already installed
# install.packages(c("EpiEstim", "incidence", "ggplot2", "dplyr", "lubridate"))

library(EpiEstim)
library(incidence)
library(ggplot2)
library(dplyr)
library(lubridate)

# Load the data
data <- read.csv("C:/Users/user/Desktop/Deng/Dengue_Daily_EN.csv")

# Convert onset date to Date type
data$Date_Onset <- as.Date(data$Date_Onset, format = "%Y/%m/%d")

# Filter out NA dates
data <- data[!is.na(data$Date_Onset), ]

# Create incidence object (case counts)
cases <- incidence(data$Date_Onset)

# Get daily counts
daily_cases <- as.data.frame(cases)
colnames(daily_cases) <- c("date", "cases")

# Define the serial interval (assumed; adjust if better estimates are available)
mean_si <- 4.7
std_si <- 2.9

##################################################
### Rt estimation: Piecewise constant (14-day) ###
##################################################
res_window <- estimate_R(
  incid = daily_cases$cases,
  method = "parametric_si",
  config = make_config(list(
    mean_si = mean_si,
    std_si = std_si,
    t_start = seq(2, nrow(daily_cases) - 13),
    t_end = seq(15, nrow(daily_cases))
  ))
)

# Extract results
Rt_window <- res_window$R
Rt_window$method <- "14-day Window"
Rt_window$date <- daily_cases$date[Rt_window$t_end]

###############################################
### Rt estimation: Smoothed (Random Walk) #####
###############################################
res_rw <- estimate_R(
  incid = daily_cases$cases,
  method = "parametric_si",
  config = make_config(list(
    mean_si = mean_si,
    std_si = std_si
  ))
)

Rt_smooth <- res_rw$R
Rt_smooth$method <- "Random Walk"
Rt_smooth$date <- daily_cases$date[Rt_smooth$t_end]

#######################################
### Combine both Rt estimates #########
#######################################
Rt_all <- bind_rows(Rt_window, Rt_smooth)

##################################
### Plot Rt and case counts #####
##################################
ggplot() +
  geom_col(data = daily_cases, aes(x = date, y = cases), fill = "gray80", alpha = 0.6) +
  geom_line(data = Rt_all, aes(x = date, y = `Mean(R)`, color = method), size = 1.1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Effective Reproduction Number (Rt) Estimates",
    subtitle = "Comparison of 14-day Window vs Random Walk Smoothing",
    x = "Date", y = "Rt / Daily Cases",
    color = "Estimation Method"
  ) +
  scale_color_manual(values = c("14-day Window" = "blue", "Random Walk" = "darkgreen")) +
  theme_minimal() +
  theme(legend.position = "top")

##########################################
### Combine and Plot Both Rt Estimates ###
##########################################


Rt_all <- bind_rows(Rt_window, Rt_smooth)

ggplot(Rt_all, aes(x = date, y = `Mean(R)`, color = method, fill = method)) +
  geom_line(size = 1.1) +
  geom_ribbon(aes(ymin = `Quantile.0.025(R)`, ymax = `Quantile.0.975(R)`), alpha = 0.2, color = NA) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Effective Reproduction Number (Rt) Estimates",
    subtitle = "Comparison of 14-day Window and Random Walk Approaches",
    x = "Date", y = "Rt",
    color = "Estimation Method", fill = "Estimation Method"
  ) +
  scale_color_manual(values = c("14-day Window" = "blue", "Random Walk" = "darkgreen")) +
  scale_fill_manual(values = c("14-day Window" = "blue", "Random Walk" = "darkgreen")) +
  theme_minimal() +
  theme(legend.position = "top")