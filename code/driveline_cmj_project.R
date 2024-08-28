#Using data from Driveline Baseball's OpenBiomechanics Project, I will be comparing bat speed (mph) to specific variables from a counter-movement jump (CMJ)
#The CMJ variables will be peak power (W), peak force (N --> concentric and eccentric), and body weight (lbs)
#Citation: Wasserberger et al. (2022). "The OpenBiomechanics Project". Driveline Baseball

#Opening required libraries
install.packages("corrplot")
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)

#Importing the data:
file_path = "/Users/stevenleicht/Desktop/db_cmj_data.csv"
cmj_data = read.csv(file_path)
head(cmj_data)

#Selecting the data I want to analyze:
selected_data = cmj_data %>%
  dplyr::select(bat_speed_mph, peak_power_.w._mean_cmj,
         concentric_peak_force_.n._mean_cmj, eccentric_peak_force_.n._mean_cmj, body_weight_.lbs.)
print(head(selected_data))

#Creating a multiple linear regression model to measure the correlation between all dependent variables and bat speed:
mlr_model = lm(bat_speed_mph ~ peak_power_.w._mean_cmj + concentric_peak_force_.n._mean_cmj + 
                 eccentric_peak_force_.n._mean_cmj + body_weight_.lbs., data = selected_data)
summary(mlr_model)
#Residual standard error = 4.113 mph on 908 degrees of freedom
#Adjusted r-squared = 0.7111... meaning that the model is a strong fit
#P-value is < 0.001... meaning that it is pretty statistically significant

#Creating a linear regression model to compare bat speed with each variable (individually):

#First model will compare bat speed with peak power:
bs_pp = lm(bat_speed_mph ~ peak_power_.w._mean_cmj, data = selected_data)
summary(bs_pp)
#Residual standard error of 4.308 mph on 911 degrees of freedom
#Adjusted r-squared value of 0.6831
#P-value < 0.001

#Creating a visual to show the relationship between bat speed and peak power:
ggplot(selected_data, aes(x = bat_speed_mph, y = peak_power_.w._mean_cmj)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Relationship Between Bat Speed and Peak Power",
       x = "Bat Speed (mph)",
       y = "Peak Power (W)") +
  theme_minimal()
#Conclusion: as bat speed increases, so does peak power output

#Now trying the same thing with bat speed and concentric peak force:
bs_cpf = lm(bat_speed_mph ~ concentric_peak_force_.n._mean_cmj, data = selected_data)
summary(bs_cpf)
#Residual standard error of 4.701 mph on 911 degrees of freedom
#Adjusted r-squared value of 0.6227
#P-value < 0.001

#Creating a visual to show the relationship between bat speed and concentric peak force:
ggplot(selected_data, aes(x = bat_speed_mph, y = concentric_peak_force_.n._mean_cmj)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Relationship Between Bat Speed and Concentric Peak Force",
       x = "Bat Speed (mph)",
       y = "Concentric Peak Force (N)") +
  theme_minimal()
#Conclusion: as bat speed increases, so does concentric peak force

#Now trying with eccentric peak force:
bs_epf = lm(bat_speed_mph ~ eccentric_peak_force_.n._mean_cmj, data = selected_data)
summary(bs_epf)
#Residual standard error of 4.776 mph on 911 degrees of freedom
#Adjusted r-squared value of 0.6106
#P-value < 0.001

#Creating a visual to show the relationship between bat speed and eccentric peak force:
ggplot(selected_data, aes(x = bat_speed_mph, y = eccentric_peak_force_.n._mean_cmj)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Relationship Between Bat Speed and Eccentric Peak Force",
       x = "Bat Speed (mph)",
       y = "Eccentric Peak Force (N)") +
  theme_minimal()
#Conclusion: as bat speed increases, so does eccentric peak force

#Now trying it while combining both concentric and eccentric peak forces:
bs_pf = lm(bat_speed_mph ~ eccentric_peak_force_.n._mean_cmj, 
            concentric_peak_force_.n._mean_cmj, data = selected_data)
summary(bs_pf)
#Residual standard error of 6.418 mph on 70 degrees of freedom
#Adjusted r-squared value of 0.6038
#P-value < 0.001

#Creating a visual to show the relationship between bat speed and peak force:
ggplot() +
  geom_point(data = selected_data, aes(x = bat_speed_mph, y = eccentric_peak_force_.n._mean_cmj), color = "blue") +
  geom_smooth(data = selected_data, aes(x = bat_speed_mph, y = eccentric_peak_force_.n._mean_cmj), 
              method = "lm", color = "blue", se = FALSE) +
  geom_point(data = selected_data, aes(x = bat_speed_mph, y = concentric_peak_force_.n._mean_cmj), color = "green") +
  geom_smooth(data = selected_data, aes(x = bat_speed_mph, y = concentric_peak_force_.n._mean_cmj), 
              method = "lm", color = "green", se = FALSE) +
  labs(title = "Relationship Between Bat Speed and Peak Force",
       x = "Bat Speed (mph)",
       y = "Peak Force (N)") +
  theme_minimal()
#Conclusion: as bat speed increases, so does overall peak force

#Now trying with bat speed and body weight:
bs_bw = lm(bat_speed_mph ~ body_weight_.lbs., data = selected_data)
summary(bs_bw)
#Residual standard error of 5.253 mph on 911 degrees of freedom
#Adjusted r-squared value of 0.5289
#P-value < 0.001
  
#Creating a visual to show the relationship between bat speed and body weight:
ggplot(selected_data, aes(x = bat_speed_mph, y = body_weight_.lbs.)) +
    geom_point(color = "blue") +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    labs(title = "Relationship Between Bat Speed and Body Weight",
         x = "Bat Speed (mph)",
         y = "Body Weight (lbs)") +
  theme_minimal()
#Conclusion: generally, as bat speed increases, so does body weight

#Creating a facet plot to show the relationship between bat speed and all other variables together:
#Converting and combining the data to long format for visualization purposes:
long_data = selected_data %>%
  select(bat_speed_mph, peak_power_.w._mean_cmj, 
         concentric_peak_force_.n._mean_cmj, 
         eccentric_peak_force_.n._mean_cmj, body_weight_.lbs.) %>%
  pivot_longer(cols = -bat_speed_mph, 
               names_to = "variable", 
               values_to = "values")

#Creating the facet plot:
ggplot(long_data, aes(x = bat_speed_mph, y = values)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "Relationship Between Bat Speed and All Other Variables",
       x = "Bat Speed (mph)",
       y = "Values") +
  theme_minimal()

#Creating a correlation matrix to determine how much each variable correlates to bat speed:
cor_matrix = cor(selected_data, use = "complete.obs")
print(cor_matrix)
#There is a strong positive correlation (0.8267) between bat speed and peak power. This means that the higher the bat speed, the higher the peak power during a CMJ (and vice versa)
#There is a strong positive correlation (0.7894) between bat speed and concentric peak force. This means that the higher the bat speed, the higher the concentric peak force (and vice versa)
#There is a strong positive correlation (0.7817) between bat speed and eccentric peak force. This means that the higher the bat speed, the higher the eccentric peak force (and vice versa)
#There is a positive correlation (0.7276) between bat speed and body weight. This means that - generally - the higher the bat speed, the higher the body weight (and vice versa)

#Creating a heat-map to visualize the correlation matrix:
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", # Add correlation coefficient values
         col = colorRampPalette(c("blue", "white", "red"))(200))

#Overall conclusion:
#Peak power and peak force during a counter-movement jump (CMJ) have a strong positive correlation to bat speed
#Body weight has a positive correlation to bat speed
#Obviously, performing a CMJ and swinging a bat are two completely different movements
#There are always anomalies to take into consideration and athlete development should be more specified, but having a philosophy that focuses on increasing the dependent variables is a statistically respectable approach to overall improvement of the player

