
diabetes <- read.csv("C:\\Users\\paava\\OneDrive\\Desktop\\Data Science\\PDS2\\rawdata//diabetes.csv")


diabetes_clean <- diabetes


sum(is.na(diabetes_clean))


diabetes_clean$Glucose <- ifelse(diabetes_clean$Glucose == 0, median(diabetes_clean$Glucose), diabetes_clean$Glucose)
diabetes_clean$BloodPressure <- ifelse(diabetes_clean$BloodPressure == 0, median(diabetes_clean$BloodPressure), diabetes_clean$BloodPressure)
diabetes_clean$SkinThickness <- ifelse(diabetes_clean$SkinThickness == 0, median(diabetes_clean$SkinThickness), diabetes_clean$SkinThickness)
diabetes_clean$Insulin <- ifelse(diabetes_clean$Insulin == 0, median(diabetes_clean$Insulin), diabetes_clean$Insulin)
diabetes_clean$BMI <- ifelse(diabetes_clean$BMI == 0, median(diabetes_clean$BMI), diabetes_clean$BMI)

library(dplyr)
diabetes_clean %>% 
  select(-Outcome) %>% 
  boxplot()


diabetes_clean <- diabetes_clean %>%
  filter(Glucose <= 200 & BloodPressure <= 120 & BMI <= 60)


summary(diabetes_clean)


write.csv(diabetes_clean, "C:\\Users\\paava\\OneDrive\\Desktop\\Data Science\\PDS2\\cleandata\\diabetes_cleaned.csv", row.names = FALSE)

diabetes_clean = read.csv("C:\\Users\\paava\\OneDrive\\Desktop\\Data Science\\PDS2\\cleandata\\diabetes_cleaned.csv")




set.seed(1234)


sample_data <- diabetes_clean[sample(nrow(diabetes_clean), 25), ]


mean_glucose <- mean(sample_data$Glucose)
max_glucose <- max(sample_data$Glucose)


result_df <- data.frame(mean_glucose, max_glucose)


write.table(result_df, file = "C:\\Users\\paava\\OneDrive\\Desktop\\Data Science\\PDS2\\results\\Mean_Max_Glucosevalues", row.names = FALSE)


pop_mean_glucose <- mean(diabetes_clean$Glucose)
pop_max_glucose <- max(diabetes_clean$Glucose)


result_df <- data.frame(pop_mean_glucose, pop_max_glucose)

write.table(result_df, file = "C:\\Users\\paava\\OneDrive\\Desktop\\Data Science\\PDS2\\results\\Mean_Max_Population", row.names = FALSE)


mean_data <- data.frame(Type = c("Population", "Sample"),
                        Glucose = c(pop_mean_glucose, mean_glucose))
library(ggplot2)
ggplot(mean_data, aes(x = Type, y = Glucose, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Comparison of Mean Glucose Values") +
  xlab("") +
  ylab("Mean Glucose") +
  theme_bw()


ggsave("C:\\Users\\paava\\OneDrive\\Desktop\\Data Science\\PDS2\\results\\Mean_Comparison.png")


max_data <- data.frame(Type = c("Population", "Sample"),
                       Glucose = c(pop_max_glucose, max_glucose))

ggplot(max_data, aes(x = Type, y = Glucose, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Comparison of Highest Glucose Values") +
  xlab("") +
  ylab("Highest Glucose") +
  theme_bw()

ggsave("C:\\Users\\paava\\OneDrive\\Desktop\\Data Science\\PDS2\\results\\Max_Comparison.png")





sample_bmi_percentile <- quantile(sample_data$BMI, 0.98)
pop_bmi_percentile <- quantile(diabetes_clean$BMI, 0.98)

write.table(c(sample_bmi_percentile, pop_bmi_percentile), file = "C:\\Users\\paava\\OneDrive\\Desktop\\Data Science\\PDS2\\results//bmi_percentiles.txt")

percentile_data <- data.frame(Type = c("Population", "Sample"),
                              BMI = c(pop_bmi_percentile, sample_bmi_percentile))
ggplot(percentile_data, aes(x = BMI, fill = Type)) +
  geom_histogram(binwidth = 1, alpha = 0.5, position = "dodge") +
  ggtitle("Comparison of 98th Percentile of BMI Values") +
  xlab("98th Percentile of BMI") +
  ylab("Count") +
  theme_bw()

ggsave("C:\\Users\\paava\\OneDrive\\Desktop\\Data Science\\PDS2\\results\\compare_98thpercentile.png")





num_samples <- 500
sample_size <- 150


bootstrap_data <- matrix(nrow = num_samples, ncol = sample_size)


for (i in 1:num_samples) {
  bootstrap_sample <- sample(diabetes_clean$BloodPressure, sample_size, replace = TRUE)
  bootstrap_data[i, ] <- bootstrap_sample
}


bootstrap_means <- apply(bootstrap_data, 1, mean)
bootstrap_mean_avg <- mean(bootstrap_means)
bootstrap_mean_sd <- sd(bootstrap_means)
bootstrap_percentile <- quantile(bootstrap_data, 0.98)


result_df <- data.frame(bootstrap_mean_avg, bootstrap_mean_sd, bootstrap_percentile)


write.table(result_df, file = "C:\\Users\\paava\\OneDrive\\Desktop\\Data Science\\PDS2\\results\\bootstrap_mean_avg_sd_percentile_values", row.names = FALSE)


pop_mean <- mean(diabetes_clean$BloodPressure)
pop_sd <- sd(diabetes_clean$BloodPressure)
pop_percentile <- quantile(diabetes_clean$BloodPressure, 0.98)


result_df <- data.frame(pop_mean, pop_sd, pop_percentile)

write.table(result_df, file = "C:\\Users\\paava\\OneDrive\\Desktop\\Data Science\\PDS2\\results\\pop_mean_sd_percentile_values", row.names = FALSE)



mean_data <- data.frame(Type = c("Population", "Bootstrap"),
                        Mean = c(pop_mean, bootstrap_mean_avg))
ggplot(mean_data, aes(x = Type, y = Mean, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Comparison of Mean BloodPressure Values") +
  xlab("") +
  ylab("Mean BloodPressure") +
  theme_bw()


ggsave("C:\\Users\\paava\\OneDrive\\Desktop\\Data Science\\PDS2\\results\\mean_bloodpressure_values_comparison.png")


sd_data <- data.frame(Type = c("Population", "Bootstrap"),
                      SD = c(pop_sd, bootstrap_mean_sd))

ggplot(sd_data, aes(x="", y=SD, fill=Type)) +
  geom_bar(stat="identity", width=1) +
  coord_polar(theta="y") +
  ggtitle("Comparison of Standard Deviation of Blood Pressure Values") +
  xlab("") +
  ylab("") +
  theme_void() +
  scale_fill_manual(values=c("#1f77b4","#ff7f0e"))

ggsave("C:\\Users\\paava\\OneDrive\\Desktop\\Data Science\\PDS2\\results\\sd_bloodpressure_comparison.png")




percentile_data <- data.frame(Type = c("Population", "Bootstrap"),
                              BloodPressure = c(pop_percentile, bootstrap_percentile))
ggplot(percentile_data, aes(x = "", y = BloodPressure, fill = Type)) +
  geom_col(width = 1) +
  ggtitle("Comparison of 98th Percentile of BloodPressure Values") +
  xlab("") +
  ylab("") +
  theme_bw() +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("Population" = "green", "Bootstrap" = "purple"))
ggsave("C:\\Users\\paava\\OneDrive\\Desktop\\Data Science\\PDS2\\results\\bloodpressure_comparison_piechart.png")

