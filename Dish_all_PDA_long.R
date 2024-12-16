

df_Dish_longcvs <- read.csv("/... Dish_all_PDA_longcsv.csv", sep=";")

head(df_Dish_longcvs)
library(tidyverse)

# install.packages("devtools")
devtools::install_github("r-lib/conflicted")
library(conflicted)
library(dplyr)

str(df_Dish_longcvs)
colnames (df_Dish_longcvs)
#remove the NA columns X, X.1, X.2, X.3)

#df_Dish_longcvs <- df_Dish_longcvs %>% 
#  select (-df_Dish_longcvs$X, -df_Dish_longcvs$X.1, -df_Dish_longcvs$X.2, 
#          -df_Dish_longcvs$X.3)

df_Dish_longcvs$X <-NULL
df_Dish_longcvs$X.1<-NULL
df_Dish_longcvs$X.2 <-NULL
df_Dish_longcvs$X.3 <-NULL


#df <- df[, colSums(is.na(df)) == 0] 

class(df_Dish_longcvs$Values_mm)
df_Dish_longcvs$Values_mm <- as.numeric(df_Dish_longcvs$Values_mm, na.rm = TRUE)

#descriptive statistics
shapiro.test(df_Dish_longcvs$Values_mm)
hist(df_Dish_longcvs$Values_mm)

df_Dish_longcvs$Treatment_seed <-factor (df_Dish_longcvs$Treatment_seed, 
                                       levels = c("H2O", "NB1", "NB2", "NB3", "NB5", "NB6", "NB7", "NB8", "NB9", "ABA09",
                                                  "ABF11","ABE11", "LAG16", "PVA", "Proline"))
levels (df_Dish_longcvs$Treatment_seed)
class (df_Dish_longcvs$Treatment_seed)

df_Dish_longcvs$Supp_activity <- factor (df_Dish_longcvs$Supp_activity)
levels (df_Dish_longcvs$Supp_activity)
class (df_Dish_longcvs$Supp_activity)

df_Dish_longcvs$Exudates <- factor (df_Dish_longcvs$Exudates)
levels (df_Dish_longcvs$Exudates)
class (df_Dish_longcvs$Exudates)


df_Dish_longcvs$Timepoint <- as.numeric(df_Dish_longcvs$Timepoint)
class(df_Dish_longcvs)
df_Dish_longcvs

sapply(df_Dish_longcvs, class)


#Simple Anova analysis (One-way-ANOVA)
# Ensure the necessary packages are installed
install.packages("dplyr")
install.packages("car")  # For Anova() function
install.packages("ggplot2")

library(dplyr)
library(car)
library(ggplot2)

#filter the data from multiple Timepoints
filtered_data <- df_Dish_longcvs %>%
  dplyr::filter(Timepoint %in% c(3, 5, 7))
filtered_data

#Check if there are any NA values in filtered data
has_na <- anyNA(filtered_data$Values_mm)
#has_na <- anyNA(f_data)

na_count <-colSums(is.na(filtered_data))
print(na_count)

filtered_data [filtered_data =="na" | filtered_data == "NA" | filtered_data == "" |filtered_data == " "] <-NA
na_count <-colSums(is.na(filtered_data))
print (na_count)

#replace specific missing values representations with NA
filtered_data_clean <-filtered_data %>%
  dplyr::filter (!is.na(Values_mm))

class(filtered_data_clean$Values_mm)
str (filtered_data_clean)
filtered_data_clean$Values_mm <- as.numeric(filtered_data_clean$Values_mm)
#filtered1_data$Values_mm <- as.numeric(filtered1_data$Values_mm)

#Print the results
if (has_na) {
  print ("Trere are NA values in the filtered data")
} else {
    print ("Trere are no NA")
  }

#Count the number of NA values in the filtered data
na_count <-is.na(filtered_data_clean)

#Print the namber of NA values
print(paste("Total number of NA values in the filtered data:", na_count))






ancova_result <- aov(Values_mm ~Treatment_seed +Timepoint, data = filtered_data_clean)
summary(ancova_result)

#Df Sum Sq Mean Sq  F value   Pr(>F)    
#Treatment_seed   12    459      38    2.906 0.000564 ***
#  Timepoint         1  15644   15644 1187.683  < 2e-16 ***
#  Residuals      1121  14766      13                      
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



#ancova for d3,5,7
f_data <- filtered_data_clean %>%
  dplyr::filter(Treatment_seed %in% c('H2O', 'NB1', 'NB2', 'NB3', 'NB5', 'NB6', 'NB7', 'NB8', 'NB9', 'LAG16'))

ancova_result <- aov(Values_mm ~Treatment_seed +Timepoint, data = f_data)
summary(ancova_result)

#Df Sum Sq Mean Sq F value  Pr(>F)    
#Treatment_seed   9    296      33   2.522 0.00741 ** 
#  Timepoint        1  11629   11629 892.113 < 2e-16 ***
#  Residuals      888  11576      13                    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



#ANcova for d3
f_data_d3 <- filtered_data_clean %>%
  dplyr::filter(Timepoint %in% c(3))
filtered_data

ancova_result <- aov(Values_mm ~Treatment_seed, data = f_data_d3)
summary(ancova_result)

#  Df Sum Sq Mean Sq F value Pr(>F)    
#Treatment_seed  14   1612  115.15   29.94 <2e-16 ***
#  Residuals      432   1662    3.85                   
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



#f_data <- filtered1_data %>%
#  dplyr::filter(Treatment_seed %in% c('H2O', 'NB1'))

# Load necessary library
library(ggplot2)

# Perform ANCOVA
ancova_result <- aov(Values_mm ~ Treatment_seed+Timepoint, data = f_data)
summary(ancova_result)

# Get the p-value for the Treatment_seed factor
p_value <- summary(ancova_result)[[1]]["Treatment_seed", "Pr(>F)"]

# Generate significance label based on p-value
signif_label <- ifelse(p_value < 0.001, "***",
                       ifelse(p_value < 0.01, "**",
                              ifelse(p_value < 0.05, "*", "ns")))

# Plot with significance annotation
ggplot(f_data, aes(x = Timepoint, y = Values_mm, color = Treatment_seed)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +  # Adds regression lines with confidence intervals
  labs(title = "ANCOVA: Values_mm vs. Timepoint by Treatment",
       x = "Timepoint",
       y = "Values_mm",
       color = "Treatment Seed") +
  annotate("text", x = Inf, y = Inf, label = paste("p =", signif(p_value, digits = 3), signif_label), 
           hjust = 1.1, vjust = 2, size = 5, color = "black") +
  theme_bw()

# Plot with significance annotation
ggplot(f_data, aes(x = Timepoint, y = Values_mm, fill = Treatment_seed)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  #geom_smooth(method = "lm", se = FALSE) +  # Adds regression lines with confidence intervals
  labs(title = "ANCOVA: Values_mm vs. Timepoint by Treatment",
       x = "Timepoint",
       y = "Values_mm",
       color = "Treatment Seed") +
  annotate("text", x = Inf, y = Inf, label = paste("p =", signif(p_value, digits = 3), signif_label), 
           hjust = 1.1, vjust = 2, size = 5, color = "black") +
  theme_bw()


# Plot with significance annotation for day 3
ggplot(f_data, aes(x = Treatment_seed, y = Values_mm, fill = Treatment_seed)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  #geom_smooth(method = "lm", se = FALSE) +  # Adds regression lines with confidence intervals
  labs(title = "ANCOVA: Values_mm vs. Timepoint by Treatment",
       x = "Treatment",
       y = "Values_mm",
       color = "Treatment Seed") +
  annotate("text", x = Inf, y = Inf, label = paste("p =", signif(p_value, digits = 2), 
                                                   signif_label), 
           hjust = 1.1, vjust = 2, size = 5, color = "black") +
  theme_bw()


# Assuming you have the p-value and significance levels ready
p_value <- 5.5e-55  # Replace with your actual p-value
signif_label <- "***"  # Based on the p-value thresholds





# 1Plot with significance annotation for day 3
ggplot(f_data, aes(x = Treatment_seed, y = Values_mm, fill = Treatment_seed)) + 
  geom_boxplot() + 
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") + 
  labs(title = "ANCOVA: Fungicide activity vs. Treatment",
       x = "Treatment",
       y = "Values_mm",
       color = "Treatment Seed") + 
  annotate("text", x = Inf, y = Inf, label = paste("p =", signif(p_value, digits = 2), signif_label),
           hjust = 1.1, vjust = 2, size = 5, color = "black") + 
  theme_bw() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))


#2plot 2 attempt

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Calculate the ANCOVA result
ancova_result <- aov(Values_mm ~ Treatment_seed + Timepoint, data = f_data)
summary(ancova_result)

# Get the p-value for the Treatment_seed factor
p_value <- summary(ancova_result)[[1]]["Treatment_seed", "Pr(>F)"]

# Generate significance label based on p-value
signif_label <- ifelse(p_value < 0.001, "***",
                       ifelse(p_value < 0.01, "**",
                              ifelse(p_value < 0.05, "*", "ns")))

# Create the plot with significance annotation
ggplot(f_data, aes(x = Treatment_seed, y = Values_mm, fill = Treatment_seed)) + 
  geom_boxplot() + 
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") + 
  labs(title = "ANCOVA: Values_mm vs. Treatment Seed",
       x = "Treatment",
       y = "Values_mm",
       fill = "Treatment Seed") + 
  annotate("text", x = Inf, y = Inf, label = paste("p =", signif(p_value, digits = 2), signif_label),
           hjust = 1.1, vjust = 2, size = 5, color = "black") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"))




# ANOVA for day3 for paiwise comparison

# Filter the data for the 3rd day
day3_data <- f_data %>% dplyr::filter(Timepoint == 3)

# Perform one-way ANOVA
anova_result <- aov(Values_mm ~ Treatment_seed, data = day3_data)
summary(anova_result)

#Df Sum Sq Mean Sq F value   Pr(>F)    
#Treatment_seed   9  150.5  16.726   5.044 2.47e-06 ***
#  Residuals      290  961.6   3.316                     
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# Perform Tukey's HSD post-hoc test
tukey_result <- TukeyHSD(anova_result)

#Tukey multiple comparisons of means
#95% family-wise confidence level##

#Fit: aov(formula = Values_mm ~ Treatment_seed, data = day3_data)

#$Treatment_seed
#diff        lwr         upr     p adj
#NB1-H2O    2.333333e+00  0.8341506  3.83251604 0.0000516
#NB2-H2O    2.066667e+00  0.5674840  3.56584937 0.0006463
#NB3-H2O    1.800000e+00  0.3008173  3.29918271 0.0060385
#NB5-H2O    2.400000e+00  0.9008173  3.89918271 0.0000263
#NB6-H2O    1.800000e+00  0.3008173  3.29918271 0.0060385
#NB7-H2O    1.733333e+00  0.2341506  3.23251604 0.0100363
#NB8-H2O    1.633333e+00  0.1341506  3.13251604 0.0206391
#NB9-H2O    9.333333e-01 -0.5658494  2.43251604 0.6102363
#LAG16-H2O  8.666667e-01 -0.6325160  2.36584937 0.7069659
#NB2-NB1   -2.666667e-01 -1.7658494  1.23251604 0.9999126
#NB3-NB1   -5.333333e-01 -2.0325160  0.96584937 0.9807642
#NB5-NB1    6.666667e-02 -1.4325160  1.56584937 1.0000000
#NB6-NB1   -5.333333e-01 -2.0325160  0.96584937 0.9807642
#NB7-NB1   -6.000000e-01 -2.0991827  0.89918271 0.9582333
#NB8-NB1   -7.000000e-01 -2.1991827  0.79918271 0.8957906
#NB9-NB1   -1.400000e+00 -2.8991827  0.09918271 0.0901155
#LAG16-NB1 -1.466667e+00 -2.9658494  0.03251604 0.0610360
#NB3-NB2   -2.666667e-01 -1.7658494  1.23251604 0.9999126
#NB5-NB2    3.333333e-01 -1.1658494  1.83251604 0.9994468
#NB6-NB2   -2.666667e-01 -1.7658494  1.23251604 0.9999126
#NB7-NB2   -3.333333e-01 -1.8325160  1.16584937 0.9994468
#NB8-NB2   -4.333333e-01 -1.9325160  1.06584937 0.9956808
#NB9-NB2   -1.133333e+00 -2.6325160  0.36584937 0.3234717
#LAG16-NB2 -1.200000e+00 -2.6991827  0.29918271 0.2456738
#NB5-NB3    6.000000e-01 -0.8991827  2.09918271 0.9582333
#NB6-NB3   -6.217249e-15 -1.4991827  1.49918271 1.0000000
#NB7-NB3   -6.666667e-02 -1.5658494  1.43251604 1.0000000
#NB8-NB3   -1.666667e-01 -1.6658494  1.33251604 0.9999985
#NB9-NB3   -8.666667e-01 -2.3658494  0.63251604 0.7069659
#LAG16-NB3 -9.333333e-01 -2.4325160  0.56584937 0.6102363
#NB6-NB5   -6.000000e-01 -2.0991827  0.89918271 0.9582333
#NB7-NB5   -6.666667e-01 -2.1658494  0.83251604 0.9209034
#NB8-NB5   -7.666667e-01 -2.2658494  0.73251604 0.8321572
#NB9-NB5   -1.466667e+00 -2.9658494  0.03251604 0.0610360
#LAG16-NB5 -1.533333e+00 -3.0325160 -0.03415063 0.0402901
#NB7-NB6   -6.666667e-02 -1.5658494  1.43251604 1.0000000
#NB8-NB6   -1.666667e-01 -1.6658494  1.33251604 0.9999985
#NB9-NB6   -8.666667e-01 -2.3658494  0.63251604 0.7069659
#LAG16-NB6 -9.333333e-01 -2.4325160  0.56584937 0.6102363
#NB8-NB7   -1.000000e-01 -1.5991827  1.39918271 1.0000000
#NB9-NB7   -8.000000e-01 -2.2991827  0.69918271 0.7939665
#LAG16-NB7 -8.666667e-01 -2.3658494  0.63251604 0.7069659
#NB9-NB8   -7.000000e-01 -2.1991827  0.79918271 0.8957906
#LAG16-NB8 -7.666667e-01 -2.2658494  0.73251604 0.8321572
#LAG16-NB9 -6.666667e-02 -1.5658494  1.43251604 1.0000000



# Convert the Tukey HSD result to a data frame
tukey_pvalues <- as.data.frame(tukey_result$Treatment_seed)
tukey_pvalues$Treatment_Comparison <- rownames(tukey_pvalues)  # Add comparison column



# Extract p-values and significance levels
tukey_pvalues <- as.data.frame(tukey_result$Treatment_seed)
tukey_pvalues$signif <- ifelse(tukey_pvalues$`p.adj` < 0.001, "***",
                               ifelse(tukey_pvalues$`p.adj` < 0.01, "**",
                                      ifelse(tukey_pvalues$`p.adj` < 0.05, "*", "ns")))

#Split the Treatment_Comparison into two separate treatment columns
tukey_pvalues <- tukey_pvalues %>%
  separate(Treatment_Comparison, into = c("Treatment_seed", "Comparison"), sep = "-")

#Filter comparisons to only involving H2O
tukey_pvalues <- tukey_pvalues %>% dplyr::filter(Comparison=="H2O")


#Merge the significance labels with the day3 data
day3_data <- day3_data %>% 
  left_join(tukey_pvalues, by = "Treatment_seed")


str(tukey_pvalues)
head (day3_data)

# Add significance labels based on adjusted p-values
if ("p adj" %in% colnames(tukey_pvalues)) {
  tukey_pvalues$signif <- ifelse(tukey_pvalues$`p adj` < 0.001, "***",
                                 ifelse(tukey_pvalues$`p adj` < 0.01, "**",
                                        ifelse(tukey_pvalues$`p adj` < 0.05, "*", "ns")))
} else {
  stop("The column 'p adj' was not found in the Tukey HSD results.")
}


# Create the plot with significance annotations
ggplot(day3_data, aes(x = Treatment_seed, y = Values_mm, fill = Treatment_seed)) +
  geom_boxplot() + 
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  geom_text(data = tukey_pvalues, aes(label = signif, y = max(day3_data$Values_mm) + 1), 
            position = position_dodge (width = 0.75), vjust = 0.75, color = "red", size = 5) + 
  labs(title = "One-Way ANOVA with Tukey HSD Post-Hoc Test",
       subtitle = "Significance of Treatment Effects Compared to H2O on Day 3",
       x = "Treatment",
       y = "Values_mm",
       fill = "Treatment Seed") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 14))




















#Filter the data to include only the treatmrnts and control
f_data1 <- filtered_data_clean %>%
  dplyr::filter(Treatment_seed %in% c('H2O', 'NB1', 'NB2', 'NB3', 'NB5', 'NB6', 'NB7', 'NB8', 'NB9','LAG16'))


anova_result <- aov (Values_mm ~ Treatment_seed, data = f_data)
summary(anova_result)

posthoc <-TukeyHSD (anova_result)
print(posthoc)

#Check for missing data
missing_data_summary <- colSums(is.na(f_data))
print (missing_data_summary)

names (f_data)

#t-test analysis
t_data <- f_data %>%
  dplyr::filter (Treatment_seed %in% c("H2O", "NB1"))

t_test_result <-t.test (Values_mm ~ Treatment_seed, data = t_data)
print (t_test_result)


t_data <- f_data %>%
  dplyr::filter (Treatment_seed %in% c("H2O", "NB2"))
t_test_result <-t.test (Values_mm ~ Treatment_seed, data = t_data)
print (t_test_result)

t_data <- f_data %>%
  dplyr::filter (Treatment_seed %in% c("H2O", "NB3"))
t_test_result <-t.test (Values_mm ~ Treatment_seed, data = t_data)
print (t_test_result)

t_data <- f_data %>%
  dplyr::filter (Treatment_seed %in% c("H2O", "NB5"))
t_test_result <-t.test (Values_mm ~ Treatment_seed, data = t_data)
print (t_test_result)

t_data <- f_data %>%
  dplyr::filter (Treatment_seed %in% c("H2O", "NB6"))
t_test_result <-t.test (Values_mm ~ Treatment_seed, data = t_data)
print (t_test_result)

t_data <- f_data %>%
  dplyr::filter (Treatment_seed %in% c("H2O", "NB7"))
t_test_result <-t.test (Values_mm ~ Treatment_seed, data = t_data)
print (t_test_result)

t_data <- f_data %>%
  dplyr::filter (Treatment_seed %in% c("H2O", "NB8"))
t_test_result <-t.test (Values_mm ~ Treatment_seed, data = t_data)
print (t_test_result)

t_data <- f_data %>%
  dplyr::filter (Treatment_seed %in% c("H2O", "NB9"))
t_test_result <-t.test (Values_mm ~ Treatment_seed, data = t_data)
print (t_test_result)


t_data <- f_data %>%
  dplyr::filter (Treatment_seed %in% c("H2O", "ABA09"))
t_test_result <-t.test (Values_mm ~ Treatment_seed, data = t_data)
print (t_test_result)

levels(t_data$Treatment_seed)
str(t_data)
t_data <- f_data %>%
  dplyr::filter (Treatment_seed %in% c("H2O", "ABF11"))
t_test_result <-t.test (Values_mm ~ Treatment_seed, data = t_data)
print (t_test_result)

t_test_result <-t.test (Values_mm ~ Treatment_seed, data = t_data)
print (t_test_result)

t_data <- f_data %>%
  dplyr::filter (Treatment_seed %in% c("H2O", "NB1"))

t_test_result <-t.test (Values_mm ~ Treatment_seed, data = t_data)
print (t_test_result)

t_data <- f_data %>%
  dplyr::filter (Treatment_seed %in% c("H2O", "NB1"))

t_test_result <-t.test (Values_mm ~ Treatment_seed, data = t_data)
print (t_test_result)

t_data <- f_data %>%
  dplyr::filter (Treatment_seed %in% c("H2O", "NB1"))

t_test_result <-t.test (Values_mm ~ Treatment_seed, data = t_data)
print (t_test_result)

t_data <- f_data %>%
  dplyr::filter (Treatment_seed %in% c("H2O", "NB1"))

t_test_result <-t.test (Values_mm ~ Treatment_seed, data = t_data)
print (t_test_result)

t_data <- f_data %>%
  dplyr::filter (Treatment_seed %in% c("H2O", "NB1"))

t_test_result <-t.test (Values_mm ~ Treatment_seed, data = t_data)
print (t_test_result)

t_data <- f_data %>%
  dplyr::filter (Treatment_seed %in% c("H2O", "NB1"))

t_test_result <-t.test (Values_mm ~ Treatment_seed, data = t_data)
print (t_test_result)






# Perform ANOVA for each comparison (H2O vs. NB1, H2O vs. NB2, etc.)
anova_results <- list()
treatment_levels <- levels(df_Dish_longcvs$Treatment_seed)

for (i in 2:length(treatment_levels)) {
  # Subset data for H2O vs. each NB treatment
  subset_data <- df_Dish_longcvs %>%
    dplyr::filter(Treatment_seed %in% c("H2O", treatment_levels[i]))
  
  # Perform ANOVA
  anova_result <- aov(Values_mm ~ Treatment_seed, data = subset_data)
  anova_summary <- summary(anova_result)
  
  # Save the result to the list
  anova_results[[paste("H2O vs", treatment_levels[i])]] <- anova_summary
}

# Print the ANOVA results
anova_results

#$`H2O vs NB1`
#Df Sum Sq Mean Sq F value Pr(>F)  
#Treatment_seed   1    277  277.44   5.668 0.0177 *
#  Residuals      410  20068   48.95                 
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#248 observations deleted due to missingness

#$`H2O vs NB3`
#Df Sum Sq Mean Sq F value Pr(>F)  
#Treatment_seed   1    139  138.99   2.789 0.0957 .
#Residuals      400  19937   49.84                 
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#258 observations deleted due to missingness

#$`H2O vs Proline`
#Df Sum Sq Mean Sq F value   Pr(>F)    
#Treatment_seed   1   3570    3570   69.98 7.16e-16 ***
#  Residuals      462  23568      51                     
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#196 observations deleted due to missingness

# Create a boxplot to visualize the ANOVA results
ggplot(df_Dish_longcvs, aes(x = Treatment_seed, y = Values_mm)) +
  geom_boxplot(aes(fill = Treatment_seed)) +  # Create boxplot
  labs(title = "Boxplot of Values_mm by Treatment_seed",
       x = "Treatment Seed",
       y = "Fusarium Growth (Values_mm)") +
  theme_minimal() +  # Use a minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability


# Q-Q plot
ggplot(df_Dish_longcvs, aes(sample = Values_mm)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "Q-Q Plot of Values_mm") +
  theme_minimal()




#####---------Performance of T-test for df_noProPVA pairwase comparison

# Assuming df_Dish_clean is your dataframe and it contains a Treatment_seed factor with two levels.
# Ensure Treatment_seed has only two levels (e.g., "Treatment_A" and "Treatment_B")
df_TwoGroups <- df_noProPVA %>%
  dplyr::filter(Treatment_seed %in% c("H2O", "NB1"))

shapiro_test_A <- shapiro.test(df_TwoGroups$Values_mm[df_TwoGroups$Treatment_seed == "H2O"])
shapiro_test_B <- shapiro.test(df_TwoGroups$Values_mm[df_TwoGroups$Treatment_seed == "NB1"])
print(shapiro_test_A)
print(shapiro_test_B)

# Perform Independent Samples T-Test
t_test_result <- t.test(Values_mm ~ Treatment_seed, data = df_TwoGroups)

# Print the result
print(t_test_result)

#Welch Two Sample t-test.  H2O vs. NB1 - significant (and no any else)

#data:  Values_mm by Treatment_seed
#t = -2.3447, df = 365.87, p-value = 0.01958
#alternative hypothesis: true difference in means between group H2O and group NB1 is not equal to 0
#95 percent confidence interval:
#  -3.0280031 -0.2656596
#sample estimates:
#  mean in group H2O mean in group NB1 
#1.169312          2.816143 


# Ensure Treatment_seed has only two levels (e.g., "Treatment_A" and "Treatment_B")
df_TwoGroups <- df_noProPVA %>%
  dplyr::filter(Treatment_seed %in% c("H2O", "NB3"))
# Perform Independent Samples T-Test
t_test_result <- t.test(Values_mm ~ Treatment_seed, data = df_TwoGroups)

# Print the result
print(t_test_result)





####--------F-test. to compare the variances of values_mm between H2O and each Treatment group 


# Load necessary libraries
library(dplyr)

# Initialize a list to store F-test results
f_test_results <- list()

# Get the unique treatment levels excluding "H2O"
treatment_levels <- unique(df_noProPVA$Treatment_seed)
treatment_levels <- treatment_levels[treatment_levels != "H2O"]

df_noProPVA_filtered <-df_noProPVA %>%
  dplyr::filter (!Treatment_seed %in% c("H2O", "Proline", "PVA"))
levels(df_noProPVA_filtered$Treatment_seed)


#Filter the data to exclude "H2O", "Proline", and "PVA"
df_noProPVA_filtered <- df_noProPVA %>%
  dplyr::filter(!Treatment_seed %in% c("H2O", "Proline", "PVA"))

# Drop unused levels from the Treatment_seed factor
df_noProPVA_filtered$Treatment_seed <- droplevels(df_noProPVA_filtered$Treatment_seed)

# Check the levels to confirm they have been removed
levels(df_noProPVA_filtered$Treatment_seed)



# Manually inspect the data for "H2O" and "LAG16"
  subset_data_LAG16 <- df_noProPVA %>%
    filter(Treatment_seed %in% c("H2O", "LAG16"))
  
  # Print to see if both levels are present
  print(subset_data_LAG16)


##########----------3.attempt

# Initialize a list to store F-test results
f_test_results <- list()

# Loop over each treatment level and perform the F-test against "H2O"
for (i in seq_along(treatment_levels)) {
  # Subset data for H2O vs. each NB treatment
  subset_data <- df_noProPVA %>%
    filter(Treatment_seed %in% c("H2O", as.character(treatment_levels[i])))
  
  # Print the levels present in the subset
  print(paste("Comparing H2O vs", treatment_levels[i], "with levels:", unique(subset_data$Treatment_seed)))
  
  # Check if there are exactly 2 levels in the subset data
  if (length(unique(subset_data$Treatment_seed)) == 2) {
    # Perform F-Test
    f_test_result <- var.test(Values_mm ~ Treatment_seed, data = subset_data)
    
    # Save the result to the list
    f_test_results[[paste("H2O vs", treatment_levels[i])]] <- f_test_result
  } else {
    # Print a message if there are not exactly 2 levels
    print(paste("Skipping:", treatment_levels[i], "due to insufficient levels"))
  }
}

# Print the F-test results for each comparison
f_test_results

#$`H2O vs NB1`. - significant

#F test to compare two variances

#data:  Values_mm by Treatment_seed
#F = 1.4529, num df = 188, denom df = 222, p-value = 0.007542
#alternative hypothesis: true ratio of variances is not equal to 1
#95 percent confidence interval:
#  1.104815 1.916841
#sample estimates:
#  ratio of variances 
#1.452933 


#$`H2O vs NB3`. -significant

#F test to compare two variances

#data:  Values_mm by Treatment_seed
#F = 1.408, num df = 188, denom df = 212, p-value = 0.01563
#alternative hypothesis: true ratio of variances is not equal to 1
#95 percent confidence interval:
#  1.067029 1.862345
#sample estimates:
#  ratio of variances 
#1.408012 






#####----------Friedman test (I have non-parametric and unequal Values_mm data )
# Check the structure of your data to ensure it has all necessary combinations
library(dplyr)
library(tidyr)

# Check completeness
df_noProPVA_complete <- df_noProPVA %>%
  complete(Replicates, Treatment_seed)

# Check for complete cases within each replicate
df_noProPVA_complete <- df_noProPVA %>%
  group_by(Replicates) %>%
  dplyr::filter(all(!is.na(Values_mm))) %>%
  ungroup()

# Run the Friedman test with the cleaned data
friedman_test_result <- friedman.test(Values_mm ~ Treatment_seed | Replicates, data = df_noProPVA_complete)

# Print the result
print(friedman_test_result)

#Simple mean imputation for missing Values_mm
df_noProPVA_complete <- df_noProPVA %>%
  group_by(Treatment_seed, Replicates) %>%
  mutate(Values_mm = ifelse(is.na(Values_mm), mean(Values_mm, na.rm = TRUE), Values_mm)) %>%
  ungroup()

#count the number of treatments per replicate
completeness_check <- df_noProPVA %>%
  group_by (Replicates) %>%
  summarise (count = n_distinct(Treatment_seed))
print (completeness_check)

# Assuming each replicate should have the same number of treatments
required_treatments <- n_distinct(df_noProPVA$Treatment_seed)

# Filter out incomplete blocks
df_noProPVA_complete <- df_noProPVA %>%
  group_by(Replicates) %>%
  dplyr::filter(n_distinct(Treatment_seed) == required_treatments) %>%
  ungroup()

# Run the Friedman test with the imputed data
friedman_test_result <- friedman.test(Values_mm ~ Treatment_seed | Replicates, data = df_noProPVA_complete)

# Print the result
print(friedman_test_result)

#######----no results!!!!!!





##### ---------PERMANOVA analysis 
install.packages("vegan")

library(vegan)

# Convert the Treatment_seed to a factor (ensure it is already a factor)
df_Dish_longcvs$Treatment_seed <- as.factor(df_Dish_longcvs$Treatment_seed)

# Check for missing values
sum(is.na(df_Dish_longcvs$Values_mm))  # Count missing values in Values_mm

# Remove rows with missing Values_mm
df_Dish_clean <- df_Dish_longcvs[!is.na(df_Dish_longcvs$Values_mm), ]
str(df_Dish_clean)
# Perform PERMANOVA with cleaned data
permanova_results <- adonis2(df_Dish_clean$Values_mm ~ df_Dish_clean$Treatment_seed, data = df_Dish_clean, method = "euclidean") # permutations = 199)

# Print the PERMANOVA results
print(permanova_results)


####____permutations=199
#Permutation test for adonis under reduced model
#Terms added sequentially (first to last)
#Permutation: free
#Number of permutations: 199

#adonis2(formula = df_Dish_clean$Values_mm ~ df_Dish_clean$Treatment_seed, data = df_Dish_clean, permutations = 199, method = "euclidean")
#Df SumOfSqs      R2      F Pr(>F)   
#df_Dish_clean$Treatment_seed   14     6820 0.04149 9.2379  0.005 **
#  Residual                     2988   157563 0.95851                 
#Total                        3002   164383 1.00000                 
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


####--------permutations=999
#Permutation test for adonis under reduced model
#Terms added sequentially (first to last)
#Permutation: free
#Number of permutations: 999

#adonis2(formula = df_Dish_clean$Values_mm ~ df_Dish_clean$Treatment_seed, data = df_Dish_clean, method = "euclidean")
#Df SumOfSqs      R2      F Pr(>F)    
#df_Dish_clean$Treatment_seed   14     6820 0.04149 9.2379  0.001 ***
#  Residual                     2988   157563 0.95851                  
#Total                        3002   164383 1.00000                  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1





#####_________PERMANOVA for df_noProPVA

df_noProPVA <- df_Dish_clean %>% dplyr::filter(!df_Dish_clean$Treatment_seed %in% c("Proline", "PVA"))
unique (df_noProPVA$Treatment_Seed)

# Perform PERMANOVA with cleaned data
permanova_results <- adonis2(df_noProPVA$Values_mm ~ df_noProPVA$Treatment_seed, data = df_noProPVA, method = "euclidean") # permutations = 199)

# Print the PERMANOVA results
print(permanova_results)

#Permutation test for adonis under reduced model
#Terms added sequentially (first to last)
#Permutation: free
#Number of permutations: 999

#adonis2(formula = df_noProPVA$Values_mm ~ df_noProPVA$Treatment_seed, data = df_noProPVA, method = "euclidean")
#Df SumOfSqs     R2      F Pr(>F)
#df_noProPVA$Treatment_seed   12      682 0.0051 1.0863   0.35
#Residual                   2542   133008 0.9949              
#Total                      2554   133690 1.0000    


####------Permanova for df_Dish_clean include two more factors: Supp_activity, Timepoint

# Ensure the Supp_activity and Timepoint are properly formatted
df_Dish_clean$Supp_activity <- as.factor(df_Dish_clean$Supp_activity)
df_Dish_clean$Timepoint <- as.numeric(df_Dish_clean$Timepoint)

# Perform PERMANOVA with additional factors
permanova_results <- adonis2(df_Dish_clean$Values_mm ~ df_Dish_clean$Treatment_seed + df_Dish_clean$Supp_activity + df_Dish_clean$Timepoint, 
                             data = df_Dish_clean, method = "euclidean")

# Print the PERMANOVA results
print(permanova_results)

#Permutation test for adonis under reduced model
#Terms added sequentially (first to last)
#Permutation: free
#Number of permutations: 999

#adonis2(formula = df_Dish_clean$Values_mm ~ df_Dish_clean$Treatment_seed + df_Dish_clean$Supp_activity + df_Dish_clean$Timepoint, data = df_Dish_clean, method = "euclidean")
#Df SumOfSqs      R2       F Pr(>F)    
#df_Dish_clean$Treatment_seed   14     6820 0.04149  11.356  0.001 ***
#  df_Dish_clean$Supp_activity     1     4761 0.02896 110.988  0.001 ***
#  df_Dish_clean$Timepoint         1    24717 0.15036 576.227  0.001 ***
#  Residual                     2986   128085 0.77919                   
#Total                        3002   164383 1.00000                   
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1




####------Permanova for df_noProPVA include two more factors: Supp_activity, Timepoint

# Ensure the Supp_activity and Timepoint are properly formatted
df_noProPVA$Supp_activity <- as.factor(df_noProPVA$Supp_activity)
df_noProPVA$Timepoint <- as.numeric(df_noProPVA$Timepoint)

# Perform PERMANOVA with additional factors
permanova_results <- adonis2(df_noProPVA$Values_mm ~ df_noProPVA$Treatment_seed + df_noProPVA$Supp_activity + df_noProPVA$Timepoint, 
                             data = df_noProPVA, method = "euclidean")

# Print the PERMANOVA results
print(permanova_results)

#Permutation test for adonis under reduced model
#Terms added sequentially (first to last)
#Permutation: free
#Number of permutations: 999

#adonis2(formula = df_noProPVA$Values_mm ~ df_noProPVA$Treatment_seed + df_noProPVA$Supp_activity + df_noProPVA$Timepoint, data = df_noProPVA, method = "euclidean")
#Df SumOfSqs      R2        F Pr(>F)    
#df_noProPVA$Treatment_seed   12      682 0.00510   1.2964  0.217    
#df_noProPVA$Supp_activity     1     3839 0.02871  87.5533  0.001 ***
#  df_noProPVA$Timepoint         1    17810 0.13322 406.2334  0.001 ***
#  Residual                   2540   111360 0.83297                    
#Total                      2554   133690 1.00000                    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



#####----- to check which Treatment has stat signiricance from each we perform Pairwise Wilcoxon Test
# Pairwise Wilcoxon Test for Treatment_seed
pairwise_wilcox_test <- pairwise.wilcox.test(df_noProPVA$Values_mm, df_noProPVA$Treatment_seed+df_noProPVA$Timepoint, 
                                             p.adjust.method = "bonferroni")
print(pairwise_wilcox_test)

#Pairwise comparisons using Wilcoxon rank sum test with continuity correction 

#data:  df_noProPVA$Values_mm and df_noProPVA$Treatment_seed 

#         H2O  NB1  NB2  NB3  NB5  NB6  NB7  NB8  NB9  ABA09 ABF11 ABE11
#  NB1   1.00   -    -    -    -    -    -    -    -    -     -     -    
#  NB2   1.00 1.00   -    -    -    -    -    -    -    -     -     -    
#  NB3   1.00 1.00 1.00   -    -    -    -    -    -    -     -     -    
#  NB5   1.00 1.00 1.00 1.00   -    -    -    -    -    -     -     -    
#  NB6   1.00 1.00 1.00 1.00 1.00   -    -    -    -    -     -     -    
#  NB7   1.00 1.00 1.00 1.00 1.00 1.00   -    -    -    -     -     -    
#  NB8   1.00 1.00 1.00 1.00 1.00 1.00 1.00   -    -    -     -     -    
#  NB9   1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00   -    -     -     -    
#  ABA09 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00   -     -     -    
#  ABF11 1.00 0.22 1.00 1.00 1.00 1.00 1.00 0.45 1.00 1.00    -     -    
#  ABE11 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00  1.00    -    
#  LAG16 1.00 0.24 1.00 1.00 1.00 1.00 1.00 0.53 1.00 1.00  1.00  1.00 

#P value adjustment method: bonferroni  


# Perform Wilcoxon tests stratified by Timepoint
stratified_wilcox_results <- df_noProPVA %>%
  group_by(Timepoint) %>%
  do(pairwise_wilcox_test = pairwise.wilcox.test(.$Values_mm, .$Treatment_seed, 
                                                 p.adjust.method = "bonferroni"))

# Print results for each Timepoint
stratified_wilcox_results$pairwise_wilcox_test

#day3 [[2]]

#Pairwise comparisons using Wilcoxon rank sum test with continuity correction 

#data:  .$Values_mm and .$Treatment_seed 

#           H2O     NB1     NB2     NB3     NB5     NB6     NB7     NB8     NB9     ABA09   ABF11   ABE11  
#  NB1   0.00090 -       -       -       -       -       -       -       -       -       -       -      
#  NB2   0.00056 1.00000 -       -       -       -       -       -       -       -       -       -      
#  NB3   0.06634 1.00000 1.00000 -       -       -       -       -       -       -       -       -      
#  NB5   0.00036 1.00000 1.00000 1.00000 -       -       -       -       -       -       -       -      
#  NB6   0.09571 1.00000 1.00000 1.00000 1.00000 -       -       -       -       -       -       -      
#  NB7   0.03073 1.00000 1.00000 1.00000 1.00000 1.00000 -       -       -       -       -       -      
#  NB8   0.15828 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 -       -       -       -       -      
#  NB9   1.00000 0.29826 1.00000 1.00000 0.48895 1.00000 1.00000 1.00000 -       -       -       -      
#  ABA09 1.00000 0.67764 1.00000 1.00000 0.29723 1.00000 1.00000 1.00000 1.00000 -       -       -      
#  ABF11 1.00000 0.17275 0.46418 1.00000 0.04466 1.00000 1.00000 1.00000 1.00000 1.00000 -       -      
#  ABE11 0.52908 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 -      
#  LAG16 1.00000 0.12061 0.16266 1.00000 0.02678 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000

# P value adjustment method: bonferroni 


# Perform Wilcoxon tests stratified by Sup_activity
stratified_wilcox_results <- df_noProPVA %>%
  group_by(Supp_activity) %>%
  do(pairwise_wilcox_test = pairwise.wilcox.test(.$Values_mm, .$Treatment_seed, 
                                                 p.adjust.method = "bonferroni"))

# Print results for each Sup_activity
stratified_wilcox_results$pairwise_wilcox_test

#yes - [[2]]

#Pairwise comparisons using Wilcoxon rank sum test with continuity correction 

#data:  .$Values_mm and .$Treatment_seed 

# H2O      NB1   NB2   NB3   NB5   NB6   NB7   NB8   NB9   ABA09 ABF11 ABE11
#  NB1   1.000 -     -     -     -     -     -     -     -     -     -     -    
#  NB2   1.000 0.806 -     -     -     -     -     -     -     -     -     -    
#  NB3   1.000 1.000 1.000 -     -     -     -     -     -     -     -     -    
#  NB5   1.000 1.000 1.000 1.000 -     -     -     -     -     -     -     -    
#  NB6   1.000 1.000 0.489 1.000 1.000 -     -     -     -     -     -     -    
#  NB7   1.000 1.000 0.020 0.079 0.751 1.000 -     -     -     -     -     -    
#  NB8   1.000 1.000 0.012 0.029 0.359 1.000 1.000 -     -     -     -     -    
#  NB9   1.000 1.000 1.000 1.000 1.000 1.000 0.480 0.463 -     -     -     -    
#  ABA09 1.000 1.000 1.000 1.000 1.000 1.000 0.111 0.079 1.000 -     -     -    
#  ABF11 1.000 1.000 1.000 1.000 1.000 1.000 0.222 0.156 1.000 1.000 -     -    
#  ABE11 1.000 1.000 0.516 1.000 1.000 1.000 1.000 1.000 1.000 1.000 1.000 -    
#  LAG16 1.000 0.274 1.000 1.000 1.000 0.327 0.015 0.012 1.000 1.000 1.000 0.537

#P value adjustment method: bonferroni




#####------------Boxplot Visualization (not really good one!)
# Load ggplot2
install.packages("ggplot2")
library(ggplot2)

# Boxplot for Treatment_seed: distribution of "Values_mm" across different treatment groups
ggplot(df_noProPVA, aes(x = Treatment_seed, y = Values_mm, fill = Treatment_seed)) +
  geom_boxplot() +
  labs(title = "Boxplot of Values_mm by Treatment_seed",
       x = "Treatment Seed",
       y = "Values_mm") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot for Supp_activity: distribution of "Values_mm" across different treatment groups with Supress_activity factor
ggplot(df_noProPVA, aes(x = Supp_activity, y = Values_mm, fill = Supp_activity)) +
  geom_boxplot() +
  labs(title = "Boxplot of Values_mm by Supp_activity",
       x = "Supp Activity",
       y = "Values_mm") +
  theme_minimal()

# Boxplot for Timepoint: distribution of "Values_mm" across different treatment groups with changes across different time points 
ggplot(df_noProPVA, aes(x = factor(Timepoint), y = Values_mm, fill = factor(Timepoint))) +
  geom_boxplot() +
  labs(title = "Boxplot of Values_mm by Timepoint",
       x = "Timepoint",
       y = "Values_mm") +
  theme_minimal()





#########------Kruskal-Wallis test for non-parametric data set

kruskal_test <- kruskal.test(Values_mm ~ Treatment_seed, data = df_noProPVA)
print(kruskal_test)

#Kruskal-Wallis rank sum test

#data:  Values_mm by Treatment_seed
#Kruskal-Wallis chi-squared = 23.036, df = 12, p-value = 0.02742





##########------ Pairwise Wilcoxon Rank-Sum Test (Mann-Whitney U Test)
pairwise_wilcox_test <- pairwise.wilcox.test(df_noProPVA$Values_mm, df_noProPVA$Treatment_seed,
                                             p.adjust.method = "bonferroni")
print(pairwise_wilcox_test)


#Pairwise comparisons using Wilcoxon rank sum test with continuity correction 

#data:  df_noProPVA$Values_mm and df_noProPVA$Treatment_seed 

#.       H2O  NB1  NB2  NB3  NB5  NB6  NB7  NB8  NB9  ABA09 ABF11 ABE11
#  NB1   1.00 -    -    -    -    -    -    -    -    -     -     -    
#  NB2   1.00 1.00 -    -    -    -    -    -    -    -     -     -    
#  NB3   1.00 1.00 1.00 -    -    -    -    -    -    -     -     -    
#  NB5   1.00 1.00 1.00 1.00 -    -    -    -    -    -     -     -    
#  NB6   1.00 1.00 1.00 1.00 1.00 -    -    -    -    -     -     -    
#  NB7   1.00 1.00 1.00 1.00 1.00 1.00 -    -    -    -     -     -    
#  NB8   1.00 1.00 1.00 1.00 1.00 1.00 1.00 -    -    -     -     -    
#  NB9   1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 -    -     -     -    
#  ABA09 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 -     -     -    
#  ABF11 1.00 0.22 1.00 1.00 1.00 1.00 1.00 0.45 1.00 1.00  -     -    
#  ABE11 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00  1.00  -    
#  LAG16 1.00 0.24 1.00 1.00 1.00 1.00 1.00 0.53 1.00 1.00  1.00  1.00 

# P value adjustment method: bonferroni 

pairwise_wilcox_test <- pairwise.wilcox.test(df_Dish_longcvs$Values_mm, df_Dish_longcvs$Treatment_seed,
                                             p.adjust.method = "bonferroni")
print(pairwise_wilcox_test)

#Pairwise comparisons using Wilcoxon rank sum test with continuity correction 

#data:  df_Dish_longcvs$Values_mm and df_Dish_longcvs$Treatment_seed 

#           H2O     NB1     NB2     NB3     NB5     NB6     NB7     NB8     NB9     ABA09   ABF11   ABE11   LAG16   PVA    
#  NB1     1.00    -       -       -       -       -       -       -       -       -       -       -       -       -      
#  NB2     1.00    1.00    -       -       -       -       -       -       -       -       -       -       -       -      
#  NB3     1.00    1.00    1.00    -       -       -       -       -       -       -       -       -       -       -      
#  NB5     1.00    1.00    1.00    1.00    -       -       -       -       -       -       -       -       -       -      
#  NB6     1.00    1.00    1.00    1.00    1.00    -       -       -       -       -       -       -       -       -      
#  NB7     1.00    1.00    1.00    1.00    1.00    1.00    -       -       -       -       -       -       -       -      
#  NB8     1.00    1.00    1.00    1.00    1.00    1.00    1.00    -       -       -       -       -       -       -      
#  NB9     1.00    1.00    1.00    1.00    1.00    1.00    1.00    1.00    -       -       -       -       -       -      
#  ABA09   1.00    1.00    1.00    1.00    1.00    1.00    1.00    1.00    1.00    -       -       -       -       -      
#  ABF11   1.00    0.30    1.00    1.00    1.00    1.00    1.00    0.60    1.00    1.00    -       -       -       -      
#  ABE11   1.00    1.00    1.00    1.00    1.00    1.00    1.00    1.00    1.00    1.00    1.00    -       -       -      
#  LAG16   1.00    0.32    1.00    1.00    1.00    1.00    1.00    0.71    1.00    1.00    1.00    1.00    -       -      
#  PVA     1.00    1.00    1.00    1.00    1.00    1.00    1.00    1.00    1.00    1.00    1.00    1.00    1.00    -      
#  Proline 7.4e-16 5.0e-12 1.0e-15 6.4e-14 1.0e-12 2.5e-12 2.0e-12 5.3e-12 5.9e-15 5.0e-14 4.0e-16 2.2e-10 < 2e-16 1.5e-10

#P value adjustment method: bonferroni 



#######---------Boxplots for KW test 

#Load ggplot2 library
library(ggplot2)

# Boxplot for Kruskal-Wallis Test
ggplot(df_Dish_longcvs, aes(x = Treatment_seed, y = Values_mm)) +
  geom_boxplot(aes(fill = Treatment_seed)) +
  labs(title = "Boxplot of Values_mm by Treatment_seed (Kruskal-Wallis Test)",
       x = "Treatment Seed",
       y = "Fusarium Growth (Values_mm)") +
  theme() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#######---------Boxplots with Compact Letter Display (CLD) for KW test and W M-W test bonferroni
# Install and load necessary packages
install.packages("rstatix")
install.packages("ggplot2")
install.packages("rcompanion")
library(rcompanion)
library(rstatix)
library(ggplot2)

# Perform pairwise Wilcoxon test and add significance letters (Compact Letter Display - CLD)
pairwise_wilcox_results <- df_Dish_longcvs %>%
  pairwise_wilcox_test(Values_mm ~ Treatment_seed, p.adjust.method = "bonferroni") 

#Create a summery table
pairwise_summary <- pairwise_wilcox_results %>%
  select (group1, group2, p.adj) %>%
  mutate(p.adj = ifelse(is.na(p.adj), 1, p.adj))
 
#Create a summery column which combines group1 and group2 with a hyphen
pairwise_summary <- pairwise_summary %>%
  mutate(comparison = paste (group1, group2, sep = "-"))
          
#Use the cldfuntion to generate the CLD
cld <- cldList (p.adj ~ comparison, data = pairwise_summary, threshold = 0.05)

#Print the CLD
print(cld)

#Group Letter MonoLetter
#1      H2O      a         a 
#2      NB1      a         a 
#3      NB2      a         a 
#4      NB3      a         a 
#5      NB5      a         a 
#6      NB6      a         a 
#7      NB7      a         a 
#8      NB8      a         a 
#9      NB9      a         a 
#10    ABA9      a         a 
#11   ABF11      a         a 
#12   ABE11      a         a 
#13   LAG16      a         a 
#14     PVA      a         a 
#15 Proline      b          b
 


#Merge CLD with original data for plotting
cld_df <- data.frame (Treatment_seed = cld$Group, CLD = cld$Letter)
df_Dish_longcvs <- df_Dish_longcvs %>%
  left_join(cld_df, by = "Treatment_seed")

head (df_Dish_longcvs)

# Plot with the significant letters
library(ggplot2)
ggplot(df_Dish_longcvs, aes (x=Treatment_seed, y=Values_mm))+
  geom_boxplot()+
  geom_text(aes(label = CLD, y = max (Values_mm) + 1), vjust = 0 )+
  labs (title = "Pairwise Wilcoxon Test with CLD",
        x = "Treatment seeds",
        y = "Values_mm") +
  theme()


print (cld_df)


###need to check the data (to get some with sign. differences)







#######----------Linear Mixed-Effect Model (LME)

df_noProPVA$CLD.Group.x<-NULL

df_noProPVA$CLD.Letter.x <-NULL
df_noProPVA$CLD.MonoLetter.x <-NULL
df_noProPVA$CLD.Group.y <-NULL
df_noProPVA$CLD.Letter.y <-NULL
df_noProPVA$CLD.MonoLetter.y <-NULL
df_noProPVA$CLD.MonoLetter <-NULL
df_noProPVA$CLD.Group <-NULL
df_noProPVA$CLD.Letter<-NULL
head (df_noProPVA)

install.packages("lme4")
install.packages("lmerTest")
install.packages("ggplot2")
install.packages("emmeans")
install.packages("ggsignif")

library(lme4)
library(lmerTest)
library(ggplot2)
library(emmeans)
library(ggsignif)

df_noProPVA$Treatment_seed <-factor (df_noProPVA$Treatment_seed, 
                                         levels = c("H2O", "NB1", "NB2", "NB3", "NB5", "NB6", "NB7", "NB8", "NB9", "ABA09",
                                                    "ABF11","ABE11", "LAG16")) 


#Fit the Linear Mixed_Effect (LME) Model
lme_model <-lmer (df_noProPVA$Values_mm ~ df_noProPVA$Treatment_seed + 
                    (1 | df_noProPVA$Timepoint), data = df_noProPVA)

#Display the summary of the model
summary (lme_model)

print(lme_model, correlation=TRUE)

#Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
#Formula: df_noProPVA$Values_mm ~ df_noProPVA$Treatment_seed + (1 | df_noProPVA$Timepoint)
#   Data: df_noProPVA

#REML criterion at convergence: 15226.1

#Scaled residuals: 
#    Min      1Q  Median      3Q     Max 
#-4.4663 -0.5094  0.0673  0.5573  2.5214 

#Random effects:
# Groups                Name        Variance Std.Dev.
# df_noProPVA$Timepoint (Intercept) 35.44    5.953   
# Residual                          22.09    4.700   
#Number of obs: 2555, groups:  df_noProPVA$Timepoint, 15

#Fixed effects:
#                                  Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)                        0.91830    1.57605   15.33438   0.583  0.56859    
#df_noProPVA$Treatment_seedNB1      1.81592    0.46512 2528.06019   3.904  9.7e-05 ***
#df_noProPVA$Treatment_seedNB2      0.39519    0.47589 2528.06373   0.830  0.40637    
#df_noProPVA$Treatment_seedNB3      1.24529    0.46997 2528.05652   2.650  0.00811 ** 
#df_noProPVA$Treatment_seedNB5      1.04259    0.47750 2528.03893   2.183  0.02909 *  
#df_noProPVA$Treatment_seedNB6      1.09326    0.47326 2528.07678   2.310  0.02097 *  
#df_noProPVA$Treatment_seedNB7      0.76419    0.48056 2528.05498   1.590  0.11191    
#df_noProPVA$Treatment_seedNB8      1.38858    0.47001 2528.06673   2.954  0.00316 ** 
#df_noProPVA$Treatment_seedNB9      0.82250    0.47471 2528.04427   1.733  0.08328 .  
#df_noProPVA$Treatment_seedABA09    0.57153    0.48853 2528.07822   1.170  0.24215    
#df_noProPVA$Treatment_seedABF11   -0.06500    0.48755 2528.03247  -0.133  0.89396    
#df_noProPVA$Treatment_seedABE11    0.44027    0.50901 2528.07805   0.865  0.38715    
#df_noProPVA$Treatment_seedLAG16    0.06816    0.48292 2528.03138   0.141  0.88777    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Create a summary of the model
lme_summary <-(lme_model)

#Extract fixed effects estimates (coefficients) and p-values
lme_summary <-summary (lme_model)
coefficients_df <- as.data.frame(lme_summary$coefficients)

#Add row names (which include Treatment_Seed levels) as a column
coefficients_df$Treatment_seed <-rownames(coefficients_df)

#Select relevant columns
coefficients_df <-coefficients_df %>%
  select (Treatment_seed, Estimate, `Std. Error`, `Pr(>|t|)`)
  
#Add significance indicators base on p-values
coefficients_df$signif <- cut (coefficients_df$`Pr(>|t|)`, 
                               breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                               labels = c("***", "**","*", ".", ""))

head(coefficients_df)

# Rename Treatment_seed levels
coefficients_df$Treatment_seed <- gsub("df_noProPVA\\$Treatment_seed", "", coefficients_df$Treatment_seed)

# # Rename the Intercept to H2O
coefficients_df$Treatment_seed[is.na(coefficients_df$Treatment_seed) | coefficients_df$Treatment_seed == "(Intercept)"] <- "H2O(Intercept)"
  
  
 #ifelse(coefficients_df$Treatment_seed == "(Intercept)", "H2O", coefficients_df$Treatment_seed)

#Remove the (Intersept) row
#coefficients_df <-coefficients_df %>% dplyr::filter(Treatment_seed !="(Intercept)")

#Reorder the factors levels
coefficients_df$Treatment_seed <-factor(coefficients_df$Treatment_seed,
                                        levels = c("H2O(Intercept)", "NB1", "NB2", "NB3", "NB5", "NB6", "NB7", "NB8", "NB9", "ABA09",
                                                   "ABF11","ABE11", "LAG16"))



# 1Plot the fixed effects with confidence intervals and significance stars (classical vuew)
ggplot(coefficients_df, aes(x = Treatment_seed, y = Estimate)) +
  geom_pointrange(aes(ymin = Estimate - `Std. Error`, ymax = Estimate + `Std. Error`), color = "blue", size = 1.2) +
  geom_text(aes(label = signif), vjust = -1.5, color = "red", size = 12) +
  labs(title = "Fixed Effects from LME Model",
       x = "Treatment Seed",
       y = "Estimated Effect on Values_mm") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#2Plot. Works
# Define the color palette with names corresponding to the levels in the Treatment_seed factor
my_palette <- c("H2O(Intercept)" = "#1f77b4", "NB1" = "#ff7f0e", "NB2" = "#2ca02c",
                "NB3" = "#d62728", "NB5" = "#9467bd", "NB6" = "#8c564b",
                "NB7" = "#e377c2", "NB8" = "#7f7f7f", "NB9" = "#bcbd22",
                "ABA09" = "#17becf", "ABF11" = "#aec7e8", "ABE11" = "#ffbb78",
                "LAG16" = "#98df8a")

# Ensure that Treatment_seed is a factor with the correct levels
coefficients_df$Treatment_seed <- factor(coefficients_df$Treatment_seed, levels = names(my_palette))

# Plot the fixed effects with confidence intervals and significance symbols
ggplot(coefficients_df, aes(x = Treatment_seed, y = Estimate, color = Treatment_seed)) +
  geom_pointrange(aes(ymin = Estimate - `Std. Error`, ymax = Estimate + `Std. Error`), size = 1.2) +
  geom_text(aes(label = signif), vjust = -2.5, color = "red", size = 12) +
  geom_errorbar(aes(ymin = Estimate - `Std. Error`, ymax = Estimate + `Std. Error`), width = 0.2, color = "black") +
  scale_color_manual(values = my_palette) + 
  labs(title = "Fixed Effects from LME Model: 
       Estimated Impact of Treatment on Fusarium Growth (Values_mm), 
       Controlling for Timepoint Variability",
       x = "Treatment Seed",
       y = "Estimated Effect on Values_mm",
       color = "Treatment Seed",
       caption = "Significance levels: *** p < 0.001, ** p < 0.01, * p < 0.05, . p < 0.1, '' p ≥ 0.1") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(hjust = 1, size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.caption = element_text(size = 14, hjust = 0.5, vjust = 2),
        legend.position = "right",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))



#3Plot. Add explanation in Subtitle in the Plot`s Caption

ggplot(coefficients_df, aes(x = Treatment_seed, y = Estimate, color = Treatment_seed)) +
  geom_pointrange(aes(ymin = Estimate - `Std. Error`, ymax = Estimate + `Std. Error`), size = 1.2) +
  geom_text(aes(label = signif), vjust = -1.5, color = "red", size = 12) +
  scale_color_manual(values = c("blue", "green", "purple", "orange", "pink", "yellow", "cyan", "red", "brown", "black", "gray", "violet", "darkgreen")) +
  labs(title = "Fixed Effects from LME Model",
       subtitle = "Significance levels: *** p < 0.001, ** p < 0.01, * p < 0.05, . p < 0.1, '' p ≥ 0.1",
       x = "Treatment Seed",
       y = "Estimated Effect on Values_mm",
       color = "Treatment Seed") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") 
        #subtitle.position = "buttom")# Adjust the legend position as needed

#4Plot. Add explanation in the Plot`s Caption
install.packages("RColorBrewer")
library(RColorBrewer)
library(ggplot2)

#create a custom palette using a predefined pallete
my_palette <- brewer.pal(n=8, name = "Set1") #We can choose from Set 1-3, Dark2 etc.
#or

# Define a custom color palette with 13 colors
my_palette <- c("H2O(Intercept)" = "#1f77b4", "NB1" = "#ff7f0e", "NB2" = "#2ca02c", 
                "NB3" = "#d62728", "NB5" = "#9467bd", "NB6" = "#8c564b",
                "NB7" = "#e377c2", "NB8" = "#7f7f7f", "NB9" = "#bcbd22",
                "ABA09" = "#17becf", "ABF11" = "#aec7e8", "ABE11" = "#ffbb78",
                "LAG16" = "#98df8a")


# 4Plot the fixed effects with confidence intervals and significance symbols
ggplot(coefficients_df, aes(x = Treatment_seed, y = Estimate, color = Treatment_seed)) +
  geom_pointrange(aes(ymin = Estimate - `Std. Error`, ymax = Estimate + `Std. Error`),  size = 1.5) +
  #geom_errorbar(aes(ymin = Estimate - `Std. Error`, ymax = Estimate + `Std. Error`), width = 0.2, color = "black") +
  geom_text(aes(label = signif), vjust = -1.5, color = "red", size = 10) +
  scale_color_manual(values = my_palette) +
  labs(title = "Fixed Effects from LME Model: 
       Estimated Impact of Treatment on Fusarium Growth (Values_mm), 
       Controlling for Timepoint Variability",
       x = "Treatment Seed",
       y = "Estimated Effect on Values_mm",
       color = "Treatment Seed",
       caption = "Significance levels: *** p < 0.001, ** p < 0.01, * p < 0.05, . p < 0.1, '' p ≥ 0.1") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold"),
axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
axis.text.y = element_text(hjust = 1, size = 14),
axis.title.x = element_text(size = 16),
axis.title.y = element_text(size = 16),
plot.caption = element_text(size = 14, hjust = 0.5, vjust = 2),
legend.position = "right",
legend.text = element_text(12),
legend.title = element_text(size = 14))


#######---------GENERALIZED LINEAR MODEL (GLM)
install.packages("lme4")
install.packages("lmerTest")
install.packages("ggplot2")
install.packages("emmeans")
install.packages("ggsignif")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("broom")
install.packages("ggpubr")
install.packages("car")

library(lme4)
library(lmerTest)
library(ggplot2)
library(emmeans)
library(ggsignif)
library(broom)
library(dplyr)
library(car)
library(ggpubr)


df_noProPVA
str(df_noProPVA)


#Fit the logistic regression linear model
logistic_model <-glm(df_noProPVA$Supp_activity ~ df_noProPVA$Treatment_seed + df_noProPVA$Values_mm, 
                     data = df_noProPVA, family = binomial)
summary (logistic_model)

# Tidy the model results for easy plotting
tidy_logistic <- tidy(logistic_model)

# Add significance levels (***, , *, .) based on p-values
tidy_logistic <- tidy_logistic %>%
  mutate(signif = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    p.value < 0.1   ~ ".",
    TRUE            ~ ""
  ))

#Check the levels of `term`
tidy_logistic$term <-as.factor (tidy_logistic$term)
levels(tidy_logistic$term)

# Assuming tidy_logistic is the dataframe containing the results from the logistic regression
tidy_logistic$term <- gsub("df_noProPVA\\$Treatment_seed", "", tidy_logistic$term)

#Rename the Intercept to "H2O(Intercept)" 
tidy_logistic$term <- gsub("\\(Intercept\\)", "H2O(Inretcept)", tidy_logistic$term)

#Remove the (Values_mm) row
tidy_logistic <-tidy_logistic %>% dplyr::filter(tidy_logistic$term !="df_noProPVA$Values_mm")

#Convert term to a factor and specify the levels explicitly
tidy_logistic$term <- factor (tidy_logistic$term, 
                      levels = c("H2O(Inretcept)", "NB1", "NB2", "NB3", "NB5", "NB6", "NB7", "NB8", "NB9", "ABA09",
                                 "ABF11","ABE11", "LAG16"))



# Define the color palette with names corresponding to the levels in the Treatment_seed factor
my_palette <- c("H2O(Intercept)" = "#1f77b4", "NB1" = "#ff7f0e", "NB2" = "#2ca02c",
                "NB3" = "#d62728", "NB5" = "#9467bd", "NB6" = "#8c564b",
                "NB7" = "#e377c2", "NB8" = "#7f7f7f", "NB9" = "#bcbd22",
                "ABA09" = "#17becf", "ABF11" = "#aec7e8", "ABE11" = "#ffbb78",
                "LAG16" = "#98df8a")

# Ensure that Treatment_seed is a factor with the correct levels
tidy_logistic$term <- factor(tidy_logistic$term, levels = names(my_palette))






#3Plot.  Works. The fixed effects with confidence intervals and significance symbols
ggplot(tidy_logistic, aes(x = term, y = estimate, color = term)) +
  geom_pointrange(aes(ymin = estimate - std.error, ymax = estimate + std.error), color = "blue", size = 1.2) +
  geom_text(aes(label = signif), vjust = -2.0, color = "red", size = 10) +
  #scale_color_manual(values = my_palette) +  # You can customize colors
  labs(title = "Logistic Regression Linear Model. 
       Predicted probability of Fusarium Suppression activity",
       x = "Predictors",
       y = "Estimate (Log-Odds)",
       color = "Predictors",
       caption = "Significance levels: *** p < 0.001, ** p < 0.01, * p < 0.05, . p < 0.1, '' p ≥ 0.1") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(hjust = 1, size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        plot.caption = element_text(size = 14, hjust = 0.5, vjust = 2),
        legend.text = element_text(12),
        legend.title = element_text(size = 14))




# 1Plot.
ggplot(tidy_logistic, aes(x = term, y = estimate, color = term)) +
  geom_pointrange(aes(ymin = estimate - std.error, ymax = estimate + std.error), 
                  size = 1.2) +
  geom_text(aes(label = signif), vjust = -1.5, color = "red", size = 10) +
  geom_errorbar(aes(ymin = Estimate - `Std. Error`, ymax = Estimate + `Std. Error`), width = 0.2, color = "black") 
  scale_color_manual(values = my_palette) + # You can customize colors
  labs(title = "Logistic Regression Coefficients",
       x = "Predictors",
       y = "Estimate (Log-Odds)",
       color = "Predictors",
       caption = "Significance levels: *** p < 0.001, ** p < 0.01, * p < 0.05, . p < 0.1, '' p ≥ 0.1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(size = 10, hjust = 0.5, vjust = 2))

#2Plot.
  ggplot(tidy_logistic, aes(x = term, y = estimate, color = term)) +
    geom_pointrange(aes(ymin = estimate - std.error, ymax = estimate + std.error), size = 1.2) +
    geom_text(aes(label = signif), vjust = -1.5, color = "red", size = 10) +
    
    # geom_errorbar is redundant with geom_pointrange, so it can be removed unless you need it separately
    # Remove this or adjust the variables to match your dataframe's column names
    # geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2, color = "black") +
    
    scale_color_manual(values = my_palette) + # Customize colors
    
    labs(
      title = "Logistic Regression Linear Model. Predicted probability of Fusarium Suppression activity based on Treatment with NB solutions",
      x = "Predictors",
      y = "Estimate (Log-Odds)",
      color = "Predictors",
      caption = "Significance levels: *** p < 0.001, ** p < 0.01, * p < 0.05, . p < 0.1, '' p ≥ 0.1"
    ) +
    
    theme_minimal() +
    
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.caption = element_text(size = 10, hjust = 0.5, vjust = 2)
    ) 
  
  
  
  
  
  
  
  
  










########----------Visualization pairwise comparisons between the levels of Treatment_Seed, incl. p-values

library(ggplot2)
library(lme4)
library(emmeans)
library(ggsignif)

# Fit the model (assuming this has been done)
# lme_model <- lmer(df_noProPVA$Values_mm ~ df_noProPVA$Treatment_seed + (1 | df_noProPVA$Timepoint), data = df_noProPVA)

# Obtain estimated marginal means (emmeans) for visualization
emm <- emmeans(lme_model, ~ Treatment_seed)

# Convert to a data frame for ggplot
emm_df <- as.data.frame(emm)

#Inspect the structure of emm_df
str(emm_df)

#Classes ‘summary_emm’ and 'data.frame':	13 obs. of  6 variables:
#  $ Treatment_seed: Factor w/ 13 levels "H2O","NB1","NB2",..: 1 2 3 4 5 6 7 8 9 10 ...
#$ emmean        : num  0.918 2.734 1.313 2.164 1.961 ...
#$ SE            : num  1.58 1.57 1.57 1.57 1.57 ...
#$ df            : num  15.3 15.1 15.2 15.1 15.2 ...
#$ lower.CL      : num  -2.435 -0.611 -2.036 -1.183 -1.389 ...
#$ upper.CL      : num  4.27 6.08 4.66 5.51 5.31 ...
#- attr(*, "estName")= chr "emmean"
#- attr(*, "clNames")= chr [1:2] "lower.CL" "upper.CL"
#- attr(*, "pri.vars")= chr "Treatment_seed"
#- attr(*, "adjust")= chr "none"
#- attr(*, "side")= num 0
#- attr(*, "delta")= num 0
#- attr(*, "type")= chr "link"
#- attr(*, "mesg")= chr [1:2] "Degrees-of-freedom method: kenward-roger" "Confidence level used: 0.95"
#- attr(*, "digits")= int 7

### we have no p-values in the str (emm_df)

# so we will obtain pairwise comparisons and p-values
pairwise_comparisons <- contrast (emm, method = "pairwise")

#convert to a dataframe
pairwise_df <- as.data.frame(pairwise_comparisons)

print (pairwise_df)

#contrast        estimate        SE      df t.ratio p.value
#H2O - NB1     -1.8159234 0.4651160 2528.04  -3.904  0.0064
#H2O - NB2     -0.3951917 0.4758864 2528.04  -0.830  0.9998
#H2O - NB3     -1.2452858 0.4699666 2528.03  -2.650  0.2811
#H2O - NB5     -1.0425932 0.4774967 2528.02  -2.183  0.6035
#H2O - NB6     -1.0932583 0.4732641 2528.06  -2.310  0.5097
#H2O - NB7     -0.7641937 0.4805596 2528.03  -1.590  0.9332
#H2O - NB8     -1.3885794 0.4700082 2528.05  -2.954  0.1392
#H2O - NB9     -0.8224979 0.4747074 2528.02  -1.733  0.8814
#H2O - ABA09   -0.5715307 0.4885334 2528.06  -1.170  0.9945
#H2O - ABF11    0.0649961 0.4875542 2528.01   0.133  1.0000
#H2O - ABE11   -0.4402669 0.5090154 2528.06  -0.865  0.9997
#H2O - LAG16   -0.0681609 0.4829239 2528.01  -0.141  1.0000
#NB1 - NB2      1.4207318 0.4568716 2528.05   3.110  0.0919
#NB1 - NB3      0.5706377 0.4504321 2528.03   1.267  0.9889
#NB1 - NB5      0.7733303 0.4586627 2528.04   1.686  0.9005
#NB1 - NB6      0.7226651 0.4538740 2528.03   1.592  0.9326
#NB1 - NB7      1.0517297 0.4619160 2528.05   2.277  0.5342
#NB1 - NB8      0.4273441 0.4506269 2528.06   0.948  0.9993
#NB1 - NB9      0.9934255 0.4556724 2528.04   2.180  0.6060
#NB1 - ABA09    1.2443928 0.4701076 2528.06   2.647  0.2827
#NB1 - ABF11    1.8809196 0.4695408 2528.06   4.006  0.0043
#NB1 - ABE11    1.3756565 0.4913779 2528.08   2.800  0.2028
#NB1 - LAG16    1.7477625 0.4645456 2528.05   3.762  0.0109
#NB2 - NB3     -0.8500941 0.4620560 2528.10  -1.840  0.8295
#NB2 - NB5     -0.6474015 0.4697203 2528.06  -1.378  0.9775
#NB2 - NB6     -0.6980667 0.4653038 2528.09  -1.500  0.9563
#NB2 - NB7     -0.3690020 0.4725701 2528.01  -0.781  0.9999
#NB2 - NB8     -0.9933877 0.4617605 2528.02  -2.151  0.6271
#NB2 - NB9     -0.4273062 0.4666277 2528.02  -0.916  0.9995
#NB2 - ABA09   -0.1763390 0.4807464 2528.07  -0.367  1.0000
#NB2 - ABF11    0.4601878 0.4799841 2528.04   0.959  0.9992
#NB2 - ABE11   -0.0450752 0.5014540 2528.04  -0.090  1.0000
#NB2 - LAG16    0.3270308 0.4753212 2528.05   0.688  1.0000
#NB3 - NB5      0.2026926 0.4635217 2528.02   0.437  1.0000
#NB3 - NB6      0.1520274 0.4589247 2528.03   0.331  1.0000
#NB3 - NB7      0.4810921 0.4669618 2528.09   1.030  0.9983
#NB3 - NB8     -0.1432936 0.4558794 2528.09  -0.314  1.0000
#NB3 - NB9      0.4227879 0.4607080 2528.05   0.918  0.9995
#NB3 - ABA09    0.6737551 0.4749194 2528.06   1.419  0.9716
#NB3 - ABF11    1.3102819 0.4743451 2528.07   2.762  0.2208
#NB3 - ABE11    0.8050189 0.4960794 2528.10   1.623  0.9231
#NB3 - LAG16    1.1771249 0.4694700 2528.06   2.507  0.3698
#NB5 - NB6     -0.0506652 0.4669552 2528.06  -0.109  1.0000
#NB5 - NB7      0.2783995 0.4745480 2528.07   0.587  1.0000
#NB5 - NB8     -0.3459862 0.4636864 2528.06  -0.746  0.9999
#NB5 - NB9      0.2200953 0.4684798 2528.03   0.470  1.0000
#NB5 - ABA09    0.4710625 0.4824739 2528.06   0.976  0.9990
#NB5 - ABF11    1.1075893 0.4816456 2528.04   2.300  0.5174
#NB5 - ABE11    0.6023263 0.5033225 2528.08   1.197  0.9933
#NB5 - LAG16    0.9744323 0.4769834 2528.04   2.043  0.7042
#NB6 - NB7      0.3290646 0.4700763 2528.07   0.700  1.0000
#NB6 - NB8     -0.2953210 0.4594044 2528.13  -0.643  1.0000
#NB6 - NB9      0.2707604 0.4640013 2528.06   0.584  1.0000
#NB6 - ABA09    0.5217277 0.4777950 2528.02   1.092  0.9971
#NB6 - ABF11    1.1582545 0.4774598 2528.07   2.426  0.4258
#NB6 - ABE11    0.6529915 0.4988785 2528.06   1.309  0.9853
#NB6 - LAG16    1.0250974 0.4727012 2528.07   2.169  0.6144
#NB7 - NB8     -0.6243856 0.4667482 2528.04  -1.338  0.9824
#NB7 - NB9     -0.0583042 0.4714481 2528.02  -0.124  1.0000
#NB7 - ABA09    0.1926630 0.4853491 2528.05   0.397  1.0000
#NB7 - ABF11    0.8291899 0.4845164 2528.02   1.711  0.8903
#NB7 - ABE11    0.3239268 0.5059048 2528.04   0.640  1.0000
#NB7 - LAG16    0.6960328 0.4799560 2528.04   1.450  0.9663
#NB8 - NB9      0.5660815 0.4606729 2528.04   1.229  0.9915
#NB8 - ABA09    0.8170487 0.4752474 2528.13   1.719  0.8871
#NB8 - ABF11    1.4535755 0.4741770 2528.04   3.065  0.1038
#NB8 - ABE11    0.9483125 0.4961291 2528.09   1.911  0.7889
#NB8 - LAG16    1.3204185 0.4693485 2528.04   2.813  0.1965
#NB9 - ABA09    0.2509672 0.4794893 2528.04   0.523  1.0000
#NB9 - ABF11    0.8874940 0.4788656 2528.03   1.853  0.8221
#NB9 - ABE11    0.3822310 0.5002788 2528.04   0.764  0.9999
#NB9 - LAG16    0.7543370 0.4742011 2528.04   1.591  0.9330
#ABA09 - ABF11  0.6365268 0.4925052 2528.06   1.292  0.9868
#ABA09 - ABE11  0.1312638 0.5130465 2528.02   0.256  1.0000
#ABA09 - LAG16  0.5033698 0.4880771 2528.08   1.031  0.9983
#ABF11 - ABE11 -0.5052630 0.5128807 2528.06  -0.985  0.9989
#ABF11 - LAG16 -0.1331570 0.4868838 2528.01  -0.273  1.0000
#ABE11 - LAG16  0.3721060 0.5085717 2528.07   0.732  1.0000

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: tukey method for comparing a family of 13 estimates 

# Merge p-values back with the emmeans data frame
pairwise_df <- pairwise_df %>%
  mutate(Treatment_seed = gsub ("- . * $", "", contrast))


#Select the relevant colums
pairwise_df <-pairwise_df [, c("Treatment_seed", "p.value")]

#Merge with the emmeans dataframe
emm_df <-merge (emm_df, pairwise_df, by = "Treatment_seed", all.x = TRUE)


#Add significant indicators based on p-values
emm_df$signif <- cut(emm_df$p.value,
                     breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                     labels = c("***", "**", "*", ".", ""))


# Plot the estimated marginal means with confidence intervals and significance stars
ggplot(emm_df, aes(x = Treatment_seed, y = emmean)) +
  geom_pointrange(aes(ymin = lower.CL, ymax = upper.CL), color = "blue", size = 1.2) +
  geom_text(aes(label = signif), vjust = -1, color = "red", size = 4) +
  labs(title = "Estimated Marginal Means for Treatment Seeds",
       x = "Treatment Seed",
       y = "Estimated Values_mm") +
  theme() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Add significance indicators based on p-values
emm_df$signif <- cut(emm_df$p.value,
                     breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                     labels = c("***", "**", "*", ".", ""))

# Plot the estimated marginal means with confidence intervals
ggplot(emm_df, aes(x = Treatment_seed, y = emmean)) +
  geom_pointrange(aes(ymin = lower.CL, ymax = upper.CL), color = "blue", size = 1.2) +
  geom_text(aes(label = signif), vjust = -1, color = "red", size = 5) +
  labs(title = "Estimated Marginal Means for Treatment Seeds",
       x = "Treatment Seed",
       y = "Estimated Values_mm") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





library(dplyr)
library(ggplot2)


df_Dish_longcvs$Values_mm <- as.numeric(df_Dish_longcvs$Values_mm)
class(df_Dish_longcvs$Values_mm)

df_Dish_longcvs$Timepoint <- as.numeric(df_Dish_longcvs$Timepoint)
class(df_Dish_longcvs$Timepoint)


install.packages("tidyr")
library(tidyr)
library(broom)
library(ggplot2)




#plots

df_Dish_longcvs %>% 
  dplyr::filter(Timepoint%in%c(3, 5, 7, 9, 12, 15, 20, 25, 35 )) %>% 
  drop_na(Treatment_seed )%>%
  ggplot(aes(Treatment_seed,Values_mm,fill=Treatment_seed))+
  geom_boxplot(outlier.shape = NULL)+
  geom_point(position = position_jitter())+
  geom_jitter(width = 0.01, alpha = 0.1) + 
  theme(axis.text.x = element_text (size = 8, angle = 45, vjust = 0.01, hjust=0.1),
        legend.title = element_text (size = 12),
        legend.text = element_text (size = 10))+
  geom_hline(yintercept = 0,linetype="dashed")+
  facet_wrap(~Timepoint,scale="free")

df_noProPVA %>% 
  dplyr::filter(Timepoint%in%c(3, 5, 7, 9, 12, 15, 20, 25, 35 )) %>% 
  drop_na(Treatment_seed )%>%
  ggplot(aes(Treatment_seed, Values_mm, fill =Treatment_seed ))+
  geom_boxplot(outlier.shape = NULL )+
  geom_point(position = position_jitter())+
  geom_jitter(width = 0.01, alpha = 0.1)+
  scale_color_manual(values = my_palette) +
  theme(axis.text.x = element_text (size = 8, angle = 45, vjust = 0.01, hjust=0.1),
        legend.title = element_text (size = 12),
        legend.text = element_text (size = 10))+
  geom_hline(yintercept = 0, linetype ="dashed", color = "red",  size = 1)+
  facet_wrap(~Timepoint,scale="free")



#New plot.
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Define a new color palette
my_new_palette <- c("#D32F2F", "#1976D2", "#388E3C", "#FBC02D", "#8E24AA",
                    "#E64A19", "#43A047", "#7B1FA2", "#0288D1", "#C2185B",
                    "#689F38", "#F57C00", "#303F9F")

# Create the plot with modifications
df_noProPVA %>%
  dplyr::filter(Timepoint %in% c(3, 5, 7, 9, 12, 15, 20, 25, 35)) %>%
  drop_na(Treatment_seed) %>%
  ggplot(aes(Treatment_seed, Values_mm, fill = Treatment_seed)) +
  geom_boxplot(outlier.shape = NA) +  # Remove outlier shapes
  geom_jitter(width = 0.2, alpha = 0.5, color = "black") +  # Add jitter with transparency and color
  scale_fill_manual(values = my_new_palette) +  # Use the new color palette
  theme(axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5, hjust = 1),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = "right") +  # Position the legend to the right
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 1) +  # Solid line instead of dashed
  facet_wrap(~Timepoint, scale = "free") +
  labs(
    title = "Boxplot of Values Over Time by Treatment Seed",
    x = "Treatment Seed",
    y = "Values (mm)",
    fill = "Treatment Seed",
    caption = "Red line indicates zero value"
  ) +
  theme_minimal()


#####New plot. Pastel palette
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Define a gentle pastel color palette
pastel_palette <- c("#FFB3BA", "#FFDFBA", "#FFFFBA", "#BAFFC9", "#BAE1FF",
                    "#C4B0E2", "#FFCCF9", "#FFDEA6", "#B5EAD7", "#FFDAC1",
                    "#D4A5A5", "#C7CEEA", "#FF9AA2")

# Create the plot with modifications
df_noProPVA %>%
  dplyr::filter(Timepoint %in% c(3, 5, 7, 9, 12, 15, 20, 25, 35)) %>%
  drop_na(Treatment_seed) %>%
  ggplot(aes(Treatment_seed, Values_mm, fill = Treatment_seed)) +
  geom_boxplot(outlier.shape = NA) +  # Remove outlier shapes
  geom_jitter(width = 0.2, alpha = 0.3, color = "black") +  # Add jitter with transparency and color
  scale_fill_manual(values = pastel_palette) +  # Use the pastel color palette
  theme(axis.text.x = element_text (size = 12, angle = 15, vjust = 1, hjust=1),  # Rotate x-axis labels
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "right",
        plot.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        #axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.caption = element_text(size = 12, hjust = 0.5)) +  # Position the legend to the right
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 1) +  # Solid line instead of dashed
  facet_wrap(~Timepoint, scale = "free") +
  labs(
    title = "Boxplot of Fungicide activity of NB-treatment seeds, grouped by days of screening",
    x = "Treatment Seed",
    y = "Values (mm)",
    fill = "Treatment Seed",
    caption = "Red line indicates seed position in the P.Dish") +
    theme_bw()
  

# # Load necessary libraries
library(ggplot2)
library(dplyr)

# Define a new color palette
pastel_palette <- c("#FFB3BA", "#FFDFBA", "#FFFFBA", "#BAFFC9", "#BAE1FF", 
                    "#E6B3FF", "#FFB3E1", "#B3D9FF", "#FFB3BA", "#FFDFBA", 
                    "#FFFFBA", "#BAFFC9", "#BAE1FF")

# 3.Plot. Works. Create the plot with modifications
df_noProPVA %>%
  dplyr::filter(Timepoint %in% c(3, 5, 7, 9, 12, 15, 20, 25, 35)) %>%
  drop_na(Treatment_seed) %>%
  ggplot(aes(Treatment_seed, Values_mm, fill = Treatment_seed)) +
  
  geom_boxplot(outlier.shape = NA) +  # Remove outlier shapes
  geom_jitter(width = 0.2, alpha = 0.2, color = "black") +  # Add jitter with transparency and color
  scale_fill_manual(values = pastel_palette) +  # Use the pastel color palette
  
  theme_bw() +  # Add theme_bw for a clean background
  
  theme(
    axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),  # Rotate x-axis labels to 45 degrees
    axis.text.y = element_text(size = 14),  # Increase y-axis text size
    axis.title.y = element_text(size = 16),  # Increase y-axis title text size
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "right",  # Position the legend to the right
    plot.title = element_text(size = 18, face = "bold"),
    strip.text = element_text(size = 14),  # Increase facet (day) labels text size
    plot.caption = element_text(size = 10, hjust = 0.5, vjust = 1.5)  # Move the caption up
  ) +
  
  geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 1) +  # Solid line instead of dashed
  facet_wrap(~Timepoint, scale = "free") +
  
  labs(
    title = "Boxplot of Fungicide Activity of NB-Treatment Seeds, Grouped by Days of Screening",
    x = "Treatment Seed",
    y = "Values (mm)",
    fill = "Treatment Seed",
    caption = "Red line indicates seed position in the P.Dish"
  )









df_Dish_longcvs %>% 
  #dplyr::filter(df_Dish_longcvs$Treatment_seed !="Proline") %>% 
  dplyr::filter(Timepoint%in%c(3, 5, 7, 15, 20, 25, 35 )) %>%
  drop_na(Treatment_seed )%>%
  ggplot(aes(Treatment_seed,Values_mm,fill=Treatment_seed))+
  geom_boxplot(outlier.shape = NULL)+
  geom_point(position = position_jitter())+
  geom_jitter(width = 0.1, alpha = 0.01) + 
  theme(axis.text.x = element_text (size = 8, angle = 45, vjust = 0.1, hjust=0.1),
        legend.title = element_text (size = 12),
        legend.text = element_text (size = 10))+
  geom_hline(yintercept = 0,linetype="dashed")+
  facet_wrap(~Supp_activity,scale="free")
  #facet_grid(~Timepoint,scale="free")


# Plot antifungal activity over time
  ggplot(df_clean, aes(x = Timepoint, fill = Supp_activity)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ Treatment_seed) +
  labs(title = "Antifungal Activity Over Time", x = "Days", y = "Values, mm")

# Plot distance over time for each NB treatment
  
  # Ensure Timepoint and Values_mm are numeric
  df_clean$Timepoint <- as.numeric(as.character(df_clean$Timepoint))
  df_clean$Values_mm <- as.numeric(as.character(df_clean$Values_mm))
  
  # Plot with ggplot2
  ggplot(df_clean, aes(x = Timepoint, y = Values_mm, color = as.factor(Treatment_seed))) +
    geom_line(aes(group = interaction(Treatment_seed, Replicates)), alpha = 0.3) +
    stat_summary(fun = mean, geom = "line", size = 1) +
    labs(title = "Distance Over Time by Treatment Type", x = "Days", y = "Values (mm)")
  
  
  ggplot(df_clean, aes(x = Timepoint, y = Values_mm, color = as.factor(Treatment_seed))) +
    geom_line(aes(group = interaction(Treatment_seed, replicate)), alpha = 0.3) +
    stat_summary(fun = mean, geom = "line", size = 1) +
    labs(title = "Distance Over Time by Treatment Type", x = "Days", y = "Values, (mm)")


  
  
  
  
  
  
  
  
  
  
  
  
  
#######------------- Plot. Lines of Fungicide activity in days
  df_noProPVA %>%
    drop_na(Treatment_seed) %>%
    ggplot(aes(Timepoint, Values_mm, color = Treatment_seed)) +
    geom_point() +
    geom_smooth(se = FALSE)+
    labs(title = "Distance Over Time by Treatment Type", x = "Days", y = "Values (mm)")
  
  
  # Filter the data for H2O and NB1
  df_filtered_H2O_NB1_NB3_NB5_NB6_NB8 <- df_noProPVA %>%
    dplyr::filter(Treatment_seed %in% c("H2O", "NB1", "NB3","NB5","NB6", "NB8")) %>%
    drop_na(Treatment_seed)
  
  # Create the plot
  ggplot(df_filtered_H2O_NB1_NB3_NB5_NB6_NB8, aes(x = Timepoint, y = Values_mm, color = Treatment_seed)) +
    geom_point() +
    geom_smooth(se = FALSE) +
    scale_color_manual(values = c("H2O" = "#d62728", "NB1" = "#98df8a", "NB3" = "#1f77b4",  "NB5" = "#17becf", "NB6" = "#1f77b4", "NB8" = "#e377c2")) +
    labs(
      title = "Fungicide activity over time: Comparison between H2O and NB Treated seeds",
      x = "Days",
      y = "Fungicide Activity (Values_mm)",
      color = "Treatment Seed",
      #caption = "H2O: Control treatment (blue line), NB1: green line, NB6: "
    ) +
    theme_linedraw() +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      #plot.caption = element_text(size = 12, hjust = 0.5),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14)
    )  
  
  
  
  
  
  
  
  
  
  
  
install.packages("ggplot2")
library(ggplot2)
library(tidyverse)

df_Dish_longcvs %>%
  drop_na(Treatment_seed) %>%
  ggplot(aes(fct_infreq(Treatment_seed)))+
  geom_bar (fill= "green")+
  #coord_flip()
  theme(axis.text.x = element_text (size = 8, angle = 45, vjust = 0.1, hjust=0.1))+
  facet_wrap(~Timepoint,scale="free")+
  labs (x = "Seed_treatment",
        y = NULL,
        title = "Number of observation in each NB families and controls")

df_Dish_longcvs %>%
  #drop_na(Values_mm) %>%
  ggplot(aes(fct_infreq(Supp_activity)))+
  geom_bar (fill= "blue")+
  #coord_flip()
  theme_bw()+
  facet_wrap(~Timepoint,scale="free")
  labs (x = "Supp_activity)",
        y = NULL,
        title = "Number of observation in each Supp_activity")




df_Dish_longcvs %>%
  drop_na(Treatment_seed) %>%
  ggplot(aes(Timepoint, Values_mm, color = Treatment_seed)) +
  geom_point() +
  geom_smooth(se = FALSE)

#plots Suppress activity in days of exposure
plot (df_Dish_longcvs$Timepoint [df_Dish_longcvs$Supp_activity=="yes"], 
      df_Dish_longcvs$Values_mm[df_Dish_longcvs$Supp_activity=="yes"],
      col="blue", ylim=c(-23,20), xlim=c(0, 35), xlab="Days", 
      ylab="mm", main="Days exposure vs. suppress_act")

points(df_Dish_longcvs$Timepoint [df_Dish_longcvs$Supp_activity=="no"],
       df_Dish_longcvs$Values_mm[df_Dish_longcvs$Supp_activity=="no"], col="red", pch=16)

#Add the legend
legend(20,20, legend=c("Inhibite fus", "Noninhibite fus"), col=c("blue", "red"),
       pch = c(1,16), bty="n", cex=0.75)

#plots Exudates in days of exposure
plot (df_Dish_longcvs$Timepoint [df_Dish_longcvs$Exudates=="yes"], 
      df_Dish_longcvs$Values_mm[df_Dish_longcvs$Exudates=="yes"],
      col="blue", ylim=c(-23,20), xlim=c(0, 35), xlab="Days", 
      ylab="mm", main="Days exposure vs. Exudates")

points(df_Dish_longcvs$Timepoint [df_Dish_longcvs$Exudates=="no"],
       df_Dish_longcvs$Values_mm[df_Dish_longcvs$Exudates=="no"], col="grey", pch=16)
legend(20,20, legend=c("Exudates", "Noexudates"), col=c("blue", "grey"),
       pch = c(1,16), bty="n", cex=1)

#plots Suppress activity in NB families
plot (df_Dish_longcvs$Treatment_seed [df_Dish_longcvs$Supp_activity=="yes"], 
      df_Dish_longcvs$Values_mm[df_Dish_longcvs$Supp_activity=="yes"],
      col="blue", ylim=c(-23,20), xlim=c(1, 15), xlab="Nanobodies families", 
      ylab="mm", main="Treatment of seed vs. suppress_act")

points(df_Dish_longcvs$Treatment_seed [df_Dish_longcvs$Supp_activity=="no"],
       df_Dish_longcvs$Values_mm[df_Dish_longcvs$Supp_activity=="no"], col="red", pch=16)


#plots Suppress activity in NB families (restricted)
plot (df_Dish_longcvs$Treatment_seed [df_Dish_longcvs$Supp_activity=="yes"], 
      df_Dish_longcvs$Values_mm[df_Dish_longcvs$Supp_activity=="yes"],
      col="blue", pch=8, ylim=c(-5,15), xlim=c(1, 15), xlab="Nanobodies families", 
      ylab="mm", main="Treatment of seed vs. suppress_act")

points(df_Dish_longcvs$Treatment_seed [df_Dish_longcvs$Supp_activity=="no"],
       df_Dish_longcvs$Values_mm[df_Dish_longcvs$Supp_activity=="no"], col="red", pch=16)
      
points(df_Dish_longcvs$Treatment_seed [df_Dish_longcvs$Exudates=="no"],
       df_Dish_longcvs$Values_mm[df_Dish_longcvs$Exudates=="no"], col="grey", pch=6)

#Add the legend
legend(1,12, legend=c("Inhibite fus", "Noninhibite fus"), col=c("blue", "red")
       +pch=c(7,12), bty="n")


#correlations##########

#correlations 
cor(df_Dish_longcvs$Values_mm, df_Dish_longcvs$Supp_activity_bi)


#conversion numeric to categorical variables
?cut
#creation the distance categories of A=<-1, B=2-0, C=2-6,
#                                   D=6-10, E=10-20, F>=20

catdist <-cut(df_Dish_longcvs$Values_mm, breaks = c(-20, -1, 0, 2, 6, 10, 20), 
labels= c("A", "B", "C", "D", "E", "F"))
df_Dish_longcvs$Values_mm [200:210]
catdist[1000:1100]
catdist[160:200]
catdist[450:460]
#change the closed intervals to the right side [)
catdist <-cut(df_Dish_longcvs$Values_mm, breaks = c(-20, -1, 0, 2, 6, 10, 20), 
              labels= c("A", "B", "C", "D", "E", "F"), right = FALSE)
catdist[151:155]

catdist <-cut(df_Dish_longcvs$Values_mm, breaks = c(-20, -1, 0, 2, 6, 10, 20), 
              right = FALSE)
catdist[151:155]
catdist[161:170]

#convert the indicators into binary indicators 0,1
#Supp_activity
df_Dish_longcvs$Supp_activity_bi <- ifelse(df_Dish_longcvs$Supp_activity == "yes", 1, 0)

class(df_Dish_longcvs$Supp_activity_bi)
df_Dish_longcvs$Supp_activity_bifac <- factor (df_Dish_longcvs$Supp_activity_bi, levels=c(1,0),
                                               labels=c("yes", "no"))
class(df_Dish_longcvs$Supp_activity_bifac)


df_Dish_longcvs$Supp_activity_bi <- ifelse(df_Dish_longcvs$Supp_activity == "yes", 1, 0)


#Exudates
df_Dish_longcvs$Exudates_bi <- ifelse(df_Dish_longcvs$Exudates == "yes", 1, 0)
class(df_Dish_longcvs$Exudates_bi)
df_Dish_longcvs$Exudates_bifac <- factor (df_Dish_longcvs$Exudates_bi, levels=c(1,0),
                                               labels=c("yes", "no"))
class(df_Dish_longcvs$Exudates_bifac)



#Calculate the mean for each of Catdist
mean(df_Dish_longcvs$Values_mm[catdist=="A"], na.rm=TRUE)

# Calculate the mean for each level of 'catdist'
aggregate(Values_mm ~ catdist, data=df_Dish_longcvs, FUN=mean, na.rm=TRUE)

#mod <-lm(df_Dish_longcvs$Values_mm ~ catdist, na.rm=TRUE)
mod <- lm(df_Dish_longcvs$Values_mm ~ catdist, na.action=na.omit)

summary(mod)

#> mod <- lm(df_Dish_longcvs$Values_mm ~ catdist, na.action=na.omit)
#> View(mod)
#> summary(mod)

#Call:
  #lm(formula = df_Dish_longcvs$Values_mm ~ catdist, na.action = na.omit)

#Residuals:
  #Min       1Q   Median       3Q      Max 
#-10.2537  -1.4638   0.2056   1.2056   7.7463 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  -9.7463     0.1491  -65.38   <2e-16 ***
#  catdistB      8.7463     0.6930   12.62   <2e-16 ***
#  catdistC     10.1015     0.2478   40.77   <2e-16 ***
#  catdistD     13.5407     0.1969   68.78   <2e-16 ***
#  catdistE     17.2960     0.2246   77.01   <2e-16 ***
#  catdistF     21.2100     0.3294   64.39   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 2.44 on 1067 degrees of freedom
#(577 observations deleted due to missingness)
#Multiple R-squared:  0.8882,	Adjusted R-squared:  0.8877 
#F-statistic:  1695 on 5 and 1067 DF,  p-value: < 2.2e-16




df_Dish_longcvs$Treatment_seed <-factor (df_Dish_longcvs$Treatment_seed, 
                                         levels = c("H2O", "NB1", "NB2", "NB3", "NB5", "NB6", "NB7", "NB8", "NB9", "ABA09",
                                                    "ABF11","ABE11", "LAG16", "PVA", "Proline"))

mod <- lm(df_Dish_longcvs$Values_mm ~ df_Dish_longcvs$Treatment_seed, na.action=na.omit)
summary(mod)

#attempt from 05.08.24
#Call:
#  lm(formula = df_Dish_longcvs$Values_mm ~ df_Dish_longcvs$Treatment_seed, 
#     na.action = na.omit)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-25.150  -2.750   1.155   3.498  18.918 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                            1.20213    0.52989   2.269   0.0234 *  
#  df_Dish_longcvs$Treatment_seedNB1      1.61402    0.71937   2.244   0.0249 *  
#  df_Dish_longcvs$Treatment_seedNB2      0.30039    0.73894   0.407   0.6844    
#df_Dish_longcvs$Treatment_seedNB3      1.14529    0.72705   1.575   0.1153    
#df_Dish_longcvs$Treatment_seedNB5      1.05168    0.74077   1.420   0.1558    
#df_Dish_longcvs$Treatment_seedNB6      0.94763    0.73198   1.295   0.1956    
#df_Dish_longcvs$Treatment_seedNB7      0.64323    0.74356   0.865   0.3871    
#df_Dish_longcvs$Treatment_seedNB8      1.21493    0.72867   1.667   0.0956 .  
#df_Dish_longcvs$Treatment_seedNB9      0.63611    0.73453   0.866   0.3866    
#df_Dish_longcvs$Treatment_seedABA09    0.55062    0.75552   0.729   0.4662    
#df_Dish_longcvs$Treatment_seedABF11   -0.12016    0.75447  -0.159   0.8735    
#df_Dish_longcvs$Treatment_seedABE11    0.54787    0.78686   0.696   0.4863    
#df_Dish_longcvs$Treatment_seedLAG16   -0.00636    0.74838  -0.008   0.9932    
#df_Dish_longcvs$Treatment_seedPVA      0.56088    0.76544   0.733   0.4638    
#df_Dish_longcvs$Treatment_seedProline  5.61242    0.68755   8.163 4.77e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 7.265 on 2979 degrees of freedom
#(1715 observations deleted due to missingness)
#Multiple R-squared:  0.04122,	Adjusted R-squared:  0.03671 
#F-statistic: 9.147 on 14 and 2979 DF,  p-value: < 2.2e-16










Call:
  lm(formula = df_Dish_longcvs$Values_mm ~ df_Dish_longcvs$Treatment_seed, 
     na.action = na.omit)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-25.150  -2.750   1.155   3.498  18.918 

#Coefficients:
#                                      Estimate Std. Error  t value  Pr(>|t|)
#(Intercept)                            1.20213    0.52989   2.269   0.0234
#df_Dish_longcvs$Treatment_seedNB1      1.61402    0.71937   2.244   0.0249
#df_Dish_longcvs$Treatment_seedNB2      0.30039    0.73894   0.407   0.6844
#df_Dish_longcvs$Treatment_seedNB3      1.14529    0.72705   1.575   0.1153
#df_Dish_longcvs$Treatment_seedNB5      1.05168    0.74077   1.420   0.1558
#df_Dish_longcvs$Treatment_seedNB6      0.94763    0.73198   1.295   0.1956
#df_Dish_longcvs$Treatment_seedNB7      0.64323    0.74356   0.865   0.3871
#df_Dish_longcvs$Treatment_seedNB8      1.21493    0.72867   1.667   0.0956
#df_Dish_longcvs$Treatment_seedNB9      0.63611    0.73453   0.866   0.3866
#df_Dish_longcvs$Treatment_seedABA09    0.55062    0.75552   0.729   0.4662
#df_Dish_longcvs$Treatment_seedABF11   -0.12016    0.75447  -0.159   0.8735
#df_Dish_longcvs$Treatment_seedABE11    0.54787    0.78686   0.696   0.4863
#df_Dish_longcvs$Treatment_seedLAG16   -0.00636    0.74838  -0.008   0.9932
#df_Dish_longcvs$Treatment_seedPVA      0.56088    0.76544   0.733   0.4638
#df_Dish_longcvs$Treatment_seedProline  5.61242    0.68755   8.163 4.77e-16

#(Intercept)                           *  
#  df_Dish_longcvs$Treatment_seedNB1     *  
#  df_Dish_longcvs$Treatment_seedNB2        
#df_Dish_longcvs$Treatment_seedNB3        
#df_Dish_longcvs$Treatment_seedNB5        
#df_Dish_longcvs$Treatment_seedNB6        
#df_Dish_longcvs$Treatment_seedNB7        
#df_Dish_longcvs$Treatment_seedNB8     .  
#df_Dish_longcvs$Treatment_seedNB9        
#df_Dish_longcvs$Treatment_seedABA09      
#df_Dish_longcvs$Treatment_seedABF11      
#df_Dish_longcvs$Treatment_seedABE11      
#df_Dish_longcvs$Treatment_seedLAG16      
#df_Dish_longcvs$Treatment_seedPVA        
#df_Dish_longcvs$Treatment_seedProline ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 7.265 on 2979 degrees of freedom
#(1715 observations deleted due to missingness)
#Multiple R-squared:  0.04122,	Adjusted R-squared:  0.03671 
#F-statistic: 9.147 on 14 and 2979 DF,  p-value: < 2.2e-16


df_Dish_longcvs$Treatment_seed <-factor (df_Dish_longcvs$Treatment_seed, 
                                         levels = c("H2O", "NB1", "NB2", "NB3", "NB5", "NB6", "NB7", "NB8", "NB9", "ABA09",
                                                    "ABF11","ABE11", "LAG16", "PVA", "Proline"))

df_Dish_longcvs_nopro <- df_Dish_longcvs  %>% filter(df_Dish_longcvs$Treatment_seed !="Proline" )

lm(formula = df_Dish_longcvs$Values_mm ~ df_Dish_longcvs$Treatment_seed, 
   na.action = na.omit)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-26.3068  -3.9684   0.5859   4.5859  18.6301 

#Coefficients:
#                                       Estimate Std. Error  t value Pr(>|t|)    
#(Intercept)                             4.1932     0.9812   4.273  2.08e-05 ***
#  df_Dish_longcvs$Treatment_seedNB2    -2.4364     1.4518  -1.678  0.09357 .  
#df_Dish_longcvs$Treatment_seedNB3      -0.2248     1.3619  -0.165  0.86894    
#df_Dish_longcvs$Treatment_seedNB5      -0.3376     1.3800  -0.245  0.80676    
#df_Dish_longcvs$Treatment_seedNB6      -0.8864     1.3877  -0.639  0.52312    
#df_Dish_longcvs$Treatment_seedNB7      -2.8233     1.4572  -1.937  0.05292 .  
#df_Dish_longcvs$Treatment_seedNB8      -1.8465     1.4466  -1.276  0.20203    
#df_Dish_longcvs$Treatment_seedNB9      -1.4307     1.4220  -1.006  0.31455    
#df_Dish_longcvs$Treatment_seedABA09    -1.0307     1.4220  -0.725  0.46869    
#df_Dish_longcvs$Treatment_seedABF11    -2.3132     1.4466  -1.599  0.11006    
#df_Dish_longcvs$Treatment_seedABE11    -2.2218     1.4742  -1.507  0.13205    
#df_Dish_longcvs$Treatment_seedLAG16    -1.9132     1.4466  -1.323  0.18623    
#df_Dish_longcvs$Treatment_seedH2O      -1.7182     1.4220  -1.208  0.22716    
#df_Dish_longcvs$Treatment_seedPVA      -0.1226     1.3999  -0.088  0.93023    
#df_Dish_longcvs$Treatment_seedProline   4.2210     1.3486   3.130  0.00179 ** 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 9.205 on 1212 degrees of freedom
#(423 observations deleted due to missingness)
#Multiple R-squared:  0.03502,	Adjusted R-squared:  0.02388 
#F-statistic: 3.142 on 14 and 1212 DF,  p-value: 7.335e-05


#logistic regressiom modeling from data lab
install.packages("glmnet")
library(readr)
library(tidymodels)
install.packages("tidymodels")
install.packages("ggplot2")
library(ggplot2)


# Plot Treatment_seed against the Supp_activity variable
ggplot(df_Dish_longcvs, aes(Treatment_seed, fill = Values_mm)) +
  geom_bar() +
  coord_flip()

class(df_Dish_longcvs$Values_mm)


#one more time logregression

library(dplyr)
library(ggplot2)
library(lme4)
library(broom)
install.packages("tidyr")
library(tidyr)
install.packages("Matrix")
install.packages("lmerTest")
library(lmerTest)

#Summary statistics for distance
summary_stats <- df_Dish_longcvs %>%
  drop_na(df_Dish_longcvs$X1) %>%
  group_by(df_Dish_longcvs$Treatment_seed) %>%
  summarise(mean_Values_mm = mean(df_Dish_longcvs$Values_mm), 
            sd_Values_mm = sd(df_Dish_longcvs$Values_mm))

print(summary_stats)

#clean the na 
sum(is.na(df_Dish_longcvs))
df_clean <- df_Dish_longcvs[!is.na(df_Dish_longcvs$Values_mm), ]
df_clean
df_clean <- df_Dish_longcvs %>% 
  drop_na (Values_mm)
colnames(df_Dish_longcvs)
df_clean
class(df_clean)
levels(df_clean)
head (df_clean)
  class(df_clean$Supp_activity_bifac)
  class(df_clean$Supp_activity)
  class(df_clean$Exudates)

#Summary statistics for distance
  summary_stats <- df_clean %>%
      group_by(df_clean$Treatment_seed) %>%
    summarise(mean = mean(df_clean$Values_mm, na.rm = TRUE), 
              sd = sd(df_clean$Values_mm, na.rm = TRUE))
  print(summary_stats)
  
  
  summary_stats <- df_clean %>%
    group_by(Treatment_seed) %>%
    summarise(mean = mean(Values_mm, na.rm = TRUE), 
              sd = sd(Values_mm, na.rm = TRUE))
  
  print(summary_stats)
  
 # Treatment_seed  mean    sd
#  <fct>          <dbl> <dbl>
#    1 H2O             1.20  7.68
#  2 NB1             2.82  6.37
#  3 NB2             1.50  7.42
#  4 NB3             2.35  6.47
#  5 NB5             2.25  6.95
#  6 NB6             2.15  7.43
#  7 NB7             1.85  7.78
#  8 NB8             2.42  7.16
#  9 NB9             1.84  6.86
#  10 ABA09           1.75  7.10
#  11 ABF11           1.08  7.52
#  12 ABE11           1.75  8.26
#  13 LAG16           1.20  7.31
#  14 PVA             1.76  8.37
#  15 Proline         6.81  6.75  
  
  

#Check unique values for Treatment_seed
  unique(df_clean$Treatment_seed)
  
#Check distribution of Values_mm for each treatment
  df_clean %>%
    group_by(Treatment_seed) %>%
    summarise(count= n(), min_value = min(Values_mm), 
              max_value = max(Values_mm)) 
  
# Inspect mean and sd calculation for a specific Treatment_seed
df_NB1 <- df_clean %>% filter(Treatment_seed == "NB1")
print(mean(df_NB1$Values_mm, na.rm = TRUE))
print(sd(df_NB1$Values_mm, na.rm = TRUE))  
  
df_NB2 <- df_clean %>% filter(Treatment_seed == "NB2")
print(mean(df_NB2$Values_mm, na.rm = TRUE))
print(sd(df_NB2$Values_mm, na.rm = TRUE)) 
  
df_NB3 <- df_clean %>% filter(Treatment_seed == "NB3")
print(mean(df_NB3$Values_mm, na.rm = TRUE))
print(sd(df_NB3$Values_mm, na.rm = TRUE))  

df_NB5 <- df_clean %>% filter(Treatment_seed == "NB5")
print(mean(df_NB5$Values_mm, na.rm = TRUE))
print(sd(df_NB5$Values_mm, na.rm = TRUE))   

df_NB6 <- df_clean %>% filter(Treatment_seed == "NB6")
print(mean(df_NB6$Values_mm, na.rm = TRUE))
print(sd(df_NB6$Values_mm, na.rm = TRUE)) 

df_NB7 <- df_clean %>% filter(Treatment_seed == "NB7")
print(mean(df_NB7$Values_mm, na.rm = TRUE))
print(sd(df_NB1$Values_mm, na.rm = TRUE)) 

df_NB8 <- df_clean %>% filter(Treatment_seed == "NB8")
print(mean(df_NB8$Values_mm, na.rm = TRUE))
print(sd(df_NB8$Values_mm, na.rm = TRUE)) 

df_NB9 <- df_clean %>% filter(Treatment_seed == "NB9")
print(mean(df_NB9$Values_mm, na.rm = TRUE))
print(sd(df_NB9$Values_mm, na.rm = TRUE)) 

df_ABA09 <- df_clean %>% filter(Treatment_seed == "ABA09")
print(mean(df_ABA09$Values_mm, na.rm = TRUE))
print(sd(df_ABA09$Values_mm, na.rm = TRUE)) 

df_ABF11 <- df_clean %>% filter(Treatment_seed == "ABF11")
print(mean(df_ABF11$Values_mm, na.rm = TRUE))
print(sd(df_ABF11$Values_mm, na.rm = TRUE))

df_ABE11 <- df_clean %>% filter(Treatment_seed == "ABE11")
print(mean(df_ABE11$Values_mm, na.rm = TRUE))
print(sd(df_ABE11$Values_mm, na.rm = TRUE))

df_LAG16 <- df_clean %>% filter(Treatment_seed == "LAG16")
print(mean(df_LAG16$Values_mm, na.rm = TRUE))
print(sd(df_LAG16$Values_mm, na.rm = TRUE))

df_Proline <- df_clean %>% filter(Treatment_seed == "Proline")
print(mean(df_Proline$Values_mm, na.rm = TRUE))
print(sd(df_Proline$Values_mm, na.rm = TRUE))

df_PVA <- df_clean %>% filter(Treatment_seed == "PVA")
print(mean(df_PVA$Values_mm, na.rm = TRUE))
print(sd(df_PVA$Values_mm, na.rm = TRUE))

df_H2O <- df_clean %>% filter(Treatment_seed == "H2O")
print(mean(df_H2O$Values_mm, na.rm = TRUE))
print(sd(df_H2O$Values_mm, na.rm = TRUE))

#df_clean %>%
#  group_by(df_clean$Treatment_seed) %>%
#  filter (Supp_activity == "yes", na.rm = TRUE)
#plot (df_clean$Treatment_seed ~ df_clean$Supp_activity, 
#      fil=df_clean$Treatment_seed)


#####


# Install and load the car package
install.packages("car")
library(car)

# Check for multicollinearity
vif(lm(Values_mm ~ Treatment_seed * Timepoint, data = df_clean))

#GVIF Df GVIF^(1/(2*Df))
#Treatment_seed           1.500432e+06 14        1.661802
#Timepoint                1.827916e+01  1        4.275414
#Treatment_seed:Timepoint 1.096098e+07 14        1.784116
#> 

vif(lm(Values_mm ~ Treatment_seed * Timepoint, data = df_clean), type = "predictor")


anova_model <- aov(Values_mm ~ Treatment_seed + Timepoint + Error(Replicates/Timepoint), 
                   data = df_clean)
# Install and load the lme4 package
install.packages("lme4")
library(lme4)

# Fit the model using lmer
anova_model <- lme4::lmer(Values_mm ~ Treatment_seed * Timepoint + (1|Replicates/Timepoint), 
                    data = df_clean)

df_clean <- na.omit(df_clean)



#Repeated measures ANOVA
anova_model <-aov (Values_mm ~ Treatment_seed*Timepoint + Error(Replicates/Timepoint), 
                   data = df_clean)
summary (anova_model)

#Error: Replicates
#                Df Sum Sq Mean Sq
#Treatment_seed  9  397.5   44.17

#Error: Replicates:Timepoint
#.              Df  Sum Sq   Mean Sq
#Treatment_seed 10  14905    1490

#Error: Within
#.                            Df  Sum Sq Mean Sq  F value  Pr(>F)    
# Treatment_seed              14   5780   412.9   5.839   3.6e-11 ***
# Treatment_seed:Timepoint    14   1972   140.9   1.992   0.0156 *  
#  Residuals                 1179  83364    70.7                    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


install.packages("dplyr")
library(dplyr)
df_nopro <- df_clean %>% dplyr::filter (df_clean$Treatment_seed != "Proline")

#Repeated ANOVA measurements data withiut proline 
anova_model <-aov (Values_mm ~ Treatment_seed*Timepoint + Error(Replicates/Timepoint), 
                   data = df_nopro)
summary (anova_model)

#Error: Replicates
#                Df Sum Sq Mean Sq
#Treatment_seed  9  558.5   62.06

#Error: Replicates:Timepoint
#.              Df Sum Sq Mean Sq
#Treatment_seed 10  12937    1294

#Error: Within
#                             Df  Sum Sq Mean Sq  F value  Pr(>F)   
#Treatment_seed               13   2251  173.12   2.298    0.00535 **
#  Treatment_seed:Timepoint   13   1538  118.29   1.570    0.08745 . 
#Residuals                    1082  81520   75.34                   
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#


#logistic regression to see the effect of NB-seed treatment and day on antifungal activity
logistic_model <- glm(Supp_activity ~ Treatment_seed*Timepoint, 
                      data = df_clean, family=binomial)
summary(logistic_model)

#> summary(logistic_model)

#Call:
#  glm(formula = Supp_activity ~ Treatment_seed * Timepoint, family = binomial, 
#      data = df_clean)
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                     -2.0042482  0.5545840  -3.614 0.000302 ***
#  Treatment_seedNB2               -0.0389261  0.7552844  -0.052 0.958897    
#Treatment_seedNB3                0.9455939  0.7013935   1.348 0.177605    
#Treatment_seedNB5               -0.1249166  0.7767970  -0.161 0.872243    
#Treatment_seedNB6                0.8861635  0.6914782   1.282 0.200001    
#Treatment_seedNB7               -1.0614453  0.8990575  -1.181 0.237754    
#Treatment_seedNB8                0.2706901  0.7184684   0.377 0.706352    
#Treatment_seedNB9                0.2851535  0.7199839   0.396 0.692064    
#Treatment_seedABA09              0.0886887  0.7458024   0.119 0.905341    
#Treatment_seedABF11             -0.7279766  0.8328825  -0.874 0.382094    
#Treatment_seedABE11             -2.4727187  1.2092657  -2.045 0.040874 *  
#  Treatment_seedLAG16             -0.5174709  0.8081212  -0.640 0.521953    
#Treatment_seedH2O                0.3668246  0.7098515   0.517 0.605322    
#Treatment_seedPVA                0.0001479  0.7647185   0.000 0.999846    
#Treatment_seedProline            2.1971604  0.6629380   3.314 0.000919 ***
#  Timepoint                        0.2781963  0.0653181   4.259 2.05e-05 ***
#  Treatment_seedNB2:Timepoint     -0.1478938  0.0806092  -1.835 0.066550 .  
#Treatment_seedNB3:Timepoint     -0.1176632  0.0783444  -1.502 0.133131    
#Treatment_seedNB5:Timepoint     -0.0290555  0.0874242  -0.332 0.739625    
#Treatment_seedNB6:Timepoint     -0.1552886  0.0755461  -2.056 0.039826 *  
#  Treatment_seedNB7:Timepoint     -0.0208530  0.0968844  -0.215 0.829584    
#Treatment_seedNB8:Timepoint     -0.1673667  0.0767331  -2.181 0.029172 *  
#  Treatment_seedNB9:Timepoint     -0.1425872  0.0774439  -1.841 0.065597 .  
#Treatment_seedABA09:Timepoint   -0.1060881  0.0814405  -1.303 0.192696    
#Treatment_seedABF11:Timepoint   -0.0668906  0.0885300  -0.756 0.449907    
#Treatment_seedABE11:Timepoint    0.1007273  0.1270542   0.793 0.427900    
#Treatment_seedLAG16:Timepoint   -0.0807730  0.0866853  -0.932 0.351442    
#Treatment_seedH2O:Timepoint     -0.1581479  0.0759099  -2.083 0.037218 *  
#  Treatment_seedPVA:Timepoint     -0.0573516  0.0856219  -0.670 0.502970    
#Treatment_seedProline:Timepoint -0.2518626  0.0712829  -3.533 0.000410 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 1685.2  on 1226  degrees of freedom
#Residual deviance: 1297.4  on 1197  degrees of freedom
#AIC: 1357.4

#Number of Fisher Scoring iterations: 6


#Logistic regression _no_Proline, H2O as a control

df_nopro$Treatment_seed <- factor(df_nopro$Treatment_seed, levels = c(
  "H2O", "NB1", "NB2", "NB3", "NB5", "NB6", "NB7", 
  "NB8", "NB9", "ABA09", "ABF11", "ABE11", "LAG16", "PVA"))



logistic_model <- glm(Supp_activity ~ Treatment_seed*Timepoint, 
                      data = df_nopro, family=binomial)
summary(logistic_model)


#Call:
#  glm(formula = Supp_activity ~ Treatment_seed * Timepoint, family = binomial, 
#      data = df_nopro)

#Coefficients:
#                                Estimate Std. Error z value  Pr(>|z|)    
#(Intercept)                   -1.637424   0.443087  -3.695 0.000219 ***
#  Treatment_seedNB1             -0.366825   0.709851  -0.517 0.605322    
#Treatment_seedNB2             -0.405751   0.677655  -0.599 0.549335    
#Treatment_seedNB3              0.578769   0.617021   0.938 0.348242    
#Treatment_seedNB5             -0.491741   0.701552  -0.701 0.483345    
#Treatment_seedNB6              0.519339   0.605726   0.857 0.391234    
#Treatment_seedNB7             -1.428270   0.834905  -1.711 0.087137 .  
#Treatment_seedNB8             -0.096135   0.636364  -0.151 0.879922    
#Treatment_seedNB9             -0.081671   0.638075  -0.128 0.898152    
#Treatment_seedABA09           -0.278136   0.667071  -0.417 0.676714    
#Treatment_seedABF11           -1.094801   0.763188  -1.435 0.151427    
#Treatment_seedABE11           -2.839543   1.162362  -2.443 0.014569 *  
#  Treatment_seedLAG16           -0.884295   0.736086  -1.201 0.229616    
#Treatment_seedPVA             -0.366677   0.688155  -0.533 0.594144    
#Timepoint                      0.120048   0.038676   3.104 0.001910 ** 
#  Treatment_seedNB1:Timepoint    0.158148   0.075910   2.083 0.037218 *  
#  Treatment_seedNB2:Timepoint    0.010254   0.061051   0.168 0.866616    
#Treatment_seedNB3:Timepoint    0.040485   0.058028   0.698 0.485381    
#Treatment_seedNB5:Timepoint    0.129092   0.069803   1.849 0.064401 .  
#Treatment_seedNB6:Timepoint    0.002859   0.054191   0.053 0.957921    
#Treatment_seedNB7:Timepoint    0.137295   0.081339   1.688 0.091423 .  
#Treatment_seedNB8:Timepoint   -0.009219   0.055834  -0.165 0.868855    
#Treatment_seedNB9:Timepoint    0.015561   0.056806   0.274 0.784143    
#Treatment_seedABA09:Timepoint  0.052060   0.062145   0.838 0.402189    
#Treatment_seedABF11:Timepoint  0.091257   0.071183   1.282 0.199837    
#Treatment_seedLAG16:Timepoint  0.077375   0.068875   1.123 0.261263    
#Treatment_seedPVA:Timepoint    0.100796   0.067532   1.493 0.135548    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)
#Null deviance: 1540.4  on 1127  degrees of freedom
#Residual deviance: 1166.4  on 1100  degrees of freedom
#AIC: 1222.4
#Number of Fisher Scoring iterations: 6



#Mixed-Effects Model for Values_mm with random effect for replicate
mixed_model <- lmerTest::lmer(Values_mm ~ Treatment_seed * Timepoint + (1|Replicates), 
                     data = df_nopro)
summary(mixed_model)
print(mixed_model, correlation=TRUE) 

#Linear mixed model fit by REML. t-tests use Satterthwaite's method [
#lmerModLmerTest]
#Formula: Values_mm ~ Treatment_seed * Timepoint + (1 | Replicates)
#   Data: df_nopro

#REML criterion at convergence: 8072

#Scaled residuals: 
#    Min      1Q  Median      3Q     Max 
#-3.0783 -0.5055  0.1398  0.4902  2.3126 

#Random effects:
# Groups     Name        Variance Std.Dev.
# Replicates (Intercept)  0.6978  0.8354  
# Residual               75.9729  8.7162  
#Number of obs: 1128, groups:  Replicates, 10

#Fixed effects:
#                                Estimate Std. Error         df t value Pr(>|t|)
#(Intercept)                    7.918e+00  1.513e+00  8.059e+02   5.234 2.12e-07
#Treatment_seedNB2             -2.536e-01  2.203e+00  1.093e+03  -0.115  0.90835
#Treatment_seedNB3             -1.492e-01  2.077e+00  1.091e+03  -0.072  0.94275
#Treatment_seedNB5             -3.997e-01  2.091e+00  1.091e+03  -0.191  0.84842
#Treatment_seedNB6             -2.527e-01  2.107e+00  1.092e+03  -0.120  0.90456
#Treatment_seedNB7             -1.922e+00  2.203e+00  1.094e+03  -0.872  0.38314
#Treatment_seedNB8             -4.769e-01  2.159e+00  1.092e+03  -0.221  0.82520
#Treatment_seedNB9             -3.330e-01  2.130e+00  1.092e+03  -0.156  0.87579
#Treatment_seedABA09           -7.087e-01  2.130e+00  1.092e+03  -0.333  0.73937
#Treatment_seedABF11           -1.805e+00  2.159e+00  1.093e+03  -0.836  0.40346
#Treatment_seedABE11           -1.377e+00  2.206e+00  1.094e+03  -0.624  0.53280
#Treatment_seedLAG16           -1.452e+00  2.159e+00  1.092e+03  -0.672  0.50151
#Treatment_seedH2O             -2.044e+00  2.130e+00  1.092e+03  -0.960  0.33737
#Treatment_seedPVA             -7.705e-01  2.108e+00  1.091e+03  -0.365  0.71481
#Timepoint                     -3.472e-01  1.102e-01  1.098e+03  -3.152  0.00167
#Treatment_seedNB2:Timepoint   -3.498e-01  1.856e-01  1.100e+03  -1.885  0.05968
#Treatment_seedNB3:Timepoint    1.040e-02  1.482e-01  1.099e+03   0.070  0.94404
#Treatment_seedNB5:Timepoint    1.731e-02  1.515e-01  1.096e+03   0.114  0.90905
#Treatment_seedNB6:Timepoint   -7.046e-02  1.558e-01  1.099e+03  -0.452  0.65125
#Treatment_seedNB7:Timepoint   -2.115e-01  1.860e-01  1.100e+03  -1.137  0.25585
#Treatment_seedNB8:Timepoint   -2.271e-01  1.728e-01  1.099e+03  -1.314  0.18915
#Treatment_seedNB9:Timepoint   -1.530e-01  1.633e-01  1.100e+03  -0.937  0.34881
#Treatment_seedABA09:Timepoint -6.631e-02  1.633e-01  1.100e+03  -0.406  0.68474
#Treatment_seedABE11:Timepoint -2.265e-01  1.896e-01  1.099e+03  -1.195  0.23243
#Treatment_seedLAG16:Timepoint -1.227e-01  1.728e-01  1.099e+03  -0.710  0.47770
#Treatment_seedPVA:Timepoint    4.749e-02  1.563e-01  1.097e+03   0.304  0.76136
#                                 
#(Intercept)                   ***
#Treatment_seedNB2                
#Treatment_seedNB3                
#Treatment_seedNB5                
#Treatment_seedNB6                
#Treatment_seedNB7                
#Treatment_seedNB8                
#Treatment_seedNB9                
#Treatment_seedABA09              
#Treatment_seedABF11              
#Treatment_seedABE11              
#Treatment_seedLAG16              
#Treatment_seedH2O                
#Treatment_seedPVA                
#Timepoint                     ** 
#Treatment_seedNB2:Timepoint   .  
#Treatment_seedNB3:Timepoint      
#Treatment_seedNB5:Timepoint      
#Treatment_seedNB6:Timepoint      
#Treatment_seedNB7:Timepoint      
#Treatment_seedNB8:Timepoint      
#Treatment_seedNB9:Timepoint      
#Treatment_seedABA09:Timepoint    
#Treatment_seedABF11:Timepoint    
#Treatment_seedABE11:Timepoint    
#Treatment_seedLAG16:Timepoint    
#Treatment_seedH2O:Timepoint      
#Treatment_seedPVA:Timepoint      
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#    

#> print(mixed_model, correlation=TRUE) 
#Linear mixed model fit by REML ['lmerModLmerTest']
#Formula: Values_mm ~ Treatment_seed * Timepoint + (1 | Replicates)
#Data: df_nopro
#REML criterion at convergence: 8071.968
#Random effects:
#  Groups     Name        Std.Dev.
#Replicates (Intercept) 0.8354  
#Residual               8.7162  
#Number of obs: 1128, groups:  Replicates, 10
#Fixed Effects:
#  (Intercept)              Treatment_seedNB2  
#7.918310                      -0.253620  
#Treatment_seedNB3              Treatment_seedNB5  
#-0.149187                      -0.399735  
#Treatment_seedNB6              Treatment_seedNB7  
#-0.252683                      -1.922209  
#Treatment_seedNB8              Treatment_seedNB9  
#-0.476934                      -0.332991  
#Treatment_seedABA09            Treatment_seedABF11  
#-0.708740                      -1.804737  
#Treatment_seedABE11            Treatment_seedLAG16  
#-1.376520                      -1.451566  
#Treatment_seedH2O              Treatment_seedPVA  
#-2.044053                      -0.770490  
#Timepoint    Treatment_seedNB2:Timepoint  
#-0.347187                      -0.349811  
#Treatment_seedNB3:Timepoint    Treatment_seedNB5:Timepoint  
#0.010402                       0.017313  
#Treatment_seedNB6:Timepoint    Treatment_seedNB7:Timepoint  
#-0.070455                      -0.211487  
#Treatment_seedNB8:Timepoint    Treatment_seedNB9:Timepoint  
#-0.227058                      -0.153039  
#Treatment_seedABA09:Timepoint  Treatment_seedABF11:Timepoint  
#-0.066307                      -0.134391  
#Treatment_seedABE11:Timepoint  Treatment_seedLAG16:Timepoint  
#-0.226521                      -0.122737  
#Treatment_seedH2O:Timepoint    Treatment_seedPVA:Timepoint  
#0.007109                       0.047489  
#> 


# Mann-Whitney U Test
wilcox.test(Values_mm ~ Treatment_seed, data = df_clean)

# Kruskal-Wallis H Test
kruskal.test(Values_mm ~ Treatment_seed, data = df_clean)

# Wilcoxon Signed-Rank Test
wilcox.test(df_clean$Values_mm_, df_clean$Values_mm_after, paired = TRUE)

#Pearson’s Chi-Squared Test of Independence, which is used to determine 
#if there is a significant association between two categorical variables 
#Create a contingency table
table <- table(df_clean$Supp_activity, df_clean$Timepoint)

# Chi-Square Test of Independence
chisq.test(table)

# Create a contingency table
table <- table(df_clean$Treatment_seed, df_clean$Supp_activity)

# Chi-Square Test of Independence
chisq.test(table)

# Fisher's Exact Test
fisher.test(table)

#Tukey’s HSD (Honestly Significant Difference)
#This test is used after ANOVA to determine which specific groups differ from each other. 
#Perform ANOVA
anova_model <- aov(Values_mm ~ Treatment_seed, data = df_clean)

#Tukey’s HSD 
TukeyHSD(anova_model)

#Bonferroni correction
# Perform pairwise t-tests with Bonferroni correction
pairwise.t.test(df_clean$Values_mm, df_clean$Treatment_seed, p.adjust.method = "bonferroni")

#Bonferroni correction
# Perform pairwise t-tests with Bonferroni correction
pairwise.t.test(df_nopro$Values_mm, df_nopro$Treatment_seed, p.adjust.method = "bonferroni")



#Pairwise comparisons using t tests with pooled SD 

#data:  df_clean$Values_mm and df_clean$Treatment_seed 

#H2O     NB1     NB2     NB3     NB5     NB6     NB7     NB8     NB9     ABA09   ABF11   ABE11  
#NB1     1.00000 -       -       -       -       -       -       -       -       -       -       -      
#  NB2     1.00000 1.00000 -       -       -       -       -       -       -       -       -       -      
#  NB3     1.00000 1.00000 1.00000 -       -       -       -       -       -       -       -       -      
#  NB5     1.00000 1.00000 1.00000 1.00000 -       -       -       -       -       -       -       -      
#  NB6     1.00000 1.00000 1.00000 1.00000 1.00000 -       -       -       -       -       -       -      
#  NB7     1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 -       -       -       -       -       -      
#  NB8     1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 -       -       -       -       -      
#  NB9     1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 -       -       -       -      
#  ABA09   1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 -       -       -      
#  ABF11   1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 -       -      
#  LAG16   1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000
#PVA     1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000 1.00000
#Proline 0.00201 0.18799 0.00030 0.08354 0.07297 0.01678 8.4e-05 0.00189 0.00495 0.01627 0.00041 0.00085
#LAG16   PVA    
#NB1     -       -      
#  NB2     -       -      
#  NB3     -       -      
#  NB5     -       -      
#  NB6     -       -      
#  NB7     -       -      
#  NB8     -       -      
#  NB9     -       -      
#  ABA09   -       -      
#  ABF11   -       -      
#  ABE11   -       -      
#  LAG16   -       -      
#  PVA     1.00000 -      
#  Proline 0.00153 0.15262

#P value adjustment method: bonferroni 
# #Bonferroni correction
#  Perform pairwise t-tests with Bonferroni correction
#  pairwise.t.test(df_nopro$Values_mm, df_nopro$Treatment_seed, p.adjust.method = "bonferroni")

#Pairwise comparisons using t tests with pooled SD 

#data:  df_nopro$Values_mm and df_nopro$Treatment_seed 

#H2O NB1 NB2 NB3 NB5 NB6 NB7 NB8 NB9 ABA09 ABF11 ABE11 LAG16
#NB1   1   -   -   -   -   -   -   -   -   -     -     -     -    
#  NB2   1   1   -   -   -   -   -   -   -   -     -     -     -    
#  NB3   1   1   1   -   -   -   -   -   -   -     -     -     -    
#  NB5   1   1   1   1   -   -   -   -   -   -     -     -     -    
#  NB6   1   1   1   1   1   -   -   -   -   -     -     -     -    
#  NB7   1   1   1   1   1   1   -   -   -   -     -     -     -    
#  NB8   1   1   1   1   1   1   1   -   -   -     -     -     -    
#  NB9   1   1   1   1   1   1   1   1   -   -     -     -     -    
#  ABA09 1   1   1   1   1   1   1   1   1   -     -     -     -    
#  ABF11 1   1   1   1   1   1   1   1   1   1     -     -     -    
#  ABE11 1   1   1   1   1   1   1   1   1   1     1     -     -    
#  LAG16 1   1   1   1   1   1   1   1   1   1     1     1     -    
#  PVA   1   1   1   1   1   1   1   1   1   1     1     1     1    




#correlation between antifungal activity and seed exudates
df_nopro$Supp_activity_bi <-ifelse(df_nopro$Supp_activity == "yes", 1, 0)
df_nopro$Supp_activity_bifac <- factor(df_nopro$Supp_activity_bi)
class(df_nopro$Supp_activity_bifac)
levels(df_nopro$Supp_activity_bifac)

df_nopro$Exudates_bi <-ifelse(df_nopro$Exudates == "yes", 1, 0)
df_nopro$Exudates_bifac <- factor(df_nopro$Exudates_bi)
class(df_nopro$Exudates_bifac)
levels(df_nopro$Exudates_bifac)

df_nopro$Supp_activity_bi <-ifelse(df_nopro$Supp_activity == "yes", 1, 0)
df_nopro$Supp_activity_bifac <- as.numeric(df_nopro$Supp_activity_bi)
class(df_nopro$Supp_activity_bifac)
levels(df_nopro$Supp_activity_bifac)

df_nopro$Exudates_bi <-ifelse(df_nopro$Exudates == "yes", 1, 0)
df_nopro$Exudates_bifac <- as.numeric(df_nopro$Exudates_bi)
class(df_nopro$Exudates_bifac)
levels(df_nopro$Exudates_bifac)

#*****
correlation <- cor.test(df_nopro$Supp_activity_bifac, 
                        df_nopro$Exudates_bifac)
print(correlation)

#Pearson's product-moment correlation

#data:  df_nopro$Supp_activity_bifac and df_nopro$Exudates_bifac
#t = 22.46, df = 1126, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.5145673 0.5952718
#sample estimates:
#      cor  0.5562297 


#correlation Supp_activity vs. Values_mm
correlation <- cor.test(df_nopro$Supp_activity_bifac, 
                        df_nopro$Values_mm)
print(correlation)

#Pearson's product-moment correlation

#data:  df_nopro$Supp_activity_bifac and df_nopro$Values_mm
#t = 2.4795, df = 1126, p-value = 0.0133
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.01538985 0.13149489
#sample estimates:
#       cor 0.07369207 


#correlation Exudates vs. Values_mm
correlation <- cor.test(df_nopro$Exudates_bifac, 
                        df_nopro$Values_mm)
print(correlation)

#Pearson's product-moment correlation

#data:  df_nopro$Exudates_bifac and df_nopro$Values_mm
#t = -7.4387, df = 1126, p-value = 2.011e-13
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.2713671 -0.1600805
#sample estimates:
#       cor -0.2164267

#visualisation corr plots

install.packages("ggplot2")
library(ggplot2)

#Scatter plot########## with jitter for binary variable and smooth line
str(df_nopro)
summary(df_nopro)
df_nopro$Supp_activity_bifac <- as.factor(df_nopro$Supp_activity_bifac)
class(df_nopro$Supp_activity_bifac)
df_nopro$Exudates_bifac <- as.factor(df_nopro$Exudates_bifac)
class(df_nopro$Exudates_bifac)
df_nopro$Treatment_seed <-as.factor(df_nopro$Treatment_seed)
class(df_nopro$Treatment_seed)
  
  
update.packages("ggplot2")

  p <- ggplot(df_nopro, aes(x=as.factor(Supp_activity), y=Values_mm))+
  geom_jitter( width = 0.1, height = 0, aes (color = as.factor(Supp_activity)))+
  geom_smooth(method = "lm", se = FALSE, color = "black")+
  labs(title = "Scetter Plot of Suppress Activity vs. FOL growth, mm",
       x = "Suppress Activity",
       y = "FOL groth values, mm",
       color = "Supp_activity_bifac") +
  scale_color_manual(values = c("red", "green"), labels = c("No", "Yes"))+
  theme_minimal()+
facet_wrap( ~ Treatment_seed, scales = "free")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)

  p <- p+geom_smooth(method = "lm", se = FALSE, color = "black")
  
  
#percentages######### of "Yes/No" Supp_activity/ Exudates
#Load required packages
  install.packages("dplyr")
  library(dplyr)
  library(ggplot2)
  
  # Calculate percentages for Supp_activity
  percentages <- df_nopro %>%
    group_by(df_nopro$Treatment_seed, df_nopro$Supp_activity) %>%
    summarise(count = n()) %>%
    mutate(percentage = count / sum(count) * 100)
  
  # Print the calculated percentages
  print(percentages, n= 44) 
  
  
  
  df_pSup_Ex <- percentages
 
  # A tibble: 28 × 4
  # Groups:   df_nopro$Treatment_seed [14]
 # `df_nopro$Treatment_seed` `df_nopro$Supp_activity` count percentage
#  <fct>                     <fct>                    <int>      <dbl>
#    1 NB1                       no                          36       40.9
#  2 NB1                       yes                         52       59.1
#  3 NB2                       no                          52       70.3
#  4 NB2                       yes                         22       29.7
#  5 NB3                       no                          36       37.9
#  6 NB3                       yes                         59       62.1
#  7 NB5                       no                          41       45.6
#  8 NB5                       yes                         49       54.4
#  9 NB6                       no                          42       47.7
#  10 NB6                       yes                         46       52.3
#  11 NB7                       no                          50       68.5
#  12 NB7                       yes                         23       31.5
#  13 NB8                       no                          50       66.7
#  14 NB8                       yes                         25       33.3
#  15 NB9                       no                          48       60  
#  16 NB9                       yes                         32       40  
#  17 ABA09                     no                          46       57.5
#  18 ABA09                     yes                         34       42.5
#  19 ABF11                     no                          51       68  
#  20 ABF11                     yes                         24       32  
#  21 ABE11                     no                          52       74.3
#  22 ABE11                     yes                         18       25.7
#  23 LAG16                     no                          50       66.7
#  24 LAG16                     yes                         25       33.3
#  25 H2O                       no                          49       61.3
#  26 H2O                       yes                         31       38.8
#  27 PVA                       no                          42       49.4
#  28 PVA                       yes                         43       50.6
 
  # Calculate percentages for Exudates
  percentages <- df_nopro %>%
    group_by(df_nopro$Treatment_seed, df_nopro$Exudates) %>%
    summarise(count = n()) %>%
    mutate(percentage = count / sum(count) * 100)
  
  # Print the calculated percentages
  print(percentages, n= 28)   
  
# A tibble: 28 × 4
# Groups:   df_nopro$Treatment_seed [14]
#  `df_nopro$Treatment_seed` `df_nopro$Exudates` count percentage
#  <fct>                     <fct>               <int>      <dbl>
#    1 NB1                       no                     25       28.4
#  2 NB1                       yes                    63       71.6
#  3 NB2                       no                     19       25.7
#  4 NB2                       yes                    55       74.3
#  5 NB3                       no                     16       16.8
#  6 NB3                       yes                    79       83.2
#  7 NB5                       no                     27       30  
#  8 NB5                       yes                    63       70  
#  9 NB6                       no                     11       12.5
#  10 NB6                       yes                    77       87.5
#  11 NB7                       no                     44       60.3
#  12 NB7                       yes                    29       39.7
#  13 NB8                       no                     50       66.7
#  14 NB8                       yes                    25       33.3
#  15 NB9                       no                     40       50  
#  16 NB9                       yes                    40       50  
#  17 ABA09                     no                     18       22.5
#  18 ABA09                     yes                    62       77.5
#  19 ABF11                     no                     15       20  
#  20 ABF11                     yes                    60       80  
#  21 ABE11                     no                     29       41.4
#  22 ABE11                     yes                    41       58.6
#  23 LAG16                     no                     16       21.3
#  24 LAG16                     yes                    59       78.7
#  25 H2O                       no                     35       43.8
#  26 H2O                       yes                    45       56.2
#  27 PVA                       no                     26       30.6
#  28 PVA                       yes                    59       69.4  
   
#percentage Supp_activity vs. Exudates 
  percentages <- df_nopro %>%
    group_by(df_nopro$Supp_activity, df_nopro$Exudates) %>%
    summarise(count = n()) %>%
    mutate(percentage = count / sum(count) * 100)
  
  # Print the calculated percentages
  print(percentages, n= 28) 
  
  # A tibble: 4 × 4
  # Groups:   df_nopro$Supp_activity [2]
#`df_nopro$Supp_activity` `df_nopro$Exudates` count percentage
#  <fct>                    <fct>               <int>      <dbl>
#  1 no                       no                    358      55.5 
#  2 no                       yes                   287      44.5 
#  3 yes                      no                    13       2.69
#  4 yes                      yes                   470      97.3 
#  >  
  
  
 
   # A tibble: 4 × 4
  # Groups:   df_nopro$Exudates [2]
#`df_nopro$Exudates` `df_nopro$Supp_activity` count percentage
#  <fct>               <fct>                    <int>      <dbl>
#  1 no                  no                         358      96.5 
#  2 no                  yes                         13       3.50
#  3 yes                 no                         287      37.9 
#  4 yes                 yes                        470      62.1 
  
  
  # Ensure the necessary libraries are loaded
  library(dplyr)
  library(ggplot2)
  install.packages("ggmosaic")
  library(ggmosaic)
  
  # Ensure the columns are factors where needed
  df_clean$Supp_activity <- as.factor(df_clean$Supp_activity)
  df_clean$Treatment_seed <- as.factor(df_clean$Treatment_seed)
  
  
# Calculate percentages including Exudates
  percentages <- df_clean %>%
    group_by(Treatment_seed, Supp_activity) %>%
    summarise(count = n()) %>%
    mutate(percentage = count / sum(count) * 100)
  
# Create the plot
  ggplot(df_clean, aes(x = Supp_activity)) +
    geom_bar(aes(fill = Supp_activity), color = "black") +
    labs(title = "Distribution of Antifungal Activity", x = "Supp_activity", y = "Count") +
    scale_fill_manual(values = c("blue", "orange"), labels = c("No", "Yes")) +
    facet_wrap(~Treatment_seed + Timepoint, scale="free")+
    theme_minimal()
  
  ggplot(df_clean, aes(x = Exudates)) +
    geom_bar(aes(fill = Exudates), color = "black") +
    labs(title = "Distribution of Exudates", x = "Exudates", y = "Count") +
    scale_fill_manual(values = c("red", "green"), labels = c("No", "Yes")) +
    facet_wrap(~Timepoint, scale="free")+
    theme_minimal()
  
  
  #Mosaic plot
  
  ggplot(data = df_clean) +
    geom_mosaic(aes(x = product(Exudates, Supp_activity), fill = Supp_activity)) +
    labs(title = "Mosaic Plot of Antifungal Activity vs. Seed Exudates", x = "Seed Exudates", y = "Antifungal Activity") +
    scale_fill_manual(values = c("yellow", "green"), labels = c("No", "Yes")) +
    facet_wrap(~Timepoint)+
    theme_minimal()
  
  
  df_long <- df_clean %>%
    mutate(Supp_activity = as.factor(Supp_activity),
           Exudates = as.factor(Exudates)) %>%
    pivot_longer(cols = c(Supp_activity, Exudates), names_to = "Variable", values_to = "Value")
  
  ggplot(df_long, aes(x = Value)) +
    geom_bar(aes(fill = Value), color = "black") +
    facet_wrap(~ Variable, scales = "free_x") +
    labs(title = "Distribution of Antifungal Activity and Seed Exudates", x = "Value", y = "Count") +
    scale_fill_manual(values = c("red", "green", "blue", "orange"), labels = c("No", "Yes", "No", "Yes")) +
    theme_minimal()
  
  # Scatter plot with jitter for binary variable and smooth line
  ggplot(df_clean, aes(x = Supp_activity, y = Values_mm)) +
    geom_jitter(width = 0.1, height = 0, aes(color = Supp_activity)) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    labs(title = "Scatter Plot of Antifungal Activity vs. Values_mm",
         x = "Antifungal Activity",
         y = "Values_mm",
         color = "Antifungal Activity") +
    facet_wrap(~Timepoint, scale="free")+
    facet_grid(~Treatment_seed)+
    scale_color_manual(values = c("red", "green"), labels = c("No", "Yes")) +
    theme_minimal()
  
  
  
#Ensure the columns are factors where needed
  df_nopro$Supp_activity_bifac <- as.factor(df_nopro$Supp_activity_bifac)
  df_nopro$Treatment_seed <- as.factor(df_nopro$Treatment_seed)
  df_nopro$Exudates_bifac <- as.factor(df_nopro$Exudates_bifac)
  
  # Basic scatter plot with jitter and smooth line
  p <- ggplot(df_clean, aes(x = Supp_activity, y = Values_mm)) +
    geom_jitter(width = 0.1, height = 0, aes(color = Supp_activity)) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    labs(title = "Scatter Plot of Suppress Activity vs. FOL Growth, mm",
         x = "Suppress Activity",
         y = "FOL Growth Values, mm",
         color = "Suppress Activity") +
    scale_color_manual(values = c("red", "green"), labels = c("No", "Yes")) +
    theme_minimal() +
    facet_wrap(~ Treatment_seed + Exudates, scales = "free") +
    #facet_grid(~ Timepoint, scales = "free")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Annotate plot with percentages
  p + geom_text(data = percentages, 
                aes(x = Supp_activity, 
                    y = Inf, 
                    label = paste0(round(percentage, 1), "%")), 
                vjust = 1.5, 
                position = position_dodge(width = 0.9),
                size = 2,
                color = "black")
  
  
  
  
  
  
  
  p <- ggplot(df_clean, aes(x = Supp_activity, y = Values_mm)) +
    geom_jitter(width = 0.1, height = 0, aes(color = Supp_activity)) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    labs(title = "Scatter Plot of Suppress Activity vs. FOL Growth, mm",
         x = "Suppress Activity",
         y = "FOL Growth Values, mm",
         color = "Suppress Activity") +
    scale_color_manual(values = c("red", "green"), labels = c("No", "Yes")) +
    theme_minimal() +
    facet_wrap(~ Treatment_seed + Supp_activity, scales = "free") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print (p)
  
  # Annotate plot with percentages
  p + geom_text(data = percentages, 
                aes(x = df_nopro$Supp_activity, 
                    y = Inf, 
                    label = paste0(round(percentage, 1), "%")), 
                vjust = 1.5, 
                position = position_dodge(width = 0.9),
                size = 3,
                color = "black") 
  
  
  
  
  
  
  
  
# Ensure the columns are factors where needed
  df_nopro$Supp_activity_bifac <- as.factor(df_nopro$Supp_activity_bifac)
  df_nopro$Treatment_seed <- as.factor(df_nopro$Treatment_seed)
  df_nopro$Exudates_bifac <- as.factor(df_nopro$Exudates_bifac)
  
# Basic scatter plot with jitter and smooth line
  p <- ggplot(df_nopro, aes(x = Supp_activity_bifac, y = Values_mm)) +
    geom_jitter(width = 0.1, height = 0, aes(color = Supp_activity_bifac)) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    labs(title = "Scatter Plot of Suppress Activity vs. FOL Growth, mm",
         x = "Suppress Activity",
         y = "FOL Growth Values, mm",
         color = "Suppress Activity") +
    scale_color_manual(values = c("red", "green"), labels = c("No", "Yes")) +
    theme_minimal() +
    facet_wrap(~ Treatment_seed, scales = "free") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# Annotate plot with percentages
  p + geom_text(data = percentages, 
                aes(x = Supp_activity_bifac, 
                    y = Inf, 
                    label = paste0(round(percentage, 1), "%")), 
                vjust = 1.5, 
                position = position_dodge(width = 0.9),
                size = 3,
                color = "black")  
  
 
####full plot with percentages########
  # Load required packages
  library(dplyr)
  library(ggplot2)
  
  # Ensure the columns are factors where needed
  df_nopro$Supp_activity_bifac <- as.factor(df_nopro$Supp_activity_bifac)
  df_nopro$Treatment_seed <- as.factor(df_nopro$Treatment_seed)
  df_nopro$Exudates_bifac <- as.factor(df_nopro$Exudates_bifac)
  
  # Calculate percentages including Exudates
  percentages <- df_nopro %>%
    group_by(Treatment_seed, Supp_activity, Exudates) %>%
    summarise(count = n()) %>%
    mutate(percentage = count / sum(count) * 100)
  
  # Print the calculated percentages
  print(percentages)
  
  # Create the plot Supp_act vs Exudates facet in Treatment in %
  p <- ggplot(df_clean, aes(x = Supp_activity, y = Values_mm)) +
    geom_jitter(width = 0.1, height = 0, aes(color = Supp_activity)) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    labs(title = "Scatter Plot of Suppress Activity vs. FOL Growth, mm",
         x = "Suppress Activity",
         y = "FOL Growth Values, mm",
         color = "Suppress Activity") +
    scale_color_manual(values = c("red", "green"), labels = c("No", "Yes")) +
    theme_minimal() +
    facet_wrap(~ Treatment_seed + Exudates, scales = "free") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 16),  # Increase x-axis text size
          axis.text.y = element_text(size = 10),  # Increase y-axis text size
          axis.title.x = element_text(size = 14),  # Increase x-axis title size
          axis.title.y = element_text(size = 14),  # Increase y-axis title size
          plot.title = element_text(size = 20),
          strip.text = element_text(size = 20) )  # Increase plot title size
  
  # Annotate plot with percentages
  p + geom_text(data = percentages, 
                aes(x = Supp_activity, 
                    y = Inf, 
                    label = paste0(round(percentage, 2), "%")), 
                vjust = 2.5, 
                position = position_dodge(width = 0.9),
                size = 5,
                color = "black")
  
  
  # Create the plot Supp_act + Exudates vs Timepoint  in %
  p <- ggplot(df_clean, aes(x = Supp_activity, y = Values_mm)) +
    geom_jitter(width = 0.1, height = 0, aes(color = Supp_activity)) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    labs(title = "Scatter Plot of Suppress Activity vs. FOL Growth, mm",
         x = "Suppress Activity",
         y = "FOL Growth Values, mm",
         color = "Suppress Activity") +
    scale_color_manual(values = c("orange", "blue"), labels = c("No", "Yes")) +
    theme_minimal() +
    facet_wrap(~ Timepoint + Exudates, scales = "free") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 16),  # Increase x-axis text size
          axis.text.y = element_text(size = 10),  # Increase y-axis text size
          axis.title.x = element_text(size = 14),  # Increase x-axis title size
          axis.title.y = element_text(size = 14),  # Increase y-axis title size
          plot.title = element_text(size = 20),
          strip.text = element_text(size = 20) )  # Increase plot title size
  
  # Annotate plot with percentages
  p + geom_text(data = percentages, 
                aes(x = Supp_activity, 
                    y = Inf, 
                    label = paste0(round(percentage, 2), "%")), 
                vjust = 2.5, 
                position = position_dodge(width = 0.9),
                size = 5,
                color = "black")
  

