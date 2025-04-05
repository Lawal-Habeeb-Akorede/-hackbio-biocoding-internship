#Task code 2.3 Botany and plant

library(ggplot2)
library(tidyr)
library(readr)

botany_data<-read.csv("https://raw.githubusercontent.com/HackBio-Internship/2025_project_collection/refs/heads/main/Python/Dataset/Pesticide_treatment_data.txt", header = TRUE, sep= "\t", row.names = 1)
  

wt_dmso <- botany_data %>% filter(grepl("WT_DMSO", rownames(botany_data)))
#print(wt_dmso)
wt_24h <- botany_data %>% filter(grepl("WT_pesticide_24h", rownames(botany_data)))
mutant_dmso <- botany_data %>% filter(grepl("mutant_DMSO", rownames(botany_data)))
mutant_24h <- botany_data %>% filter(grepl("mutant_pesticide_24h", rownames(botany_data)))
delta_M_wt <- wt_24h - wt_dmso
delta_M_mutant <- mutant_24h - mutant_dmso


delta_M <- data.frame(
  WT = as.numeric(delta_M_wt),
  Mutant = as.numeric(delta_M_mutant)
)

delta_M$Residual <- delta_M$Mutant - delta_M$WT
residual_cutoff <- 0.3
delta_M$Residual_Color <- ifelse(
  abs(delta_M$Residual) <= residual_cutoff, "Within Cutoff", "Outside Cutoff"
)



# Start plotting 

ggplot(delta_M, aes(x = WT, y = Mutant, color = Residual_Color)) +
  geom_point(size = 2) +  # Scatter plot points
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # y = x line
  scale_color_manual(values = c("Within Cutoff" = "grey", "Outside Cutoff" = "salmon"))+# Color mapping
  labs(
    title = "Difference in Metabolic Response (ΔM)",
    x = "ΔM (WT)",
    y = "ΔM (Mutant)",
    color = "Residual Cutoff"
    
  ) +theme_minimal() +

  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )  


# Define the outliers 

outliers <- delta_M[abs(delta_M$Residual) > residual_cutoff, ]
outlier_metabolites <- rownames(outliers)[1:6]  # Select the first 6 metabolites
metabolite_data <- botany_data[outlier_metabolites, ]
plot_data <- data.frame(
  Metabolite = rep(outlier_metabolites, each = 6),
  Time = rep(c("0h", "8h", "24h"), times = 2 * length(outlier_metabolites)),
  Treatment = rep(c("WT", "Mutant"), each = 3, times = length(outlier_metabolites)),
  Value = as.numeric(t(metabolite_data))
)
ggplot(plot_data, aes(x = Time, y = Value, color = Treatment, group = Treatment)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  facet_wrap(~ Metabolite, scales = "free_y") +
  labs(
    title = "Metabolite Levels Over Time (0h, 8h, 24h)",
    x = "Time",
    y = "Metabolite Level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12, face = "bold")
  ) 
