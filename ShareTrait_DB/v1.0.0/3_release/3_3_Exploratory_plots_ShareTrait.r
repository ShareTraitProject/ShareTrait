#clear your work environment
rm(list=ls()) 

#Libraries
library(dplyr)
library(tidyverse)
library(ggExtra)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)
library(stringr)
library(cowplot)
library(ggrepel)
library(colorRamps)

df <- read.csv("../3_release/2_2_ShareTrait_v0.0.1.csv")  %>%
  filter(!is.na(trait_value))
  
Fig_1a <- df %>%
  filter(trait_name == "metabolic_rate") %>%
  filter(!is.na(fresh_mass_gram)) %>%
  filter(trait_value > 0 ) %>%
  ggplot(aes(x = log10(fresh_mass_gram), y = log10(trait_value))) + 
  geom_point(size = 3, aes(color = realm_general), alpha = 0.4) +
  scale_x_continuous(
    name = "Fresh mass in grams",
    labels = scales::math_format(10^.x),
    limits = c(-6, 4)) +
  scale_y_continuous(
    name = "Metabolic rate (mg oxygen per hour per ind)",
    labels = scales::math_format(10^.x),
    limits = c(-6, 4)) +
  annotation_logticks() +
  scale_color_manual(values = c("brackish" = "#FF7733", "freshwater" = "#0077CC", "intertidal" = "#55AA33", "terrestrial" = "#885544")) +  # Specific colors in RGB format
  theme_bw() + 
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  theme(legend.position = "bottom",  # Legend position
        legend.justification = "center",  # Legend justification
        legend.box = "horizontal",  # Legend box style
        legend.title = element_blank(),  # Remove legend title
        axis.title.x = element_text(size = 12),  # Size of x-axis label text
        axis.title.y = element_text(size = 12),  # Size of y-axis label text
        axis.text = element_text(size = 10),  # Size of axis tick labels
        axis.title = element_text(size = 12)) +  # Size of axis titles
  annotate("text", x = 0, y = -6.5, label = "Fresh mass in grams", hjust = 0, size = 4) +  # Annotation for x-axis label
  annotate("text", x = -7, y = 0, label = "Metabolic rate (mg oxygen per hour per ind)", hjust = 0, size = 4)  # Annotation for y-axis label

Fig_1a


Fig_1b <- df %>%
  filter(trait_name == "development") %>%
  filter(!is.na(test_temperature)) %>%
  ggplot(aes(x = test_temperature, y = trait_value)) + 
  geom_point(size = 3, alpha = 0.6, shape = 16, aes(color = realm_general)) +
  scale_color_manual(values = c("brackish" = "#FF7733", "freshwater" = "#0077CC", "intertidal" = "#55AA33", "terrestrial" = "#885544")) +  # Specify specific colors for each category in realm_general
  scale_x_continuous(
    name = "Test Temperature",  # X-axis label
    limits = c(0,40),  # Specify the desired limits
    labels = scales::math_format()  # Format the x-axis tick labels
  ) +
  scale_y_continuous(
    name = "Development time (1/days)",  # Y-axis label
    limits = c(0,100),  # Specify the desired limits
    labels = scales::math_format()  # Format the y-axis tick labels
  ) +
  theme_bw() +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  theme(
    legend.position = "bottom",  # Position the legend at the bottom
    legend.justification = "center",  # Center the legend horizontally
    legend.box = "horizontal",  # Style the legend box
    legend.title = element_blank(),  # Remove the legend title
    axis.title.x = element_text(size = 12),  # Size of x-axis label text
    axis.title.y = element_text(size = 12),  # Size of y-axis label text
    axis.text = element_text(size = 10),  # Size of axis tick labels
    axis.title = element_text(size = 12)  # Size of axis titles
  )

Fig_1b



Fig_1c <- df %>%
  filter(trait_name == "fecundity") %>%
  filter(!is.na(fresh_mass_gram)) %>%
  filter(trait_value > 0 ) %>%
  ggplot(aes(x = log10(fresh_mass_gram), y = log10(trait_value))) + 
  geom_point(size = 3, aes(color = realm_general), alpha = 0.4) +
  scale_x_continuous(
    name = "Fresh mass in grams",
    labels = scales::math_format(10^.x),
    limits = c(-6, 4)) +
  scale_y_continuous(
    name = "Metabolic rate (mg oxygen per hour per ind)",
    labels = scales::math_format(10^.x),
    limits = c(-6, 4)) +
  annotation_logticks() +
  scale_color_manual(values = c("brackish" = "#FF7733", "freshwater" = "#0077CC", "intertidal" = "#55AA33", "terrestrial" = "#885544")) +  # Specific colors in RGB format
  theme_bw() + 
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  theme(legend.position = "bottom",  # Legend position
        legend.justification = "center",  # Legend justification
        legend.box = "horizontal",  # Legend box style
        legend.title = element_blank(),  # Remove legend title
        axis.title.x = element_text(size = 12),  # Size of x-axis label text
        axis.title.y = element_text(size = 12),  # Size of y-axis label text
        axis.text = element_text(size = 10),  # Size of axis tick labels
        axis.title = element_text(size = 12)) +  # Size of axis titles
  annotate("text", x = 0, y = -6.5, label = "Fresh mass in grams", hjust = 0, size = 4) +  # Annotation for x-axis label
  annotate("text", x = -7, y = 0, label = "Metabolic rate (mg oxygen per hour per ind)", hjust = 0, size = 4)  # Annotation for y-axis label

Fig_1c


#-------------------------------------------------------------------------------
Figure_1 <- plot_grid(Fig_1a,Fig_1b,Fig_1c,nrow = 1,ncol = 3)

ggsave('../3_release/Metabolism_Development_Fecundity.pdf',Figure_1,width=30,height=8)

# saving session information with all packages versions for reproducibility purposes

sink("../outputs/2_2_trait_units_R_session.txt")
sessionInfo()
sink()
# END OF SCRIPT
