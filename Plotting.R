library(tidyverse)
library(ggplot2)
library(viridisLite)

data <- data.frame(
  Design = factor(rep(c("Compact", "Diffuse"), each = 3), levels = c("Compact", "Diffuse")),
  Coefficient = rep(c("Sigma", "Density", "Detection"), times = 2),
  Estimate = c(12.23, 0.12, 0.023, 3.27, 0.534, 0.023),
  CI_lower = c(8.78, 0.041, 0.013, 2.45, 0.256, 0.013),
  CI_upper = c(17.04, 0.316, 0.040, 4.35, 1.113, 0.040),
  Session = factor(rep(c("0", "1"), each = 3))
)

plot <- ggplot(data, aes(x = Session, y = Estimate, color = Design)) +
  geom_point(position = position_dodge(width = 0.25), size = 3) + # thicker points for estimates
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.25, position = position_dodge(width = 0.25), size = 1.5) +  # thicker error bars
  facet_wrap(~Coefficient, scales = "free_y") +
  theme_bw() +
  labs(x = "", y = "Estimate") +
  scale_color_manual(values = c("Compact" = "darkgreen", "Diffuse" = "darkblue")) +
  scale_x_discrete(labels = c("0", "1")) +  # Set specific labels for the x axis.
  theme(
    axis.text.x = element_text(size = 12),  # Adjust x-axis text size
    axis.text.y = element_text(size = 12),  # Adjust y-axis text size
    axis.title.y = element_text(size = 14),  # Adjust y-axis label size
    strip.text = element_text(size = 14)  # Adjust facet label size
  )

# Save the plot with transparent background
ggsave("plot.png", plot, bg = "transparent")