# Code to generate hex sticker for asianamsurvey package

# Install required packages if needed
pkgs <- c("hexSticker", "showtext", "ggplot2", "here")
invisible(lapply(pkgs, function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
}))

# Load required packages
library(hexSticker)
library(showtext)
library(ggplot2)
library(here)

# Add Google fonts
font_add_google("Noto Sans", "noto")
showtext_auto()

# Create heart data using parametric equations
t <- seq(0, 2*pi, by = 0.01)
heart_data <- data.frame(
  x = 0.5 + 0.02 * (16 * sin(t)^3),
  y = 0.8 + 0.02 * (13*cos(t) - 5*cos(2*t) - 2*cos(3*t) - cos(4*t))
)

# Create person-heart icon plot
p <- ggplot() +
  # Heart using parametric equations
  geom_polygon(data = heart_data, 
              aes(x, y), 
              fill = "#4A90E2", 
              color = NA) +
  # Person (stick figure)
  geom_point(aes(0.5, 0.3), color = "#4A90E2", size = 8) +  # head
  geom_segment(aes(x = 0.5, xend = 0.5, y = 0.2, yend = -0.1),  # body
              color = "#4A90E2", linewidth = 2) +
  geom_segment(aes(x = 0.5, xend = 0.7, y = 0.1, yend = 0.2),   # right arm up
              color = "#4A90E2", linewidth = 2) +
  geom_segment(aes(x = 0.5, xend = 0.3, y = 0.1, yend = 0.2),   # left arm up
              color = "#4A90E2", linewidth = 2) +
  coord_fixed(xlim = c(0, 1), ylim = c(-0.2, 1)) +
  theme_void() +
  theme_transparent()

# Get the package root directory and create figures directory
pkg_root <- here::here()
fig_dir <- file.path(pkg_root, "inst", "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# Create the sticker
sticker(
  subplot = p,
  package = "asianamsurvey",
  s_x = 1,    # Center the subplot
  s_y = 0.8,  # Move subplot slightly up
  s_width = 1.4,
  s_height = 1.4,
  p_size = 14,
  p_color = "#2C3E50",
  p_family = "noto",
  h_fill = "#F8FBFF",  # Very light blue background
  h_color = "#4A90E2", # Light blue border
  filename = file.path(fig_dir, "hex_asianamsurvey.png"),
  dpi = 300
)

file.exists(file.path(here::here(), "inst", "figures", "hex_asianamsurvey.png"))