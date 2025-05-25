library(tidyverse)
library(kernlab)
library(MASS)
library(ggfx)

# Step 1: Generate data
f_true <- function(x) sin(x)
x_train <- seq(-6, 6, length.out = 5)
y_train <- f_true(x_train)
x_grid <- seq(-10, 10, length.out = 400)

# Step 2: GP posterior
rbf_kernel <- rbfdot(sigma = 0.05)
K_xx <- kernelMatrix(rbf_kernel, as.matrix(x_train))
K_xX <- kernelMatrix(rbf_kernel, as.matrix(x_train), as.matrix(x_grid))
K_XX <- kernelMatrix(rbf_kernel, as.matrix(x_grid))
K_xx_inv <- solve(K_xx)
posterior_mean <- t(K_xX) %*% K_xx_inv %*% y_train
posterior_cov <- K_XX - t(K_xX) %*% K_xx_inv %*% K_xX

# Step 3: Sample and reshape
set.seed(42)
samples_post <- t(MASS::mvrnorm(5, mu = as.vector(posterior_mean), Sigma = posterior_cov))
samples_df <- as_tibble(samples_post, .name_repair = "unique") %>%
  mutate(x = x_grid) %>%
  pivot_longer(-x, names_to = "curve", values_to = "y") %>%
  mutate(curve = factor(curve, levels = unique(curve)))

posterior_sd <- sqrt(pmax(diag(posterior_cov), 0))
ribbon_df <- tibble(
  x = x_grid,
  mean = posterior_mean,
  ymin = posterior_mean - 2 * posterior_sd,
  ymax = posterior_mean + 2 * posterior_sd
)

data_df <- tibble(x = x_train, y = y_train)

# Step 4: Set distinct colors for all 5 functions
curve_colors <- c(
  "#e41a1c",  # red
  "#ff7f00",  # orange
  "#4daf4a",  # green
  "#984ea3",  # purple
  "#66c2a5"   # teal
)

# Step 5: Final plot
gp_plot <- ggplot() +
  geom_ribbon(data = ribbon_df, aes(x = x, ymin = ymin, ymax = ymax),
              fill = "#444b5a", alpha = 1) +
  with_outer_glow(
    geom_line(data = ribbon_df, aes(x = x, y = mean),
              color = "white", linewidth = 1.3),
    colour = "white", sigma = 4
  ) +
  geom_line(data = samples_df, aes(x = x, y = y, group = curve, color = curve),
            linewidth = 1.3) +
  geom_point(data = data_df, aes(x = x, y = y),
             color = "white", size = 2.8) +
  geom_vline(xintercept = x_train, color = "white", linetype = "dashed", alpha = 0.2) +
  scale_color_manual(values = curve_colors) +
  coord_cartesian(expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#1e1e1e", color = NA),
    panel.background = element_rect(fill = "#1e1e1e", color = NA),
    plot.margin = margin(0, 0, 0, 0),
    legend.position = "none"
  )

# Step 6: Export
ggsave("gp_blog_final_colored.png", gp_plot,
       width = 4, height = 4, dpi = 600, bg = "#1e1e1e")
