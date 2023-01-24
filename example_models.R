
library(bbsBayes2)
library(patchwork)
library(tidyverse)

setwd("C:/Users/SmithAC/Documents/GitHub/bbsBayes2_testing")
# fitting example models --------------------------------------------------


species <- "Barn Swallow"

stratification <- "latlong"

model = "gamye"

model_variant <- "spatial"


s <- stratify(by = stratification,
              species = species)


p <- prepare_data(s)

ps <- prepare_spatial(p,
                      strata_map = load_map(stratification))


pm <- prepare_model(ps,
                    model = model,
                    model_variant = model_variant)

fit <- run_model(pm,
                 refresh = 200,
                 adapt_delta = 0.8,
                 output_dir = "output",
                 output_basename = paste("Barn_Swallow",stratification,model,model_variant,sep = "_"))

#fit <- readRDS("BBS_STAN_gamye_spatial_2023-01-19.rds")

indices <- generate_indices(fit,
                            regions = c("continent","stratum"))

indices_smooth <- generate_indices(fit,
                            regions = c("continent","stratum"),
                            alternate_n = "n_smooth")

trends = generate_trends(indices_smooth,
                         prob_decrease = c(1,30,50))

trends_slope = generate_trends(indices,
                         prob_decrease = c(1,30,50),
                         prob_increase = c(1,200),
                         slope = TRUE)


trajectories <- plot_indices(indices)
trajectories_smooth <- plot_indices(indices_smooth)
index_plot <- indices_smooth$indices %>% 
  filter(region == "continent")

map1 <- plot_map(trends = trends)
map2 <- plot_map(trends = trends_slope)


print(map1 + map2)
print(trajectories[[1]] + 
        geom_ribbon(data = index_plot,
                    aes(x = year,ymin = index_q_0.05,ymax = index_q_0.95),
                    alpha = 0.2) +
        geom_line(data = index_plot,
                  aes(x = year, y = index),
                  alpha = 0.5))

print(map2)

