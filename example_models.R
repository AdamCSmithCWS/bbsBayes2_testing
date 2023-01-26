
library(bbsBayes2)
library(patchwork)
library(tidyverse)

setwd("C:/Users/SmithAC/Documents/GitHub/bbsBayes2_testing")
# fitting example models --------------------------------------------------

start_year <- NULL

species <- "Yellow-headed Blackbird"
species <- "Barn Swallow"

sp_aou <- bbsBayes2::search_species(species)$aou

stratification <- "latlong"

model = "gamye"

model_variant <- "spatial"

if(stratification == "latlong"){
  nrts <- 1
}else{
  nrts <- 3
}
s <- stratify(by = stratification,
              species = species)


p <- prepare_data(s,
                  min_year = start_year,
                  min_max_route_years = 2,
                  min_n_routes = nrts)

if(model_variant == "spatial"){
  ps <- prepare_spatial(p,
                      strata_map = load_map(stratification))
}else{
  ps <- p
}

pm <- prepare_model(ps,
                    model = model,
                    model_variant = model_variant)

fit <- run_model(pm,
                 refresh = 200,
                 adapt_delta = 0.8,
                 output_dir = "output",
                 output_basename = paste(sp_aou,stratification,model,model_variant,sep = "_"))

#fit <- readRDS(paste0("output/",paste("Barn_Swallow",stratification,model,model_variant,sep = "_"),".rds"))


indices <- generate_indices(fit,
                            regions = c("continent","stratum"))

indices_smooth <- generate_indices(fit,
                            regions = c("continent","stratum"),
                            alternate_n = "n_smooth")

trends = generate_trends(indices_smooth,
                         prob_decrease = c(0,30,50))

trends_slope = generate_trends(indices,
                         prob_decrease = c(0,30,50),
                         prob_increase = c(0,100),
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

