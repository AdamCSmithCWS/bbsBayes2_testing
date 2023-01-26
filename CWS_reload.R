

# loading the fitted 2021 CWS analyses and inserting them into a bbsBayes2 fit-object.



library(bbsBayes2)
library(tidyverse)





start_year <- NULL

species <- "Barn Swallow"

stratification <- "bbs_cws"

model = "gamye"

model_variant <- "hier"


s <- stratify(by = stratification,
              species = species)


p <- prepare_data(s,
                  min_year = start_year,
                  min_max_route_years = 2)

if(model_variant == "spatial"){
  ps <- prepare_spatial(p,
                        strata_map = load_map(stratification))
}else{
  ps <- p
}

# pm <- prepare_model(ps,
#                     model = model,
#                     model_variant = model_variant)


fit <- readRDS(paste0("output/",paste("Barn_Swallow",stratification,model,model_variant,sep = "_"),".rds"))

load("output/Barn_Swallow_gamye_BBS_Stan_fit.rdata")


fit_new <- list(model_fit = stanfit,
                model_data = ps[["model_data"]],
                meta_data = ps[["meta_data"]],
                meta_strata = ps[["meta_strata"]],
                raw_data = ps[["raw_data"]])


indices <- generate_indices(fit_new)

indices_smooth <- generate_indices(fit_new,
                                   alternate_n = "nsmooth")

trends <- generate_trends(indices_smooth,
                          prob_decrease = c(0,30,50),
                          min_year = 1970)

map <- plot_map(trends)
print(map)




