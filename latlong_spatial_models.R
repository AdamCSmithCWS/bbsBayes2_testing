
library(bbsBayes2)
library(patchwork)
library(tidyverse)

setwd("C:/Users/SmithAC/Documents/GitHub/bbsBayes2_testing")
# fitting example models --------------------------------------------------

# alt_gamye <- bbsBayes2::copy_model_file("gamye",
#                            model_variant = "spatial",
#                            dir = "alt_models")
# alt_first_diff <- bbsBayes2::copy_model_file("first_diff",
#                            model_variant = "spatial",
#                            dir = "alt_models")

start_year <- NULL

species_list <- c("Brown-headed Nuthatch","Cactus Wren")
model_list <- c("gamye","first_diff")



sp_aou <- bbsBayes2::search_species(species)$aou

stratification <- "latlong"


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
                  #min_max_route_years = 2,
                  min_n_routes = nrts)

if(model_variant == "spatial"){
  ps <- prepare_spatial(p,
                      strata_map = load_map(stratification))
  print(ps$spatial_data$map)
  
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


# Alternate Priors --------------------------------------------------------

alt_prior_model <- paste0("alt_models/",model,"_",model_variant,"_bbs_CV_COPY.stan")


pm2 <- prepare_model(ps,
                    model = model,
                    model_variant = model_variant,
                    model_file = alt_prior_model)



fit2 <- run_model(pm2,
                 refresh = 200,
                 #adapt_delta = 0.8,
                 output_dir = "output",
                 output_basename = paste(sp_aou,stratification,"alt",model,model_variant,sep = "_"))




# Alternate spatial gamye --------------------------------------------------------

alt_prior_model2 <- paste0("alt_models/",model,"_double_",model_variant,"_bbs_CV_COPY.stan")


pm3 <- prepare_model(ps,
                     model = model,
                     model_variant = model_variant,
                     model_file = alt_prior_model2)

pm3[["init_values"]][["sdyear"]] <- runif(1,0.02,1)
pm3[["init_values"]][["sdYEAR"]] <- runif(1,0.02,1)
pm3[["init_values"]][["YEAREFFECT_raw"]] <- rnorm(pm3[["model_data"]][["n_years"]],0,0.3)

fit3 <- run_model(pm3,
                  refresh = 200,
                  #adapt_delta = 0.8,
                  output_dir = "output",
                  output_basename = paste(sp_aou,stratification,"alt",model,"double",model_variant,sep = "_"))




# compare convergence -----------------------------------------------------

summ <- get_summary(fit)
summ2 <- get_summary(fit2)


sd1 <- summ %>% filter(grepl("sd",variable)) %>% 
  mutate(model = "standard") %>% 
  arrange(ess_bulk)
sd1

sd2 <- summ2 %>% filter(grepl("sd",variable)) %>% 
  mutate(model = "alternate") %>% 
  arrange(ess_bulk)
sd2


summ %>% filter(variable == "phi")
summ2 %>% filter(variable == "phi")


sd1 <- summ %>% filter(grepl("BETA_raw",variable)) %>% 
  mutate(model = "standard") 
sd1

sd2 <- summ2 %>% filter(grepl("BETA_raw",variable)) %>% 
  mutate(model = "alternate") 
sd2



sd1 <- summ %>% filter(grepl("beta_raw",variable)) %>% 
  mutate(model = "standard") %>% 
  arrange(-rhat)
sd1

sd2 <- summ2 %>% filter(grepl("beta_raw",variable)) %>% 
  mutate(model = "alternate") %>% 
  arrange(-rhat)
sd2

sd1 <- summ %>% filter(grepl("n[",variable,fixed = TRUE)) %>% 
  mutate(model = "standard") %>% 
  arrange(ess_bulk)
sd1

sd2 <- summ2 %>% filter(grepl("n[",variable,fixed = TRUE)) %>% 
  mutate(model = "alternate") %>% 
  arrange(ess_bulk)
sd2

sd1 <- summ %>% filter(grepl("n_smooth[",variable,fixed = TRUE)) %>% 
  mutate(model = "standard") %>% 
  arrange(ess_bulk)
sd1

sd2 <- summ2 %>% filter(grepl("n_smooth[",variable,fixed = TRUE)) %>% 
  mutate(model = "alternate") %>% 
  arrange(ess_bulk)
sd2


sd1 <- summ %>% filter(grepl("sdyear",variable)) %>% 
  mutate(model = "standard") 
sd1

sd2 <- summ2 %>% filter(grepl("sdyear",variable)) %>% 
  mutate(model = "alternate") 
sd2

sd1 <- summ %>% filter(grepl("yeareffect[",variable,fixed = TRUE)) %>% 
  mutate(model = "standard") %>% 
  arrange(ess_bulk)
sd1

sd2 <- summ2 %>% filter(grepl("yeareffect[",variable,fixed = TRUE)) %>% 
  mutate(model = "alternate") %>% 
  arrange(ess_bulk)
sd2


indices <- generate_indices(fit,
                            regions = c("continent","stratum"))

indices2 <- generate_indices(fit2,
                            regions = c("continent","stratum"))

trajectories <- plot_indices(indices)

trajectories2 <- plot_indices(indices2)

indices_smooth <- generate_indices(fit,
                                   regions = c("continent","stratum"),
                                   alternate_n = "n_smooth")

trajectories_smooth <- plot_indices(indices_smooth)


#fit <- readRDS(paste0("output/",paste("Barn_Swallow",stratification,model,model_variant,sep = "_"),".rds"))


indices_smooth <- generate_indices(fit,
                            regions = c("continent","stratum"),
                            alternate_n = "n_smooth")

trends = generate_trends(indices_smooth,
                         prob_decrease = c(0,30,50))

indices <- generate_indices(fit,
                            regions = c("continent","stratum"))

trends_slope = generate_trends(indices,
                         prob_decrease = c(0,30,50),
                         prob_increase = c(0,100),
                         slope = TRUE)


trajectories <- plot_indices(indices)
#trajectories_smooth <- plot_indices(indices_smooth)
index_plot <- indices_smooth$indices %>%
  filter(region == "continent")

#map1 <- plot_map(trends = trends)
 map2 <- plot_map(trends = trends_slope)


#print(map1 + map2)

print(trajectories[[1]])# + 
        # geom_ribbon(data = index_plot,
        #             aes(x = year,ymin = index_q_0.05,ymax = index_q_0.95),
        #             alpha = 0.2) +
        # geom_line(data = index_plot,
        #           aes(x = year, y = index),
        #           alpha = 0.5))

print(map2)



# convergence etc. --------------------------------------------------------

parameter_summary <- get_summary(fit)

