require(mirt)
require(janitor)

# MIRT FUNCTION HELPERS ---------------------------------------------------
fit_mod_intuitive <- function(data, groups){
  # Rasch model with item difficulties free per group, latent means fixed at 0
  # and latent variances free. With no cross-group equality constraints this
  # multiple-group model factorizes exactly into separate single-group fits, so
  # we fit each group with mirt(). (multipleGroup(..., invariance = "free_var")
  # is now rejected as unidentified, and multipleGroup() in mirt 1.46 silently
  # returns NULL with ~675+ items, e.g. the 680-item CDI:WS.)
  grp_levels <- unique(groups)
  mods <- lapply(grp_levels, function(g)
    mirt(data[groups == g, , drop = FALSE], 1, itemtype = "Rasch", SE = TRUE, verbose = FALSE))
  names(mods) <- grp_levels
  class(mods) <- c("dif_by_group", "list")
  mods
}

# coef() for either a mirt MultipleGroupClass or a dif_by_group list
group_coefs <- function(group_model) {
  if (inherits(group_model, "dif_by_group")) lapply(group_model, coef, simplify = TRUE)
  else coef(group_model, simplify = TRUE)
}


mod_intuitive_to_draws_df <- function(mod){
  par_draws <- MASS::mvrnorm(n = 10000, mu = extract.mirt(mod, "parvec"), Sigma = extract.mirt(mod, "vcov"))
  par_draws <- par_draws[ , str_detect(colnames(par_draws), "d")]
  draws_df <- tibble(run = 1:nrow(par_draws))
  stopifnot(ncol(par_draws) %% 2 == 0)
  n_items <- ncol(par_draws) / 2
  for (i in 1:n_items) {
    draws_df[[paste0("item", i)]] <- par_draws[ , i] - par_draws[ , i + n_items]
  }
  draws_df
}

draws_df_to_logit_plot <- function(draws_df){
  draws_df %>%
    select(-run) %>%
    gather(var, val) %>%
    mutate(var = paste0("Item ", add_zero(parse_number(as.character(var))))) %>%
    ggplot(aes(x = val, y = var)) +
    ggridges::geom_density_ridges() +
    labs(x = "", y = "")
}

# CREATE GLIMMER PLOT ----------------------------------------------------------
plot_glimmer <- function(mod_intuitive, item_names, items_to_plot, plotName='', width=8, height=80) {
  draws_df <- mod_intuitive %>% mod_intuitive_to_draws_df()
  names(draws_df) = c("run", item_names)
  
  
  p <- draws_df %>% 
    clean_names() %>%
    gather(var, val, -run) %>%
    filter(is.element(var, items_to_plot)) %>%
    mutate(var = fct_reorder(var, val)) %>%
    ggplot(aes(x = val, y = var)) +
    ggridges::geom_density_ridges() +
    geom_vline(aes(xintercept=0), linetype='dashed', alpha=.5) +
    labs(x = "", y = "") + theme_classic()
  if(plotName!='') ggsave(paste0(plotName,'.pdf'), 
                          width=width, height=height, # .2 * length(item_names)
                          limitsize = F) 
  return(p)
}


extract_group_df <- function(group_model, groups=c("Male","Female")) {
  cf <- group_coefs(group_model)
  Mit = as_tibble(cf[[groups[1]]]$items) %>%
    mutate(definition = rownames(cf[[groups[1]]]$items),
           group1 = groups[1],
           group2 = groups[2]) %>%
    dplyr::select(-any_of(c("g", "u"))) %>%
    #mutate(d = -d) %>% # from easiness to difficulty
    rename(d_g1 = d) 
  Fit = as_tibble(cf[[groups[2]]]$items) %>%
    mutate(definition = rownames(cf[[groups[2]]]$items)) %>%
    dplyr::select(-any_of(c("g", "u", "a1"))) %>%
    #mutate(d = -d) %>% # from easiness to difficulty
    rename(d_g2 = d) 
    
  combo <- Mit %>% left_join(Fit, by = "definition") %>%
    mutate(d_diff = d_g2 - d_g1,
           d_diff_abs = abs(d_diff)) 
  return(combo)
}

get_extreme_item_difficulty_differences <- function(mm) {
  #yfit <- dnorm(mm$d_diff, mean = mean(mm$d_diff), sd = sd(mm$d_diff)) 
  mm <- mm %>% mutate(d_diff = d_g2 - d_g1)
  max_dif = mean(mm$d_diff) + 2*sd(mm$d_diff)
  min_dif = mean(mm$d_diff) - 2*sd(mm$d_diff)
  mm <- mm %>% mutate(extreme = ifelse((d_diff > max_dif) | (d_diff < min_dif), T, F))
  print(paste("mininum difference:",min_dif, "maximum difference:",max_dif))
  return(mm)
}

item_difficulty_difference_histogram <- function(mm, withNormal=F) {
  #yfit <- dnorm(mm$d_diff, mean = mean(mm$d_diff), sd = sd(mm$d_diff)) 
  mm <- mm %>% mutate(d_diff = d_g2 - d_g1)
  max_dif = mean(mm$d_diff) + 2*sd(mm$d_diff)
  min_dif = mean(mm$d_diff) - 2*sd(mm$d_diff)
  mm <- mm %>% mutate(extreme = ifelse((d_diff > max_dif) | (d_diff < min_dif), T, F))
  
  p <- mm %>% ggplot(aes(x=d_diff)) + # , fill=extrem
    geom_histogram(aes(y =..density..), alpha=.7) + theme_classic() +
    geom_vline(aes(xintercept=median(d_diff)), linetype="dashed") +
    geom_vline(aes(xintercept=max_dif), linetype="dashed", color="red") +
    geom_vline(aes(xintercept=min_dif), linetype="dashed", color="red") +
    xlab(paste(mm$group2, "-", mm$group1, "Item Difficulty"))
  
  if(withNormal) {
    p <- p + stat_function(fun = dnorm, alpha=.6, 
                           args = list(mean = mean(mm$d_diff), sd = sd(mm$d_diff)))
  }
  return(p)
}
