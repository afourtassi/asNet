
mod <- lm(aoa_AS_2 ~ poly(aoa_TD, 2), 
          data = all_aoa)
hist(resid(mod))

ggplot(all_aoa, 
       aes(x = aoa_TD, y = aoa_AS_2, col = lexical_class)) + 
  geom_point(alpha = .5) + 
  ggthemes::theme_few() +
  ggthemes::scale_color_ptol(name = "Lexical Class") +
  xlim(0,36) + 
  ylim(0,90) + 
  geom_abline(slope = 1, lty = 2) +
  # geom_abline(slope = 2, lty = 2) +
  geom_smooth(method = "lm", se = FALSE, 
              formula = y ~ poly(x,2)) + 
  ggrepel::geom_label_repel(data = 
                              filter(all_aoa,
                                     abs(resid(mod)) > 10), 
                            aes(label = definition)) + 
  xlab("TD Age of Acquisition (mo)") +
  ylab("ASD Age of Acquisition (mo)") + 
  theme(legend.position = "bottom")
  
  
  
  ggplot(all_aoa, 
         aes(x = aoa_TD, y = aoa_AS_2, col = lexical_class)) + 
    geom_point(alpha = .5) + 
    ggthemes::theme_few() +
    ggthemes::scale_color_ptol(name = "Lexical Class") +
    xlim(0,18) + 
    ylim(0,36) + 
    geom_abline(slope = 1, lty = 2) +
    # geom_abline(slope = 2, lty = 2) +
    geom_smooth(method = "lm", se = FALSE, 
                formula = y ~ poly(x,2)) + 
    ggrepel::geom_label_repel(data = 
                                filter(all_aoa,
                                       abs(resid(mod)) > 10), 
                                       aes(label = definition)) + 
                                xlab("TD Age of Acquisition (mo)") +
                                ylab("ASD Age of Acquisition (mo)") + 
                                theme(legend.position = "bottom")
  