source("./trend1/trend_function.R")

### figures
w_com$highlight <- "Sites"
f0a <- w_com %>% 
  ggplot() +
  geom_sf(data = rgn, aes(fill = region), color = "gray0") + # US border
  
  # First fill scale for region
  scale_fill_manual(
    values = setNames(scales::hue_pal(l = 70, c = 40)(6), 
                      c("West","Mtwest","Midwest","Texas","South","Northeast")),
    name = ""
  ) +
  
  # Introduce a new fill scale
  ggnewscale::new_scale_fill() +
  
  # Overlay s_com with highlight fill
  geom_sf(data = w_com, aes(fill = highlight), color = "gray10", size = 0.7, stroke = 0.2) +
  
  # Second fill scale for highlight
  scale_fill_manual(
    values = c("Sites" = "gray10"),
    name = ""
  ) +
  
  
  # geom_sf(aes(fill = highlight), color = NA, size = 0.3) +
  
  facet_wrap(~ class, nrow = 3) +
  
  theme_minimal() +
  
  labs(title = "", fill = "") +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), 
           ylim = c(-2300000,730000), expand = FALSE, datum = NA) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(color = 'black', size=12),
        strip.text = element_text(color = 'black',family="Franklin Gothic Book",size=12, face = "bold"),
        legend.position = "right",
        legend.key.size = unit(1, 'cm'), 
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=12),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
        plot.title=element_text(family="Franklin Gothic Demi", size=20, hjust = 0.5)) +
  guides(fill = guide_legend(reverse = T))


# count
f0b1 <- wind_que %>% 
  mutate(region = case_when(state %in% northeast ~ "Northeast",
                            state %in% midwest ~ "Midwest",
                            state %in% west ~ "West",
                            state %in% south ~ "South",
                            state %in% mtwest ~ "Mtwest",
                            T ~ "Texas")) %>% 
  group_by(region) %>% 
  summarise(Count = n()) %>% 
  ungroup() %>% 
  mutate(class = "Queue") %>% 
  rbind(
    w %>% 
      st_drop_geometry() %>% 
      mutate(region = case_when(state %in% northeast ~ "Northeast",
                                state %in% midwest ~ "Midwest",
                                state %in% west ~ "West",
                                state %in% south ~ "South",
                                state %in% mtwest ~ "Mtwest",
                                T ~ "Texas")) %>% 
      
      group_by(region) %>% 
      summarise(Count = n()) %>% 
      ungroup() %>% 
      mutate(class = "Project")
  ) %>% 
  pivot_wider(names_from = class, values_from = Count) %>% 
  mutate(Difference = Queue - Project) %>% 
  gather(class, Count, Queue:Difference) %>% 
  
  mutate(region = factor(region, levels = c("West","Mtwest","Midwest","Texas","South","Northeast")),
         region_fill = factor(region, levels = rev(c("West","Mtwest","Midwest","Texas","South","Northeast")))) %>% 
  mutate(class = factor(class, levels = c("Project","Queue","Difference"))) %>% 
  
  ggplot() +
  geom_col(aes(x = Count, y = region_fill, fill = region)) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  
  scale_fill_manual(
    values = setNames(scales::hue_pal(l = 70, c = 40)(6), 
                      c("West","Mtwest","Midwest","Texas","South","Northeast")),
    name = ""
  ) +
  
  labs(x = "Count", y = "",fill = "") +
  facet_wrap(~ class, nrow = 3) +
  theme_classic() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12, face = "bold"),
        legend.position = "none",
        # axis.text.x = element_blank(),
        # axis.ticks.x = element_blank(),
        axis.text = element_text(color = "black",family="Franklin Gothic Book",size=12),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
        plot.title=element_text(family="Franklin Gothic Demi", size=20)) +
  guides(fill = guide_legend(byrow=T, nrow = 1))

# f0b <- wind_inter %>% 
#   dplyr::rename(state = STATE) %>% 
#   st_drop_geometry() %>% 
#   mutate(region = case_when(state %in% northeast ~ "Northeast",
#                             state %in% midwest ~ "Midwest",
#                             state %in% west ~ "West",
#                             state %in% south ~ "South",
#                             state %in% mtwest ~ "Mtwest",
#                             T ~ "Texas")) %>% 
#   group_by(region) %>% 
#   summarise(Count = sum(count, na.rm = T)) %>% 
#   ungroup() %>% 
#   mutate(class = "Queue") %>% 
#   rbind(
#     w %>% 
#       st_drop_geometry() %>% 
#       mutate(region = case_when(state %in% northeast ~ "Northeast",
#                                 state %in% midwest ~ "Midwest",
#                                 state %in% west ~ "West",
#                                 state %in% south ~ "South",
#                                 state %in% mtwest ~ "Mtwest",
#                                 T ~ "Texas")) %>% 
#       
#       group_by(region) %>% 
#       summarise(Count = n()) %>% 
#       ungroup() %>% 
#       mutate(class = "Project")
#   ) %>% 
#   pivot_wider(names_from = class, values_from = Count) %>% 
#   mutate(Difference = Queue - Project) %>% 
#   gather(class, Count, Queue:Difference) %>% 
#   
#   mutate(region = factor(region, levels = c("West","Mtwest","Midwest","Texas","South","Northeast")),
#          region_fill = factor(region, levels = rev(c("West","Mtwest","Midwest","Texas","South","Northeast")))) %>% 
#   mutate(class = factor(class, levels = c("Project","Queue","Difference"))) %>% 
#   
#   ggplot() +
#   geom_col(aes(x = Count, y = region_fill, fill = region)) +
#   geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
#   
#   scale_fill_manual(
#     values = setNames(scales::hue_pal(l = 70, c = 40)(6), 
#                       c("West","Mtwest","Midwest","Texas","South","Northeast")),
#     name = ""
#   ) +
#   
#   labs(x = "Count", y = "",fill = "") +
#   facet_wrap(~ class, nrow = 3) +
#   theme_classic() +
#   theme(panel.grid.minor = element_blank(),
#         panel.grid.major.x = element_blank(),
#         strip.background =element_rect(fill="gray22",color="gray22"),
#         strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12, face = "bold"),
#         legend.position = "none",
#         # axis.text.x = element_blank(),
#         # axis.ticks.x = element_blank(),
#         axis.text = element_text(color = "black",family="Franklin Gothic Book",size=12),
#         axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
#         plot.title=element_text(family="Franklin Gothic Demi", size=20)) +
#   guides(fill = guide_legend(byrow=T, nrow = 1))

# f0b <- s_com %>% 
#   filter(class != "Substation") %>% 
#   st_drop_geometry() %>% 
#   mutate(region = case_when(state %in% northeast ~ "Northeast",
#                             state %in% midwest ~ "Midwest",
#                             state %in% west ~ "West",
#                             state %in% south ~ "South",
#                             state %in% mtwest ~ "Mtwest",
#                             T ~ "Texas")) %>% 
#   
#   group_by(region, class) %>% 
#   summarise(Count = n()) %>% 
#   ungroup() %>% 
#   pivot_wider(names_from = class, values_from = Count) %>% 
#   mutate(Difference = Queue - Project) %>% 
#   gather(class, Count, Queue:Difference) %>% 
# 
#   mutate(region = factor(region, levels = c("West","Mtwest","Texas","Midwest","South","Northeast")),
#          region_fill = factor(region, levels = rev(c("West","Mtwest","Texas","Midwest","South","Northeast")))) %>% 
#   mutate(class = factor(class, levels = c("Queue","Project","Difference"))) %>% 
# 
#   ggplot() +
#   geom_col(aes(x = region, y = Count, fill = region)) +
#   labs(x = "Count", y = "",fill = "") +
#   facet_wrap(~ class, nrow = 1) +
#   theme_classic() +
#   theme(panel.grid.minor = element_blank(),
#         panel.grid.major.x = element_blank(),
#         strip.background =element_rect(fill="gray22",color="gray22"),
#         strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12, face = "bold"),
#         legend.position = "bottom",
#         axis.text.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=12),
#         axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
#         plot.title=element_text(family="Franklin Gothic Demi", size=20)) +
#   guides(fill = guide_legend(byrow=T, nrow = 1))


# capacity/ count
f0c1 <- wind_que %>% 
  mutate(region = case_when(state %in% northeast ~ "Northeast",
                            state %in% midwest ~ "Midwest",
                            state %in% west ~ "West",
                            state %in% south ~ "South",
                            state %in% mtwest ~ "Mtwest",
                            T ~ "Texas")) %>% 
  group_by(region) %>% 
  summarise(Capacity = sum(capacity_mw, na.rm = T)/n()) %>% 
  ungroup() %>% 
  mutate(class = "Queue") %>% 
  rbind(
    w %>% 
      st_drop_geometry() %>% 
      mutate(region = case_when(state %in% northeast ~ "Northeast",
                                state %in% midwest ~ "Midwest",
                                state %in% west ~ "West",
                                state %in% south ~ "South",
                                state %in% mtwest ~ "Mtwest",
                                T ~ "Texas")) %>% 
      
      group_by(region) %>% 
      summarise(Capacity = sum(capacity/1000, na.rm = T)/n()) %>% 
      ungroup() %>% 
      mutate(class = "Project")
  ) %>% 
  pivot_wider(names_from = class, values_from = Capacity) %>% 
  mutate(Difference = Queue - Project) %>% 
  gather(class, Capacity, Queue:Difference) %>% 
  
  mutate(region = factor(region, levels = c("West","Mtwest","Midwest","Texas","South","Northeast")),
         region_fill = factor(region, levels = rev(c("West","Mtwest","Midwest","Texas","South","Northeast")))) %>% 
  mutate(class = factor(class, levels = c("Project","Queue","Difference"))) %>% 
  
  ggplot() +
  geom_col(aes(x = Capacity, y = region_fill, fill = region)) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  
  scale_fill_manual(
    values = setNames(scales::hue_pal(l = 70, c = 40)(6), 
                      c("West","Mtwest","Midwest","Texas","South","Northeast")),
    name = ""
  ) +
  
  labs(x = "Capacity (MW/project)", y = "",fill = "") +
  facet_wrap(~ class, nrow = 3) +
  theme_classic() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12, face = "bold"),
        legend.position = "none",
        # axis.text.x = element_blank(),
        # axis.ticks.x = element_blank(),
        axis.text = element_text(color = "black",family="Franklin Gothic Book",size=12),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
        plot.title=element_text(family="Franklin Gothic Demi", size=20)) +
  guides(fill = guide_legend(byrow=T, nrow = 1))


# # capacity
# f0c2 <- wind_que %>% 
#   mutate(region = case_when(state %in% northeast ~ "Northeast",
#                             state %in% midwest ~ "Midwest",
#                             state %in% west ~ "West",
#                             state %in% south ~ "South",
#                             state %in% mtwest ~ "Mtwest",
#                             T ~ "Texas")) %>% 
#   group_by(region) %>% 
#   summarise(Capacity = sum(capacity_mw, na.rm = T)) %>% 
#   ungroup() %>% 
#   mutate(class = "Queue") %>% 
#   rbind(
#     w %>% 
#       st_drop_geometry() %>% 
#       mutate(region = case_when(state %in% northeast ~ "Northeast",
#                                 state %in% midwest ~ "Midwest",
#                                 state %in% west ~ "West",
#                                 state %in% south ~ "South",
#                                 state %in% mtwest ~ "Mtwest",
#                                 T ~ "Texas")) %>% 
#       
#       group_by(region) %>% 
#       summarise(Capacity = sum(capacity/1000, na.rm = T)) %>% 
#       ungroup() %>% 
#       mutate(class = "Project")
#   ) %>% 
#   pivot_wider(names_from = class, values_from = Capacity) %>% 
#   mutate(Difference = Queue - Project) %>% 
#   gather(class, Capacity, Queue:Difference) %>% 
#   
#   mutate(region = factor(region, levels = c("West","Mtwest","Midwest","Texas","South","Northeast")),
#          region_fill = factor(region, levels = rev(c("West","Mtwest","Midwest","Texas","South","Northeast")))) %>% 
#   mutate(class = factor(class, levels = c("Project","Queue","Difference"))) %>% 
#   
#   ggplot() +
#   geom_col(aes(x = Capacity, y = region_fill, fill = region)) +
#   geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
#   
#   scale_fill_manual(
#     values = setNames(scales::hue_pal(l = 70, c = 40)(6), 
#                       c("West","Mtwest","Midwest","Texas","South","Northeast")),
#     name = ""
#   ) +
#   
#   labs(x = "Capacity (MW)", y = "",fill = "") +
#   facet_wrap(~ class, nrow = 3) +
#   theme_classic() +
#   theme(panel.grid.minor = element_blank(),
#         panel.grid.major.x = element_blank(),
#         strip.background =element_rect(fill="gray22",color="gray22"),
#         strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12, face = "bold"),
#         legend.position = "none",
#         # axis.text.x = element_blank(),
#         # axis.ticks.x = element_blank(),
#         axis.text = element_text(color = "black",family="Franklin Gothic Book",size=12),
#         axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
#         plot.title=element_text(family="Franklin Gothic Demi", size=20)) +
#   guides(fill = guide_legend(byrow=T, nrow = 1))
# 
# 
# f0c <- wind_inter %>% 
#   dplyr::rename(state = STATE) %>% 
#   st_drop_geometry() %>% 
#   mutate(region = case_when(state %in% northeast ~ "Northeast",
#                             state %in% midwest ~ "Midwest",
#                             state %in% west ~ "West",
#                             state %in% south ~ "South",
#                             state %in% mtwest ~ "Mtwest",
#                             T ~ "Texas")) %>% 
#   group_by(region) %>% 
#   summarise(Capacity = sum(capacity_mw, na.rm = T)/sum(count, na.rm = T)) %>% 
#   ungroup() %>% 
#   mutate(class = "Queue") %>% 
#   rbind(
#     w %>% 
#       st_drop_geometry() %>% 
#       mutate(region = case_when(state %in% northeast ~ "Northeast",
#                                 state %in% midwest ~ "Midwest",
#                                 state %in% west ~ "West",
#                                 state %in% south ~ "South",
#                                 state %in% mtwest ~ "Mtwest",
#                                 T ~ "Texas")) %>% 
#       
#       group_by(region) %>% 
#       summarise(Capacity = sum(capacity, na.rm = T)/n()) %>% 
#       ungroup() %>% 
#       mutate(class = "Project")
#   ) %>% 
#   pivot_wider(names_from = class, values_from = Capacity) %>% 
#   mutate(Difference = Queue - Project) %>% 
#   gather(class, Capacity, Queue:Difference) %>% 
#   
#   mutate(region = factor(region, levels = c("West","Mtwest","Midwest","Texas","South","Northeast")),
#          region_fill = factor(region, levels = rev(c("West","Mtwest","Midwest","Texas","South","Northeast")))) %>% 
#   mutate(class = factor(class, levels = c("Project","Queue","Difference"))) %>% 
#   
#   ggplot() +
#   geom_col(aes(x = Capacity, y = region_fill, fill = region)) +
#   geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
#   
#   scale_fill_manual(
#     values = setNames(scales::hue_pal(l = 70, c = 40)(6), 
#                       c("West","Mtwest","Midwest","Texas","South","Northeast")),
#     name = ""
#   ) +
#   
#   labs(x = "Capacity (MW/count)", y = "",fill = "") +
#   facet_wrap(~ class, nrow = 3) +
#   theme_classic() +
#   theme(panel.grid.minor = element_blank(),
#         panel.grid.major.x = element_blank(),
#         strip.background =element_rect(fill="gray22",color="gray22"),
#         strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12, face = "bold"),
#         legend.position = "none",
#         # axis.text.x = element_blank(),
#         # axis.ticks.x = element_blank(),
#         axis.text = element_text(color = "black",family="Franklin Gothic Book",size=12),
#         axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
#         plot.title=element_text(family="Franklin Gothic Demi", size=20)) +
#   guides(fill = guide_legend(byrow=T, nrow = 1))
    

f0 <- ggarrange(
  f0a,
  ggarrange(f0b1,f0c1, nrow = 1),
  nrow = 1, widths = c(2,1.8),
  labels = c("A", "B"),  # Adds labels to plots
  label.x = 0,        # Adjust horizontal position of labels
  label.y = 1,        # Adjust vertical position of labels
  # vjust = 1,
  # hjust = -1,
  font.label = list(size = 14, face = "bold")
)

ggsave("./trend1/fig/f0.png", f0, width = 12, height = 8)



f1 <- w_dat_compare %>% 
  rst("Project") %>% 
  rbind(
    w_dat_compare %>% 
      rst("Substation"),
    w_dat_compare %>% 
      rst("Queue")
  ) %>% 
  mutate(class = factor(class, levels = c("Project","Substation","Queue"))) %>% 
  mutate(vari = rep(v_name, 3),
         vari = fct_reorder(vari, exp(pe))) %>% 
  filter(!var == "(Intercept)") %>%
  mutate(domain = case_when(str_detect(var, "tx|roads|landAcq|cf|slope") ~ "Technical",
                            str_detect(var, "env|hail|fire") ~ "Environmental risk",
                            str_detect(var, "lowincome|minority|unemploy|pop") ~ "Social",
                            str_detect(var, "lag|community") ~ "Spatial/policy",
                            str_detect(var, "lulc") ~ "Land use",
                            str_detect(var, "region") ~ "Regional"),
         domain = factor(domain, levels = c("Technical","Environmental risk","Spatial/policy",
                                            "Social","Land use", "Regional"))) %>% 
  
  dplyr::rename(sig = color) %>% 
  ggplot(aes(x = exp(pe), y = vari, color = class)) +
  geom_vline(xintercept = 1,linetype = "dashed", size = 0.5, color = "gray30") +
  geom_errorbar(aes(xmin=exp(pe-1.96*se), xmax=exp(pe+1.96*se), color = class), width = 0.3, size = 0.7,
                position = position_dodge(width = 0.4)) +
  geom_point(aes(fill = class),size = 2,pch=21,
             position = position_dodge(width = 0.4)) +
  theme_bw() +
  facet_wrap(~ domain, nrow = 2, scales = "free") +
  scale_color_manual(values = c("tomato4","salmon2","steelblue1")) +
  scale_fill_manual(values = c("tomato4","salmon2","steelblue1")) +
  
  scale_x_continuous(
    trans = scales::pseudo_log_trans(base = 10, sigma = 0.2),  # Pseudo-log transformation
    # breaks = c(0, 0.3, 0.6, 1, 1.3, 2),                     # Custom breaks
    # labels = scales::label_number(accuracy = 0.1)              # Format labels
  )  +
  
  # scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
  #               labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  
  labs(fill = "", y = "", x = "Odds ratio", title = "", color = "") +
  # scale_fill_manual(values=c("red", "gray")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12, face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=10),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=12),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
        plot.title=element_text(family="Franklin Gothic Demi", size=16, hjust = -0.09, face = "bold"))

ggsave("./trend1/fig/f1.png", f1, width = 12, height = 8)



w_plot <- r_plot(w_d) 

w_map <- rgn %>% 
  mutate(region = recode(region, 
                         "mtwest" = "Mtwest",
                         "midwest" = "Midwest",
                         "northeast" = "Northeast",
                         "south" = "South",
                         "texas" = "Texas",
                         "west" = "West")) %>% 
  left_join(w_d, by = "region") %>% 
  mutate(class = factor(class, levels = c("Project","Queue")))
var <- c("Capacity factor", "Hail", "Wildfire", "Energy community")

w_m <- mping(w_map, var, "")


f2 <- ggarrange(w_m, w_plot, widths = c(3,1.7), nrow = 1,
                labels = c("A", "B"),  # Adds labels to plots
                label.x = 0,        # Adjust horizontal position of labels
                label.y = 1,        # Adjust vertical position of labels
                # vjust = 1,
                # hjust = -1,
                font.label = list(size = 14, face = "bold"))

ggsave("./trend1/fig/f2.png", f2, width = 12, height = 10)



f3a <- prediction_plot(new_results[[1]], w_pts, "Project")
f3b <- prediction_plot(new_results[[3]], inter_pts, "Queue")


### probability change trend
f3c <- ggplot() +
  geom_sf(data = polygons_vect, fill = "gray50", color = NA) +
  geom_spatraster(data = df_results, aes(fill = pred_logReg_w_queue)) +
  geom_sf(data = polygons_vect, fill = NA, color = "black", linewidth = 0.5) +
  # Use the same scales for consistency, but legend is hidden here.
  scale_fill_distiller(palette = "RdBu", name = "Probability \ndifference ",
                       na.value = NA,
                       limits = c(-0.7, 0.7)) +      # Emphasizes extremes, compresses center
  
  # scale_fill_gradient2(
  #   low = "blue",
  #   mid = "yellow",
  #   high = "red",
  #   midpoint = 0,
  #   name = "Probability \ndifference",
  #   na.value = NA,
  #   limits = c(-0.5, 0.5)  # Adjust based on your data range
  # ) +

  # scale_color_manual(
  #   name = "",
  #   values = c("Sites" = "red")
  # ) +
  labs(title = "Probability change") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom" # Hide the legend on individual plots
  )



f3d <- rst_plot(f3_glm) +
  labs(y = "Probability difference")


f3 <- ggarrange(
  ggarrange(f3a,f3b, nrow = 1, common.legend = T, legend = "bottom"),
  ggarrange(f3c,f3d, widths = c(1,1.2), nrow = 1),
  heights = c(1,1.2),
  nrow = 2,
  labels = c("A", "B"),  # Adds labels to plots
  label.x = 0,        # Adjust horizontal position of labels
  label.y = 1,        # Adjust vertical position of labels
  # vjust = 1,
  # hjust = -1,
  font.label = list(size = 14, face = "bold"))

ggsave("./trend1/fig/f3.png", f3, width = 12, height = 10)


### capacity 
f4a <- wind_queue %>%
  na.omit() %>% 
  mutate(capacity_bin = cut(capacity_mw,
                            breaks = c(1, 50, 200, 500, 1000, Inf),
                            labels = c("1–50", "51–200", "201–500", "501–1000", "1000+"),
                            right = FALSE)) %>%
  ggplot() +
  geom_sf(data = rgn, color = NA, fill = "gray50") + # US border
  geom_sf(aes(color = capacity_bin, size = capacity_bin), alpha = 0.7) +
  geom_sf(data = rgn, color = "black", fill = NA, linewidth = 0.7) + # US border
  scale_color_brewer(palette = "RdYlBu", direction = -1, name = "Capacity (MW)") +
  scale_size_manual(
    values = c(0.2, 1, 3, 4, 5)
  ) +

  theme_minimal() +
  labs(title = "Queue projects", fill = "", size = "Capacity (MW)") +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), 
           ylim = c(-2300000,730000), expand = FALSE, datum = NA) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.text = element_text(color = 'black', size=12),
        strip.text = element_text(color = 'black',family="Franklin Gothic Book",size=12, face = "bold"),
        legend.position = "bottom",
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.key.size = unit(1, 'cm'), 
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=12),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
        plot.title=element_text(hjust = 0.5)) +
  guides(color = guide_legend(nrow = 2, byrow = T))





f4b <- rst_plot(f4_glm1) +
  scale_y_continuous(
    labels = scales::label_number(accuracy = 1)             
  ) 



tp <- sqrt(attr(ranef(f4_glm2, condVar=T)[[1]], "postVar"))*1.96
f4c <- as.data.frame(ranef(f4_glm2)$status) %>% 
  tibble::rownames_to_column("status") %>% 
  dplyr::select(-`(Intercept)`) %>% 
  
  gather(variable, R_effect, -status) %>% 
  
  mutate(vari = rep(v_name[c(-1,-6,-10:-24)],each = 2),
         vari = fct_reorder(vari, R_effect)) %>% 
  
  mutate(SE = c(tp[2,2,],tp[3,3,],tp[4,4,],tp[5,5,],tp[6,6,],
                tp[7,7,],tp[8,8,],tp[9,9,],tp[10,10,],
                tp[11,11,])) %>% 
  
  # mutate(variable = recode(variable, "cf" = "Capacity factor",
  #                          "hail" = "Hail",
  #                          "fire" = "Fire",
  #                          "community" = "Energy community",
  #                          "lag" = "Spatial effects")) %>% 
  # mutate(variable = factor(variable, levels = c("Capacity factor", "Hail", "Fire", "Energy community", "Spatial effects"))) %>% 
  mutate(status = factor(status, levels = c("Late","Early"))) %>% 
  mutate(upper = R_effect+SE,
         lower = R_effect-SE) %>% 
  
  mutate(domain = case_when(str_detect(variable, "tx|roads|landAcq|cf|slope") ~ "Technical",
                            str_detect(variable, "env|hail|fire") ~ "Environmental risk",
                            str_detect(variable, "rps|lag|community") ~ "Spatial/policy"),
         domain = factor(domain, levels = c("Technical","Environmental risk","Spatial/policy"))) %>% 

  
  ggplot(aes(y = R_effect, x = vari, 
             ymin=lower, ymax=upper, color = status)) +
  geom_hline(yintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  
  geom_errorbar(width = 0.3, size = 0.8,
                position = position_dodge(width = 0.5)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  
  # geom_pointrangeh(position = position_dodge2v(height = 0.4), fatten = 2, size = 0.7) +
  
  facet_wrap(~domain, scales = "free") +
  theme_bw() +
  
  
  labs(y = "Capacity (MW)", x ="", color = "Phase",
       title = "") +
  
  scale_color_manual(values=c("brown", "darkblue")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12, face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size=12),
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=12,
                                   angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=12),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
        plot.title=element_text(family="Franklin Gothic Demi", size=20)) 


f4 <- ggarrange(
  ggarrange(f4a, f4b, widths = c(1,1.2), nrow = 1),
  f4c,
  nrow = 2,
  heights = c(1,0.9),
  labels = c("A", "B"),  # Adds labels to plots
  label.x = 0,        # Adjust horizontal position of labels
  label.y = 1,        # Adjust vertical position of labels
  # vjust = 1,
  # hjust = -1,
  font.label = list(size = 14, face = "bold")
  
)

ggsave("./trend1/fig/f4.png", f4, width = 12, height = 12)



### regression results
# Extract tidy fixed effects summary
fixed_df <- tidy(fit, effects = "fixed", conf.int = TRUE)

# Define name mapping
name_map <- c(
  "TYPEHUQ" = "Multifamily",
  "SQFTEST" = "Larger home",
  "YEARMADERANGE" = "Newer home",
  "TOTROOMS" = "More rooms",
  "NHSLDMEM" = "More people",
  "FUELH2O1" = "Electricity (Water Heating)",
  "FUELH2O5" = "Fuel (Water Heating)",
  "ACEQUIPM1" = "Central AC",
  "ACEQUIPM3" = "Room AC",
  "ACEQUIPM4" = "Portable AC",
  "ACEQUIPM6" = "Other AC",
  "MONEYPY" = "Income",
  "EDUCATION" = "Education",
  "HOUSEHOLDER_RACE2" = "Black",
  "HOUSEHOLDER_RACE3" = "Asian",
  "HOUSEHOLDER_RACE4" = "Other Race"
)

# Apply the name mapping to the tidy dataframe
fixed_df <- fixed_df %>%
  mutate(term = recode(term, !!!name_map))

# Display as formatted table
kable(fixed_df, digits = 3, caption = "Fixed Effects Summary from Mixed Effect Model") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)
# tidy(fit, effects = "ran_pars")

