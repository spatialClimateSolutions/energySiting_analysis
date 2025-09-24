source("./syntax/function.R")
source("./syntax/data.R")

### f1
f1a <- solar_locations %>% 
  mutate(geometry = st_centroid(geometry)) %>% 
  ggplot() +
  geom_sf(data = state_boundaries, fill = "gray50", color = "gray0") + # US border
  geom_sf(aes(color = p_cap_ac), size = 0.5) +
  theme_minimal() +
  scale_color_gradientn(colors = brewer.pal(n = 9, name = "YlOrRd")) +
  # scale_color_viridis_c(direction = -1) +
  labs(title = "Existing solar", color = "Capacity\n(MW)") +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), 
           ylim = c(-2300000,730000), expand = FALSE, datum = NA) +
  theme(legend.position = "right")


f1b <- wind_locations %>% 
  mutate(geometry = st_centroid(geometry)) %>% 
  ggplot() +
  geom_sf(data = state_boundaries, fill = "gray50", color = "gray0") + # US border
  geom_sf(aes(color = p_cap/1000), size = 0.5) +
  theme_minimal() +
  scale_color_gradientn(colors = brewer.pal(n = 9, name = "YlOrRd")) +
  # scale_color_viridis_c(direction = -1) +
  labs(title = "Existing wind", color = "Capacity\n(GW)") +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), 
           ylim = c(-2300000,730000), expand = FALSE, datum = NA) +
  theme(legend.position = "right")

f1c <- s_ab_sf %>% 
  st_buffer(dist = 5000) %>% 
  ggplot() +
  geom_sf(data = state_boundaries, fill = "gray50", color = "gray0") + # US border
  geom_sf(aes(fill = factor(iteration)), color = "NA", size = 0.001) +
  theme_minimal() +
  scale_fill_manual(values = brewer.pal(n = 10, name = "Paired")) +
  # scale_fill_viridis_b(option = "B", n.breaks = 10, direction = -1) +
  labs(title = "Pseudo-absence - solar", fill = "Iteration") +
  coord_sf(crs = 2163, xlim = c(-2500000, 2500000), 
           ylim = c(-2300000,730000), expand = FALSE, datum = NA) +
  theme(legend.position = "right")

f1d <- w_ab_sf %>% 
  ggplot() +
  geom_sf(data = state_boundaries, fill = "gray50", color = "gray0") + # US border
  geom_sf(aes(fill = factor(iteration)), color = "NA", size = 0.001) +
  theme_minimal() +
  scale_fill_manual(values = brewer.pal(n = 10, name = "Paired")) +
  # scale_fill_viridis_b(option = "B", n.breaks = 10, direction = -1) +
  labs(title = "Pseudo-absence - wind", fill = "Iteration") +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), 
           ylim = c(-2300000,730000), expand = FALSE, datum = NA) +
  theme(legend.position = "right")


ggsave("./fig/f1.png", 
       ggarrange(f1a,f1b,f1c,f1d, nrow=2, ncol = 2), width = 14, height = 10, dpi = 300)


### f2
s_reg <- rst_s %>% 
  ggplot(aes(x = pe, y = reorder(var, pe))) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  geom_errorbar(aes(xmin=pe-1.96*se, xmax=pe+1.96*se),color="gray50", width = 0.5) +
  geom_point(aes(fill = color),size = 2,pch=21) +
  theme_bw() +
  facet_wrap(~ Group, nrow = 1) +
  
  labs(fill = "Significant", y = "", x = "Odds ratio (log scale)", title = "A") +
  scale_fill_manual(values=c("gray", "red")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
        legend.position = "none",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=6),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=16, hjust = -0.09)) 


s_reg1 <- rst_s %>% 
  mutate(Group = factor(Group),
         color = factor(color, levels = c("Y","N"))) %>% 
  ggplot(aes(x = pe, y = reorder(var, pe))) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  geom_point(aes(fill = color),size = 2,pch=21) +
  theme_bw() +
  
  labs(fill = "Significant", y = "", x = "Odds ratio (log scale)", title = "A") +
  scale_fill_manual(values=c("red", "gray")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
        legend.position = "none",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=6),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=16, hjust = -0.22)) 


s_reg2 <- rst_s %>% 
  filter(Group == 5) %>% 
  mutate(color = factor(color, levels = c("Y","N"))) %>% 
  ggplot(aes(x = pe, y = reorder(var,pe))) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  geom_errorbar(aes(xmin=pe-1.96*se, xmax=pe+1.96*se),color="gray50", width = 0.5) +
  geom_point(aes(fill = color),size = 2,pch=21) +
  theme_bw() +
  
  labs(fill = "Significant", y = "", x = "Odds ratio (log scale)", title = "") +
  scale_fill_manual(values=c("red", "gray")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
        legend.position = c(0.7, 0.3),
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=6),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=16, hjust = -0.09)) 


w_reg <- rst_w %>% 
  ggplot(aes(x = pe, y = reorder(var, pe))) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  geom_errorbar(aes(xmin=pe-1.96*se, xmax=pe+1.96*se),color="gray50", width = 0.5) +
  geom_point(aes(fill = color),size = 2,pch=21) +
  theme_bw() +
  labs(title = "B") +
  facet_wrap(~ Group, nrow = 1) +
  
  labs(fill = "Significant", y = "", x = "Odds ratio (log scale)") +
  scale_fill_manual(values=c("gray", "red")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
        legend.position = "bottom",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=6),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=16,  hjust = -0.09)) 


w_reg1 <- rst_w %>% 
  mutate(Group = factor(Group),
         color = factor(color, levels = c("Y","N"))) %>% 
  ggplot(aes(x = pe, y = reorder(var, pe))) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  geom_point(aes(fill = color),size = 2,pch=21) +
  theme_bw() +
  
  labs(fill = "Significant", y = "", x = "Odds ratio (log scale)", title = "B") +
  scale_fill_manual(values=c("red", "gray")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
        legend.position = "none",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=6),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=16, hjust = -0.22)) 

w_reg2 <- rst_w %>% 
  filter(Group == 5) %>% 
  mutate(color = factor(color, levels = c("Y","N"))) %>% 
  ggplot(aes(x = pe, y = reorder(var, pe))) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  geom_errorbar(aes(xmin=pe-1.96*se, xmax=pe+1.96*se),color="gray50", width = 0.5) +
  geom_point(aes(fill = color),size = 2,pch=21) +
  theme_bw() +
  
  labs(fill = "Significant", y = "", x = "Odds ratio (log scale)", title = "") +
  scale_fill_manual(values=c("red", "gray")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
        legend.position = c(0.2, 0.7),
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=6),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=20)) 


importance_s <- imp %>% 
  filter(tech == "Solar") %>% 
  ggplot() +
  geom_col(aes(x = reorder(rowname, desc(Overall)), y = Overall, fill = model), width = 0.7, position = position_dodge(0.8)) +
  labs(x = "", y = "Importance (%)", fill = "", title = "") +
  theme_bw() +
  scale_fill_brewer(palette = "Paired") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
        legend.position = "right",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=11, angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=9),
        plot.title=element_text(family="Franklin Gothic Demi", size=16, hjust = -0.03)) 


importance_w <- imp %>% 
  filter(tech == "Wind") %>% 
  ggplot() +
  geom_col(aes(x = reorder(rowname, desc(Overall)), y = Overall, fill = model), width = 0.7, position = position_dodge(0.8)) +
  labs(x = "", y = "Importance (%)", fill = "", title = "") +
  theme_bw() +
  scale_fill_brewer(palette = "Paired") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
        legend.position = "right",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=11,angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=9),
        plot.title=element_text(family="Franklin Gothic Demi", size=16, hjust = -0.02)) 


f2 <- ggarrange(ggarrange(ggarrange(s_reg1, s_reg2, ncol = 2), 
                             importance_s, 
                             heights = c(1.5, 1),
                             nrow = 2),
                   ggarrange(ggarrange(w_reg1, w_reg2, ncol = 2), 
                             importance_w, 
                             heights = c(1.5, 1),
                             nrow = 2), 
                   nrow = 2, heights = c(1,1))
ggsave("./fig/f2.png", f2, width = 12, height = 14, dpi = 300)

### f3 
