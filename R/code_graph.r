library(tidyverse)
library(DeclareDesign)
library(wesanderson)
library(tidyr)
library(patchwork)
load("./data/results_simulations_power.Rdata")

RColorBrewer::display.brewer.all()
my_font <- "Palatino Linotype"
my_bkgd <- "white"
#my_bkgd <- "#f5f5f2"
pal <- RColorBrewer::brewer.pal(9, "Spectral")


my_theme <- theme(text = element_text(family = my_font, color = "#22211d"),
                  rect = element_rect(fill = my_bkgd),
                  plot.background = element_rect(fill = my_bkgd, color = NA),
                  panel.background = element_rect(fill = my_bkgd, color = NA),
                  panel.border = element_rect(color="black"), 
                  strip.background = element_rect(color="black", fill="gray85"), 
                  legend.background = element_rect(fill = my_bkgd, color = NA),
                  legend.key = element_rect(size = 6, fill = "white", colour = NA), 
                  legend.key.size = unit(1, "cm"),
                  legend.text = element_text(size = 14, family = my_font),
                  legend.title = element_text(size=14),
                  plot.title = element_text(size = 22, face = "bold", family=my_font),
                  plot.subtitle = element_text(size=16, family=my_font),
                  axis.title= element_text(size=22),
                  
                  axis.text = element_text(size=14, family=my_font),
                  axis.title.x = element_text(hjust=1),
                  strip.text = element_text(family = my_font, color = "#22211d",
                                            size = 13, face="italic"))
theme_set(theme_bw() + my_theme)



results_c <-results  %>%
  mutate(N_fct=as.factor(N),
        eff1=as.factor(eff1), 
        eff2=as.factor(eff2),
        power_bin=ifelse(power>0.79, "Power > 80%", "Power < 80%")) %>%
  mutate(Treatment_Effects=ifelse(term=="violence", "Exposure to Violence", 
                                  "Exposure to Non-Violence"))

res_vio <- results_c %>% 
            filter(term=="violence") %>%
            group_by(N_fct, eff1, term, Treatment_Effects) %>%
            summarise(power=mean(power)) %>%
            ungroup() %>%
            mutate(power_bin=ifelse(power>0.79, "Power > 80%", "Power < 80%"))
            
res_goods <- results_c %>% 
  filter(term=="goods") %>%
  group_by(N_fct, eff2, term, Treatment_Effects) %>%
  summarise(power=mean(power)) %>%
  ungroup() %>%
  mutate(power_bin=ifelse(power>0.79, "Power > 80%", "Power < 80%"))



# Violence
pal <- wes_palette("Zissou1", n=5)

violence <- ggplot(res_vio %>% filter(term=="violence"), aes(x=N_fct,y=eff1, fill=fct_rev(power_bin)))+
  geom_tile(colour="gray95",size=0.5, alpha=.8)  +
  guides(fill=guide_legend(title="Power Results"))+ 
  labs(x="Number of Observations",
       y="") +
  scale_fill_manual(values=c(pal[1], pal[5]))  +
  facet_grid(~ Treatment_Effects) +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=10), 
        strip.text = element_text(family = my_font, color = "#22211d",
                                  size = 14, face="italic"), 
        plot.caption = element_text(size=10)) 

goods <- ggplot(res_goods %>% filter(term=="goods"), aes(x=N_fct,y=eff2, fill=fct_rev(power_bin)))+
  geom_tile(colour="gray95",size=0.5, alpha=.8)  +
  guides(fill=guide_legend(title="Power Results"))+ 
  labs(x="",y="Effect Size",
       caption="")+
  guides(fill=FALSE) +
  scale_fill_manual(values=c(pal[1], pal[5]))  +
  facet_grid(~ Treatment_Effects) +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=10), 
        strip.text = element_text(family = my_font, color = "#22211d",
                                  size = 14, face="italic"), 
        plot.caption = element_text(size=10)) 

graph <- goods + violence + 
  plot_annotation(title="Power Analysis for List Experiments",
                  subtitle="Criminal governance amid the COVID-19 pandemic (EGAP GRANT)", 
                  caption="Power Analysis estimated using the DeclareDesign framework")

getwd()
?ggsave
ggsave(graph, filename = "./power_analysis/egap_covid_poweranalysis/power_analysis.png",    width = 14, height = 8, units = "in", pointsize = 12, bg = "white")
