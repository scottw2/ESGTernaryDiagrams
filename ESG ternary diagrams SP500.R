# import packages
library(tidyverse)
library(ggtern)
library(rcartocolor)
library(extrafont)

# import ESG data for S&P500 firms scraped from Yahoo finance
SPESG <- as_tibble(read.csv("sp500ESGdataYahoo210528.csv"))

# tidy up a bit
SPESG <- SPESG %>% 
  select(-X)
SPESGF <- SPESG %>% 
  select(governanceScore, environmentScore, socialScore, totalEsg, company_ticker)

# import S&P500 data scraped from wikipedia
SP <- as_tibble(read.csv("S&P500-Info.csv"))
SP <- SP %>% rename(company_ticker = Symbol)

# join datasets
SPESGF2 <- SPESGF %>% 
  inner_join(SP, by = "company_ticker") %>% 
  rename(E= environmentScore, G= governanceScore, S = socialScore)

#find median per industry (for industry diagram)
SPESGF2med <- SPESGF2 %>% 
  group_by(GICS.Sector) %>% 
  summarise(E = median(E), S = median(S), G = median(G))

# ternary diagrams --------

# main diagram
Tern1B <- SPESGF2 %>% 
  ggtern(aes(x = E, y = S, z = G, colour = GICS.Sector)) +
  limit_tern(breaks = c(1, 0.75, 0.5, 0.25, 0), minor_breaks = NULL) +
  geom_point(alpha = 0.60, size = 3) +
  theme_nomask() +
  theme_bw() +
  # annotate("text", x = -5, y = 80, z = 15,angle = -(180/3) ,label = "Higher Social Risk", family = "Lato") +
  theme(text = element_text(family = "Lato", colour = "white"),
        plot.title = element_text(family = "Lato Black", hjust = 0.5),
        axis.title = element_text(family = "Lato", face = "bold"),
        plot.subtitle = element_text(family = "Lato", hjust = 0.5),
        legend.title = element_text(family = "Lato", face = "bold"),
        plot.margin = unit( c(0.5,0.5,0.5,0.5), units = "in"),
        legend.position = "bottom",
        legend.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "NA"),
        plot.background = element_rect(fill = "black"),
        panel.grid = element_line(colour = "grey75"),
        tern.axis.line = element_line(colour = "white"),
        axis.text = element_text(family = "Lato", colour = "white"),
        legend.key = element_rect(fill = NA)) +
  labs(title = "Ternary Diagram of S&P500 Companies by ESG Risk", subtitle = "Showing 453 companies where risks were measured", colour = "Industry") +
  scale_color_carto_d(palette = "Bold") +
  guides(colour=guide_legend(title.position="top", 
                             title.hjust =0.5))
# export
ggsave(Tern1B, filename = "Tern1B.png", dpi = 400,
       width = 12, height = 12, units = "in", type = "cairo") 

# 2nd ternary diagram with colour representing total ESG
Tern1ESG <- SPESGF2 %>% 
  ggtern(aes(x = E, y = S, z = G, colour = totalEsg)) +
  limit_tern(breaks = c(1, 0.75, 0.5, 0.25, 0), minor_breaks = NULL) +
  geom_point(alpha = 0.85, size = 3) +
  theme_nomask() +
  theme_bw() +
  # annotate("text", x = -5, y = 80, z = 15,angle = -(180/3) ,label = "Higher Social Risk", family = "Lato") +
  theme(text = element_text(family = "Lato", colour = "white"),
        plot.title = element_text(family = "Lato Black", hjust = 0.5),
        axis.title = element_text(family = "Lato", face = "bold"),
        plot.subtitle = element_text(family = "Lato", hjust = 0.5),
        legend.title = element_text(family = "Lato", face = "bold"),
        plot.margin = unit( c(0.5,0.5,0.5,0.5), units = "in"),
        legend.position = "bottom",
        legend.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = NA),
        plot.background = element_rect(fill = "black"),
        panel.grid = element_line(colour = "grey75"),
        tern.axis.line = element_line(colour = "white"),
        axis.text = element_text(family = "Lato", colour = "white"),
        legend.key = element_rect(fill = NA)) +
  labs(title = "Ternary Diagram of S&P500 Companies by Total ESG Risk", subtitle = "Showing 453 companies where risks were measured", colour = "Total ESG Risk") +
  scale_colour_carto_c(palette = "BluYl", direction = -1, limits = c(0, 50), breaks = c(10, 20, 30 ,40)) +
  guides(colour=guide_colorsteps(title.position="top", 
                                 title.hjust =0.5, barwidth = unit(85, "mm"), barheight = unit(3, "mm"), show.limits = T))

# export
ggsave(Tern1ESG, filename = "Tern1ESG.png", dpi = 400,
       width = 12, height = 12, units = "in", type = "cairo") 

# hexbinned ternary diagram, showing distribution of firms better
Tern1HB <- SPESGF2 %>% 
  ggtern(aes(x = E, y = S, z = G)) +
  limit_tern(breaks = c(1, 0.75, 0.5, 0.25, 0), minor_breaks = NULL) +
  geom_hex_tern(colour = "white") +
  theme_nomask() +
  theme_bw() +
  # annotate("text", x = -5, y = 80, z = 15,angle = -(180/3) ,label = "Higher Social Risk", family = "Lato") +
  theme(text = element_text(family = "Lato", colour = "white"),
        plot.title = element_text(family = "Lato Black", hjust = 0.5),
        axis.title = element_text(family = "Lato", face = "bold"),
        plot.subtitle = element_text(family = "Lato", hjust = 0.5),
        legend.title = element_text(family = "Lato", face = "bold"),
        plot.margin = unit( c(0.5,0.5,0.5,0.5), units = "in"),
        legend.position = "bottom",
        legend.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "NA"),
        plot.background = element_rect(fill = "black"),
        panel.grid = element_line(colour = "grey75"),
        tern.axis.line = element_line(colour = "white"),
        axis.text = element_text(family = "Lato", colour = "white"),
        legend.key = element_rect(fill = NA)) +
  scale_fill_carto_c(palette = "PinkYl", direction = 1, limits = c(1, 10), breaks = c(4, 7)) +
  guides(fill=guide_colorsteps(title.position="top", 
                               title.hjust =0.5, barwidth = unit(85, "mm"), barheight = unit(3, "mm"), show.limits = T)) +
  
  labs(title = "Hexbin Distribution of S&P500 Companies by ESG Risk", subtitle = "Showing 453 companies where risks were measured", fill = "Number of companies")

# export
ggsave(Tern1HB, filename = "Tern1HB2.png", dpi = 400,
       width = 12, height = 12, units = "in", type = "cairo") 

# ternary diagrams facetted by industry
Tern2Facet <- SPESGF2 %>% 
  ggtern(aes(x = E, y = S, z = G, colour = totalEsg)) +
  limit_tern(breaks = c(1, 0.75, 0.5, 0.25, 0), minor_breaks = NULL) +
  geom_point(alpha = 0.6, size = 2.25) +
  geom_point(data = SPESGF2med, colour = "white") +
  facet_wrap(~GICS.Sector) +
  theme_bw() +
  theme_nomask() +
  theme_hidelabels() +
  theme(text = element_text(family = "Lato", colour = "white"),
        plot.title = element_text(family = "Lato Black", hjust = 0.5),
        axis.title = element_text(family = "Lato Black"),
        plot.subtitle = element_text(family = "Lato", hjust = 0.5),
        legend.title = element_text(family = "Lato", face = "bold"),
        plot.margin = unit( c(0.5,0.5,0.5,0.5), units = "in"),
        legend.position = "bottom",
        panel.background = element_rect(fill = "NA"),
        plot.background = element_rect(fill = "black"),
        panel.grid = element_line(colour = "grey75"),
        tern.axis.line = element_line(colour = "white"),
        axis.text = element_text(family = "Lato", colour = "white"),
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text = element_text(family = "Lato Black", colour = "white"),
        legend.background = element_rect(fill = "black")) +
  labs(title = "ESG Risks of S&P500 Companies, Faceted by Industry", subtitle = "Showing 453 companies where risks were measured", colour = "Total ESG Risk") +
  scale_colour_carto_c(palette = "BluYl", direction = -1, limits = c(0, 50), breaks = c(10, 20, 30 ,40)) +
  guides(colour=guide_colorsteps(title.position="top", 
                                 title.hjust =0.5, barwidth = unit(85, "mm"), barheight = unit(3, "mm"), show.limits = T))

# export
ggsave(Tern2Facet, filename = "Tern2Facet2.png", dpi = 400,
       width = 12, height = 12, units = "in", type = "cairo") 
