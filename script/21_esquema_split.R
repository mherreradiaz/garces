source('script/00_setup.R')
library(patchwork)

data <- tibble(x = c(1,2,3,4.5,5.5,6.5)) |> 
  group_by(x) |> 
  reframe(grupo = 1:2) |> 
  group_by(x,grupo) |> 
  reframe(panel = factor(c(1,2,3,4,5,6))) |> 
  group_by(x,grupo,panel) |> 
  reframe(y = 1:5) |> 
  mutate(panel = as.factor(panel), grupo = as.factor(grupo),
         split_2 = ifelse(panel %in% c(2,5),1,2))

data$split_1 <- sample(c(rep(1, 0.25 * nrow(data)), rep(2, 0.75 * nrow(data))))

levels(data$panel) <- c('date[1]','date[2]','date[3]','...','date[n-1]','date[n]')

plot_1 <- data |> 
  filter(grupo == 1) |> 
  ggplot(aes(x,y,color = as.factor(split_1))) +
  geom_point() +
  facet_grid(.~panel,switch = "y",labeller=label_parsed) +
  ylim(0.5,7.5) +
  labs(y = 'Estimation\nrnd-split',x=NULL,color=NULL) +
  scale_x_discrete(expand=c(.2,0)) +
  scale_color_manual(values = c("1" = "red", "2" = "black")) + 
  geom_text(aes(x = 2, y = 5.9, label = 'Río\nClaro'), vjust = 0, 
            color = 'grey20', size = 2.2) +
  geom_text(aes(x = 5.5, y = 5.9, label = 'La\nEsperanza'), vjust = 0, 
            color = 'grey20', size = 2.2) +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white',linetype = 'blank'),
        panel.spacing = unit(0, "lines"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(angle = 0,hjust=.5,vjust=0,size=9),
        legend.position = "none",
        aspect.ratio = 1)

plot_2 <- data |> 
  filter(grupo == 1) |> 
  ggplot(aes(x,y,color=as.factor(split_2))) +
  geom_point(alpha = .8) +
  facet_grid(.~panel,switch = "y",labeller=label_parsed) +
  ylim(0.5,7.5) +
  labs(y = 'Prediction\ntme-split',x=NULL,color = NULL) +
  scale_x_discrete(expand=c(.2,0)) +
  scale_color_manual(values = c("1" = "red", "2" = "black"),
                     label = c('Test dataset','Training dataset')) + 
  geom_text(aes(x = 2, y = 5.9, label = 'Río\nClaro'), vjust = 0, 
            color = 'grey20', size = 2.2) +
  geom_text(aes(x = 5.5, y = 5.9, label = 'La\nEsperanza'), vjust = 0, 
            color = 'grey20', size = 2.2) +
  theme_bw() +
  theme(strip.text.x = element_blank(), 
        strip.background = element_rect(fill = 'white'),
        panel.spacing = unit(0, "lines"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(angle = 0,hjust=.5,vjust=.5,size = 9),
        legend.position = "bottom",
        legend.margin = margin(t = -2),
        aspect.ratio = 1)

plot_1/plot_2

ggsave(paste0('C:/Hemera/garces/output/figs/esquema_split.png'), width = 7, height = 3)