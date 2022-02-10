#.............................................................................................................#
#.......................................... Fig1 Distribution of Biomass .....................................#
#.............................................................................................................#


data_br$Rep <- factor(data_br$Rep, levels = c("Rep_1", "Rep_2", "Rep_3"))
g1 <- ggplot(data_br, aes(x=May, fill=Rep)) + 
  geom_density(alpha=0.7)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 12))+
  scale_fill_manual(values = c("red","green", "blue"))+
  annotate("text", x = 12, y = 0.3, size=10, label = "May") 

data_br$Rep <- factor(data_br$Rep, levels = c("Rep_1", "Rep_2", "Rep_3"))
g5 <- ggplot(data_br, aes(x=July, fill=Rep)) + 
  geom_density(alpha=0.7)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        legend.position = "none")+
  scale_fill_manual(values =c("red","green", "blue"))+
  annotate("text", x = 11.5, y = 0.3, size=10, label = "July") 

data_br$Rep <- factor(data_br$Rep, levels = c("Rep_1", "Rep_2", "Rep_3"))
g9 <- ggplot(data_br, aes(x=Sept, fill=Rep)) + 
  geom_density(alpha=0.7)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        legend.position = c(0.8, 0.4),
        axis.text = element_text(size = 12))+
  scale_fill_manual(values = c("red","green", "blue"))+
  annotate("text", x = 4, y = 2.5, size=10, label = "Sept") +
  labs(fill= "Replication")

#+++++++++++++++++++++++++++++++++++++++++++++++++++++#
cor <- round(cor(data_br$May, data_br$July), 2)
g4 <- ggplot() + 
  annotate("text", x = 4, y = 25, size=9, label = "-0.05 = \n Median(-0.05, -0.02, -0.06)") + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
  )

cor <- round(cor(data_br$May, data_br$Sept), 2)
g7 <- ggplot() + 
  annotate("text", x = 4, y = 25, size=9, label = "-0.10 = \n Median(-0.10, -0.12, 0.03)") + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

cor <- round(cor(data_br$July, data_br$Sept), 2)
g8 <- ggplot() + 
  annotate("text", x = 4, y = 25, size=9, label = "0.61 = \n Median(0.79, 0.61, 0.16)") + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

#+++++++++++++++++++++++++++++++++++++++++++++++++++++#
data_br$Rep <- factor(data_br$Rep, levels = c("Rep_1", "Rep_2", "Rep_3"))
g2 <- ggplot(data_br) + geom_point(aes(x=May, y=July, color = Rep), shape=21)+
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 12)
  )+
  scale_color_manual(values = c("red", "green", "blue")) +
  xlim(0, 15)

g3 <- ggplot(data_br) + geom_point(aes(x=May, y=Sept, color = Rep), shape=21)+
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 12)
  )+
  scale_color_manual(values = c("red", "green", "blue")) +
  xlim(0, 15)

g6 <- ggplot(data_br) + geom_point(aes(x=July, y=Sept, color = Rep), shape=21)+
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 12)
  )+
  scale_color_manual(values = c("red", "green", "blue")) +
  xlim(0, 15)


gg_list2 <- list(g1, g2, g3, g4,g5,g6,g7,g8,g9)

gg_m2 <- ggmatrix(
  plots = gg_list2, # 绘图对象列表
  nrow = 3, ncol = 3, # 行数和列数
  byrow = FALSE # 按列排
)
gg_m2

