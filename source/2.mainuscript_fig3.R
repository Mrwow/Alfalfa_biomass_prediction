#.............................................................................................................#
#......................... Fig3 correlations among biomass and five image features ...........................#
#.............................................................................................................#


data_std$Month <- factor(data_std$Month, levels = c("May", "July", "Sept")) # reorder the legend

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Diag ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

g1<- ggplot() + 
  annotate("text", x = 4, y = 25, size=8, label = "Biomass") + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

g8 <- ggplot() + 
  annotate("text", x = 4, y = 25, size=8, label = "Area") + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

g15 <- ggplot() + 
  annotate("text", x = 4, y = 25, size=8, label = "Height") + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())


g29<- ggplot() + 
  annotate("text", x = 4, y = 25, size=8, label = "NDRE") + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

g36<- ggplot() + 
  annotate("text", x = 4, y = 25, size=8, label = "NGRDI") + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Lower ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## only use data between [-3,3]
g2 <- ggscatter(data_std, x="Biomass", y="Area", color = "Month", shape = "Replication",size = 1)+
  scale_color_manual(values = c("Blue", "Green", "red"))+
  scale_shape(solid = FALSE) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        text = element_text(size = 15)
  )+
  scale_shape(solid = FALSE) + #make the solid dot into a empty dot
  scale_color_manual(values = c("Blue", "Green", "red"))

g3 <- ggscatter(data_std, x="Biomass", y="Height", color = "Month", shape = "Replication",size = 1)+
  scale_color_manual(values = c("Blue", "Green", "red"))+
  scale_shape(solid = FALSE) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        text = element_text(size = 15)
  )+
  scale_shape(solid = FALSE) + #make the solid dot into a empty dot
  scale_color_manual(values = c("Blue", "Green", "red"))

g5 <- ggscatter(data_std, x="Biomass", y="NDRE", color = "Month", shape = "Replication",size = 1)+
  scale_color_manual(values = c("Blue", "Green", "red"))+
  scale_shape(solid = FALSE) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        text = element_text(size = 15)
  )+
  scale_shape(solid = FALSE) + #make the solid dot into a empty dot
  scale_color_manual(values = c("Blue", "Green", "red"))

g6 <- ggscatter(data_std, x="Biomass", y="NGRDI", color = "Month", shape = "Replication",size = 1)+
  scale_color_manual(values = c("Blue", "Green", "red"))+
  scale_shape(solid = FALSE) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        text = element_text(size = 15)
  )+
  scale_shape(solid = FALSE) + #make the solid dot into a empty dot
  scale_color_manual(values = c("Blue", "Green", "red"))

g9 <- ggscatter(data_std, x="Area", y="Height", color = "Month", shape = "Replication",size = 1)+
  scale_color_manual(values = c("Blue", "Green", "red"))+
  scale_shape(solid = FALSE) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        text = element_text(size = 15)
  )+
  scale_shape(solid = FALSE) + #make the solid dot into a empty dot
  scale_color_manual(values = c("Blue", "Green", "red"))

g11 <- ggscatter(data_std, x="Area", y="NDRE", color = "Month", shape = "Replication",size = 1)+
  scale_color_manual(values = c("Blue", "Green", "red"))+
  scale_shape(solid = FALSE) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        text = element_text(size = 15)
  )+
  scale_shape(solid = FALSE) + #make the solid dot into a empty dot
  scale_color_manual(values = c("Blue", "Green", "red"))

g12 <- ggscatter(data_std, x="Area", y="NGRDI", color = "Month", shape = "Replication",size = 1)+
  scale_color_manual(values = c("Blue", "Green", "red"))+
  scale_shape(solid = FALSE) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        text = element_text(size = 15)
  )+
  scale_shape(solid = FALSE) + #make the solid dot into a empty dot
  scale_color_manual(values = c("Blue", "Green", "red"))

g17 <- ggscatter(data_std, x="Height", y="NDRE", color = "Month", shape = "Replication",size = 1)+
  scale_color_manual(values = c("Blue", "Green", "red"))+
  scale_shape(solid = FALSE) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        text = element_text(size = 15)
  )+
  scale_shape(solid = FALSE) + #make the solid dot into a empty dot
  scale_color_manual(values = c("Blue", "Green", "red"))

g18 <- ggscatter(data_std, x="Height", y="NGRDI", color = "Month", shape = "Replication",size = 1)+
  scale_color_manual(values = c("Blue", "Green", "red"))+
  scale_shape(solid = FALSE) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        text = element_text(size = 15)
  )+
  scale_shape(solid = FALSE) + #make the solid dot into a empty dot
  scale_color_manual(values = c("Blue", "Green", "red"))

g30 <- ggscatter(data_std, x="NDRE", y="NGRDI", color = "Month", shape = "Replication",size = 1)+
  scale_color_manual(values = c("Blue", "Green", "red"))+
  scale_shape(solid = FALSE) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        text = element_text(size = 15)
  )+
  scale_shape(solid = FALSE) + #make the solid dot into a empty dot
  scale_color_manual(values = c("Blue", "Green", "red"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Upper ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# bold the meadian correaltion
find_cell <- function(table, row, col, name="core-fg"){
  l <- table$layout
  which(l$t==row & l$l==col & l$name==name)
}

median_cell <- function(table, row, col, name="core-fg"){
  cors_list <- c()
  for (i in 2:col){
    for (j in 2:row){
      ind <- find_cell(table=table, row = i, col = j, name = "core-fg")
      cors_list <- c(cors_list, as.numeric(table$grobs[[ind]]$label))
    }
  }
  indx <- which(cors_list == median(cors_list))
  print(cors_list)
  print(indx)
  for(k in indx){
    nrow = floor((k-1)/(row-1)) + 2
    ncol = (k-1)%%(col-1) + 2
    print(k)
    print(nrow)
    print(ncol)
    ind_f <- find_cell(table = table, row = nrow, col = ncol, name = "core-fg")
    table$grobs[ind_f][[1]][['gp']] <- gpar(fontface="bold", fontsize=15, col="black")
  }
  table
}

corCol <- "#FFFFBF"
thm <- ttheme_minimal(base_size = 14, colhead= list(padding=unit.c(unit(0.5, "mm"), unit(5, "mm"))))
g7_cors <- ddply(data_std, .(Month, Replication), summarize, R = round(cor(Biomass, Area), digits = 2))
g7_table <- spread(g7_cors, Month, R)
tg <- tableGrob(g7_table, rows = NULL, theme = thm)
tg_c <- median_cell(table = tg,row = 4, col = 4, name = "core-fg")
g7 <- ggplot() + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background=element_rect(fill=corCol))+
  annotation_custom(tg_c)

g13_cors <- ddply(data_std, .(Month, Replication), summarize, R = round(cor(Biomass, Height), digits = 2))
g13_table <- spread(g13_cors, Month, R)
tg <- tableGrob(g13_table, rows = NULL, theme = thm)
tg_c <- median_cell(table = tg,row = 4, col = 4, name = "core-fg")
g13 <- ggplot() + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background=element_rect(fill=corCol))+
  annotation_custom(tg_c)

g14_cors <- ddply(data_std, .(Month, Replication), summarize, R = round(cor(Area, Height), digits = 2))
g14_table <- spread(g14_cors, Month, R)
tg <- tableGrob(g14_table, rows = NULL, theme = thm)
tg_c <- median_cell(table = tg,row = 4, col = 4, name = "core-fg")
g14 <- ggplot() + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background=element_rect(fill=corCol))+
  annotation_custom(tg_c)

g25_cors <- ddply(data_std, .(Month, Replication), summarize, R = round(cor(Biomass, NDRE), digits = 2))
g25_table <- spread(g25_cors, Month, R)
tg <- tableGrob(g25_table, rows = NULL, theme = thm)
tg_c <- median_cell(table = tg,row = 4, col = 4, name = "core-fg")
g25 <- ggplot() + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background=element_rect(fill=corCol))+
  annotation_custom(tg_c)


g26_cors <- ddply(data_std, .(Month, Replication), summarize, R = round(cor(Area, NDRE), digits = 2))
g26_table <- spread(g26_cors, Month, R)
tg <- tableGrob(g26_table, rows = NULL, theme = thm)
tg_c <- median_cell(table = tg,row = 4, col = 4, name = "core-fg")
g26 <- ggplot() + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background=element_rect(fill=corCol))+
  annotation_custom(tg_c)


g27_cors <- ddply(data_std, .(Month, Replication), summarize, R = round(cor(Height, NDRE), digits = 2))
g27_table <- spread(g27_cors, Month, R)
tg <- tableGrob(g27_table, rows = NULL, theme = thm)
tg_c <- median_cell(table = tg,row = 4, col = 4, name = "core-fg")
g27 <- ggplot() + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background=element_rect(fill=corCol))+
  annotation_custom(tg_c)


g31_cors <- ddply(data_std, .(Month, Replication), summarize, R = round(cor(Biomass, NGRDI), digits = 2))
g31_table <- spread(g31_cors, Month, R)
tg <- tableGrob(g31_table, rows = NULL, theme = thm)
tg_c <- median_cell(table = tg,row = 4, col = 4, name = "core-fg")
g31 <- ggplot() + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background=element_rect(fill=corCol))+
  annotation_custom(tg_c)


g32_cors <- ddply(data_std, .(Month, Replication), summarize, R = round(cor(Area, NGRDI), digits = 2))
g32_table <- spread(g32_cors, Month, R)
tg <- tableGrob(g32_table, rows = NULL, theme = thm)
tg_c <- median_cell(table = tg,row = 4, col = 4, name = "core-fg")
g32 <- ggplot() + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background=element_rect(fill=corCol))+
  annotation_custom(tg_c)


g33_cors <- ddply(data_std, .(Month, Replication), summarize, R = round(cor(Height, NGRDI), digits = 2))
g33_table <- spread(g33_cors, Month, R)
tg <- tableGrob(g33_table, rows = NULL, theme = thm)
tg_c <- median_cell(table = tg,row = 4, col = 4, name = "core-fg")
g33 <- ggplot() + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background=element_rect(fill=corCol))+
  annotation_custom(tg_c)

g35_cors <- ddply(data_std, .(Month, Replication), summarize, R = round(cor(NDRE, NGRDI), digits = 2))
g35_table <- spread(g35_cors, Month, R)
tg <- tableGrob(g35_table, rows = NULL, theme = thm)
tg_c <- median_cell(table = tg,row = 4, col = 4, name = "core-fg")
g35 <- ggplot() + 
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background=element_rect(fill=corCol))+
  annotation_custom(tg_c)

# biomass, Area, Height, NDRE, NGRDI
gg_list2 <- list(g1, g2, g3, g5, g6,
                 g7, g8, g9, g11, g12,
                 g13, g14, g15, g17, g18,
                 g25, g26, g27, g29, g30,
                 g31, g32, g33,g35, g36
)
gg_m <- ggmatrix(
  plots = gg_list2, # 绘图对象列表
  nrow = 5, ncol = 5, # 行数和列数
  byrow = FALSE # 按列排
)
tiff("../Result/Version10/Fig3.png", res = 600, units="in", width=15, height=15)
gg_m
dev.off()

