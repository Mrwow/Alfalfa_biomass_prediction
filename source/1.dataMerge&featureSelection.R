# Selection predictors using RFE, machine learning model, train and test, split data
library(caret)
# grid search parameter 
library(grid)
# standarlize data within group for using ddply function
library(plyr)
# plot
library(ggplot2)
library(GGally)
library(ggpubr)
# grid.arrange
library(gridExtra)
library(stringr)
# for adding table in a ggplot plot
library(gridExtra)
# orgnize data into table
library(data.table)
# reshape data.frame
library(tidyr)
setwd("/Users/ZhouTang/Downloads/zzlab/1.Project/Alfalfa_biomassPrediction/Source")
# pheatmap
library(pheatmap)
# for rmse
library(Metrics) 

#.............................................................................................................#
#........................................ organizing data ....................................................#
#.............................................................................................................#
# NIR and red bind to define the area     NDVI
data_br <- read.table(file = "biomass.txt", sep = "\t", header = TRUE)
data_br <- na.omit(data_br)
data_b_m <- data.frame(id = data_br$ID, Biomass = data_br$May, mon = "May")
data_b_j <- data.frame(id = data_br$ID, Biomass = data_br$July, mon = "July")
data_b_s <- data.frame(id = data_br$ID, Biomass = data_br$Sept, mon = "Sept")
data_b <- rbind(data_b_m, data_b_j, data_b_s)

## drone data
### function to load drone data from grid
org_data <- function(url, reps, mons, rp){
  data <- read.table(file = url, sep = ",", header = T)
  data$var <- sub("\\.0","",data$var)
  data <- data[,c("var", "area_veg","ch_0","ch_1","ch_2","ch_3", "ch_4","ch_5")]
  data$id <- paste(reps, data$var, sep = "")
  data$rep <- reps
  data$Replication <- rp
  data$mon <- mons
  data$Month <- mons
  data
}

### May
may_1 <- org_data("../Processing/photo_grid_May/Version06/rep1_data.csv",reps = "1_", rp="Rep_1",mons = "May")
may_2 <- org_data("../Processing/photo_grid_May/Version06/rep2_data.csv",reps = "2_", rp="Rep_2",mons = "May")
may_3 <- org_data("../Processing/photo_grid_May/Version06/rep3_data.csv",reps = "3_", rp="Rep_3",mons = "May")

### July
july_1 <- org_data("../Processing/photo_grid_July/Version06/rep1_data.csv",reps = "1_", rp="Rep_1",mons = "July")
july_2 <- org_data("../Processing/photo_grid_July/Version06/rep2_data.csv",reps = "2_", rp="Rep_2",mons = "July")
july_3 <- org_data("../Processing/photo_grid_July/Version06/rep3_data.csv",reps = "3_", rp="Rep_3",mons = "July")

### Sept
sept_1 <- org_data("../Processing/photo_grid_Sep02/Version06/rep1_data.csv",reps = "1_", rp="Rep_1",mons = "Sept")
sept_2 <- org_data("../Processing/photo_grid_Sep02/Version06/rep2_data.csv",reps = "2_", rp="Rep_2",mons = "Sept")
sept_3 <- org_data("../Processing/photo_grid_Sep02/Version06/rep3_data.csv",reps = "3_", rp="Rep_3",mons = "Sept")

### rbind
data_d <- rbind(may_1,may_2,may_3,july_1,july_2,july_3,sept_1,sept_2,sept_3)

## combine biomass and drone data
data <- merge(data_b, data_d, by=c("id","mon"))
data <- na.omit(data)

## calculate features
red = data$ch_0
green = data$ch_1
blue = data$ch_2
rd = data$ch_3
nir = data$ch_4
h = data$ch_5
area = data$area_veg
biomass = data$Biomass

## NIR based VIs
CLGreen = nir/green - 1  #CLGreen 
CLRedEdge = nir/rd - 1   #CLRedEdge
CVI = nir*red/green^2    #cvi
NDRE = (nir-rd)/(nir+rd) #ndre
RVI  = red/nir           #rvi
SR  = nir/rd             #sr
NDVI = (nir-red)/(nir+red)                             #ndvi
GNDVI = (nir - green)/(nir + green)                    #gndvi
WDRVI = (0.1*nir - red)/(0.1*nir + red)                #wdrvi
TVI = 0.5*(120*(nir - green)- 200*(red - green))       #tvi
EVI2 = 2.5*(nir - red)/(nir + 6 * red -7.5*blue + 1)   #evi2
LCI = (nir - rd)/(nir - red)                           #lci
MCARI  = ((rd - red)- 0.2*(rd-green))*red/rd           #mcari

## Visible VIs 
GLI  = (2*green - red - blue)/(2*green + red + blue)   #gli
VARI = (green - red)/(green + red - blue)              #vari
EXG = 2*green - red - blue                             #exg
NGRDI  = (green - red)/(green + red)                   #ngrdi
GRVI  = (green^2 - red^2)/(green^2 + red^2)            #grvi
GRBVI  = (green^2 - blue*red)/(green^2 + blue*red)     #grbvi

Vol = area*h                                           #Vol

## corelation 
# data_f <- data.frame(Biomass = biomass, Area= area, Height= h, NDVI=ndvi, NDRE = ndre, NGRDI = ngrdi )
data_f <- data.frame(Id=data$id,Biomass = biomass, Area= area, Height= h, NDVI=NDVI, NDRE = NDRE, NGRDI = NGRDI,
                     EXG = EXG,CLGreen=CLGreen, CLRedEdge=CLRedEdge, CVI=CVI, RVI=RVI, SR=SR, 
                     GNDVI=GNDVI, WDRVI=WDRVI, TVI=TVI, EVI2=EVI2, LCI=LCI, 
                     MCARI=MCARI, GLI=GLI, VARI=VARI, GRVI = GRVI, GRBVI=GRBVI ,Vol=Vol)
data_f$Month <- data$Month
data_f$Replication <- data$Replication
data_f <- na.omit(data_f)

## standardization data by month and replication
data_std <- ddply(data_f, c("Month", "Replication"), transform, Biomass = scale(Biomass), 
                  Area = scale(Area), 
                  Height = scale(Height),
                  NDVI = scale(NDVI),
                  NDRE = scale(NDRE),
                  NGRDI = scale(NGRDI),
                  EXG = scale(EXG),
                  CLGreen = scale(CLGreen),
                  CLRedEdge = scale(CLRedEdge),
                  CVI = scale(CVI),
                  RVI = scale(RVI),
                  SR = scale(SR),
                  GNDVI = scale(GNDVI),
                  WDRVI = scale(WDRVI),
                  TVI = scale(TVI),
                  EVI2 = scale(EVI2),
                  LCI = scale(LCI),
                  MCARI = scale(MCARI),
                  GLI = scale(GLI),
                  VARI = scale(VARI),
                  GRVI = scale(GRVI),
                  GRBVI = scale(GRBVI),
                  Vol = scale(Vol)
)


## output data and data_f
write.table(data, file = "NDVI_biomass&bands.csv", sep = ",", col=NA)
write.table(data_f, file = "NDVI_biomass&features.csv", sep = ",", col=NA)
write.table(data_std, file = "NDVI_biomass&features_standarliseWithinMonRep.csv", sep = ",", col = NA)

#.............................................................................................................#
#............................................... features selection ..........................................#
#.............................................................................................................#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~ anova test for each features ~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
rank_feature <- function(data_frame){
  name <- c()
  res  <- c()
  if ( "Month" %in% colnames(data_frame) ){
    data_frame <- subset(data_frame, select = -c(Month, Replication, Id))
  }
  name <- c(name, "Full_model")
  res <- c(res,tail(summary(aov(Biomass ~ . , data = data_frame))[[1]]$`Sum Sq`, n=1))
  num_features <- dim(data_frame)[2]
  name_features <- colnames(data_frame)
  for (i in 2:num_features){
    name <- c(name, name_features[i])
    a <- data_frame[,-i]
    res <- c(res, tail(summary(aov(Biomass ~ . , data = a))[[1]]$`Sum Sq`, n=1) )
    
  }
  out <- data.frame(Model = name, Residual = res )
  out
}

features_rank <- rank_feature(data_std)
write.table(features_rank, file = "features_rank_NDVI.csv", sep = ",", col = NA)

resis <- features_rank[,2] - features_rank[1,2]
features_rank$Residual <- resis
features_rank <- features_rank[-1,]
features_rank <- features_rank[order(-features_rank$Residual),]

features_name<- features_rank$Model
features_rank$Model <- factor(features_rank$Model, levels = features_name)
ggplot(features_rank, aes(x=Model, y=Residual)) +
  geom_col() + 
  ylab("Excess of residual sum square") +
  xlab("Image features") +
  theme_classic() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.position = c(0.2, 0.2),
        legend.title=element_text(size=15, face="bold"),
        legend.text=element_text(size=15),
        axis.text = element_text(size = 16, colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, size = 22),
        axis.line = element_line(colour = "black", size = 0.6, linetype = "solid"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~ heatmap for each features ~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
## standardization features heatmap
data_htm <- data_std[, -c(1, 2, 25, 26)]
rownames(data_htm) = paste0("Test", 1:dim(data_htm)[1], sep = "")

annotation_row = data.frame(
  Month = data_std[, 25]
)
rownames(annotation_row) = rownames(data_htm)
annotation_row$Month <- factor(annotation_row$Month, levels = c("May", "July", "Sept"))

# using the annotation_col to get extra space between heatmap and tree_col
annotation_col = data.frame(Place1 = rep("tt",dim(data_std)[1]), 
                            Place2 = rep("tt",dim(data_std)[1]),
                            Place3 = rep("tt",dim(data_std)[1]),
                            Place4 = rep("tt",dim(data_std)[1]))
ann_colors = list(Place1 = c("white"),
                  Place2 = c("white"),
                  Place3 = c("white"),
                  Place4 = c("white"))

tiff("../Result/Version10/S10.png", res = 600, units="in", width=8, height=12)
pheatmap(data_htm, cluster_rows = TRUE, cluster_cols = TRUE, scale = "none", 
         show_rownames = FALSE ,annotation_row = annotation_row, cellwidth = 10, cellheight = 0.2,
         annotation_names_row = TRUE, annotation_col = annotation_col, annotation_colors = ann_colors,
         treeheight_col = 100, treeheight_row = 150)
dev.off()

## original scale features heatmap
data_htm_org <- read.table(file = "NDVI_biomass&features.csv", sep = ",", header = TRUE)
rownames(data_htm_org) = paste0("Test",1:dim(data_htm_org)[1])

annotation_row = data.frame(
  Month = data_htm_org$Month
)
rownames(annotation_row) = rownames(data_htm_org)
annotation_row$Month <- factor(annotation_row$Month, levels = c("May", "July", "Sept"))

annotation_col = data.frame(Place1 = rep("tt",dim(data_htm_org)[1]), 
                            Place2 = rep("tt",dim(data_htm_org)[1]))
ann_colors = list(Place1 = c("white"),
                  Place2 = c("white"))

data_htm_org <- data_htm_org[, -c(1,2,3,26,27)]
names(data_htm_org)[c(1,2)] <- c("Area", "Height")

tiff("../Result/Version10/S09.png", res = 600, units="in", width=8, height=12)
pheatmap(data_htm_org, cluster_rows = TRUE, cluster_cols = TRUE, scale = "column",
         show_rownames = FALSE,annotation_row = annotation_row, cellwidth = 10, cellheight = 0.2,
         annotation_names_row = TRUE, annotation_col = annotation_col, annotation_colors = ann_colors,
         treeheight_col = 100, treeheight_row = 150)
dev.off()


## bands
data_s7_1 <- data[,c("ch_0", "ch_1", "ch_2", "ch_3", "ch_4", "ch_5")]
names(data_s7_1) <- c("R","G","B","Red_Edge","NIR","Height")
rownames(data_s7_1) = paste("Test", 1:dim(data_s7_1)[1], sep = "")

annotation_row = data.frame(
  Month = data$Month
)
rownames(annotation_row) = rownames(data_s7_1)
annotation_row$Month <- factor(annotation_row$Month, levels = c("May", "July", "Sept"))

annotation_col = data.frame(Place1 = rep("tt",dim(data)[1]), 
                            Place2 = rep("tt",dim(data)[1]))
ann_colors = list(Place1 = c("white"),
                  Place2 = c("white"))

tiff("../Result/Version10/S7_a.png", res = 600, units="in", width=8, height=12)
pheatmap(data_s7_1, cluster_rows = TRUE, cluster_cols = TRUE, scale = "column", 
         show_rownames = FALSE ,annotation_row = annotation_row, cellwidth = 15, cellheight = 0.2,
         annotation_names_row = TRUE, annotation_col = annotation_col, annotation_colors = ann_colors,
         treeheight_col = 100, treeheight_row = 150)
dev.off()

data_s7_2 <- ddply(data, c("Month", "Replication"), transform, 
                                 R = scale(ch_0),
                                 G = scale(ch_1),
                                 B = scale(ch_2),
                                 Red_Edge = scale(ch_3),
                                 NIR = scale(ch_4),
                                 Height = scale(ch_5))
data_s7_2 <- data_s7_2[,c("R","G","B","Red_Edge","NIR","Height")]
rownames(data_s7_2) = paste("Test", 1:dim(data_std)[1], sep = "")

tiff("../Result/Version10/S7_b.png", res = 600, units="in", width=8, height=12)
pheatmap(data_s7_2, cluster_rows = TRUE, cluster_cols = TRUE, scale = "none", 
         show_rownames = FALSE ,annotation_row = annotation_row, cellwidth = 10, cellheight = 0.2,
         annotation_names_row = TRUE, annotation_col = annotation_col, annotation_colors = ann_colors,
         treeheight_col = 100, treeheight_row = 150)
dev.off()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~ corelation for each features ~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#### features
panel_dig <- function(x, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1) )
  legend( x="bottomleft", title = "Month",
          legend=c("May","July", "Sept"),
          col=c("blue","green", "red"), pch=20 )
  legend( x = "bottomright",title = "Replication",
          legend=c("Rep_1","Rep_2", "Rep_3"),
          col="grey", pch=c(1,0,2) )
}

panel_lower<-function(x,y,col=par("col"),bg=NA,pch = par("pch"),cex=1,col.smooth="black",...){
  points(x, y,cex=0.12)
}


panel_upper <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <-round(cor(x, y),2)
  corColors <- RColorBrewer::brewer.pal(n = 9, name = "RdBu")
  if (r > 0.7) {
    corCol <- corColors[1]
  } else if (r > 0.5) {
    corCol <- corColors[2]
  } else if (r > 0.3) {
    corCol <- corColors[3]
  } else if (r > 0.1) {
    corCol <- corColors[4]
  } else if (r > -0.1) {
    corCol <- corColors[5]
  } else if (r > -0.3) {
    corCol <- corColors[6]
  } else if (r > -0.5) {
    corCol <- corColors[7]
  } else if (r > -0.7) {
    corCol <- corColors[8]
  } else {
    corCol <- corColors[9]
  }
  rect(xleft = 0, ybottom = 0, xright = 1, ytop = 1, col = corCol)
  if(abs(r)<0.65){
    size_n = 1.1
  }else{
    size_n = 1.8*abs(r)
  }
  text(0.5, 0.5, r, cex = size_n)
}

data_s11 <- data_std
names(data_s11)[c(9,10)] <- c("CL\nGreen", "CLRed\nEdge")
data_11 <- data_s11[,-c(1,25, 26)]
col_order <- c("Biomass","Area", "Vol",
               "Height",
               "GRVI", "NGRDI", "VARI", "GRBVI", "GLI",
               "NDVI","WDRVI", "TVI", "EVI2", "LCI",
               "RVI","MCARI", "CVI", "CL\nGreen", "GNDVI",
               "SR", "CLRed\nEdge","NDRE","EXG")

data_s11 <- data_s11[, col_order]

pairs(data_s11,
      upper.panel=panel_upper,
      lower.panel=panel_lower, cex.labels = 1)

## bands
panel_upper2 <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <-round(cor(x, y),2)
  corColors <- RColorBrewer::brewer.pal(n = 9, name = "RdBu")
  if (r > 0.7) {
    corCol <- corColors[1]
  } else if (r > 0.5) {
    corCol <- corColors[2]
  } else if (r > 0.3) {
    corCol <- corColors[3]
  } else if (r > 0.1) {
    corCol <- corColors[4]
  } else if (r > -0.1) {
    corCol <- corColors[5]
  } else if (r > -0.3) {
    corCol <- corColors[6]
  } else if (r > -0.5) {
    corCol <- corColors[7]
  } else if (r > -0.7) {
    corCol <- corColors[8]
  } else {
    corCol <- corColors[9]
  }
  rect(xleft = 0, ybottom = 0, xright = 1, ytop = 1, col = corCol)
  if(abs(r)<0.65){
    size_n = 2.2
  }else{
    size_n = 4*abs(r)
  }
  text(0.5, 0.5, r, cex = size_n)
}

names(data_s7_2)[4] <- "Red edge"
pairs(data_s7_2,
      upper.panel=panel_upper2,
      lower.panel=panel_lower, cex.labels = 4)
