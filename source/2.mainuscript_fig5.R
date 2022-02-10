#.............................................................................................................#
#............................................. Fig5 Validation of Prediction .................................#
#.............................................................................................................#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~ organize test data ~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
data_br <- read.table(file = "../Processing/right/biomass_right_sept.txt", sep = "\t", header = TRUE)
data_br <- na.omit(data_br)
data_b_s <- data.frame(id = data_br$id, Biomass = data_br$Sept, mon = "Sept")
data_b <- rbind(data_b_s)

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

### Sept
sept_1 <- org_data("../Processing/right/Version06/rep1_data.csv",reps = "1_", rp="Rep_1",mons = "Sept")
sept_1 <- na.omit(sept_1)
sept_2 <- org_data("../Processing/right/Version06/rep2_data.csv",reps = "2_", rp="Rep_2",mons = "Sept")
sept_2 <- na.omit(sept_2)
sept_3 <- org_data("../Processing/right/Version06/rep3_data.csv",reps = "3_", rp="Rep_3",mons = "Sept")
sept_3 <- na.omit(sept_3)

### rbind
data_d <- rbind(sept_1,sept_2,sept_3)
data_d <- na.omit(data_d)

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
write.table(data, file = "NDVI_test_biomass&bands.csv", sep = ",", col=NA)
write.table(data_f, file = "NDVI_test_biomass&features.csv", sep = ",", col=NA)
write.table(data_std, file = "NDVI_test_biomass&features_standarliseWithinMonRep.csv", sep = ",", col = NA)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~ validation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
## train data
train_data2 <- read.table(file = "NDVI_biomass&features_standarliseWithinMonRep.csv", sep = ",", header = TRUE)
train_data2 <- train_data2[,-c(1,2)]
# remove the rep3 of Sept from trainning data
train_data2$mon_rep <- paste0(train_data2$Month,"_", train_data2$Replication)
train_data_del <- train_data2[train_data2$mon_rep != "Sept_Rep_3", ]

## test data
test_data2<- read.table(file = "NDVI_test_biomass&features_standarliseWithinMonRep.csv", sep = ",", header = TRUE)
test_data2 <- test_data2[,-c(1,2)]

lmModel2 <- train(Biomass ~ Area + Height + NGRDI + NDRE, data=train_data_del, method = "lm")
summary(lmModel2)
preY2 <- predict(lmModel2, test_data2)
test_data2$Prediction <- preY2


## plot
thm_test <- ttheme_minimal(base_size = 18, colhead= list(padding=unit.c(unit(3.5, "mm"), unit(3.5, "mm"))))
### get r_square
test_data2_cors <- ddply(test_data2, .(Replication), summarize, R_square = round(cor(Biomass, Prediction)^2, digits = 2))
test_data2$Replication <- factor(test_data2$Replication, levels = c("Rep_1", "Rep_2", "Rep_3")) # reorder the legend
### get Spearman correlation coefficients
Spearman <- ddply(test_data2, .(Replication), summarize, Spearman = round(cor(Biomass, Prediction,method = "spearman")^2, digits = 2))
test_data2_cors$Spearman <- Spearman$Spearman
### get RMSE
test_data2_cors$RMSE <- test_data2_RMSE$RMSE


test_data2_plot <- ggscatter(test_data2, x="Prediction", y="Biomass", color = "Replication",shape = 1, alpha = 0.8, legend="none", size = 2)+
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 26),
        plot.title = element_text(hjust = 0.5, size = 22))+
  scale_shape(solid = FALSE) + #make the solid dot into a empty dot
  scale_color_manual(values = c("red", "Blue", "Green")) +
  annotation_custom(tableGrob(test_data2_cors, rows = NULL, theme = thm_test), xmin = 1.5, xmax = 3.5, ymin = -1, ymax = -2)+
  ylab("Observed biomass") + 
  xlab("Predicted biomass")
# annotate("text",x=-2,y=5.5,,parse=TRUE,label=paste("R^2==",MLR_3fd_cor,sep = ""),size=5,hjust=0)

test_data2_plot

#.............................................................................................................#
#................................................... FigS4-S6 ................................................#
#.............................................................................................................#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~ get prediction for train and test data ~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## train data
train_data <- read.table(file = "NDVI_biomass&features_standarliseWithinMonRep.csv", sep = ",", header = TRUE)
train_data <- train_data[,-1]

# remove the rep3 of Sept from trainning data
train_data_del <- train_data
train_data_del$mon_rep <- paste0(train_data_del$Month,"_", train_data_del$Replication)
train_data_del <- train_data_del[train_data_del$mon_rep != "Sept_Rep_3", ]

## test data
test_data<- read.table(file = "NDVI_test_biomass&features_standarliseWithinMonRep.csv", sep = ",", header = TRUE)
test_data <- test_data[,-c(1)]

## prediction in train_data
lmModel <- train(Biomass ~ Area + Height + NDRE + NGRDI, data=train_data, method = "lm")
preY <- predict(lmModel, train_data)
train_data$Prediction <- preY
write.table(train_data, file = "NDVI_biomass&features_standarliseWithinMonRep&predication.csv", sep = ",", col = NA)

## prediction in test_data
lmModel2 <- train(Biomass ~ Area + Height + NDRE + NGRDI, data=train_data_del, method = "lm")
preY2 <- predict(lmModel2, test_data)
test_data$Prediction <- preY2
write.table(test_data, file = "NDVI_test_biomass&features_standarliseWithinMonRep&predication.csv", sep = ",", col = NA)


a <- read.table(file = "NDVI_biomass&bands.csv", sep = ",", header = T)
names(a)[2] <- "Id"
r_ht <- merge(train_data, a, by=c("Id","Month", "Replication"))
head(r_ht)
out_data <- data.frame(id = r_ht$Id, month = r_ht$Month, rep = r_ht$Replication, 
                       biomass = r_ht$Biomass.y, biomass_std = r_ht$Biomass.x, biomass_pre = r_ht$Prediction,
                       height = r_ht$ch_5, height_std = r_ht$Height)
write.table(out_data, file = "../Result/Version10/heatmap/first/biomass&height_first.csv", sep = ",", col=NA)

a <- read.table(file = "NDVI_Test_biomass&bands.csv", sep = ",", header = T)
names(a)[2] <- "Id"
r_ht <- merge(test_data, a, by=c("Id","Month", "Replication"))
head(r_ht)
out_data <- data.frame(id = r_ht$Id, month = r_ht$Month, rep = r_ht$Replication, 
                       biomass = r_ht$Biomass.y, biomass_std = r_ht$Biomass.x, biomass_pre = r_ht$Prediction,
                       height = r_ht$ch_5, height_std = r_ht$Height)
write.table(out_data, file = "../Result/Version10/heatmap/second/biomass&height_second.csv", sep = ",", col=NA)





data2map <- function(data, map, rep, mon, cs){
  r_m <- dim(map)[1]
  c_m <- dim(map)[2]
  
  r_d <- dim(data)[1]
  c_d <- dim(data)[2]
  
  out <- matrix(nrow = r_m, ncol = c_m)
  
  for (i in 1:r_m) {
    for (j in 1:c_m) {
      if(!is.na(map[i,j]) & !map[i,j] =="" ){
        id_m <- paste0(rep,"_",as.character(map[i,j]))
        for(k in 1:r_d){
          if (id_m == data[k,1] & data[k,2] == mon & data[k,3] == paste0("Rep_", rep) ){
            out[i,j] <- data[k,cs]
          }
        }
      }
    }
  }
  out
}

c_data <- read.table(file = "../Result/Version10/heatmap/first/biomass&height_first.csv", sep = ",",header = T)
c_data <- c_data[,-1]
map_1 <- read.table(file = "../Result/Version10/heatmap/first/map_1.txt", sep = "\t", header = FALSE)
map_2 <- read.table(file = "../Result/Version10/heatmap/first/map_2.txt", sep = "\t", header = FALSE)
map_3 <- read.table(file = "../Result/Version10/heatmap/first/map_3.txt", sep = "\t", header = FALSE)

may_1 <- data2map(data = c_data, map = map_1, rep = 1, mon = "May", cs =7)
may_2 <- data2map(data = c_data, map = map_2, rep = 2, mon = "May", cs =7)
may_3 <- data2map(data = c_data, map = map_3, rep = 3, mon = "May", cs =7)

july_1 <- data2map(data = c_data, map = map_1, rep = 1, mon = "July", cs =7)
july_2 <- data2map(data = c_data, map = map_2, rep = 2, mon = "July", cs =7)
july_3 <- data2map(data = c_data, map = map_3, rep = 3, mon = "July", cs =7)

sept_1 <- data2map(data = c_data, map = map_1, rep = 1, mon = "Sept", cs =7)
sept_2 <- data2map(data = c_data, map = map_2, rep = 2, mon = "Sept", cs =7)
sept_3 <- data2map(data = c_data, map = map_3, rep = 3, mon = "Sept", cs =7)

a <- rbind(may_1, may_2, may_3, july_1, july_2, july_3, sept_1, sept_2, sept_3)
write.table(a, file ="../Result/Version10/heatmap/first/height.csv", sep = ",", col = NA)

may_1 <- data2map(data = c_data, map = map_1, rep = 1, mon = "May", cs =6)
may_2 <- data2map(data = c_data, map = map_2, rep = 2, mon = "May", cs =6)
may_3 <- data2map(data = c_data, map = map_3, rep = 3, mon = "May", cs =6)

july_1 <- data2map(data = c_data, map = map_1, rep = 1, mon = "July", cs =6)
july_2 <- data2map(data = c_data, map = map_2, rep = 2, mon = "July", cs =6)
july_3 <- data2map(data = c_data, map = map_3, rep = 3, mon = "July", cs =6)

sept_1 <- data2map(data = c_data, map = map_1, rep = 1, mon = "Sept", cs =6)
sept_2 <- data2map(data = c_data, map = map_2, rep = 2, mon = "Sept", cs =6)
sept_3 <- data2map(data = c_data, map = map_3, rep = 3, mon = "Sept", cs =6)

a <- rbind(may_1, may_2, may_3, july_1, july_2, july_3, sept_1, sept_2, sept_3)
write.table(a, file ="../Result/Version10/heatmap/first/biomassPredic.csv", sep = ",", col = NA)


c_data <- read.table(file = "../Result/Version10/heatmap/second/biomass&height_second.csv", sep = ",",header = T)
c_data <- c_data[,-1]
map_1 <- read.table(file = "../Result/Version10/heatmap/second/map_1.txt", sep = "\t", header = FALSE)
map_2 <- read.table(file = "../Result/Version10/heatmap/second/map_2.txt", sep = "\t", header = FALSE)
map_3 <- read.table(file = "../Result/Version10/heatmap/second/map_3.txt", sep = "\t", header = FALSE)
sept_1 <- data2map(data = c_data, map = map_1, rep = 1, mon = "Sept", cs =4)
sept_2 <- data2map(data = c_data, map = map_2, rep = 2, mon = "Sept", cs =4)
sept_3 <- data2map(data = c_data, map = map_3, rep = 3, mon = "Sept", cs =4)
a <- rbind(sept_1, sept_2, sept_3)
write.table(a, file ="../Result/Version10/heatmap/second/biomass.csv", sep = ",", col = NA)





