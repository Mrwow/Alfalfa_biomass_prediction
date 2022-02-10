#.............................................................................................................#
#............................................. Fig4 Prediction accuracy ......................................#
#.............................................................................................................#

prediction_MLR <- function(ds, ds2, ds3){
  trainData <- rbind(ds, ds2)
  testData  <- ds3
  lmModel <- train(Biomass ~ Area + Height + NDRE + NGRDI, data=trainData, method = "lm")
  preY <- predict(lmModel, testData)
  testData$Prediction <- preY
  out <- testData
  coefs <- summary(lmModel)[[4]][,1]
  
  trainData <- rbind(ds, ds3)
  testData  <- ds2
  lmModel <- train(Biomass ~ Area + Height + NDRE + NGRDI, data=trainData, method = "lm")
  preY <- predict(lmModel, testData)
  testData$Prediction <- preY
  out <- rbind(out, testData)
  coefs <- cbind(coefs, summary(lmModel)[[4]][,1])
  
  trainData <- rbind(ds2, ds3)
  testData  <- ds
  lmModel <- train(Biomass ~ Area + Height + NDRE + NGRDI, data=trainData, method = "lm")
  preY <- predict(lmModel, testData)
  testData$Prediction <- preY
  out <- rbind(out, testData)
  coefs <- cbind(coefs, summary(lmModel)[[4]][,1])
  
  list(out, coefs)
}

set.seed(1231)
thm <- ttheme_minimal(base_size = 15, colhead= list(padding=unit.c(unit(1, "mm"), unit(5, "mm"))))
data_may <- data_std[data_std$Month == "May", ]
data_july <- data_std[data_std$Month == "July", ]
data_sep <- data_std[data_std$Month == "Sept", ]

MLR_mon_outs <- prediction_MLR(ds=data_may, ds2 = data_july, ds3 = data_sep)
MLR_mon <- MLR_mon_outs[[1]]
MLR_mon$Month <- factor(MLR_mon$Month, levels = c("May", "July", "Sept")) # reorder the legend
MLR_mon_coef <- MLR_mon_outs[[2]]
MLR_mon_cor <-  round(cor(MLR_mon$Biomass, MLR_mon$Prediction)^2, digits = 3)
MLR_mon_cors <- ddply(MLR_mon, .(Month, Replication), summarize, R_squar = round(cor(Biomass, Prediction)^2, digits = 2))
MLR_mon_table <- spread(MLR_mon_cors, Month, R_squar)

MLR_mon_plot <- ggscatter(MLR_mon, x="Prediction", y="Biomass", color = "Month", shape = "Replication", alpha = 0.8, legend="none", size = 1)+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, size = 22)) + 
  scale_shape(solid = FALSE) +
  scale_color_manual(values = c("Blue", "Green", "red"))+
  scale_shape_manual(values = c(1, 0, 2))+
  annotation_custom(tableGrob(MLR_mon_table, rows = NULL, theme = thm), xmin = 2.5, xmax = 4, ymin = -4.5, ymax = -1)+
  ylab("Observed biomass") + 
  xlab("Predicted biomass")

set1 <- MLR_mon[(MLR_mon$Month == "July" & MLR_mon$Replication == "Rep_1"), ]
set2 <- MLR_mon[(MLR_mon$Month == "May" & MLR_mon$Replication == "Rep_2"), ]
set3 <- MLR_mon[(MLR_mon$Month == "Sept" & MLR_mon$Replication == "Rep_1"), ]
set4 <- MLR_mon[(MLR_mon$Month == "July" & MLR_mon$Replication == "Rep_2"), ]
set5 <- MLR_mon[(MLR_mon$Month == "May" & MLR_mon$Replication == "Rep_3"), ]

MLR_mon_plot <- MLR_mon_plot + 
  geom_point(data = set5, aes(x=Prediction, y=Biomass), size=1, color = "blue", shape = 2, alpha = 0.8) +
  geom_point(data = set4, aes(x=Prediction, y=Biomass), size=1, color = "green", shape = 0, alpha = 0.8) +
  geom_point(data = set3, aes(x=Prediction, y=Biomass), size=1, color = "red", shape = 1, alpha = 0.8) +
  geom_point(data = set2, aes(x=Prediction, y=Biomass), size=1, color = "blue", shape = 0, alpha = 0.8) +
  geom_point(data = set1, aes(x=Prediction, y=Biomass), size=1, color = "green", shape = 1, alpha = 0.8)+
  xlim(-5.5, 5.5)

MLR_mon_plot

## MLR, data split by rep
set.seed(1232)
data_rep1 <- data_std[data_std$Replication == "Rep_1", ]
data_rep2 <- data_std[data_std$Replication == "Rep_2", ]
data_rep3 <- data_std[data_std$Replication == "Rep_3", ]

MLR_rep_outs <- prediction_MLR(ds=data_rep1, ds2 = data_rep2, ds3 = data_rep3)
MLR_rep <- MLR_rep_outs[[1]]
MLR_rep$Month <- factor(MLR_rep$Month, levels = c("May", "July", "Sept")) # reorder the legend
MLR_rep_coef <- MLR_rep_outs[[2]]
MLR_rep_cor <-  round(cor(MLR_rep$Biomass, MLR_rep$Prediction)^2, digits = 3)
MLR_rep_cors <- ddply(MLR_rep, .(Month, Replication), summarize, R_squar = round(cor(Biomass, Prediction)^2, digits = 2))
MLR_rep_table <- spread(MLR_rep_cors, Month, R_squar)
MLR_rep_table


MLR_rep_plot <-  ggscatter(MLR_rep, x="Prediction", y="Biomass", color = "Month", shape = "Replication", alpha = 0.8, legend="none", size = 1)+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, size = 22)) + 
  scale_shape(solid = FALSE) + #make the solid dot into a empty dot
  scale_color_manual(values = c("Blue", "Green", "red")) +  # manually change the color of legend
  scale_shape_manual(values = c(1, 0, 2))+
  annotation_custom(tableGrob(MLR_rep_table, rows = NULL, theme = thm), xmin = 2.5, xmax = 4, ymin = -4.5, ymax = -1)+
  ylab("Observed biomass") + 
  xlab("Predicted biomass")

set1 <- MLR_rep[(MLR_rep$Month == "July" & MLR_rep$Replication == "Rep_1"), ]
set2 <- MLR_rep[(MLR_rep$Month == "May" & MLR_rep$Replication == "Rep_2"), ]
set3 <- MLR_rep[(MLR_rep$Month == "Sept" & MLR_rep$Replication == "Rep_1"), ]
set4 <- MLR_rep[(MLR_rep$Month == "July" & MLR_rep$Replication == "Rep_2"), ]
set5 <- MLR_rep[(MLR_rep$Month == "May" & MLR_rep$Replication == "Rep_3"), ]

MLR_rep_plot  <- MLR_rep_plot +
  geom_point(data = set5, aes(x=Prediction, y=Biomass), size=1, color = "blue", shape = 2, alpha = 0.8) +
  geom_point(data = set4, aes(x=Prediction, y=Biomass), size=1, color = "green", shape = 0, alpha = 0.8) +
  geom_point(data = set3, aes(x=Prediction, y=Biomass), size=1, color = "red", shape = 1, alpha = 0.8) +
  geom_point(data = set2, aes(x=Prediction, y=Biomass), size=1, color = "blue", shape = 0, alpha = 0.8) +
  geom_point(data = set1, aes(x=Prediction, y=Biomass), size=1, color = "green", shape = 1, alpha = 0.8)+
  xlim(-5.5, 5.5)

MLR_rep_plot

## MLR, data split by random 3 fold
set.seed(1234)
flds <- createFolds(data_std$Biomass, k=3, list = TRUE, returnTrain = FALSE)
data_f1 <- data_std[flds$Fold1, ]
data_f2 <- data_std[flds$Fold2, ]
data_f3 <- data_std[flds$Fold3, ]

MLR_3fd_outs <- prediction_MLR(ds=data_f1, ds2 = data_f2, ds3 = data_f3)
MLR_3fd <- MLR_3fd_outs[[1]]
MLR_3fd$Month <- factor(MLR_3fd$Month, levels = c("May", "July", "Sept")) # reorder the legend
MLR_3fd_coef <- MLR_3fd_outs[[2]]
MLR_3fd_cor <-  round(cor(MLR_3fd$Biomass, MLR_3fd$Prediction)^2, digits = 3)
MLR_3fd_cors <- ddply(MLR_3fd, .(Month, Replication), summarize, R_squar = round(cor(Biomass, Prediction)^2, digits = 2))
MLR_3fd_table <- spread(MLR_3fd_cors, Month, R_squar)

MLR_3fd_plot <- ggscatter(MLR_3fd, x="Prediction", y="Biomass", color = "Month", shape = "Replication", alpha = 0.8, legend="none", size = 1)+
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, size = 22)) + 
  scale_shape(solid = FALSE) + #make the solid dot into a empty dot
  scale_color_manual(values = c("Blue", "Green", "red"))+
  scale_shape_manual(values = c(1, 0, 2))+
  annotation_custom(tableGrob(MLR_3fd_table, rows = NULL, theme = thm), xmin = 2.5, xmax = 4, ymin = -4.5, ymax = -1)+
  ylab("Observed biomass") + 
  xlab("Predicted biomass")

set1 <- MLR_3fd[(MLR_3fd$Month == "July" & MLR_3fd$Replication == "Rep_1"), ]
set2 <- MLR_3fd[(MLR_3fd$Month == "May" & MLR_3fd$Replication == "Rep_2"), ]
set3 <- MLR_3fd[(MLR_3fd$Month == "Sept" & MLR_3fd$Replication == "Rep_1"), ]
set4 <- MLR_3fd[(MLR_3fd$Month == "July" & MLR_3fd$Replication == "Rep_2"), ]
set5 <- MLR_3fd[(MLR_3fd$Month == "May" & MLR_3fd$Replication == "Rep_3"), ]

MLR_3fd_plot  <- MLR_3fd_plot +
  geom_point(data = set5, aes(x=Prediction, y=Biomass), size=1, color = "blue", shape = 2, alpha = 0.8) +
  geom_point(data = set4, aes(x=Prediction, y=Biomass), size=1, color = "green", shape = 0, alpha = 0.8) +
  geom_point(data = set3, aes(x=Prediction, y=Biomass), size=1, color = "red", shape = 1, alpha = 0.8) +
  geom_point(data = set2, aes(x=Prediction, y=Biomass), size=1, color = "blue", shape = 0, alpha = 0.8) +
  geom_point(data = set1, aes(x=Prediction, y=Biomass), size=1, color = "green", shape = 1, alpha = 0.8)+
  xlim(-5.5, 5.5)

MLR_3fd_plot

set.seed(12345)
MLR_3fd_rs_box <- list()
MLR_3fd_rs_coef <- matrix(nrow = 5, ncol = 0)
for (i in 1:100){
  # idxSample = sample(1:dim(data_std)[1])
  # flds = list(Fold1=idxSample[1:100], Fold2=idxSample[1500:1600], Fold3 = idxSample[2200:2300]) 
  flds <- createFolds(data_std$Biomass, k=3, list = TRUE, returnTrain = FALSE)
  data_f1 <- data_std[flds$Fold1,]
  data_f2 <- data_std[flds$Fold2,]
  data_f3 <- data_std[flds$Fold3,]
  
  MLR_3fd_rs_outs <- prediction_MLR(ds=data_f1, ds2 = data_f2, ds3 = data_f3)
  a <- MLR_3fd_rs_outs[[1]]
  b <- ddply(a, .(Month, Replication), summarize, R_square = round(cor(Biomass, Prediction)^2, digits = 3))
  MLR_3fd_rs_box <- rbind(MLR_3fd_rs_box, b)
  c <- MLR_3fd_rs_outs[[2]]
  MLR_3fd_rs_coef <- cbind(MLR_3fd_rs_coef, c)
}

MLR_3fd_rs_box$Month <- factor(MLR_3fd_rs_box$Month, levels = c("May", "July", "Sept"))
MLR_3fd_rs_box$Replication <- factor(MLR_3fd_rs_box$Replication, levels = c("Rep_1", "Rep_2", "Rep_3"))
MLR_3fd_rs_box_plot <- ggplot(MLR_3fd_rs_box, aes(x=Month, y=R_square, color = Replication)) + 
  ylab("Prediction Accuracy") +
  xlab("Month") +
  geom_boxplot() +
  scale_color_manual(values = c("orangered4", "orange", "olivedrab3")) +
  theme_classic() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.position = c(0.2, 0.2),
        legend.title=element_text(size=15, face="bold"),
        legend.text=element_text(size=15),
        axis.text = element_text(size = 16, colour = "black"),
        axis.title = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, size = 22),
        axis.line = element_line(colour = "black", size = 0.6, linetype = "solid"))

MLR_3fd_rs_box_plot

## combine 4 plot 
grid.arrange(MLR_mon_plot, MLR_rep_plot, MLR_3fd_plot, MLR_3fd_rs_box_plot, ncol=2)

colnames(MLR_mon_coef) = c("May_July", "May_Sept", "July_Sept")
write.table(t(MLR_mon_coef),"../Result/Version11/coefficient summary by month.csv", sep = ",", col = NA)

colnames(MLR_rep_coef) = c("Rep1_2", "Rep1_3", "Rep2_3")
write.table(t(MLR_rep_coef),"../Result/Version11/coefficient summary by rep.csv", sep = ",", col = NA)

MLR_3fd_rs_coef_smry <- data.frame(mean = apply(MLR_3fd_rs_coef, 1, mean), std = apply(MLR_3fd_rs_coef, 1, sd))
write.table(t(MLR_3fd_rs_coef_smry),"../Result/Version11/coefficient summary by rd.csv", sep = ",", col = NA)

