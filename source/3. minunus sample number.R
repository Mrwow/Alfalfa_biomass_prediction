data <- read.table(file = "NDVI_biomass&features.csv", sep = ",", header = TRUE)
data$mon_rep <- paste0(data$Month,"_", data$Replication)

data_may_rep1 <- data[data$mon_rep == "May_Rep_1", ]

s <- sample(1:dim(data_may_rep1)[1],10)

train_data <- data_may_rep1[s, ]

test_data <- data_may_rep1[-s,]

lmModel <- train(Biomass ~ Area + Height + NGRDI + NDRE, data=train_data, method = "lm")
summary(lmModel)
preY <- predict(lmModel, test_data)
test_data$Prediction <- preY
cor(test_data$Biomass, test_data$Prediction)^2


library(glmnet)
a <- data_std[data_std$Month == "Sept" & data_std$Replication == "Rep_2", ]
a <- as.matrix(a[,2:24])
sr0.1 <- linear_modeling(data = a, sampling = 0.1, times = 20)
mean(sr0.1)


linear_modeling <- function(data, sampling, times){
  n <- dim(data)[1]
  co.list <- c()
  for(i in 1:times){
    train_rows <- sample(1:n, sampling*n)
    x.train <- a[train_rows, 2:23]
    y.train <- a[train_rows, 1]
    
    x.test <- a[-train_rows, 2:23]
    y.test <- a[-train_rows, 1]
    alpha1.fit <- cv.glmnet(x.train, y.train, type.measure = "mse", alpha= 1, family = "gaussian", nfolds = 3)
    alpha1.predicted <- predict(alpha1.fit, s=alpha1.fit$lambda.1se, newx = x.test)
    co <- cor(y.test, alpha1.predicted)^2
    if(!is.na(co)){
      co.list <- cbind(co.list, co)
    }
  }
  co.list
}

sr0.2 <- linear_modeling(data = a, sampling = 0.2, times = 20)
sr0.3 <- linear_modeling(data = a, sampling = 0.3, times = 20)
sr0.4 <- linear_modeling(data = a, sampling = 0.4, times = 20)
sr0.5 <- linear_modeling(data = a, sampling = 0.5, times = 20)
sr0.6 <- linear_modeling(data = a, sampling = 0.6, times = 20)

data_box <- data.frame(sr0.1, sr0.2, sr0.3, sr0.4, sr0.5, sr0.6)
head(data_box)

data_box_plot <- ggplot(data_box) + 
  geom_boxplot() 
data_box_plot 

mean(sr0.1)


n <- dim(a)[1]
co.list <- list()
for (i in 1:20){
  train_rows <- sample(1:n, 0.1*n)
  x.train <- a[train_rows, 2:23]
  y.train <- a[train_rows, 1]
  
  x.test <- a[-train_rows, 2:23]
  y.test <- a[-train_rows, 1]
  
  alpha1.fit <- cv.glmnet(x.train, y.train, type.measure = "mse", alpha= 1, family = "gaussian", nfolds = 3)
  alpha1.predicted <- predict(alpha1.fit, s=alpha1.fit$lambda.1se, newx = x.test)
  co <- cor(y.test, alpha1.predicted)^2
}




