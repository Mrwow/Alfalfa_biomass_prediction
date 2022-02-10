library(dplyr)
data_1 <- read.csv("NDVI_test_biomass&bands.csv",sep = ",", header = TRUE)
colnames(data_1)[c(2,3)] <- c("Id","Month")
data_2 <- read.table("NDVI_test_biomass&features_standarliseWithinMonRep&predication.csv",sep = ",", header = TRUE)

data <- merge(data_1, data_2, by=c("Id"))

df1 <- data[c("Id","rep","Biomass.x","Biomass.y","Prediction")]
cor(df1$Biomass.y, df1$Prediction)^2
# get the mean and st of raw biomass by replicate
df2 <- ddply(df1, .(rep), summarize,  Avg=mean(Biomass.x), Std=sd(Biomass.x))
# use the prediction to multiply the std and plus the mean of ras biomass
df_a <- df1[df1[,"rep"] == df2[1,1], "Prediction"] * df2[1,3] + df2[1,2]
df_b <- df1[df1[,"rep"] == df2[2,1], "Prediction"] * df2[2,3] + df2[2,2]
df_c <- df1[df1[,"rep"] == df2[3,1], "Prediction"] * df2[3,3] + df2[3,2]
df_Pre <- c(df_a,df_b, df_c)
df1$Pre_raw <- df_Pre
# get RMSE by replicate
# test_data2_RMSE <- ddply(df1, .(rep), summarize, RMSE = round(rmse(Biomass.x, Pre_raw), digits = 2))
# lb to kg
test_data2_RMSE <- ddply(df1, .(rep), summarize, RMSE = round(rmse(0.4536*Biomass.x, 0.4536*Pre_raw), digits = 2))

