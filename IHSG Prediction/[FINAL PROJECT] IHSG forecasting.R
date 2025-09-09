#Input data
library(readr)
df <- read_csv("Data IHSG.csv", col_types = cols(Date = col_date(format = "%Y-%m-%d")))
View(df)

library(tseries)
library(forecast)
library(TTR)
library(TSA)
library(imputeTS)
library(ggplot2)
library(ggfortify)
library(lmtest)
library(ggpubr)

# Cek Missing value
sum(is.na(df$Close)) 

# Cek berdasarkan plot
ggplot(df, aes(x=Date, y=Close), x_axis_labels=df$Date)+
  labs(title = "Distribution of Missing Values",
       y="Nilai IHSG",
       subtitle = "Time Series with missing regions") +
  geom_line() + geom_point() + theme_minimal() + 
  theme(legend.position="none", plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


#Plot data
plot_df <- ggplot(df, aes(x=Date, y=Close))+
  scale_x_date(date_breaks = "6 weeks", date_labels = "%Y %b") +
  labs(title = "Nilai Penutupan Indeks Harga Saham Gabungan (IHSG) Harian",
       y="Nilai IHSG",
       subtitle = "(Januari 2023 sd Januari 2024)") +
  geom_line()+
  theme(plot.title =  element_text(face = "bold", hjust=.5),
        plot.subtitle = element_text(hjust=.5))
plot_df

#Cek Variansi
lambda <- BoxCox.lambda(df$Close)
print(lambda)

#Splitting data
p = 0.825 #proporsi data training yang dipilih
freq_train=as.integer(p*nrow(df))
train <- 1:freq_train
y_lin <- df$Close
df_train <- ts(y_lin[train])
df_test <- ts(y_lin[-train], start = freq_train+1)
ggplot(df[train,], aes(x=Date, y=Close)) +
  geom_line() +
  scale_x_date(date_breaks = "6 weeks", date_labels = "%Y %b") +
  labs(title = "Plot Time Series Data Training",
       subtitle = "(Januari 2023 sd Januari 2024)",
       y="Harga IHSG") +
  theme(plot.title =  element_text(face = "bold", hjust=.5),
        plot.subtitle = element_text(hjust=.5))
ggplot(df[-train,], aes(x=Date, y=Close)) +
  geom_line() +
  scale_x_date(date_breaks = "6 weeks", date_labels = "%Y %b") +
  labs(title = "Plot Time Series Data Testing",
       subtitle = "(Januari 2023 sd Januari 2024)",
       y="Harga IHSG") +
  theme(plot.title =  element_text(face = "bold", hjust=.5),
        plot.subtitle = element_text(hjust=.5))
#kapan perpotongan data terjadi
df[freq_train,1]

  
#Identifikasi Stasioner
plot.ts(df$Close, ylab=expression(Y[t]), main = "Plot Time Series Data IHSG harian")
acf(df$Close, main="Series Data IHSG harian")

#Uji Formal
adf.test(df$Close)

#Differencing
df_diff <- diff(df$Close, differences = 1)
adf.test(df_diff)


#Pendugaan Parameter
acf(df_diff)
pacf(df_diff)
eacf(df_diff)

#MODELLING
model1 <- auto.arima(df_train, lambda = 0)
model1

model2 <- Arima(df_train, order=c(1,1,1), lambda = 0, method="ML")
summary(model2)

model3<- Arima(df_train, order=c(0,1,1), lambda = 0, method="ML")
summary(model3)

model4<- Arima(df_train, order=c(0,1,2), lambda = 0, method="ML")
summary(model4)

model5<- Arima(df_train, order=c(1,1,2), lambda = 0, method="ML")
summary(model5)

model6<- Arima(df_train, order=c(2,1,2), lambda = 0, method="ML")
summary(model6)

#Cek keakuratan Model
akurasi_model = data.frame("Model" = c("ARIMA(3,1,1)", "ARIMA(1,1,1)", "ARIMA(0,1,1)", "ARIMA(0,1,2)", "ARIMA(1,1,2)", "ARIMA(2,1,2)"),
                           "AIC" = c(model1$aic, model2$aic, model3$aic, model4$aic, model5$aic, model6$aic))
akurasi_model[order(akurasi_model[,2]),]

#Overfitting 
model6a<- Arima(df_train, order=c(3,1,2), lambda = 0, method="ML")
summary(model6a)

model6b<- Arima(df_train, order=c(2,1,3), lambda = 0, method="ML")
summary(model6b)

#Cek keakuratan Model overfitting
akurasi_model = data.frame("Model" = c("ARIMA(2,1,2)", "ARIMA(3,1,2)", "ARIMA(2,1,3)"),
                           "AIC" = c(model6$aic, model6a$aic, model6b$aic))
akurasi_model[order(akurasi_model[,2]),]

#Eksplorasi residual
residual <- model6$residuals
par(mfrow=c(2,2))
qqnorm(residual)
qqline(residual, col = "blue", lwd = 2)
plot(as.numeric(model6$fitted), as.numeric(residual), xlab="Fitted", ylab="residual", main="Residuals vs Fitted"); abline(h=0, col="red")
acf(residual, lag.max = 20)
plot(1:length(residual),residual,type='o', xlab="Order",
     main="Residuals vs Order"); abline(h=0, col="red")


#UJI FORMAL
### L-Jung Box Test

#H_0: residual saling bebas (tidak terdapat korelasi)

#H_1: residual tidak saling bebas (terdapat korelasi)

Box.test(residual,type = "Ljung-Box")

### T-Test

#H_0: Rataan residual sama dengan 0

#H_1: Rataan residual tidak sama dengan 0

t.test(residual, mu = 0, conf.level = 0.95)

### Box-Cox Test

BoxCox.lambda(residual)


# Validasi Model

## Plot
autoplot(cbind(df_train, model6$fitted))
forecasting <- forecast(model6, h = 65)
accuracy(forecasting)

#Nilai akurasi
m_test <- Arima(df_test, model = model6)
autoplot(cbind(df_test, m_test$fitted))
accuracy(m_test)

# Load necessary libraries
library(forecast)
library(ggplot2)

# Gabungkan data training dan testing
data_keseluruhan <- c(df_train, df_test)

# Bangun model ARIMA(2,1,2) pada data keseluruhan
model_arima <- Arima(data_keseluruhan, order=c(2,1,2), lambda = 0, method="ML")

# Lakukan prediksi (misalnya untuk 10 periode ke depan)
n_pred <- 10
future_forecast <- forecast(model_arima, h = n_pred)

# Menampilkan nilai prediksi secara rinci
prediksi_nilai <- data.frame(
  Time = time(future_forecast$mean),
  Predicted = as.numeric(future_forecast$mean),
  Lower_80 = as.numeric(future_forecast$lower[,1]),
  Upper_80 = as.numeric(future_forecast$upper[,1]),
  Lower_95 = as.numeric(future_forecast$lower[,2]),
  Upper_95 = as.numeric(future_forecast$upper[,2])
)

# Print nilai prediksi secara rinci
print(prediksi_nilai)


# Menampilkan nilai prediksi secara rinci
prediksi_nilai <- data.frame(
  Time = time(future_forecast$mean),
  Predicted = as.numeric(future_forecast$mean),
  Lower_80 = as.numeric(future_forecast$lower[,1]),
  Upper_80 = as.numeric(future_forecast$upper[,1]),
  Lower_95 = as.numeric(future_forecast$lower[,2]),
  Upper_95 = as.numeric(future_forecast$upper[,2])
)

# Print nilai prediksi secara rinci
print(prediksi_nilai)

# Membuat plot untuk prediksi 10 hari ke depan
autoplot(future_forecast) +
  ggtitle("Forecast 10 Hari ke Depan") +
  xlab("Waktu") +
  ylab("Nilai Prediksi") +
  theme_minimal()

# Plot nilai prediksi beserta interval kepercayaan
ggplot(prediksi_nilai, aes(x = Time)) +
  geom_ribbon(aes(ymin = Lower_95, ymax = Upper_95), fill = "blue", alpha = 0.2) +
  geom_ribbon(aes(ymin = Lower_80, ymax = Upper_80), fill = "blue", alpha = 0.4) +
  geom_line(aes(y = Predicted), color = "blue", size = 1) +
  ggtitle("Prediksi 10 Hari ke Depan dengan Interval Kepercayaan") +
  xlab("Waktu") +
  ylab("Nilai Prediksi") +
  theme_minimal()
