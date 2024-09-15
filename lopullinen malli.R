library(ggplot2)
library(imputeTS)
library(TSA)
library(tsDyn)
library(lmtest)
library(tseries)
library(FinTS)
library(sandwich)
library(strucchange)
library(forecast)


# sea level data
sea_lvl <- ekman_2003_stockholm_3[, c("Year", "Avarage")]

# impute 
sea_lvl$Avarage <- na_ma(sea_lvl$Avarage, weighting = "exponential")

plot(sea_lvl)

# transform to ts object
x <- ts(sea_lvl$Avarage, start = 1, frequency =  1)

# plot time series 
plot(x)


# STAR model
star_mod <- star(x, m = 1, d = 1 ,mTh = c(1), control = list(maxit= 20000), starting.control = list(gammaInt = c(1, 200)))


summary(star_mod)

# LSTAR-model
lstar_mod <- lstar(x, m = 1, d = 1, mTh = c(1), control = list(maxit= 20000), starting.control = list(gammaInt = c(1, 500)))


summary(lstar_mod)

# Define the actual years corresponding to x
years <- seq(1774, 1774 + length(x) - 1, by = 1)

# Plot with custom x-axis
plot(years, x, type = "l", col = "blue", lwd = 2, main = "Merenkorkeus vs LSTAR-malli", 
     xlab = "Aika", ylab = "Merenkorkeus (m)")

# Add the fitted values
lines(years, fitted_values, col = "red", lwd = 2)

# Add a legend
legend("topright", legend = c("Merenkorkeus", "LSTAR malli"), col = c("blue", "red"), lty = 1, lwd = 2)


# residual diagnostics
checkresiduals(lstar_mod)

residuals_star <- lstar_mod$residuals

# Residual plot

residuals_plot <- ggplot(data.frame(x = 1:length(residuals_star), y = residuals_star), aes(x = x, y = y)) +
  geom_line() +
  labs(title = "Residuaalit", x = "", y = "") +
  theme_minimal()
ggsave("residuals_plot.png", residuals_plot)


acf_plot <- ggAcf(residuals_star, main = "Jäännösten ACF") +
  theme_minimal()
ggsave("acf_plot.png", acf_plot)


hist_plot <- ggplot(data.frame(residuals = residuals_star), aes(x = residuals_star)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "grey", color = "white") +
  geom_density(color = "red") +
  labs(title = "", x = "jäännökset", y = "tiheys") +
  theme_minimal()
ggsave("hist_plot.png", hist_plot)



# evaluation
arch_test <- ArchTest(residuals_star, lags=5)


shapiro_test <- shapiro.test(residuals_star)


bp_test <- bptest(residuals_star ~ lstar_mod$fitted.values)

adf_test <- adf.test(lstar_mod$residuals)


bg_test <- bgtest(residuals_star ~ lstar_mod$fitted.values)




# Residual plot
plot(lstar_mod$fitted.values, lstar_mod$residuals, main="Residual Plot", xlab="Fitted Values", ylab="Residuals")
abline(h=0, col="red")


# Tulosta testien tulokset
print(arch_test)
print(shapiro_test)
print(bp_test)
print(adf_test)
print(bg_test)
print(adf_test)

help(lstar)

AIC(lstar_mod)
BIC(lstar_mod)



# Split data
train <- window(x, start = 1, end = 240) 
test <- window(x, start = 241)     

# Fit model on training data
lstar_mod_train <- lstar(train, m = 1, d = 1, mTh = c(1), 
                       control = list(maxit = 20000), 
                       starting.control = list(gammaInt = c(1, 500)))


# Forecast and compare with test data
forecasts <- predict(lstar_mod_train, n.ahead = length(test))

accuracy(forecasts, test)

plot(train)
plot(forecasts)

# Create a combined data frame
combined_data <- data.frame(
  Aika = 1:length(x),
  Todellinen = as.numeric(x),
  Sovitettu = c(fitted(lstar_mod_train), rep(NA, length(test))),
  Ennuste = c(rep(NA, length(train)), forecasts)
)

combined_data$Aika <- 1773 + combined_data$Aika


ggplot(combined_data, aes(x = Aika)) +
  geom_line(aes(y = Todellinen, color = "Todellinen")) +
  geom_line(aes(y = Sovitettu, color = "Sovitettu")) +
  geom_line(aes(y = Ennuste, color = "Ennuste")) +
  labs(title = "Todelliset vs Sovitetut vs Ennustetut Arvot",
       x = "Vuosi",
       y = "Arvot") +
  scale_color_manual(name = "Selite", values = c("Todellinen" = "black", "Sovitettu" = "blue", "Ennuste" = "red")) +
  theme_minimal()

help("accuracy")
help(setar)
help(forecast)
help(shapiro.test)
help(checkresiduals)

