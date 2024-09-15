
# Asetetaan ty?hakemisto SET Working Directory
# Muokkaa tarvittaessa oman ty?hakemistosi polku
# RStudiossa t?m?n voi tehd? my?s valikosta Session->Set working directory

# Luetaan s?hk?nkulutus- ja l?mp?tiladata, hyp?t??n headerrivin yli
eletemp = read.table(file = "sahko.csv",
                     sep = ";",
                     dec = ",",
                     skip = 1,
                     col.names = c('kWh','Celcius'))

# Sähkönkulutus aikasarjaksi
ele = ts(eletemp$kWh[1:816], start = 1, frequency = 24)

# Lämpötila kahdeksi aikasarjaksi: 816 ensimmäistä havaintoa käytetään mallin estimointiin
# ja 24 viimeistä havaintoa ennustamiseen.
temp = ts(eletemp$Celcius, start = 1, frequency = 24)
temp816 = ts(eletemp$Celcius[1:816], start = 1, frequency = 24)
# start parametrina vektori: 817. tunti = 35. päivän ensimmäinen tunti
temp24 = ts(eletemp$Celcius[817:840], start = c(35,1), frequency = 24)

# Plotataan aikasarjat
ts.plot(ele,
        xlab = "aika/vrk",
        ylab = "kulutus/kWh")
ts.plot(temp816,temp24,
        xlab = "aika/vrk",
        ylab = expression(~degree~C),
        col = c("black", "blue"))

# Määritellään 2x2 plottausruudukko.
par(mfrow=c(2,2))

# Plotataan autokorrelaatio-, osittaisautokorrelaatio- ja ristikorrelaatiofunktiot.
acf(ele, lag.max=168)
acf(ele, lag.max=168, type = "partial")
acf(temp, lag.max=168)
acf(temp, lag.max=168, type = "partial")
# Piirretään ristikorrelaatiofunktio omaan kuvaan
par(mfrow=c(1,1))
ccf(ele,temp, lag.max=168)

# Stationarisoidaan aikasarjat. Määrittele parametrit d,S,D
d = 1 # Differoinnin kertaluku d
S = 24 # Kausidifferoinnin jakso S
D = 1 # Kausidifferensoinnin kertaluku D
S2 = 168  # viikottainen vaihtelu

dele = ele
dtemp = temp816
if (d > 0) {
  dele = diff(dele, lag = 1, differences = d)
  dtemp = diff(dtemp, lag = 1, differences = d)
}
if (D > 0) {
  dele = diff(dele, lag = S2, differences = D)
  dtemp = diff(dtemp, lag = S, differences = D)
}

# Differoitujen aikasarjojen autokorrelaatio-, osittaisautokorrelaatio- ja ristikorrelaatiofunktiot.
acf(dele, lag.max=168)
acf(dele, lag.max=168, type = "partial")
acf(dtemp, lag.max=168)
acf(dtemp, lag.max=168, type = "partial")
ccf(dele, dtemp, lag.max=168)


adf.test(dtemp)
adf.test(dele)


'now we can assume that our time series is stationary'


# Estimoidaan malli ja lasketaan ennusteet ilman ulkoista muuttujaa.
p = 1
q = 1
P = 1
Q = 0
malli = arima(ele,
              order = c(p,d,q),
              seasonal = list(order = c(P, D, Q),period = S2),
              method = "CSS")
enne = predict(malli, n.ahead = 24)

# Estimoidaan malli lämpötilan kanssa. Määritä lämpötilan mahdollinen viive L = 1.
L = 1


tempestimointi = eletemp$Celcius[1:(816-L)]
tempennuste = eletemp$Celcius[(816-L+1):(816-L+24)]
eleestimointi = ts(eletemp$kWh[(1+L):816], start =1+L, frequency = 24)
malli2 = arima(eleestimointi,
               order = c(p,d,q),
               seasonal = list(order = c(P, D, Q), period = S2),
               xreg = tempestimointi,
               method = "CSS")

summary(malli2)


enne2 = predict(malli2,
                n.ahead = 24,
                newxreg = tempennuste)

# Esimerkki Portmanteau-testistä. Onko residuaaliaikasarjanalussa nollia?
Box.test(malli2$residuals,
         lag = 14,
         type = "Ljung-Box",
         fitdf = p + q + P + Q)


# Palautetaan plottaus normaaliin 1x1 ruutuun
par(mfrow=c(1,1))

# Plotataan kuva sähkönkulutusaikasarjasta, mallin (2) sovitteesta,
# ennusteesta ja ennusteen 95 %:n eli 1,96*sigma-luottamusväleistä.
ts.plot(eleestimointi,
        eleestimointi - malli2$residuals,
        enne2$pred,
        enne2$pred + 1.96*enne2$se,
        enne2$pred - 1.96*enne2$se,
        ylab = "kulutus/kWh",
        xlab = "aika/vrk",
        col = c("black", "red", "blue", "blue", "blue"),
        main  = "Sovite ja ennuste")

# Plotataan kuva pelkästä ennusteesta.
ts.plot(enne2$pred,
        enne2$pred + 1.96*enne2$se,
        enne2$pred - 1.96*enne2$se,
        ylab = "kulutus/kWh",
        xlab = "aika/vrk",
        col = c("black", "blue", "blue"),
        main = "Ennuste ja  95 %:n luottamusvälit")

# Kirjoitetaan ennuste ja luottamusvälit .csv-tiedostoon, jonka voi avata Excelillä.
output = cbind(enne2$pred,
               enne2$pred + 1.96*enne2$se,
               enne2$pred - 1.96*enne2$se)
write.csv2(output, file = 'ennuste.csv')

which(is.na(enne2$se))