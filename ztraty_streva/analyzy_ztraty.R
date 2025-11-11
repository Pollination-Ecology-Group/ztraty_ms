# Analýza dat

# Data 2024 - stěry a střeva ####

summary(data2024$celk_pyl)
summary(data2024$celk_streva)

# STĚRY A STŘEVA V ČASE 

# STĚRY
with(data2024, plot(2*suc_stery~cas))
sucster <- lm(suc_stery ~ cas, data = data2024)
abline(sucster)

with(data2024, plot(log1p(suc_stery)~cas))

data2024$cas_num <- as.numeric(data2024$cas, units = "hours")

# polynom modely
polynom2 <- lm(log1p(suc_stery) ~ poly(cas_num,2), data = data2024)
summary(polynom2)

polynom2het <- lm(log1p(ost_stery) ~ poly(cas_num,2), data = data2024)
summary(polynom2het)

polynom4 <- lm(log1p(suc_stery) ~ poly(cas_num,4), data = data2024)
summary(polynom4)

polynom4het <- lm(log1p(ost_stery) ~ poly(cas_num,4), data = data2024)
summary(polynom4het)

polystreva2 <- lm(log1p(suc_streva) ~ poly(cas_num,2), data = data2024)
summary(polystreva2)

polystreva2het <- lm(log1p(ost_streva) ~ poly(cas_num,2), data = data2024)
summary(polystreva2het)

# GAM graf
library(mgcv)
gam_model <- gam(log1p(suc_stery) ~ s(cas_num), data = data2024)
plot(gam_model, shade = TRUE, main = "GAM - log1p(suc_stery) ~ cas")


# zkouška grafu s polynomem 2
plot(data2024$cas_num, log1p(data2024$suc_stery),
     pch = 19, col = "darkgray",
     xlab = "cas_num",
     ylab = "log1p(suc_stery)",
     main = "Pyl Succisa na 1/2 těla v čase")

cas_seq <- seq(min(data2024$cas_num), max(data2024$cas_num), length.out = 200)
pred_vals <- predict(polynom2, newdata = data.frame(cas_num = cas_seq))
lines(cas_seq, pred_vals, col = "black", lwd = 2)



with(data2024, plot(log1p(ost_stery)~cas))

with(data2024, plot(log(celk_pyl)~cas))

# STĚRY základní graf
par(mar = c(5, 6, 4, 2),
    mgp = c(3.5, 1, 0))

y_log_range <- range(c(log1p(data2024$suc_stery), log1p(data2024$ost_stery)), na.rm = TRUE)

y_real <- c(0, 1, 5, 20, 50, 100, 200, 500, 1000,5000, 10000)  

y_log <- log1p(y_real)

with(data2024, {
  plot(cas, log1p(suc_stery), 
       pch = 16, col = "lightseagreen", 
       xlab = "Daytime", ylab = "Log(1+x) Pollen load (1/2 of body)",
       main = "",
       xlim = range(cas), 
       ylim = y_log_range,
       yaxt = "n")
  
  points(cas, log1p(ost_stery), 
         pch = 16, col = "lightcoral")
  
  axis(side = 2, at = y_log, labels = y_real, las = 1)
  
  legend("topright", legend = c("Conspecific", "Heterospecific"),
         col = c("lightseagreen", "lightcoral"),
         pch = c(16, 16))
})
# nejlépe proložit něčím vhodnějším než přímkou, ale čím? Polynom 4.stupně

cas_seq <- seq(min(data2024$cas_num), max(data2024$cas_num), length.out = 200)
pred_vals <- predict(polynom2, newdata = data.frame(cas_num = cas_seq))
#lines(cas_seq, pred_vals, col = "lightseagreen", lwd = 2)

pred_vals_het <- predict(polynom2het, newdata = data.frame(cas_num = cas_seq))
#lines(cas_seq, pred_vals_het, col = "lightcoral", lwd = 2)

pred_poly4 <- predict(polynom4, newdata = data.frame(cas_num = cas_seq))
lines(cas_seq, pred_poly4, col = "lightseagreen", lwd = 2)

pred_poly4_het <- predict(polynom4het, newdata = data.frame(cas_num = cas_seq))
lines(cas_seq, pred_poly4_het, col = "lightcoral", lwd = 2)

#konfidenční intervaly ke stěrovým grafům

pred_poly4 <- predict(polynom4, 
                      newdata = data.frame(cas_num = cas_seq), 
                      interval = "confidence")

lines(cas_seq, pred_poly4[, "fit"], col = "lightseagreen", lwd = 2)

lines(cas_seq, pred_poly4[, "lwr"], col = "lightseagreen", lty = 2)  # dolní mez
lines(cas_seq, pred_poly4[, "upr"], col = "lightseagreen", lty = 2)  # horní mez

polygon(c(cas_seq, rev(cas_seq)), 
        c(pred_poly4[, "lwr"], rev(pred_poly4[, "upr"])), 
        col = rgb(0, 0, 1, 0.2), border = NA)


pred_poly4_het <- predict(polynom4het, 
                          newdata = data.frame(cas_num = cas_seq), 
                          interval = "confidence")

lines(cas_seq, pred_poly4_het[, "fit"], col = "lightcoral", lwd = 2)

lines(cas_seq, pred_poly4_het[, "lwr"], col = "lightcoral", lty = 2)  # dolní mez
lines(cas_seq, pred_poly4_het[, "upr"], col = "lightcoral", lty = 2)  # horní mez

polygon(c(cas_seq, rev(cas_seq)), 
        c(pred_poly4_het[, "lwr"], rev(pred_poly4_het[, "upr"])), 
        col = rgb(1, 0, 0, 0.2), border = NA)


# STŘEVA
with(data2024, plot(log(suc_streva)~cas))

with(data2024, plot(log(ost_streva)~cas))

with(data2024, plot(celk_streva~cas))

# STŘEVA základní graf

y_log_rangeS <- range(c(log1p(data2024$suc_streva), log1p(data2024$ost_streva)), na.rm = TRUE)

y_realS <- c(0, 1, 5, 20, 50, 100, 200, 500, 1000,5000, 10000)  

y_logS <- log1p(y_realS)

with(data2024, {
  plot(cas, log1p(suc_streva), 
       pch = 16, col = "lightseagreen", 
       xlab = "Daytime", ylab = "Log(1+x) Consumed pollen",
       main = "",
       xlim = range(cas), 
       ylim = y_log_rangeS,
       yaxt = "n")
  
  points(cas, log1p(ost_streva), 
         pch = 16, col = "lightcoral")
  
  axis(side = 2, at = y_logS, labels = y_realS, las = 1)
  
  legend("topright", legend = c("Conspecific", "Heterospecific"),
         col = c("lightseagreen", "lightcoral"),
         pch = c(16, 16))
})
# nejlépe proložit něčím vhodnějším než přímkou, ale čím? Polynom 2.stupně

cas_seq <- seq(min(data2024$cas_num), max(data2024$cas_num), length.out = 200)
pred_streva <- predict(polystreva2, newdata = data.frame(cas_num = cas_seq))
lines(cas_seq, pred_streva, col = "lightseagreen", lwd = 2)

pred_streva_het <- predict(polystreva2het, newdata = data.frame(cas_num = cas_seq))
lines(cas_seq, pred_streva_het, col = "lightcoral", lwd = 2)

#konfidenční intervaly pro střeva

pred_streva <- predict(polystreva2, 
                       newdata = data.frame(cas_num = cas_seq), 
                       interval = "confidence")

lines(cas_seq, pred_streva[, "fit"], col = "lightseagreen", lwd = 2)
lines(cas_seq, pred_streva[, "lwr"], col = "lightseagreen", lty = 2)
lines(cas_seq, pred_streva[, "upr"], col = "lightseagreen", lty = 2)

polygon(c(cas_seq, rev(cas_seq)), 
        c(pred_streva[, "lwr"], rev(pred_streva[, "upr"])), 
        col = rgb(0, 0, 1, 0.2), border = NA)

pred_streva_het <- predict(polystreva2het, 
                           newdata = data.frame(cas_num = cas_seq), 
                           interval = "confidence")

lines(cas_seq, pred_streva_het[, "fit"], col = "lightcoral", lwd = 2)
lines(cas_seq, pred_streva_het[, "lwr"], col = "lightcoral", lty = 2)
lines(cas_seq, pred_streva_het[, "upr"], col = "lightcoral", lty = 2)

polygon(c(cas_seq, rev(cas_seq)), 
        c(pred_streva_het[, "lwr"], rev(pred_streva_het[, "upr"])), 
        col = rgb(1, 0, 0, 0.2), border = NA)


# na těle a v těle

with(data2024, plot(celk_pyl~celk_streva))
with(data2024, plot(suc_stery~suc_streva))
with(data2024, plot(ost_stery~ost_streva))

# rozdíly v pohlaví (nejsou)

data2024$pohlavi <- as.factor(data2024$pohlavi)

with(data2024, plot(log(celk_pyl)~pohlavi))
with(data2024, plot(suc_stery~pohlavi))

with(data2024, plot(celk_streva~pohlavi))
with(data2024, plot(suc_streva~pohlavi))

# rozdíly v druzích (nejsou)

data2024$druh <- as.factor(data2024$druh)

with(data2024, plot(celk_pyl~druh, las=2))
with(data2024, plot(suc_stery~druh))

with(data2024, plot(celk_streva~druh))
with(data2024, plot(suc_streva~druh))


# podíl konspecifického pylu

with(data2024, plot(konsp_stery~druh))
with(data2024, plot(konsp_streva~druh))

with(data2024, plot(konsp_stery~cas))
with(data2024, plot(konsp_streva~cas))

with(data2024, plot(konsp_stery~pohlavi))
with(data2024, plot(konsp_streva~pohlavi))

# Koeficienty variance (CVs)

suc_stery_cv <- (sd(data2024$suc_stery) / mean(data2024$suc_stery)) * 100
suc_stery_cv

ost_stery_cv <- (sd(data2024$ost_stery) / mean(data2024$ost_stery)) * 100
ost_stery_cv

suc_streva_cv <- (sd(data2024$suc_streva) / mean(data2024$suc_streva)) * 100
suc_streva_cv

ost_streva_cv <- (sd(data2024$ost_streva) / mean(data2024$ost_streva)) * 100
ost_streva_cv

cv_data <- data.frame(
  promenna = c("suc_stery", "ost_stery", "suc_streva", "ost_streva"),
  CV = c(129.6186, 262.0532, 144.4358, 191.0376)
)
cv_data

barplot(cv_data$CV,
        names.arg = cv_data$promenna,
        col = c("lightseagreen", "lightcoral"),
        main = "Srovnání koeficientu variance (CV)",
        ylab = "CV (%)",
        ylim = c(0, max(cv_data$CV) * 1.2))

text(x = 1:length(cv_data$CV),
     y = cv_data$CV,
     labels = round(cv_data$CV, 2),
     pos = 3, cex = 0.9)

