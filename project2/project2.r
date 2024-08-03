library(xtable)

setwd("/home/mort/Documents/Stat/project2/soenderborg2")
D <- read.table("soenderborg2_data.csv", sep = ";", header = TRUE)

# Make 't'a date variable in R
D$t <- as.Date(D$t, format = "%d/%m/%Y")
# Choose data from 15 Oct 2009 to 15 Apr 2010 for the four houses
D_model <- subset(D, ("2009-10-15" <= t & t < "2010-04-16") &
    (houseId %in% c(3, 5, 10, 17)))
# Remove observations with missing values
D_model <- na.omit(D_model)
((summary(D_model[c("Q", "Ta", "G")])))

D_summary <- apply(D_model[, c("Q", "Ta", "G")], 2, function(x) {
    c(
        Observations = sum(!is.na(x)),
        Mean = mean(x, na.rm = TRUE),
        Variance = var(x, na.rm = TRUE),
        "Standard Deviation" = sd(x, na.rm = TRUE),
        "First Quartile" = unname(quantile(x, 0.25, na.rm = TRUE)),
        Median = median(x, na.rm = TRUE),
        "Third Quartile" = unname(quantile(x, 0.75, na.rm = TRUE))
    )
})
D_summary <- t(D_summary)
D_summary
xtable(D_summary, digits = 3)


par(mfrow = c(1, 3))
hist(D_model$Q, main = "", xlab = "Heat consumption", col = "red", breaks = 20, cex.lab = 2, cex.axis = 2)
hist(D_model$Ta, main = "", xlab = "Ambient temperature", col = "blue", breaks = 20, cex.lab = 2, cex.axis = 2)
hist(D_model$G, main = "", xlab = "Insolation", col = "orange", breaks = 20, cex.lab = 2, cex.axis = 2)
mtext("Histograms of measurements", side = 3, line = -2, outer = TRUE, cex = 2)

par(mfrow = c(1, 3))
boxplot(D_model$Q, main = "", xlab = "Heat consumption", col = "red", cex.lab = 2, cex.axis = 2)
boxplot(D_model$Ta, main = "", xlab = "Ambient temperature", col = "blue", cex.lab = 2, cex.axis = 2)
boxplot(D_model$G, main = "", xlab = "Insolation", col = "orange", cex.lab = 2, cex.axis = 2)
mtext("Boxplots of measurements", side = 3, line = -2, outer = TRUE, cex = 2)

par(mfrow = c(1, 2))
plot(D_model$Q, D_model$Ta, xlab = "Heat consumption", ylab = "Ambient temperature", col = c("blue", "red"), cex.lab = 1.7, cex.axis = 1.7)
plot(D_model$Q, D_model$G, xlab = "Heat consumption", ylab = "Insolation", col = "orange", cex.lab = 1.7, cex.axis = 1.7)
mtext("Heat consumption vs. Ambient temperature and Insolation", side = 3, line = -2, outer = TRUE, cex = 2) # nolint: line_length_linter.


# Estimate multiple linear regression model
fit <- lm(Q ~ Ta + G, data = D_model)
# Show parameter estimates etc.
summary(fit)


# Plots for model validation
par(mfrow = c(1, 1))
# Observations against fitted values
plot(fit$fitted.values, D_model$Q,
    xlab = "Fitted values",
    ylab = "Heat consumption", col = "red", cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5
)
abline(0, 1, col = "red")
# Residuals against each of the explanatory variables
par(mfrow = c(2, 1))
plot(D_model$Ta, fit$residuals,
    xlab = "Ta", ylab = "Residuals", col = "blue", cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5
)
plot(D_model$G, fit$residuals,
    xlab = "G", ylab = "Residuals", col = "orange", cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5
)
# Residuals against fitted values
par(mfrow = c(1, 1))
plot(fit$fitted.values, fit$residuals,
    xlab = "Fitted values",
    ylab = "Residuals", col = "red", cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5
)
# Normal QQ-plot of the residuals
qqnorm(fit$residuals, ylab = "Residuals", xlab = "Z-scores", col = "red", cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
qqline(fit$residuals)

# Confidence intervals for the model coefficients
confint(fit, level = 0.95)

summary(fit)$coefficients[2, 1] + c(-1, 1) * qt(0.975, df = 573) * summary(fit)$coefficients[2, 2]

hyp <- -0.25
t_obs <- (summary(fit)$coefficients[2, 1] - hyp) / summary(fit)$coefficients[2, 2]
t_obs
pval <- 2 * (1 - pt(abs(t_obs), 573))
pval

lm1 <- lm(Q ~ Ta + G, data = D_model)
lm2 <- lm(Q ~ Ta, data = D_model)
lm3 <- lm(Q ~ G, data = D_model)
summary(lm1)
summary(lm2)
summary(lm3)

# Make dataset for validation
D_test <- subset(D, (t == "2008-12-06" & houseId == 3) |
    (t == "2009-02-22" & houseId == 5) |
    (t == "2009-03-12" & houseId == 10) |
    (t == "2009-04-01" & houseId == 17))

# Predictions and 95% prediction intervals
pred <- predict(lm1,
    newdata = D_test,
    interval = "prediction", level = 0.95
)
# Observed values and predictions
cbind(id = D_test$houseId, Q = D_test$Q, pred)
xtable(cbind(id = D_test$houseId, Q = D_test$Q, pred), digits = 4)
