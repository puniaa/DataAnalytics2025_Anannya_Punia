library(readr)
library(EnvStats)
library(nortest)

# set working directory (relative path)
setwd("~/ds/")

# read data
data <- read_csv("epi_results_2024_pop_gdp.csv")

# view dataframe
View(data)

#--------------------------
# Choose the new variables
#--------------------------
SPI <- data$SPI.new
TBN <- data$TBN.old

#--------------------------
# 1. Summaries
#--------------------------
summary(SPI)
summary(TBN)

#--------------------------
# 2. Boxplots
#--------------------------
boxplot(SPI, TBN,
        names = c("SPI.new", "TBN.old"),
        main = "Boxplots of SPI.new and TBN.old",
        col = c("lightblue", "lightgreen"))

#--------------------------
# 3. Histograms + normal overlays
#--------------------------
par(mfrow = c(1,2))  # side-by-side

hist(SPI, prob = TRUE, main = "SPI.new with Normal Overlay",
     xlab = "SPI.new", col = "lightgray", border = "white")
curve(dnorm(x, mean = mean(SPI, na.rm = TRUE),
            sd = sd(SPI, na.rm = TRUE)),
      col = "red", lwd = 2, add = TRUE)
rug(SPI)

hist(TBN, prob = TRUE, main = "TBN.old with Normal Overlay",
     xlab = "TBN.old", col = "lightgray", border = "white")
curve(dnorm(x, mean = mean(TBN, na.rm = TRUE),
            sd = sd(TBN, na.rm = TRUE)),
      col = "blue", lwd = 2, add = TRUE)
rug(TBN)

par(mfrow = c(1,1))  # reset

#--------------------------
# 4. Empirical CDF plots
#--------------------------
plot(ecdf(SPI), main = "ECDF of SPI.new",
     col = "red", lwd = 2)
plot(ecdf(TBN), main = "ECDF of TBN.old",
     col = "blue", lwd = 2)

#--------------------------
# 5. Q–Q plots vs normal
#--------------------------
qqnorm(SPI, main = "Normal Q-Q Plot: SPI.new")
qqline(SPI, col = "red")

qqnorm(TBN, main = "Normal Q-Q Plot: TBN.old")
qqline(TBN, col = "blue")

#--------------------------
# 6. Q–Q plot SPI vs TBN
#--------------------------
qqplot(SPI, TBN,
       xlab = "SPI.new Quantiles",
       ylab = "TBN.old Quantiles",
       main = "Q-Q Plot: SPI.new vs TBN.old")
abline(0, 1, col = "darkgreen", lwd = 2)

#--------------------------
# 7. Normality statistical tests
#--------------------------

# Shapiro-Wilk normality test
shapiro.test(SPI)
shapiro.test(TBN)

# Anderson-Darling normality test
ad.test(SPI)
ad.test(TBN)

#--------------------------
# 8. Test if distributions are identical
#--------------------------
ks.test(SPI, TBN)        # Kolmogorov-Smirnov
wilcox.test(SPI, TBN)    # Wilcoxon rank-sum (nonparametric)

