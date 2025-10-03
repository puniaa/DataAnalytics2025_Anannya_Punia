####### Data Analytics Fall 2025 Lab 2 ######
library(ggplot2)

# --- Read data---
setwd("C:/Users/Anannya Punia/OneDrive/Desktop/DS/lab2")
csv_name <- "NY-House-Dataset.csv"
if (!file.exists(csv_name)) stop(paste("Could not find", csv_name, "in", getwd()))
epi.data <- read.csv(csv_name, header=TRUE)

# --- Choose region label for coloring---
region.var <- if ("LOCALITY" %in% names(epi.data)) {
  epi.data$LOCALITY
} else if ("ADMINISTRATIVE_AREA_LEVEL_2" %in% names(epi.data)) {
  epi.data$ADMINISTRATIVE_AREA_LEVEL_2
} else if ("STATE" %in% names(epi.data)) {
  epi.data$STATE
} else {
  epi.data$STREET_NAME
}
region.fac <- as.factor(region.var)

# --- Variables ---
PRICE <- epi.data$PRICE
sqft  <- epi.data$PROPERTYSQFT
epi.data$log_sqft <- log10(sqft)

# --- Models (three total, mirroring your sample flow) ---
# M0: PRICE ~ SqFt
lin.mod0 <- lm(PRICE ~ sqft, epi.data)

# M1: PRICE ~ log10(SqFt)
lin.mod1 <- lm(PRICE ~ log_sqft, epi.data)

# M2: Subset by region (exclude three labels), PRICE ~ log10(SqFt)
exclude_these <- c("Eastern Europe","Former Soviet States","Latin America & Caribbean")
epi.data.subset <- epi.data[! region.fac %in% exclude_these, ]
lin.mod2 <- lm(PRICE ~ log_sqft, epi.data.subset)

# --- Capture summaries ---
sum_mod0 <- capture.output(summary(lin.mod0))
sum_mod1 <- capture.output(summary(lin.mod1))
sum_mod2 <- capture.output(summary(lin.mod2))

# --- Helper to add a text page to the PDF ---
text_page <- function(lines, title=NULL, cex=0.9){
  plot.new(); par(mar=c(1,1,3,1))
  if (!is.null(title)) title(main=title, cex.main=1.2)
  y <- 0.95
  for (ln in lines) {
    wrapped <- strwrap(ln, width=110)
    for (w in wrapped) {
      text(x=0.02, y=y, labels=w, adj=c(0,1), cex=cex)
      y <- y - 0.035
      if (y < 0.05) { plot.new(); par(mar=c(1,1,3,1)); if (!is.null(title)) title(main=title, cex.main=1.2); y <- 0.95 }
    }
  }
}

# --- Output PDF ---
out_dir <- "lab2_outputs"; if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
pdf_path <- file.path(out_dir, "lab2_report.pdf")
pdf(pdf_path, width=11, height=8.5)  # landscape-ish

# ===== Model 0 =====
print(
  ggplot(epi.data, aes(x = sqft, y = PRICE, colour = region.fac)) +
    geom_point() +
    stat_smooth(method = "lm") +
    labs(title="Model 0: PRICE ~ SqFt", x="PropertySqFt", y="Price", colour="Region") +
    theme_minimal()
)
lin.mod0.df <- data.frame(.fitted = fitted(lin.mod0), .resid = resid(lin.mod0))
print(
  ggplot(lin.mod0.df, aes(x = .fitted, y = .resid)) +
    geom_point() + geom_hline(yintercept = 0) +
    labs(title='Model 0: Residuals vs Fitted', x='Fitted Values', y='Residuals') +
    theme_minimal()
)
text_page(sum_mod0, title="Model 0 — Summary Statistics")

# ===== Model 1 =====
print(
  ggplot(epi.data, aes(x = log_sqft, y = PRICE, colour = region.fac)) +
    geom_point() +
    stat_smooth(method = "lm") +
    labs(title="Model 1: PRICE ~ log10(SqFt)", x="log10(PropertySqFt)", y="Price", colour="Region") +
    theme_minimal()
)
lin.mod1.df <- data.frame(.fitted = fitted(lin.mod1), .resid = resid(lin.mod1))
print(
  ggplot(lin.mod1.df, aes(x = .fitted, y = .resid)) +
    geom_point() + geom_hline(yintercept = 0) +
    labs(title='Model 1: Residuals vs Fitted', x='Fitted Values', y='Residuals') +
    theme_minimal()
)
text_page(sum_mod1, title="Model 1 — Summary Statistics")

# ===== Model 2 (subset) =====
print(
  ggplot(epi.data.subset, aes(x = log_sqft, y = PRICE, colour = factor(region.var))) +
    geom_point() +
    stat_smooth(method = "lm") +
    labs(title="Model 2 (subset): PRICE ~ log10(SqFt)", x="log10(PropertySqFt)", y="Price", colour="Region") +
    theme_minimal()
)
lin.mod2.df <- data.frame(.fitted = fitted(lin.mod2), .resid = resid(lin.mod2))
print(
  ggplot(lin.mod2.df, aes(x = .fitted, y = .resid)) +
    geom_point() + geom_hline(yintercept = 0) +
    labs(title='Model 2 (subset): Residuals vs Fitted', x='Fitted Values', y='Residuals') +
    theme_minimal()
)
text_page(sum_mod2, title="Model 2 (subset) — Summary Statistics")

dev.off()

cat("\nSaved PDF report to:", normalizePath(pdf_path), "\n")
