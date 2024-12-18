library(ggplot2)
library(dplyr)

data <- read.csv("E:/Study/STA108/Project/CDI.csv")

print("Data Structure:")
str(data)

print("First Few Rows of Data:")
head(data)

print("Summary of Data:")
summary(data)

#scatter plot
colors <- c("NE" = "blue", "NC" = "red", "S" = "green", "W" = "yellow")
plot(data$degree, data$income, main="Income vs. Degree Percentage", xlab="Degree Percentage", ylab="Income", col=colors[data$region], pch=19)
legend("topright", legend=names(colors), col=colors, pch=19)

#summary by region
data %>%
  group_by(region) %>%
  summarise(
    mean_income = mean(income),
    median_income = median(income),
    sd_income = sd(income),
    max_income = max(income),
    min_income = min(income),
    mean_degree = mean(degree),
    median_degree = median(degree),
    sd_degree = sd(degree),
    max_degree = max(degree),
    min_degree = min(degree)
  )

# IQR
incomeIQR <- IQR(data$income)
degreeIQR <- IQR(data$degree)

#upper lower bound
incomeUpper <- quantile(data$income, 0.75) + 1.5 * incomeIQR
incomeLower <- quantile(data$income, 0.25) - 1.5 * incomeIQR
degreeUpper <- quantile(data$degree, 0.75) + 1.5 * degreeIQR
degreeLower <- quantile(data$degree, 0.25) - 1.5 * degreeIQR

# remove outliers and print them
cleaneddata <- subset(data, income > incomeLower & income < incomeUpper & degree > degreeLower & degree < degreeUpper)

outliers <- subset(data, income <= incomeLower | income >= incomeUpper | degree <= degreeLower | degree >= degreeUpper)
print("Removed Outliers:")
print(outliers)

summary(cleaneddata)

x_range <- range(data$degree)
y_range <- range(data$income)

#plot without outliers
plot(cleaneddata$degree, cleaneddata$income, main="Cleaned Data: Income vs. Degree Percentage", xlab="Degree Percentage", ylab="Income", col=colors[cleaneddata$region], pch=19, xlim=x_range, ylim=y_range)
legend("topright", legend=names(colors), col=colors, pch=19)

###########################################################################
#Model fitting
# get data for each region
dataNE <- subset(cleaneddata, region == "NE")
dataNC <- subset(cleaneddata, region == "NC")
dataS <- subset(cleaneddata, region == "S")
dataW <- subset(cleaneddata, region == "W")

# function
plotfunc <- function(data, region, color) {
  model <- lm(income ~ degree, data = data)
  summary_model <- summary(model)
  # draw data and fit line
  plot(data$degree, data$income, main=paste("Income vs. Degree Percentage -", region), 
       xlab="Degree Percentage", ylab="Income", col=color, pch=19, xlim=x_range, ylim=y_range)
  abline(model, col="red")
  # sigma2
  sigma2 <- sum(residuals(model)^2) / model$df.residual
  # ret
  return(list(model = model, sigma2 = sigma2, R2 = summary_model$r.squared))
}

# draw for each region
resNE <- plotfunc(dataNE, "NE", colors["NE"])
resNC <- plotfunc(dataNC, "NC", colors["NC"])
resS <- plotfunc(dataS, "S", colors["S"])
resW <- plotfunc(dataW, "W", colors["W"])

# print sigma2 and R2
print(paste("NE Region - Sigma2:", resNE$sigma2, "R2:", resNE$R2))
print(paste("NC Region - Sigma2:", resNC$sigma2, "R2:", resNC$R2))
print(paste("S Region - Sigma2:", resS$sigma2, "R2:", resS$R2))
print(paste("W Region - Sigma2:", resW$sigma2, "R2:", resW$R2))

# best predictor
R2_values <- c(NE = resNE$R2, NC = resNC$R2, S = resS$R2, W = resW$R2)
bestRegion <- names(which.max(R2_values))
print(paste("The best predictor/region is:", bestRegion))

bestModel <- switch(bestRegion,
                     "NE" = resNE$model,
                     "NC" = resNC$model,
                     "S" = resS$model,
                     "W" = resW$model)

plot(bestModel)

# LR model
modelNE <- lm(income ~ degree, data = subset(data, region == "NE"))
summary(modelNE)

# 95% CI
coefNE <- coef(modelNE)
confintNE <- confint(modelNE, level = 0.95)

print(coefNE)
print(confintNE)











































