setwd("C:\\Users\\SheLilan\\Desktop")
library(glmnet)
data <- read.csv("lassodata.csv")
y <- data$Result                     
x <- as.matrix(data[, -1])            
set.seed(123)
cvfit <- cv.glmnet(x, y, family = "binomial", alpha = 1, 
                   nfolds = 10, standardize = TRUE)

par(mfrow = c(1, 2))
plot(cvfit, sign.lambda=1)
title("LASSO Cross-Validation (Internal Standardization)", line = 2.5)

x_scaled <- scale(x)                 
fit_scaled <- glmnet(x_scaled, y, family = "binomial", alpha = 1, 
                     standardize = FALSE)  
plot(fit_scaled, xvar = "lambda", label = TRUE, sign.lambda=1)
title("LASSO Coefficient Paths (Standardized Scale)", line = 2.5)

coef_1se <- coef(cvfit, s = "lambda.1se")
cat("\nCoefficients at lambda.1se (original scale):\n")
print(coef_1se)
non_zero_1se <- rownames(coef_1se)[which(coef_1se[,1] != 0)][-1]
cat("\nNon-zero variables at lambda.1se:\n")
print(non_zero_1se)