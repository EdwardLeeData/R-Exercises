```{r}
pairs(realestate)
```

```{r}
y <- realestate$SalePrice
x1 <- realestate$SqFeet
x2 <- realestate$Beds
x3 <- realestate$Baths
x4 <- realestate$Air
x5 <- realestate$Garage
x6 <- realestate$Pool
x7 <- realestate$Year
x8 <- factor(realestate$Quality)
x9 <- realestate$Style
x10 <- realestate$Lot
x11 <- realestate$Highway



fit <- lm((realestate$SalePrice) ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11, data = realestate)
plot(fit)
summary(fit)
anova(fit)
```

mod0 = lm(realestate$SalePrice ~ 1, data = realestate)
#the following is the same as including all predictors in lm()
mod.all = lm(realestate$SalePrice ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11, data = realestate)
step(mod0, scope = list(lower = mod0, upper = mod.all))

```{r}
mod = regsubsets(cbind(realestate$SqFeet, realestate$Quality, realestate$Year, realestate$Lot, realestate$Style, realestate$Baths, realestate$Highway, realestate$Garage), realestate$SalePrice)
summary.mod = summary(mod)
names(summary.mod)

summary.mod$which

summary.mod$adjr2
```
