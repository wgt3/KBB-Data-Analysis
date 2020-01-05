#Log Transformation of price
boxplot(Price)
summary(Price)
sd(Price)
boxplot(log(Price))
qqnorm(Price)
qqline(Price)
qqnorm(log(Price))
qqline(log(Price))
#Basic boxplot of price vs make of car
boxplot(log(Price)~Make)
by(Price, Make, summary)
by(Price, Make, sd)


#boxplot of Price vs type
boxplot(log(Price)~Type)
by(Price, Type, summary)
by(Price, Type, sd)

#scatterplot of Mileage with different colors for different makes
plot(Mileage, log(Price), main = "Price vs. Mileage across Make of Car")
points(Mileage[Make=="Buick"], log(Price[Make=="Buick"]),pch=16, col="Red")
points(Mileage[Make=="Cadillac"], log(Price[Make=="Cadillac"]),pch=16, col="Orange")
points(Mileage[Make=="Chevrolet"], log(Price[Make=="Chevrolet"]),pch=16, col="black")
points(Mileage[Make=="Pontiac"], log(Price[Make=="Pontiac"]),pch=16, col="Green")
points(Mileage[Make=="SAAB"], log(Price[Make=="SAAB"]),pch=16, col="Blue")
points(Mileage[Make=="Saturn"], log(Price[Make=="Saturn"]),pch=16, col="Purple")

abline(lm(log(Price[Make=="Buick"]) ~ Mileage[Make=="Buick"]), col="red")
abline(lm(log(Price[Make=="Cadillac"]) ~ Mileage[Make=="Cadillac"]), col="Orange")
abline(lm(log(Price[Make=="Chevrolet"]) ~ Mileage[Make=="Chevrolet"]), col="black")
abline(lm(log(Price[Make=="Pontiac"]) ~ Mileage[Make=="Pontiac"]), col="Green")
abline(lm(log(Price[Make=="SAAB"]) ~ Mileage[Make=="SAAB"]), col="blue")
abline(lm(log(Price[Make=="Saturn"]) ~ Mileage[Make=="Saturn"]), col="Purple")
legend("bottomright", c("Buick", "Cadillac", "Chevrolet", "Pontiac", "SAAB", "Saturn"), pch=c(16, 16),
       col=c("red", "orange","black","green","blue","purple"), bty="o",bg="grey",seg.len = 2)




#Boxplots of price for each different model for each make of car
boxplot(Price[Make == "Buick"] ~Model[Make == "Buick"])
boxplot(Price[Make == "Cadillac"] ~Model[Make == "Cadillac"])
boxplot(Price[Make == "Chevrolet"] ~Model[Make == "Chevrolet"])
boxplot(Price[Make == "Pontiac"] ~Model[Make == "Pontiac"])
boxplot(Price[Make == "SAAB"] ~Model[Make == "SAAB"])
boxplot(Price[Make == "Saturn"] ~Model[Make == "Saturn"])
#Boxplots of price for each trim for different makes
boxplot(Price[Make == "Buick"] ~Trim[Make == "Buick"])
boxplot(Price[Make == "Cadillac"] ~Trim[Make == "Cadillac"])
boxplot(Price[Make == "Chevrolet"] ~Trim[Make == "Chevrolet"])
boxplot(Price[Make == "Pontiac"] ~Trim[Make == "Pontiac"])
boxplot(Price[Make == "SAAB"] ~Trim[Make == "SAAB"])
boxplot(Price[Make == "Saturn"] ~Trim[Make == "Saturn"])

##Turn categorical variables into indicator variables by releveling
Make1 = relevel(as.factor(Make), ref="Buick")
Model1 = relevel(as.factor(Model), ref ="Century")
Trim1  = relevel(as.factor(Trim), ref ="Sedan 4D")
Cyl1  = relevel(as.factor(Cyl), ref ="4")
Liter1 = relevel(as.factor(Liter), ref = "3.1")
Doors1 = relevel(as.factor(Doors), ref ="2")
Type1 = relevel(as.factor(Type), ref ="Sedan")
Sound = as.factor(Sound)
Leather = as.factor(Leather)
Cruise = as.factor(Cruise)
#model with all covariates
fit.all = lm (log(Price)~Mileage+Make1+Model1+Trim1+Cyl1+Liter+Doors1+Cruise+Sound+Leather)
summary(fit.all)
anova(fit.all)

#multiway anova
t.test(log(Price)[Sound==0], log(Price)[Sound==1])
t.test(log(Price)[Cruise==0], log(Price)[Cruise==1])
t.test(log(Price)[Leather==0], log(Price)[Leather==1])
#discrepancies with sound
qqnorm(log(Price)[Sound==1])
qqline(log(Price)[Sound==1])
qqnorm(log(Price)[Sound==0])
qqline(log(Price)[Sound==0])
boxplot(log(Price)~Sound)

interaction.plot(Make1, Sound, log(Price), col=1:6)
interaction.plot(Trim1, Sound, log(Price), col=1:47)
interaction.plot(Type1, Sound, log(Price), col=1:5)
interaction.plot(Model1, Sound, log(Price), col=1:32)



boxplot(log(Price)~Cruise)
boxplot(log(Price)~Leather)
aov.fit = aov(log(Price)~(Sound*Leather)+(Sound*Cruise)+(Leather*Cruise))
summary(aov.fit)



par(mfrow=c(1,1))
plot(aov.fit$fitted.values, rstandard(aov.fit),
             xlab="fitted values", ylab="standardized residuals")
abline(0,0,lty=2)
qqnorm(rstandard(aov.fit)); qqline(rstandard(aov.fit))


fit.add.ons = lm(log(Price)~Sound+Leather+Cruise+(Sound*Leather)+(Sound*Cruise))
summary(fit.add.ons)
fit.p = lm(log(Price)~Sound)

interaction.plot(Sound, Leather, log(Price), col=1:2)
interaction.plot(Sound, Cruise, log(Price), col=1:2)
interaction.plot(Cruise, Leather, log(Price), col=1:2)

#models with different covariates - just Mileage and Make
fit1 = lm(log(Price)~Mileage+Make1)
summary(fit1)
anova(fit1)
vif(fit1)

#pretty good model
fit2 = lm(log(Price)~Mileage+Model1+Type1+ Cyl1 +Sound+Leather+(Sound*Leather)+(Sound*Cruise))
summary(fit2)
anova(fit2)

vif(fit2)

fit2sel = regsubsets(log(Price)~Mileage+Model1+Type1+Cyl1+Cruise+Sound+Leather+(Sound*Leather)+(Sound*Cruise), data = Cars, nbest = 1)
#install.packages("HH")

library(HH)
summaryHH(fit2sel)

backsel = step(fit2, direction="backward")

##Check for influence and leverage for fit2
hi = hatvalues(fit2)
plot(hi)
sel = which(hi > 3*mean(hi))
Mileage[sel]

cooks.distance(fit2)
plot(cooks.distance(fit2), type="h")
abline(.01,0,lty=2)
sel2 = which(cooks.distance(fit2)>.01)
Mileage[sel2]

dfbetas(fit2)
plot(abs(dffits(fit2)), type="h")
which(abs(dfbetas(fit2)[,1])>.48)



#model with Model1
fit3 = lm(log(Price)~Mileage+Model1+Trim1+Cruise+Sound+Leather)
summary(fit3)
vif(fit3)

backsel = step(fit3, direction="backward")

fit3sel = regsubsets(log(Price)~Mileage+Model1+Trim1+Cruise+Sound+Leather, data = Cars, nbest = 1)
#install.packages("HH")

library(HH)
summaryHH(fit3sel)

backsel = step(fit2, direction="backward")

Cars.new = Cars[-c(382,384,388,741,741,743,744),]
Model1new = relevel(as.factor(Cars.new$Model), ref ="Century")
Type1new = relevel(as.factor(Cars.new$Type), ref ="Sedan")
Cyl1new = relevel(as.factor(Cars.new$Cyl), ref ="6")

fit2.new = lm(log(Price)~Mileage+Model1new+Type1new+ Cyl1new +Sound+Leather, data = Cars.new)
summary(fit2.new)


#Subsetting data
Buick.century = subset(Cars, Model=="Century")
fit.BuickCentury = lm(log(Price)~Mileage+Sound+Cruise+Leather, data = Buick.century)
summary(fit.BuickCentury)
anova(fit.BuickCentury)
qqnorm(fit.BuickCentury$residuals)
qqline(fit.BuickCentury$residuals)
plot(fit.BuickCentury$fitted.values, fit.BuickCentury$residuals)
abline(0,0)


Chevy.AVEO = subset(Cars, Model=="AVEO")
Trim.AVEO  = relevel(as.factor(Chevy.AVEO$Trim), ref ="LS Hatchback 4D")

fit.ChevyAVEO = lm(log(Price)~Mileage+Sound+Cruise+Leather, data = Chevy.AVEO)
summary(fit.ChevyAVEO)
