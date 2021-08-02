# Part A Comprehensive Descriptive Analysis 


# removing the outliers 

unsanitised <- read.csv("Google Drive/Data Science & Analytics Degree /DS&A Year 1/Practical Data Science (2020)/Practical Data Science CourseWork/swedishInsurance.csv", header = TRUE)
Q1 <- quantile(unsanitised$Claims, .25)
Q3 <- quantile(unsanitised$Claims, .75)
IQR <- IQR(unsanitised$Claims)
swedishInsurance <- subset(unsanitised, unsanitised$Claims> (Q1 - 1.5*IQR) & unsanitised$Claims< (Q3 + 1.5*IQR))


# Central Tendencies 

# Mean number of claims for each make of car

test2 <- filter(swedishInsurance, Make == 1 | Make == 2 | Make == 3 | Make == 4 | Make == 5 | Make == 6 | Make == 7)

Make1 <- filter(swedishInsurance, Make == 1)
mean(Make1$Claims)

# mean number of claims for make 1 47.43673

Make2 <- filter(swedishInsurance, Make == 2)
mean(Make2$Claims)

# mean number of claims for make 2 11.21224

Make3 <- filter(swedishInsurance, Make == 3)
mean(Make3$Claims)

# mean number of claims for make 3 7.632231

Make4 <- filter(swedishInsurance, Make == 4)
mean(Make4$Claims)

# mean number of claims for make 4 8.676471

Make5 <- filter(swedishInsurance, Make == 5)
mean(Make5$Claims)

# mean number of claims for make 5 12.68033

Make6 <- filter(swedishInsurance, Make == 6)
mean(Make6$Claims)

# mean number of claims for make 6 12.68033

Make7 <- filter(swedishInsurance, Make == 7)
mean(Make7$Claims)

# mean number of claims for make 7 9.008264

Make8 <- filter(swedishInsurance, Make == 8)
mean(Make8$Claims)

# mean number of claims for make 8 4.654008

Make9 <- filter(swedishInsurance, Make == 9)
mean(Make9$Claims)

# mean number of claims for make 9 342.2408


kilometres1 <- filter(swedishInsurance, Kilometres == 1)
mean(kilometres1$Claims)

# mean number of claims for kilometres 1 75.59453

kilometres2 <- filter(swedishInsurance, Kilometres == 2)
mean(kilometres2$Claims)

# mean number of claims for kilometres 2 89.27664

kilometres3 <- filter(swedishInsurance, Kilometres == 3)
mean(kilometres3$Claims)

# mean number of claims for kilometres 3 54.161

kilometres4 <- filter(swedishInsurance, Kilometres == 4)
mean(kilometres4$Claims)

# mean number of claims for kilometres 4 20.79493

kilometres5 <- filter(swedishInsurance, Kilometres == 5)
mean(kilometres5$Claims)

# mean number of claims for kilometres 5 18.04215





zone1 <- filter(swedishInsurance, Zone == 1)
sd(zone1$Claims)
mean(zone1$Claims)
mean(zone1$Insured)
mean(zone1$Payment)
sd(zone1$Claims)
# the mean number of claims for zone 1 is 73.56825
# the mean number of insured people is 1036.172

zone2 <- filter(swedishInsurance, Zone == 2)
mean(zone2$Claims)
mean(zone2$Insured)
mean(zone1$Payment)
# the mean number of claims for zone 2 is 67.6254
# the mean number of insured people is 1231.482

zone3 <- filter(swedishInsurance, Zone == 3)
mean(zone3$Claims)
mean(zone3$Insured)
mean(zone1$Payment)
# the mean number of claims for zone 3 is 63.29524
# the mean number of insured people is 1362.959

zone4 <- filter(swedishInsurance, Zone == 4)
mean(zone4$Claims)
mean(zone4$Insured)
mean(zone1$Payment)
# the mean number of claims for zone 4 is 101.3111
# the mean number of insured people is 2689.38


zone5 <- filter(swedishInsurance, Zone == 5)
mean(zone5$Claims)
mean(zone5$Insured)
mean(zone1$Payment)
# the mean number of claims for zone 5 is  19.04792
# the mean number of insured people is 384.8019

zone6 <- filter(swedishInsurance, Zone == 6)
mean(zone6$Claims)
mean(zone6$Insured)
mean(zone1$Payment)
# the mean number of claims for zone 6 is 32.57778
# the mean number of insured people is 802.6846

zone7 <- filter(swedishInsurance, Zone == 7)
mean(zone7$Claims)
mean(zone7$Insured)
mean(zone1$Payment)
# the mean number of claims for zone 7 is 2.108844
# the mean number of insured people is 64.91071

# We are now comparing the means in terms of the zone's 

cor(zone7$Claims, zone7$Insured, method = "pearson")
cor(zone6$Claims, zone6$Insured, method = "pearson")
cor(zone5$Claims, zone5$Insured, method = "pearson")
cor(zone7$Claims, zone7$Insured, method = "pearson")

# meanNumberOfClaims
meanNumberOfClaims <- data.frame(
   zone=c("Zone 1", "Zone 2","Zone 3","Zone 4","Zone 5","Zone 6","Zone 7") ,  
   means=c(10.78346,9.832669,10.29804,12.18455,5.619377,8.310714,1.43299)
)

# Barplot mean number of payments for the highest and lowest zone 
ggplot(meanNumberOfClaims, aes(x=zone, y=means)) + 
   geom_bar(stat = "identity", width=0.2)  + ggtitle("Mean Number Of Claims For All Zones") +
   xlab("Zones") + ylab("Mean Number Of Claims")  + scale_fill_hue(c = 40) + theme(legend.position="none")

# meanNumberOfInsuredPeople
meanNumberOfInsuredPeople <- data.frame(
   zone=c("Zone 1", "Zone 2","Zone 3","Zone 4","Zone 5","Zone 6","Zone 7") ,  
   insured=c(1036.172,1231.482,1362.959,2689.38,384.8019,802.6846,64.91071)
)

# Barplot mean number of payments for the highest and lowest zone 
ggplot(meanNumberOfInsuredPeople, aes(x=zone, y=insured)) + 
   geom_bar(stat = "identity", width=0.2)  + ggtitle("Mean Number Of Insured People For All Zones") +
   xlab("Zones") + ylab("Mean Number Of Insured People")  + scale_fill_hue(c = 40) + theme(legend.position="none")



# meanMakeClaims
meanMakeClaims <- data.frame(
   make=c("Make 1", "Make 2","Make 3","Make 4","Make 5","Make 6","Make 7","Make 8", "Make 9") ,  
   claims=c(15.47514,7.709402,5.757447,5.724891,8.869565,11.00455,5.931034,4.238298,16.45614)
)

# Barplot mean number of payments for the highest and lowest zone 
ggplot(meanMakeClaims, aes(x=make, y=claims)) + 
   geom_bar(stat = "identity", width=0.2)  + ggtitle("Mean Number Of Claims Per Make") +
   xlab("Make") + ylab("Mean Number Of Claims Per Make")  + scale_fill_hue(c = 40) + theme(legend.position="none")


# meanKilometresClaims
meanKilometresClaims <- data.frame(
   kilometres=c("1000","1000-15000","15000-20000","20000-25000","25000") ,  
   claims=c(9.793872,11.57771,9.030055,5.858586,4.98977)
)

# Barplot mean number of payments for the highest and lowest zone 
ggplot(meanKilometresClaims, aes(x=kilometres, y=claims)) + 
   geom_bar(stat = "identity", width=0.2)  + ggtitle("Mean Number Of Claims For Kilometres") +
   xlab("Kilometres") + ylab("Mean Number Of Claims For Kilometres")  + scale_fill_hue(c = 40) + theme(legend.position="none")




# the standard deviation of the highest mean zone is 333.4714
var <- var(zone4$Claims)
sum <- summary(zone4$Claims)
sdd <- sd(zone4$Claims)

test10 <- ggplot(zone4, aes(x=var)) + geom_density()

# the standard deviation of the lowest mean zone is 7.744759
sd(zone7$Claims)

# find the mean number of payments for each car brand then compare

carModel1 <- filter(swedishInsurance, Make == 1)
mean(carModel1$Payment)


plots2 <- c(carModel3$Claims,carModel2$Claims)

plot(density(plots2))


plot(density(carModel1$Claims),          # Plot density of x
     xlim = c(0, 50),
     ylim = c(0, 0.4))
lines(density(carModel2$Claims),         # Add density of y
      col = 2)
lines(density(carModel3$Claims),         # Add density of z
      col = 3)



var1 <- var(carModel1$Payment)
sum1 <- summary(carModel1$Payment)
sdd1 <- sd(carModel1$Payment)

test <- filter(swedishInsurance, Make == 1 & Make == 2 & Make == 3 & Make == 4 & Make == 5 & Make == 6 & Make == 7)

carModel1 <- filter(swedishInsurance, Make == 1)
mean(carModel1$Payment)
# Mean payment made for Car Model 1 is 24,8975.4

carModel2 <- filter(swedishInsurance, Make == 2)
mean(carModel2$Payment)
# Mean payment made for Car Model 2 is 56,760.36

carModel3 <- filter(swedishInsurance, Make == 3)
mean(carModel3$Payment)
# Mean payment made for Car Model 3 is 43,785.92

carModel4 <- filter(swedishInsurance, Make == 4)
mean(carModel4$Payment)
# Mean payment made for Car Model 4 is 36,746.52

carModel5 <- filter(swedishInsurance, Make == 5)
mean(carModel5$Payment)
# Mean payment made for Car Model 5 is 60,519.64

carModel6 <- filter(swedishInsurance, Make == 6)
mean(carModel6$Payment)
# Mean payment made for Car Model 6 is 96,216.83

carModel7 <- filter(swedishInsurance, Make == 7)
mean(carModel7$Payment)
# Mean payment made for Car Model 7 is 42,280.04

carModel8 <- filter(swedishInsurance, Make == 8)
mean(carModel8$Payment)
# Mean payment made for Car Model 8 is 31,053.71

otherCarModels <- filter(swedishInsurance, Make == 9)
mean(otherCarModels$Payment)
# Mean payment made for Car Model 9 is 16,76361

# also find the mean distance and compare to the payment too

kilometersVariable1 <- filter(swedishInsurance, Kilometres == 1)
mean(kilometersVariable1$Payment)
# Mean payment made for kilometers < 1000 is 167,6361

kilometersVariable2 <- filter(swedishInsurance, Kilometres == 2)
mean(kilometersVariable2$Payment)
# Mean payment made for kilometers >= 1000 & <= 15000 is 442,523.8

kilometersVariable3 <- filter(swedishInsurance, Kilometres == 3)
mean(kilometersVariable3$Payment)
# Mean payment made for kilometers >= 15000 & <= 20000 is 272,012.6

kilometersVariable4 <- filter(swedishInsurance, Kilometres == 4)
mean(kilometersVariable4$Payment)
# Mean payment made for kilometers > 25000 is 93,306.12

kilometersVariable5 <- filter(swedishInsurance, Kilometres == 5)
mean(kilometersVariable5$Payment)
# Mean payment made for kilometers >= 20000 & <= 25000 is 108,213.4

# maybe try something with the model and distance

mean(largeCities$Claims)
# the mean number of claims for large cities is 70.59683
mean(smallCities$Claims)
# the mean number of claims for small cities is 41.24204
mean(ruralAreas$Claims)
# the mean number of claims for rurualAreas is 66.94444
mean(gotland$Claims)
# the mean number of claims for gotland is 2.108844

# Error bars 

# Load ggplot2
library(ggplot2)

# create dummy data
data <- data.frame(
   name=letters[1:5],
   value=sample(seq(4,15),5),
   sd=c(1,0.2,3,2,4)
)

sd(zone1$Claims)
sd(zone2$Claims)
sd(zone3$Claims)
sd(zone4$Claims)
sd(zone5$Claims)
sd(zone6$Claims)
sd(zone7$Claims)

# DISPERSION


meanNumberOfClaims <- data.frame(
   zone=c("Zone 1", "Zone 2","Zone 3","Zone 4","Zone 5","Zone 6","Zone 7") ,  
   means=c(10.78346,9.832669,10.29804,12.18455,5.619377,8.310714,1.43299),
   sd=c(11.40437,10.48637,11.15484,12.1919,8.794753,11.0936,3.870954),
   se=c(0.7155738,0.6618936,0.6985433,0.7987182,0.5173384,0.6629695,0.2269194)
)

se(zone1$Claims)
se(zone2$Claims)
se(zone3$Claims)
se(zone4$Claims)
se(zone5$Claims)
se(zone6$Claims)
se(zone7$Claims)

ggplot(meanNumberOfClaims) +
   geom_bar( aes(x=zone, y=means), stat="identity", fill="black", alpha=0.7) +
   geom_errorbar( aes(x=zone, ymin=means-se, ymax=means+se), width=0.4, colour="blue", alpha=0.9, size=1.3) 

# label graph

sd(zone1$Claims)
sd(zone2$Claims)
sd(zone3$Claims)
sd(zone4$Claims)
sd(zone5$Claims)
sd(zone6$Claims)
sd(zone7$Claims)

test55 <- c(10.78346 / 11.40437)

summary(swedishInsurance$Claims)


# Most basic error bar for mean number of claims for each zone



# use the frequency of each variable and then pass this in 


# create a bar chart with two bars one with mean and with the SD (DISPERSION) START HERE
# create a table for each value and then create a graph

                  # Install & load ggplot2 package
 
# CONTINUE WORKING ON THIS GROUPED BARS FOR YOUR DISPERSION ANALYSIS  !!!1





ggplot(meanNumberOfClaims, aes(x = zoneNumber, y = mean, fill = sd)) +
   geom_col(position = position_dodge())

barplot(mytable2, beside=TRUE)


sd(zone1$Claims)
sd(zone2$Claims)
sd(zone3$Claims)
sd(zone4$Claims)
sd(zone5$Claims)
sd(zone6$Claims)
sd(zone7$Claims)
mean(zone1$Claims)

# box plots showing the disrebuitin of the data

boxplot(Claims~Zone,
        data=swedishInsurance,
        main="Claims & Zone Dispersion Measures",
        xlab="Zone",
        ylab="Claims",
        ylim=c(0,50),
        col="darkgrey",
        border="blue"
)

summary(zone1$Claims)
summary(zone2$Claims)
summary(zone3$Claims)
summary(zone4$Claims)
summary(zone5$Claims)
summary(zone6$Claims)
summary(zone7$Claims)

boxplot(Claims~Make,
        data=swedishInsurance,
        main="Claims & Make Dispersion Measures",
        xlab="Make",
        ylab="Claims",
        ylim=c(0,50),
        col="darkgrey",
        border="blue"
)

summary(Make1$Claims)
summary(Make2$Claims)
summary(Make3$Claims)
summary(Make4$Claims)
summary(Make5$Claims)
summary(Make6$Claims)
summary(Make7$Claims)
summary(Make8$Claims)
summary(Make9$Claims)



summary(kilometres4$Claims)

boxplot(Claims~Kilometres,
        data=swedishInsurance,
        main="Claims & Kilometres Dispersion Measures",
        xlab="Kilometres",
        ylab="Claims",
        ylim=c(0,50),
        col="darkgrey",
        border="blue"
)

summary(kilometres1$Claims)
summary(kilometres2$Claims)
summary(kilometres3$Claims)
summary(kilometres4$Claims)
summary(kilometres5$Claims)

# Kernel Density Plot
d <- density(swedishInsurance$Claims) # returns the density data 
plot(d) # plots the results

shapiro.test(swedishInsurance$Claims)





# Load ggplot2
library(ggplot2)

# The mtcars dataset is natively available
# head(mtcars)

# A really basic boxplot.
ggplot(swedishInsurance, aes(x=as.factor(Zone), y=Claims)) + 
   geom_boxplot(fill="slateblue", alpha=0.3) + 
   xlab("Zones")

# Compare MPG distributions for cars with 
# 4,6, or 8 cylinders
library(sm)
attach(mtcars)


# Part B Correlation  Analysis 

# we are filtering only zone 1 and zone 2 and our pearson = [1] 0.9964591 showing that this has a high correlation
largeCities <- filter(swedishInsurance, Zone == 1 | Zone == 2)
cor(largeCities$Claims, largeCities$Payment, method = "spearman")



cor(swedishInsurance$Claims, swedishInsurance$Payment, method = "pearson")
cor(swedishInsurance$Claims, swedishInsurance$Payment, method = "spearman")

plot(largeCities$Claims, largeCities$Payment, main = "Large Cities",
     xlab = "Large Cities Total Claims", ylab = "Large Cities Total Payment",
     pch = 19) + lines(lowess(largeCities$Claims,largeCities$Payment), col="blue") # lowess line (x,y)


# trying to remove outliers wasting time and cannot get it to work 
boxplot(largeCities$Claims)$out
largeCitiesOutliers <- boxplot(largeCities$Claims, plot=FALSE)$out
print(largeCitiesOutliers)
swedishInsurance[which(largeCities$Claims %in% largeCitiesOutliers),1]
boxplot(largeCities$Claims)

largeCitiesScatterPlot <- ggplot(largeCities, aes(Claims, Payment))
largeCitiesScatterPlot + geom_point() + geom_smooth(method = "lm" , se= F) + labs(x="Claims", y="Payment") + ggtitle("Large Cities & Payment Correlation")


# we are filtering only zone 3 and zone 5 and our pearson = 0.9961211 showing that this has a high correlation
smallCities <- filter(swedishInsurance, Zone == 3 | Zone == 5)

cor(smallCities$Claims, smallCities$Payment, method = "spearman")

plot(smallCities$Claims, smallCities$Payment, main = "Small Cities",
     xlab = "Small Cities Claims", ylab = "Small Cities Total Payment",
     pch = 19) + lines(lowess(smallCities$Claims,smallCities$Payment), col="blue") # lowess line (x,y)

smallCitiesScatterPlot <- ggplot(smallCities, aes(Claims, Payment))
smallCitiesScatterPlot + geom_point() + geom_smooth(method = "lm" , se= F) + labs(x="Claims", y="Payment") + ggtitle("Small Cities & Payment Correlation")


# we are filtering only zone 4 and rural areas 6 and our pearson = 0.9978324 showing that this has a high correlation
ruralAreas <- filter(swedishInsurance, Zone == 4 | Zone == 6)
cor(ruralAreas$Claims, ruralAreas$Payment, method = "spearman")

plot(ruralAreas$Claims, ruralAreas$Payment, main = "Rural Areas",
     xlab = "Rural Areas Total Claims", ylab = "Rural Areas Total Payment",
     pch = 19) + lines(lowess(ruralAreas$Claims,ruralAreas$Payment), col="blue") # lowess line (x,y)

ruralAreasScatterPlot <- ggplot(ruralAreas, aes(Claims, Payment))
ruralAreasScatterPlot + geom_point() + geom_smooth(method = "lm" , se= F) + labs(x="Claims", y="Payment") + ggtitle("Rural Areas & Payment Correlation")


# we are filtering only zone 7  our pearson = 0.946404 showing that this has a high correlation
gotland <- filter(swedishInsurance, Zone == 7)
count(gotland, Zone)
cor(gotland$Claims, gotland$Payment, method = "spearman")

gotlandScatterPlot <- ggplot(gotland, aes(Claims, Payment))
gotlandScatterPlot + geom_point() + geom_smooth(method = "lm" , se= F) + labs(x="Claims", y="Payment") + ggtitle("Gotland & Payment Correlation")

count(swedishInsurance, Claims)


# create a bar chart showing which make has the highest correlation agaist the payment for make 
# then do the correlation between the claimed and insured agaist the payment 
cor(Make1$Claims, Make1$Payment, method = "spearman")
cor(Make2$Claims, Make2$Payment, method = "spearman")
cor(Make3$Claims, Make3$Payment, method = "spearman")
cor(Make4$Claims, Make4$Payment, method = "spearman")
cor(Make5$Claims, Make5$Payment, method = "spearman")
cor(Make6$Claims, Make6$Payment, method = "spearman")
cor(Make7$Claims, Make7$Payment, method = "spearman")
cor(Make8$Claims, Make8$Payment, method = "spearman")
cor(Make9$Claims, Make9$Payment, method = "spearman")

# make 4 highest 
# make 7 the lowest

cor(kilometres1$Claims, kilometres1$Payment, method = "pearson")
cor(kilometres2$Claims, kilometres2$Payment, method = "pearson")
cor(kilometres3$Claims, kilometres3$Payment, method = "pearson")
cor(kilometres4$Claims, kilometres4$Payment, method = "pearson")
cor(kilometres5$Claims, kilometres5$Payment, method = "pearson")


make4Correlation <- ggplot(Make4, aes(Claims, Payment))
make4Correlation + geom_point() + geom_smooth(method = "lm" , se= F) + labs(x="Claims", y="Payment") + ggtitle("Make 4 & Payment Correlation")


make7Correlation <- ggplot(Make7, aes(Claims, Payment))
make7Correlation + geom_point() + geom_smooth(method = "lm" , se= F) + labs(x="Claims", y="Payment") + ggtitle("Make 7 & Payment Correlation")

cor(Make4$Claims, Make4$Payment, method = "spearman")
cor(Make9$Claims, Make9$Payment, method = "spearman")
cor(Make4$Claims, Make4$Payment, method = "pearson")


make8Correlation <- ggplot(Make8, aes(Claims, Payment))
make8Correlation + geom_point() + geom_smooth(method = "lm" , se= F) + labs(x="Claims", y="Payment") + ggtitle("Make 8 & Payment Correlation")
cor(Make8$Claims, Make8$Payment, method = "pearson")



kilometres2Correlation <- ggplot(kilometres2, aes(Claims, Payment))
kilometres2 + geom_point() + geom_smooth(method = "lm" , se= F) + labs(x="Claims", y="Payment") + ggtitle("Zone 2 & Payment Correlation")
cor(kilometres2$Claims, kilometres2$Payment, method = "pearson")



kilometres5Correlation <- ggplot(kilometres5, aes(Claims, Payment))
kilometres5Correlation + geom_point() + geom_smooth(method = "lm" , se= F) + labs(x="Claims", y="Payment") + ggtitle("Zone 5 & Payment Correlation")
cor(kilometres5$Claims, kilometres5$Payment, method = "pearson")


claimspaymentcorrelation <- ggplot(swedishInsurance, aes(Claims, Payment))
claimspaymentcorrelation + geom_point() + geom_smooth(method = "lm" , se= F) + labs(x="Claims", y="Payment") + ggtitle("Claims & Payment Correlation")
cor(swedishInsurance$Claims, swedishInsurance$Payment, method = "spearman")



insuredpaymentcorrelation <- ggplot(swedishInsurance, aes(Insured, Payment))
insuredpaymentcorrelation + geom_point() + geom_smooth(method = "lm" , se= F) + labs(x="Insured", y="Payment") + ggtitle("Insured & Payment Correlation")
cor(swedishInsurance$Insured, swedishInsurance$Payment, method = "spearman")

# correlation between kilometers with the highest and lowest and do the above 



# we are filtering only zone 3 and zone 5 and our pearson = 0.9961211 showing that this has a high correlation
smallCities <- filter(swedishInsurance, Zone == 3 | Zone == 5)
cor(smallCities$Claims, smallCities$Payment, method = "pearson")
cor(smallCities$Claims, smallCities$Payment, method = "spearman")

plot(smallCities$Claims, smallCities$Payment, main = "Small Cities",
     xlab = "Small Cities Claims", ylab = "Small Cities Total Payment",
     pch = 19) + lines(lowess(smallCities$Claims,smallCities$Payment), col="blue") # lowess line (x,y)

smallCitiesScatterPlot <- ggplot(smallCities, aes(Claims, Payment))
smallCitiesScatterPlot + geom_point() + geom_smooth(method = "lm" , se= F) + labs(x="Claims", y="Payment") + ggtitle("Small Cities & Payment Correlation")



# for loop for the correlation analysis comparing the payment variable and all the other variables and then returning the pearson value 




# not happy with the correlation as i am trying to use a chacterastic variable and a numberical variable 
#  person -0.1208864
cor(swedishInsurance$Kilometres, swedishInsurance$Payment, method = "pearson")
kilometresPaymentScatterPlot <- ggplot(swedishInsurance, aes(Kilometres, Payment))
kilometresPaymentScatterPlot + geom_point() + geom_smooth(method = "lm" , se= F) + labs(x="Kilometres", y="Payment") + ggtitle("Kilometres & Payment Correlation")



# not happy with the correlation as i am trying to use a chacterastic variable and a numberical variable
#  person -0.1026947
cor(swedishInsurance$Zone, swedishInsurance$Payment, method = "pearson")


# not happy with the correlation as i am trying to use a chacterastic variable and a numberical variable
#  person 0.1180327
cor(swedishInsurance$Bonus, swedishInsurance$Payment, method = "pearson")


# happy with this as i am using a numberical variable agaist a numberical variable
#  person 0.2435393
cor(swedishInsurance$Make, swedishInsurance$Payment, method = "pearson")


# happy with this as i am using a numberical variable agaist a numberical variable
#  person 0.933217
cor(swedishInsurance$Insured, swedishInsurance$Payment, method = "pearson")
cor(Make1$Make, Make1$Payment, method = "pearson")
insuredPaymentScatterPlot <- ggplot(Make1, aes(Insured, Payment))
insuredPaymentScatterPlot + geom_point() + geom_smooth(method = "lm" , se= F) + labs(x="Make 1", y="Payment") + ggtitle("Insured & Payment Correlation")



# happy with this as i am using a numberical variable agaist a numberical variable
#  person 0.9954003
cor(swedishInsurance$Claims, swedishInsurance$Payment, method = "pearson")
cor(swedishInsurance$Claims, swedishInsurance$Payment, method = "spearman")




cor(smallCities$Claims, smallCities$Payment, method = "spearman")












# REGRESSION MODELING


# make and payment model

cor(swedishInsurance$Make, swedishInsurance$Payment, method = 'pearson')

makeAndPaymentModel <- lm(swedishInsurance$Payment ~ swedishInsurance$Claims)

summary(newModel)

ggplot(swedishInsurance, aes(Claims, Payment)) + geom_point() + geom_point(aes(y = lm(Payment ~ Claims, data = swedishInsurance)$fitted.values), colour = 'blue')

# bonus and payment model

cor(swedishInsurance$Bonus, swedishInsurance$Payment, method = 'pearson')

bonusAndPaymentModel <- lm(swedishInsurance$Payment ~ swedishInsurance$Bonus)

summary(bonusAndPaymentModel)

ggplot(swedishInsurance, aes(Bonus, Payment)) + geom_point() + geom_point(aes(y = lm(Payment ~ Bonus, data = swedishInsurance)$fitted.values), colour = 'blue')


# insured and payment model

cor(swedishInsurance$Insured, swedishInsurance$Payment, method = 'pearson')

bonusAndPaymentModel <- lm(swedishInsurance$Payment ~ swedishInsurance$Insured)

summary(bonusAndPaymentModel)

ggplot(swedishInsurance, aes(Insured, Payment)) + geom_point() + geom_point(aes(y = lm(Payment ~ Insured, data = swedishInsurance)$fitted.values), colour = 'blue')  + geom_smooth(method = "lm" , se= F) + labs(x="Insured", y="Payment") + ggtitle("Insured & Payment Regression Model")


# kilometres and payment model

cor(swedishInsurance$Kilometres, swedishInsurance$Payment, method = 'pearson')

kilometresAndPaymentModel <- lm(swedishInsurance$Payment ~ swedishInsurance$Kilometres)

summary(kilometresAndPaymentModel)

ggplot(swedishInsurance, aes(Kilometres, Payment)) + geom_point() + geom_point(aes(y = lm(Payment ~ Kilometres, data = swedishInsurance)$fitted.values), colour = 'blue')

# zone and payment model

cor(swedishInsurance$Zone, swedishInsurance$Payment, method = 'pearson')

kilometresAndPaymentModel <- lm(swedishInsurance$Payment ~ swedishInsurance$Zone)

summary(kilometresAndPaymentModel)

ggplot(swedishInsurance, aes(Zone, Payment)) + geom_point() + geom_point(aes(y = lm(Payment ~ Zone, data = swedishInsurance)$fitted.values), colour = 'blue') + geom_smooth(method = "lm" , se= F)


# claims and payment model this is the one with the regression line 

cor(swedishInsurance$Claims, swedishInsurance$Payment, method = 'pearson')

claimsAndPaymentModel <- lm(swedishInsurance$Payment ~ swedishInsurance$Claims)

summary(claimsAndPaymentModel)

ggplot(swedishInsurance, aes(Claims, Payment)) + geom_point() + geom_point(aes(y = lm(Payment ~ Claims, data = swedishInsurance)$fitted.values), colour = 'blue') + geom_smooth(method = "lm" , se= F) + labs(x="Claims", y="Payment") + ggtitle("Claims & Payment Regression Model")    



# claims and payment model this is the one with the regression line 

cor(swedishInsurance$Claims, swedishInsurance$Payment, method = 'pearson')

claimsAndPaymentModel <- lm(swedishInsurance$Payment ~ swedishInsurance$Claims)

summary(claimsAndPaymentModel)

ggplot(swedishInsurance, aes(Claims, Payment)) + geom_point() + geom_point(aes(y = lm(Payment ~ Claims, data = swedishInsurance)$fitted.values), colour = 'blue') + geom_smooth(method = "lm" , se= F) + labs(x="Claims", y="Payment") + ggtitle("Claims & Payment Regression Model")    




# Multiple linear regression model 


model <- lm(Payment ~ Kilometres + Zone + Bonus + Make + Insured + Claims, data = swedishInsurance)

summary(model)

summary(model)$coefficient


step(model, direction = "backward")

firstModel <- lm(Payment ~ 1, data = swedishInsurance)

step(model, direction = "forward", scope = formula(model))

step(model, direction = "both", scope = formula(model))

plyr::count(rstandard(lm(Payment ~. , data = swedishInsurance)) > 3.29)
plyr::count(rstandard(lm(Payment ~. , data = swedishInsurance)) > 2.58)
plyr::count(rstandard(lm(Payment ~. , data = swedishInsurance)) > 1.96)

secondModel <- lm(Claims ~ Payment + Zone + Bonus + Make + Insured + Kilometres, data = swedishInsurance)

summary(secondModel)

summary(secondModel)$coefficient

plyr::count(rstandard(lm(Claims ~. , data = swedishInsurance)) > 3.29)
plyr::count(rstandard(lm(Claims ~. , data = swedishInsurance)) > 2.58)
plyr::count(rstandard(lm(Claims ~. , data = swedishInsurance)) > 1.96)



