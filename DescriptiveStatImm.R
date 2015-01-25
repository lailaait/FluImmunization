## STATS DESCRIPTIVES ##

setwd("C:/Users/Laila/Desktop/R")
dat <- read.csv("obesity_flu_absences_merged.csv")
summary(dat)

dat <- na.omit(dat)

summary(dat)

describe(dat)
#8639 observations total on the 3 years

datbis2011 <- subset(dat, dat$year==2011)
describe(datbis2011)
#1916 observations

datbis2012 <- subset(dat, dat$year==2012)
describe(datbis2012)
#3030 observations

datbis2013 <- subset(dat, dat$year==2013)
describe(datbis2013)
#3693 observations

library(Hmisc)
describe(dat)

## 1. Ethnic characteristics

# Weight type per type of race
attach(dat)
mytable <- table(dat$race,dat$cat) # A will be rows, B will be columns 
mytable # print table 

margin.table(mytable, 1) # A frequencies (summed over B) 
margin.table(mytable, 2) # B frequencies (summed over A)

x <- round (prop.table(mytable, 1), digits = 4) * 100 # row percentages 
y <- round (prop.table(mytable, 2), digits = 4) * 100 # row percentages 

barplot(x, beside = T, legend = T)
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)

barplot(y, beside = T, legend = T)
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)

print(mytable)

# Proportions of weight categories per race
x <- round(prop.table(table(dat$cat, dat$race), margin = 2) * 100, digits = 1)
x     
barplot(x, beside = T, legend = T)
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)

# Proportions of weight categories among White and nonWhite
y <- round(prop.table(table(dat$cat, dat$race_rec), margin = 2) * 100, digits = 1)
y    
barplot(y, beside = T, legend = T)
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)

# Free lunches and ethnicity
attach(dat)
ethnfree <- table(dat$race,dat$lunch_rec) # A will be rows, B will be columns 
ethnfree # print table 

margin.table(ethnfree, 1) # A frequencies (summed over B) 
margin.table(ethnfree, 2) # B frequencies (summed over A)

prop.table(ethnfree) # cell percentages
z <- round (prop.table(ethnfree, 1), digits = 4) * 100 # row percentages 
t <- round (prop.table(ethnfree, 2), digits = 4) * 100 # row percentages 

print(ethnfree)

barplot(z, beside = T, legend = T)
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)


# Other freelunch graphs per ethnicity
y <- round(prop.table(table(dat$lunch, dat$race), margin = 2) * 100, digits = 1)
y     
barplot(y, beside = T, legend = T)
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)



# Consent form and race

#interpret the results with caution, almost 1 missing value out of 2
describe(cf)

d <- round(prop.table(table(dat$cf, dat$race), margin = 2) * 100, digits = 1)
d
barplot(d, beside = T, legend = T, xlab="Ethnicity", ylab="% of answers to consent form")
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)

# free lunch variable repartition

hist(lunch,
     xlab = "Free Lunch Variable Distribution",
     breaks = 60,
     xlim = c(0,10),
     col = adjustcolor("blue", alpha.f = 0.5),
     main = NA)


# 2. Health characteristics of children

# School level repartition per year
round(prop.table(table(dat$year, dat$type), 1) * 100, digits=2)
round(prop.table(table(dat$year, dat$type), 2) * 100, digits=2)

# BMI repartition per White/nonWhite
y <- round(prop.table(table(dat$cat, dat$race_rec), margin = 2) * 100, digits = 1)
y
barplot(y, beside = T, legend = T)
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)

# Quantiles population of children 
y <- round(prop.table(table(dat$type, dat$year), margin = 2) * 100, digits = 1)
y
barplot(y, beside = T, legend = T)
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)



# Age repartition of children

hist(dat$age /12,
     xlab = "Age",
     breaks = 100,
     xlim = c(0,20),
     col = adjustcolor("blue", alpha.f = 0.5),
     main = NA)


# Relationship b/w height and weight for children
plot(dat$height, dat$weight,
     col = adjustcolor("blue", alpha.f = 0.2),
     pch = 16,
     xlab = "Height (inches)",
     ylab = "Weight (pounds)")
points(dat$height, dat$weight,
       col = adjustcolor("black", alpha.f = 0.2))


# Quantiles population of children 
y <- round(prop.table(table(dat$cat, dat$year), margin = 2) * 100, digits = 1)
y
barplot(y, beside = T, legend = T)
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)

# 3. Immunization

(mean(imm_rate)) * 100
immratepctage <- imm_rate*100

# necessary to get rid of NA values before observing quantiles (since NA for mean value)
immratepctage <- na.omit(immratepctage)

boxplot(immratepctage,
        main = "Repartition of immunization",
        ylab = "Percentage of kids immunized")
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty=6)

hist(immratepctage,
        main = "Repartition of immunization",
        xlab = "Percentage of kids immunized",
        ylab = "Number of kids concerned")

quantile(immratepctage)

# Immunization rates per ethnicity

z <- round(prop.table(table(dat$imm, dat$race), margin = 2) * 100, digits = 1)
z     
barplot(z, beside = T, legend = T, xlab='Ethnicity', ylab='% kids immunized or not')
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)


# Consent form and age of children

d <- round(prop.table(table(dat$imm, dat$cf), margin = 2) * 100, digits = 1)
d
barplot(d, beside = T, legend = T, xlab="Answer to consent form", ylab="% of kids immunized")
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)


# Freelunch and immunization
b <- round(prop.table(table(dat$imm, dat$lunch_rec), margin = 2) * 100, digits = 1)
b     
barplot(b, beside = T, legend = T)
abline(h=seq(0,100,10), col = adjustcolor("black", alpha.f = 0.2),
       lty = 6)


# Absences flu and non-flu children
#### Starting with a regression to test a simple causality
probit1 <- glm(dat$imm ~ dat$cf + dat$imm_rate + dat$cf * dat$imm_rate, family = binomial(link=probit))

## model summary
summary(probit1)


# Absences flu and non-flu children

#idem, we have to get rid of the NA values before
imm_rate_complete <- na.omit(dat$imm_rate)
imm_rate_complete <- imm_rate_complete * 100
mean(imm_rate_complete)
quantile(imm_rate_complete)


# Corrélation flu absences et immunization (of the whole class / of the child)

ab_flu_complete <- na.omit(dat$ab_flu)
imm_complete <- na.omit(dat$imm)
ab_non_flu_complete <- na.omit(dat$ab_non_flu)

#cf_dummy <- as.numeric(dat$cf)
#cf_dummy_complete <- na.omit(cf_dummy)
#cor(ab_non_flu_complete, ab_flu_complete)


library(Hmisc)
rcorr(imm_complete, ab_flu_complete)
cor(imm_complete, ab_flu_complete)
cor(imm_complete, imm_rate_complete)
cor(ab_non_flu_complete, ab_flu_complete)


## GENERAL OBSERVATIONS

library(pastecs)

age_years <- age_months/12

obsvar1<-cbind(ab_flu_complete, ab_non_flu_complete)
options(scipen=100)
options(digits=2)
## for complete descr stats (i didn't want that much info though)
## stat.desc(varobserved)
stat.desc(obsvar1, basic=F)

obsvar2<-cbind(bmi, grade, age_years, height, weight)
options(scipen=100)
options(digits=2)
stat.desc(obsvar2, basic=F)