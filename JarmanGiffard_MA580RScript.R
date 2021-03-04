usRp <-"https://cran.csiro.au/"

if(!require(ggplot2))   install.packages("ggplot2"  ,repos = usRp)
if(!require(dplyr))     install.packages("dplyr"    ,repos = usRp)
if(!require(tidyverse)) install.packages("tidyverse",repos = usRp)
if(!require(backports)) install.packages("backports",repos = usRp)
if(!require(ggpubr))    install.packages("ggpubr"   ,repos = usRp)
if(!require(evaluate))  install.packages("evaluate" ,repos = usRp)
if(!require(magrittr))  install.packages("magrittr" ,repos = usRp)
if(!require(qqplotr))   install.packages("qqplotr"  ,repos = usRp)
if(!require(psych))     install.packages("psych"    ,repos = usRp) #install PSYCH pack
if(!require(tidyr))     install.packages("tidyr"    ,repos = usRp)
if(!require(sqldf))     install.packages("sqldf"    ,repos = usRp)
if(!require(textir))    install.packages("textir"   ,repos = usRp)
if(!require(geoR))      install.packages("geoR"     ,repos = usRp)
if(!require(moments))   install.packages("moments"  ,repos = usRp)
if(!require(mblm))      install.packages("mblm"     ,repos = usRp)
if(!require(countrycode)) install.packages("countrycode"     ,repos = usRp)
if(!require(forcats))  install.packages("forcats"   ,repos = usRp)
if(!require(ggeasy))   install.packages("ggeasy"    ,repos = usRp)
if(!require(lattice))  install.packages("lattice"   ,repos = usRp)
if(!require(MASS))     install.packages("MASS"      ,repos = usRp)
if(!require(epitools)) install.packages("epitools"  ,repos = usRp)
if(!require(lmtest))   install.packages("lmtest"    ,repos = usRp)
if(!require(robust))   install.packages("robust"    ,repos = usRp)
if(!require(boot))     install.packages("boot"      ,repos = usRp)
if(!require(car))      install.packages("car"       ,repos = usRp)
if(!require(caret))    install.packages("caret"     ,repos = usRp)

library(ggplot2)
library(dplyr)
library(tidyverse)
library(backports)
library(ggpubr)
library(evaluate)
library(magrittr)
library(qqplotr)
library(psych)        #assign PSYCH pack
library(tidyr)
library(sqldf)
library(textir)
library(geoR)
library(moments)
library(mblm)
library(countrycode)
library(forcats)
library(ggeasy)
library(lattice)
library(MASS)
library(epitools)
library(lmtest)
library(robust)
library(boot)
library(car)
library(caret)

par(mfrow=c(1,4))

#====================================================================================================================================#

#-------------
# Import Data
#-------------

loc_DS_HDI                      <- "C:/Users/jarma/OneDrive/Documents/Uni/Stats/Assignment3/hdi_human_development_index.csv"
loc_DS_PlaneCrashes_Affected    <- "C:/Users/jarma/OneDrive/Documents/Uni/Stats/Assignment3/plane_crash_affected_annual_number.csv"
loc_DS_PlaneCrashes_Deaths      <- "C:/Users/jarma/OneDrive/Documents/Uni/Stats/Assignment3/plane_crash_deaths_annual_number.csv"
export_loc                      <- "C:/Users/jarma/OneDrive/Documents/Uni/Stats/Assignment3/SummaryExport.xlsx"

DS_HDI <- read.csv(loc_DS_HDI)   #import HDI
DS_CRASH_AFF <- read.csv(loc_DS_PlaneCrashes_Affected)  #import Crash Affected
DS_CRASH_DTH <- read.csv(loc_DS_PlaneCrashes_Deaths)    #import Crash Death

DS_HDI         #view HDI
DS_CRASH_AFF   #view Crash Affected
DS_CRASH_DTH   #view Crash Death

#====================================================================================================================================#

#----------------------------------------------
# Validate dataset formats, and brief summaries
#----------------------------------------------


str(DS_HDI)             #confirm the data has imported in correct variable format (i.e. not char)
summary(DS_HDI)         #basic summary statistics
describe(DS_HDI)        #a few more summary statistics using PSYCH

str(DS_CRASH_AFF)       #confirm the data has imported in correct variable format (i.e. not char)
summary(DS_CRASH_AFF)   #basic summary statistics
describe(DS_CRASH_AFF)  #a few more summary statistics using PSYCH

str(DS_CRASH_DTH)       #confirm the data has imported in correct variable format (i.e. not char)
summary(DS_CRASH_DTH)   #basic summary statistics
describe(DS_CRASH_DTH)  #a few more summary statistics using PSYCH


TRANS_HDI       <- DS_HDI %>% pivot_longer(-country, names_to = "year", values_to = "hdi")
TRANS_CRASH_AFF <- DS_CRASH_AFF %>% pivot_longer(-country, names_to = "year", values_to = "DeathsInjuriesPerYear")
TRANS_CRASH_DTH <- DS_CRASH_DTH %>% pivot_longer(-country, names_to = "year", values_to = "DeathsPerYear")

#====================================================================================================================================#

#------------------------------
# Final Clean Datasets - merged
#------------------------------

fullMerge <- sqldf(" SELECT  HDI.*
                            ,AFF.DeathsInjuriesPerYear
                            ,DTH.DeathsPerYear
                     FROM       TRANS_HDI       as HDI
                     INNER JOIN TRANS_CRASH_AFF as AFF  on    HDI.YEAR    = AFF.YEAR
                                                          and HDI.COUNTRY = AFF.COUNTRY
                     INNER JOIN TRANS_CRASH_DTH as DTH  on    HDI.YEAR    = DTH.YEAR
                                                          and HDI.COUNTRY = DTH.COUNTRY
                     ORDER BY YEAR")

fullMerge1      <- transform(fullMerge,CleanYear = substr(year,2,5))
CharYear_ToNum  <- as.numeric(fullMerge1$CleanYear,replace = T)
MergeBack       <- data.frame(fullMerge1,CharYear_ToNum)

#------- Before Removing zeros from   -------#
#Identify observations that are not recorded properly
#Histogram of illogical observations
ggplot(fullMerge1, aes(x=DeathsPerYear)) +
  geom_histogram(colour="dodgerblue",fill=rgb(1,.54,0,.7), bins = 30) +
  scale_y_continuous(name="count") +
  labs(title="Histogram of number of people killed in air accidents from 1970 to 2008")


fullMerge1_final <- sqldf(" SELECT  a.country
                              ,CharYear_ToNum as Year
                              ,case   when a.hdi <= 0.333                     then '1'
                                      when a.hdi > 0.333 and a.hdi <= 0.666   then '2'
                                      when a.hdi > 0.666 and a.hdi <= 1       then '3'
                               else '4' end as HDI_Banding
                              ,case   when CharYear_ToNum between 1990 and 1994    then '1990 - 1994'
                                      when CharYear_ToNum between 1995 and 1999    then '1995 - 1999'
                                      when CharYear_ToNum between 2000 and 2004    then '2000 - 2004'
                               else '2005 +' end as Year_Banding
                              ,case   when (DeathsInjuriesPerYear - DeathsPerYear) > 0 then 1 else 0 end as Survived
                              ,case   when CharYear_ToNum between 1990 and 1999    then '1990 - 1999'
                                      when CharYear_ToNum between 2000 and 2008    then '2000 - 2008'
                               else '' end as Decade_Banding
                              ,count(DeathsInjuriesPerYear) as Count_Crashes
                              ,DeathsInjuriesPerYear
                              ,DeathsPerYear
                              ,(DeathsInjuriesPerYear - DeathsPerYear)            as InjuriesPerYear
                              ,cast(DeathsPerYear as float)/DeathsInjuriesPerYear as RatioOfDeath
                      FROM MergeBack as a
                      WHERE a.hdi is not null
                        and DeathsPerYear > 0
                          group by country,Year,HDI_Banding,Year_Banding,Survived,Decade_Banding")

fullMerge1_final

fullMerge1_final$continent <- countrycode(sourcevar = fullMerge1_final[, "country"],
                         origin = "country.name",
                         destination = "continent")

seed <- set.seed(1234)
Random_Sample <- sample(1:nrow(fullMerge1_final), 100)
fullMerge2 <- fullMerge1_final[Random_Sample, ]
fullMerge2


# Deaths per year by continent Bar chart:
fullMerge2 %>%
  mutate(name = fct_reorder(continent, desc(DeathsPerYear))) %>%
  ggplot( aes(x=continent, y=DeathsPerYear)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  ylab("Deaths Per Year") +
  xlab("Continents") +
  ggtitle("Plane Crash Deaths Per Year By Continents") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Deaths per year by continent Bar chart:
fullMerge2 %>%
  mutate(name = fct_reorder(HDI_Banding, desc(DeathsPerYear))) %>%
  ggplot( aes(x=HDI_Banding, y=DeathsPerYear)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  ylab("Deaths Per Year") +
  xlab("HDI Banding") +
  ggtitle("Plane Crash Deaths Per Year By HDI Banding") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#====================================================================================================================================#
# Hypothesis Testing
#====================================================================================================================================#

#------- Objective 1: --------#
# H0: Mean of plane crash deaths for HDI Banding 1, 2 and 3 are equal
# H1: Mean of plane crash deaths for HDI Banding 1, 2 and 3 are NOT equal
#-----------------------------#

ggqqplot(fullMerge2$DeathsPerYear, ylab="Deaths from Plane Crash Samples (#)") #distribution check
shapiro.test(fullMerge2$DeathsPerYear) #Normality test - Not normally distributed

histogram(~ DeathsPerYear | HDI_Banding,
          data=fullMerge2,
          layout=c(1,3))

kruskal.test(DeathsPerYear ~ HDI_Banding, data = fullMerge2) # REJECT NULL HYPOTHESIS

# RESULT: With significance level of 0.05, there is not enough evidence to reject null hypothesis with p-value of 0.6851
#         and conclude that mean is same for each HDI banding which is consistent with historgram we generated
#------------------------------------

#====================================================================================================================================#

#------- Objective 2: --------#
# H0: Mean of plane crash deaths for HDI Banding 1, 2 and 3 are equal
# H1: Mean of plane crash deaths for HDI Banding 1, 2 and 3 are NOT equal
#-----------------------------#

# Summarise data by
fullMerge2_chisq <- sqldf("SELECT  Year_Banding
                             ,Survived
                             ,sum(DeathsPerYear) as DeathsPerYear
                      FROM fullMerge2
                      where HDI_Banding <> '1'
                      group by Year_Banding, Survived")

fullMerge2_chisq

Chisq_table <- matrix(c(555,786,527,770,960,1410,834,525),nrow=4,ncol=2)
Chisq_table
chisq.test(Chisq_table, correct=FALSE)

prop.test(Chisq_table) # Proportion for groups 1 to 4

# RESULT: With p-value of 2.2e-16, we conclude that null hypothesis is rejected and conclude year and Survival in plane crash are dependent
#------------------------------------

#====================================================================================================================================#

#------- Objective 3: --------#
# H0: Mean of plane crash deaths for HDI Banding 1, 2 and 3 are equal
# H1: Mean of plane crash deaths for HDI Banding 1, 2 and 3 are NOT equal
#-----------------------------#

#-----------------------------#
# Logistic regression
#-----------------------------#
Decade_1990=fullMerge2$Decade_Banding=="1990 - 1999"
Decade_2000=fullMerge2$Decade_Banding=="2000 - 2008"

Decade_1990=as.numeric(Decade_1990)
Decade_2000=as.numeric(Decade_2000)

HDI_1=fullMerge2$HDI_Banding=="1"
HDI_2=fullMerge2$HDI_Banding=="2"
HDI_3=fullMerge2$HDI_Banding=="3"

HDI_1=as.numeric(HDI_1)
HDI_2=as.numeric(HDI_2)
HDI_3=as.numeric(HDI_3)

glm_model1 <- glm(Survived~HDI_1,data = fullMerge2, family = "binomial")  #REJECT
summary(glm_model1)

glm_model2 <- glm(Survived~HDI_2,data = fullMerge2, family = "binomial")  #REJECT
summary(glm_model2)

glm_model3 <- glm(Survived~HDI_3,data = fullMerge2, family = "binomial")  #REJECT
summary(glm_model3)

glm_model4 <- glm(Survived~Decade_1990,data = fullMerge2, family = "binomial")  #ACCEPT
summary(glm_model4)

glm_model5 <- glm(Survived~Decade_2000,data = fullMerge2, family = "binomial")  #ACCEPT
summary(glm_model5)


confint.default(glm_model4) # Confidence Intervals - Model 4
confint.default(glm_model5) # Confidence Intervals - Model 5

ggqqplot(fullMerge2$Count_Crashes, ylab="Deaths from Plane Crash Samples (#)") #distribution check

#====================================================================================================================================#

###############################                                THE END                             ###################################

#====================================================================================================================================#

# Summarise data by
test <- sqldf("SELECT  count (country) as country
                      FROM fullmerge1_final")
test


#------- Attempt to Linear regression -------#

ggqqplot(fullMerge2$Count_Crashes, ylab="Deaths from Plane Crash Samples (#)") #distribution check


plot(fullMerge2$DeathsPerYear)
boxplot(fullMerge2$DeathsPerYear ~ fullMerge2$HDI_Banding, xlab = "HDI Banding", ylab="Deaths from Plane Crash (#)")

shapiro.test(fullMerge2$DeathsPerYear) #Normality test
skewness(fullMerge2$DeathsPerYear)     # Skewness before transformation


hist(fullMerge2$DeathsPerYear, xlab="Deaths Per Year", main="Before Log Transformation")         # Shows if the data is normally distributed - it is right skewed
hist(log(fullMerge2$DeathsPerYear), xlab="Deaths Per Year - Log Transformation", main="After Log Transformation")  # Checking to see if log10 transformation is normally distributed

summary(fullMerge2$DeathsPerYear)
summary(log(fullMerge2$DeathsPerYear))

#****** Log Transformation *****

fullMerge2$Log_DeathsPerYear         <- log(fullMerge2$DeathsPerYear)
fullMerge2$Log_InjuriesPerYear <- log(fullMerge2$InjuriesPerYear)

#------- After Transformation -------#

shapiro.test(fullMerge2$Log_DeathsPerYear) #Normality test
skewness(fullMerge2$Log_DeathsPerYear) # Skewness after transformation

par(mfrow=c(1,1))
hist(log(fullMerge2$DeathsPerYear), xlab="Deaths Per Year - Log Transformation", main="After Log Transformation")  # Checking to see if log10 transformation is normally distributed

plot(log(fullMerge2$DeathsPerYear), ylab="Deaths Per Year - log Transformation")

#-----------------------------#
# Estimate the parameters
#-----------------------------#

ggplot(fullMerge2, aes(Year_Banding, Survived)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

## Model Creation
model <- glm(Survived ~ Count_Crashes,family=binomial,data=fullMerge2)
summary(model)

anova(model, test="Chisq")

ci = confint.default(model); ci

# Summarise data by
model1 <- lm(DeathsPerYear ~ Decade_Banding+HDI_Banding, data=fullMerge2)
summary(model1)

model2 <- lm(InjuriesPerYear ~ HDI_Banding, data=fullMerge2)
summary(model2)

summary(lm(InjuriesPerYear ~ I(HDI_Banding == 1) + I(HDI_Banding == 2) + I(HDI_Banding == 3), data = fullMerge2))

predict(model2, interval = "confidence", level = 0.95)

Deaths.fitted <- fitted(model2)

plot(Deaths.fitted, fullMerge2$InjuriesPerYear, xlab="Fitted Deaths from plane crash (#)", ylab="Actual Deaths from plane crash (#)")
abline(0,1, lty=2)

Deaths.resid <- resid(model2)

qqnorm(Deaths.resid)
qqline(Deaths.resid)

plot(Deaths.resid, type="b", ylab="Residuals")
abline(h=0, lty=2, col="grey")

dwtest(model1)

plot(Deaths.fitted, Deaths.resid, xlab="Fitted Deaths from plane crash (#)", ylab="Residual")
abline(h=0, lty=2)

Anova(model1, model2)

#------- Attempt to try Robust regression -------#

robust_model = lmRob(formula = Log_DeathsPerYear ~ HDI_Banding, data=fullMerge2)
summary(robust_model)

summary(rr.huber <- rlm(formula = Log_DeathsPerYear ~ HDI_Banding, data=fullMerge2))

fit.mod = dynlm(Log_DeathsPerYear ~ HDI_Banding)

fit.compare = fit.models(list(Robust = "lmRob",
                              "LS" = "lm"), formula = Log_DeathsPerYear ~ HDI_Banding, data = fullMerge2)
summary(fit.compare)


#====================================================================================================================================#