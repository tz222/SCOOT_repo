library(psych)
library(corrplot)
library(GPArotation)
library(lavaan)
#library(semPlot)

load("scoot_cfa.RData")
psychdf <- temp_merge[as.numeric(as.character(temp_merge$q10)) <=20 & as.numeric(as.character(temp_merge$q15))<20,
                      c(1,24:55)]
colnames(psychdf)[2:7] <- c('SN1','SN2','SN3','SN4','SN5','SN6')
colnames(psychdf)[8:12] <- c('EP1','EP2','EP3','EP4','EP5')
colnames(psychdf)[13:16] <- c('EB1','EB2','EB3','EB4')
colnames(psychdf)[17:19] <- c('PR1','PR2','PR3')
colnames(psychdf)[20:23] <- c('RE1','RE2','RE3','RE4')
colnames(psychdf)[24:27] <- c('EV1','EV2','EV3','EV4')
colnames(psychdf)[28:30] <- c('AA1','AA2','AA3')
colnames(psychdf)[31:33] <- c('IU1','IU2','IU3')

agree <- function(x) {
  if(x=='Strongly agree') 6
  else if (x=='Agree') 5
  else if (x=='Slightly agree') 4
  else if(x=='Slightly disagree') 3
  else if(x=='Disagree') 2
  else  1}

psycode <- psychdf
#psycode[c(2,3,5,6,8:12,15,19,21,23:31,33)] <- lapply(psycode[c(2,3,5,6,8:12,15,19,21,23:31,33)],agree)
colsagree <- c(2,3,5,6,8:12,15,19,21,23:31,33)
for( i in colsagree){
  #print(i)
  psycode[,i] <- sapply(psycode[,i], agree )
}

more <- function(x) {
  if(x=='A lot more') 6
  else if (x=='More') 5
  else if (x=='Slightly more') 4
  else if(x=='Slightly fewer') 3
  else if(x=='Fewer') 2
  else  1}

psycode$SN3<- sapply(psycode$SN3, more)

positive <- function(x) {
  if(x=='Very positive') 6
  else if (x=='Positive') 5
  else if (x=='Slightly positive') 4
  else if(x=='Slightly negative') 3
  else if(x=='Negative') 2
  else  1}
psycode$SN6<- sapply(psycode$SN6, positive)

likely <- function(x) {
  if(x=='Very likely') 6
  else if (x=='Likely') 5
  else if (x=='Slightly likely') 4
  else if(x=='Slightly unlikely') 3
  else if(x=='Unlikely') 2
  else  1}

psycode$EB1 <- psychdf$EB1
psycode$EB1<- sapply(psycode$EB1, likely)
psycode$EB2<- sapply(psycode$EB2, likely)

increase <- function(x) {
  if(x=='Significantly increase') 6
  else if (x=='Increase') 5
  else if (x=='Slightly increase') 4
  else if(x=='Slightly decrease') 3
  else if(x=='Decrease') 2
  else  1}
psycode$EB4<- sapply(psycode$EB4, increase)

safe <- function(x) {
  if(x=='Very safe') 6
  else if (x=='Safe') 5
  else if (x=='Somewhat safe') 4
  else if(x=='Somewhat unsafe') 3
  else if(x=='Unsafe') 2
  else  1}
psycode$PR1<- sapply(psycode$PR1, safe)
psycode$PR2<- sapply(psycode$PR2, safe)

psycode$RE1 <- psychdf$RE1
psycode$RE2 <- psychdf$RE2
psycode$RE2<- sapply(psycode$RE2, agree)
psycode$RE1<- ifelse(psycode$RE1=='Yes', 1,0)
psycode$RE3<- ifelse(psycode$RE3=='Yes', 1,0)

would <- function(x) {
  if(x=='Definetely would') 6
  else if (x=='Would') 5
  else if (x=='Probably would') 4
  else if(x=='Probably would not') 3
  else if(x=='Would not') 2
  else  1}
psycode$IU2 <- psychdf$IU2
psycode$IU2<- sapply(psycode$IU2, would)

psycl <-psycode[,-c(1,31:33)]
psycl <-psycode[,-c(1)]

describe(psycl)
cor(psycl)
corrplot(cor(psycl), method="color")
KMO(psycl)


#scree plot
fa.parallel(psycl)

# efa
mod.efa8 <- fa(psycl,nfactors=8,rotate = "promax",fm='ml') # allow correlation between laten factors
print(mod.efa8)
summary(mod.efa8)
mod.efa8
fa.diagram(mod.efa8)
print(mod.efa8$loadings,cutoff = 0.5)

#mod.efa7nc <- fa(psycl,nfactors=8,rotate = "varimax",fm='pa') # not allow correlation between laten factors
#print(mod.efa7nc)
#fa.diagram(mod.efa7nc)

# use factanal
mod.efa8fact <- factanal(psycl,8,rotation='promax',fm='ml')# allow correlation between laten factors

#fa.diagram(mod.efa8fact )
print(mod.efa8fact , digits=3, cutoff=.3, sort=TRUE)

# use factanal
#mod.efa8factvar <- factanal(psycl,8,rotation='varimax')
#fa.diagram(mod.efa8fact )
#print(mod.efa8factvar , digits=3, cutoff=.5, sort=TRUE)

#cfa
lavaan::cor2cov
cfa.mod <- 'SN =~ SN1 + SN2 + SN3 + SN4 + SN5 + SN6
            EP =~ EP1 + EP2 + EP3 + EP4 + EP5 + EB1
            EB =~ EB1 + EB2 + EB3 + EB4
            PR =~ PR1 + PR2 + PR3 + AA2
            RE =~ RE2 + RE4 + EP4 + EP5
            EV =~ EV1 + EV2 + EV3 + EV4
            AA =~ AA1 + AA2 + AA3 + SN3 + SN5 + EB4 + EV4 + PR3 + EP1 + IU3 +EV1
            IU =~ IU1 + IU2 + IU3
            SN1 ~~ SN2
            EV1 ~~ EV2
            EB1 ~~ EB2
            RE2 ~~ RE4'

cfa.mod2 <-'SN =~ SN1 + SN2 
            EP =~ EP2 + EP3 + EP5
            EB =~ EB1 + EB2  
            PR =~ PR1 + PR2 + PR3
            RE =~ RE2 + RE4 
            EV =~ EV1 + EV2 + EV3 
            AA =~ AA1 + AA2 + AA3
            '
#cfa.fit <- cfa(cfa.mod, data=psycl)
#summary(cfa.fit)
#fa.diagram(cfa.fit)


cfa.fit <- sem(cfa.mod,
               data=psycl)
summary(cfa.fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
fitted(cfa.fit)
modindices(cfa.fit, sort=TRUE)

saveRDS(cfa.fit, 'cfa_fit.rds')

cfa.fit2 <- sem(cfa.mod2,
               data=psycl)
summary(cfa.fit2, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# specify path model
reg.model <- '
            SN =~ SN1 + SN2 + SN3 + SN4 + SN5 + SN6
            EP =~ EP1 + EP2 + EP3 + EP4 + EP5 + EB1
            EB =~ EB1 + EB2 + EB3 + EB4
            PR =~ PR1 + PR2 + PR3 + AA2
            RE =~ RE2 + RE4 + EP4 + EP5
            EV =~ EV1 + EV2 + EV3 + EV4
            AA =~ AA1 + AA2 + AA3 + SN3 + SN5 + EB4 + EV4 + PR3 + EP1 + IU3 +EV1
            IU =~ IU1 + IU2 + IU3
            SN1 ~~ SN2
            EV1 ~~ EV2
            EB1 ~~ EB2
            RE2 ~~ RE4
            # regressions
            IU ~ SN+EP+EB+PR+EV+AA+RE'

reg.model2 <-'SN =~ SN1 + SN2 + SN3 + SN4 + SN5 + SN6
            EP =~ EP1 + EP2 + EP3 + EP4 + EP5
            EB =~ EB1 + EB2 + EB3 + EB4
            PR =~ PR1 + PR2 + PR3
            RE =~ RE2 + RE4
            EV =~ EV1 + EV2 + EV3 + EV4
            AA =~ AA1 + AA2 + AA3
            IU =~ IU1 + IU2 + IU3
# regressions
            IU ~ SN+EP+EB+PR+EV+AA+RE'

model <- sem(reg.model,
             data=psycl)
summary(model, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

model2 <- sem(reg.model2,
             data=psycl)
summary(model2, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

save(temp_merge, psychdf,psycode, psycl, file = "scoot_cfa.RData")
