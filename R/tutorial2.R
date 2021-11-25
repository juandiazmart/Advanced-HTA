setwd("~/Advanced HTA/R")

babies <- read.csv('babies.csv',header=T, sep=",", 
                   na.strings=c("", "998", "999"))

dim(babies)
names(babies)

babies[,"id"] # babies[,1]
babies[,c("id","dage")]
babies[1:10,]

###############################

dad.over.40<- which(babies$dage>=40)
d<-babies[dad.over.40, c("age","dage","marital")]
dim(d)
# can also call the function directly
# View(d)

summary(babies$smoke)

babies$smokeCat <- factor(babies$smoke,
                          levels = c(0,2,3,1),
                          labels = c("Never smoker","Past smoker","Past smoker","Current smoker"))

table(babies$smoke,babies$smokeCat,useNA = "always")

#with(babies, table(smoke, smokeCat,useNA = 'always'), exclude=NULL)

babies$smokeNow <- ifelse(babies$smoke==1 , "Yes", "No")

babies$eduCat <- factor(babies$ed, 
                        levels=0:6, 
                        labels=c("< 8th grade",
                                 "8th-12th grade",
                                 "HS graduate",
                                 "HS+trade",
                                 "HS+some college",
                                 "College graduate",
                                 "Trade school/HS unclear"))
with(babies, table(ed, eduCat, useNA='always'))

table(babies$marital)
t<-with(babies,table(marital))
prop.table(t)*100

babies$first.preg<- with(babies, ifelse(parity==0, 'first','not first'))
table(babies$first.preg)

gestAge<- babies$gestation
mean(gestAge,na.rm = T)

x<- 1121.933384
round(x, 3)

round(x, -2)


paste("Mean (days) =", mean(gestAge,na.rm = T))

paste("Mean (days) =",round(mean(gestAge,na.rm = T),2))

paste('mean day (sd) = ', 
      round(mean(gestAge,na.rm = T),0), 
      ' (', 
      round(sd(gestAge,na.rm = T), 1), 
      ')')

babies$preterm <- babies$gestation < 37*7
babies$whenBorn <- factor(babies$preterm, 
                          levels=c(TRUE,FALSE), 
                          labels=c("Preterm","Full-term"))

wt.by.smoke.term<- with(babies, 
                        tapply(wt, list(whenBorn,smokeCat), median, na.rm=T))

calculadora <- function(a,b){
  c=c(a+b,a*b)
  return(c)
}

calculadora(9,4)

custom.summary<- function(x){
  out<- paste(round(mean(x, na.rm=T), 2), 
              ' (SD=', 
              round(sd(x, na.rm=T), 2), 
              ')', 
              sep='') 
  return(out)
}
custom.summary(babies$age)

write.csv(wt.by.smoke.term, 'test.csv', quote= F)

bin.sum<- function(x){
  t<- table(x)
  pct<- round(100*prop.table(t), 1)
  paste(t[2],
              "/",
              sum(t),
              " (", pct[2], "%)", sep="")
}

bin.sum(babies$preterm)

babies$preterm
tabla <- table(babies$preterm)
porcentajes <- prop.table(tabla)
paste(tabla[2],"/",sum(tabla), " (",round(porcentajes[2]*100,2),"%)")

babies$wtKg <- babies$wt/(16*2.2)
babies$macrosomia <- babies$wtKg > 4


macro.smoke<- with(babies, tapply(macrosomia, smokeCat, bin.sum))
macro.smoke
