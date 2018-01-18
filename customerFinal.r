setwd("C:/Users/vishanthsurresh/Desktop")
dataset <- read.csv("ActivityData.csv")
#Table for Total number of males and females
result<-data.frame(dataset$Cus_State,dataset$Cus_Sex)
table(result)
#Head dataset
head(dataset)
#Seperating male from female
MaleData <- dataset[dataset$Cus_Sex == 'M',]
print(MaleData)

#viewing for only males Customers where city is Georgia
my.data.frame <- dataset[dataset$Cus_Sex == "M" & dataset$Cus_State == "Georgia",]
View(my.data.frame)
#Finding Mean and Standard deviaton value
mean1<-mean(my.data.frame$activ_BaseAmt)
sd1<-sd(my.data.frame$activ_BaseAmt)
print(mean1)
print(sd1)
#Finding the total Treshold Value
treshold<-(3*sd1)+mean1
print(treshold)
#For a single Customer
#initializing counter
cust.data <- data.frame(custId=character(),state=character(),avg=double(),sdev=double())

for (cust in unique(my.data.frame$Cus_CustId)){
  print(cust)
  newdf<-dataset[dataset$Cus_CustId==cust,]
  mean2<-mean(newdf$activ_BaseAmt)
  sd2<-sd(newdf$activ_BaseAmt)
  state1<-newdf$Cus_State
    print(mean2)
    cust.newdata <-data.frame(
    custId = cust,state=state1,
    avg = mean2,
    sdev= sd2
  )
  cust.data <- rbind(cust.data,cust.newdata)
}
cust.data[is.na(cust.data)]<-0
View(cust.data)
cust1.data <- data.frame(custId=character(),state12=character(),avg=double(),sd=double(),tres=double(),gptres=double())


for (cust1 in unique(my.data.frame$Cus_CustId))
{
  print(cust1)
  newdf1<-cust.data[cust.data$custId==cust1,]
  sdeviation<-newdf1$sdev
  meandeviation<-newdf1$avg
  state123<-newdf1$state
  treshold1<-(3*sdeviation)+meandeviation
  cust1.newdata<-data.frame(custId=cust1,state12=state123,avg=meandeviation,sd=sdeviation,tres=treshold1,gptres=treshold)
  cust1.data<-rbind(cust1.data,cust1.newdata)
}
View(cust1.data)
abc<-cbind(cust1.data,ouput=ifelse(cust1.data$tres>=cust1.data$gptres, 2, 1))
View(abc)
#Georgia Male Ends..................................................................
result<-data.frame(dataset$Cus_State,dataset$Cus_Sex)
table(result)

FeMaleData <- dataset[dataset$Cus_Sex == 'F',]
print(FeMaleData)
my.data.frameFemaleGeorgia <- dataset[dataset$Cus_Sex == "F" & dataset$Cus_State == "Georgia",]
View(my.data.frameFemaleGeorgia)
meanFemaleGeorgia<-mean(my.data.frameFemaleGeorgia$activ_BaseAmt)
sdFemaleGeoriga<-sd(my.data.frameFemaleGeorgia$activ_BaseAmt)
print(meanFemaleGeorgia)
print(sdFemaleGeoriga)
tresholdFemaleGeorgia<-(3*sdFemaleGeoriga)+meanFemaleGeorgia
print(tresholdFemaleGeorgia)
cust.dataFemaleGeorgia <- data.frame(custIdFemaleGeorgia=character(),state12345=character(),avgFemaleGeorgia=double(),sdevFemaleGeoriga=double())

for (custFemaleGeorgia in unique(my.data.frameFemaleGeorgia$Cus_CustId)){
  print(custFemaleGeorgia)
  newdfFemaleGeorgia<-dataset[dataset$Cus_CustId==custFemaleGeorgia,]
  mean2FemaleGeorgia<-mean(newdfFemaleGeorgia$activ_BaseAmt)
  sd2FemaleGeorgia<-sd(newdfFemaleGeorgia$activ_BaseAmt)
  state12345<-newdfFemaleGeorgia$Cus_State
  print(mean2FemaleGeorgia)
  cust.newdataFemaleGeorgia <-data.frame(
    custIdFemaleGeorgia = custFemaleGeorgia,
    state1234=state12345,
    avgFemaleGeorgia = mean2FemaleGeorgia,
    sdevFemaleGeorgia= sd2FemaleGeorgia
  )
  cust.dataFemaleGeorgia <- rbind(cust.dataFemaleGeorgia,cust.newdataFemaleGeorgia)
}
cust.dataFemaleGeorgia[is.na(cust.dataFemaleGeorgia)]<-0
View(cust.dataFemaleGeorgia)
cust12.data <- data.frame(custId=character(),state123456=character(),avgFemalegeorgia=double(),sdevFemaleGeorgia=double(),tres=double(),gpres=double())


for (cust12 in unique(my.data.frameFemaleGeorgia$Cus_CustId))
{
  print(cust12)
  newdf12<-cust.dataFemaleGeorgia[cust.dataFemaleGeorgia$custId==cust12,]
  sdeviation<-newdf12$sdevFemaleGeorgia
  meandeviation<-newdf12$avgFemaleGeorgia
  state1234567<-newdf12$state1234
  treshold12<-(3*sdeviation)+meandeviation
  cust12.newdata<-data.frame(custId=cust12,state123456=state1234567,avgFemaleGeorgia=meandeviation,sdevFemaleGeorgia=sdeviation,tres=treshold12,gptres=tresholdFemaleGeorgia)
  cust12.data<-rbind(cust12.data,cust12.newdata)
}
View(cust12.data)

def<-cbind(cust12.data,ouput=ifelse(cust12.data$tres>=cust12.data$gptres, 2, 1))
View(def)
#Georgia Female ends..............................................................
result<-data.frame(dataset$Cus_State,dataset$Cus_Sex)
table(result)

FeMaleDatacalifornia <- dataset[dataset$Cus_Sex == 'F',]
print(FeMaleDatacalifornia)
my.data.frameFemalecalifornia <- dataset[dataset$Cus_Sex == "F" & dataset$Cus_State == "California",]
View(my.data.frameFemalecalifornia)
meanFemalecalifornia<-mean(my.data.frameFemalecalifornia$activ_BaseAmt)
sdFemalecalifornia<-sd(my.data.frameFemalecalifornia$activ_BaseAmt)
print(meanFemalecalifornia)
print(sdFemalecalifornia)
tresholdFemaleCalifornia<-(3*sdFemalecalifornia)+meanFemalecalifornia
print(tresholdFemaleCalifornia)
cust.dataFemalecalifornia<- data.frame(custIdFemalecalifornia=character(),statecalifornia=character(),avgFemalecalifornia=double(),sdevFemalecalifornia=double())

for (custFemalecalifornia in unique(my.data.frameFemalecalifornia$Cus_CustId)){
  print(custFemalecalifornia)
  newdfFemalecalifornia<-dataset[dataset$Cus_CustId==custFemalecalifornia,]
  mean2Femalecalifornia<-mean(newdfFemalecalifornia$activ_BaseAmt)
  sd2Femalecalifornia<-sd(newdfFemalecalifornia$activ_BaseAmt)
  statecalifornia12<-newdfFemalecalifornia$Cus_State
  print(mean2Femalecalifornia)
  cust.newdataFemalecalifornia <-data.frame(
    custIdFemalecalifornia = custFemalecalifornia,
    avgFemalecalifornia = mean2Femalecalifornia,
    statecalifornia=statecalifornia12,
    sdevFemalecalifornia= sd2Femalecalifornia
  )
  cust.dataFemalecalifornia <- rbind(cust.dataFemalecalifornia,cust.newdataFemalecalifornia)
}
cust.dataFemalecalifornia[is.na(cust.dataFemalecalifornia)]<-0
View(cust.dataFemalecalifornia)
cust123.data <- data.frame(custId=character(),statecalifornia123=character(),avgFemalecalifornia=double(),sdevFemalecalifornia=double(),tres=double(),gptres=double())


for (cust123 in unique(my.data.frameFemalecalifornia$Cus_CustId))
{
  print(cust123)
  newdf123<-cust.dataFemalecalifornia[cust.dataFemalecalifornia$custIdFemalecalifornia==cust123,]
  sdeviation123<-newdf123$sdevFemalecalifornia
  meandeviation123<-newdf123$avgFemalecalifornia
  statecalifornia1234<-newdf123$statecalifornia
  treshold123<-(3*sdeviation123)+meandeviation123
  cust123.newdata<-data.frame(custId=cust123,statecalifornia123=statecalifornia1234,avgFemalecalifornia=meandeviation123,sdevFemalecalifornia=sdeviation123,tres=treshold123,gptres=tresholdFemaleCalifornia)
  cust123.data<-rbind(cust123.data,cust123.newdata)
}
View(cust123.data)

ghi<-cbind(cust123.data,ouput=ifelse(cust123.data$tres>=cust123.data$gptres, 2, 1))
View(ghi)
#California Female ends............................................................
result<-data.frame(dataset$Cus_State,dataset$Cus_Sex)
table(result)

MaleDatacalifornia <- dataset[dataset$Cus_Sex == 'M',]
print(MaleDatacalifornia)
my.data.framemalecalifornia <- dataset[dataset$Cus_Sex == "M" & dataset$Cus_State == "California",]
View(my.data.framemalecalifornia)
meanmalecalifornia<-mean(my.data.framemalecalifornia$activ_BaseAmt)
sdmalecalifornia<-sd(my.data.framemalecalifornia$activ_BaseAmt)
print(meanmalecalifornia)
print(sdmalecalifornia)
tresholdmaleCalifornia<-(3*sdmalecalifornia)+meanmalecalifornia
print(tresholdmaleCalifornia)
cust.datamalecalifornia<- data.frame(custIdmalecalifornia=character(),statecaliforniamale=character(),avgmalecalifornia=double(),sdevmalecalifornia=double())

for (custmalecalifornia in unique(my.data.framemalecalifornia$Cus_CustId)){
  print(custmalecalifornia)
  newdfmalecalifornia<-dataset[dataset$Cus_CustId==custmalecalifornia,]
  mean2malecalifornia<-mean(newdfmalecalifornia$activ_BaseAmt)
  sd2malecalifornia<-sd(newdfmalecalifornia$activ_BaseAmt)
  statecaliforniamale12<-newdfmalecalifornia$Cus_State
  print(mean2malecalifornia)
  cust.newdatamalecalifornia <-data.frame(
    custIdmalecalifornia = custmalecalifornia,
    statecaliforniamale=statecaliforniamale12,
    avgmalecalifornia = mean2malecalifornia,
    sdevmalecalifornia= sd2malecalifornia
  )
  cust.datamalecalifornia <- rbind(cust.datamalecalifornia,cust.newdatamalecalifornia)
}
cust.datamalecalifornia[is.na(cust.datamalecalifornia)]<-0
View(cust.datamalecalifornia)
cust1234.data <- data.frame(custId=character(),statecaliforniamale123=character(),avgmalecalifornia=double(),sdevmalecalifornia=double(),tres=double(),gptres=double())


for (cust1234 in unique(my.data.framemalecalifornia$Cus_CustId))
{
  print(cust1234)
  newdf1234<-cust.datamalecalifornia[cust.datamalecalifornia$custIdmalecalifornia==cust1234,]
  sdeviation1234<-newdf1234$sdevmalecalifornia
  meandeviation1234<-newdf1234$avgmalecalifornia
  statecalifornia12345<-newdf1234$statecaliforniamale
  treshold1234<-(3*sdeviation1234)+meandeviation1234
  cust1234.newdata<-data.frame(custId=cust1234,statecaliforniamale123=statecalifornia12345,avgmalecalifornia=meandeviation1234,sdevmalecalifornia=sdeviation1234,tres=treshold1234,gptres=tresholdmaleCalifornia)
  cust1234.data<-rbind(cust1234.data,cust1234.newdata)
}
View(cust1234.data)

jkl<-cbind(cust1234.data,ouput=ifelse(cust1234.data$tres>=cust1234.data$gptres, 2, 1))
View(jkl)
#California male ends...............................................................
result<-data.frame(dataset$Cus_State,dataset$Cus_Sex)
table(result)

FeMaleDatahouston<- dataset[dataset$Cus_Sex == 'F',]
print(FeMaleDatahouston)
my.data.frameFemalehouston <- dataset[dataset$Cus_Sex == "F" & dataset$Cus_State == "Houston",]
View(my.data.frameFemalehouston)
meanFemalehouston<-mean(my.data.frameFemalehouston$activ_BaseAmt)
sdFemalehouston<-sd(my.data.frameFemalehouston$activ_BaseAmt)
print(meanFemalehouston)
print(sdFemalehouston)
tresholdFemalehouston<-(3*sdFemalehouston)+meanFemalehouston
print(tresholdFemalehouston)
cust.dataFemalehouston<- data.frame(custIdFemalehouston=character(),statehoustonamale=character(),avgFemalehouston=double(),sdevFemalehouston=double())

for (custFemalehouston in unique(my.data.frameFemalehouston$Cus_CustId)){
  print(custFemalehouston)
  newdfFemalehouston<-dataset[dataset$Cus_CustId==custFemalehouston,]
  mean2Femalehouston<-mean(newdfFemalehouston$activ_BaseAmt)
  sd2Femalehouston<-sd(newdfFemalehouston$activ_BaseAmt)
  statehoustonmale12<-newdfFemalehouston$Cus_State
  print(mean2Femalehouston)
  cust.newdataFemalehouston <-data.frame(
    custIdFemalehouston = custFemalehouston,
    statehoustonFemale=statehoustonmale12,
        avgFemalehouston = mean2Femalehouston,
    sdevFemalehouston= sd2Femalehouston
  )
  cust.dataFemalehouston <- rbind(cust.dataFemalehouston,cust.newdataFemalehouston)
}
cust.dataFemalehouston[is.na(cust.dataFemalehouston)]<-0
View(cust.dataFemalehouston)

cust12345.data <- data.frame(custId=character(),statehouston123=character(),avgFemalehouston=double(),sdevFemalehouston=double(),tres=double(),gptres=double())


for (cust12345 in unique(my.data.frameFemalehouston$Cus_CustId))
{
  print(cust12345)
  newdf12345<-cust.dataFemalehouston[cust.dataFemalehouston$custIdFemalehouston==cust12345,]
  sdeviation12345<-newdf12345$sdevFemalehouston
  meandeviation12345<-newdf12345$avgFemalehouston
  statehoustonFemale1234<-newdf12345$statehoustonFemale
  treshold12345<-(3*sdeviation12345)+meandeviation12345
  cust12345.newdata<-data.frame(custId=cust12345,statehouston123=statehoustonFemale1234,avgFemalehouston=meandeviation12345,sdevFemalehouston=sdeviation12345,tres=treshold12345,gptres=tresholdFemalehouston)
  cust12345.data<-rbind(cust12345.data,cust12345.newdata)
}
View(cust12345.data)

mno<-cbind(cust12345.data,ouput=ifelse(cust12345.data$tres>=cust12345.data$gptres, 2, 1))
View(mno)
#houston Female ends................................................................
result<-data.frame(dataset$Cus_State,dataset$Cus_Sex)
table(result)

MaleDatahouston<- dataset[dataset$Cus_Sex == 'M',]
print(MaleDatahouston)
my.data.framemalehouston <- dataset[dataset$Cus_Sex == "M" & dataset$Cus_State == "Houston",]
View(my.data.framemalehouston)
meanmalehouston<-mean(my.data.framemalehouston$activ_BaseAmt)
sdmalehouston<-sd(my.data.framemalehouston$activ_BaseAmt)
print(meanmalehouston)
print(sdmalehouston)
tresholdmalehouston<-(3*sdmalehouston)+meanmalehouston
print(tresholdmalehouston)
cust.datamalehouston<- data.frame(custIdmalehouston=character(),statehoustonmale=character(),avgmalehouston=double(),sdevmalehouston=double())

for (custmalehouston in unique(my.data.framemalehouston$Cus_CustId)){
  print(custmalehouston)
  newdfmalehouston<-dataset[dataset$Cus_CustId==custmalehouston,]
  mean2malehouston<-mean(newdfmalehouston$activ_BaseAmt)
  sd2malehouston<-sd(newdfmalehouston$activ_BaseAmt)
  statehoustonmale1234<-newdfmalehouston$Cus_State
  print(mean2malehouston)
  cust.newdatamalehouston <-data.frame(
    custIdmalehouston = custmalehouston,
    statehoustonmale=statehoustonmale1234,
    avgmalehouston = mean2malehouston,
    sdevmalehouston= sd2malehouston
  )
  cust.datamalehouston <- rbind(cust.datamalehouston,cust.newdatamalehouston)
}
cust.datamalehouston[is.na(cust.datamalehouston)]<-0
View(cust.datamalehouston)

cust123456.data <- data.frame(custId=character(),statehoustonmaleends=character(),avgmalehouston=double(),sdevmalehouston=double(),tres=double(),gptres=double())


for (cust123456 in unique(my.data.framemalehouston$Cus_CustId))
{
  print(cust123456)
  newdf123456<-cust.datamalehouston[cust.datamalehouston$custIdmalehouston==cust123456,]
  sdeviation123456<-newdf123456$sdevmalehouston
  meandeviation123456<-newdf123456$avgmalehouston
  statehouston12345678<-newdf123456$statehoustonmale
  treshold123456<-(3*sdeviation123456)+meandeviation123456
  cust123456.newdata<-data.frame(custId=cust123456,statehoustonmaleends=statehouston12345678,avgmalehouston=meandeviation123456,sdevmalehouston=sdeviation123456,tres=treshold123456,gptres=tresholdmalehouston)
  cust123456.data<-rbind(cust123456.data,cust123456.newdata)
}
View(cust123456.data)

pqr<-cbind(cust123456.data,ouput=ifelse(cust123456.data$tres>=cust123456.data$gptres, 2, 1))
View(pqr)

#Houston male ends..................................................................
result<-data.frame(dataset$Cus_State,dataset$Cus_Sex)
table(result)

FeMaleDatanewyork<- dataset[dataset$Cus_Sex == 'F',]
print(FeMaleDatanewyork)
my.data.frameFemalenewyork <- dataset[dataset$Cus_Sex == "F" & dataset$Cus_State == "New York",]
View(my.data.frameFemalenewyork)
meanFemalenewyork<-mean(my.data.frameFemalenewyork$activ_BaseAmt)
sdFemalenewyork<-sd(my.data.frameFemalenewyork$activ_BaseAmt)
print(meanFemalenewyork)
print(sdFemalenewyork)
tresholdFemalenewyork<-(3*sdFemalenewyork)+meanFemalenewyork
print(tresholdFemalenewyork)
cust.dataFemalenewyork<- data.frame(custIdFemalenewyork=character(),statenewyork=character(),avgFemalenewyork=double(),sdevFemalenewyork=double())

for (custFemalenewyork in unique(my.data.frameFemalenewyork$Cus_CustId)){
  print(custFemalenewyork)
  newdfFemalenewyork<-dataset[dataset$Cus_CustId==custFemalenewyork,]
  mean2Femalenewyork<-mean(newdfFemalenewyork$activ_BaseAmt)
  sd2Femalenewyork<-sd(newdfFemalenewyork$activ_BaseAmt)
  statenewyork12<-newdfFemalenewyork$Cus_State
  print(mean2Femalenewyork)
  cust.newdataFemalenewyork <-data.frame(
    custIdFemalenewyork = custFemalenewyork,
    statenewyork=statenewyork12,
    avgFemalenewyork = mean2Femalenewyork,
    sdevFemalenewyork= sd2Femalenewyork
  )
  cust.dataFemalenewyork <- rbind(cust.dataFemalenewyork,cust.newdataFemalenewyork)
}
cust.dataFemalenewyork[is.na(cust.dataFemalenewyork)]<-0
View(cust.dataFemalenewyork)

cust1234567.data <- data.frame(custId=character(),statenewyorkFemale=character(),avgFemalenewyork=double(),sdevFemalenewyork=double(),tres=double(),gptres=double())


for (cust1234567 in unique(my.data.frameFemalenewyork$Cus_CustId))
{
  print(cust1234567)
  newdf1234567<-cust.dataFemalenewyork[cust.dataFemalenewyork$custIdFemalenewyork==cust1234567,]
  sdeviation1234567<-newdf1234567$sdevFemalenewyork
  meandeviation1234567<-newdf1234567$avgFemalenewyork
  statenewyork123<-newdf1234567$statenewyork
  treshold1234567<-(3*sdeviation1234567)+meandeviation1234567
  cust1234567.newdata<-data.frame(custId=cust1234567,statenewyorkFemale=statenewyork123,avgFemalenewyork=meandeviation1234567,sdevFemalenewyork=sdeviation1234567,tres=treshold1234567,gptres=tresholdFemalenewyork)
  cust1234567.data<-rbind(cust1234567.data,cust1234567.newdata)
}
View(cust1234567.data)

stu<-cbind(cust1234567.data,ouput=ifelse(cust1234567.data$tres>=cust1234567.data$gptres, 2, 1))
View(stu)
#Female newyork ends...............................................................
result<-data.frame(dataset$Cus_State,dataset$Cus_Sex)
table(result)

MaleDatanewyork<- dataset[dataset$Cus_Sex == 'M',]
print(MaleDatanewyork)
my.data.framemalenewyork <- dataset[dataset$Cus_Sex == "M" & dataset$Cus_State == "New York",]
View(my.data.framemalenewyork)
meanmalenewyork<-mean(my.data.framemalenewyork$activ_BaseAmt)
sdmalenewyork<-sd(my.data.framemalenewyork$activ_BaseAmt)
print(meanmalenewyork)
print(sdmalenewyork)
tresholdmalenewyork<-(3*sdmalenewyork)+meanmalenewyork
print(tresholdmalenewyork)
cust.datamalenewyork<- data.frame(custIdmalenewyork=character(),statenewyorkmale=character(),avgmalenewyork=double(),sdevmalenewyork=double())

for (custmalenewyork in unique(my.data.framemalenewyork$Cus_CustId)){
  print(custmalenewyork)
  newdfmalenewyork<-dataset[dataset$Cus_CustId==custmalenewyork,]
  mean2malenewyork<-mean(newdfmalenewyork$activ_BaseAmt)
  sd2malenewyork<-sd(newdfmalenewyork$activ_BaseAmt)
  statenewyorkmale12<-newdfmalenewyork$Cus_State
  print(mean2malenewyork)
  cust.newdatamalenewyork <-data.frame(
    custIdmalenewyork = custmalenewyork,
    statenewyorkmale=statenewyorkmale12,
    avgmalenewyork = mean2malenewyork,
    sdevmalenewyork= sd2malenewyork
  )
  cust.datamalenewyork <- rbind(cust.datamalenewyork,cust.newdatamalenewyork)
}
cust.datamalenewyork[is.na(cust.datamalenewyork)]<-0
View(cust.datamalenewyork)

cust12345678.data <- data.frame(custId=character(),statenewyorkmale123=character(),avgmalenewyork=double(),sdevmalenewyork=double(),tres=double(),gptres=double())


for (cust12345678 in unique(my.data.framemalenewyork$Cus_CustId))
{
  print(cust12345678)
  newdf12345678<-cust.datamalenewyork[cust.datamalenewyork$custIdmalenewyork==cust12345678,]
  sdeviation12345678<-newdf12345678$sdevmalenewyork
  meandeviation12345678<-newdf12345678$avgmalenewyork
  statenewyorkmale12345<-newdf12345678$statenewyorkmale
  treshold12345678<-(3*sdeviation12345678)+meandeviation12345678
  cust12345678.newdata<-data.frame(custId=cust12345678,statenewyorkmale123=statenewyorkmale12345,avgmalenewyork=meandeviation12345678,sdevmalenewyork=sdeviation12345678,tres=treshold12345678,gptres=tresholdmalenewyork)
  cust12345678.data<-rbind(cust12345678.data,cust12345678.newdata)
}
View(cust12345678.data)

vwx<-cbind(cust123456078.data,ouput=ifelse(cust12345678.data$tres>=cust12345678.data$gptres, 2, 1))
View(vwx)
#newyork male ends..............................................................
names(abc)<-c("custID","state","mean","sdeviaiton","individualtreshold","grouptreshold","output")
names(def)<-c("custID","state","mean","sdeviaiton","individualtreshold","grouptreshold","output")
names(ghi)<-c("custID","state","mean","sdeviaiton","individualtreshold","grouptreshold","output")
names(jkl)<-c("custID","state","mean","sdeviaiton","individualtreshold","grouptreshold","output")
names(mno)<-c("custID","state","mean","sdeviaiton","individualtreshold","grouptreshold","output")
names(pqr)<-c("custID","state","mean","sdeviaiton","individualtreshold","grouptreshold","output")
names(stu)<-c("custID","state","mean","sdeviaiton","individualtreshold","grouptreshold","output")
names(vwx)<-c("custID","state","mean","sdeviaiton","individualtreshold","grouptreshold","output")

test<-rbind(abc,def)
test<-rbind(test,ghi)
test<-rbind(test,jkl)
test<-rbind(test,mno)
test<-rbind(test,pqr)
test<-rbind(test,stu)
test<-rbind(test,vwx)


View(test)
install.packages("randomForest")
library(randomForest)

#Dividing the data into training and testing data's
set.seed(2)
id<-sample(2,nrow(test),prob<-c(0.7,0.3),replace=TRUE)
test_train<-test[id==1,]
test_test<-test[id==2,]

test_forest<-randomForest(output~state,data=test_train)
test_forest

importance(test_forest)
prd1_test<-predict(test_forest,newdata = test_test,type = "class")
prd1_test

library(caret)
confusionMatrix(table(prd1_test,test_test$state))
varImpPlot(test_forest)
plot(test$state,col="orange",xlab="state",ylab="output",main="mainchart")
plot(test$output,type="o",col="blue")
