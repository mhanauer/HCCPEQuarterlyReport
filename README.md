---
title: "HCCPEQuarterlyReports"
output: ''
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
First set where the data is located.
```{r}
setwd("T:/prevention/STAFF REPORTS 17-18/Chat HIV    FY2/Evaluation")
HCCPEDat = read.csv("HCCPEDat.csv", header = TRUE)
head(HCCPEDat)
```
How to get matched values.  Need to seperate the data based on INTTYPE.  Then merge according to right merge where right is the 3 month reassesment.  Then only matched people will be included.
```{r}
datBASE = subset(HCCPEDat, INTTYPE ==1)
dat3month = subset(HCCPEDat, INTTYPE == 3)
HCCPEDat = merge(dat3month, datBASE, by = "PARTID", all.x = TRUE)
```
Objective A KNOW_HIV, KNOW_SA

Let us write a function that takes the percentage change and then run all variables through it.  Need to put baseline in first then 3month into the function.
```{r}
attach(HCCPEDat)
head(HCCPEDat)
pChange = function(x,y){
  x = mean(x, na.rm = TRUE)
  y = mean(y, na.rm = TRUE)
  round((y-x)/x,2)
}
HIVKnowledge = pChange(KNOW_HIV.x, KNOW_HIV.y)
HIVKnowledge

SAKnowledge = pChange(KNOW_SA.x, KNOW_SA.y)
SAKnowledge
```
Objective B SA and HIV Risk

Maybe create a function that sums the total using NA ok and creates a single measure that can then be run throught the pChange function.
```{r}
head(HCCPEDat)
BaseSARisk = data.frame(RSKALC.x, RSKCIG.x, RSKMJ.x)
compositeFun = function(x){
  apply(x, 1, sum, na.rm = TRUE)
}
BaseSARisk =  compositeFun(BaseSARisk)
BaseSARisk

Month3SARisk = data.frame(RSKALC.y, RSKCIG.y, RSKMJ.y)
Month3SARisk = compositeFun(Month3SARisk)
mean(Month3SARisk)

ObjectiveBSA = pChange(BaseSARisk, Month3SARisk)
ObjectiveBSA

BaseHIVRisk = data.frame(RSKANYSEX_UNP.x, RSKSEX_ALCDRG.x, RSKNDL_SHR.x)
BaseHIVRisk = compositeFun(BaseHIVRisk)
BaseHIVRisk

Month3HIVRisk = data.frame(RSKANYSEX_UNP.y, RSKSEX_ALCDRG.y, RSKNDL_SHR.y)
Month3HIVRisk = compositeFun(Month3HIVRisk)

ObjectiveBHIV = pChange(BaseHIVRisk, Month3HIVRisk)
ObjectiveBHIV
```
Objective F

Objective G: No alcohol.  Percentage of people who are not alcohol at pre and post is not much different or at least lower.

So everyone drinks so just look at the percentage change.
```{r}
ObjectiveG = pChange(ALC30D.x, ALC30D.y)
ObjectiveG
```
Objective H: Decrese people using unprotected sex
```{r}
LASTSEX_UNP = data.frame(LASTSEX_UNP.x, LASTSEX_UNP.y)
LASTSEX_UNP = subset(LASTSEX_UNP, LASTSEX_UNP.x == 2) 
attach(LASTSEX_UNP)
ObjectiveH = pChange(LASTSEX_UNP.x, LASTSEX_UNP.y)
ObjectiveH
```
Centerstone metrics
***Follow up rate.  First get the original number of people so redo the data read in process before the merge.
*** Need to load in the data on a R script not R markdown to get the original data back
```{r}
#setwd("T:/prevention/STAFF REPORTS 17-18/Chat HIV    FY2/Evaluation")
HCCPEDat = read.csv("HCCPEDat.csv", header = TRUE)
datBASE = subset(HCCPEDat, INTTYPE ==1)
dat3month = subset(HCCPEDat, INTTYPE == 3)

dim(dat3month)[1] / dim(datBASE)[1]
```
Gender
```{r}
describe.factor(datBASE$GENDER)
```
Race
```{r}
HCCPErace = cbind(datBASE$R_WHITE_N, datBASE$R_BLACK_N, datBASE$E_NONHISPAN, datBASE$R_ASIAIN_N)
apply(HCCPErace, 2, function(x) describe.factor(x))
```
Edu
1 = Elementary school
2 = Middle school
3 = High school
4= Community college or technical or trade school
5= Four-year college
6= Beyond four-year college
98= Left blank or multiple responses selected

```{r}
describe.factor(datBASE$EDLEVEL_N)
```
Age
```{r}
mean(2018-datBASE$YOB, na.rm = TRUE)
```
Substance use
```{r}

pDesc = function(x,y){
  x = mean(x, na.rm= TRUE)
  y = mean(y, na.rm = TRUE)
  d = cbind(x,y)
}
ObjectiveG = pDesc(ALC30D.x, ALC30D.y)
ObjectiveG

```
Condom use
```{r}
LASTSEX_UNP = data.frame(LASTSEX_UNP.x, LASTSEX_UNP.y)
LASTSEX_UNP = subset(LASTSEX_UNP, LASTSEX_UNP.x == 2) 
attach(LASTSEX_UNP)
ObjectiveH = pDesc(LASTSEX_UNP.x, LASTSEX_UNP.y)
ObjectiveH
```
HIV and SA knowledge
```{r}
BaseSARisk = data.frame(RSKALC.x, RSKCIG.x, RSKMJ.x)
compositeFun = function(x){
  apply(x, 1, sum, na.rm = TRUE)
}
BaseSARisk =  compositeFun(BaseSARisk)
BaseSARisk

Month3SARisk = data.frame(RSKALC.y, RSKCIG.y, RSKMJ.y)
Month3SARisk = compositeFun(Month3SARisk)
mean(Month3SARisk)

ObjectiveBSA = pDesc(BaseSARisk, Month3SARisk)
ObjectiveBSA

BaseHIVRisk = data.frame(RSKANYSEX_UNP.x, RSKSEX_ALCDRG.x, RSKNDL_SHR.x)
BaseHIVRisk = compositeFun(BaseHIVRisk)
BaseHIVRisk

Month3HIVRisk = data.frame(RSKANYSEX_UNP.y, RSKSEX_ALCDRG.y, RSKNDL_SHR.y)
Month3HIVRisk = compositeFun(Month3HIVRisk)

ObjectiveBHIV = pDesc(BaseHIVRisk, Month3HIVRisk)
ObjectiveBHIV
```



