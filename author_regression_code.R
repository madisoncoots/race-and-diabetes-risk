#Rahul Aggarwal
#Asian Diabetes by BMI

###Load Data
library(Hmisc)
library(readr)
library(jtools)


#2017-2018
setwd("/home/mcoots/harvard/research/race-in-healthcare/data/raw/2017-2018")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/BPX_J.XPT", bpx <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/BPQ_J.XPT", bpq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/SMQ_J.XPT", smq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/HIQ_J.XPT", hiq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/TCHOL_J.XPT", tchol <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/GLU_J.XPT", glu <- tempfile(), mode="wb")
bp1 <- foreign::read.xport(bpx) %>% clean_names()
bpq1 <- foreign::read.xport(bpq) %>% clean_names()
smoke1 <- foreign::read.xport(smq) %>% clean_names()
insurance1 <- foreign::read.xport(hiq) %>% clean_names()
cholesterol1 <- foreign::read.xport(tchol) %>% clean_names()
fasting1 <- foreign::read.xport(glu) %>% clean_names()
demo1 = sasxport.get("DEMO_J.XPT")
# bp1 = sasxport.get("BPX_J.XPT")
# bpq1 = sasxport.get("BPQ_J.XPT")
# insurance1 = sasxport.get("HIQ_J.XPT")
bmx1=sasxport.get("BMX_J.XPT")
diabetes1 = sasxport.get("DIQ_J.XPT")
a1c1=sasxport.get("GHB_J.XPT")
# smoke1=sasxport.get("SMQ_J.XPT")
# cholesterol1=sasxport.get("TCHOL_J.XPT")
# fasting1=sasxport.get("GLU_J.XPT")

###2015-2016
setwd("/home/mcoots/harvard/research/race-in-healthcare/data/raw/2015-2016")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/BPX_I.XPT", bpx <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/BPQ_I.XPT", bpq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/SMQ_I.XPT", smq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/HIQ_I.XPT", hiq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/TCHOL_I.XPT", tchol <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/GLU_I.XPT", glu <- tempfile(), mode="wb")
bp2 <- foreign::read.xport(bpx) %>% clean_names()
bpq2 <- foreign::read.xport(bpq) %>% clean_names()
smoke2 <- foreign::read.xport(smq) %>% clean_names()
insurance2 <- foreign::read.xport(hiq) %>% clean_names()
cholesterol2 <- foreign::read.xport(tchol) %>% clean_names()
fasting2 <- foreign::read.xport(glu) %>% clean_names()
demo2 = sasxport.get("DEMO_I.XPT")
# bp2 = sasxport.get("BPX_I.XPT")
# bpq2 = sasxport.get("BPQ_I.XPT")
bmx2=sasxport.get("BMX_I.XPT")
# smoke2=sasxport.get("SMQ_I.XPT")
diabetes2 = sasxport.get("DIQ_I.XPT")
# insurance2 = sasxport.get("HIQ_I.XPT")
a1c2=sasxport.get("GHB_I.XPT")
# cholesterol2=sasxport.get("TCHOL_I.XPT")
# fasting2=sasxport.get("GLU_I.XPT")

###2013-2014
setwd("/home/mcoots/harvard/research/race-in-healthcare/data/raw/2013-2014")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/BPX_H.XPT", bpx <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/BPQ_H.XPT", bpq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/SMQ_H.XPT", smq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/HIQ_H.XPT", hiq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/TCHOL_H.XPT", tchol <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/GLU_H.XPT", glu <- tempfile(), mode="wb")
bp3 <- foreign::read.xport(bpx) %>% clean_names()
bpq3 <- foreign::read.xport(bpq) %>% clean_names()
smoke3 <- foreign::read.xport(smq) %>% clean_names()
insurance3 <- foreign::read.xport(hiq) %>% clean_names()
cholesterol3 <- foreign::read.xport(tchol) %>% clean_names()
fasting3 <- foreign::read.xport(glu) %>% clean_names()


demo3 = sasxport.get("DEMO_H.XPT")
# bp3 = sasxport.get("BPX_H.XPT")
# bpq3 = sasxport.get("BPQ_H.XPT")
bmx3=sasxport.get("BMX_H.XPT")
# smoke3=sasxport.get("SMQ_H.XPT")
diabetes3 = sasxport.get("DIQ_H.XPT")
# insurance3 = sasxport.get("HIQ_H.XPT")
a1c3=sasxport.get("GHB_H.XPT")
# cholesterol3=sasxport.get("TCHOL_H.XPT")
# fasting3=sasxport.get("GLU_H.XPT")

###2011-2012
setwd("/home/mcoots/harvard/research/race-in-healthcare/data/raw/2011-2012")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/BPX_G.XPT", bpx <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/BPQ_G.XPT", bpq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/SMQ_G.XPT", smq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/HIQ_G.XPT", hiq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/TCHOL_G.XPT", tchol <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/GLU_G.XPT", glu <- tempfile(), mode="wb")
bp4 <- foreign::read.xport(bpx) %>% clean_names()
bpq4 <- foreign::read.xport(bpq) %>% clean_names()
smoke4 <- foreign::read.xport(smq) %>% clean_names()
insurance4 <- foreign::read.xport(hiq) %>% clean_names()
cholesterol4 <- foreign::read.xport(tchol) %>% clean_names()
fasting4 <- foreign::read.xport(glu) %>% clean_names()

demo4 = sasxport.get("DEMO_G.XPT")
# bp4 = sasxport.get("BPX_G.XPT")
# bpq4 = sasxport.get("BPQ_G.XPT")
bmx4=sasxport.get("BMX_G.XPT")
# smoke4=sasxport.get("SMQ_G.XPT")
diabetes4 = sasxport.get("DIQ_G.XPT")
# insurance4 = sasxport.get("HIQ_G.XPT")
a1c4=sasxport.get("GHB_G.XPT")
# cholesterol4=sasxport.get("TCHOL_G.XPT")
# fasting4=sasxport.get("GLU_G.XPT")

###Combine dataframes
demo_columns=Reduce(intersect,list(colnames(demo1),colnames(demo2),colnames(demo3),colnames(demo4)))
bp_columns=Reduce(intersect,list(colnames(bp1),colnames(bp2),colnames(bp3),colnames(bp4)))
bpq_columns=Reduce(intersect,list(colnames(bpq1),colnames(bpq2),colnames(bpq3),colnames(bpq4)))
insurance_columns=Reduce(intersect,list(colnames(insurance1),colnames(insurance2),colnames(insurance3),colnames(insurance4)))
bmx_columns=Reduce(intersect,list(colnames(bmx1),colnames(bmx2),colnames(bmx3),colnames(bmx4)))
smoke_columns=Reduce(intersect,list(colnames(smoke1),colnames(smoke2),colnames(smoke3),colnames(smoke4)))
diabetes_columns=Reduce(intersect,list(colnames(diabetes1),colnames(diabetes2),colnames(diabetes3),colnames(diabetes4)))
a1c_columns=Reduce(intersect,list(colnames(a1c1),colnames(a1c2),colnames(a1c3),colnames(a1c4)))
cholesterol_columns=Reduce(intersect,list(colnames(cholesterol1),colnames(cholesterol2),colnames(cholesterol3),colnames(cholesterol4)))
fasting_columns=Reduce(intersect,list(colnames(fasting1),colnames(fasting2),colnames(fasting3),colnames(fasting4)))

demo=rbind(demo1[,demo_columns],demo2[,demo_columns],demo3[,demo_columns],demo4[,demo_columns])
bp=rbind(bp1[,bp_columns],bp2[,bp_columns],bp3[,bp_columns],bp4[,bp_columns])
bpq=rbind(bpq1[,bpq_columns],bpq2[,bpq_columns],bpq3[,bpq_columns],bpq4[,bpq_columns])
insurance=rbind(insurance1[,insurance_columns],insurance2[,insurance_columns],insurance3[,insurance_columns],insurance4[,insurance_columns])
bmx=rbind(bmx1[,bmx_columns],bmx2[,bmx_columns],bmx3[,bmx_columns],bmx4[,bmx_columns])
diabetes=rbind(diabetes1[,diabetes_columns],diabetes2[,diabetes_columns],diabetes3[,diabetes_columns],diabetes4[,diabetes_columns])
smoke=rbind(smoke1[,smoke_columns],smoke2[,smoke_columns],smoke3[,smoke_columns],smoke4[,smoke_columns])
a1c=rbind(a1c1[,a1c_columns],a1c2[,a1c_columns],a1c3[,a1c_columns],a1c4[,a1c_columns])
cholesterol=rbind(cholesterol1[,cholesterol_columns],cholesterol2[,cholesterol_columns],cholesterol3[,cholesterol_columns],cholesterol4[,cholesterol_columns])
fasting=rbind(fasting1[,fasting_columns],fasting2[,fasting_columns],fasting3[,fasting_columns],fasting4[,fasting_columns])


rm(demo1,demo2,demo3,demo4,bp4,bpq4,insurance4,bmx4,diabetes4,smoke4,a1c4,cholesterol4,fasting4)
rm(bp1,bp2,bp3)
rm(bpq1,bpq2,bpq3)
rm(insurance1,insurance2,insurance3)
rm(bp_columns,bpq_columns,demo_columns,insurance_columns,bmx_columns,
   diabetes_columns,smoke_columns,a1c_columns,cholesterol_columns,fasting_columns)
rm(bmx1,bmx2,bmx3)
rm(diabetes1,diabetes2,diabetes3)
rm(smoke1,smoke2,smoke3)
rm(a1c1,a1c2,a1c3)
rm(cholesterol1,cholesterol2,cholesterol3)
rm(fasting1,fasting2,fasting3)

#Create Four Year dataframes
demo$wtint8yr=demo$wtint2yr*1/4
demo$wtmec8yr=demo$wtmec2yr*1/4
fasting$wtsaf8yr=fasting$wtsaf2yr*1/4

#Merge dataframes
df=merge(demo,bp,all.x=TRUE,by.x="seqn",by.y="seqn")
df=merge(df,bpq,all.x=TRUE,by.x="seqn",by.y="seqn")
df=merge(df,a1c,all.x=TRUE,by.x="seqn",by.y="seqn")
df=merge(df,diabetes,all.x=TRUE,by.x="seqn",by.y="seqn")
df=merge(df,smoke,all.x=TRUE,by.x="seqn",by.y="seqn")
df=merge(df,insurance,all.x=TRUE,by.x="seqn",by.y="seqn")
df=merge(df,bmx,all.x=TRUE,by.x="seqn",by.y="seqn")
df=merge(df,cholesterol,all.x=TRUE,by.x="seqn",by.y="seqn")
df=merge(df,fasting,all.x=TRUE,by.x="seqn",by.y="seqn")

#Create obesity measures
summary(df$bmxbmi)
df$bmi=cut(df$bmxbmi,breaks=c(0,18.5,25,30,1000),right=FALSE)
df$bmi=relevel(df$bmi,ref="[0,18.5)")

df$bmi_granular=cut(df$bmxbmi,breaks=c(0,18.5,25,30,35,40,1000),right=FALSE)

#Obesity with divided Ranges
df$bmi_granular2=cut(df$bmxbmi,breaks=c(0,18.5,21.75,25,30,35,40,1000),right=FALSE)
df$bmi_granular3=cut(df$bmxbmi,breaks=c(0,18.5,22,27,32,37,1000),right=FALSE)
df$bmi_granular4=cut(df$bmxbmi,breaks=c(0,18.5,22,27,1000),right=FALSE)

#BMI integer

df$bmi_integer=round(df$bmxbmi,digits=0)
head(df$bmi_integer)

#Total Cholesterol with
df$cholesterol=cut(df$lbxtc,breaks=c(0,200,240,1000),right=FALSE)

#Reclassifying Obesity by Race
df$new_bmi_classifier=NA
df[df$bmi=="[0,18.5)" &!is.na(df$bmi),]$new_bmi_classifier="Underweight"
df[df$ridreth3==3 & df$bmi=="[18.5,25)" &!is.na(df$bmi),]$new_bmi_classifier="Normal"
df[df$ridreth3==3 & df$bmi=="[25,30)" &!is.na(df$bmi),]$new_bmi_classifier="Overweight"
df[df$ridreth3==3 & df$bmi=="[30,1e+03)" &!is.na(df$bmi),]$new_bmi_classifier="Obese"

df[df$ridreth3==6 & df$bmi_granular4=="[18.5,22)" &!is.na(df$bmi),]$new_bmi_classifier="Normal"
df[df$ridreth3==6 & df$bmi_granular4=="[22,27)" &!is.na(df$bmi),]$new_bmi_classifier="Overweight"
df[df$ridreth3==6 & df$bmi_granular4=="[27,1e+03)" &!is.na(df$bmi),]$new_bmi_classifier="Obese"




#A1c Measures
summary(df$lbxgh)
df$a1c=cut(df$lbxgh,breaks=c(0,5.7,6.5,1000),right=FALSE)
df$a1c_granular=cut(df$lbxgh,breaks=c(0,5.7,6.5,8,10,12,1000),right=FALSE)

#Asian Variable with White 
df$asian_white=as.character(df$ridreth3)
table(df$asian_white)
df[df$asian_white=="3",]$asian_white="0"
df[df$asian_white %in% c(1,2,4,7),]$asian_white=NA
df[df$asian_white %in% c(6),]$asian_white="1"
df$asian_white=as.numeric(df$asian_white)

#Asian vs all others
df$asian_other=df$ridreth3
df[!df$asian_other==6,]$asian_other=0
df[df$asian_other==6,]$asian_other=1
table(df$asian_other)


#Diagnosed / Undiagnosed Variables
table(df$diq010)

df$diagnosed_diabetes=df$diq010
df[df$diagnosed_diabetes %in% c(2,3,9),]$diagnosed_diabetes=0
df[df$diagnosed_diabetes %in% 7,]$diagnosed_diabetes=NA

df$diabetes=df$diagnosed_diabetes
df[df$a1c=="[6.5,1e+03)" &!is.na(df$a1c),]$diabetes=1

df$prediabetes=df$diabetes
df[df$prediabetes==1 &!is.na(df$prediabetes),]$prediabetes=0 #This retains the NA people
df[df$a1c=="[5.7,6.5)" & !df$diabetes==1 &!is.na(df$diabetes) &!is.na(df$a1c),]$prediabetes=1

df$normal_a1c=df$diabetes+df$prediabetes
table(df$normal_a1c)
df[df$normal_a1c==0 &!is.na(df$normal_a1c),]$normal_a1c=2
df[df$normal_a1c==1 &!is.na(df$normal_a1c),]$normal_a1c=0
df[df$normal_a1c==2 &!is.na(df$normal_a1c),]$normal_a1c=1
summary(df[df$normal_a1c==1,]$lbxgh) #all have a1c less than 5.7

df$undiagnosed_diabetes=df$diabetes
df[df$undiagnosed_diabetes==0 &!is.na(df$undiagnosed_diabetes),]$undiagnosed_diabetes=NA #this marks those without diabetes as NA's
df[df$undiagnosed_diabetes==1 & !is.na(df$undiagnosed_diabetes) & df$diq010 %in% c(1),]$undiagnosed_diabetes=0
table(df[df$undiagnosed_diabetes==1 & !is.na(df$undiagnosed_diabetes),]$diq010) #no 7s to worry about (refused to answer)

summary(df$diabetes)
table(df$diabetes)

df$diabetes_categories=as.character(df$diabetes)
df[df$diabetes_categories=="1" &!is.na(df$diabetes_categories),]$diabetes_categories="Diabetes"
df[df$prediabetes==1 & !is.na(df$prediabetes),]$diabetes_categories="Pre-Diabetes"
df[df$normal_a1c==1 & !is.na(df$normal_a1c),]$diabetes_categories="Normal"
df$diabetes_categories=factor(df$diabetes_categories,levels=c("Normal","Pre-Diabetes","Diabetes"))
table(df$diabetes_categories)


df$undiagnosed_prediabetes=df$prediabetes
df[df$undiagnosed_prediabetes==0 &!is.na(df$undiagnosed_prediabetes),]$undiagnosed_prediabetes=NA #this marks those without diabetes as NA's
df[df$undiagnosed_prediabetes==1 & !is.na(df$undiagnosed_prediabetes) & df$diq160 %in% c(1),]$undiagnosed_prediabetes=0
table(df[df$undiagnosed_prediabetes==1 & !is.na(df$undiagnosed_prediabetes),]$diq160) #no 7s to worry about (refused to answer)

table(df$undiagnosed_prediabetes,df$diabetes_categories)

#Diabetes 2 is if they have fasting blood sugar >=126
df$diabetes2=df$diabetes
summary(df$lbxglu)
df[df$lbxglu>=126 &!is.na(df$lbxglu),]$diabetes2=1
table(df$diabetes)
table(df$diabetes2)

#Diabetes 3 is if they have fasting and a1c >=126
df$diabetes3=df$diagnosed_diabetes
df[df$a1c=="[6.5,1e+03)" &!is.na(df$a1c) & df$lbxglu>=126 &!is.na(df$lbxglu),]$diabetes3=1

#Factor ridreth 3 with ref
table(df$ridreth3)
df$ridreth3=factor(df$ridreth3,levels=c("3","1","2","4","6","7"))

#Inverse bmi
summary(df$bmxbmi)
df$bmxbmi_inverse=86.20-df$bmxbmi

###BMI cuts for regression
df$bmi_cuts=cut(df$bmxbmi,breaks=c(0,18,19,20,21,22,23,24,25,26,1000),right = FALSE)
head(df$bmi_cuts)
levels(df$bmi_cuts)

df$bmi_cuts_numeric=as.numeric(df$bmi_cuts)+16
table(df$bmi_cuts_numeric)
df[df$bmi_cuts_numeric %in% c(17,26),]$bmi_cuts_numeric=NA

#BMI cuts 2 
df$bmi_cuts_2=cut(df$bmxbmi,breaks=c(0,18.5,19.5,20.5,21.5,22.5,23.5,24.5,25.5,26.5,1000),right = FALSE)


#BMI new
df$bmi_new=0
df[df$bmxbmi %in% seq(21,22.9,by=.1),]$bmi_new=1

df$bmi_new_21=0
df[df$bmxbmi %in% seq(20,21.9,by=.1),]$bmi_new_21=1

#BMI old
df$bmi_old=0
df[df$bmxbmi %in% seq(24,25.9,by=.1),]$bmi_old=1

#Ridreth 2 combine mexican and other hispanic
df$ridreth2=df$ridreth3
df[df$ridreth2 %in% c(1,2),]$ridreth2=1

df$bmxbmi_sq=df$bmxbmi*df$bmxbmi


#Diabetes and prediabets composite
df$diabetes_or_predm=df$diabetes
df[df$prediabetes==1 &!is.na(df$prediabetes),]$diabetes_or_predm=1
table(df$diabetes_or_predm)

###BMI Missigness
prop.table(table(is.na(df[df$ridageyr>=18 & df$ridstatr==2,]$bmxbmi)))
prop.table(table(is.na(df[df$ridageyr>=18 & df$ridstatr==2,]$lbxgh)))

###############################################
#############Survey Analysis 
#####Prevalence Measures
library(survey)
df$oneperson=1

#Create Survey Frames
df_survey=svydesign(id = ~sdmvpsu, strata = ~sdmvstra, nest=TRUE,
                    weights = ~wtmec8yr, data=df %>% 
                      mutate(# Making the race variable more readable
                        race = case_when(ridreth3 == 1 ~ "Mexican American",
                                         ridreth3 == 2 ~ "Other Hispanic American",
                                         ridreth3 == 3 ~ "White American",
                                         ridreth3 == 4 ~ "Black American",
                                         ridreth3 == 6 ~ "Asian American",
                                         ridreth3 == 7 ~ "Other"), 
                        # Converting race to factor
                        race = factor(race),
                        # Re-leveling the race factor, so that White is base level (as in paper)
                        race = relevel(race, ref = "White American")))

unwtd.count(~oneperson,df_survey)
svytotal(~oneperson,df_survey)
svytotal(~oneperson,subset(df_survey,ridageyr>=12))

df_survey_final=subset(df_survey,(!ridexprg==1 |is.na(ridexprg)))

unwtd.count(~oneperson,df_survey_final) 

race_only_formula <- diabetes ~ race

svyglm(race_only_formula,subset(df_survey_final,ridageyr>=35 & ridageyr<=70 ),family=binomial())

final_model_formula <- diabetes ~ race + ridageyr + bmxbmi +
  I(bmxbmi^2) + race*ridageyr + race*bmxbmi +
  race*I(bmxbmi^2) + ridageyr*bmxbmi + race*ridageyr*bmxbmi

df_survey_combined_race=svydesign(id = ~sdmvpsu, strata = ~sdmvstra, nest=TRUE,
                    weights = ~wtmec8yr, data=df %>% 
                      mutate(# Making the race variable more readable
                        race = case_when(ridreth3 == 1 | ridreth3 == 2 ~ "Hispanic American",
                                         ridreth3 == 3 ~ "White American",
                                         ridreth3 == 4 ~ "Black American",
                                         ridreth3 == 6 ~ "Asian American",
                                         ridreth3 == 7 ~ "Other"), 
                        # Converting race to factor
                        race = factor(race),
                        # Re-leveling the race factor, so that White is base level (as in paper)
                        race = relevel(race, ref = "White American")) %>%
                      filter(race != "Other"))

df_survey_combined_race_final=subset(df_survey_combined_race,(!ridexprg==1 |is.na(ridexprg)))

final_model <- svyglm(final_model_formula,subset(df_survey_combined_race_final,ridageyr>=35 & ridageyr<=70 & bmxbmi >=15 & bmxbmi <=50),family=binomial())

test_data <- data.frame(race = c("White American", "Asian American", "Black American", "Hispanic American"), 
                        ridageyr = rep(35, 4),
                        bmxbmi = rep(25,4))

predict(final_model, newdata = test_data, type = "response")

save_path <- "/home/mcoots/harvard/research/race-in-healthcare/models/"

saveRDS(final_model, file = paste(save_path, "paper_final_model.rda", sep = ""))

# Table 2

bmi_vector <- c(c(25:19), 18.5)
age_vector <- rep(35, length(age_vector))
white_vector <- rep("White American", length(age_vector))
asian_vector <- rep("Asian American", length(age_vector))
black_vector <- rep("Black American", length(age_vector))
hispanic_vector <- rep("Hispanic American", length(age_vector))

table_2_data <- data.frame(race = c(white_vector, asian_vector, black_vector, hispanic_vector),
                           ridageyr = rep(age_vector, 4),
                           bmxbmi = rep(bmi_vector, 4))

table_2_pred <- predict(final_model, newdata = table_2_data, type = "response") * 100

white_pred <- table_2_pred[1:8]
asian_pred <- table_2_pred[9:16]
black_pred <- table_2_pred[17:24]
hispanic_pred <- table_2_pred[25:32]

table_2_replicated <- data.frame(White_Americans = white_pred,
                                 Asian_Americans = asian_pred,
                                 Black_Americans = black_pred,
                                 Hispanic_Americans = hispanic_pred) %>%
  `rownames<-`(bmi_vector)

table_2_replicated

# Table 3
