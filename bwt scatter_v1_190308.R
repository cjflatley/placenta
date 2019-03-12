
library(Hmisc)  
library(summarytools)
library(ggplot2)
library(glm2)

mfr<-spss.get("C:/Users/cjfla/Desktop/v10/spss/2018_02_01_PDB315_MFR_520_v10.sav", 
              lowernames=FALSE, datevars = NULL,
              use.value.labels = TRUE, to.data.frame = TRUE,
              max.value.labels = Inf, force.single=TRUE,
              allow=NULL, charfactor=FALSE, reencode = NA)

mfr1<-subset(mfr, PLACENTAVEKT<2000 & PLACENTAVEKT>200 & VEKT>1000 & (KJONN=="Boy"|KJONN=="Girl"), select=c(PLACENTAVEKT,VEKT, SVLEN.UL.DG, SVLEN.SM.DG, FAAR, HELSEREGION, KJONN))
mfr2<-subset(mfr, PLACENTAVEKT<2000 & PLACENTAVEKT>200 & VEKT>1000 & (KJONN=="Boy"|KJONN=="Girl")
             & FLERFODSEL=="Single birth", select=c(PLACENTAVEKT,VEKT, SVLEN.UL.DG, SVLEN.SM.DG, FAAR, HELSEREGION, KJONN))

mfr3<-mfr2[(mfr2$PLACENTAVEKT<1500|mfr2$VEKT>2200),]

mfr4<-mfr3[(mfr3$PLACENTAVEKT<1700|mfr3$VEKT>4000),]

ggplot(mfr1, aes(x=PLACENTAVEKT, y=VEKT))+ geom_point() + 
  stat_smooth(method='lm',
              formula = y ~ x )

ggplot(mfr2, aes(x=PLACENTAVEKT, y=VEKT))+ geom_point() + 
  stat_smooth(method='lm',
             formula = y ~ x )

ggplot(mfr3, aes(x=PLACENTAVEKT, y=VEKT))+ geom_point() + 
  stat_smooth(method='lm',
              formula = y ~ x )

ggplot(mfr4, aes(x = PLACENTAVEKT, y = VEKT)) + geom_point() + 
   xlab("Placental Weight") + ylab("Birth Weight") +
  (geom_smooth(method="gam",formula=y ~ s(x, bs = "cs"))) 
  
stat_smooth(method='lm',
              formula = y ~ x + I(x^2))

glm4<-glm2(VEKT~PLACENTAVEKT + I(PLACENTAVEKT^2), data=mfr4, family=gaussian)
summary(glm4)



# create multiple linear model
lm_fit <- lm(vekt ~ PLACENTAVEKT + SVLEN.UL.DG, data=mfr1)
summary(lm_fit)

# save predictions of the model in the new data frame 
# together with variable you want to plot against
predicted_df <- mfr1(bwt_pred = predict(lm_fit, mfr1), hp=mfr1$hp)

# this is the predicted line of multiple linear regression
ggplot(data = df, aes(x = mpg, y = hp)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = predicted_df, aes(x=mpg_pred, y=hp))


