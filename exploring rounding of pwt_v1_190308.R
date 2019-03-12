
mfr5<-subset(mfr4, mfr4$FAAR<2000,select=c(PLACENTAVEKT,VEKT, SVLEN.UL.DG, SVLEN.SM.DG, FAAR))

ggplot(mfr5, aes(x = PLACENTAVEKT, y = VEKT)) + geom_point() + 
  xlab("Placental Weight") + ylab("Birth Weight") +
  (geom_smooth(method="gam",formula=y ~ s(x, bs = "cs"))) 

mfr6<-subset(mfr4, mfr4$FAAR>1999 & mfr4$FAAR<2001,select=c(PLACENTAVEKT,VEKT, SVLEN.UL.DG, SVLEN.SM.DG, FAAR))

ggplot(mfr6, aes(x = PLACENTAVEKT, y = VEKT)) + geom_point() + 
  xlab("Placental Weight") + ylab("Birth Weight") +
  (geom_smooth(method="gam",formula=y ~ s(x, bs = "cs"))) 

mfr7<-subset(mfr4, mfr4$FAAR>2000 & mfr4$FAAR<2002,select=c(PLACENTAVEKT,VEKT, SVLEN.UL.DG, SVLEN.SM.DG, FAAR))

ggplot(mfr7, aes(x = PLACENTAVEKT, y = VEKT)) + geom_point() + 
  xlab("Placental Weight") + ylab("Birth Weight") +
  (geom_smooth(method="gam",formula=y ~ s(x, bs = "cs"))) 

mfr8<-subset(mfr4, mfr4$FAAR>2001 & mfr4$FAAR<2003,select=c(PLACENTAVEKT,VEKT, SVLEN.UL.DG, SVLEN.SM.DG, FAAR))

ggplot(mfr8, aes(x = PLACENTAVEKT, y = VEKT)) + geom_point() + 
  xlab("Placental Weight") + ylab("Birth Weight") +
  (geom_smooth(method="gam",formula=y ~ s(x, bs = "cs"))) 

mfr9<-subset(mfr4, mfr4$FAAR>2002 & mfr4$FAAR<2004,select=c(PLACENTAVEKT,VEKT, SVLEN.UL.DG, SVLEN.SM.DG, FAAR))

ggplot(mfr9, aes(x = PLACENTAVEKT, y = VEKT)) + geom_point() + 
  xlab("Placental Weight") + ylab("Birth Weight") +
  (geom_smooth(method="gam",formula=y ~ s(x, bs = "cs"))) 

mfr10<-subset(mfr4, mfr4$FAAR>2003 & mfr4$FAAR<2005,select=c(PLACENTAVEKT,VEKT, SVLEN.UL.DG, SVLEN.SM.DG, FAAR))

ggplot(mfr10, aes(x = PLACENTAVEKT, y = VEKT)) + geom_point() + 
  xlab("Placental Weight") + ylab("Birth Weight") +
  (geom_smooth(method="gam",formula=y ~ s(x, bs = "cs"))) 

mfr11<-subset(mfr4, mfr4$FAAR>2004 & mfr4$FAAR<2006,select=c(PLACENTAVEKT,VEKT, SVLEN.UL.DG, SVLEN.SM.DG, FAAR))

ggplot(mfr11, aes(x = PLACENTAVEKT, y = VEKT)) + geom_point() + 
  xlab("Placental Weight") + ylab("Birth Weight") +
  (geom_smooth(method="gam",formula=y ~ s(x, bs = "cs"))) 

mfr12<-subset(mfr4, mfr4$FAAR>2005 & mfr4$FAAR<2007,select=c(PLACENTAVEKT,VEKT, SVLEN.UL.DG, SVLEN.SM.DG, FAAR))

ggplot(mfr12, aes(x = PLACENTAVEKT, y = VEKT)) + geom_point() + 
  xlab("Placental Weight") + ylab("Birth Weight") +
  (geom_smooth(method="gam",formula=y ~ s(x, bs = "cs"))) 

mfr13<-subset(mfr4, mfr4$FAAR>2006 & mfr4$FAAR<2008,select=c(PLACENTAVEKT,VEKT, SVLEN.UL.DG, SVLEN.SM.DG, FAAR))

ggplot(mfr13, aes(x = PLACENTAVEKT, y = VEKT)) + geom_point() + 
  xlab("Placental Weight") + ylab("Birth Weight") +
  (geom_smooth(method="gam",formula=y ~ s(x, bs = "cs"))) 

mfr14<-subset(mfr4, mfr4$FAAR>2007 & mfr4$FAAR<2009,select=c(PLACENTAVEKT,VEKT, SVLEN.UL.DG, SVLEN.SM.DG, FAAR))

ggplot(mfr14, aes(x = PLACENTAVEKT, y = VEKT)) + geom_point() + 
  xlab("Placental Weight") + ylab("Birth Weight") +
  (geom_smooth(method="gam",formula=y ~ s(x, bs = "cs"))) 

mfr15<-subset(mfr4, mfr4$FAAR>2008 & mfr4$FAAR<2010,select=c(PLACENTAVEKT,VEKT, SVLEN.UL.DG, SVLEN.SM.DG, FAAR))

ggplot(mfr15, aes(x = PLACENTAVEKT, y = VEKT)) + geom_point() + 
  xlab("Placental Weight") + ylab("Birth Weight") +
  (geom_smooth(method="gam",formula=y ~ s(x, bs = "cs"))) 


mfr16<-subset(mfr4, mfr4$FAAR>2000 & mfr4$FAAR<2002 & HELSEREGION=="South/East",select=c(PLACENTAVEKT,VEKT, SVLEN.UL.DG, SVLEN.SM.DG, FAAR))

ggplot(mfr16, aes(x = PLACENTAVEKT, y = VEKT)) + geom_point() + 
  xlab("Placental Weight") + ylab("Birth Weight") +
  (geom_smooth(method="gam",formula=y ~ s(x, bs = "cs"))) 