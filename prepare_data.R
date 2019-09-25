# prepare data for guest lecture
# Natasha Zhang Foutz
# Associate Professor of Commerce
# McIntire School of Commerce
# 340 Rouss and Robertson Halls


# for linear models
# page 250, ALSM
# ps = patient satisfaction score
# age = age of patient
# illness = severity of illness
# anxiety = anxiety level

ps <- read.table("/Users/jcf2d/Box Sync/applied_linear_statistical_models/Chapter  6 Data Sets/CH06PR15.txt")
names(ps) <- c("ps", "age", "illness", "anxiety")
write.csv(ps, file = "/Users/jcf2d/Box Sync/__Consults/Natasha_Foutz/patient_satisfaction.csv", row.names = F)

# for logistic regression
# page 1355, ALSM
# vcf = presence or absence of venture capital funding
# cmpy_value = est face value of company from prospectus
# shares = num shares offered
# buyout = presence/absence of leveraged buyout

ipo <- read.table("/Users/jcf2d/Box Sync/applied_linear_statistical_models/Appendix C Data Sets/APPENC11.txt")
names(ipo) <- c("id", "vcf", "cmpy_value", "shares", "buyout")
write.csv(ipo, file = "/Users/jcf2d/Box Sync/__Consults/Natasha_Foutz/ipo.csv", row.names = F)
