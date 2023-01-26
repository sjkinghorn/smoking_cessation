library(asaur)
library(tidyverse)
library(survival)
library(ggfortify)
data("pharmacoSmoking")
smoking = pharmacoSmoking
df = filter(smoking, ttr!=0)

# survival object
Y = Surv(df$ttr, df$relapse==1)

# intercept only
kmfit1 = survfit(Y ~ 1)
plot(kmfit1)
autoplot(kmfit1) + 
  labs(x = "\n Cessation Time (Days) ", y = "Survival Probabilities \n", 
       title = "Cessation Times \n Of Smokers \n") + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12))

# treatment
kmfit2 = survfit(Y ~ factor(grp), data=df)

plot(kmfit2, lty = c('solid', 'dashed'), col=c('red', 'blue'),
     xlab='survival time in years', ylab='survival probabilities')
legend('topright', c('Combination','Patch only'), lty=c('solid','dashed'),
       col=c('red', 'blue'), cex=.5)

autoplot(kmfit2) + 
  labs(x = "\n Cessation Time (Days) ", y = "Survival Probabilities \n", 
       title = "Cessation Times \n Of Smokers \n") + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        legend.title = element_text(face="bold", size = 10))

survdiff(Y ~ factor(grp), data=df)

# employment
kmfit3 = survfit(Y ~ df$employment)

plot(kmfit3, lty = c('solid', 'dashed', 'dotted'), col=c('black','red', 'blue'),
     xlab='survival time in years',ylab='survival probabilities')
legend('topright', c('Full-time','Other', 'Part-time'), lty=c('solid','dashed', 'dotted'),
       col=c('black','red', 'blue'), cex=.5)

autoplot(kmfit3) + 
  labs(x = "\n Cessation Time (Days) ", y = "Survival Probabilities \n", 
       title = "Cessation Times \n Of Smokers \n") + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        legend.title = element_text(face="bold", size = 10))

survdiff(Y ~ employment, data=df)

# ph assumption
plot(kmfit2,fun='cloglog',xlab='time in years using logarithmic
     scale',ylab='log-log survival', main='log-log curves by treatment')
plot(kmfit3,fun='cloglog',xlab='time in years using logarithmic
     scale',ylab='log-log survival', main='log-log curves by employment')

# cox model
model1 = coxph(Y ~ grp, data=df)
summary(model1)

model2 = coxph(Y ~ grp + age + employment, data=df)
summary(model2)

model3 = coxph(Y ~ grp + age + employment, data=df)
summary(model3)

# compare with lrt
lrt_surv = function(mod_full, mod_reduced, df) {
  lrt = (-2)*( mod_reduced$loglik[2] - mod_full$loglik[2])
  pvalue = 1-pchisq(lrt, df)
  return(pvalue)
}
lrt_surv(model2, model3, 2)

anova(model2, model3)

# numerical ph test
cox.zph(model3, transform=rank)
plot(cox.zph(model3, transform=rank), se=F, var='grp')

# adjusted survival curve
pattern1 = data.frame(grp='patchOnly', age=mean(df$age), employment='ft')
plot(survfit(model3, newdata=pattern1), conf.int=F, 
     main="Adjusted survival for grp=patchOnly, age=mean(age), employment=ft", 
     cex.main=.7)

autoplot(survfit(model3, newdata=pattern1)) + 
  labs(x = "\n Cessation Time (Days) ", y = "Survival Probabilities \n", 
       title = "Adjusted survival for \n grp=patchOnly, age=mean(age), employment=ft \n") + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        legend.title = element_text(face="bold", size = 10))

# asses weibull assumption
plot(survfit(Y ~ factor(grp), data=df),fun='cloglog',xlab='time in years using logarithmic
     scale',ylab='log-log survival', main='log-log curves by treatment')

# weibull model
length(df$ttr[df$ttr==0])
df_subset = filter(df, ttr!=0)
weib = survreg(Surv(df_subset$ttr, df_subset$relapse==1) ~ grp + age + employment, 
               data=df_subset, dist='weibull')
summary(weib)

# frailty model
cox_frail = coxph(Surv(df$ttr, df$relapse==1) ~ grp + age + employment + 
                    frailty(id, distribution="gamma"), data=df)
summary(cox_frail)


