library(foreign)
pulse = read.spss("pulse.sav", to.data.frame=TRUE)
# Exercise 1.
# A  group  of  students  participated  in  a  simple  experiment.    First,  they
# recorded  their  height,  weight,  gender,  smoking  status,  usual  activity  level  and  resting
# pulse rate.
# Then they  ipped coins,  and those whose coins came up heads ran on the spot for
# one minute.  Then the entire group recorded their pulse rate once more.
# The data are in the  le
# pulse.sav
# on Studentcentral.  Answer the questions below
# for the entire group and separately for men and women (where it makes sense).  Use at
# least two di erent tests for each of the questions.  Write down the hypothesis and your
# conclusion in each case.


# 1.  Is the resting pulse rate of students equal to 75?    
# H0:resting pulse = 75
# HA:resting pulse <> 75
# use independent t test
t.test(pulse$pulse1,mu=75) # p-value = 0.1029
wilcox.test(pulse$pulse1,mu=75) # p-value = 0.06766
# do not reject null hypothesis

# Is it above 72?
# H0:resting pulse = 72
# HA:resting pulse > 72
t.test(pulse$pulse1,alternative="greater",mu=72) # p-value = 0.1566
wilcox.test(pulse$pulse1,alternative="greater",mu=72)  # p-value = 0.2738
# do not reject null hypothesis

# Is it below 80?
# H0:resting pulse = 80
# HA:resting pulse < 80
t.test(pulse$pulse1,alternative="less",mu=80) # p-value = 1.401e-08
wilcox.test(pulse$pulse1,alternative="less",mu=80) # p-value = 1.339e-07
# reject null hypothesis


# 2.  Does pulse rate increase after running?  
# H0:resting pulse mean(p1) = mean(p2)
# HA:resting pulse mean(p1) <> mean(p2)
# use paired test
t.test(pulse$pulse2[pulse$ran=="ran in place"],pulse$pulse1[pulse$ran=="ran in place"],paired=TRUE,alternative="greater") 
# p-value = 6.324e-09
# reject null hypothesis

# Does pulse rate change without running?:
# H0:resting pulse mean(p1) = mean(p2)
# HA:resting pulse mean(p1) <> mean(p2)
# use 
t.test(pulse$pulse2[pulse$ran=="did not run in place"],pulse$pulse1[pulse$ran=="did not run in place"],alternative="two.sided") 
# p-value = 0.9105
# do not reject null hypothesis

# 3.  Is there a difference in the resting pulse rates of regular and non-regular smokers?
# H0:resting pulse mean(smokers) = mean(non-smokers)
# HA:resting pulse mean(smokers) <> mean(non-smokers)
# use 
t.test(pulse$pulse1[pulse$smokes=="smokes regularly"],pulse$pulse1[pulse$smokes=="does not smoke regularly"],alternative="two.sided") 
# p-value = 0.1432
# do not reject null hypothesis

# 4.  Is there a difference in the resting pulse rates of men and women?
# H0:resting pulse men = women
# HA:resting pulse men <> women
# use 
t.test(pulse$pulse1[pulse$sex=="male"],pulse$pulse1[pulse$sex=="female"],alternative="two.sided") 
# p-value = 0.01238
# do not reject null hypothesis

# 5.  Answer the previous two questions for the second pulse rate and for the change of
# the pulse rate, for the entire group and separately for those who ran and those who
# did not.

# Smokers/non-smokers pulse 2 entire
# H0:pulse 2 pulse mean(smokers) = mean(non-smokers)
# HA:pulse 2 pulse mean(smokers) <> mean(non-smokers)
# use 
t.test(pulse$pulse2[pulse$smokes=="smokes regularly"],pulse$pulse2[pulse$smokes=="does not smoke regularly"],alternative="two.sided") 
# p-value = 0.4571
# do not reject null hypothesis

# Smokers/non-smokers pulse diff entire
# H0:pulse diff mean(smokers) = mean(non-smokers)
# HA:pulse diff mean(smokers) <> mean(non-smokers)
# use 
pulse$pulsediff = pulse$pulse2 - pulse$pulse1
t.test(pulse$pulsediff[pulse$smokes=="smokes regularly"],pulse$pulsediff[pulse$smokes=="does not smoke regularly"],alternative="two.sided") 
# p-value = 0.6342
# do not reject null hypothesis

# Smokers/non-smokers pulse 2 runners
# H0:pulse 2 pulse mean(smokers) = mean(non-smokers)
# HA:pulse 2 pulse mean(smokers) <> mean(non-smokers)
# use 
t.test(pulse$pulse2[pulse$smokes=="smokes regularly" & pulse$ran=="ran in place"],pulse$pulse2[pulse$smokes=="does not smoke regularly" & pulse$ran=="ran in place"],alternative="two.sided") 
# p-value = 0.5735
# do not reject null hypothesis

# Smokers/non-smokers pulse 2 non-runners
# H0:pulse 2 pulse mean(smokers) = mean(non-smokers)
# HA:pulse 2 pulse mean(smokers) <> mean(non-smokers)
# use 
t.test(pulse$pulse2[pulse$smokes=="smokes regularly" & pulse$ran=="did not run in place"],pulse$pulse2[pulse$smokes=="does not smoke regularly" & pulse$ran=="did not run in place"],alternative="two.sided") 
# p-value = 0.2208
# do not reject null hypothesis


