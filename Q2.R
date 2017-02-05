library(foreign)
tablets = read.spss("tablets.sav", to.data.frame=TRUE)
# Exercise 2. A formulation scientist studies the effects of lubricant (magnesium stearate)
# and compression force on the time required for tablet disintegration. Tablets were formulated
# to incorporate either 0.5% or 1% w/w magnesium stearate and were manufactured
# by compression at 10, 15 or 20 tonnes in a pilot scale tablet press. The times required for
# disintegration of samples of five tablets at each combination are in the file tablets.sav
# on Studentcentral.
# Use two different tests for each of the questions below. Write down the hypothesis
# and your conclusion in each case.

# 1. Is there a difference in the disintegration times for the two concentrations of magnesium
# stearate, if we ignore the compression force?
# H0:Ignoring compression force, there is no difference in disintegration times between 0.5% and 1% w/w magnesium stearate.
# HA:Ignoring compression force, there is a difference in disintegration times between 0.5% and 1% w/w magnesium stearate.
# use:
tablets$ignore=tablets$TIME/tablets$COMPRESS
t.test(tablets$ignore[tablets$MS==1.0],tablets$ignore[tablets$MS==0.5],alternative="two.sided")
# p-value = 1.516e-08
# reject null hypothesis

# 2. Do the disintegration times increase with the concentration of magnesium stearate,
# if the compression force is 20 tonnes?
# H0: At a compression force of 20 tonnes, there is no difference in disintegration times between 0.5% and 1% w/w magnesium stearate.
# HA: At a compression force of 20 tonnes, there is a difference in disintegration times between 0.5% and 1% w/w magnesium stearate.
# use:
t.test(tablets$TIME[tablets$MS==1.0 & tablets$COMPRESS==20],tablets$TIME[tablets$MS==0.5 & tablets$COMPRESS==20],alternative="two.sided")
# p-value = 5.288e-09
# reject null hypothesis



# 3. Is there a difference in the disintegration times for 0.5% concentration of magnesium
# stearate with the compression force of 20 tonnes and 1% concentration of magnesium
# stearate with the compression force of 10 tonnes?

# H0: There is no difference in the disintegration times for 0.5% concentration of magnesium
# stearate with the compression force of 20 tonnes and 1% concentration of magnesium.
# HA: There is a difference in the disintegration times for 0.5% concentration of magnesium
# stearate with the compression force of 20 tonnes and 1% concentration of magnesium.
t.test(tablets$TIME[tablets$MS==1.0 & tablets$COMPRESS==10],tablets$TIME[tablets$MS==0.5 & tablets$COMPRESS==20],alternative="two.sided")
# p-value = 3.313e-07
# reject null hypothesis