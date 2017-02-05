library(foreign)
basket = read.spss("basketball.sav", to.data.frame=TRUE)
# Exercise 3. 12 basketball players were told to shoot 25 free throws and the number of
# goals was recorded. They were then given an extensive workout and then told to shoot
# another 25 free throws; the number of goals was again recorded. The data are in the file
# basketball.sav on Studentcentral. Does a workout affect goal scoring ability?

# H0: An extensive workout does not affect goal scoring ability
# H1: An extensive workout does affect goal scoring ability
t.test(basket$BEFORE,basket$AFTER, paired = TRUE)
# p-value = 0.03738
# reject null hypothesis