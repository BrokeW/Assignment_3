#Create the data structure
heights <- c(27.75, 24.5, 25.5, 26, 25, 27.75, 26.5, 27, 26.75, 26.75, 27.5)
circ <- c(17.5, 17.1, 17.1, 17.3, 16.9, 17.6, 17.3, 17.5, 17.3, 17.5, 17.5)
data <- data.frame(heights, circ)
#Inspect graphically the data
plot(data$heights, data$circ)
#Test the potential linear correlation between heights and circumferences
cor.test(data$circ, data$heights)
#Quantify the relationship
m <- lm(data$circ ~ data$heights)
abline(m)
#Compute the Cook's distance to screen out any outlier
CD <- cooks.distance(m)
plot(CD)
#Remove the suspicious points and make the "clean" data
data.clean <- data[c(-2, -5), ]
#Test the potential linear correlation of the cleaned dataset
cor.test(data.clean$circ, data.clean$heights)
#Quantify the new relationship
m.cleaned <- lm(data.clean$circ ~ data.clean$heights)
plot(data.clean$heights, data.clean$circ)
abline(m.cleaned)

#Check the distribution of residuals
m.cleaned.rs <- rstudent(m.cleaned)
plot(data.clean$heights, m.cleaned.rs)
qqnorm(m.cleaned$res)
qqline(m.cleaned$res)
hist(m.cleaned$res)
#Looking very good!

#Try to fit a quadratic model
plot(data$heights, data$circ)
H2 <- data$heights^2
m2 <- lm (data$circ ~ data$heights+H2)
plot(m2)