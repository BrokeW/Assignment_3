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
