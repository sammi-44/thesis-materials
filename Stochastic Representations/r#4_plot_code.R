#install.packages("ggplot2")
#install.packages("ggbreak")
#library(ggplot2)
#library(ggbreak)
#install.packages("plotrix")
#library(plotrix)

par(mfrow = c(1, 3))
layout(matrix(c(1, 2, 3), nrow = 1), widths = c(1, 1, 2))

x1 <- seq(-4163, -4143, length.out = 1000)

plot(x1, dnorm(x1, mean = -4153.438, sd = 2), type = "l",
     col = "blue", lwd = 2,
     ylim = c(0, 0.3),
     xlab = "Constraint Rankings", ylab = " ")

abline(v = -4153.438, col = "black", lwd = 1, lty = 2)

x2 <- seq(-2890, -2870, length.out = 1000)

plot(x2, dnorm(x2, mean = -2880.258, sd = 2), type = "l",
     col = "purple", lwd = 2,
     ylim = c(0, 0.3),
     xlab = "Constraint Rankings", ylab = " ")

abline(v = -2880.258, col = "black", lwd = 1, lty = 2)

x3 <- seq(90, 110, length.out = 1000)

#ggplot(mpg, aes(displ, hwy)) +
#  geom_point() +
#  scale_x_break(breaks = c(-2875, 85)) # Removes the range between 3 and 5

plot(x3, dnorm(x3, mean = 100.533, sd = 2), type = "l",
     col = "red", lwd = 2,
     ylim = c(0, 0.3),
     xlab = "Constraint Rankings", ylab = " ")

lines(x3, dnorm(x3, mean = 99.467, sd = 2), col = "darkorange", lwd = 2)

legend("topright", 
       legend = c("ID-COns", "*Rhotic", "Max-Seg & ONSET", "ID-Ons & ID-Place"), 
       col = c("red", "darkorange", "green", "blue"),
       lwd = 2, 
       cex = 1.5)

abline(v = 100.533, col = "black", lwd = 1, lty = 2)   
abline(v = 99.467, col = "black", lwd = 1, lty = 2)
       

# Apply gap.plot function
#p + scale_x_break(c(-2875, 85))
#scale_x_break(c(-2875, 85), symbol = "slash")

#par(mfrow = c(1, 1))

#FIGURE OUT HOW TO DO 2 IN 1