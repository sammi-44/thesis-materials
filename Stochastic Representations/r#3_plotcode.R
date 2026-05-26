#install.packages("ggplot2")
#install.packages("ggbreak")
#library(ggplot2)
#library(ggbreak)
#install.packages("plotrix")
#library(plotrix)

#par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))

layout(matrix(c(1, 2), nrow = 1), widths = c(1, 4))

x1 <- seq(-195, -175, length.out = 1000)

plot(x1, dnorm(x1, mean = -181.454, sd = 2), type = "l",
     col = "brown", lwd = 2,
     ylim = c(0, 0.3),
     xlab = "Constraint Rankings", ylab = " ")

lines(x1, dnorm(x1, mean = -189.967, sd = 2), col = "grey", lwd = 2)

abline(v = -181.454, col = "black", lwd = 1, lty = 2)
abline(v = -189.967, col = "black", lwd = 1, lty = 2)

x2 <- seq(75, 130, length.out = 1000)

#ggplot(mpg, aes(displ, hwy)) +
#  geom_point() +
#  scale_x_break(breaks = c(-2875, 85)) # Removes the range between 3 and 5

plot(x2, dnorm(x2, mean = 107.259, sd = 2), type = "l",
     col = "red", lwd = 2,
     ylim = c(0, 0.3),
     xlab = "Constraint Rankings", ylab = " ")


lines(x2, dnorm(x2, mean = 104.985, sd = 2), col = "orange", lwd = 2)
lines(x2, dnorm(x2, mean = 100.409, sd = 2), col = "yellow", lwd = 2)
lines(x2, dnorm(x2, mean = 99.591, sd = 2), col = "green", lwd = 2)
lines(x2, dnorm(x2, mean = 97.937, sd = 2), col = "darkgreen", lwd = 2)
lines(x2, dnorm(x2, mean = 97.191, sd = 2), col = "blue", lwd = 2)
lines(x2, dnorm(x2, mean = 95.438, sd = 2), col = "purple", lwd = 2)
lines(x2, dnorm(x2, mean = 93.463, sd = 2), col = "pink", lwd = 2)
lines(x2, dnorm(x2, mean = 78.101, sd = 2), col = "black", lwd = 2)


legend("topright", 
       legend = c("*Liquid = 107.259", "*Cor[+ant]/_[+front] = 104.985", "ID-Affricate = 100.409", "NoAffricate = 99.591", "UNIF = 97.937", "*Complex = 97.191", "ID-Sibilant = 95.438", "ID-Continuant = 93.463", "ID-Sonorant = 78.101", "Max-Seg = -181.454", "ID-Place = -189.967"), 
       col = c("red", "orange", "yellow", "green", "darkgreen", "blue", "purple", "pink", "black", "brown", "grey"),
       lwd = 2,
       cex = 1.35)

abline(v = 107.259, col = "black", lwd = 1, lty = 2)    
abline(v = 104.985, col = "black", lwd = 1, lty = 2)        
abline(v = 100.409, col = "black", lwd = 1, lty = 2)       
abline(v = 99.591, col = "black", lwd = 1, lty = 2)
abline(v = 97.937, col = "black", lwd = 1, lty = 2)        
abline(v = 97.191, col = "black", lwd = 1, lty = 2)       
abline(v = 95.438, col = "black", lwd = 1, lty = 2)
abline(v = 93.463, col = "black", lwd = 1, lty = 2)        
abline(v = 78.101, col = "black", lwd = 1, lty = 2) 

  
# Apply gap.plot function
#p + scale_x_break(c(-2875, 85))
#scale_x_break(c(-2875, 85), symbol = "slash")

#par(mfrow = c(1, 1))

#FIGURE OUT HOW TO DO 2 IN 1