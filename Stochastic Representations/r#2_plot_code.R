x <- seq(89, 119, length.out = 1000)

plot(x, dnorm(x, mean = 106.418, sd = 2), type = "l",
     col = "red", lwd = 2,
     ylim = c(0, 0.3),
     xlab = "Constraint Rankings", ylab = " ")

lines(x, dnorm(x, mean = 101.145, sd = 2), col = "darkorange", lwd = 2)
lines(x, dnorm(x, mean = 100.221, sd = 2), col = "green", lwd = 2)
lines(x, dnorm(x, mean = 98.634, sd = 2), col = "blue", lwd = 2)
lines(x, dnorm(x, mean = 94.997, sd = 2), col = "purple", lwd = 2)

legend("topright", 
       legend = c("Complex-Ons = 106.418", "ONSET & ID-Vowel = 101.145", "*Ons/Plos = 100.221", "ID-Ons & *Ons/Glott = 98.634", "Max-Seg = 94.997"), 
       col = c("red", "darkorange", "green", "blue", "purple"),
       lwd = 2, 
       cex = 1.4)

abline(v = 106.418, col = "black", lwd = 1, lty = 2)    
abline(v = 101.145, col = "black", lwd = 1, lty = 2)        
abline(v = 100.221, col = "black", lwd = 1, lty = 2)       
abline(v = 98.634, col = "black", lwd = 1, lty = 2)       
abline(v = 94.997, col = "black", lwd = 1, lty = 2)       