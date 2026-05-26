x <- seq(90, 110, length.out = 1000)

plot(x, dnorm(x, mean = 101.944, sd = 2), type = "l",
     col = "red", lwd = 2,
     ylim = c(0, 0.3),
     xlab = "Constraint Rankings", ylab = " ")

lines(x, dnorm(x, mean = 100.000, sd = 2), col = "darkorange", lwd = 2)
lines(x, dnorm(x, mean = 98.056, sd = 2), col = "green", lwd = 2)

legend("topright", 
       legend = c("ID-Vowel & ID-Cons = 101.632", "Max-Seg = 100.000", "*Rhotic & *DIPH = 98.368"), 
       col = c("red", "darkorange", "green"),
       lwd = 2,
       cex = 1.4)

abline(v = 101.944, col = "black", lwd = 1, lty = 2)    
abline(v = 100.000, col = "black", lwd = 1, lty = 2)        
abline(v = 98.056, col = "black", lwd = 1, lty = 2)       