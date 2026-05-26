x <- seq(85, 115, length.out = 1000)

plot(x, dnorm(x, mean = 105.895, sd = 2), type = "l",
     col = "red", lwd = 2,
     ylim = c(0, 0.3),
     xlab = "Constraint Rankings", ylab = " ")

lines(x, dnorm(x, mean = 100.068, sd = 2), col = "darkorange", lwd = 2)
lines(x, dnorm(x, mean = 99.932, sd = 2), col = "green", lwd = 2)
lines(x, dnorm(x, mean = 96.107, sd = 2), col = "blue", lwd = 2)
lines(x, dnorm(x, mean = 94.037, sd = 2), col = "purple", lwd = 2)
lines(x, dnorm(x, mean = 92.103, sd = 2), col = "pink", lwd = 2)

legend("topright", 
       legend = c("*Rhotic & *Cmplx = 105.895", "ID-Place = 100.068", "Parse-Syll = 99.932", "*Max-Seg = 96.107", "ID-Voice = 94.037", "ID-Cons = 92.103"), 
       col = c("red", "darkorange", "green", "blue", "purple", "pink"),
       lwd = 2, 
       cex = 1.5)

abline(v = 105.895, col = "black", lwd = 1, lty = 2)    
abline(v = 100.068, col = "black", lwd = 1, lty = 2)        
abline(v = 99.932, col = "black", lwd = 1, lty = 2)       
abline(v = 96.107, col = "black", lwd = 1, lty = 2)       
abline(v = 94.037, col = "black", lwd = 1, lty = 2)       
abline(v = 92.103, col = "black", lwd = 1, lty = 2) 