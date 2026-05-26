par(mfrow = c(1,1))

x <- seq(90, 110, length.out = 1000)

plot(x, dnorm(x, mean = 101.581, sd = 2), type = "l",
     col = "red", lwd = 2,
     ylim = c(0, 0.3),
     xlab = "Constraint Rankings", ylab = " ")

lines(x, dnorm(x, mean = 100.000, sd = 2), col = "darkorange", lwd = 2)
lines(x, dnorm(x, mean = 98.419, sd = 2), col = "green", lwd = 2)

legend("topright", 
       legend = c("ID-Place & ID-Cons = 101.240", "ID-Cont & ID-Son & Max-Seg = 100.000", "*#F & *V-esh = 98.760"), 
       col = c("red", "darkorange", "green"),
       lwd = 2,
       cex = 1.4)

abline(v = 101.581, col = "black", lwd = 1, lty = 2)    
abline(v = 100.000, col = "black", lwd = 1, lty = 2)        
abline(v = 98.419, col = "black", lwd = 1, lty = 2)   