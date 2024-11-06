rm(list = ls())

#=== Legge dei Grandi Numeri e Teorema Centrale del Limite ===

ns <- c(1, 5, 10, 20, 100, 10000)
p <- 2 / 3

pdf("GN_TCL.pdf", width = 7, height = 14)
op <- par(mfrow = c(6, 2))

for (n in ns) {
  
  # Densità della media campionaria
  densita <- dbinom(x = 0:n, size = n, prob = p)
  plot(0:n / n, densita, 
       type = "h", 
       xlab = expression(bar(x)[n]), 
       ylab = "Densità", 
       xlim = c(0, 1),
       main = paste("n =", n), 
       lwd = 2)
  abline(v = p, lwd = 2, col = 2, lty = "dotted")
  
  # Funzione di ripartizione della media campionaria standardizzata
  fdr <- c(0, pbinom(q = 0:n, size = n, prob = p), 1)
  plot(c(-5, sqrt(n) * (0:n / n - p) / sqrt(p * (1 - p)), 5), 
       fdr, 
       type = "s", 
       xlab = "z", 
       ylab = "Funzione di ripartizione", 
       xlim = c(-5, 5), 
       main = paste("n =", n),
       lwd = 2)
  curve(pnorm, -5, 5, add = TRUE, col = 2, lwd = 2)
}

par(op)
dev.off()

