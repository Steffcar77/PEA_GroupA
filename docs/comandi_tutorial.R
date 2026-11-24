install.packages("C:/Users/Utente/Downloads/FEAR-MSWin-11.zip", repos = NULL, type = "win.binary")
library(FEAR)
setwd("C:/Users/Utente/Desktop/newcode")
set.seed(131071)
# Dati
data(ccr)
x <- t(ccr[, 2:6]);       y <- t(ccr[, 7:9])
x1 <- t(ccr[1:49,  2:6]); y1 <- t(ccr[1:49,  7:9])
x2 <- t(ccr[50:70, 2:6]); y2 <- t(ccr[50:70, 7:9])
z <- c(rep(1, 49), rep(0, 21))
# Riduzione della dimensionalità per gruppo
t1 <- reduce.dim(x1, y1); x1r <- t1$xred; y1r <- t1$yred
t2 <- reduce.dim(x2, y2); x2r <- t2$xred; y2r <- t2$yred
# 13. Test di convessità (versione fast)
res1 <- test.convexity.fast(X = x1r, Y = y1r, ORIENTATION = 1, METRIC = 2, NSPLIT = 100)
res2 <- test.convexity.fast(X = x2r, Y = y2r, ORIENTATION = 1, METRIC = 2, NSPLIT = 100)
print(res1); print(res2)
res1 <- test.rts(
X = x1r, Y = y1r,
ORIENTATION = 1, METRIC = 2, NSPLIT = 10,
CONVEXITY = FALSE
)
res2 <- test.rts(
X = x2r, Y = y2r,
ORIENTATION = 1, METRIC = 2, NSPLIT = 10,
CONVEXITY = FALSE
)
print(res1); print(res2)
set.seed(131071)
rts1 <- test.rts.fast(X = x1r, Y = y1r, ORIENTATION = 1, METRIC = 2,
NSPLIT = 1000, CONVEXITY = FALSE)
rts2 <- test.rts.fast(X = x2r, Y = y2r, ORIENTATION = 1, METRIC = 2,
NSPLIT = 1000, CONVEXITY = FALSE)
print(rts1); print(rts2)
# 16. Stime FDH finali
d1 <- fdh(x1r, y1r, METRIC = 2)
d2 <- fdh(x2r, y2r, METRIC = 2)
summary(d1); summary(d2)
# 18. Efficienza media
mean(d1); mean(d2)                       # attesi ≈ 0.855 e 0.867
m1 <- mean_eff(x1r, y1r, METRIC = 2, ESTIMATOR = 1); print(m1)
m2 <- mean_eff(x2r, y2r, METRIC = 2, ESTIMATOR = 1); print(m2)
# 19. Confronto delle medie tra i gruppi
mt <- test.means(x1r, y1r, x2r, y2r, METRIC = 2, COMMON = 0, ESTIMATOR = 1)
print(mt)                                # p-value bilaterale atteso ≈ 0.70
save.image("FEAR_tutorial.RData")
savehistory("C:/Users/Utente/Desktop/newcode/comandi_tutorial.R")
