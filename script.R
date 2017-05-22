

bank=input.means(bank)
summary(bank)
bank=normalize(bank)
w=create.w.matrix(outer.m)
Y=create.y.matrix(bank,outer.m)
summary(Y)
sd(Y[,1])
Y=normalize(Y)
