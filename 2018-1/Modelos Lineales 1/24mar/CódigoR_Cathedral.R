xminstalled.packages("faraway")
library(faraway)
attach(cathedral)
str(cathedral)
tab<- rownames(cathedral)

plot(cathedral$x,cathedral$y)
text(cathedral$x,cathedral$y,lab = tab)

n <- dim(cathedral)[1];n
x = cathedral$x; x
y <- cathedral$y; y
cathedral

xm = sum(x)/n
ym = sum(y)/n
ssx = sum (x^2)
ssy = sum (y^2)
sxy = sum (x*y)
ssxc = ssx-n*xm^2
ssyc = ssy-n*ym^2
sxyc = sxy-n*xm*ym
varx = ssxc/n
covxy = sxyc/n

bh = sxyc/ssxc
ah = ym - bh*xm

abline(ah,bh,col="red")

w = (x-xm)^2 / ssxc
b = (y-ym)/(x-xm)



