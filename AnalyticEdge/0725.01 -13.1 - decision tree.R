#CHAPTER 13: DECISION TREES

#Example 1: Prostate Cancer 

prostate <- read.csv("C:/Users/Nas/Documents/R/prostate.csv")
prostate
install.packages('tree')
library(tree)
## Construct the tree
pstree <- tree(lcavol ~., data=prostate, mindev=0.1, mincut=1)
pstree <- tree(lcavol ~., data=prostate, mincut=1)
pstree
plot(pstree, col=9)
text(pstree, digits=0)

pstcut <- prune.tree(pstree,k=1.7)
plot(pstcut)
pstcut

pstcut <- prune.tree(pstree,k=2.05)
plot(pstcut)
pstcut

pstcut <- prune.tree(pstree,k=3)
plot(pstcut)
pstcut

pstcut <- prune.tree(pstree)
pstcut
plot(pstcut)

pstcut <- prune.tree(pstree,best=3)
pstcut
plot(pstcut)

## Use cross-validation to prune the tree
set.seed(2)
cvpst <- cv.tree(pstree, K=10)
cvpst$size
cvpst$dev
plot(cvpst, pch=21, bg=8, type="p", cex=1.5, ylim=c(65,100))

pstcut <- prune.tree(pstree, best=3)
pstcut
plot(pstcut, col=8)
text(pstcut)

## Plot what we end up with
plot(prostate[,c("lcp","lpsa")],cex=0.2*exp(prostate$lcavol))
abline(v=.261624, col=4, lwd=2)
lines(x=c(-2,.261624), y=c(2.30257,2.30257), col=4, lwd=2)
