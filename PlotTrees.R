library(TreeTools)
library(ggtree)

newtree <- ReadTntTree("tree.tre")
sctree <- ReadTntTree("sc.tre")

newtree.plt <- ggplot(newtree, aes(x, y)) + geom_tree() + theme_tree()
sctree.plt <- ggplot(sctree, aes(x, y)) + geom_tree() + theme_tree()

newtree.plt + scale_y_reverse() + geom_tiplab(size=2)
sctree.plt + scale_y_reverse() + geom_tiplab(size=2)


pdf("newtree.pdf")
newtree.plt + scale_y_reverse() + geom_tiplab(size=2)
dev.off()

pdf("sctree.pdf")
sctree.plt + scale_y_reverse() + geom_tiplab(size=2)
dev.off()