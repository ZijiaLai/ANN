library(limma)

expFile=" "
conFile=" "
treatFile=" "
diffFile="diff.txt"
setwd("")

rt = read.table(expFile, header=T, sep="\t", check.names=F)
rt = as.matrix(rt)
rownames(rt) = rt[,1]
exp = rt[,2:ncol(rt)]
data = matrix(as.numeric(as.matrix(exp)), nrow=nrow(exp), dimnames=list(rownames(exp), colnames(exp)))
rt = avereps(data)

qx = as.numeric(quantile(rt, c(0,0.25,0.5,0.75,0.99,1), na.rm=T))
LogC = (qx[5]>100) || ((qx[6]-qx[1])>50 && qx[2]>0)

if(LogC){
  rt[rt<0]=0
  rt = log2(rt+1)
}

data = normalizeBetweenArrays(rt)

con = read.table(conFile, header=F)
treat = read.table(treatFile, header=F)

conData = data[, as.vector(con[,1])]
treatData = data[, as.vector(treat[,1])]
data = cbind(conData, treatData)

colnames(data) = paste0(colnames(data), "_", c(rep("con",ncol(conData)), rep("treat",ncol(treatData))))

diffRT = read.table(diffFile, header=T, sep="\t", check.names=F, row.names=1)
sameGene = intersect(row.names(data), row.names(diffRT))
data = data[sameGene,]
diffRT = diffRT[sameGene,]

dataUp = data[diffRT$logFC>0,]
dataDown = data[diffRT$logFC<0,]

dataUp2 = t(apply(dataUp,1,function(x)ifelse(x>median(x),1,0)))
dataDown2 = t(apply(dataDown,1,function(x)ifelse(x>median(x),0,1)))

outTab = rbind(dataUp2, dataDown2)
outTab = rbind(id=colnames(outTab), outTab)

write.table(outTab, file="testGeneScore.txt", sep="\t", quote=F, col.names=F)
