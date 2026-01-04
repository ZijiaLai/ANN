library(neuralnet)

set.seed(12345678)

trainFile="gene.txt"
testFile="testGeneScore.txt"
setwd("")

data = read.table(trainFile, header=T, sep="\t", check.names=F, row.names=1)
data = as.data.frame(t(data))

group = gsub("(.*)\\_(.*)", "\\2", row.names(data))
data$con = ifelse(group=="con",1,0)
data$treat = ifelse(group=="treat",1,0)

fit = neuralnet(con+treat ~ ., data, hidden=5)

data2 = read.table(testFile, header=T, sep="\t", check.names=F, row.names=1)
data2 = t(data2)
sameGene = intersect(colnames(data), colnames(data2))
data2 = data2[,sameGene]

net.predict = compute(fit, data2)$net.result
net.prediction = c("con","treat")[apply(net.predict,1,which.max)]

predict.table = table(gsub("(.*)\\_(.*)", "\\2", row.names(data2)), net.prediction)
predict.table

colnames(net.predict) = c("con","treat")
outTab = rbind(id=colnames(net.predict), net.predict)

write.table(outTab, file="test.neuralPredict.txt", sep="\t", quote=F, col.names=F)
