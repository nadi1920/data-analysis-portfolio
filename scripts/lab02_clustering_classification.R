install.packages(c("gplots", "plotly", "htmlwidgets", "cluster",

"factoextra", "patchwork", "RColorBrewer",

"caTools", "e1071", "caret", "reshape",

"vegan", "NbClust"))

library(gplots)

library(plotly)

library(htmlwidgets)

library(cluster)

library(factoextra)

library(patchwork)

library(RColorBrewer)

library(caTools)

library(e1071)

library(caret)

library(reshape)

library(vegan)

library(NbClust)

set.seed(123)

getwd()

list.files()

data <- read.csv(file.path("data", "var_7_lab2.csv"), sep = ";", header = TRUE)

View(data)

str(data)

dim(data)

p <- 4

quant <- data[, 2:(p+1)]

View(quant)

data1 <- scale(quant, center = TRUE, scale = TRUE)

View(data1)

colMeans(data1)

apply(data1, 2, sd)

dim(data1)

dist.eucl <- dist(data1, method = "euclidean")

clust.ward <- hclust(dist.eucl, "ward.D2")

clust.ward

plot(clust.ward, hang = -1,

main = "Дендрограмма: Ward + Euclidean",

xlab = "Объекты", ylab = "Расстояние")

x <- 1:(nrow(data1) - 1)

plot(x, clust.ward$height, type = "b",

xlab = "Номер шага", ylab = "Коэффициент слияния",

main = "Коэффициенты слияния (Ward + Euclidean)")

k1 <- 4

plot(clust.ward, hang = -1,

main = "Дендрограмма с кластерами (Ward)")

rect.hclust(clust.ward, k = k1, border = "red")

d2.ward <- cophenetic(clust.ward)

cor(dist.eucl, d2.ward)

cosine_similarity <- function(A, B) {

sum(A * B) / (sqrt(sum(A^2)) * sqrt(sum(B^2)))

}

cosine_similarity(as.matrix(dist.eucl), as.matrix(d2.ward))

mantel_result <- mantel(as.dist(dist.eucl), as.dist(d2.ward))

mantel_result

dist.cheb <- dist(data1, method = "maximum")

clust.single <- hclust(dist.cheb, "single")

clust.single

plot(clust.single, hang = -1,

main = "Дендрограмма: Single + Chebyshev",

xlab = "Объекты", ylab = "Расстояние")

plot(x, clust.single$height, type = "b",

xlab = "Номер шага", ylab = "Коэффициент слияния",

main = "Коэффициенты слияния (Single + Chebyshev)")

k2 <- 3

plot(clust.single, hang = -1,

main = "Дендрограмма с кластерами (Single)")

rect.hclust(clust.single, k = k2, border = "blue")

d2.single <- cophenetic(clust.single)

cor(dist.cheb, d2.single)

cosine_similarity(as.matrix(dist.cheb), as.matrix(d2.single))

data3 <- as.matrix(data1)

dist.heat  <- function(x) dist(x, method = "euclidean")

clust.heat <- function(x) hclust(x, "ward.D2")

heatmap.2(data3,

distfun = dist.heat,

hclustfun = clust.heat,

trace = "none",

density.info = "none",

margins = c(8, 6),

main = "Тепловая карта + дендрограмма (Ward + Euclidean)")

groups.ward <- cutree(clust.ward, k = k1)

table(groups.ward)

mean.cl <- sapply(1:k1, function(i)

colMeans(data1[groups.ward == i, , drop = FALSE]))

mean.cl

cols <- brewer.pal(k1, "Set2")

plot(1:p, mean.cl[, 1], type = "b", col = cols[1],

xaxt = "n", ylim = range(mean.cl),

xlab = "Признак", ylab = "Среднее значение",

main = "Средние значения по кластерам (Ward)")

axis(1, at = 1:p, labels = colnames(data1))

for (i in 2:k1) lines(1:p, mean.cl[, i], type = "b", col = cols[i])

legend("topright", legend = paste("Кластер", 1:k1),

col = cols, lty = 1, pch = 1)

onehot <- function(v, prefix) {

v <- factor(v)

m <- sapply(levels(v), function(l) as.numeric(v == l))

colnames(m) <- paste0(prefix, "_", levels(v))

m

}

qual <- data[, c("Prof", "Gender")]

q <- do.call(cbind, lapply(names(qual), function(n) onehot(qual[[n]], n)))

head(q)

stopifnot(all(rowSums(q) == ncol(qual)))

dist.q <- dist(q, method = "binary")

clust.q <- hclust(dist.q, "ward.D2")

plot(clust.q, hang = -1,

main = "Дендрограмма (качественные признаки)")

plot(1:(nrow(q) - 1), clust.q$height, type = "b",

xlab = "Номер шага", ylab = "Коэффициент слияния",

main = "Коэффициенты слияния (качественные)")

k3 <- 3

plot(clust.q, hang = -1, main = "Качественные: k = 3")

rect.hclust(clust.q, k = k3, border = "blue")

groups.q <- cutree(clust.q, k = k3)

table(groups.q)

coph.q <- cophenetic(clust.q)

cor(dist.q, coph.q)

cosine_similarity(as.matrix(dist.q), as.matrix(coph.q))

k <- 4

summ.1 <- kmeans(data1, centers = k, nstart = 25, iter.max = 100)

names(summ.1)

summ.1$cluster

summ.1$centers

summ.1$withinss

summ.1$tot.withinss

summ.1$betweenss

summ.1$size

plot(1:p, summ.1$centers[1, ], type = "b", col = cols[1],

xaxt = "n", ylim = range(summ.1$centers),

xlab = "Признак", ylab = "Среднее значение",

main = "Центры кластеров (k-средних)")

axis(1, at = 1:p, labels = colnames(data1))

for (i in 2:k) lines(1:p, summ.1$centers[i, ], type = "b", col = cols[i])

legend("topright", legend = paste("Кластер", 1:k),

col = cols, lty = 1, pch = 1)

data$cluster <- summ.1$cluster

View(data)

for (i in 1:k) {

cat("\n=== Кластер", i, "===\n")

print(data$N[data$cluster == i])

}

cl <- factor(summ.1$cluster)

anova.res <- t(sapply(1:p, function(j) {

s <- summary(aov(data1[, j] ~ cl))[[1]]

c(F = s[1, "F value"], p.value = s[1, "Pr(>F)"])

}))

rownames(anova.res) <- colnames(data1)

anova.res[order(anova.res[, "F"], decreasing = TRUE), ]

F.val <- anova.res[, "F"]

top3 <- order(F.val, decreasing = TRUE)[1:3]

nm <- colnames(data1)[top3]

nm

fig <- plot_ly(

x = data1[, top3[1]], y = data1[, top3[2]], z = data1[, top3[3]],

color = cl, type = "scatter3d", mode = "markers",

marker = list(size = 5)

)

fig <- plotly::layout(fig,

scene = list(

xaxis = list(title = nm[1]),

yaxis = list(title = nm[2]),

zaxis = list(title = nm[3])

),

title = "3D-диаграмма рассеяния (k-средних)"

)

fig

n.max <- 10

wss <- numeric(n.max); bss <- numeric(n.max)

wss[1] <- sum(scale(data1, center = TRUE, scale = FALSE)^2)

for (i in 2:n.max) {

f <- kmeans(data1, centers = i, nstart = 25)

wss[i] <- f$tot.withinss

bss[i] <- f$betweenss

}

par(mfrow = c(1, 2))

plot(1:n.max, wss, type = "b",

xlab = "Число кластеров", ylab = "Сумма квадратов внутри",

main = "WSS")

plot(1:n.max, bss, type = "b",

xlab = "Число кластеров", ylab = "Сумма квадратов между",

main = "BSS")

sil.km <- silhouette(summ.1$cluster, dist(data1))

summary(sil.km)$avg.width

fviz_silhouette(sil.km, palette = cols) +

ggplot2::ggtitle("Силуэтный анализ (k-средних)") +

ggplot2::theme_minimal()

fviz_cluster(summ.1, data = data1,

palette = cols,

ellipse.type = "norm",

ellipse = TRUE,

ggtheme = ggplot2::theme_minimal()) +

ggplot2::ggtitle("Кластеры (PCA, k-средних)")

data$cluster <- summ.1$cluster

data$cluster <- as.factor(data$cluster)

table(data$cluster)

library(caTools)

set.seed(123)

split <- sample.split(data$cluster, SplitRatio = 0.8)

Train <- subset(data, split == TRUE)

Test  <- subset(data, split == FALSE)

nrow(Train)

nrow(Test)

table(Train$cluster)

table(Test$cluster)

library(e1071)

form <- cluster ~ Age + Staz + Hours + Income

naive <- naiveBayes(form, data = Train)

naive

y.train <- predict(naive, newdata = Train)

y.test  <- predict(naive, newdata = Test)

Train$cluster <- factor(Train$cluster, levels = c("1","2","3","4"))

Test$cluster  <- factor(Test$cluster,  levels = c("1","2","3","4"))

library(caret)

confusionMatrix(y.train, Train$cluster, mode = "everything")

confusionMatrix(y.test,  Test$cluster,  mode = "everything")

save.image("lab2.RData")
