getwd()

list.files()

data <- read.table(file.path("data", "var_7.csv"), header = TRUE, sep = ";", fileEncoding = "UTF-8")

str(data)

View(data)

View(data[1:5, ])

View(data[c(3)])

df <- replace(data, 3, 0)

View(df)

names(data) <- c("N", "Group", "Gender", "Age", "Purchases",

"AvgPrice", "AvgPages", "SupportReq",

"Surveys", "SatisfScore", "SatisfLevel")

data$Group       <- as.factor(data$Group)

data$Gender      <- as.factor(data$Gender)

data$Surveys     <- as.factor(data$Surveys)

data$SatisfLevel <- as.factor(data$SatisfLevel)

str(data)

group1 <- subset(data, Group == 1)

group2 <- subset(data, Group == 2)

View(group1)

View(group2)

nrow(group1)

nrow(group2)

get_mode <- function(v) {

uniq_vals <- unique(v)

uniq_vals[which.max(tabulate(match(v, uniq_vals)))]

}

get_skewness <- function(x) {

n <- length(x); mean_x <- mean(x); sd_x <- sd(x)

(n / ((n - 1) * (n - 2))) * sum(((x - mean_x) / sd_x)^3)

}

get_excess <- function(x) {

n <- length(x); mean_x <- mean(x); sd_x <- sd(x)

(n * (n + 1) / ((n - 1) * (n - 2) * (n - 3))) * sum(((x - mean_x) / sd_x)^4) -

(3 * (n - 1)^2) / ((n - 2) * (n - 3))

}

summary(data[, c(4:9)])

apply(data[, c(4:9)], 2, sd)

apply(data[, c(4:9)], 2, var)

print(lapply(data[, c(4:9)], get_mode))

print(lapply(data[, c(4:9)], get_skewness))

print(lapply(data[, c(4:9)], get_excess))

summary(group1[, c(4:9)])

apply(group1[, c(4:9)], 2, sd)

print(lapply(group1[, c(4:9)], get_mode))

print(lapply(group1[, c(4:9)], get_skewness))

print(lapply(group1[, c(4:9)], get_excess))

summary(group2[, c(4:9)])

apply(group2[, c(4:9)], 2, sd)

print(lapply(group2[, c(4:9)], get_mode))

print(lapply(group2[, c(4:9)], get_skewness))

print(lapply(group2[, c(4:9)], get_excess))

plot(data$AvgPrice, data$SatisfScore,

xlab = "Средняя стоимость покупок",

ylab = "Удовлетворённость (баллы)",

main = "Диаграмма рассеяния: AvgPrice vs SatisfScore",

col = "blue", pch = 19)

diagram1 <- data$SatisfLevel

diagram1_count <- table(diagram1)

piepercent <- round(100 * diagram1_count / sum(diagram1_count), 1)

pie(diagram1_count, radius = 1,

main = "Радиальная диаграмма по уровню удовлетворённости",

col = c("green", "pink", "blue"), clockwise = TRUE,

labels = paste(names(diagram1_count), "(", piepercent, "%)"))

legend("topright", c("High", "Low", "Medium"), cex = 0.8,

fill = c("green", "pink", "blue"))

par(mfrow = c(2, 2))

d2 <- table(data$SatisfLevel[data$Gender == 1])

pie(d2, main = "Мужчины", col = c("green","pink","blue"), clockwise = TRUE,

labels = paste(names(d2), "(", round(100*d2/sum(d2),1), "%)"))

d3 <- table(data$SatisfLevel[data$Gender == 2])

pie(d3, main = "Женщины", col = c("green","pink","blue"), clockwise = TRUE,

labels = paste(names(d3), "(", round(100*d3/sum(d3),1), "%)"))

d4 <- table(data$SatisfLevel[data$Group == 1])

pie(d4, main = "Группа 1", col = c("green","pink","blue"), clockwise = TRUE,

labels = paste(names(d4), "(", round(100*d4/sum(d4),1), "%)"))

d5 <- table(data$SatisfLevel[data$Group == 2])

pie(d5, main = "Группа 2", col = c("green","pink","blue"), clockwise = TRUE,

labels = paste(names(d5), "(", round(100*d5/sum(d5),1), "%)"))

par(mfrow = c(2, 2))

barplot(table(data$Surveys[data$Gender == 1]), col = "yellow",

main = "Опросы: мужчины", xlab = "Участие в опросах", ylab = "Чел.")

barplot(table(data$Surveys[data$Gender == 2]), col = "red",

main = "Опросы: женщины", xlab = "Участие в опросах", ylab = "Чел.")

barplot(table(data$Surveys[data$Group == 1]), col = "coral",

main = "Опросы: группа 1", xlab = "Участие в опросах", ylab = "Чел.")

barplot(table(data$Surveys[data$Group == 2]), col = "gold",

main = "Опросы: группа 2", xlab = "Участие в опросах", ylab = "Чел.")

par(mfrow = c(1, 2))

boxplot(Age ~ Gender, data = data,

main = "Возраст по полу", xlab = "Пол", ylab = "Возраст",

col = c("tan", "wheat"))

boxplot(Age ~ Group, data = data,

main = "Возраст по группе", xlab = "Группа", ylab = "Возраст",

col = c("lightblue", "salmon"))

par(mfrow = c(3, 2))

hist(data$Age,         freq = FALSE, breaks = 10, col = "powderblue",

xlab = "Возраст", main = "Age")

hist(data$Purchases,   freq = FALSE, breaks = 10, col = "palevioletred",

xlab = "Покупки", main = "Purchases")

hist(data$AvgPrice,    freq = FALSE, breaks = 10, col = "mediumvioletred",

xlab = "Средняя стоимость", main = "AvgPrice")

hist(data$AvgPages,    freq = FALSE, breaks = 10, col = "lawngreen",

xlab = "Страницы", main = "AvgPages")

hist(data$SupportReq,  freq = FALSE, breaks = 10, col = "palegreen",

xlab = "Обращения", main = "SupportReq")

hist(data$SatisfScore, freq = FALSE, breaks = 10, col = "royalblue",

xlab = "Удовлетворённость", main = "SatisfScore")

pairs(~ Age + Purchases + AvgPrice + AvgPages + SupportReq + SatisfScore,

data = data,

main = "Матричный график по всем количественным переменным")

chisq.test(table(data$Gender[data$Group == 1], data$SatisfLevel[data$Group == 1]))

fisher.test(table(data$Gender[data$Group == 1], data$SatisfLevel[data$Group == 1]))

chisq.test(table(data$Gender[data$Group == 2], data$SatisfLevel[data$Group == 2]))

fisher.test(table(data$Gender[data$Group == 2], data$SatisfLevel[data$Group == 2]))

aov_model <- aov(Age ~ SatisfLevel, data = data);         summary(aov_model)

aov_model <- aov(AvgPrice ~ SatisfLevel, data = data);    summary(aov_model)

aov_model <- aov(Purchases ~ SatisfLevel, data = data);   summary(aov_model)

aov_model <- aov(AvgPages ~ SatisfLevel, data = data);    summary(aov_model)

aov_model <- aov(SupportReq ~ SatisfLevel, data = data);  summary(aov_model)

aov_model <- aov(SatisfScore ~ SatisfLevel, data = data); summary(aov_model)

kruskal.test(Age ~ SatisfLevel, data = data)

kruskal.test(AvgPrice ~ SatisfLevel, data = data)

kruskal.test(Purchases ~ SatisfLevel, data = data)

kruskal.test(AvgPages ~ SatisfLevel, data = data)

kruskal.test(SupportReq ~ SatisfLevel, data = data)

kruskal.test(SatisfScore ~ SatisfLevel, data = data)

data1 <- data[data$Group == 1, c(4:9)]

M <- data1[, unlist(lapply(data1, is.numeric))]

M1 <- cor(M, use = "pairwise.complete.obs", method = "pearson")

M2 <- cor(M, use = "pairwise.complete.obs", method = "spearman")

M3 <- cor(M, use = "pairwise.complete.obs", method = "kendall")

View(M1); View(M2); View(M3)

data2 <- data[data$Group == 2, c(4:9)]

N <- data2[, unlist(lapply(data2, is.numeric))]

N1 <- cor(N, use = "pairwise.complete.obs", method = "pearson")

N2 <- cor(N, use = "pairwise.complete.obs", method = "spearman")

N3 <- cor(N, use = "pairwise.complete.obs", method = "kendall")

View(N1); View(N2); View(N3)

install.packages("ppcor")

library(ppcor)

pcor.test(M[, 3], M[, 2], M[, -c(3, 2)])

pcor.test(N[, 2], N[, 1], N[, -c(2, 1)])

install.packages("pheatmap")

library(pheatmap)

pheatmap(M1, cutree_rows = 1, display_numbers = TRUE)

pheatmap(M2, cutree_rows = 1, display_numbers = TRUE)

pheatmap(M3, cutree_rows = 1, display_numbers = TRUE)

pheatmap(N1, cutree_rows = 1, display_numbers = TRUE)

pheatmap(N2, cutree_rows = 1, display_numbers = TRUE)

pheatmap(N3, cutree_rows = 1, display_numbers = TRUE)

install.packages("nortest")

library(nortest)

shapiro.test(group1$AvgPrice)

lillie.test(group1$AvgPrice)

pearson.test(group1$AvgPrice)

ad.test(group1$AvgPrice)

install.packages("rcompanion")

library(rcompanion)

plotNormalHistogram(group1$AvgPrice,

xlab = "Средняя стоимость",

ylab = "Количество наблюдений",

length = 1000,

breaks = seq(min(group1$AvgPrice),

max(group1$AvgPrice),

length.out = 7))

install.packages("GGally")

install.packages("ggplot2")

library(GGally)

library(ggplot2)

ggpairs(data, columns = c(2, 3, 4, 5, 6, 10),

aes(color = Group, alpha = 0.5))

save.image("lab1.RData")
