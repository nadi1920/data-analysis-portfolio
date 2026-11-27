load("lab2.RData")

data2 <- data

if ("cluster" %in% names(data2)) {

data2$groups <- data2$cluster

data2$cluster <- NULL

}

data2$groups <- factor(data2$groups)

str(data2)

table(data2$groups)

saveRDS(data2, file = "data_LR2.rds")

library(caTools)

set.seed(123)

split <- sample.split(data2$groups, SplitRatio = 0.8)

Train <- subset(data2, split == TRUE)

Test  <- subset(data2, split == FALSE)

table(Train$groups)

table(Test$groups)

v <- c("Age", "Staz", "Hours", "Income")

table(Класс = data2$groups, Профессия = data2$Prof)

table(Класс = data2$groups, Пол = data2$Gender)

aggregate(data2[, v], by = list(Класс = data2$groups), FUN = mean)

round(cor(Train[, v]), 3)

library(rpart)

library(rpart.plot)

library(rattle)

CART1 <- rpart(groups ~ Age + Staz + Hours + Income,

data = Train, method = "class")

fancyRpartPlot(CART1, main = "Дерево классификации (Модель 1)")

pred_cart1_train <- predict(CART1, Train, type = "class")

pred_cart1_test  <- predict(CART1, Test, type = "class")

table(Train$groups, pred_cart1_train)

table(Test$groups, pred_cart1_test)

plotcp(CART1)

print(CART1$cptable)

CART2 <- rpart(groups ~ Age + Staz + Hours + Income,

data = Train, method = "class",

control = rpart.control(cp = 0.01, minsplit = 4,

maxdepth = 4))

fancyRpartPlot(CART2, main = "Дерево классификации (Модель 2)")

pred_cart2_train <- predict(CART2, Train, type = "class")

pred_cart2_test  <- predict(CART2, Test, type = "class")

table(Train$groups, pred_cart2_train)

table(Test$groups, pred_cart2_test)

library(MASS)

library(heplots)

round(sapply(v, function(x)

tapply(Train[[x]], Train$groups, function(z) shapiro.test(z)$p.value)), 4)

boxM(Train[, v], Train$groups)

LDA1 <- lda(groups ~ Age + Staz + Hours + Income, data = Train)

pred_lda1_train <- predict(LDA1, Train)$class

pred_lda1_test  <- predict(LDA1, Test)$class

table(Train$groups, pred_lda1_train)

table(Test$groups, pred_lda1_test)

LDA2 <- lda(groups ~ Age + Staz + Income, data = Train)

pred_lda2_train <- predict(LDA2, Train)$class

pred_lda2_test  <- predict(LDA2, Test)$class

table(Train$groups, pred_lda2_train)

table(Test$groups, pred_lda2_test)

library(nnet)

vif_values <- round(sapply(v, function(x) {

r2 <- summary(lm(reformulate(setdiff(v, x), x), data = Train))$r.squared

1 / (1 - r2)

}), 2)

print(vif_values)

LR1 <- multinom(groups ~ Age + Staz + Hours + Income,

data = Train, trace = FALSE)

pred_lr1_train <- predict(LR1, Train)

pred_lr1_test  <- predict(LR1, Test)

table(Train$groups, pred_lr1_train)

table(Test$groups, pred_lr1_test)

LR2 <- multinom(groups ~ Age + Staz + Income,

data = Train, trace = FALSE)

pred_lr2_train <- predict(LR2, Train)

pred_lr2_test  <- predict(LR2, Test)

table(Train$groups, pred_lr2_train)

table(Test$groups, pred_lr2_test)

library(yardstick)

Train$groups <- as.factor(Train$groups)

Test$groups  <- as.factor(Test$groups)

acc <- function(pred, actual) {

round(accuracy_vec(actual, pred), 3)

}

cat("Дерево 1: Train =", acc(pred_cart1_train, Train$groups),

"| Test =", acc(pred_cart1_test, Test$groups), "\n")

cat("Дерево 2: Train =", acc(pred_cart2_train, Train$groups),

"| Test =", acc(pred_cart2_test, Test$groups), "\n")

cat("LDA 1: Train =", acc(pred_lda1_train, Train$groups),

"| Test =", acc(pred_lda1_test, Test$groups), "\n")

cat("LDA 2: Train =", acc(pred_lda2_train, Train$groups),

"| Test =", acc(pred_lda2_test, Test$groups), "\n")

cat("LR 1: Train =", acc(pred_lr1_train, Train$groups),

"| Test =", acc(pred_lr1_test, Test$groups), "\n")

cat("LR 2: Train =", acc(pred_lr2_train, Train$groups),

"| Test =", acc(pred_lr2_test, Test$groups), "\n")

prob_cart1 <- predict(CART1, Test, type = "prob")

prob_cart2 <- predict(CART2, Test, type = "prob")

prob_lda1  <- predict(LDA1, Test)$posterior

prob_lda2  <- predict(LDA2, Test)$posterior

prob_lr1   <- predict(LR1, Test, type = "probs")

prob_lr2   <- predict(LR2, Test, type = "probs")

cat("Дерево 1:", round(roc_auc_vec(Test$groups, prob_cart1,

estimator = "macro"), 3), "\n")

cat("Дерево 2:", round(roc_auc_vec(Test$groups, prob_cart2,

estimator = "macro"), 3), "\n")

cat("LDA 1:", round(roc_auc_vec(Test$groups, prob_lda1,

estimator = "macro"), 3), "\n")

cat("LDA 2:", round(roc_auc_vec(Test$groups, prob_lda2,

estimator = "macro"), 3), "\n")

cat("LR 1:", round(roc_auc_vec(Test$groups, prob_lr1,

estimator = "macro"), 3), "\n")

cat("LR 2:", round(roc_auc_vec(Test$groups, prob_lr2,

estimator = "macro"), 3), "\n")

prob_cart1_train <- predict(CART1, Train, type = "prob")

prob_cart2_train <- predict(CART2, Train, type = "prob")

prob_lda1_train  <- predict(LDA1, Train)$posterior

prob_lda2_train  <- predict(LDA2, Train)$posterior

prob_lr1_train   <- predict(LR1, Train, type = "probs")

prob_lr2_train   <- predict(LR2, Train, type = "probs")

prob_cart1_test  <- prob_cart1

prob_cart2_test  <- prob_cart2

head(round(prob_lda1_train, 3))

library(caret)

confusionMatrix(pred_cart1_train, Train$groups, mode = "everything")

confusionMatrix(pred_cart1_test,  Test$groups,  mode = "everything")

confusionMatrix(pred_cart2_train, Train$groups, mode = "everything")

confusionMatrix(pred_cart2_test,  Test$groups,  mode = "everything")

confusionMatrix(pred_lda1_train,  Train$groups, mode = "everything")

confusionMatrix(pred_lda1_test,   Test$groups,  mode = "everything")

confusionMatrix(pred_lda2_train,  Train$groups, mode = "everything")

confusionMatrix(pred_lda2_test,   Test$groups,  mode = "everything")

confusionMatrix(pred_lr1_train,   Train$groups, mode = "everything")

confusionMatrix(pred_lr1_test,    Test$groups,  mode = "everything")

confusionMatrix(pred_lr2_train,   Train$groups, mode = "everything")

confusionMatrix(pred_lr2_test,    Test$groups,  mode = "everything")

library(ROCR)

roc_plot <- function(prob, truth, title) {

auc_values <- numeric(4)

plot(1, type = "n", xlim = c(0, 1), ylim = c(0, 1), main = title,

xlab = "1 - Специфичность (FPR)",

ylab = "Чувствительность (TPR)")

grid()

for (i in 1:4) {

true_class <- as.numeric(truth == levels(truth)[i])

pred <- prediction(prob[, i], true_class)

plot(performance(pred, "tpr", "fpr"),

col = rainbow(4)[i], lwd = 2, add = TRUE)

auc_values[i] <- performance(pred, "auc")@y.values[[1]]

text(0.5, 0.5 - i * 0.1, paste("AUC =", round(auc_values[i], 4)),

col = rainbow(4)[i])

}

text(0.5, 0.0, paste("Средний AUC =", round(mean(auc_values), 4)))

legend("bottomright", legend = levels(truth), col = rainbow(4),

lwd = 2, title = "Класс")

}

roc_plot(prob_lda1_train, Train$groups, "ROC: LDA 1, Train")

prob_best <- prob_lda1

auc_values <- numeric(4)

plot(1, type = "n", xlim = c(0, 1), ylim = c(0, 1),

xlab = "1 - Специфичность", ylab = "Чувствительность",

main = "ROC-кривые (LDA 1, Тест)")

for (i in 1:4) {

true_class <- as.numeric(Test$groups == levels(Test$groups)[i])

pred <- prediction(prob_best[, i], true_class)

perf <- performance(pred, "tpr", "fpr")

plot(perf, col = rainbow(4)[i], add = TRUE)

auc_value <- performance(pred, measure = "auc")@y.values[[1]]

auc_values[i] <- auc_value

text(0.5, 0.5 - i * 0.1, paste("AUC =", round(auc_value, 4)),

col = rainbow(4)[i])

}

text(0.5, 0.0, paste("Средний AUC =", round(mean(auc_values), 4)),

col = "black")

legend("bottomright", legend = levels(Test$groups),

col = rainbow(4), lwd = 2)

acc_test <- c(0.5, 0.7, 0.9, 0.9, 0.6, 0.5)

names(acc_test) <- c("Дерево 1", "Дерево 2", "LDA 1", "LDA 2",

"LR 1", "LR 2")

barplot(acc_test, main = "Сравнение Accuracy на тестовой выборке",

ylab = "Accuracy", col = "steelblue", ylim = c(0, 1))
