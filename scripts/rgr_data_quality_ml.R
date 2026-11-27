pkgs <- c("ggplot2", "corrplot", "VIM", "isotree", "caret",
          "randomForest", "smotefamily", "pROC", "class")
new_pkgs <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(new_pkgs) > 0) install.packages(new_pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

set.seed(42)
dir.create("plots", showWarnings = FALSE)


df <- read.csv(file.path("data", "data.csv"))
df <- df[, colSums(is.na(df)) < nrow(df)]
if ("id" %in% names(df)) df$id <- NULL
names(df) <- gsub("\\.", "_", names(df))
df$diagnosis <- factor(df$diagnosis, levels = c("B", "M"))

feat <- function(d) setdiff(names(d), "diagnosis")
X <- df[, feat(df)]
y <- df$diagnosis

str(df)
summary(df[, 1:7])
table(y); round(prop.table(table(y)), 3)
cat("Пропусков:", sum(is.na(df)), " Дубликатов:", sum(duplicated(df)), "\n")


p <- ggplot(df, aes(diagnosis, fill = diagnosis)) + geom_bar(width = 0.6) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.4) +
  scale_x_discrete(labels = c("B (доброкач.)", "M (злокач.)")) +
  labs(x = "Диагноз", y = "Количество наблюдений") + theme_minimal() +
  theme(legend.position = "none")
print(p)
ggsave("plots/01_classes.png", p, width = 5, height = 4, dpi = 150)


cm_X <- cor(X)
corrplot(cm_X, method = "color", type = "upper", tl.cex = 0.55, tl.col = "black")
dev.copy(png, "plots/02_corr.png", width = 1400, height = 1400, res = 150); dev.off()
high <- which(abs(cm_X) > 0.9 & upper.tri(cm_X), arr.ind = TRUE)
cat("Пар признаков с |r| > 0.9:", nrow(high), "\n")



metrics <- function(truth, prob) {
  pred <- factor(ifelse(prob > 0.5, "M", "B"), levels = c("B", "M"))
  TP <- sum(pred == "M" & truth == "M"); TN <- sum(pred == "B" & truth == "B")
  FP <- sum(pred == "M" & truth == "B"); FN <- sum(pred == "B" & truth == "M")
  sens <- TP / (TP + FN); spec <- TN / (TN + FP); prec <- TP / (TP + FP)
  c(Accuracy    = (TP + TN) / length(truth),
    Sensitivity = sens,
    Specificity = spec,
    F1          = 2 * prec * sens / (prec + sens),
    AUC         = as.numeric(pROC::auc(truth, prob, levels = c("B", "M"),
                                       direction = "<", quiet = TRUE)))
}


fit_predict <- function(train, test, model) {
  if (model == "RF") {
    m <- randomForest(diagnosis ~ ., data = train, ntree = 300)
    prob <- predict(m, test, type = "prob")[, "M"]
  } else {
    xs <- as.matrix(train[, feat(train)]); xt <- as.matrix(test[, feat(test)])


    if (!all(grepl("^PC[0-9]+$", colnames(xs)))) {
      xs <- scale(xs)
      xt <- scale(xt, center = attr(xs, "scaled:center"),
                  scale = attr(xs, "scaled:scale"))
    }
    p <- class::knn(xs, xt, train$diagnosis, k = 5, prob = TRUE)
    prob <- ifelse(p == "M", attr(p, "prob"), 1 - attr(p, "prob"))
  }
  metrics(test$diagnosis, prob)
}




folds <- createMultiFolds(y, k = 5, times = 5)

run_cv <- function(data, prep = NULL, label = "Базовый") {
  res <- NULL
  for (f in names(folds)) {
    tr <- data[folds[[f]], ]; te <- data[-folds[[f]], ]
    if (!is.null(prep)) { pp <- prep(tr, te); tr <- pp$train; te <- pp$test }
    for (m in c("RF", "kNN")) {
      res <- rbind(res, data.frame(Method = label, Model = m,
                                   t(fit_predict(tr, te, m))))
    }
  }
  agg <- aggregate(cbind(Accuracy, Sensitivity, Specificity, F1, AUC) ~
                     Method + Model, data = res, FUN = mean)
  print(agg, digits = 4)
  agg
}

results <- list()
results$base <- run_cv(df, label = "Базовый (исходные данные)")




set.seed(42)
mask <- matrix(runif(nrow(X) * ncol(X)) < 0.15, nrow(X))
X_na <- X; X_na[mask] <- NA
cat("Доля пропусков:", round(mean(is.na(X_na)), 3),
    " Полных строк:", sum(complete.cases(X_na)), "из", nrow(X_na), "\n")

aggr(X_na, numbers = FALSE, sortVars = FALSE, cex.axis = 0.5,
     ylab = c("Доля пропусков", "Структура пропусков"))
dev.copy(png, "plots/03_missing.png", width = 1600, height = 900, res = 150); dev.off()


X_med <- as.data.frame(lapply(X_na, function(v) {
  v[is.na(v)] <- median(v, na.rm = TRUE); v }))


X_knn <- VIM::kNN(X_na, k = 5, imp_var = FALSE)


nrmse <- function(X_imp) {
  mean(sapply(seq_along(X), function(j) {
    m <- mask[, j]
    sqrt(mean((X_imp[m, j] - X[m, j])^2)) / sd(X[, j])
  }))
}
imp_err <- data.frame(Method = c("Медиана", "kNN"),
                      NRMSE  = c(nrmse(X_med), nrmse(X_knn)))
print(imp_err)


idx <- mask[, which(names(X) == "area_worst")]
imp_df <- rbind(
  data.frame(true = X$area_worst[idx], imputed = X_med$area_worst[idx], method = "Медиана"),
  data.frame(true = X$area_worst[idx], imputed = X_knn$area_worst[idx], method = "kNN"))
p <- ggplot(imp_df, aes(true, imputed, colour = method)) +
  geom_point(alpha = 0.7) + geom_abline(linetype = 2) +
  labs(x = "Истинное значение area_worst", y = "Восстановленное значение",
       colour = "Метод") + theme_minimal()
print(p)
ggsave("plots/04_impute.png", p, width = 6, height = 4.5, dpi = 150)

results$med <- run_cv(data.frame(diagnosis = y, X_med),
                      label = "Пропуски 15 % + медиана")
results$knn <- run_cv(data.frame(diagnosis = y, X_knn),
                      label = "Пропуски 15 % + kNN-импутация")



iso_all <- isolation.forest(X, ntrees = 200, seed = 1, nthreads = 1)
score <- predict(iso_all, X)
anom <- score > quantile(score, 0.95)
cat("Аномалий:", sum(anom), "\n"); print(table(Аномалия = anom, Диагноз = y))

pc <- prcomp(X, center = TRUE, scale. = TRUE)
p <- ggplot(data.frame(pc$x[, 1:2], diagnosis = y,
                       status = ifelse(anom, "аномалия", "норма")),
            aes(PC1, PC2, colour = status, shape = diagnosis)) +
  geom_point(alpha = 0.75, size = 2) +
  scale_colour_manual(values = c("аномалия" = "red", "норма" = "grey55")) +
  labs(colour = "Isolation Forest", shape = "Диагноз") + theme_minimal()
print(p)
ggsave("plots/05_iforest.png", p, width = 6.5, height = 4.5, dpi = 150)


prep_iforest <- function(tr, te, share = 0.05) {
  iso <- isolation.forest(tr[, feat(tr)], ntrees = 200, seed = 1, nthreads = 1)
  s <- predict(iso, tr[, feat(tr)])
  list(train = tr[s <= quantile(s, 1 - share), ], test = te)
}
results$iforest <- run_cv(df, prep_iforest, label = "Isolation Forest (удаление 5 %)")




rf_funcs <- rfFuncs
rf_funcs$selectSize <- function(x, metric, maximize)
  pickSizeTolerance(x, metric, tol = 1.5, maximize = maximize)
ctrl <- rfeControl(functions = rf_funcs, method = "cv", number = 5)
set.seed(42)
rfe_all <- rfe(X, y, sizes = c(5, 10, 15, 20), rfeControl = ctrl)
print(rfe_all)
print(predictors(rfe_all))
p <- ggplot(rfe_all) + theme_minimal() +
  labs(x = "Число признаков", y = "Accuracy (5-fold CV)")
print(p)
ggsave("plots/06_rfe.png", p, width = 6, height = 4, dpi = 150)

rfe_sizes <- c()
prep_rfe <- function(tr, te) {
  r <- rfe(tr[, feat(tr)], tr$diagnosis, sizes = c(5, 10, 15, 20),
           rfeControl = ctrl)
  v <- predictors(r)
  rfe_sizes <<- c(rfe_sizes, length(v))
  list(train = tr[, c(v, "diagnosis")], test = te[, c(v, "diagnosis")])
}
results$rfe <- run_cv(df, prep_rfe, label = "RFE (отбор признаков)")
cat("Число отобранных признаков в блоках: среднее", mean(rfe_sizes),
    " мин", min(rfe_sizes), " макс", max(rfe_sizes), "\n")



prep_smote <- function(tr, te) {
  n_min <- sum(tr$diagnosis == "M"); n_maj <- sum(tr$diagnosis == "B")
  sm <- smotefamily::SMOTE(tr[, feat(tr)], as.character(tr$diagnosis), K = 5, dup_size = 1)
  syn <- sm$syn_data
  syn <- syn[sample(nrow(syn), n_maj - n_min), ]
  names(syn)[names(syn) == "class"] <- "diagnosis"
  syn$diagnosis <- factor(syn$diagnosis, levels = c("B", "M"))
  list(train = rbind(tr, syn[, names(tr)]), test = te)
}

sm_demo <- prep_smote(df, df)$train
cat("После SMOTE:\n"); print(table(sm_demo$diagnosis))
pc_sm <- predict(pc, sm_demo[, feat(sm_demo)])
p <- ggplot(data.frame(pc_sm[, 1:2], diagnosis = sm_demo$diagnosis,
                       type = rep(c("исходные", "синтетические"),
                                  c(nrow(df), nrow(sm_demo) - nrow(df)))),
            aes(PC1, PC2, colour = interaction(diagnosis, type, sep = ", "))) +
  geom_point(alpha = 0.7, size = 1.8) + labs(colour = "Класс, тип") +
  theme_minimal()
print(p)
ggsave("plots/07_smote.png", p, width = 6.5, height = 4.5, dpi = 150)

results$smote <- run_cv(df, prep_smote, label = "SMOTE (балансировка)")


summary(pc)$importance[, 1:10]
k95 <- which(cumsum(pc$sdev^2) / sum(pc$sdev^2) >= 0.95)[1]
cat("Компонент для 95 % дисперсии:", k95, " Сжатие:", ncol(X), "->", k95, "\n")
var_df <- data.frame(PC = seq_along(pc$sdev),
                     cum = cumsum(pc$sdev^2) / sum(pc$sdev^2))
p <- ggplot(var_df[1:15, ], aes(PC, cum)) + geom_col(fill = "steelblue") +
  geom_hline(yintercept = 0.95, linetype = 2, colour = "red") +
  labs(x = "Число главных компонент", y = "Накопленная доля дисперсии") +
  theme_minimal()
print(p)
ggsave("plots/08_pca.png", p, width = 6, height = 4, dpi = 150)

pca_k <- c()
prep_pca <- function(tr, te) {
  pca <- prcomp(tr[, feat(tr)], center = TRUE, scale. = TRUE)
  k <- which(cumsum(pca$sdev^2) / sum(pca$sdev^2) >= 0.95)[1]
  pca_k <<- c(pca_k, k)
  ptr <- data.frame(pca$x[, 1:k, drop = FALSE], diagnosis = tr$diagnosis)
  pte <- data.frame(predict(pca, te[, feat(te)])[, 1:k, drop = FALSE],
                    diagnosis = te$diagnosis)
  list(train = ptr, test = pte)
}
results$pca <- run_cv(df, prep_pca, label = "PCA (95 % дисперсии)")
cat("Число компонент в блоках:", range(pca_k), "\n")




results$combo <- run_cv(data.frame(diagnosis = y, X_knn), prep_smote,
                        label = "Конвейер: kNN-импутация + SMOTE")


final <- do.call(rbind, results)
rownames(final) <- NULL
final[, 3:7] <- round(final[, 3:7], 4)
print(final)
write.csv(final, "results.csv", row.names = FALSE)

final$Method <- factor(final$Method, levels = unique(final$Method))
long <- reshape(final[, c("Method", "Model", "Sensitivity", "F1")],
                direction = "long", varying = c("Sensitivity", "F1"),
                v.names = "value", timevar = "metric",
                times = c("Sensitivity", "F1"))
p <- ggplot(long, aes(Method, value, fill = Model)) +
  geom_col(position = "dodge") + facet_wrap(~metric, ncol = 1) +
  coord_cartesian(ylim = c(0.85, 1)) +
  labs(x = NULL, y = NULL, fill = "Модель") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
print(p)
ggsave("plots/09_summary.png", p, width = 8, height = 6, dpi = 150)
