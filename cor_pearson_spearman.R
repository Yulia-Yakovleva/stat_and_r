# создаем тестовый датафрейм
test_df_pearson <- data.frame(Xagr = c(24, 27, 26, 21, 20, 31, 26, 22, 20, 18, 30, 29, 24, 26),
                      Yiq = c(100, 115, 117, 119, 134, 94, 105, 103, 111, 124, 122, 109, 110, 86))

### КОЭФФИЦИЕНТ КОРРЕЛЯЦИИ ПИРСОНА ###

# пишем функцию
cor_pearson <- function(x, y){
  devX <- x - mean(x)
  devY <- y - mean(y)
  sum_sqrt_devX = sum(devX^2)
  sum_sqrt_devY = sum(devY^2)
  composition <- sum((devX) * (devY))
  final_formula <- composition/sqrt(sum(devX^2)*sum(devY^2))
  final_formula
}

# запускаем функцию
cor_pearson(x = test_df_pearson$Xagr, y = test_df_pearson$Yiq)

# проверяем с помощью функции cor
output_1 <- cor_pearson(x = test_df_pearson$Xagr, y = test_df_pearson$Yiq)
cor(x = test_df_pearson$Xagr, y = test_df_pearson$Yiq, method = "pearson") == output_1

# TRUE Ура!

### КОЭФФИЦИЕНТ КОРРЕЛЯЦИИ СПИРМАНА ###

# создаем тестовый датафрейм
test_df_spearman <- data.frame(Xiq = c(100, 118, 112, 97, 99, 103, 102, 132, 122, 121, 115, 117, 109, 111),
                               Yx = c(154, 123, 120, 213, 200, 187, 155, 100, 114, 115, 107, 176, 143, 111))

# пишем функцию
cor_spearman <- function(x, y){
  sum_sqrt_q <- sum((rank(x) - rank(y))^2)
  N <- length(x)
  final_formula <- (1 - ((6 * sum_sqrt_q)/(N*(N^2-1))))
  final_formula
}

# запускаем функцию
cor_spearman(test_df_spearman$Xiq, test_df_spearman$Yx)

# проверяем с помощью функции cor
output_2 <- cor_spearman(test_df_spearman$Xiq, test_df_spearman$Yx)
round((cor(x = test_df_spearman$Xiq, y = test_df_spearman$Yx, method = "spearman")), 20) ==
  round(output_2, 20)

# где-то на очень маленьких долях значение, посчитанное cor отличается от значения, полученного нашей функцией
# но округление до 20-го знака дает TRUE, так что значения почти одинаковые
# Ура!
