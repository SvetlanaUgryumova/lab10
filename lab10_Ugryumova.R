# Лабораторная работа №10:
# Классификация методом К-ближайших соседей.


install.packages("class")
install.packages("caret")
install.packages("lattice")
install.packages("e1071")
install.packages("mailR")

# Подключение библиотек
library(class)
library(caret)
library(lattice)
library(e1071)
library(mailR)

# Устанавливаем рабочую директорию и загружаем данные
setwd("~/Этот компьютер/Рабочий стол/all/Учеба/4 курс/Лаб10")
iris <- read.table("iris.csv", header = T, sep = ",")

# Изучаем структуру данных

str(iris)
head(iris)
tail(iris)
summary(iris) # сводная таблица с опистельными статистиками


# Проверяем на наличие пропущенных значений (NA)

na_check <- function(x){
  
  if (missing(x))
    print("iris")
  
  else
    if (sum(is.na(x)) > 0) 
      print(paste(c("Обнаружено"), round(sum(is.na(x)), digits = 2), c("пропущенных значений")
      )
      )
  else
    print("Пропущенных данных не обнаружено")
  
}

na_check(iris)

# Разделение данных на обучающий и тестовый наборы

set.seed(123)
train_index <- createDataPartition(iris$variety, p = 0.8, list = FALSE)
train_data <- iris[train_index, ]
test_data <- iris[-train_index, ]

# Обучение модели KNN

k <- 3
model_knn <- knn(train = train_data[, 1:4], test = test_data[, 1:4], cl = train_data$variety, k = k)

# Оценка точности модели

accuracy <- sum(model_knn == test_data$variety) / length(test_data$variety)
cat("Accuracy:", accuracy, "\n")
set.seed(4)

# Визуализация данных

ggplot(iris, aes(x = sepal.length, y = sepal.width, color = variety)) +
  geom_point() +
  labs(title = "Scatter Plot of Iris Data", x = "Sepal Length", y = "Sepal Width") +
  theme_minimal()
  
install.packages("rstudioapi")
library(mailR) # only sending function
send.mail(from = "suetlana.ug@yandex.ru",
          to = c("suetlana.ug@yandex.ru"),
          replyTo = c("Reply to someone else <suetlana.ug@yandex.ru>"),
          subject = "результаты классификации KNN",
          body = paste("Точность модели KNN:", accuracy),
          smtp = list(host.name = "smtp.yandex.ru", port = 465, user.name = "name@yandex.ru", passwd = rstudioapi::askForPassword(), ssl = TRUE),
          authenticate = TRUE,
          send = TRUE, 
          attach.files = c("./fraud.txt")) # file must be in working dir
