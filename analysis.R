library(mlr)
library(dplyr)

set.seed(1792)

mieszkania <- na.omit(read.csv(file = "./data/mieszkania_dane.csv", encoding = "UTF-8"))

# zadania - kazde zadanie to inny zbior danych
predict_price <- makeRegrTask(id = "cenaMieszkan", data = mieszkania, target = "cena_m2")

predict_price2 <- makeRegrTask(id = "cenaMieszkanBezDzielnicy", 
                               data = select(mieszkania, -dzielnica), 
                               target = "cena_m2")

predict_price3 <- makeRegrTask(id = "cenaMieszkanBezMaksPieter", 
                               data = select(mieszkania, -pietro_maks), 
                               target = "cena_m2")


all_learners <- listLearners()

learnerRF <- makeLearner("regr.randomForest")
learnerNN <- makeLearner("regr.nnet")
learnerSVM <- makeLearner("regr.ksvm")

filter(all_learners, class == "regr.svm")
bench_ho <- holdout(learnerRF, predict_price)

bench_cv <- crossval(learnerRF, predict_price)
getRRPredictionList(bench_cv)


bench_regr <- benchmark(learners = list(learnerRF,
                                        learnerNN,
                                        learnerSVM),
                        tasks = list(predict_price,
                                     predict_price2,
                                     predict_price3))
# patrz również batchmark - obliczenia równoległe
as.data.frame(bench_regr)
getBMRAggrPerformances(bench_regr)
getBMRPerformances(bench_regr)
plotBMRBoxplots(bench_regr)

save(bench_regr, bench_cv, bench_ho, file = "./data/regression_cache.RData")
