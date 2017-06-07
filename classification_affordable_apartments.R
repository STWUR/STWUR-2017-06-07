#' na które mieszkania stać Wrocławian?
#' 
#' Według GUS (http://wroclaw.stat.gov.pl/) przeciętne (średnie)
#' wyngrodzenie we Wrocławiu wyniosło 4532,14 zł brutto.
#' To jest 3 223,81 zł netto
pensja <- 3223.81
#odjmiemy 1500 złotych na życie (jedzenie, ubrania, rozrywkę)
max_rata <- pensja - 1500

#zalozmy kredyt na 5% rocznie 

# rata = kwota kredytu * (1 + procent/12)^ liczba_rat * ((1 + procent/12)-1)/((1 + procent/12)^liczba_rat-1)
liczba_rat <- 25*12
r <- 0.05
max_rata/(1 + r/12)^liczba_rat/((1 + r/12)-1)*((1 + r/12)^liczba_rat-1)

#wychodzi 300K kredytu

library(dplyr)
library(mlr)
mieszkania <- na.omit(read.csv(file = "./data/mieszkania_dane.csv", encoding = "UTF-8"))

mieszkania <- mieszkania %>%
  mutate(cena = metraz*cena_m2) %>%
  mutate(tanie = cena < 300000) %>%
  select(-cena)

#' Pytanie: Jaki procent mieszkań ma cenę poniżej 300K złotych?

predict_affordable <- makeClassifTask(id = "affordableApartments", 
                                      data = mieszkania, target = "tanie")

all_learners <- listLearners()
filter(all_learners, type == "classif")[["class"]]
learnerRF <- makeLearner("classif.randomForest")
learnerNN <- makeLearner("classif.nnet")
learnerGLM <- makeLearner("classif.glmnet")

bench_regr <- benchmark(learners = list(learnerRF,
                                        learnerNN,
                                        learnerGLM),
                        tasks = list(predict_affordable))


#jak policzyć AUC?
#potrzebujemy p-stw, a nie tylko etykietek, zmieniamy opcję predict.type
learnerGLM <- makeLearner("classif.glmnet", predict.type = "prob")
# zamiast crossval możemy użyć innych sposobów weryfikacji dobroci modeli,
# np. holdout czy repcv (więcej info w dokumentacji)
GLM_probs = crossval(learnerGLM, predict_affordable)
performance(GLM_probs$pred, measures = list(auc))

listMeasures(predict_affordable)

#' Pytanie: porównaj modele oparte o lasy losowe i sieci neuronowe
#' pod wzgledem auc i false positives (fp), czyli mieszkań, które
#' przewidujemy jako tansze, ale w istocie takie nie sa


#### Które zmienne są istotne i jaki mają wpływ na koszt mieszkania?

#uczymy uogólniony model liniowy z lasso z konkretną wartością lambda
learnerGLM_one_lambda <- makeLearner("classif.glmnet", lambda = 0.00001)
learnerGLM_one_lambda <- train(learnerGLM_one_lambda, predict_affordable)
#parametry dopasowanego modelu
getLearnerModel(learnerGLM_one_lambda)$beta


#jak poprawić model? Dodać nowe ważne zmienne
mieszkanie_new_features <- mieszkania %>%
  mutate(pietro = ifelse(pietro == 0, "parter",
                         ifelse(pietro == pietro_maks, "ostatnie_pietro",
                                ifelse(pietro > 15, "wysoko",
                                       ifelse(pietro_maks < 3, "niska_zabudowa", "inne")))),
         pietro = factor(pietro),
         rok = ifelse(rok < 1945, "przedwojenne", 
                      ifelse(rok < 1970, "powojenne",
                             ifelse(rok < 1989, "wielka_plyta", "po_1989"))),
         rok = factor(rok))

predict_affordable_new_features <- 
  makeClassifTask(id = "affordableApartments", data = mieszkanie_new_features, 
                  target = "tanie")

# Pytanie: czy mamy lepszą predykcję dla modelu learnerGLM dla nowych danych?


#### Istotność współczynników i interpretacja

#uczymy uogólniony model liniowy z lasso z konkretną wartością lambda
learnerGLM_one_lambda <- makeLearner("classif.glmnet", lambda = 0.0001)
#lambde da sie wybrac w systematyczny sposob za pomoca mlr
learnerGLM_one_lambda <- train(learnerGLM_one_lambda, predict_affordable_new_features)
#parametry dopasowanego modelu
knitr::kable(as.matrix(getLearnerModel(learnerGLM_one_lambda)$beta))

# Pytanie: jak należy interpretować te współczynniki?

# Podobnie możemy sprawdzić, które zmienne są istotne w modelu lasu losowego?
learnerRF_imp_trained <- train(learnerRF, predict_affordable_new_features)
getFeatureImportance(learnerRF_imp_trained)
