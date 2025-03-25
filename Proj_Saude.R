#-------- Projeto para a Área de Saúde -----------#

## Este é um projeto de análise estatística utilizando dados relativos à saúde
## de pacientes.

# 1. Carregando o dataset
df_saude <- read.csv("D:/CURSOS/DSA/FCD/Projeto_Saude/dataset_saude.csv", stringsAsFactors = TRUE)
head(df_saude)

# 2. Análise Exploratória
str(df_saude)
summary(df_saude)

## 2.1 Checagem de valores ausentes
colSums(is.na(df_saude))


# 3. Medidas de Tendência Central
# 3.1 Média e Mediana
mean(df_saude$Idade)
median(df_saude$Idade)

#- Nesse ponto, a distribuição da Idade parece aproximadamente simétrica porque a média (49.005) e
#- a mediana (49.5) estão muito próximas.

# 3.1.1 Média Aparada - Reduz o impacto de outliers
mean(df_saude$Glicemia, trim = 0.1)

# 3.1.2 Média Ponderada - Quando algum valor é mais importante que outro
weighted.mean(df_saude$Glicemia, w = df_saude$Peso)

# 3.1.3 Moda
moda_diag <- names(which.max(table(df_saude$Diagnostico)))
moda_diag

barplot(table(df_saude$Diagnostico), col = c("red", "blue", "green"), main = "Distribuição dos Diagnósticos")

# 3.4 Máximo e Mínimo
max(df_saude$Altura)
min(df_saude$Altura)


# 4. Medidas Separatrizes
# 4.1 Quartis
quantile(df_saude$Pressao_Sistolica, probs = c(0.25, 0.5, 0.75))

# 4.2 Percentis
quantile(df_saude$Pressao_Diastolica, probs = c(0.10, 0.25, 0.50, 0.75, 0.90))


# 5. Medidas de Dispersão
# 5.1 Amplitude
max(df_saude$Glicemia) - min(df_saude$Glicemia)

# 5.2 Desvio Padrão
pesos_abaixo_median <- df_saude$Peso[df_saude$Peso <= median(df_saude$Peso)]
pesos_acima_median <- df_saude$Peso[df_saude$Peso > median(df_saude$Peso)]

sd(pesos_abaixo_median)
sd(pesos_acima_median)

#- Como o desvio padrão da metade superior é maior (10.03386) do que da metade inferior (8.570406), confirmamos que
#- os valores acima da mediana estão mais dispersos.

# 5.3 Variância
var(df_saude$Pressao_Sistolica)

# 5.3.1 Coeficiente de Variação
cv <- sd(df_saude$Pressao_Sistolica) / mean(df_saude$Pressao_Sistolica) * 100
cv

## 6. Distribuição de variáveis
## 6.1 Histograma da Idade
hist(df_saude$Idade, main = "Distribuição de Idade", xlab = "Idade", col = "lightblue")

## 6.2 Boxplot de Peso
boxplot(df_saude$Peso, main = "Boxplot do Peso", col = "lightblue")

#- Analisando o boxplot criado, vemos que:
#- Existem outliers (pequenos círculos isolados);
#- Os bigodes são semelhantes, indicando distribuição simétrica;
#- A mediana está deslocada para cima indicando que os valores entre esta e Q3 estão mais dispersos.


# 6.3 Coeficiente de Assimetria - Skewness
install.packages('e1071')
library(e1071)
skewness(df_saude$Idade)

#- Confirmamos que a distribuição da idade é simétrica, pois o skewness (0.01998) está próximo de 0 (zero).

# 6.4 Coeficiente de Kurtosis
library(moments)
kurtosis(df_saude$Idade)

#- o valor de curtose (1.78), ou seja, menor que 3, indica que a distribuição de Idade é platicúrtica (menos outliers)

# 7. Medidas de Associação
# 7.1 Covariância
cov(df_saude$Idade, df_saude$Pressao_Sistolica)

# 7.2 Coeficiente de Correlação
cor(df_saude$Idade, df_saude$Pressao_Sistolica)

#- A covariância (24.49) indica leve relação positiva, mas a correlação (0.050) muito baixa sugere associação muito fraca entre
#- as variáveis.

# 8. Testes Estatísticos

# 8.1 Teste t de Student
#- Queremos responder à questão: "A média da Pressão Sistólica é diferente entre pessoas saudáveis e doentes?"
#- Hipóteses:
  #- H0: Não há diferença significativa
  #- H1: Há diferença significativa

# 8.1.1 Verificando a normalidade com Shapiro-Wilk
ps_saudavel <- df_saude$Pressao_Sistolica[df_saude$Diagnostico == "Saudável"]
ps_doente <- df_saude$Pressao_Sistolica[df_saude$Diagnostico == "Doente"]

shapiro.test(ps_saudavel)
shapiro.test(ps_doente)

#- Como o valor de p é <= 0.05 para o grupo "Doente", então REJEITAMOS a normalidade e aplicaremos um teste
#- não paramétrico

# 8.1.1.2 Gráfico Q-Q Plot
par(mfrow = c(1, 2))

qqnorm(ps_saudavel, main = 'Q-Q Plot - Saudável', col = 'blue')
qqline(ps_saudavel, col = 'blue', lwd = 2)

qqnorm(ps_doente, main = 'Q-Q Plot - Doente', col = 'red')
qqline(ps_doente, col = 'red', lwd = 2)

# 8.1.2 Teste de Wilcoxon (não assume normalidade)
wilcox.test(ps_saudavel, ps_doente)

#- Como o valor de p é <= 0.05 REJEITAMOS H0 e ACEITAMOS H1

  # 8.1.2.1 Cálculo da Mediana para entender a diferença
  median(ps_saudavel)
  median(ps_doente)
  
  #- A pressão sistólica de indivíduos doentes é significativamente maior que dos saudáveis.
  
# 8.2 Teste Qui-quadrado
#- Agrupando Atividade Física em categorias
  df_saude$Atividade_labels <- cut(df_saude$Atividade_Fisica, breaks = c(-1, 2, 4, 6),
                                   labels = c("Baixa", "Moderada", "Alta"))
  
  tbl_freq <- table(df_saude$Atividade_labels, df_saude$Diagnostico)
  chisq.test(tbl_freq)
  
#- p-value (0.04118) < que 0.05, indicando associação significativa entre as variáveis.
  
  # 8.2.1 Cálculo das proporções de cada categoria
  prop.table(tbl_freq, margin = 2)
  
  # 8.2.2 Gráfico
  library(ggplot2)
  
  ggplot(as.data.frame(tbl_freq), aes(x = Var2, y = Freq, fill = Var1)) +
    geom_bar(stat = "identity", position = 'fill') +
    labs(x = 'Diagnóstico', y = 'Proporção', fill = 'Atividade Física') +
    theme_minimal()
  
# 8.3 Correlação de Pearson
cor.test(df_saude$Idade, df_saude$Colesterol, method = 'pearson')

# 8.4 Correlação de Spearman
cor.test(df_saude$Idade, df_saude$Colesterol, method = 'spearman')

  #- Tanto Pearson quanto Spearman indicam que não há relação significativa entre as variáveis.

# 8.5 Teste ANOVA
  # H0: Médias iguais entre os grupos
  # H1: Média diferente em 1 grupo pelo menos

  # 8.5.1 Teste de normalidade
  shapiro.test(df_saude$Pressao_Sistolica[df_saude$Atividade_labels == 'Baixa'])
  shapiro.test(df_saude$Pressao_Sistolica[df_saude$Atividade_labels == 'Moderada'])
  shapiro.test(df_saude$Pressao_Sistolica[df_saude$Atividade_labels == 'Alta'])
  
  #- p-value < 0.05 em todos os grupos indica que Pressão Sistólica não segue uma distribuição normal.
  
  # 8.5.2 Teste de homogeneidade da variância
  library(car)
  leveneTest(Pressao_Sistolica ~ Atividade_labels, data = df_saude)
  
  #- p-value = 0.22 => Não rejeitamos H0
  
  # ANOVA
  anova_result <- aov(Pressao_Sistolica ~ Atividade_labels, data = df_saude)
  summary(anova_result)
  
  #- p-value = 0.0402 indica diferença significativa na média de Pressão Sistólica entre os grupos.

  # 8.5.3 Teste Post-Hoc
  TukeyHSD(anova_result)
  
  #- Moderada-Baixa = 0.0535 indica diferença quase significativa.

  # 8.5.4 Teste não paramétrico
  kruskal.test(Pressao_Sistolica ~ Atividade_labels, data = df_saude)
  
  #- p-value = 0.04014 confirma diferença significativa entre os grupos.
  
  # 8.5.5 Teste de comparações múltiplas - Dunn's Test
  library(FSA)
  dunnTest(Pressao_Sistolica ~ Atividade_labels, data = df_saude, method = 'bonferroni')
  
  #- Baixa-Moderada = 0.0583 indica diferença marginalmente significativa entre esses grupos.
  #- Conclusão: indivíduos com baixa atividade física tendem a ter Pressão Sistólica maior que aqueles com
  #- atividade moderada.
  
# 9. Machine Learning
  
  # 9.1 Modelo Árvore de Decisão
  library(rpart)
  library(rpart.plot)
  
  model_arvore <- rpart(Diagnostico ~ ., data = df_saude, method = 'class')

  summary(model_arvore)  

  rpart.plot(model_arvore)
  
  # 9.1.2 Treino e Teste
  library(caret)
  set.seed(123)
  
  trainIndex <- createDataPartition(df_saude$Diagnostico, p = 0.8, list = FALSE)
  df_treino <- df_saude[trainIndex, ]
  df_teste <- df_saude[-trainIndex, ]
  
  # 9.1.3 Treino do modelo
  modelo <- rpart(Diagnostico ~ ., data = df_treino, method = 'class')
  
  # 9.1.4 Previsões
  previsao <- predict(modelo, df_teste, type = 'class')
  
  # 9.1.5 Matriz de Confusão
  matrixConf <- table(Predicted = previsao, Actual = df_teste$Diagnostico)
  
  # 9.1.6 Acurácia
  accuracy <- sum(diag(matrixConf)) / sum(matrixConf)
  
  matrixConf
  accuracy
  
  #- Vamos analisar outras métricas já que a acurácia foi de 100%
  
  # 9.1.7 Avaliação do Modelo
  
    # 9.1.7.1 Matriz de Confusão
    conf_matrix <- confusionMatrix(previsao, df_teste$Diagnostico)
    conf_matrix
    
    # 9.1.7.2 Validação Cruzada
    train_controle <- trainControl(method = 'cv', number = 10)
    
      # 9.1.7.2.1 Treino com validação cruzada
      set.seed(123)
      model_cv <- train(Diagnostico ~ ., data = df_saude, method = 'rpart', trControl = train_controle)

      print(model_cv)    
      model_cv$results
  
      #- Como o modelo parece estar com "Overfitting" vamos aprofundar a análise dos dados.
      
  # 9.1.8 Análise dos Dados
    
    # 9.1.8.1 Variáveis numéricas
      summary(df_saude[, sapply(df_saude, is.numeric)])
      
    # 9.1.8.2 Desequilíbrio das Classes
      table(df_saude$Diagnostico)
      prop.table(table(df_saude$Diagnostico)) * 100
      
    # 9.1.8.3 Correlação
      cor_matrix <- cor(df_saude[, sapply(df_saude, is.numeric)])
      cor_matrix
      
    # 9.1.8.4 Outliers
      numeric_vars <- df_saude[, sapply(df_saude, is.numeric)]
      outliers <- sapply(numeric_vars, function(x) {
        Q1 <- quantile(x, 0.25)
        Q3 <- quantile(x, 0.75)
        IQR <- Q3 - Q1
        lower_bound <- Q1 - 1.5 * IQR
        upper_bound <- Q3 + 1.5 * IQR
        sum(x < lower_bound | x > upper_bound)
      })
      outliers
      
    # 9.1.9 Matrizes de Confusão para Treino e Teste
      prev_treino <- predict(modelo, df_treino, type = 'class')
      confusionMatrix(prev_treino, df_treino$Diagnostico)
      
      prev_teste <- predict(modelo, df_teste, type = 'class')
      confusionMatrix(prev_teste, df_teste$Diagnostico)

      #- Com o Overfitting comprovado, vamos utilizar um modelo mais robusto.
      
  # 9.2 Modelo Random Forest
      library(randomForest)
      modelo_rf <- randomForest(Diagnostico ~ ., data = df_treino, ntree = 100, mtry = 3, importance = TRUE)
      
      prev_rf_treino <- predict(modelo_rf, df_treino)
      prev_rf_teste <- predict(modelo_rf, df_teste)

      confusionMatrix(prev_rf_treino, df_treino$Diagnostico)
      confusionMatrix(prev_rf_teste, df_teste$Diagnostico)      

      importance(modelo_rf)
      varImpPlot(modelo_rf)
      
    # 9.2.1 Verifica Overfitting
      print(modelo_rf)
    
      #- O erro OOB de 3,11% indica ótimo desempenho do modelo.
      
    # 9.2.2 Cross-validation
      controle <- trainControl(method = 'cv', number = 10)
      
      modelo_rf_cv <- train(Diagnostico ~ ., data = df_treino, method = 'rf', trControl = controle, ntree = 100)      
      print(modelo_rf_cv)
      
      #- O modelo melhora com mais variáveis no split.
      