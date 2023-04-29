## Pacotes necessários
library(tidyr)
library(dplyr)
library(ggplot2)
library(esquisse)
library(ggthemes)
library(tibble)
library(data.table)
library(outliers)
library(BHH2)
library(latex2exp)
library(moments)
library(modeest)
## Disponibilizando a base de dados:
massa <- read.csv("C:\\Users\\vitor\\Development\\strk\\encoded_stroke_data.csv")

### Nomeando variáveis pra melhor acesso

## Qualitativas
gender = as.character(massa$gender) # Gênero do indivíduo
hypertension = as.character(massa$hypertension) # Hipertensão
heart_disease = as.character(massa$heart_disease) # Doença cardíaca
ever_married = as.character(massa$ever_married) # Estado civil
work_type = as.character(massa$work_type) # Tipo de profissão
smoking_status = as.character(massa$smoking_status) # Status de tabagismo
residence_type = as.character(massa$Residence_type) # Tipo de moradia
stroke = as.character(massa$stroke) # AVC

## Quantitativas 
age = as.numeric(massa$age) # Idade
avg_gluco_level = as.numeric(massa$avg_glucose_level) # Nível médio de glucose
bmi = as.numeric(massa$bmi) # IMC

#### Medidas descritivas
### Quantitativas
## Idade
# histograma
ggplot(massa, aes(x = age)) +
  geom_histogram(bins = 10L, fill = "#7f7caf", color = "white") +
  labs(
    x = "Idade",
    y = "Frequência",
    title = "Histograma da variável idade"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14L),
    plot.subtitle = element_text(size = 13L),
    plot.caption = element_text(size = 12L, hjust = 0)
  )
# Dada o coeficiente de simetria negativo(-0.11) podemos afirmar que a variável idade é negativamente inclinada, portanto
# sua moda(57) é maior que sua mediana(44)
# Dado o valor da curtose < 3, afirmamos que sua curva é lepticurtica 

## Nível médio de glicose
# histograma
ggplot(massa) +
  aes(x = avg_glucose_level) +
  geom_density(adjust = 1L, fill = "#7f7caf") +
  labs(
    x = 'Nível médio de glucose',
    y = 'Frequência',
    title = 'Gráfico de densidade da variável nível médio de glucose'
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14L),
    plot.subtitle = element_text(size = 13L),
    plot.caption = element_text(size = 12L, hjust = 0)
  )


## IMC
# gráfico de densidade
ggplot(massa) +
  aes(x = bmi) +
  geom_density(adjust = 1L, fill = "#7f7caf") +
  labs(
    x = "IMC",
    y = "Frequência",
    title = "Gráfica de densidade da variável IMC",
    subtitle = TeX(r"(IMC é dado por: $\frac{peso}{altura^2}$)")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14L),
    plot.subtitle = element_text(size = 13L),
    plot.caption = element_text(size = 12L, hjust = 0)
  )

### Qualitativas

## Gênero
# gráfico de barra
ggplot(massa_suja, aes(x = factor(gender))) + 
  geom_bar(aes(fill = factor(gender)), alpha = 0.8) +
  scale_fill_manual(values = c("#EEEEFF", "#7f7caf", "#9fb4c7", "#28587b", "#223843")) +
  labs(x = "Gênero", y = "Frequência", title = "Gráfico de barras da variável gênero") +
  theme_minimal() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)

# setores
ggplot(data.frame(t_gender), aes(x = "", y = Freq, fill = gender)) + 
  geom_bar(stat = "identity", color = "white", linewidth = 0.5) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = c("#7f7caf", "#28587b", "#2ca02c")) +
  labs(x = NULL, y = NULL, fill = "Gender") +
  theme_void() +
  theme(legend.position = "right")

## Tipo de trabalho
# gráfico de barra
ggplot(massa_suja, aes(x = factor(work_type))) + 
  geom_bar(aes(fill = factor(work_type)), alpha = 0.8) +
  scale_fill_manual(values = c("#EEEEFF", "#7f7caf", "#9fb4c7", "#28587b", "#223843")) +
  labs(x = "Tipo de trabalho", y = "Frequência", title = "Gráfico de barras da variável tipo de trabalho") +
  theme_minimal() + 
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)

## Hipertensão
# gráfico de barras
ggplot(massa_suja, aes(x = factor(hypertension))) + 
  geom_bar(aes(fill = factor(hypertension)), alpha = 0.8) +
  scale_fill_manual(values = c("#EEEEFF", "#7f7caf", "#9fb4c7", "#28587b", "#223843")) +
  labs(x = "Hipertensão", y = "Frequência", title = "Gráfico de barras da variável hipertensão") +
  theme_minimal() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)

## Doença cardíaca
# gráfico de barras
ggplot(massa_suja, aes(x = factor(heart_disease))) + 
  geom_bar(aes(fill = factor(heart_disease)), alpha = 0.8) +
  scale_fill_manual(values = c("#EEEEFF", "#7f7caf", "#9fb4c7", "#28587b", "#223843")) +
  labs(x = "Doença cardíaca", y = "Frequência", title = "Gráfico de barras da variável doença cardíaca") +
  theme_minimal() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)

## Estado civil
# gráfico de barras
ggplot(massa_suja, aes(x = factor(ever_married))) + 
  geom_bar(aes(fill = factor(ever_married)), alpha = 0.8) +
  scale_fill_manual(values = c("#EEEEFF", "#7f7caf", "#9fb4c7", "#28587b", "#223843")) +
  labs(x = "Estado civil", y = "Frequência", title = "Gráfico de barras da variável estado civil") +
  theme_minimal() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)

## Tipo de residência
# gráfico de barras
ggplot(massa_suja, aes(x = factor(Residence_type))) + 
  geom_bar(aes(fill = factor(Residence_type)), alpha = 0.8) +
  scale_fill_manual(values = c("#EEEEFF", "#7f7caf", "#9fb4c7", "#28587b", "#223843")) +
  labs(x = "Tipo de residência", y = "Frequência", title = "Gráfico de barras da variável tipo de residência") +
  theme_minimal() + 
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)

## Tabagismo
# gráfico de barras
ggplot(massa_suja, aes(x = factor(smoking_status))) + 
  geom_bar(aes(fill = factor(smoking_status)), alpha = 0.8) +
  scale_fill_manual(values = c("#EEEEFF", "#7f7caf", "#9fb4c7", "#28587b", "#223843")) +
  labs(x = "Tabagismo", y = "Frequência", title = "Gráfico de barras da variável tabagismo") +
  theme_minimal() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)

## AVC
# gráfico de barras
ggplot(massa_suja, aes(x = factor(stroke))) + 
  geom_bar(aes(fill = factor(stroke)), alpha = 0.8) +
  scale_fill_manual(values = c("#EEEEFF", "#7f7caf", "#9fb4c7", "#28587b", "#223843")) +
  labs(x = "AVC", y = "Frequência", title = "Gráfico de barras da variável AVC") +
  theme_minimal() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5)


#### Análises bidimensionais
### Gerando a matriz de correlação
corr_matrix <- cor(massa[, c('age', 'hypertension', 'heart_disease', 'ever_married', 'work_type', 'residence_type', 'avg_glucose_level', 'bmi', 'smoking_status', 'stroke')])
corrplot(corr_matrix, method = "number", diag = FALSE, tl.col = "black", tl.srt = 45)

