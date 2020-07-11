getwd()
setwd("/Users/fernando/Google Drive/DSA/BigDataRAzure/Projetos/Risco_de_Credito")


df <- read.csv("credit_dataset.csv", header = TRUE, sep = ",")

str(df)
# O data frame foi lido com todas a variáveis como numéricas, mas a maioria delas
# é categóricas, vou mudando essas variáveis ao longo da análise exploratória.

# Verificação de valores missing
df[, is.na(df) == TRUE]


# Analisando a variável target (credit.racing)
library(ggplot2)

df$credit.rating <- as.factor(df$credit.rating)

ggplot(df, aes(x=credit.rating)) +
  geom_bar(stat="count", width=0.7, fill=c("#9c0000", "#0000a2"))+
  labs(x = "Análise de Crédito", y = "Quantidade",
       title = "Concessão de Análise de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

table(df$credit.rating)
prop.table(table(df$credit.rating))

# Como podemos ver os dados da variável target estão bem desbalanceados, sendo 30% amostras de crédito não concedido e 70% de amostras com crédito cocedidos. Precisamos balancear esses daddos de forma que fique aproximadamente 50% cada um, mas isso só será feito após a criação das variáveis de treino e teste.

# Análise das variáveis para a predição.

# Acount.balance

# Tranformando a variável de numérica para fator
df$account.balance <- as.factor(df$account.balance)

# Quantidade de cada fator nessa variável
table(df$account.balance)

# Gráfico da contagem da variável por crédito financeiro concedido ou não.
ggplot(df, aes(account.balance, ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2") ) +
  labs(x = "Status da Conta", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Status da Conta x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Como podemos ver pelo gráfico, pessoas que tem algum montante na conta corrente têm mais chances de conseguir um crédito financeiro do que as demais, assim como pessoas com alguma conta aberta, porém sem saldo possuem mais chances de conseguir um crédito do que as pessas sem conta corrente.

# credit.duration.months

# Histograma da duração do empréstimo separado por créditos iagual a sim e não.
ggplot(df, aes(credit.duration.months, fill = credit.rating)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity',
                 bins=7) +
  scale_fill_manual(values = c("#9c0000", "#0000a2") )

# Gráfico da contagem da variável por crédito financeiro concedido ou não.
ggplot(df, aes(as.factor(credit.duration.months), ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2") ) +
  labs(x = "Duração do Empréstimo", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Duração do Empréstimo x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Como podemos ver nos 2 gráficos, a decisão de concessão de crédito financeiro muda e acordo com tempo do emprétimo, basicamente, quanto mais longo, mais difícil de se conseguir o crédito.

# A partir dessa variável será criada uma nova variável que irá juntar os valores acima em grupos, que eu acho que irá melhorar o cálculo do modelo preditivo.

# Criando a nova variável
df$fact.credit.duration.months<-findInterval(df$credit.duration.months,
                                             c(0,6,12,18,24,30,36))

# Dando nomes as observações da variável.
library(dplyr)
df<-df %>% 
  mutate(fact.credit.duration.months=as.factor(fact.credit.duration.months))
levels(df$fact.credit.duration.months)<-c("Menos de 6","6 a 12", "12 a 18",
                                          "18 a 24", "24 a 30", "30 a 36",
                                          "mais de 36")

# Visualizando os primeiros dados das duas colunas.  
head(df[, c("credit.duration.months", "fact.credit.duration.months")])

#Análise Gráfica

ggplot(df, aes(fact.credit.duration.months, ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2") ) +
  labs(x = "Duração do Empréstimo", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Duração do Empréstimo x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Agora podemos visualizar melhor a conclusão dita acima.

# previous.credit.payment.status

# Tranformando a variável de numérica para fator
df$previous.credit.payment.status <- as.factor(df$previous.credit.payment.status)

# Quantidade de cada fator nessa variável
table(df$previous.credit.payment.status)

# Gráfico da contagem da variável por crédito financeiro concedido ou não.
ggplot(df, aes(previous.credit.payment.status, ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2") ) +
  labs(x = "Histórico de Emprétimos", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Histórico de Emprétimos x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Como podemos ver pelo gráfico quem tem mais problemas históricos com empréstimos tende a ter menos chances de conseguir um novo crédito, como já era esperado.

# credit.purpose

# Tranformando a variável de numérica para fator
df$credit.purpose <- as.factor(df$credit.purpose)

# Quantidade de cada fator nessa variável
table(df$credit.purpose)

# Gráfico da contagem da variável por crédito financeiro concedido ou não.
ggplot(df, aes(credit.purpose, ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2")) +
  labs(x = "Valor Emprestado", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Valor Emprestado x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Me parece que essa variável também influencia na concessão do crédito, mas acredito que não seja muito forte essa correlação.

# credit.amount

# Histograma do valor do emprétimo separado por créditos iaguais a sim e não.
ggplot(df, aes(credit.amount, fill = credit.rating)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity',
                 bins=30) +
  scale_fill_manual(values = c("#9c0000", "#0000a2") )

# Não consigo tirar uma idéia muito clara dessa variável, mas acredito que ela faz uma pequena diferença para valores maiores também.

# Para tentar visualizar melhor vou usar a mesma estratégia empregada na variável numérica anterior. Dividir em grupos e visualizar em um gráfico.

#Criando a variável fact.credit.amount
df$fact.credit.amount<-findInterval(df$credit.amount,
                                    c(0, 2500, 5000, 10000))
df<-df %>% 
  mutate(fact.credit.amount=as.factor(fact.credit.amount))
levels(df$fact.credit.amount) <- c("Menos de 2500","2500 a 5000", 
                                   "5000 a 10000", "mais de 10000")

# Visualizando os primeiros dados das duas colunas.  
head(df[, c("credit.amount", "fact.credit.amount")])

# Análise Gráfica
ggplot(df, aes(fact.credit.amount, ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2")) +
  labs(x = "Valor Emprestado", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Valor Emprestado x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Agora sim podemos ter uma visão mais clara sobre essa variável. Quem solicita menos dinheiro, tem mais chance de receber o crédito concedido.

# savings

# Tranformando a variável de numérica para fator
df$savings <- as.factor(df$savings)

# Quantidade de cada fator nessa variável
table(df$savings)

# Gráfico da contagem da variável por crédito financeiro concedido ou não.
ggplot(df, aes(savings, ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2")) +
  labs(x = "Valor na Poupança", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Valor na Poupança x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Como esperávamos quem tem mais dinheiro na poupança tem mais chance de ter o crédito concedido.

# employment.duration

# Tranformando a variável de numérica para fator
df$employment.duration <- as.factor(df$employment.duration)

# Quantidade de cada fator nessa variável
table(df$employment.duration)

# Gráfico da contagem da variável por crédito financeiro concedido ou não.
ggplot(df, aes(employment.duration, ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2")) +
  labs(x = "Condição no Emprego", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Condição no Emprego x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Essa variável também influencia diretamente em na variável target. Quem está desempregado, ou a menos tempo no trabalho tem menos chance de ter o crédito concedido do que quem está a mais tempo empregado.

# installment.rate

# Tranformando a variável de numérica para fator
df$installment.rate <- as.factor(df$installment.rate)

# Quantidade de cada fator nessa variável
table(df$installment.rate)

# Gráfico da contagem da variável por crédito financeiro concedido ou não.
ggplot(df, aes(installment.rate, ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2")) +
  labs(x = "Taxa de Renda Disponível", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", 
       title = "Taxa de Renda Disponível x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Esta é outra variável interessante, pois como vemos, quem tem mais renda disponível, tem tem mais chance de ter o crédito concedido.

# marital.status

# Tranformando a variável de numérica para fator
df$marital.status <- as.factor(df$marital.status)

# Quantidade de cada fator nessa variável
table(df$marital.status)

# Gráfico da contagem da variável por crédito financeiro concedido ou não.
ggplot(df, aes(marital.status, ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2")) +
  labs(x = "Estado Civil", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Estado Civil x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Podemos ver nesse gráfico que homens casados / viúvos têm mais chance de ter o crédito concedido do que as outras categorias.

# guarantor

# Tranformando a variável de numérica para fator
df$guarantor <- as.factor(df$guarantor)

# Quantidade de cada fator nessa variável
table(df$guarantor)

# Gráfico da contagem da variável por crédito financeiro concedido ou não.
ggplot(df, aes(guarantor, ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2")) +
  labs(x = "Fiador", y = "Quantidade de Crédito Concedido", fill = "Crédito", title = "Fiador x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Essa variável não parece fazer muita diferença para o modelo preditivo, pois além de termos poucas observações do tipo 2 se olharmos no gráfico a relação entre os dois tipos com a concessão de crédito é quase a mesma.

perct.guarantor <- group_by(df, guarantor) %>%
  mutate(group_size = n()) %>%
  group_by(guarantor, credit.rating) %>%
  summarise(perc = (n()/max(group_size)*100))
perct.guarantor

# Apenas retificar o que eu disse acima fiz essa tabela para vermos como esses dados estão distribuidos em porcentagem.

# residence.duration

# Tranformando a variável de numérica para fator
df$residence.duration <- as.factor(df$residence.duration)

# Quantidade de cada fator nessa variável
table(df$residence.duration)

# Gráfico da contagem da variável por crédito financeiro concedido ou não.
ggplot(df, aes(residence.duration, ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2")) +
  labs(x = "Tempo de Mordia", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Tempo de Mordia x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Verificando o percentual das variáveis
perct.residence.duration <- group_by(df, residence.duration) %>%
  mutate(group_size = n()) %>%
  group_by(residence.duration, credit.rating) %>%
  summarise(perc = (n()/max(group_size)*100))
perct.residence.duration

# Conforme podemos ver no gráfico e na tabela, o percentual de chance de conseguir ou não crédito é quase o mesmo, independente do tempo em que a pessoa vive na sua residência. Portanto essa variável não será viável em meu modelo

# current.assets

# Quantidade de cada fator nessa variável
df$current.assets <- as.factor(df$current.assets)

# Quantidade de cada fator nessa variável
table(df$current.assets)

# Gráfico da contagem da variável por crédito financeiro concedido ou não.
ggplot(df, aes(current.assets, ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2")) +
  labs(x = "Recursos Disponíveis", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Recursos Disponíveis x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Verificando o percentual das variáveis
perct.current.assets <- group_by(df, current.assets) %>%
  mutate(group_size = n()) %>%
  group_by(current.assets, credit.rating) %>%
  summarise(perc = (n()/max(group_size)*100))
perct.current.assets

# Aparentemente quem é proprietário de uma casa tem menos chance de conseguir um empréstimo do que os outros. Talvez a hipoteca da casa seja um impeditivo.

# Age

# Histograma da idade separado por créditos igual a sim e não.
ggplot(df, aes(age, fill = credit.rating)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity',
                 bins=10) +
  scale_fill_manual(values = c("#9c0000", "#0000a2")) +
  labs(x = "Idade", y = "Frequência", fill = "Crédito", 
       title = "Histograma da Idade por Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Boxplot
ggplot(df, aes(x=credit.rating, y=age)) + 
  geom_boxplot() +
  labs(x = "Idade", y = "Frequência",
       fill = "Crédito", title = "Boxplot da Idade por Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Podemos ver pelo histograma e pelo boxplot que os mais jovens normalmente têm menos chance de conseguir um empréstimo.

# Vou criar a coluna de grupo de idade também para poder analisar melhor esses dados.

# Criando a variável fact.age
summary(df$age)
df$fact.age<-findInterval(df$age, c(18, 25, 33, 38, 45, 55))
df<-df %>% 
  mutate(fact.age=as.factor(fact.age))
levels(df$fact.age) <- c("Menos de 25", "25 a 33", "33 a 38", "38 a 45", "45 a 55",
                         "mais de 55")

# Visualizando os primeiros dados das duas colunas.  
head(df[, c("age", "fact.age")])

# Análise Gráfica
ggplot(df, aes(fact.age, ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2")) +
  labs(x = "Idade", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Idade x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Podemos perceber a relação entre a idade e a concessão de crédito de uma forma mais clara agora.

# other.credits

# Quantidade de cada fator nessa variável
df$other.credits <- as.factor(df$other.credits)

# Quantidade de cada fator nessa variável
table(df$other.credits)

# Gráfico da contagem da variável por crédito financeiro concedido ou não.
ggplot(df, aes(other.credits, ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2")) +
  labs(x = "Outros Créditos", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Outros Créditos x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Pessoas que tem créditos em outros bancos têm mais dificuldade em conseguir o crédito.

# apartment.type

# Quantidade de cada fator nessa variável
df$apartment.type <- as.factor(df$apartment.type)

# Quantidade de cada fator nessa variável
table(df$apartment.type)

# Gráfico da contagem da variável por crédito financeiro concedido ou não.
ggplot(df, aes(apartment.type, ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2")) +
  labs(x = "Tipo de Moradia", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Tipo de Moradia x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Essa variável também irá entrar em nosso modelo preditivo.

# bank.credits

# Quantidade de cada fator nessa variável
df$bank.credits <- as.factor(df$bank.credits)

# Quantidade de cada fator nessa variável
table(df$bank.credits)

# Gráfico da contagem da variável por crédito financeiro concedido ou não.
ggplot(df, aes(bank.credits, ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2")) +
  labs(x = "Quantidade de Créditos ja Concedidos", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Quantidade de Créditos ja Concedidos x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Verificando o percentual das variáveis
perct.bank.credits <- group_by(df, bank.credits) %>%
  mutate(group_size = n()) %>%
  group_by(bank.credits, credit.rating) %>%
  summarise(perc = (n()/max(group_size)*100))
perct.bank.credits

# A diferença no número de créditos ja concedidos é tão pouca que não vale a pena considerar no modelo preditivo.

# occupation

# Quantidade de cada fator nessa variável
df$occupation <- as.factor(df$occupation)

# Quantidade de cada fator nessa variável
table(df$occupation)

# Gráfico da contagem da variável por crédito financeiro concedido ou não.
ggplot(df, aes(occupation, ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2")) +
  labs(x = "Ocupação", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Ocupação x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Verificando o percentual das variáveis
perct.occupation <- group_by(df, occupation) %>%
  mutate(group_size = n()) %>%
  group_by(occupation, credit.rating) %>%
  summarise(perc = (n()/max(group_size)*100))
perct.occupation

# A diferença percentual de cada categoria é bem pouca, acho que não compensa utilizar essa variável em nosso algoritmo.

# dependents

# Quantidade de cada fator nessa variável
df$dependents <- as.factor(df$dependents)

# Quantidade de cada fator nessa variável
table(df$dependents)

# Gráfico da contagem da variável por crédito financeiro concedido ou não.
ggplot(df, aes(dependents, ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2")) +
  labs(x = "Dependentes", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Dependentes x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Verificando o percentual das variáveis
perct.dependents <- group_by(df, dependents) %>%
  mutate(group_size = n()) %>%
  group_by(dependents, credit.rating) %>%
  summarise(perc = (n()/max(group_size)*100))
perct.dependents

# Essa variável também não compensa se utilizada em nosso modelo preditivo.

# telephone

# Quantidade de cada fator nessa variável
df$telephone <- as.factor(df$telephone)

# Quantidade de cada fator nessa variável
table(df$telephone)

# Gráfico da contagem da variável por crédito financeiro concedido ou não.
ggplot(df, aes(telephone, ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2")) +
  labs(x = "Telefone", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Telefone x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Verificando o percentual das variáveis
perct.telephone <- group_by(df, telephone) %>%
  mutate(group_size = n()) %>%
  group_by(telephone, credit.rating) %>%
  summarise(perc = (n()/max(group_size)*100))
perct.telephone

# A existência de telefone também não muda muito em relação à concessão ou não de crédito.

# foreign.worker

# Quantidade de cada fator nessa variável
df$foreign.worker <- as.factor(df$foreign.worker)

# Quantidade de cada fator nessa variável
table(df$foreign.worker)

# Gráfico da contagem da variável por crédito financeiro concedido ou não.
ggplot(df, aes(foreign.worker, ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2")) +
  labs(x = "Trabalha Fora", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Trabalha Fora x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Verificando o percentual das variáveis
perct.foreign.worker <- group_by(df, foreign.worker) %>%
  mutate(group_size = n()) %>%
  group_by(foreign.worker, credit.rating) %>%
  summarise(perc = (n()/max(group_size)*100))
perct.foreign.worker

# Como podemos ver pessoas que trabalham e moram na mesma cidade têm mais chances de conseguir um empréstimo, mas a diferença de coletas de um de outro está muito grande. Do mesmo jeito irei usar essa variável em meu primeiro modelo.

require(caTools)

#Criando uma seed
set.seed(123)
#Dividindo o data frame em treino e teste.
sample = sample.split(df, SplitRatio = 0.70)
train = subset(df, sample ==TRUE)
test = subset(df, sample==FALSE)

#Verificando o número de linhas de cada data frame
nrow(train)
nrow(test)

# Agora é chegou a hora de rodar os algoritmos de modelo preditivo.
# Vamos começar treinando o modelo com as variáveis que eu achei mais interessantes 
# durante a fase de análise.
formula_v1 <- as.formula('credit.rating ~ account.balance +
                         fact.credit.duration.months +
                         previous.credit.payment.status + credit.purpose +
                         fact.credit.amount + savings + employment.duration +
                         installment.rate + marital.status + current.assets +
                         fact.age + other.credits + apartment.type +
                         foreign.worker')

# Treinando o modelo com o algoritmo de regressão logística
model_glm_v1 <- glm(formula = formula_v1, data = train, family = "binomial")

# Verificando alguns resultados do modelo treinado
summary(model_glm_v1)

# Realizando a predição com o modelo treinado
pred_glm_v1 <- predict(model_glm_v1, test, type="response")

# Arredondando para 0 ou 1
pred_glm_v1 <- round(pred_glm_v1)

#Confusion Matrix da predição.
library(caret)
confusionMatrix(table(data = pred_glm_v1, reference = test$credit.rating),
                positive = '1')

# A regressão logística nos entregou um bom resultado, mas vamos verificar como esses dados se comportam com outros modelos.

## Criando o modelo com o algoritmo Árvore de Decissão
library(C50)
modelo_tree_v1 = C5.0(formula_v1, data = train) 

# Previsões nos dados de teste
pred_tree_v1 = predict(modelo_tree_v1, test, type='class')

# Confusion Matrix
confusionMatrix(test$credit.rating, pred_tree_v1, positive = '1')

# Este modelo teve um desempenho pouco pior do que o modelo de regessão
# logística

# Criando o modelo com o algoritmo SVM (Suport Vector Machine)
library(e1071)

modelo_svm_v1 <- svm(formula_v1, data = train, 
                     type = 'C-classification', kernel = 'radial') 

# Previsões nos dados de teste
pred_svm_v1 = predict(modelo_svm_v1, test)

# Confusion Matrix
confusionMatrix(test$credit.rating, pred_svm_v1, positive = '1')

# O modelo de regressão logística ainda está se saindo melhor por enquanto.

# Criando o modelo com o algoritmo Random Forest
library(rpart)
modelo_rf_v1 = rpart(formula_v1, data = train, control = rpart.control(cp = .0005)) 
# Previsões nos dados de teste
pred_rf_v1 = predict(modelo_rf_v1, test, type='class')

# Confusion Matrix
confusionMatrix(test$credit.rating, pred_rf_v1, positive = '1')

# Esse foi o pior resultado até o momento.

# Criando o modelo com o algoritmo Naive Bayes
model_nb_v1 = naiveBayes(formula_v1, data=train)

# Previsões nos dados de teste
pred_nb_v1 <- predict(model_nb_v1, newdata=test)

# Confusion Matrix
confusionMatrix(test$credit.rating, pred_nb_v1, positive = '1')

# Este foi o melhor resultado encontrado.

#Antes temos que normalizar os dados numéricos que ainda não foram utilizados.
normaliza_dados <- function(df, var){
  for(v in var)
    df[[v]] <- scale(df[[v]], center=T, scale=T)
  return(df)
}
var <- c('credit.duration.months', 'credit.amount', 'age')
df<- normaliza_dados(df, var)

# Atualizando train e test
train = subset(df, sample ==TRUE)
test = subset(df, sample==FALSE)

# Feature Selection com o Random Forest
require(randomForest)
model_rf_imp_var <- randomForest(credit.rating ~ ., data = df, ntree = 100,
                                 nodesize = 10, importance = TRUE)

# Plotando as variáveis por grau de importância
varImpPlot(model_rf_imp_var)

# Neste gráfico podemos ver as variáveis mais relevantes na predição do modelo.

# Vamos utilizar o modelo as primeiras 10 variaveis do modelo de random forest para treinar o data frame novamente.
formula_v2 <- as.formula('credit.rating ~ account.balance  +
                         previous.credit.payment.status + savings +
                         fact.credit.duration.months + credit.duration.months +
                         age + credit.amount + bank.credits + fact.credit.amount +
                         other.credits')

# Criando o modelo com o algoritmo Naive Bayes
model_nb_v2 = naiveBayes(formula_v2, data=train)

# Previsões nos dados de teste
pred_nb_v2 <- predict(model_nb_v2, newdata=test)

# Confusion Matrix
confusionMatrix(test$credit.rating, pred_nb_v2, positive = '1')

# O primeiro modelo está com um melhor desempenho até o momento.

#Vamos utilizar as primeiras 15 variáveis agora.
formula_v3 <- as.formula('credit.rating ~ account.balance  +
                         previous.credit.payment.status +  savings +
                         fact.credit.duration.months + credit.duration.months +
                         age + credit.amount + bank.credits + fact.credit.amount +
                         other.credits + guarantor + employment.duration +
                         installment.rate + current.assets + residence.duration')

# Criando o modelo com o algoritmo Naive Bayes
model_nb_v3 = naiveBayes(formula_v3, data=train)

# Previsões nos dados de teste
pred_nb_v3 <- predict(model_nb_v3, newdata=test)

# Confusion Matrix
confusionMatrix(test$credit.rating, pred_nb_v3, positive = '1')

# Obtivemos a mesma acuracia do primeiro modelo, vamos utilizar todas as varuáveis agora para ver como o modelo se comporta.

formula_v4 <- as.formula('credit.rating ~ .')

# Criando o modelo com o algoritmo Naive Bayes
model_nb_v4 = naiveBayes(formula_v4, data=train)

# Previsões nos dados de teste
pred_nb_v4 <- predict(model_nb_v4, newdata=test)

# Confusion Matrix
confusionMatrix(test$credit.rating, pred_nb_v4, positive = '1')


# Antes de fazer o balanceamento precisamos transformar as variáveis que foram normalizadqos para o tipo numérico, pois durante a normalização eles ficaram em um tipo de fator numérico.
train$credit.duration.months <- as.numeric(train$credit.duration.months)
train$credit.amount  <- as.numeric(train$credit.amount )
train$age <- as.numeric(train$age )
test$credit.duration.months <- as.numeric(test$credit.duration.months)
test$credit.amount  <- as.numeric(test$credit.amount )
test$age <- as.numeric(test$age )


library(ROSE)
# Balanceando os dados
# ROSE nos dados de treino
rose_train <- ROSE(credit.rating ~ ., data = train, seed = 1)$data
prop.table(table(rose_train$credit.rating))

# ROSE nos dados de teste
rose_test <- ROSE(credit.rating ~ ., data = test, seed = 1)$data
prop.table(table(rose_test$credit.rating))

# Criando o modelo com o algoritmo Naive Bayes
model_nb_v5 = naiveBayes(formula_v3, data=rose_train)

# Previsões nos dados de teste
pred_nb_v5 <- predict(model_nb_v5, newdata=rose_test)

# Confusion Matrix
confusionMatrix(rose_test$credit.rating, pred_nb_v5, positive = '1')

# Curva roc para o modelo_v3 (balanceado)
roc.curve(test$credit.rating, pred_nb_v2, plotit = T, col = "red")

# Curva roc para o modelo_v5 (desbalanceado)
roc.curve(rose_test$credit.rating, pred_nb_v5, plotit = T,
          col = "green", add.roc = T)




