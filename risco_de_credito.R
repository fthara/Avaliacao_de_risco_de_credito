getwd()
setwd("/Users/fernando/Google Drive/DSA/BigDataRAzure/Projetos/Risco_de_Credito")


df <- read.csv("credit_dataset.csv", header = TRUE, sep = ",")
View(df)

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
       title = "Positivos e Negativos na Análise de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

table(df$credit.rating)
prop.table(table(df$credit.rating))

# Como podemos ver os dados da variável target estão bem desbalanceados, sendo 30% amostras
# de crédito não concedido e 70% de amostras com crédito cocedidos. Precisamos balancear esse
# daddos de forma que fique aproximadamente 50% cada um, mas isso só será feito após a
# criação das variáveis de treino e teste.

# Análise das variáveis para a predição.

# Acount.balance

#tranformando a variável de numérica para fator
df$account.balance <- as.factor(df$account.balance)

#Quantidade de cada fator nessa variável
table(df$account.balance)

#Gráfico da contagem da variável por crédito financeiro concedido ou não.
ggplot(df, aes(account.balance, ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2") ) +
  labs(x = "Status da Conta", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Status da Conta x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Como podemos ver pelo gráfico, pessoas que tem algum montante na conta corrente têm mais
# chances de conseguir um crédito financeiro do que as demais, assim como pessoas com alguma
# conta aberta, porém sem saldo possuem mais chances de conseguir um crédito do que as pessas
# sem conta corrente.

# credit.duration.months

# Histograma da duração do empréstimo separado por créditos iagual a sim e não.
ggplot(df, aes(credit.duration.months, fill = credit.rating)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity', bins=7) +
  scale_fill_manual(values = c("#9c0000", "#0000a2") )

# Gráfico da contagem da variável por crédito financeiro concedido ou não.
ggplot(df, aes(as.factor(credit.duration.months), ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2") ) +
  labs(x = "Duração do Empréstimo", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Duração do Empréstimo x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Como podemos ver nos 2 gráficos, a decisão de concessão de crédito financeiro muda e acordo
# com tempo do emprétimo, basicamente, quanto mais longo, mais difícil de se conseguir o crédito

# A partir dessa variável será criada uma nova variável que irá juntar os valores acima em 
# grupos, que eu acho que irá melhorar o cálculo do modelo preditivo.

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

#tranformando a variável de numérica para fator
df$previous.credit.payment.status <- as.factor(df$previous.credit.payment.status)

#Quantidade de cada fator nessa variável
table(df$previous.credit.payment.status)

#Gráfico da contagem da variável por crédito financeiro concedido ou não.
ggplot(df, aes(previous.credit.payment.status, ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2") ) +
  labs(x = "Histórico de Emprétimos", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Histórico de Emprétimos x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Como podemos ver pelo gráfico quem tem mais problemas históricos com empréstimos tende 
# a ter menos chances de conseguir um novo crédito, como já era esperado.

# credit.purpose
#tranformando a variável de numérica para fator
df$credit.purpose <- as.factor(df$credit.purpose)

#Quantidade de cada fator nessa variável
table(df$credit.purpose)

#Gráfico da contagem da variável por crédito financeiro concedido ou não.
ggplot(df, aes(credit.purpose, ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2")) +
  labs(x = "Valor Emprestado", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Valor Emprestado x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Me parece que essa variável também influencia na concessão do crédito, mas acredito que
# não seja muito forte essa correlação.

# credit.amount

# Histograma do valor do emprétimo separado por créditos iaguais a sim e não.
ggplot(df, aes(credit.amount, fill = credit.rating)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity', bins=30) +
  scale_fill_manual(values = c("#9c0000", "#0000a2") )

# Não consigo tirar uma idéia muito clara dessa variável, mas acredito que ela faz uma
# pequena diferença para valores maiores também.
# Para tentar visualizar melhor vou usar a mesma estratégia empregada na variável numérica
# anterior. Dividir em grupos e visualizar em um gráfico.

#Criando a variável fact.credit.amount
df$fact.credit.amount<-findInterval(df$credit.amount,
                                             c(0, 2500, 5000, 10000))
df<-df %>% 
  mutate(fact.credit.amount=as.factor(fact.credit.amount))
levels(df$fact.credit.amount) <- c("Menos de 2500","2500 a 5000", 
                                   "5000 a 10000", "mais de 10000")

# Visualizando os primeiros dados das duas colunas.  
head(df[, c("credit.amount", "fact.credit.amount")])

#Análise Gráfica

ggplot(df, aes(fact.credit.amount, ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2")) +
  labs(x = "Valor Emprestado", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Valor Emprestado x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Agora sim podemos ter uma visão mais clara sobre essa variável. Quem solicita menos
# dinheiro, tem mais chance de receber o crédito concedido.

# savings

#tranformando a variável de numérica para fator
df$savings <- as.factor(df$savings)

#Quantidade de cada fator nessa variável
table(df$savings)

#Gráfico da contagem da variável por crédito financeiro concedido ou não.
ggplot(df, aes(savings, ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2")) +
  labs(x = "Valor na Poupança", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Valor na Poupança x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Como esperávamos quem tem mais dinheiro na poupança tem mais chance de ter o crédito
# concedido.

# employment.duration

#tranformando a variável de numérica para fator
df$employment.duration <- as.factor(df$employment.duration)

#Quantidade de cada fator nessa variável
table(df$employment.duration)

#Gráfico da contagem da variável por crédito financeiro concedido ou não.

ggplot(df, aes(employment.duration, ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2")) +
  labs(x = "Condição no Emprego", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Condição no Emprego x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Essa variável também influencia diretamente em na variável target. Quem está desempregado,
# ou a menos tempo no trabalho tem menos chance de ter o crédito concedido do que quem está
# a mais tempo empregado.

# installment.rate

#tranformando a variável de numérica para fator
df$installment.rate <- as.factor(df$installment.rate)

#Quantidade de cada fator nessa variável
table(df$installment.rate)

#Gráfico da contagem da variável por crédito financeiro concedido ou não.

ggplot(df, aes(installment.rate, ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2")) +
  labs(x = "Condição no Emprego", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Condição no Emprego x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Esta é outra variável interessante, pois como vemos, quem tem mais renda disponível, tem
# tem mais chance de ter o crédito concedido.

# marital.status

#tranformando a variável de numérica para fator
df$marital.status <- as.factor(df$marital.status)

#Quantidade de cada fator nessa variável
table(df$marital.status)

#Gráfico da contagem da variável por crédito financeiro concedido ou não.

ggplot(df, aes(marital.status, ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2")) +
  labs(x = "Condição no Emprego", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Condição no Emprego x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Podemos ver nesse gráfico que homens casados / viúvos têm mais chance de ter o crédito
# concedido do que as outras categorias.

# guarantor

#tranformando a variável de numérica para fator
df$guarantor <- as.factor(df$guarantor)

#Quantidade de cada fator nessa variável
table(df$guarantor)

#Gráfico da contagem da variável por crédito financeiro concedido ou não.

ggplot(df, aes(guarantor, ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2")) +
  labs(x = "Participação em Créditos já Concedidos", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Participação em Créditos já Concedidos x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Essa variável não parece fazer muita diferença para o modelo preditivo, pois além de
# termos poucas observações do tipo 2 se olharmos no gráfico a relação entre os dois
# tipos com a concessão de crédito é quase a mesma.

perct.guarantor <- group_by(df, guarantor) %>%
  mutate(group_size = n()) %>%
  group_by(guarantor, credit.rating) %>%
  summarise(perc = (n()/max(group_size)*100))
perct.guarantor

# Apenas retificar o que eu disse acima fiz essa tabela para vermos como esses dados estão
# distribuidos em porcentagem.

# residence.duration

#tranformando a variável de numérica para fator
df$residence.duration <- as.factor(df$residence.duration)

#Quantidade de cada fator nessa variável
table(df$residence.duration)

#Gráfico da contagem da variável por crédito financeiro concedido ou não.
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

# Conforme podemos ver no gráfico e na tabela, o percentual de chance de conseguir ou
# não crédito é quase o mesmo, independente do tempo em que a pessoa vive na sua 
# residência. Portanto essa variável não será viável em meu modelo

# current.assets

#Quantidade de cada fator nessa variável
df$current.assets <- as.factor(df$current.assets)

#Quantidade de cada fator nessa variável
table(df$current.assets)

#Gráfico da contagem da variável por crédito financeiro concedido ou não.
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

# Aparentemente quem é proprietário de uma casa tem menos chance de conseguir um empréstimo
# do que os outros. Talvez a hipoteca da casa seja um impeditivo.

# Age

# Histograma da idade separado por créditos igual a sim e não.
ggplot(df, aes(age, fill = credit.rating)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity', bins=10) +
  scale_fill_manual(values = c("#9c0000", "#0000a2")) +
  labs(x = "Idade", y = "Frequência",
       fill = "Crédito", title = "Histograma da Idade por Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Boxplot
ggplot(df, aes(x=credit.rating, y=age)) + 
  geom_boxplot() +
  labs(x = "Idade", y = "Frequência",
       fill = "Crédito", title = "Boxplot da Idade por Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Podemos ver pelo histograma e pelo boxplot que os mais jovens normalmente têm menos chance
# de conseguir um empréstimo.

# Vou criar a coluna de grupo de idade também para poder analisar melhor esses dados.

#Criando a variável fact.age
summary(df$age)
df$fact.age<-findInterval(df$age, c(18, 25, 33, 38, 45, 55))
df<-df %>% 
  mutate(fact.age=as.factor(fact.age))
levels(df$fact.age) <- c("Menos de 25", "25 a 33", "33 a 38", "38 a 45", "45 a 55",
                         "mais de 55")

# Visualizando os primeiros dados das duas colunas.  
head(df[, c("age", "fact.age")])

#Análise Gráfica
ggplot(df, aes(fact.age, ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2")) +
  labs(x = "Idade", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Idade x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Podemos perceber a relação entre a idade e a concessão de crédito de uma forma mais
# clara agora.

# other.credits

#Quantidade de cada fator nessa variável
df$other.credits <- as.factor(df$other.credits)

#Quantidade de cada fator nessa variável
table(df$other.credits)

#Gráfico da contagem da variável por crédito financeiro concedido ou não.
ggplot(df, aes(other.credits, ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2")) +
  labs(x = "Outros Créditos", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Outros Créditos x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Pessoas que tem créditos em outros bancos têm mais dificuldade em conseguir o crédito.

# apartment.type

#Quantidade de cada fator nessa variável
df$apartment.type <- as.factor(df$apartment.type)

#Quantidade de cada fator nessa variável
table(df$apartment.type)

#Gráfico da contagem da variável por crédito financeiro concedido ou não.
ggplot(df, aes(apartment.type, ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2")) +
  labs(x = "Tipo de Moradia", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Tipo de Moradia x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Essa variável também irá entrar em nosso modelo preditivo.

# bank.credits

#Quantidade de cada fator nessa variável
df$bank.credits <- as.factor(df$bank.credits)

#Quantidade de cada fator nessa variável
table(df$bank.credits)

#Gráfico da contagem da variável por crédito financeiro concedido ou não.
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

# A diferença no número de créditos ja concedidos é tão pouca que não vale a pena
# considerar no modelo preditivo.

# occupation

#Quantidade de cada fator nessa variável
df$occupation <- as.factor(df$occupation)

#Quantidade de cada fator nessa variável
table(df$occupation)

#Gráfico da contagem da variável por crédito financeiro concedido ou não.
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

# A diferença percentual de cada categoria é bem pouca, acho que não compensa utilizar
# essa variável em nosso algoritmo.

# dependents

#Quantidade de cada fator nessa variável
df$dependents <- as.factor(df$dependents)

#Quantidade de cada fator nessa variável
table(df$dependents)

#Gráfico da contagem da variável por crédito financeiro concedido ou não.
ggplot(df, aes(dependents, ..count..)) +
  geom_bar(aes(fill = credit.rating), position = "dodge") +
  scale_fill_manual(values = c("#9c0000", "#0000a2")) +
  labs(x = "Ocupação", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Ocupação x Concessão de Crédito") +
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
  labs(x = "Ocupação", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Ocupação x Concessão de Crédito") +
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
  labs(x = "Ocupação", y = "Quantidade de Crédito Concedido",
       fill = "Crédito", title = "Ocupação x Concessão de Crédito") +
  theme(plot.title = element_text(hjust = 0.5))

# Verificando o percentual das variáveis
perct.foreign.worker <- group_by(df, foreign.worker) %>%
  mutate(group_size = n()) %>%
  group_by(foreign.worker, credit.rating) %>%
  summarise(perc = (n()/max(group_size)*100))
perct.foreign.worker

# Como podemos ver pessoas que trabalham e moram na mesma cidade têm mais chances de
# conseguir um empréstimo, mas a diferença de coletas de um de outro está muito grande.
# Inicialmente irei utilizar essa variável em meu algoritmo, mas depois testarei sem.

# Preparação para a análise preditiva.
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

# Gerando valores para balancear o data set.
# É sempre bom aplicar o balanceamento depois criado os dados de treino e teste.
# Se fizermos antes, o padrão usado para aplicar o oversampling será o mesmo nos 
# dados de treino e de teste e, assim, a avaliação do modelo fica comprometida.

library(ROSE)

#ROSE nos dados de treino
rose_train <- ROSE(credit.rating ~ ., data = train, seed = 1)$data
prop.table(table(rose_train$credit.rating))

#ROSE nos dados de teste
rose_test <- ROSE(credit.rating ~ ., data = test, seed = 1)$data
prop.table(table(rose_test$credit.rating))

# Agora que os dados estão balanceados e organizados vamos aplicar nos modelos preditivos

#Definindo as variáveis para o treino.
formula <- as.formula(paste('credit.rating ~ account.balance + fact.credit.duration.months + 
                  previous.credit.payment.status + credit.purpose + fact.credit.amount +
                  savings + employment.duration + installment.rate + marital.status +
                  current.assets + fact.age + other.credits + apartment.type + 
                  foreign.worker'))

# Criando o modelo com o algoritmo SVM (Suport Vector Machine)
library(e1071)

modelo_svm_v1 <- svm(formula, data = rose_train, 
                     type = 'C-classification', kernel = 'radial') 

# Previsões nos dados de teste
pred_test_svm_v1 = predict(modelo_svm_v1, rose_test)

# Percentual de previsões corretas com dataset de teste
mean(pred_test_svm_v1 == rose_test$credit.rating)

# Confusion Matrix
table(pred_test_svm_v1, rose_test$credit.rating)

# Criando o modelo com o algoritmo Random Forest
# Criando o modelo
library(rpart)
library(caret)

modelo_rf_v1 = rpart(formula, data = rose_train, control = rpart.control(cp = .0005)) 

# Previsões nos dados de teste
rf_pred = predict(modelo_rf_v1, rose_test, type='class')

# Percentual de previsões corretas com dataset de teste
mean(rf_pred==rose_test$credit.rating) 

# Confusion Matrix
confusionMatrix(rose_test$credit.rating, rf_pred, positive = '1')


## Criando o modelo com o algoritmo Árvore de Classificação
library(C50)
modelo_tree_v1 = C5.0(formula, data = rose_train) 

# Previsões nos dados de teste
tree_pred = predict(modelo_tree_v1, rose_test, type='class')

# Confusion Matrix
confusionMatrix(rose_test$credit.rating, tree_pred, positive = '1')

# Avaliando a curva ROC
library(ROCR)

#Curva roc para o modelo SVM
roc.curve(rose_test$credit.rating, pred_test_svm_v1, plotit = T, col = "red")

#Curva roc com o modelo SVM sem o balanceamento.
model_svm_v2 <- svm(formula, data = train, 
                    type = 'C-classification', kernel = 'radial') 

# Previsões nos dados de teste
pred_test_svm_v2 = predict(model_svm_v2, test)

# Confusion Matrix
confusionMatrix(test$credit.rating, pred_test_v2, positive = '1')

roc.curve(test$credit.rating, pred_test_v2, plotit = T, col = "green", add.roc = T)

# Analisando nosso modelo pela curva roc, percebemos que ele está bem se comporta melhor
# com o balanceamento, porém a accuracia dele piora significativamente. Vamos usar uma
# Random forest para ver quais variáveis são melhores para o nosso algoritmo.

# Modelo sem as variáveis do tipo int.
require(randomForest)
model_imp_var <- randomForest(credit.rating ~ . - credit.duration.months 
                              - credit.amount - age, data = df, ntree = 100,
                              nodesize = 10, importance = TRUE)

# Plotando as variáveis por grau de importância
varImpPlot(model_imp_var)

importance <- varImp(model_imp_var, scale = FALSE)
plot(importance)

# Vou testar o modelo com as 10 melhores variáveis com a medida de acurácia.
formula_v2 <- as.formula(paste('credit.rating ~ account.balance + fact.credit.duration.months + 
                previous.credit.payment.status + credit.purpose + fact.credit.amount +
                savings + installment.rate + marital.status + current.assets + apartment.type'))

# Criação do modelo SVM
modelo_svm_v3 <- svm(formula_v2, data = rose_train, 
                     type = 'C-classification', kernel = 'radial') 

# Previsões nos dados de teste
pred_test_v3 = predict(modelo_svm_v3, rose_test)

# Confusion Matrix
confusionMatrix(rose_test$credit.rating, pred_test_v3, positive = '1')

# Curva ROC
roc.curve(rose_test$credit.rating, pred_test_v3, plotit = T, col = "blue", add.roc = T)


# Agora farei o teste do modelo com as 15 melhores variáveis com a medida de acurácia.
formula_v3 <- as.formula(paste('credit.rating ~ account.balance +
                                fact.credit.duration.months + previous.credit.payment.status +
                                credit.purpose + fact.credit.amount + savings + 
                                installment.rate + marital.status + current.assets + 
                                apartment.type + guarantor + telephone + bank.credits +
                                fact.age + foreign.worker'))

# Criação do modelo SVM
modelo_svm_v4 <- svm(formula_v3, data = rose_train, 
                     type = 'C-classification', kernel = 'radial') 

# Previsões nos dados de teste
pred_test_v4 <- predict(modelo_svm_v4, rose_test)

# Confusion Matrix
confusionMatrix(rose_test$credit.rating, pred_test_v4, positive = '1')

# Curva ROC
roc.curve(rose_test$credit.rating, pred_test_v4, plotit = T, col = "yellow", add.roc = T)


credit.rating


- credit.duration.months 
- credit.amount - age

# Criação do modelo SVM
df[['credit.duration.months']] <- scale(df[['credit.duration.months']], center=T, scale=T)
df[['credit.amount']] <- scale(df[['credit.amount']], center=T, scale=T)
df[['age']] <- scale(df[['age']], center=T, scale=T)

modelo_svm_v5 <- svm(credit.rating ~ . , data = rose_train, 
                     type = 'C-classification', kernel = 'radial')

# Previsões nos dados de teste
pred_test_v5 <- predict(modelo_svm_v5, rose_test)

# Confusion Matrix
confusionMatrix(rose_test$credit.rating, pred_test_v5, positive = '1')

# Curva ROC
roc.curve(rose_test$credit.rating, pred_test_v5, plotit = T, col = "purple", add.roc = T)



modelo_svm_v6 <- svm(credit.rating ~ . -fact.credit.duration.months -fact.credit.amount
                     -fact.age, data = rose_train, 
                     type = 'C-classification', kernel = 'radial')

# Previsões nos dados de teste
pred_test_v6 <- predict(modelo_svm_v6, rose_test)

# Confusion Matrix
confusionMatrix(rose_test$credit.rating, pred_test_v6, positive = '1')

# Curva ROC
roc.curve(rose_test$credit.rating, pred_test_v6, plotit = T, col = "darkgreen", add.roc = T)


modelo_svm_v7 <- svm(formula_v1, data = train, 
                      type = 'C-classification', kernel = 'radial')
# Previsões nos dados de teste
pred_test_v7 <- predict(modelo_svm_v7, test)

# Confusion Matrix
confusionMatrix(test$credit.rating, pred_test_v7, positive = '1')

# Curva ROC
roc.curve(test$credit.rating, pred_test_v7, plotit = T, col = "black", add.roc = T)




formula.init <- "credit.rating ~ ."
formula.init <- as.formula(formula.init)
lr.model <- glm(formula = formula.init, data = train, family = "binomial")

lr.predictions <- predict(lr.model, test, type="response")

lr.predictions
lr.predictions <- round(lr.predictions)
lr.predictions
summary(lr.model)

confusionMatrix(test$credit.rating, lr.predictions, positive = '1')
confusionMatrix(table(data = lr.predictions, reference = test$credit.rating), positive = '1')

