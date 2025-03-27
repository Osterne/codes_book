# Inferência Bayesiana
# Vinícius Osterne








# ----------------------------------------------------------------------------
# Cálculos de posteriores
# ----------------------------------------------------------------------------



# Carregar a biblioteca necessária
library(ggplot2)

# Definir a distribuição Beta para o prior Beta(2, 2)
prior_alpha <- 2
prior_beta <- 2

# Definir os dados observados: 7 sucessos e 3 fracassos
successes <- 3
failures <- 7

# Atualizar os parâmetros para a posterior Beta(9, 5)
posterior_alpha <- prior_alpha + successes
posterior_beta <- prior_beta + failures

# Gerar valores para o eixo x (probabilidade de sucesso)
x <- seq(0, 1, length.out = 100)

# Calcular as distribuições Beta (prior e posterior)
prior_density <- dbeta(x, prior_alpha, prior_beta)
posterior_density <- dbeta(x, posterior_alpha, posterior_beta)

# Criar um dataframe para plotar
df <- data.frame(
  x = rep(x, 2),
  density = c(prior_density, posterior_density),
  distribution = rep(c("Prior", "Posterior"), each = length(x))
)

# Plotar as distribuições
ggplot(df, aes(x = x, y = density, color = distribution)) +
  geom_line(size = 1.2) +
  labs(title = "Atualização de Crenças: Prior vs Posterior",
       x = "Probabilidade de Sucesso",
       y = "Densidade") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red")) +
  theme(legend.title = element_blank())






# ----------------------------------------------------------------------------
# Cálculos de posteriores
# ----------------------------------------------------------------------------


# Carregar pacotes necessários
library(ggplot2)

# Definir os parâmetros do prior Normal(mu = 0, sigma = 10)
mu_prior <- 0
sigma_prior <- 10

# Dados observados: média amostral e desvio padrão
sample_mean <- 5
sample_sd <- 2
sample_size <- 25

# Definir parâmetros para a distribuição normal a priori e verossimilhança
mu_likelihood <- sample_mean
sigma_likelihood <- sample_sd / sqrt(sample_size)

# Definir a distribuição posterior
# A fórmula para a posterior de uma normal com prior normal é outra normal com parâmetros atualizados
mu_posterior <- (mu_prior / sigma_prior^2 + mu_likelihood / sigma_likelihood^2) / (1 / sigma_prior^2 + 1 / sigma_likelihood^2)
sigma_posterior <- sqrt(1 / (1 / sigma_prior^2 + 1 / sigma_likelihood^2))

# Gerar valores para a distribuição posterior
p_values <- seq(-10, 20, by = 0.1)
posterior_values <- dnorm(p_values, mean = mu_posterior, sd = sigma_posterior)

# Gerar valores para o prior Normal(0, 10)
prior_values <- dnorm(p_values, mean = mu_prior, sd = sigma_prior)

# Criar um data frame para visualização
df <- data.frame(
  p = p_values,
  Prior = prior_values,
  Posterior = posterior_values
)

# Visualizar as distribuições
df_melted <- reshape2::melt(df, id = "p")
ggplot(df_melted, aes(x = p, y = value, color = variable)) +
  geom_line(size = 1) +
  labs(title = "Prior e Posterior para Média Normal", 
       x = "Média Estimada", y = "Densidade") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"), labels = c("Prior", "Posterior"))






# ----------------------------------------------------------------------------
# Modelos de mistura: Mistura de duas distribuições Normais
# ----------------------------------------------------------------------------


# Passo 1: Gerar dados simulados de uma mistura de duas distribuições Normais
set.seed(123)

# Parâmetros das distribuições Normais
mu1 <- 0   # Média da primeira distribuição
mu2 <- 5   # Média da segunda distribuição
sigma1 <- 1  # Desvio padrão da primeira distribuição
sigma2 <- 1.5  # Desvio padrão da segunda distribuição
p1 <- 0.6  # Proporção de dados da primeira distribuição
n <- 200  # Número total de dados

# Gerar dados da mistura
data1 <- rnorm(n * p1, mean = mu1, sd = sigma1)
data2 <- rnorm(n * (1 - p1), mean = mu2, sd = sigma2)
data <- c(data1, data2)

# Visualizar os dados gerados
hist(data, breaks = 30, probability = TRUE, col = rgb(0,0,1,0.5), main = "Mistura de duas distribuições Normais", xlab = "Valor", ylab = "Densidade")

# Passo 2: Especificar priors e likelihoods para os parâmetros
# Prior para as médias (normal com média 0 e desvio 10)
mu_prior <- rnorm(1000, 0, 10)

# Prior para os desvios padrões (uniforme entre 0 e 5)
sigma_prior <- runif(1000, 0, 5)

# Prior para a proporção da mistura (Beta(1, 1), o que é equivalente a uma distribuição uniforme)
p1_prior <- rbeta(1000, 1, 1)

# Passo 3: Calcular a posteriori de uma forma simples (ignorando normalização exata)
# Vamos usar uma abordagem simples baseada na fórmula Bayesiana para cada amostra
posterior_mu1 <- numeric(1000)
posterior_mu2 <- numeric(1000)
posterior_sigma1 <- numeric(1000)
posterior_sigma2 <- numeric(1000)
posterior_p1 <- numeric(1000)

for (i in 1:1000) {
  # Amostra de parâmetros do prior
  mu1_sample <- mu_prior[i]
  sigma1_sample <- sigma_prior[i]
  mu2_sample <- mu_prior[i] + 5  # Ajuste para ter mu2 maior
  sigma2_sample <- sigma_prior[i]
  p1_sample <- p1_prior[i]
  
  # Calcular a likelihood dos dados dados esses parâmetros
  likelihood1 <- prod(dnorm(data, mean = mu1_sample, sd = sigma1_sample))
  likelihood2 <- prod(dnorm(data, mean = mu2_sample, sd = sigma2_sample))
  
  # Calcular a posteriori simples com base na multiplicação do prior pela likelihood
  posterior_mu1[i] <- likelihood1 * dnorm(mu1_sample, 0, 10)
  posterior_mu2[i] <- likelihood2 * dnorm(mu2_sample, 0, 10)
  posterior_sigma1[i] <- likelihood1 * dunif(sigma1_sample, 0, 5)
  posterior_sigma2[i] <- likelihood2 * dunif(sigma2_sample, 0, 5)
  posterior_p1[i] <- likelihood1 * dbeta(p1_sample, 1, 1)
}

# Passo 4: Visualização dos resultados
par(mfrow = c(2, 2))

# Plot posterior para mu1
hist(posterior_mu1, main = "Posterior de mu1", xlab = "mu1", col = "lightblue", breaks = 30)

# Plot posterior para mu2
hist(posterior_mu2, main = "Posterior de mu2", xlab = "mu2", col = "lightblue", breaks = 30)

# Plot posterior para sigma1
hist(posterior_sigma1, main = "Posterior de sigma1", xlab = "sigma1", col = "lightblue", breaks = 30)

# Plot posterior para sigma2
hist(posterior_sigma2, main = "Posterior de sigma2", xlab = "sigma2", col = "lightblue", breaks = 30)

# Plot posterior para p1 (proporção da mistura)
hist(posterior_p1, main = "Posterior de p1", xlab = "p1", col = "lightblue", breaks = 30)

                  
                  
                  








# ----------------------------------------------------------------------------
# Modelos hierarquicos (usando algoritmo Metropolis-Hastings)
# ----------------------------------------------------------------------------


# Vamos supor que estamos interessados em modelar a altura média de pessoas em 
# diferentes escolas. A altura média de cada escola é uma variável aleatória
# com uma distribuição normal, onde as médias das escolas vêm de uma 
# distribuição normal global.

# Passo 1: Gerar dados simulados

set.seed(42)

# Parâmetros reais
true_mu <- 170  # Média global da altura
true_sigma <- 10  # Desvio padrão global
num_escolas <- 5  # Número de escolas
num_pessoas <- 20  # Número de pessoas por escola

# Gerando dados simulados para as alturas
escolas_mu <- rnorm(num_escolas, mean = true_mu, sd = true_sigma)  # Média das alturas por escola
escolas_sigma <- rep(5, num_escolas)  # Supondo que o desvio padrão das alturas dentro das escolas é fixo

# Gerando as alturas para cada escola
dados <- list()
for (i in 1:num_escolas) {
  dados[[i]] <- rnorm(num_pessoas, mean = escolas_mu[i], sd = escolas_sigma[i])
}

# Visualizando os dados
dados

# Passo 2: Definir o modelo hierárquico bayesiano

# O modelo tem a forma:
# - A média das alturas de cada escola segue uma distribuição normal com média global 'mu' e desvio padrão 'sigma_mu'.
# - As observações de altura dentro de cada escola seguem uma distribuição normal com média escolar 'mu_i' e desvio padrão 'sigma'.

# Passo 3: Implementar o algoritmo de Metropolis-Hastings

# Função de verossimilhança
log_verossimilhanca <- function(mu, sigma_mu, escolas_mu, dados) {
  log_likelihood <- 0
  for (i in 1:length(escolas_mu)) {
    # Likelihood para os parâmetros das escolas
    log_likelihood <- log_likelihood + sum(dnorm(dados[[i]], mean = escolas_mu[i], sd = 5, log = TRUE))
  }
  
  # Priori para os parâmetros
  log_likelihood <- log_likelihood + dnorm(mu, mean = 170, sd = 50, log = TRUE)  # Priori para a média global
  log_likelihood <- log_likelihood + dnorm(sigma_mu, mean = 10, sd = 5, log = TRUE)  # Priori para o sigma_mu
  
  return(log_likelihood)
}

# Função de Metropolis-Hastings
metropolis_hastings <- function(iterations, initial_mu, initial_sigma_mu, escolas_mu, dados) {
  # Inicialização dos parâmetros
  mu <- initial_mu
  sigma_mu <- initial_sigma_mu
  propostas_mu <- numeric(iterations)
  propostas_sigma_mu <- numeric(iterations)
  aceitos <- numeric(iterations)
  
  for (i in 1:iterations) {
    # Proposta para o mu e sigma_mu
    proposed_mu <- rnorm(1, mean = mu, sd = 1)
    proposed_sigma_mu <- rnorm(1, mean = sigma_mu, sd = 1)
    
    # Verossimilhanças
    log_current <- log_verossimilhanca(mu, sigma_mu, escolas_mu, dados)
    log_proposed <- log_verossimilhanca(proposed_mu, proposed_sigma_mu, escolas_mu, dados)
    
    # Critério de aceitação
    acceptance_ratio <- exp(log_proposed - log_current)
    
    if (runif(1) < acceptance_ratio) {
      mu <- proposed_mu
      sigma_mu <- proposed_sigma_mu
      aceitos[i] <- 1
    }
    
    propostas_mu[i] <- mu
    propostas_sigma_mu[i] <- sigma_mu
  }
  
  list(mu = propostas_mu, sigma_mu = propostas_sigma_mu, aceitos = aceitos)
}

# Passo 4: Rodar o algoritmo de Metropolis-Hastings

resultados <- metropolis_hastings(5000, initial_mu = 160, initial_sigma_mu = 8, escolas_mu = escolas_mu, dados = dados)

# Visualizando resultados
plot(resultados$mu, type = "l", col = "blue", main = "Cadeia de Markov para a média global (mu)", xlab = "Iteração", ylab = "mu")
plot(resultados$sigma_mu, type = "l", col = "red", main = "Cadeia de Markov para o desvio padrão global (sigma_mu)", xlab = "Iteração", ylab = "sigma_mu")

# Fazendo uma análise do valor final estimado para a média global e desvio padrão
mean(resultados$mu[2000:5000])  # Média posterior para mu
mean(resultados$sigma_mu[2000:5000])  # Média posterior para sigma_mu






# ----------------------------------------------------------------------------
# Modelos de mistura (usando algoritmo Metropolis-Hastings)
# ----------------------------------------------------------------------------

# Imaginemos que temos dados que vêm de uma mistura de duas distribuições 
# normais com diferentes médias e variâncias. A ideia é estimar os
# parâmetros dessa mistura (médias, variâncias e a proporção de cada
# componente na mistura) a partir dos dados, usando uma abordagem bayesiana.


set.seed(42)

# Passo 1: Gerar dados simulados

# Parâmetros reais da mistura de duas normais
mu1 <- 5
mu2 <- 10
sigma1 <- 2
sigma2 <- 3
pi <- 0.7  # Proporção do primeiro componente da mistura (0.7 para o primeiro e 0.3 para o segundo)

# Número de amostras
n <- 500

# Gerar os dados a partir da mistura de normais
z <- rbinom(n, 1, pi)  # Probabilidade de cada amostra pertencer ao primeiro componente
dados <- ifelse(z == 1, rnorm(n, mu1, sigma1), rnorm(n, mu2, sigma2))

# Visualizar os dados simulados
hist(dados, breaks = 30, main = "Histograma dos Dados Simulados", xlab = "Valor", col = "lightblue", border = "black")

# Passo 2: Definir o modelo bayesiano

# Os parâmetros que queremos estimar são: mu1, mu2, sigma1, sigma2, pi (proporção)
# Priori:
# - mu1, mu2 ~ normal(0, 10^2)
# - sigma1, sigma2 ~ uniform(0, 10)
# - pi ~ beta(1, 1) (distribuição uniforme no intervalo [0, 1])

# Função de verossimilhança
log_verossimilhanca <- function(mu1, mu2, sigma1, sigma2, pi, dados) {
  n <- length(dados)
  log_likelihood <- 0
  
  # Calculando a probabilidade de cada dado pertencer ao primeiro ou segundo componente
  for (i in 1:n) {
    p1 <- dnorm(dados[i], mean = mu1, sd = sigma1)
    p2 <- dnorm(dados[i], mean = mu2, sd = sigma2)
    log_likelihood <- log_likelihood + log(pi * p1 + (1 - pi) * p2)
  }
  
  # Priori para os parâmetros
  log_prior <- dnorm(mu1, mean = 0, sd = 10, log = TRUE) +
    dnorm(mu2, mean = 0, sd = 10, log = TRUE) +
    dunif(sigma1, min = 0, max = 10, log = TRUE) +
    dunif(sigma2, min = 0, max = 10, log = TRUE) +
    dbeta(pi, shape1 = 1, shape2 = 1, log = TRUE)
  
  return(log_likelihood + log_prior)
}

# Passo 3: Implementar o algoritmo de Metropolis-Hastings

# Função de Metropolis-Hastings
metropolis_hastings <- function(iterations, initial_params, dados) {
  # Inicializar os parâmetros
  mu1 <- initial_params[1]
  mu2 <- initial_params[2]
  sigma1 <- initial_params[3]
  sigma2 <- initial_params[4]
  pi <- initial_params[5]
  
  # Vetores para armazenar as amostras
  propostas_mu1 <- numeric(iterations)
  propostas_mu2 <- numeric(iterations)
  propostas_sigma1 <- numeric(iterations)
  propostas_sigma2 <- numeric(iterations)
  propostas_pi <- numeric(iterations)
  
  # Rodando o Metropolis-Hastings
  for (i in 1:iterations) {
    # Propostas para os parâmetros
    proposed_mu1 <- rnorm(1, mean = mu1, sd = 0.1)
    proposed_mu2 <- rnorm(1, mean = mu2, sd = 0.1)
    proposed_sigma1 <- runif(1, min = 0.1, max = 5)
    proposed_sigma2 <- runif(1, min = 0.1, max = 5)
    proposed_pi <- runif(1, min = 0, max = 1)
    
    # Verossimilhança atual e proposta
    log_current <- log_verossimilhanca(mu1, mu2, sigma1, sigma2, pi, dados)
    log_proposed <- log_verossimilhanca(proposed_mu1, proposed_mu2, proposed_sigma1, proposed_sigma2, proposed_pi, dados)
    
    # Critério de aceitação
    acceptance_ratio <- exp(log_proposed - log_current)
    
    if (runif(1) < acceptance_ratio) {
      # Aceitar a proposta
      mu1 <- proposed_mu1
      mu2 <- proposed_mu2
      sigma1 <- proposed_sigma1
      sigma2 <- proposed_sigma2
      pi <- proposed_pi
    }
    
    # Armazenar as amostras
    propostas_mu1[i] <- mu1
    propostas_mu2[i] <- mu2
    propostas_sigma1[i] <- sigma1
    propostas_sigma2[i] <- sigma2
    propostas_pi[i] <- pi
  }
  
  return(list(mu1 = propostas_mu1, mu2 = propostas_mu2, sigma1 = propostas_sigma1, sigma2 = propostas_sigma2, pi = propostas_pi))
}

# Passo 4: Rodar o algoritmo de Metropolis-Hastings

initial_params <- c(mu1 = 0, mu2 = 0, sigma1 = 1, sigma2 = 1, pi = 0.5)
resultados <- metropolis_hastings(5000, initial_params, dados)

# Passo 5: Visualizar os resultados

# Plotando as cadeias de Markov para os parâmetros estimados
par(mfrow = c(2, 3))

plot(resultados$mu1, type = "l", main = "Cadeia para mu1", col = "blue")
plot(resultados$mu2, type = "l", main = "Cadeia para mu2", col = "red")
plot(resultados$sigma1, type = "l", main = "Cadeia para sigma1", col = "green")
plot(resultados$sigma2, type = "l", main = "Cadeia para sigma2", col = "purple")
plot(resultados$pi, type = "l", main = "Cadeia para pi", col = "orange")

# Resumo das médias após iterações
mean(resultados$mu1[2000:5000])  # Média posterior para mu1
mean(resultados$mu2[2000:5000])  # Média posterior para mu2
mean(resultados$sigma1[2000:5000])  # Média posterior para sigma1
mean(resultados$sigma2[2000:5000])  # Média posterior para sigma2
mean(resultados$pi[2000:5000])  # Média posterior para pi






# ----------------------------------------------------------------------------
# Regressão bayesiana (usando algoritmo Metropolis-Hastings)
# ----------------------------------------------------------------------------

# Passo 1: Gerar Dados Simulados
set.seed(42)

# Parâmetros reais da regressão
beta_0_true <- 2    # Intercepto
beta_1_true <- 3    # Coeficiente angular
sigma_true <- 1     # Desvio padrão do erro

# Gerar dados
n <- 100            # Número de pontos
x <- rnorm(n, mean = 5, sd = 2)  # Valores de x
epsilon <- rnorm(n, mean = 0, sd = sigma_true)  # Erro
y <- beta_0_true + beta_1_true * x + epsilon  # Resposta y

# Visualizar os dados
plot(x, y, main = "Dados simulados de Regressão Linear", xlab = "x", ylab = "y", pch = 19, col = "blue")

# Passo 2: Definir o Modelo Bayesiano
# A verossimilhança dos dados dada os parâmetros é:

# p(y | X, beta_0, beta_1, sigma) = Normal(beta_0 + beta_1 * x, sigma^2)

# Priori:
# - Para beta_0 e beta_1, usaremos uma normal com média 0 e desvio padrão grande.
# - Para sigma, usamos uma distribuição invertida-gamma (pois é a distribuição típica para variâncias).

# Função de verossimilhança (log-verossimilhança)
log_verossimilhanca <- function(beta_0, beta_1, sigma, x, y) {
  n <- length(y)
  # Verossimilhança
  log_likelihood <- -n * log(sigma) - 0.5 * sum((y - beta_0 - beta_1 * x)^2) / (sigma^2)
  return(log_likelihood)
}

# Priori para os parâmetros (normal para beta_0 e beta_1, inversa-gamma para sigma^2)
log_prior <- function(beta_0, beta_1, sigma) {
  # Priori normal para beta_0 e beta_1 (média 0 e desvio padrão 10)
  prior_beta_0 <- dnorm(beta_0, mean = 0, sd = 10, log = TRUE)
  prior_beta_1 <- dnorm(beta_1, mean = 0, sd = 10, log = TRUE)
  # Priori inversa-gamma para sigma^2 (parâmetros a=2, b=1)
  prior_sigma <- dgamma(sigma^2, shape = 2, rate = 1, log = TRUE)
  return(prior_beta_0 + prior_beta_1 + prior_sigma)
}

# Função de Metropolis-Hastings
metropolis_hastings <- function(iterations, initial_params, x, y) {
  # Inicialização dos parâmetros
  beta_0 <- initial_params[1]
  beta_1 <- initial_params[2]
  sigma <- initial_params[3]
  
  # Vetores para armazenar as amostras
  propostas_beta_0 <- numeric(iterations)
  propostas_beta_1 <- numeric(iterations)
  propostas_sigma <- numeric(iterations)
  
  for (i in 1:iterations) {
    # Propostas para os parâmetros
    proposed_beta_0 <- rnorm(1, mean = beta_0, sd = 0.1)
    proposed_beta_1 <- rnorm(1, mean = beta_1, sd = 0.1)
    proposed_sigma <- runif(1, min = 0.1, max = 3)
    
    # Calcular a verossimilhança e prior
    log_current <- log_verossimilhanca(beta_0, beta_1, sigma, x, y) + log_prior(beta_0, beta_1, sigma)
    log_proposed <- log_verossimilhanca(proposed_beta_0, proposed_beta_1, proposed_sigma, x, y) + log_prior(proposed_beta_0, proposed_beta_1, proposed_sigma)
    
    # Critério de aceitação
    acceptance_ratio <- exp(log_proposed - log_current)
    
    if (runif(1) < acceptance_ratio) {
      # Aceitar a proposta
      beta_0 <- proposed_beta_0
      beta_1 <- proposed_beta_1
      sigma <- proposed_sigma
    }
    
    # Armazenar as amostras
    propostas_beta_0[i] <- beta_0
    propostas_beta_1[i] <- beta_1
    propostas_sigma[i] <- sigma
  }
  
  return(list(beta_0 = propostas_beta_0, beta_1 = propostas_beta_1, sigma = propostas_sigma))
}

# Passo 3: Rodar o algoritmo de Metropolis-Hastings
initial_params <- c(beta_0 = 0, beta_1 = 0, sigma = 1)  # Valores iniciais
resultados <- metropolis_hastings(5000, initial_params, x, y)

# Passo 4: Visualizar os resultados

# Visualizar as cadeias de Markov para os parâmetros
par(mfrow = c(2, 2))

plot(resultados$beta_0, type = "l", main = "Cadeia para beta_0", col = "blue")
plot(resultados$beta_1, type = "l", main = "Cadeia para beta_1", col = "red")
plot(resultados$sigma, type = "l", main = "Cadeia para sigma", col = "green")

# Resumo das médias após iterações
mean(resultados$beta_0[2000:5000])  # Média posterior para beta_0
mean(resultados$beta_1[2000:5000])  # Média posterior para beta_1
mean(resultados$sigma[2000:5000])   # Média posterior para sigma

# Visualizar a linha de regressão ajustada
beta_0_mean <- mean(resultados$beta_0[2000:5000])
beta_1_mean <- mean(resultados$beta_1[2000:5000])
y_pred <- beta_0_mean + beta_1_mean * x

# Plotar os dados e a linha de regressão ajustada
plot(x, y, pch = 19, col = "blue", main = "Regressão Linear (Inferência Bayesiana)", xlab = "x", ylab = "y")
lines(sort(x), y_pred[order(x)], col = "red", lwd = 2)





# ----------------------------------------------------------------------------
# Regressão bayesiana (usando Algoritmo de Gibbs)
# ----------------------------------------------------------------------------

set.seed(123)  # Para reprodutibilidade

# Gerando dados simulados
n <- 100
x <- rnorm(n)
y <- 3 + 2 * x + rnorm(n)

# Priors
beta_0_0 <- 0  # Prior para intercepto
beta_1_0 <- 0  # Prior para coeficiente de x
sigma_0 <- 1   # Prior para sigma (erro)
tau_0 <- 1     # Prior para o parâmetro de variação

# Inicializando os parâmetros
beta_0 <- 0
beta_1 <- 0
sigma2 <- 1

# Número de iterações do algoritmo de Gibbs
n_iter <- 1000

# Vetores para armazenar as amostras
beta_0_samples <- numeric(n_iter)
beta_1_samples <- numeric(n_iter)
sigma2_samples <- numeric(n_iter)

# Algoritmo de Gibbs
for (i in 1:n_iter) {
  
  # Passo 1: Amostra beta_0 | beta_1, sigma^2, x, y
  V_beta_0 <- 1 / (n / sigma2 + 1)
  m_beta_0 <- V_beta_0 * (sum(y - beta_1 * x) / sigma2)
  beta_0 <- rnorm(1, mean = m_beta_0, sd = sqrt(V_beta_0))
  
  # Passo 2: Amostra beta_1 | beta_0, sigma^2, x, y
  V_beta_1 <- 1 / (sum(x^2) / sigma2 + 1)
  m_beta_1 <- V_beta_1 * (sum((y - beta_0) * x) / sigma2)
  beta_1 <- rnorm(1, mean = m_beta_1, sd = sqrt(V_beta_1))
  
  # Passo 3: Amostra sigma^2 | beta_0, beta_1, x, y
  residuals <- y - beta_0 - beta_1 * x
  shape_sigma2 <- (n / 2) + 0.5
  rate_sigma2 <- (sum(residuals^2) / 2) + 0.5
  sigma2 <- 1 / rgamma(1, shape = shape_sigma2, rate = rate_sigma2)
  
  # Armazenando as amostras
  beta_0_samples[i] <- beta_0
  beta_1_samples[i] <- beta_1
  sigma2_samples[i] <- sigma2
}

# Exibindo resultados
par(mfrow = c(2, 2))

# Trace plots
plot(beta_0_samples, type = "l", main = "Trace plot for Beta_0", xlab = "Iteration", ylab = "Beta_0")
plot(beta_1_samples, type = "l", main = "Trace plot for Beta_1", xlab = "Iteration", ylab = "Beta_1")
plot(sigma2_samples, type = "l", main = "Trace plot for Sigma^2", xlab = "Iteration", ylab = "Sigma^2")

# Histograma das amostras
hist(beta_0_samples, main = "Posterior Distribution of Beta_0", xlab = "Beta_0", probability = TRUE)
hist(beta_1_samples, main = "Posterior Distribution of Beta_1", xlab = "Beta_1", probability = TRUE)
hist(sigma2_samples, main = "Posterior Distribution of Sigma^2", xlab = "Sigma^2", probability = TRUE)

# Mostrando médias das amostras (estimativas pós-Bayes)
cat("Média de Beta_0:", mean(beta_0_samples), "\n")
cat("Média de Beta_1:", mean(beta_1_samples), "\n")
cat("Média de Sigma^2:", mean(sigma2_samples), "\n")



























# ----------------------------------------------------------------------------
# Classificação bayesiana (usando algoritmo Metropolis-Hastings)
# ----------------------------------------------------------------------------

#Vamos construir um exemplo de inferência bayesiana em um modelo de classificação
# bayesiana usando um modelo simples de classificação binária com distribuição Bernoulli
# para as classes. O objetivo aqui é demonstrar como usar o Teorema de Bayes para
# classificar dados e estimar a probabilidade de uma classe com base em uma variável preditora.




# Passo 1: Gerar dados simulados
set.seed(42)

# Gerando dados simulados
n <- 100
x <- rnorm(n)  # Variável independente
beta_0_true <- 0.5  # Intercepto verdadeiro
beta_1_true <- 2    # Coeficiente verdadeiro

# Probabilidade de y = 1
logit_p <- beta_0_true + beta_1_true * x
p_y_given_x <- 1 / (1 + exp(-logit_p))

# Gerar valores de y com base em p_y_given_x
y <- rbinom(n, 1, p_y_given_x)

# Plotar os dados
plot(x, y, col = "blue", pch = 19, main = "Dados de Classificação Binária", xlab = "x", ylab = "y")

# Passo 2: Definir a função de verossimilhança (likelihood)
log_verossimilhanca <- function(beta_0, beta_1, x, y) {
  # Probabilidade de y = 1 dada x
  p_y_1_given_x <- 1 / (1 + exp(-(beta_0 + beta_1 * x)))
  # Log-verossimilhança para uma distribuição Bernoulli
  log_likelihood <- sum(y * log(p_y_1_given_x) + (1 - y) * log(1 - p_y_1_given_x))
  return(log_likelihood)
}

# Passo 3: Definir as distribuições a priori
log_prior <- function(beta_0, beta_1) {
  # Priori normal para beta_0 e beta_1
  prior_beta_0 <- dnorm(beta_0, mean = 0, sd = 10, log = TRUE)
  prior_beta_1 <- dnorm(beta_1, mean = 0, sd = 10, log = TRUE)
  return(prior_beta_0 + prior_beta_1)
}

# Passo 4: Algoritmo de Metropolis-Hastings para amostragem de parâmetros
metropolis_hastings <- function(iterations, initial_params, x, y) {
  # Inicializar os parâmetros
  beta_0 <- initial_params[1]
  beta_1 <- initial_params[2]
  
  # Vetores para armazenar as amostras
  propostas_beta_0 <- numeric(iterations)
  propostas_beta_1 <- numeric(iterations)
  
  for (i in 1:iterations) {
    # Proposta para os parâmetros
    proposed_beta_0 <- rnorm(1, mean = beta_0, sd = 0.1)
    proposed_beta_1 <- rnorm(1, mean = beta_1, sd = 0.1)
    
    # Calcular a verossimilhança e prior
    log_current <- log_verossimilhanca(beta_0, beta_1, x, y) + log_prior(beta_0, beta_1)
    log_proposed <- log_verossimilhanca(proposed_beta_0, proposed_beta_1, x, y) + log_prior(proposed_beta_0, proposed_beta_1)
    
    # Critério de aceitação
    acceptance_ratio <- exp(log_proposed - log_current)
    
    if (runif(1) < acceptance_ratio) {
      # Aceitar a proposta
      beta_0 <- proposed_beta_0
      beta_1 <- proposed_beta_1
    }
    
    # Armazenar as amostras
    propostas_beta_0[i] <- beta_0
    propostas_beta_1[i] <- beta_1
  }
  
  return(list(beta_0 = propostas_beta_0, beta_1 = propostas_beta_1))
}

# Passo 5: Rodar o algoritmo de Metropolis-Hastings
initial_params <- c(beta_0 = 0, beta_1 = 0)  # Valores iniciais para os parâmetros
resultados <- metropolis_hastings(5000, initial_params, x, y)

# Passo 6: Análise das amostras
# Visualizar as cadeias de Markov para os parâmetros
par(mfrow = c(2, 2))

plot(resultados$beta_0, type = "l", main = "Cadeia de Markov para beta_0", col = "blue")
plot(resultados$beta_1, type = "l", main = "Cadeia de Markov para beta_1", col = "red")

# Média posterior dos parâmetros após o burn-in
mean(resultados$beta_0[2000:5000])  # Média posterior para beta_0
mean(resultados$beta_1[2000:5000])  # Média posterior para beta_1

# Passo 7: Classificação com base nos parâmetros amostrados
# Vamos calcular a probabilidade de y = 1 para cada valor de x usando as médias dos parâmetros
beta_0_mean <- mean(resultados$beta_0[2000:5000])
beta_1_mean <- mean(resultados$beta_1[2000:5000])

# Calcular as probabilidades de classificação
probabilidades_y_1 <- 1 / (1 + exp(-(beta_0_mean + beta_1_mean * x)))

# Visualizar as probabilidades de y = 1
plot(x, probabilidades_y_1, type = "l", col = "green", main = "Probabilidade de y = 1 dado x", xlab = "x", ylab = "P(y = 1 | x)")
points(x, y, col = "blue", pch = 19)




# ----------------------------------------------------------------------------
# Classificação bayesiana (usando algoritmo de Gibbs)
# ----------------------------------------------------------------------------


set.seed(123)  # Para reprodutibilidade

# Gerando dados simulados para classificação binária
n <- 100
x <- rnorm(n)
y <- rbinom(n, 1, prob = 1 / (1 + exp(-(1 + 2 * x))))  # Usando uma função logística para gerar y

# Priors
beta_0_0 <- 0    # Prior para intercepto
beta_1_0 <- 0    # Prior para coeficiente de x
sigma_0 <- 1     # Prior para sigma
tau_0 <- 1       # Prior para o parâmetro de variação

# Inicializando os parâmetros
beta_0 <- 0
beta_1 <- 0

# Número de iterações do algoritmo de Gibbs
n_iter <- 1000

# Vetores para armazenar as amostras
beta_0_samples <- numeric(n_iter)
beta_1_samples <- numeric(n_iter)

# Algoritmo de Gibbs
for (i in 1:n_iter) {
  
  # Passo 1: Amostra beta_0 | beta_1, x, y
  V_beta_0 <- 1 / (n + 1)
  m_beta_0 <- V_beta_0 * sum(y - 1 / (1 + exp(-(beta_0 + beta_1 * x))))
  beta_0 <- rnorm(1, mean = m_beta_0, sd = sqrt(V_beta_0))
  
  # Passo 2: Amostra beta_1 | beta_0, x, y
  V_beta_1 <- 1 / (sum(x^2) + 1)
  m_beta_1 <- V_beta_1 * sum(x * (y - 1 / (1 + exp(-(beta_0 + beta_1 * x)))))
  beta_1 <- rnorm(1, mean = m_beta_1, sd = sqrt(V_beta_1))
  
  # Armazenando as amostras
  beta_0_samples[i] <- beta_0
  beta_1_samples[i] <- beta_1
}

# Exibindo resultados
par(mfrow = c(2, 2))

# Trace plots
plot(beta_0_samples, type = "l", main = "Trace plot for Beta_0", xlab = "Iteration", ylab = "Beta_0")
plot(beta_1_samples, type = "l", main = "Trace plot for Beta_1", xlab = "Iteration", ylab = "Beta_1")

# Histograma das amostras
hist(beta_0_samples, main = "Posterior Distribution of Beta_0", xlab = "Beta_0", probability = TRUE)
hist(beta_1_samples, main = "Posterior Distribution of Beta_1", xlab = "Beta_1", probability = TRUE)

# Estimativa dos parâmetros
cat("Média de Beta_0:", mean(beta_0_samples), "\n")
cat("Média de Beta_1:", mean(beta_1_samples), "\n")

# Calculando a probabilidade de cada ponto (posterior)
beta_0_post <- mean(beta_0_samples)
beta_1_post <- mean(beta_1_samples)
prob <- 1 / (1 + exp(-(beta_0_post + beta_1_post * x)))

# Classificando as observações
y_pred <- ifelse(prob > 0.5, 1, 0)

# Avaliando a precisão
accuracy <- mean(y == y_pred)
cat("Acurácia do modelo:", accuracy, "\n")









