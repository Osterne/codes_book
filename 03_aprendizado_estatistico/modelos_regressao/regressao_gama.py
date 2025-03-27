# Script: Aplicacao de Regressao Gama em Python

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error, r2_score
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import make_pipeline
from sklearn.linear_model import GammaRegressor

# Gerar um dataset de exemplo com distribuição Gama
np.random.seed(42)
X = np.random.rand(200, 1) * 10
shape_param = 2.0
scale_param = 2.0 + 0.5 * X[:, 0]  # escala depende de X
y = np.random.gamma(shape=shape_param, scale=scale_param)

df = pd.DataFrame({'X': X[:, 0], 'y': y})

# Separar variáveis independentes (X) e dependente (y)
X = df[['X']]
y = df['y']

# Dividir em treino e teste
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Criar o modelo de regressão gama com padronização
model = make_pipeline(StandardScaler(), GammaRegressor(max_iter=10000))
model.fit(X_train, y_train)

# Fazer previsões
y_pred = model.predict(X_test)

# Avaliar o modelo
print("Erro quadrático médio (MSE):", mean_squared_error(y_test, y_pred))
print("R² score:", r2_score(y_test, y_pred))

# Visualizar os resultados
plt.scatter(X_test, y_test, color='blue', label='Dados reais')
plt.scatter(X_test, y_pred, color='red', label='Previsões', alpha=0.7)
plt.title('Regressão Gama')
plt.xlabel('X')
plt.ylabel('y')
plt.legend()
plt.grid(True)
plt.show()
