# Script: Aplicacao de Regressao Linear Multipla em Python

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error, r2_score

# Gerar um dataset de exemplo com duas variáveis independentes
np.random.seed(42)
X1 = np.random.rand(200) * 10
X2 = np.random.rand(200) * 5
y = 4 * X1 + 2 * X2 + 10 + np.random.randn(200) * 3  # y = 4*X1 + 2*X2 + 10 + ruido

df = pd.DataFrame({'X1': X1, 'X2': X2, 'y': y})

# Separar variáveis independentes (X) e dependente (y)
X = df[['X1', 'X2']]
y = df['y']

# Dividir em treino e teste
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Criar o modelo de regressão linear múltipla
model = LinearRegression()
model.fit(X_train, y_train)

# Fazer previsões
y_pred = model.predict(X_test)

# Avaliar o modelo
print("Coeficientes:", model.coef_)
print("Intercepto:", model.intercept_)
print("Erro quadrático médio (MSE):", mean_squared_error(y_test, y_pred))
print("R² score:", r2_score(y_test, y_pred))

# Visualizar os resultados com gráfico 2D para cada variável
plt.figure(figsize=(12, 5))

plt.subplot(1, 2, 1)
plt.scatter(X_test['X1'], y_test, color='blue', label='Dados reais')
plt.scatter(X_test['X1'], y_pred, color='red', label='Previsões', alpha=0.6)
plt.xlabel('X1')
plt.ylabel('y')
plt.title('Relação entre X1 e y')
plt.legend()
plt.grid(True)

plt.subplot(1, 2, 2)
plt.scatter(X_test['X2'], y_test, color='blue', label='Dados reais')
plt.scatter(X_test['X2'], y_pred, color='red', label='Previsões', alpha=0.6)
plt.xlabel('X2')
plt.ylabel('y')
plt.title('Relação entre X2 e y')
plt.legend()
plt.grid(True)

plt.tight_layout()
plt.show()
