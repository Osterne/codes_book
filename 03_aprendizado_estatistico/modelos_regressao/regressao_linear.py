# Script: Aplicacao de Regressao Linear em Python

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error, r2_score

# Gerar um dataset de exemplo
data = {
    'X': np.random.rand(100) * 10,
}
data['y'] = 3 * data['X'] + 7 + np.random.randn(100) * 2  # y = 3x + 7 + ruido

df = pd.DataFrame(data)

# Separar variáveis independentes (X) e dependente (y)
X = df[['X']]
y = df['y']

# Dividir em treino e teste
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Criar o modelo de regressão linear
model = LinearRegression()
model.fit(X_train, y_train)

# Fazer previsões
y_pred = model.predict(X_test)

# Avaliar o modelo
print("Coeficiente angular (coef_):", model.coef_[0])
print("Intercepto (intercept_):", model.intercept_)
print("Erro quadrático médio (MSE):", mean_squared_error(y_test, y_pred))
print("R² score:", r2_score(y_test, y_pred))

# Plotar resultados
plt.scatter(X_test, y_test, color='blue', label='Dados reais')
plt.plot(X_test, y_pred, color='red', linewidth=2, label='Previsões')
plt.title('Regressão Linear Simples')
plt.xlabel('X')
plt.ylabel('y')
plt.legend()
plt.grid(True)
plt.show()
