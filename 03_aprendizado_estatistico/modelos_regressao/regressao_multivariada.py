# Script: Aplicacao de Regressao Multivariada em Python

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error, r2_score

# Gerar um dataset de exemplo com 2 variáveis independentes e 2 dependentes
np.random.seed(42)
X1 = np.random.rand(200) * 10
X2 = np.random.rand(200) * 5
y1 = 3 * X1 + 2 * X2 + 5 + np.random.randn(200) * 2
y2 = -2 * X1 + 4 * X2 + 10 + np.random.randn(200) * 2

df = pd.DataFrame({'X1': X1, 'X2': X2, 'y1': y1, 'y2': y2})

# Separar variáveis independentes (X) e dependentes (Y)
X = df[['X1', 'X2']]
Y = df[['y1', 'y2']]

# Dividir em treino e teste
X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size=0.2, random_state=42)

# Criar o modelo de regressão multivariada
model = LinearRegression()
model.fit(X_train, Y_train)

# Fazer previsões
Y_pred = model.predict(X_test)

# Avaliar o modelo
print("Coeficientes:")
print(pd.DataFrame(model.coef_, columns=['X1', 'X2'], index=['y1', 'y2']))
print("\nInterceptos:", model.intercept_)

print("\nErro quadrático médio (MSE) para y1:", mean_squared_error(Y_test['y1'], Y_pred[:, 0]))
print("Erro quadrático médio (MSE) para y2:", mean_squared_error(Y_test['y2'], Y_pred[:, 1]))

print("\nR² score para y1:", r2_score(Y_test['y1'], Y_pred[:, 0]))
print("R² score para y2:", r2_score(Y_test['y2'], Y_pred[:, 1]))

# Visualizar os resultados para cada variável dependente
plt.figure(figsize=(12, 5))

plt.subplot(1, 2, 1)
plt.scatter(Y_test['y1'], Y_pred[:, 0], color='purple', alpha=0.6)
plt.xlabel('y1 real')
plt.ylabel('y1 previsto')
plt.title('y1: Real vs Previsto')
plt.grid(True)

plt.subplot(1, 2, 2)
plt.scatter(Y_test['y2'], Y_pred[:, 1], color='green', alpha=0.6)
plt.xlabel('y2 real')
plt.ylabel('y2 previsto')
plt.title('y2: Real vs Previsto')
plt.grid(True)

plt.tight_layout()
plt.show()
