# Script: Aplicacao de Regressao Logistica em Python

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import train_test_split
from sklearn.metrics import classification_report, confusion_matrix, accuracy_score

# Gerar um dataset de exemplo
np.random.seed(42)
X = np.random.rand(200, 1) * 10
y = (X[:, 0] > 5).astype(int)  # Classe 1 se X > 5, senão classe 0

df = pd.DataFrame({'X': X[:, 0], 'y': y})

# Separar variáveis independentes (X) e dependente (y)
X = df[['X']]
y = df['y']

# Dividir em treino e teste
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=42)

# Criar o modelo de regressão logística
model = LogisticRegression()
model.fit(X_train, y_train)

# Fazer previsões
y_pred = model.predict(X_test)

# Avaliar o modelo
print("Matriz de Confusão:\n", confusion_matrix(y_test, y_pred))
print("\nRelatório de Classificação:\n", classification_report(y_test, y_pred))
print("Acurácia:", accuracy_score(y_test, y_pred))

# Visualizar os resultados
plt.scatter(X_test, y_test, color='blue', label='Dados reais', alpha=0.5)
x_vals = np.linspace(0, 10, 300).reshape(-1, 1)
y_probs = model.predict_proba(x_vals)[:, 1]
plt.plot(x_vals, y_probs, color='red', label='Probabilidade prevista')
plt.title('Regressão Logística')
plt.xlabel('X')
plt.ylabel('Probabilidade de y=1')
plt.legend()
plt.grid(True)
plt.show()
