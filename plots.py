# Generate data according to switching regression model
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

np.random.seed(1111)

t = 3000
t1 = 1000
t2 = 2000

x1 = []
x2 = []
x3 = []
y = []
h = []
for i in range(t1):
    u = np.random.randint(1, 3)
    h.append(u)
    x3t = np.random.normal(0, 1)
    x3.append(x3t)
    x1t = np.random.normal(0, 1)
    x1.append(x1t)
    x2.append(3 * x1t + x3t + np.random.normal(0, 1))
    if u == 1:
        y.append(1 + 2 * x1t + np.random.normal(0, 1))
    else:
        y.append(-2 - x1t + np.random.normal(0, 1))

for i in range(t1, t2):
    u = np.random.randint(1, 3)
    h.append(u)
    x3.append(np.random.normal(0, 1))
    x2.append(np.random.normal(0, 1))
    x1t = np.random.normal(0, 1)
    x1.append(x1t)
    if u == 1:
        y.append(1 + 2 * x1t + np.random.normal(0, 1))
    else:
        y.append(-2 - x1t + np.random.normal(0, 1))

for i in range(t2, t):
    u = np.random.randint(1, 3)
    h.append(u)
    x3t = np.random.normal(0, 1)
    x3.append(x3t)
    x1t = np.random.normal(5, 2)
    x1.append(x1t)
    x2.append(3 * x1t + x3t + np.random.normal(0, 1))
    if u == 1:
        y.append(1 + 2 * x1t + np.random.normal(0, 1))
    else:
        y.append(-2 - x1t + np.random.normal(0, 1))
h = np.array(h)
x1 = np.array(x1)
x2 = np.array(x2)
x3 = np.array(x3)
y = np.array(y)

np.save('data/data_hidden', np.array([y, x1, x2, x3]))

# d1 = {'y': y, 'x1': x1, 'x2': x2, 'x3': x3, 'h': h}
# df = pd.DataFrame(data=d1)
# df.to_csv('data/data_hidden.csv')
# if __name__ == '__main__':
#     plt.plot(x3[:t][h[:t] == 1], y[:t][h[:t] == 1], 'bo')
#     plt.plot(x3[:t][h[:t] == 2], y[:t][h[:t] == 2], 'ro')
#     plt.show()
#     plt.plot(x3[t1:t2][h[t1:t2] == 1], y[t1:t2][h[t1:t2] == 1], 'bo')
#     plt.plot(x3[t1:t2][h[t1:t2] == 2], y[t1:t2][h[t1:t2] == 2], 'ro')
#     plt.show()