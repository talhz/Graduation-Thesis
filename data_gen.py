import numpy as np
import matplotlib.pyplot as plt

epsilon = 1e-6
# np.random.seed(1982)

# generate data (change point mode), user needs to configure structural equations

# x1->x2->y

# two change points
n = 300
t1 = 100
t2 = 200
x1 = []
x2 = []
y = []
for t in range(t1):
    xt1 = np.random.normal(0, 1)
    xt2 = 2 * xt1 + np.random.normal(0, 1)
    y.append(-3 * xt2 + np.random.normal(0, 1))
    x1.append(xt1)
    x2.append(xt2)
for t in range(t1, t2): # intervene on x1
    x1.append(2)
    xt2 = 2 * xt1 + np.random.normal(0, 1)
    y.append(-3 * xt2 + np.random.normal(0, 1))
    x2.append(xt2)
for t in range(t2, n): #intervene on x2
    x1.append(np.random.normal(0, 1))
    xt2 = np.random.normal(0, 1)
    y.append(-3 * xt2 + np.random.normal(0, 1))
    x2.append(xt2)

d1 = np.array([y, x1, x2])
np.save('data/data1', d1)