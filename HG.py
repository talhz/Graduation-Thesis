import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

epsilon = 1e-6
np.random.seed(1023)

# generate data (change point mode), user needs to configure structural equations

# x1->x2->y

# two change points
n = 3000
t1 = 1000
t2 = 2000
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
    xt2 = np.random.normal(0, 5)
    y.append(-3 * xt2 + np.random.normal(0, 1))
    x2.append(xt2)

# d1 = np.array([y, x1, x2])
# d1 = {'y': y, 'x1': x1, 'x2': x2}
# df = pd.DataFrame(data=d1)
# df.to_csv('data/data2.csv')
# print(x1)

d1 = np.load('data/data_hidden.npy')



def single_normal_density(x,mean,variance):
    return -1/2*np.log(2*np.pi*variance+epsilon)-1/2/(variance+epsilon)*(x-mean)*(x-mean)

def multi_normal_density(x,mean,variance):
    return -len(x)/2*np.log(2*np.pi)-np.log(np.linalg.det(variance)+epsilon)/2-1/2*((x-mean)@np.linalg.inv(variance+np.diag([epsilon]*variance.shape[0]))@(x-mean))


# HG
# data[array]: [y, x1, ..., xp]
# y[array]: y[t] is scalar
# xi[array]: xi[t] is scalar
# pi[array]: pi[k] is scalar
# a[array]: a[k][i] is scalar
# sigma[array]: sigma[k][i][j] is scalar
# ssquare[array]: ssquare[k] is scalar
# b[array]: b[k][d] is scalar
# regu[scalar]: the regularizer(lambda)
# simutime[scalar]: epoches
# K[scalar]: optional number of environments
def HG(data, simutime, K, regu=10):
    n = len(data[0]) # number of observations
    num = len(data) # number of predictors plus one
    prob = [[0 for i in range(K)] for t in range(n)] # prob[array]: prob[t][k] is scalar
    pi = np.array([1 / K for k in range(K)])
    a = np.array([[1 for d in range(num - 1)] for k in range(K)])
    sigma = np.array([np.diag([1 for i in range(num - 1)]) for k in range(K)])
    ssquare = np.array([1 for k in range(K)])
    b = np.array([[1 for d in range(num - 1)] for k in range(K)])
    slope = (0.05 - regu) / simutime # regularizer decreases linearly
    x = data[1:]
    y = data[0]
    
    for time in range(simutime):
        print('%d th simulation' % time)
        print('b', pi)
        # update prob -- soft clustering
        for t in range(n):
            element = [] # store summation terms
            for k in range(K):
                element.append((pi[k] * (np.exp(multi_normal_density(x[:, t], a[k], sigma[k])) + epsilon) * \
                               (np.exp(single_normal_density(y[t], np.transpose(x[:, t])@b[k], ssquare[k])) + epsilon))**(1 / regu))
            denomi = np.sum(element)
            for k in range(K):
                # print(element[k])
                prob[t][k] = element[k] / (denomi + epsilon)
        # print(prob[1][2])
        # update pi
        # print(np.sum(prob, axis=1))
        # return
        for k in range(K):
            pi[k] = np.sum(prob, axis=0)[k] / n
        # update a
        for k in range(K):
            numer = 0
            for t in range(n):
                numer += prob[t][k] * x[:, t]
            a[k] = numer / np.sum(prob, axis=0)[k]
        # update sigma
        for k in range(K):
            numer = 0 # store the numerator in eq.(5.6)
            for t in range(n):
                numer += prob[t][k] * np.outer((x[:, t] - a[k]), (x[:, t] - a[k]).T)
            sigma[k] = numer / np.sum(prob, axis=0)[k]
        # update b
        for k in range(K):
            left = 0 # store the left part of eq.(5.8)
            right = 0 # store the right part of eq.(5.8)
            for t in range(n):
                left += prob[t][k] * np.outer(x[:, t], x[:, t].T)
                right += prob[t][k] * y[t] * x[:, t]
            b[k] = np.linalg.inv(left + epsilon)@right
        # update ssquare
        for k in range(K):
            numer = 0 # store the numerator of eq.(5.7)
            for t in range(n):
                numer += prob[t][k] * (y[t] - np.transpose(x[:, t])@b[k])**2
            ssquare[k] = numer / (np.sum(prob, axis=0)[k] + epsilon)
        # update lambda 
        regu += slope
    return sigma

if __name__ == '__main__':
    print(HG(d1, 10, 3))
        