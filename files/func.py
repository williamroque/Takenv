# coding: utf-8
import matplotlib.pyplot as plt
import numpy as np
import os

def plot_lines(X, *Ys):
    fig, ax = plt.subplots()

    for i, Y in enumerate(Ys):
        ax.plot(X, Y, ['r','y','c','b','g','m','k','w'][i])

    ax.grid()
    fig.show()

def graph(*fs, bounds=(-10, 10), n=100):
    X = np.linspace(*bounds, n)
    plot_lines(X, *(np.vectorize(f)(X) for f in fs))

def dispov(mess):
    os.system(f'osascript ~/cbin/dispov.scpt {mess}')
