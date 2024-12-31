import pandas as pd
import time
import matplotlib.pyplot as plt


def partitions(data, n):
    part = int(len(pd.read_csv(data))) // n
    for k in range(0, part):
        bd = pd.read_csv(data, skiprows=(k)*n,  nrows=n)
        bd = bd.iloc[:, 0].tolist()
        yield bd


def sorting(data, ascending, iteracoes = 0):
    ini = 0
    fim = len(data)
    if len(data) <= 1:
        return data, iteracoes, data[0], data[0]
    meio = (ini+fim) // 2
    esquerda, iteracoes_esquerda, min_esquerda, max_esquerda = sorting(data[:meio], ascending)
    direita, iteracoes_direita, min_direita, max_direita = sorting(data[meio:], ascending)
    iteracoes += iteracoes_esquerda + iteracoes_direita
    minimo = min(min_esquerda, min_direita)
    maximo = max(max_esquerda, max_direita)

    if ascending == True:
        return merge_ascending(esquerda, direita, iteracoes, minimo, maximo)
    if ascending == False:
        return merge_decreasing(esquerda, direita, iteracoes, minimo, maximo)


def merge_ascending(esquerda, direita, iteracoes, minimo, maximo):
    lista_ordenada = []
    i = 0
    j = 0
    while i < len(esquerda) and j < len(direita):
        if esquerda[i] < direita[j]:
            lista_ordenada.append(esquerda[i])
            i += 1
        else:
            lista_ordenada.append(direita[j])
            j += 1
        iteracoes += 1
    lista_ordenada += esquerda[i:]
    lista_ordenada += direita[j:]
    minimo = min(minimo, lista_ordenada[0])
    maximo = max(maximo, lista_ordenada[-1])
    return lista_ordenada, iteracoes, minimo, maximo


def merge_decreasing(esquerda, direita, iteracoes, minimo, maximo):
    lista_ordenada = []
    i = 0
    j = 0
    while i < len(esquerda) and j < len(direita):
        if esquerda[i] > direita[j]:
            lista_ordenada.append(esquerda[i])
            i += 1
        else:
            lista_ordenada.append(direita[j])
            j += 1
        iteracoes += 1
    lista_ordenada += esquerda[i:]
    lista_ordenada += direita[j:]
    minimo = min(minimo, lista_ordenada[-1])
    maximo = max(maximo, lista_ordenada[0])
    return lista_ordenada, iteracoes, minimo, maximo


def execute(data, n, ascending=True):
    maximos = []
    minimos = []
    tempos = []
    iteracoes = []
    for p in partitions(data, n):
        ini = time.time()
        ord, itr, minimo, maximo = sorting(p, ascending)
        fim = time.time()
        tempo = fim - ini
        maximos.append(maximo)
        minimos.append(minimo)
        tempos.append(tempo)
        iteracoes.append(itr)
    resultados = pd.DataFrame({'Partições':[i for i in range(1, len(maximos)+1)],
                  'Máximo':maximos,
                  'Minimo':minimos,
                  'Tempo':tempos,
                  'Iterações': iteracoes})
    return resultados


def teste(data):
    valores = [100, 200, 500, 1000, 2000, 4000, 5000, 6250, 10000]
    tempos = []
    for n in valores:
        resultados = execute(data, n)
        media_tempos = sum(resultados.iloc[:, 3].tolist())
        tempos.append(media_tempos)

    plt.plot(valores, tempos, marker='o')
    plt.xlabel('Observações por partição')
    plt.ylabel('Tempo')
    plt.show()
