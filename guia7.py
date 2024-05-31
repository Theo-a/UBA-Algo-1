def pertence (ls:list, e:int) -> bool:
    if len(ls) == 0:
        return False
    for i in range(0,len(ls)):
        if ls[i] == e:
            return True
    return False
def divide_a_todos(ls:list,e:int)->bool:
    for i in range(0,len(ls)):
        if ls[i] % e != 0:
            return False
        else:
            return True
def suma_total(ls:list)->int:
    x = 0
    for i in range(0,len(ls)):
        x = x + ls[i]
    return x
def ordenados(ls:list)->bool:
    for i in range(1,len(ls)):
        if ls[i] >= ls[i+1]:
            return False
        else:
            return True
#print(ordenados([1,2,3,7,10]))
def palabras7(ls:list)->bool:
    for i in range(0,len(ls)):
        if len(ls[i]) > 7:
            return True
        else:
            return False
def b(ls:list)->list:
    for i in range(1,len(ls)):
        if (i-1) % 2 == 0:
            ls[i] = 0
    return ls
#print(b([1,2,3,4]))
def a(ls:list)->list:
    x = ls
    for i in range(1,len(ls)):
        if (i-1) % 2 == 0:
            x[i] = 0
        else:
            x[i] = ls[i]
    return x
#print(a([1,2,3,4]))
def c(s:list)->list:
    x = s
    for i in range(0,len(s)):
        if s[i] == 'a' or s[i] == 'e' or s[i] == 'i' or s[i] == 'o' or s[i] == 'u':
            x[i] = s[i+1]
    return x
def dar_vuelta_str(s:list)->list:
    x = s
    for i in range(0,len(s)):
        x[i] = s[len(s)-1-i]
    return x
#print(dar_vuelta_str(['a','b','c']))
#no funciona

def eliminar_reps(ls:list)->list:
    for i in range(0,len(ls)):
        if pertence(ls,ls[i]):
            return False
        else:
            return True
def aprobados(ls:list)->int:
    if aux_aprobados(ls) and (promdedio(ls) >= 7):
        return 1
    if aux_aprobados(ls) and (promdedio(ls) < 7) and (promdedio(ls) >=4 ):
        return 2
    else:
        return 3
def aux_aprobados(ls:list)->bool:
    for i in range(0,len(ls)):
        if ls[i] < 4:
            return False
        else:
            return True
def promdedio(ls:list)->int:
    x = 0
    for i in range(0,len(ls)):
        x = x + ls[i]
    return x

def d(x:str)->list:
    ls = []
    while x != "listo":
        ls.append(x)
        return input("enter name ")
    return ls
y = input("enter name ")
print(d(y))
#mal


def pertenece_a_cada_uno_version_1 (ls:list,e:int)->list:
    x = []
    for i in ls:
        x.append(pertence(i,e))
    return x
#print(pertenece_a_cada_uno_version_1([[1,2],[3,2]],2))

def es_matriz(ls:list)->bool:
    #si todas las listas tienen el mismo len, ls es lista de listas
    for i in range(0,len(ls)):
        if len(ls[i]) != len(ls[0]):
            return False
    return True
def filas_ordenadas(ls:list)->list:
    x = []
    for i in ls:
        x.append(ordenados(i))
    return x
#print(filas_ordenadas([[1,2,3],[3,4,1]]))
import numpy as np
x = x = np.random.random((2,2))
y = x = np.random.random((2,2))
#print(x)
#print(y)
def generar_matriz(d:int,p:int)->list:
    x = np.random.random((d,d))
    xs = x
    for i in range(0,p):
        xs = prod_matiz(xs,x)
    return xs
def prod_matiz(x:list,y:list)->list:
    xs = x
    for i in range(0,len(x)):
        for j in range(0,len(y)):
            for k in range(0,len(y)):
                xs[i,j] = x[i,k]*y[k,j]
    return xs
#print(prod_matiz(x,y))
#print(generar_matriz(4,3))
