from typing import List
from typing import Dict
#ejercicio 1.1
def contar_lineas(x:str)->int:
    file = open(x,'r')
    lineas_cant:int = len(file.readlines())
    file.close()
    return lineas_cant

#ejercicio 1.2
def pertenece_palabra(palabra: str, texto: str) -> bool:
    i:int = 0 
    while i < len(texto):
        if texto[i] == palabra[0]:
            if consecutivos_iguales(palabra, texto, i):
                return True
        i += 1
    return False

def consecutivos_iguales(palabra: str, texto: str, indice: int)->bool:
    k:int= 0
    while k < len(palabra):
        if indice + len(palabra) > len (texto):
            return False
        if palabra[k] != texto[indice+k]:
            return False
        k += 1
    return True

def existe_palabra(palabra: str, texto: str) -> bool:
    archivo = open(texto , "r")
    contenido: str = archivo.read()
    archivo.close()
    return pertenece_palabra_aux(palabra, contenido)

#ejercicio 1.3
def cantidad_aprariciones_aux(texto:str,palabra:str)->int:
    i:int = 0
    repeticiones:int = 0
    while i < len(texto):
        if texto[i] == palabra[0]:
            if consecutivos_iguales(palabra, texto, i):
                repeticiones = repeticiones + 1
        i += 1
    return repeticiones

def cantidad_aprariciones(palabra: str, texto: str) -> bool:
    archivo = open(texto , "r")
    contenido: str = archivo.read()
    archivo.close()
    return cantidad_aprariciones_aux(contenido, palabra)

# ejercicio 2
def clonar_sin_comentarios(texto:str):
    file = open(texto,'r+')
    clon = open("clon.txt",'w')
    for i in file.readlines():
        if not aux_clonar_sin_comentarios(i):
            clon.write(i) #puede ser con .write(i + "\n")
    clon.close()
    file.close()
            
def aux_clonar_sin_comentarios(linea:str)->bool:
    i = 0
    while i <= len(linea):
        if linea[i] == "#":
            return True
        elif linea[i] == " ":
            i = i + 1
        else:
            return False
    return False

# ejercicio 3
def invertir_lineas(archivo: str):
    file = open(archivo, "r")
    nuevo_archivo = open("reverso.txt", "w")
    lineas: list[str] = file.readlines()
    lineas_invertidas: list[str] = []
    for i in range(-1, -len(lineas) - 1, -1):
        lineas_invertidas.append(lineas[i])
    for j in lineas_invertidas:
        if "\n" in j:
            nuevo_archivo.write(j)
        else:
            nuevo_archivo.write(j + "\n")
    nuevo_archivo.close()

#ejercicio 4
def agregar_frase_al_final(texto:str,frase:str):
    archivo = open(texto , "a")  #usar "a" y luego .write(frase) es un append
    archivo.write("\n" + frase)
    archivo.close()

#ejercicio 5
def agregar_frase_al_principio(texto:str,frase:str):
    file = open(texto,"r")
    y = file.readlines()
    y[0] = frase + "\n" + y[0]
    file = open(texto,"w")
    file.writelines(y)
    file.close()

#ejercicio 6
def listar_palabras_de_archivo(texto:str)->List[str]:
    file = open(texto,"rb") #read and binary
    file_bytes = file.read()
    
    res:List[str] = []
    palabra_legible:str = ""

    for byte in file_bytes:
        caracter = chr(byte)
        if es_leible(caracter):
            palabra_legible += caracter
        else:
            if len(palabra_legible) >= 5:
                res.append(palabra_legible)
            palabra_legible = ""
    file.close()
    return res

def es_legible(caracter:str)->bool:
    res:bool = False
    if (caracter >= '0'and caracter <= '9') or (caracter >= 'a' and caracter <= 'z') or (caracter >= 'A' and caracter <= 'Z') or caracter == ' ' or caracter == '_':
        res:bool = True
    return res

#ejercicio 7
def calcular_promedio_por_estudiante(alumno_archivo_notas:str,alumno_archivo_promedio:str):
    archivo = open(alumno_archivo_notas, "r")
    lineas: List[str] = archivo.readlines()
    archivo.close()
    parciales: List[List[str]] = []
    #una linea tiene la pinta: nro de LU(str), materia (str), fecha (str), nota (float)
    for linea in lineas:
        info : List[str] = pasar_csv_a_lista(linea)
        nota = info[3]
        info[3] = sacar_salto_de_linea(nota)
        parciales.append(info)
    #un elem de parciales es: [nro de LU, materia, fecha, nota (sin "\n")]

    alumnos: List[str] = []
    for info_parcial in parciales:
        if not pertenece(alumnos, info_parcial[0]):
            alumnos.append(info_parcial[0])

    promedios = open(alumno_archivo_promedio , "w")
    for alumno in alumnos:
        suma_de_notas: int = 0
        cantidad_de_notas : int = 0
        for info_parcial in parciales:
            if info_parcial[0] == alumno:
                suma_de_notas += float(info_parcial[3]) #creo que "nota" es str
                cantidad_de_notas += 1
        promedio: int = suma_de_notas / cantidad_de_notas
        promedios.write("Alumno; " + alumno + "," + " Promedio:" + str(promedio) + "\n")
    promedios.close() 

def pasar_csv_a_lista(s:str) -> List[str]:
    lista: List[str] = []
    palabra = ""
    for i in range(len(s)):
        if s[i] != "," :
            palabra = palabra + s[i]
        else:
            if palabra != "":
                lista.append(palabra)
            palabra = ""
    if palabra != "":
        lista.append(palabra)
    return lista

def pertenece(lista: List[str], elemento: str) -> bool:
    for i in range(len(lista)):
        if lista[i] == elemento:
            return True
    return False

def sacar_salto_de_linea(string: str) -> str:
    res: str = ""
    if pertenece_palabra(string, "\n"):
        for i in range(len(string)):
            if string[i] != "\n" :
                res += string[i]
            if string[i] == "\n" :
                return res
    else: 
        return string
    
#ejercicio 8
from queue import LifoQueue as Pila
import random
def generar_nros_al_azar(cantidad:int, desde:int, hasta:int)->Pila:
    p:Pila[int] = Pila()
    for i in range(cantidad):
        p.put(random.randint(desde,hasta))
    return p
"""
d = generar_nros_al_azar(10,2,6)
for i in range(d.qsize()):
    print(d.get())


obs: 0, "", [] son False    
"""

#ejercicio 9
def cantidad_elementos(pila:Pila)->int:
    desapilar:Pila = Pila()
    i:int = 0
    while not pila.empty():
        desapilar.put(pila.get())
        i += 1
    while not desapilar.empty():
        pila.put(desapilar.get())
    return i
"""
d = generar_nros_al_azar(10,2,6)
print(cantidad_elementos(d))
for i in range(d.qsize()):
    print(d.get())
funciona bien
"""
#ejercicio 10
def buscar_el_maximo(pila:Pila)->Pila:
    while not pila.empty():
        max = pila.get()
        if max < pila.get():
            max = pila.get()
    return max

"""
d = generar_nros_al_azar(10,2,6)
e = d
print(buscar_el_maximo(d))
for i in range(e.qsize()):
    print(e.get())        
no funciona (?)
"""

#ejercicio 11
def esta_bien_balanceada (s: str) -> bool:
    p_abiertos = Pila()
    val: bool = True
    i: int = 0
    while i <len(s):
        if s[i] == "(":
            p_abiertos.put(1)
        if s[i] == ")":
            if not p_abiertos.empty(): 
                p_abiertos.get()
            else: val = False
        i += 1
    return val and p_abiertos.empty()

#ejercicio 12
"""
def evaluar_expresion0(s:str)->float:
    pila = Pila()
    for i in range(-1,-len(s)-1,-1):
        if s[i] != " ":
            pila.put(s[i])
    pila_aux:Pila = Pila()
    while not pila.empty():
        if not (pila.get() in "+-*/"):
            pila_aux.put(pila.get())
        else:
            if pila.get() == "+":
                pila_aux.put(pila_aux.get() + pila_aux.get())
            if pila.get() == "-":
                pila_aux.put(-(pila_aux.get() - pila_aux.get()))
            if pila.get() == "*":
                pila_aux.put(pila_aux.get() * pila_aux.get())
            if pila.get() == "/":
                e1 = pila_aux.get()
                e2 = pila_aux.get()
                pila_aux.put(e2 / e1)
    return pila_aux.get()
no se porque no funciona
"""
def evaluar_expresion(string : str) -> float:
    numeros: Pila = Pila()
    s = dividir_en_espacios(string)
    for i in range(len(s)):
        if s[i] != "+" and s[i] != "-" and s[i] != "*" and s[i] != "/":
            numeros.put(int(s[i]))
        else: 
            if s[i] == "+":
                numeros.put(numeros.get() + numeros.get())
            elif s[i] == "-":
                numeros.put(-(numeros.get() - numeros.get()))
            elif s[i] == "*":
                numeros.put(numeros.get() * numeros.get())
            elif s[i] == "/":
                primero: int = numeros.get()
                segundo: int = numeros.get()
                numeros.put(segundo / primero)
    return numeros.get()

def dividir_en_espacios(s:str) -> List[str]:
    lista: list[str] = []
    palabra = ""
    for i in range(len(s)):
        if s[i] != " ":
            palabra = palabra + s[i]
        else:
            if palabra != "":  #esto evita meter espacios como palabras, si estan al principio
                lista.append(palabra)
            palabra = ""
    if palabra != "":
        lista.append(palabra)
    return lista
#divir en espacios es necesario para numeros de mas de 2 digitos

#s = " 3 4 + 5 * 2 -"
#res = evaluar_expresion(s)
#print(res)

#ejercicio 13
from queue import Queue as Cola
def armar_cola(principio:int, desde:int, hasta:int) -> Cola:
    x = generar_nros_al_azar(principio,desde,hasta)
    cola = Cola()
    while not x.empty():
        cola.put(x.get())
    return cola

#ejercicio 14
def cantidad_elementos(cola:Cola)->int:
    i : int = 0
    while not cola.empty():
        cola.get()
        i += 1
    return i

#ejercicio 15
def buscar_el_maximo(cola:Cola)->int:
    while not cola.empty():
        max = cola.get()
        if max < cola.get():
            max = cola.get()
    return max

#ejercicio 16
def armar_secuencia_bingo()->Cola:
    lista = []
    cola = Cola()
    i = 0
    while i <= 99:
        j = random.randint(0,99)
        if not(j in lista):
            lista.append(j)
            cola.put(j)
            i += 1
    return cola
def jugar_carton_de_bingo(carton:List[int],bolillero:Cola)->int:
    cola:Cola = Cola()
    i:int = 0
    tiradas:int = 0
    while i < 12:
        if bolillero.get() in carton:
            i += 1
            tiradas += 1
        else:
            bolillero.get()
            tiradas += 1
    return tiradas

#ejercicio 17
def  n_pacientes_urgentes(pacientes:Cola)->int:
    p_urgentes:int = 0
    while not pacientes.empty():
        if str(pacientes.get()[0]) in "123":
            p_urgentes += 1
    return p_urgentes

#ejercicio 18
def atencion_a_clientes(clientes:Cola)->Cola:
    clientes_prioridad:Cola = Cola()
    clientes_preferencial:Cola = Cola()
    clientes_resto:Cola = Cola()
    # un elem de clientes es: (str,int,bool,bool)
    while not clientes.empty():
        #este caso puede estar de mas, consigna ambigua
        if clientes.get()[3] and clientes.get()[2]:
            clientes_prioridad.put(clientes.get())
        elif clientes.get()[3]:
            clientes_prioridad.put(clientes.get())
        elif clientes.get()[2]:
            clientes_preferencial.put(clientes.get())
        else:
            clientes_resto.put(clientes.get())
    while not clientes_preferencial.empty():
        clientes_prioridad.put(clientes_preferencial.get())
    while not clientes_resto.empty():
        clientes_prioridad.put(clientes_resto.get())
    return clientes_prioridad

#ejercicio 19
def agrupar_por_longitud(archivo:str)->dict:
    #file = open(archivo,"r")
    #texto = file.read()
    #file.close()
    texto = archivo
    palabras = dividir_en_espacios(texto)
    res = {}
    for i in palabras:
        if not(len(i) in res):
            res[len(i)] = 1
        else:
            res[len(i)] += 1
    return res

#ejercicio 20
def calcular_promedio_por_estudiante(archivo:str)->dict:
    #una linea del archivo es: nro de LU (str), materia (str), fecha (str), nota (float)
    file = open(archivo,"r")
    texto = file.readlines()
    file.close()
    texto2:list[list[str]] = []
    for j in texto:
        texto2.append(pasar_csv_a_lista(j))
    notas = {}
    for k in texto2:
        if not(k[0] in notas):
            notas[k[0]] = [float(k[3])]
        else:
            notas[k[0]].append(float(k[3]))
    promedio = {}
    for h in notas:
        promedio[h] = sumatoria(notas[h]) / len(notas[h])
    return promedio
    
def sumatoria(lista:list)->int:
    suma = 0
    for i in lista:
        suma += i
    return suma

#y= "testfile.txt"
#print(calcular_promedio_por_estudiante(y))

#ejercicio 21
def la_palabra_mas_frecuente(archivo:str)->str:
    file = open(archivo,"r")
    texto = file.read()
    file.close()
    texto = dividir_en_espacios(texto)
    palabras_frec = {}
    for i in texto:
        if not(i in palabras_frec):
            palabras_frec[i] = 1
        else:
            palabras_frec[i] += 1
    nums_aux = []            
    for k in palabras_frec:
        nums_aux.append(palabras_frec[k])
    maxim = nums_aux[0]
    for g in nums_aux:
        if g >= maxim:
            maxim = g
    for t in palabras_frec:
        if palabras_frec[t] == maxim:
            return t
# y = "testfile2.txt"
#print(la_palabra_mas_frecuente(y))

#ejercicio 22        
historiales = {"usuarioA":"pag1","usuarioB":"pagB","usuarioC":"pagC"}
def visitar_sitio(historiales:dict,usuario:str,sitio:str)->dict:
    return historiales[usuario].put(sitio)

def navegar_atras(historiales:dict,usuario:str)->dict:
    #considero el caso donde no tiene 2 pags?
    while not historiales[usuario].empty():
        ultimo_sitio = historiales[usuario].get()
        ante_ultimo_sitio = historiales[usuario].get()
        historiales[usuario].put(ante_ultimo_sitio)
        historiales[usuario].put(ultimo_sitio)
        historiales[usuario].put(ante_ultimo_sitio)
    return historiales

#ejercicio 23
def agregar_producto(invetario:dict,nombre:str,precio:float,cantidad:int):
    #if not nombre in invetario:
    invetario[nombre] = {"precio":precio,"cantidad":cantidad}
    return invetario

def actualizar_stock(invetario:dict,nombre:str,cantidad:int):
    invetario[nombre]["cantidad"] = cantidad
    return invetario

def actualizar_precios(invetario:dict,nombre:str,precio:float):
    invetario[nombre]["precio"] = precio
    return invetario

def calcular_valor_invetario(inventario:dict)->float:
    res:float = 0
    for i in inventario:
        res += inventario[i]["precio"]*inventario[i]["cantidad"]
    return res