from typing import List
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