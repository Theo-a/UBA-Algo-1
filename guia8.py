from typing import List
def contar_lineas(x:str)->int:
    with open(x,'r') as file:
        lineas:List[str] = file.readlines()
        lineas_cant:int = len(lineas)
    return lineas_cant
#x = "testfile.txt"
#print(contar_lineas(x))
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

def cantidad_aprariciones(texto:str,palabra:str)->int:
    i:int = 0
    repeticiones:int = 0
    while i < len(texto):
        if texto[i] == palabra[0]:
            if consecutivos_iguales(palabra, texto, i):
                repeticiones = repeticiones + 1
        i += 1
    return repeticiones

def clonar_sin_comentarios(texto:str)->str:
    with open(texto,'r') as file:
        lineas = file.readlines()
        for i in lineas:
            if aux_clonar_sin_comentarios(i):
                lineas.remove(i)
        return lineas
            
def aux_clonar_sin_comentarios(linea:str)->bool:
    i = 0
    while i < len(linea):
        if linea[i] == "#":
            return True
        elif linea[i] == " ":
            k = k + 1
    return False 
