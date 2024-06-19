from queue import Queue as Cola

#ej 1
def torneo_de_gallinas(estrategias: dict[str,str]) -> dict[str,int]:
    puntajes:dict[str,int] = {}
    for i in estrategias:
        puntajes[i] = 0
    A = "me desvio siempre"
    B = "me la banco y no me desvio"
    for jugador in estrategias:
        for l in estrategias:
            if jugador != l:
                if estrategias[jugador] == A and estrategias[l] ==  A:
                    puntajes[jugador] += -10
                if estrategias[jugador] == B and estrategias[l] ==  B:
                    puntajes[jugador] += -5
                if estrategias[jugador] == A and estrategias[l] ==  B:
                    puntajes[jugador] += -15
                if estrategias[jugador] == B and estrategias[l] ==  A:
                    puntajes[jugador] += 15
    return puntajes

#test ej1
entrada = {"jugador1": "me desvio siempre", "jugador2": "me desvio siempre"}
salida = {"jugador1": -10, "jugador2": -10}
#print(torneo_de_gallinas(entrada))
entrada2 = {"jugador1": "me la banco y no me desvio", "jugador2": "me la banco y no me desvio"}
salida2 = {"jugador1": -5, "jugador2": -5}
#print(torneo_de_gallinas(entrada2))
entrada3 = {"jugador1": "me la banco y no me desvio", "jugador2": "me desvio siempre"}
salida3 = {"jugador1": 10, "jugador2": -15} # aca fall da 15 en vez de 10
#print(torneo_de_gallinas(entrada3))
entrada4 = {'Juli': "me la banco y no me desvio", 'Facu': "me la banco y no me desvio", 'Lucas': "me desvio siempre", 'Ana': "me desvio siempre"}
salida4 = {'Juli': 15, 'Facu': 15, 'Lucas': -40,'Ana': -40} #por el anterior da mal
#print(torneo_de_gallinas(entrada4))

#ej 2
def reordenar_cola_priorizando_vips(fila_clientes: Cola[tuple[str,str]]) -> Cola[str]:
    cola_comun:Cola = Cola()
    cola_vip: Cola = Cola()
    while not fila_clientes.empty():
        elem = fila_clientes.get()
        if elem[1] == "comun":
            cola_comun.put(elem)
        elif elem[1] == "vip":
            cola_vip.put(elem)
    cola_final:Cola = Cola()
    while not cola_vip.empty():
        cola_final.put(cola_vip.get())
    while not cola_comun.empty():
        cola_final.put(cola_comun.get())
    return cola_final

#test ej 2
"""
entrada = Cola()
salida = Cola()
res = reordenar_cola_priorizando_vips(entrada)
#print(res.queue == entrada.queue)

entrada = Cola()
entrada.put(("a","comun"))
salida = "a"
res = reordenar_cola_priorizando_vips(entrada)
nombre = res.get()[0]
#print(nombre == salida)

fila = Cola()
fila.put(('Facu', 'comun'))
fila.put(('Ana', 'vip'))
fila.put(('Juli', 'vip'))
fila.put(('Lucas', 'comun'))
      
res = Cola()
res.put('Ana')
res.put('Juli')
res.put('Facu')
res.put('Lucas')
#print(reordenar_cola_priorizando_vips(fila).queue == res.queue)
res2 = reordenar_cola_priorizando_vips(fila)
#print(res2.queue)
#print(res.queue)
#esta mal porque la fun devuelve la cola[str,str] en vez de cola[str], incluyo el "vip" o "comun"

fila2 = Cola()
fila2.put(('Facu', 'vip'))
fila2.put(('Ana', 'comun'))
fila2.put(('Juli', 'vip'))
fila2.put(('Lucas', 'comun'))
      
res2 = Cola()
res2.put('Ana')
res2.put('Juli')
res2.put('Facu')
res2.put('Lucas')
#print(reordenar_cola_priorizando_vips(fila2).queue == res.queue)

fila3 = Cola()
fila3.put(('Facu', 'vip'))
fila3.put(('Ana', 'comienza'))
fila3.put(('Juli', 'vip'))
fila3.put(('Lucas', 'comun'))
      
res3 = Cola()
res3.put('Ana')
res3.put('Juli')
res3.put('Facu')
res3.put('Lucas')
#print(reordenar_cola_priorizando_vips(fila3).queue == res.queue)
"""

#ej 3
def cuantos_sufijos_son_palindromos(texto: str) -> int:
    sufijos:list[str] = []
    palabra:str = ""
    for i in range(len(texto)):
        for j in range(i,len(texto)):
            palabra += texto[j]
        sufijos.append(palabra)
        palabra = ""
    cont:int = 0
    invertido:str = ""
    for k in sufijos:
        for l in range(len(k)-1,-1,-1):
            invertido += k[l]
        if k  == invertido:
            cont += 1
        invertido = ""
    return cont

#test ej 3
entrada = "Diego" 
salida = 1
#print(cuantos_sufijos_son_palindromos(entrada) == salida)
texto1 = 'a'
#print(cuantos_sufijos_son_palindromos(texto1) == 1)
texto2 = 'alas'
#print(cuantos_sufijos_son_palindromos(texto2) == 2) #mala correcion, reclamar, el resto bien
texto3 = 'parrap'
#print(cuantos_sufijos_son_palindromos(texto3) == 2)
texto4 = 'ododo'
#print(cuantos_sufijos_son_palindromos(texto4) == 3)

#ej 4
def quien_gano_el_tateti_facilito(tablero: list[list[str]]) -> int:
    hay_3_X:bool = False
    hay_3_O:bool = False
    res: int = 0
    for i in range(len(tablero)):
        for j in range(len(tablero)-2):
            if tablero[j][i] == 'X':
                if tablero[j+1][i] == 'X' and tablero[j+2][i] == 'X':
                    hay_3_X = True
            if tablero[j][i] == 'O':
                if tablero[j+1][i] == 'O' and tablero[j+2][i] == 'O':
                    hay_3_O = True

    if hay_3_X and not hay_3_O:
        res = 1
    if not hay_3_O and hay_3_O: #soy taradisimo
        res = 2
    if hay_3_O and hay_3_X:
        res = 3
    if not hay_3_O and not hay_3_X:
        res = 0
    return res

#test ej 4

tablero1 = [['X', 'O', 'X', 'O', 'X'],
           ['O', 'X', 'O', 'X', 'O'],
           ['X', 'X', 'X', 'O', 'X'],
           ['X', 'O', 'O', 'X', 'O'],
           ['O', 'O', 'X', 'X', 'O']]
#print(quien_gano_el_tateti_facilito(tablero1) == 0)
tablero2 = [['X', 'O', ' ', ' ', ' '],
           ['X', 'X', ' ', ' ', ' '],
           ['X', ' ', 'O', ' ', ' '],
           [' ', ' ', ' ', 'X', ' '],
           [' ', ' ', ' ', ' ', 'O']]
#print(quien_gano_el_tateti_facilito(tablero2) == 1)
tablero3 = [['X', ' ', ' ', ' ', ' '],
           [' ', 'O', ' ', ' ', ' '],
           ['X', 'O', 'X', ' ', ' '],
           [' ', 'O', ' ', 'X', ' '],
           [' ', ' ', ' ', ' ', 'O']]
#print(quien_gano_el_tateti_facilito(tablero3) == 2) #falla
tablero4 = [[' ', 'O', ' ', ' ', ' '],
          [' ', 'X', 'X', ' ', ' '],
          [' ', ' ', 'X', ' ', 'O'],
          [' ', ' ', 'X', 'X', 'O'],
          [' ', ' ', ' ', ' ', 'O']]
#print(quien_gano_el_tateti_facilito(tablero4) == 3)
tablero5 = [['X', 'O', 'X', 'O', 'X', 'O', 'X', 'O', 'X', 'O'],
           ['O', 'X', 'O', 'X', 'O', 'X', 'O', 'X', 'O', 'X'],
           ['X', 'O', 'X', 'O', 'X', 'O', 'X', 'O', 'X', 'O'],
           ['O', 'X', 'O', 'X', 'O', 'X', 'O', 'X', 'O', 'X'],
           ['X', 'O', 'X', 'O', 'X', 'O', 'X', 'O', 'X', 'O'],
           ['O', 'X', 'O', 'X', 'O', 'X', 'O', 'X', 'O', 'X'],
           ['X', 'O', 'X', 'O', 'X', 'O', 'X', 'O', 'X', 'O'],
           ['O', 'X', 'O', 'X', 'O', 'X', 'O', 'X', 'O', 'X'],
           ['X', 'O', 'X', 'O', 'X', 'O', 'X', 'O', 'X', 'O'],
           ['O', 'X', 'O', 'X', 'O', 'X', 'O', 'X', 'O', 'X']]
#print(quien_gano_el_tateti_facilito(tablero5) == 0)