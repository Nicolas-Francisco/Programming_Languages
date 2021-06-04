############################################ Parte a:
# ¿Sus funciones son de primera clase o primer orden?

# Sea la siguiente función de python que retorna
# Una función lambda que suma uno a un n.
def add1_fun(x):
    return lambda x: x+1

# Si el lenguaje fuera de primera clase, entonces
# Una expresión como la siguiente:
two = ((add1_fun) (1))
# Compilaría, y al llamar a two daría el valor "2".

# Por otro lado, si al correr el programa este arroja
# Error en two, entonces el lenguaje no acepta que 
# Funciones sean tratadas como valores, tratandose
# de un lenguaje de Primer Orden.



############################################ Parte b:
# ¿La evaluación de sus argumentos es de izquierda a 
#  derecha o de derecha a izquierda?

# Sean las dos siguientes funciones auxiliares que retornan 
# un número y printean.
def fun1():
    print("Estoy en fun1!")
    return 1

def fun2():
    print("Estoy en fun2!")
    return 2

# Luego si corremos el siguiente programa:
n = fun1() + fun2()
# Bastaría observar lo que se imprime en la consola.

# Si se observa:
"Estoy en fun1!"
"Estoy en fun2!"
# Sabemos que el lenguaje evalua de izquerda a derecha,
# en caso contrario, es de derecha a izquierda.



############################################ Parte c:
# ¿Tiene alcance léxico o dinámico?

# Definamos dos funciones que trabajan con x
def fun1():
    return x

def fun2():
    x = 0
    return fun1()

# Si luego corremos
fun2()
# Y este no cae en un error (retorna 0), estamos en alcance
# dinámico, ya que fun1 fué capaz de llamar a x sin haberlo
# definido anteriormente.

# Si por otro lado este tira error, estamos en alcance léxico



############################################ Parte d:
# ¿Las operaciones and y or hacen cortocircuito?

# si ejecutamos el siguiente programa:
(True or 1)
(False and 1)

# Y este retorna sin haber lanzado error, entonces sabemos que
# este lenguaje no verifica ambos argumentos y optimiza el 
# resultado (Cortocircuito)

# Si alguno o ambos tiran error, sabremos cuál es el que evita
# el cortocircuito y verifica los argumentos.



############################################ Parte e:
# ¿Tiene evaluación temprana o perezosa?

# Sea la siguiente función add1
def add1(x):
    n = 1/0
    return x+1

# Si al ejecutar el siguiente programa:
add1(1)
# Este retorna el valor 2, entonces el lenguaje tiene evaluación
# perezosa, pues no evalúa a n si este no es llamado.

# Por otro lado, si este arroja error de división por 0, entonces
# estamos en evaluación Temprana, pues lo ejecuta pese a que no
# Es llamado para retornar.



############################################ Parte f:
# ¿Soporta la optimización de recursión por la cola (TRO)?

# Sea la siguiente función recursiva con recursión por la cola
def factorial(n):
    def factorial_con_acc(n, acc):
        if n == 0:
            return acc
        else:
            return factorial_con_acc(n-1, acc*n)
    return factorial_con_acc(n,1)
    
# Si ejecutamos el siguiente programa:
factorial(99999)
# Y este se ejecuta sin error de overflow de memoria, el lenguaje
# tiene optimización de recursión por la cola, pues con ejecuciones
# recursivas de altas iteracioens este no cae.

# Si por otro lado este lanza error de overflow de memoria, no está
# optimizado por TRO



############################################ Parte g:
# ¿Soporta la optimización de llamados por la cola (TCO)?

# Sean las siguientes funciones auxiliares:
def even(n):
    if n==0:
        return True 
    if n==1:
        return False
    return odd(n-1)

def odd(n):
    if n==0:
        return False
    if n==1:
        return True 
    return even(n-1)    

# Si ejecutamos el siguiente programa:
even(99999)
# Y este se ejecuta sin error de overflow de memoria, el lenguaje
# tiene optimización de llamados por la cola, pues con ejecuciones
# recursivas de altas iteracioens este no cae.

# Si por otro lado este lanza error de overflow de memoria, no está
# optimizado por TCO