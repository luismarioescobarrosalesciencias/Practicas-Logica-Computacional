Lógica Computacional
Laboratorio

Practica 05

Equipo Dinamita
Integrantes:
Escobar Rosales Luis Mario 420003818
Liera Montaño Miguel Ángel 317257421

11 de diciembre de 2020


Ejercicios:

1. Elemento: Para la implementación de esta relación ocupamos un caso base, donde el elemento de la cabeza de la lista es el elemento buscado, y en el caso recursivo, descompone la lista en sucesivas sublistas con la cola en cada llamada a la relación y se pregunta el caso base.

2. Suma: En el caso base se evalúa cuando la lista está vacía, entonces la suma de sus elementos será cero. En el caso recursivo se descompone la suma en cabeza y cola, pensando en tratar a cada elemento de esta independientemente e ir sumando en cada llamada que se hace, de manera que se aplica la relación a la nueva sublista (cola de la anterior) y se va sumando la cabeza al resultado de la suma de la sublista en la recursión.

3. Palíndromo: Es lógico pensar que una lista que es un palíndromo será igual que su inversa, por lo que aplicando este criterio llegamos a la solución de este ejercicio.

4. Ocurrir: De igual manera pensamos en descomponer la lista y hacer recursión para ir encontrando si cada elemento de la lista es igual al que se busca y así agregar a una suma un 1 en el caso de que los elementos sean iguales o no se agrega nada a la suma en el caso de que no sean iguales.

5. inOrden: Pensamos en una implementación muy clásica del algoritmo, que consiste en dos casos base: cuando el árbol está vacío o tiene un solo elemento. Para el caso recursivo se hace recursión sobre los subárboles izquierdo y derecho y para que sea un recorrido inOrden se debe de concatenar la lista generada en la recursión del subárbol izquierdo antes de concatenar el elemento de la raíz y después el de la derecha.
