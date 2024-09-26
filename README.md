# Implementación Jugable de Texas Hold'em en R

Este proyecto contiene una implementación jugable del juego de póker en su versión Texas Hold'em en **R**. A continuación se presenta un diagrama que representa un resumen del flujo del programa y de las relaciones entre funciones:

![Sin título](https://github.com/user-attachments/assets/9f097ea0-f874-4a9b-bc25-39540b0d0b79)


## 1. Preparación previa

- Se guardan los argumentos indicados en un entorno específico para mantenerlos a lo largo de las partidas: `cantidad_carteras`, `reiniciar_carteras`, `numero_jugadores`.
- Se crean los siguientes objetos de consulta en el entorno global: `baraja_consulta`, `retirados`, `eliminados`, `orden_de_juego`, `orden_de_juego_original`, `carteras`, `bote`.
- Se crean objetos en el entorno de ejecución de la función: `carteras`, `allin`, `chequeo_allin`, `turno`, `partida`.
    - Para crear el objeto `partida`, se llama a `nueva_baraja()` para generar una nueva baraja y a `repartir()` para repartir las cartas a los jugadores y a la mesa.
    - Se restan las ciegas grande y pequeña a los jugadores correspondientes.

## 2. Bucle general del juego

El flujo del juego se mantiene con un bucle de tipo `repeat{}`:

- Al inicio de cada turno, se verifica si hay tres jugadores retirados. Si es el caso, el jugador que no se haya retirado gana.
- Se crea el objeto `jugadas` para almacenar las apuestas finales de cada jugador en cada turno.
- Se crea el objeto `cantidad_anterior` para registrar las cantidades apostadas por cada jugador dentro de cada turno.
- El turno se actualiza: de 0 pasa a 1, de 1 a 2, etc.
- Se saca una nueva carta de la baraja para colocarla en la mesa.
- En el último turno, se formaliza la jugada final llamando a la función `quien_gana()` y actualizando la cartera del ganador con el bote.
- Se ofrece la opción de seguir jugando.
- Se actualiza el orden de juego y el bote en el entorno global para excluir a los retirados.

## 3. Bucle de apuestas

Este bucle se ejecuta durante los tres primeros turnos, a menos que ocurra un `all-in`, en cuyo caso se bloquean las apuestas y solo se sacan cartas:

- Se generan los siguientes objetos:
    - `apuestas_a_revisar`: Vector donde se almacenan las apuestas iniciales de los jugadores.
    - `revision`: Contador que se actualiza cada vez que se revisan apuestas.
    - `orden_de_juego_sub`: Vector copia del orden de juego, que se actualiza para seguir iterando sobre los jugadores.
    - `apuestas_maximas`: Vector donde se acumulan las apuestas máximas durante las revisiones.
    - `carteras_sub`: Variable utilizada para manejar las apuestas acumuladas y evaluar las carteras de los bots.

El bucle se mantiene activo hasta que todos los jugadores hagan una jugada válida.

### 3.1. Sub-bucle de apuestas

- Un segundo chequeo de retirados se realiza al inicio de cada revisión.
- El usuario toma decisiones (apostar, subir, retirarse) mediante un `switch`, que solo acepta respuestas válidas.
- Los bots toman decisiones mediante la función `eva_jugadas()` y, dependiendo de sus cartas, realizan apuestas basadas en probabilidades predeterminadas. Los bots también pueden hacer `all-in` en caso de no tener suficientes fondos.

## 4. Actualización de carteras

- Se mantienen las carteras tanto en el entorno global como en el de ejecución.
- Durante el juego, las carteras solo se actualizan en el entorno de ejecución.
- La actualización de las carteras en el entorno global ocurre cuando el jugador decide seguir jugando.

## 5. Último turno

- Se imprime la información de la partida (cartas y retirados).
- Se llama a la función `quien_gana()`, que evalúa las jugadas y decide el ganador. Si hay empate, se buscan sucesivamente jugadas más altas.
- Las carteras se actualizan sumando el bote al ganador.
- En caso de empate, el bote se reparte entre los empatadores.
- Se ofrece la opción de seguir jugando con una llamada recursiva a la función `jugar()`, que reinicia el ciclo.

---
