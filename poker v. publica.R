rm(list=ls())

#### Función para generar una nueva baraja ####
nueva_baraja <- function() {                                                        
  baraja <- matrix(NA, nrow = 13*4, ncol = 3)
  baraja[,2] <- rep(c("Corazones", "Diamantes", "Treboles", "Picas"), each = 13)
  baraja[,1] <- c("2", "3", "4", "5", "6", "7", "8", "9", "10", "Jota", "Reina", "Rey", "As")
  baraja <- data.frame(nombre = baraja[,1], palo = baraja[,2], valor = 1:13, id = 1:52)
  return(baraja)
}

#### Función para repartir la baraja al principio de cada partida y generar el objeto partida ####
repartir <- function(baraja, numero_jugadores, baraja_consulta) {                                                      
  indices <- sample(baraja$id, 2*numero_jugadores + 2)                                                                   
  manos_i <- matrix(indices[1:(length(indices)-2)], ncol = 2, nrow = numero_jugadores, dimnames = list(paste("Jugador", 1:numero_jugadores), c("carta 1", "carta 2")))
  manos <- manos_i
  for (indice in manos_i){
    nombre <- baraja_consulta$nombre[indice]
    palo <- baraja_consulta$palo[indice]
    manos[which(manos == indice)] <- paste(nombre, "de", palo)
    }
  mesa_i <- c(indices[length(indices):(length(indices)-1)], rep(NA, 3))
  mesa <- paste(baraja_consulta$nombre[baraja_consulta$id %in% mesa_i], "de", baraja_consulta$palo[baraja_consulta$id %in% mesa_i])
  baraja <- baraja[!(baraja$id %in% indices),]
  partida <- list(baraja = baraja, mesa = mesa, mesa_i = mesa_i, manos = manos, manos_i = manos_i)
  return(partida)
}

#### Función para evaluar la jugada (compartida entre eva_jugadas y quien_gana) ####
que_tiene <- function(jugada, baraja_de_consulta = baraja_consulta){                   # Saca baraja_consulta del entorno global
  jugada_valores <- baraja_de_consulta$valor[jugada]
  jugada_palos <- baraja_de_consulta$palo[match(jugada, baraja_de_consulta$id)]
  names(jugada_valores) <- jugada_palos
  
  # Carta más alta - la más básica de todas, en caso de que no haya otra jugada ganadora
  carta_alta <- max(jugada_valores[1:2])
  
  # Detección de valores repetidos (pareja, trio, doble pareja, color, full y poker)
  valores_pareja <- as.numeric(names(which(table(jugada_valores) == 2)))   
  valores_trio <- as.numeric(names(which(table(jugada_valores) == 3)))    
  valores_poker <- as.numeric(names(which(table(jugada_valores) == 4)))   
  valores_doble_pareja <- sort(valores_pareja, decreasing = TRUE)                            
  pareja <- ifelse(length(valores_pareja) == 0, FALSE, max(valores_pareja))            
  trio <- ifelse(length(valores_trio) == 0, FALSE, max(valores_trio))                   
  poker <- ifelse(length(valores_poker) == 0, FALSE, max(valores_poker))                
  doble_pareja <- ifelse(length(valores_pareja) < 2, FALSE, valores_doble_pareja[2])    
  color <- ifelse(any(table(jugada_palos) == 5) == 0, FALSE, 1)                                  
  full <- ifelse((pareja == FALSE) || (trio == FALSE), FALSE, sum(trio*3, pareja*2))  
  
  # Escaleras
  valor_escalera <- FALSE
  escalera_color <- FALSE
  escalera_real <- FALSE
  v <- c(1:13, 1:4)
  escaleras_posibles <- vector("list", 13)                                                              
  for (i in 1:13) if (i + 4 <= 17) escaleras_posibles[[i]] <- c(v[i+4], v[i+3], v[i+2], v[i+1], v[i])     
  
  # Jugadas a contrastar para la escalera
  tryCatch({                                      # Utilizando trycatch, no se bloquea cuando está evaluando jugadas que no llegan a tener 5 cartas (i.e. hasta el 3er turno)
    combinaciones <- t(combn(jugada_valores, 5))  # Combinaciones ordinarias # https://stackoverflow.com/questions/50084632/r-combn-function/50084745
    lista_combinaciones <- vector("list", 21) 
    for (fila in 1:nrow(combinaciones)) lista_combinaciones[[fila]] <- as.integer(sort(unique(combinaciones[fila,]), decreasing = TRUE))
    for (i in 1:13){
      if (escaleras_posibles[i] %in% lista_combinaciones){
        valor_escalera <- sum(escaleras_posibles[[i]])
        combinacion_ganadora <- escaleras_posibles[[i]]
        break}}
    
    # Escalera de color
    for (palo in names(table(names(jugada_valores)))){
      potencial_esc_color <- as.integer(sort(unique(jugada_valores[names(jugada_valores) == palo]), decreasing = TRUE)[1:5])
      if (identical(potencial_esc_color, combinacion_ganadora)){
        escalera_color <- valor_escalera
        break}}
    
    # Escalera real
    if(escalera_color && identical(combinacion_ganadora, 13:9)) escalera_real <- TRUE                                         
  }, error = function(e){})
  
  que_tiene <- list(
    escalera_real = escalera_real,
    escalera_color = escalera_color, 
    poker = poker, 
    full = full, 
    color = color,
    escalera = valor_escalera, 
    trio = trio, 
    doble_pareja = doble_pareja, 
    pareja = pareja, 
    carta_alta = carta_alta
  )
  return(que_tiene)
}

#### Función que regula el comportamiento de los bots para evaluar las jugadas que tienen y decidir qué cantidad apuestan y con qué probabilidad (tienen la opción de retirarse) ####
eva_jugadas <- function(jugador, turno, jugadas, partida, carteras, allin){
  cantidad_anterior <- jugadas$cantidad_apostada[jugador]                 # Se guarda la cantidad anterior apostada (en caso de ser la primera va a ser NA)
  jugada_bot <- rep(NA, (3+turno))                         
  jugada_bot[1:2] <- partida$manos_i[jugador,]
  jugada_bot[3:length(jugada_bot)] <- na.omit(partida$mesa_i)
  que_tiene <- unlist(que_tiene(jugada_bot))
  tiene_esto <- que_tiene[que_tiene > 0]
  cantidad <- 1+length(tiene_esto)                                        # La cantidad que apuesta es 1 más la cantidad de jugadas que tenga (la mínima va a ser 2) 
  if(tiene_esto[1] > 9) cantidad <- cantidad + length(que_tiene) + 2      # Aumenta la cantidad apostada si tiene una carta alta en su jugada
  probabilidad <- 0.5 * length(tiene_esto)                                # La probabilidad de apostar aumenta según el número de jugadas que tenga el bot
  probabilidad <- probabilidad + runif(1, -.2, .2)                        # Un poco de azar sobre la probabilidad para dificultar que los bots sean predecibles
  probabilidad <- probabilidad * (0.2 + (0.8**turno))                     # Es menos probable apostar cuantos menos turnos restantes haya
  if(carteras$cartera[jugador]/max(carteras$cartera) < 0.2) probabilidad <- probabilidad/2       # Si tiene poco dinero respecto al primero (menos de un 20% de lo que tiene el primero), la probabilidad de apostar se reduce a la mitad
  
  # En caso de que haya apostado alguien antes, el comportamiento del bot se ve afectado
  if (any(!is.na(jugadas$cantidad_apostada))){                              # Comprueba si ha apostado alguien antes
    cantidad_a_igualar <- max(jugadas$cantidad_apostada, na.rm = TRUE)      # La cantidad que tiene que igualar es el máximo de donde se están guardando las apuestas
    if (cantidad < cantidad_a_igualar){
      probabilidad <- probabilidad * 0.6             # Si la cantidad que apostaría el bot basándose en lo anterior es menor que la apuesta que haya en el momento, se reduce la probabilidad de apostar
      cantidad <- cantidad_a_igualar                 # Y la cantidad apostada es la misma, es decir, en caso de apostar no sube, solo iguala
      if(runif(1) < 0.3) {probabilidad <- 1}         # Hay un 30% de probabilidades de que apueste en cualquier caso, para adaptarse por ejemplo, al caso en que se le tira un farol
    } else {
      cantidad <- ifelse(runif(1) > 0.4, cantidad_a_igualar, cantidad)  
      probabilidad <- 1
    }
  }
  decision <- ifelse(probabilidad > runif(1), TRUE, FALSE) 
  
  # Si la cantidad que apostaría es mayor o igual de lo que tiene, hace allin (en caso de que decida apostar)
  if(decision && (cantidad >= carteras$cartera[jugador])){
    cantidad <- carteras$cartera[jugador] # En caso de que decida apostar, pero la cantidad apostada sea mayor de la que puede, va con todo lo que tenga.
    decision <- "allin"}
  
  if(decision == FALSE && !is.na(cantidad_anterior)) cantidad <- cantidad_anterior  # si la decisión es retirarse y hay una cantidad anterior, tiene que devolver la cantidad anterior porque es lo que va a restarse de la cartera del jugador
  
  return(list(cantidad = cantidad, decision = decision))
}

#### Función para evaluar quién ha ganado ####
quien_gana <- function(partida, numero_jugadores){
  lista_jugadas_finales <- vector("list", numero_jugadores)              # Se listan todas las jugadas que tiene cada jugador
  names(lista_jugadas_finales) <- paste("Jugador", 1:numero_jugadores)  
  for (jugador in which(!retirados)){
    lista_jugadas_finales[[jugador]] <- que_tiene(jugada = c(partida$manos_i[jugador,], partida$mesa_i))    
  }
  guardar_partida <<- partida
  #### Evaluación de quien ha ganado
  df_jugadas_finales <- do.call(rbind, lista_jugadas_finales)   # Genero un dataframe a partir de una lista --> https://stackoverflow.com/questions/2851327/combine-a-list-of-data-frames-into-one-data-frame-by-row
  for (columna in 1:ncol(df_jugadas_finales)){
    if (sum(df_jugadas_finales[,columna] == FALSE) < nrow(df_jugadas_finales)){
      
      # Chequeo de ganadores
      if (sum(df_jugadas_finales[,columna] == FALSE) == (nrow(df_jugadas_finales)-1)){
        return(list(ganador = names(which((df_jugadas_finales[,columna] != FALSE) == TRUE)), 
                    razon = colnames(df_jugadas_finales)[columna]))
        
        # Chequeo de empatadores
      } else if (sum(df_jugadas_finales[,columna] == FALSE) < (nrow(df_jugadas_finales)-1)){
        repeat{
          empatados <- which((df_jugadas_finales[,columna] != FALSE) == TRUE)  # Primero se guardan los jugadores empatados en la jugada 1
          valor_ganador <- suppressWarnings(max(unlist(df_jugadas_finales[,columna][which(df_jugadas_finales[,columna] != FALSE)]), na.rm = TRUE))  # Hay que hacer esto por si dos jugadores tienen la misma carta alta (que es la última jugada); elimino los warnings para jugadas que no tiene nadie
          ganadores <- which(df_jugadas_finales[,columna] == valor_ganador)  # Los ganadores son los que tienen el valor ganador; si hay más de un ganador, se mira la siguiente columna y así sucesivamente hasta que encuentre uno
          if (length(ganadores) == 1){
            return(list(ganador = names(ganadores),
                        razon = colnames(df_jugadas_finales)[columna]))
          } else {    # Si no encuentra un ganador ni siquiera en carta alta, se devuelve un empate, y el bote se divide entre los que obtengan el empate total.
            columna <- columna + 1
            if (columna > ncol(df_jugadas_finales)) {
              return(list(ganador = names(df_jugadas_finales[,(columna-1)][which(names(df_jugadas_finales[,(columna-1)]) %in% names(ganadores))]),          # Hay que volver a ajustar el índice de columna porque hemos sumado 1
                          razon = "empate en carta_alta"))
            }
          }
        }
      }
    }
  }
}

#### Función que mantiene el ciclo del juego ####
jugar <- function(reiniciar_carteras = FALSE, numero_jugadores = 4, cantidad_carteras = 100){
  
  #### Preparación de la partida ####
  
  # Guardo los argumentos de la función en un entorno específico para mantenerlos a lo largo de las partidas
  if(!exists("argumentos_jugar")) argumentos_jugar <- new.env()             
  assign("cantidad_carteras", cantidad_carteras, envir = argumentos_jugar)
  assign("reiniciar_carteras", reiniciar_carteras, envir = argumentos_jugar)
  assign("numero_jugadores", numero_jugadores, envir = argumentos_jugar)
  
  # Creo una baraja de consulta que va al entorno global
  baraja_consulta <<- nueva_baraja()                        
  nueva_baraja <- nueva_baraja()
  partida <- repartir(baraja = nueva_baraja, numero_jugadores = numero_jugadores, baraja_consulta = baraja_consulta)   
  
  # Ahora el orden de juego solo se aleatoriza en el primer turno, y el resto de turnos funciona como un poker normal
  if (!exists("orden_de_juego", where = .GlobalEnv)) {
    orden_de_juego <<- sample(1:numero_jugadores, numero_jugadores)                 
    orden_de_juego_original <<- orden_de_juego  # Creando una variable adicional, si hay retirados en una partida, no se pasan también a la siguiente
  } else {
    orden_de_juego_original <<- c(orden_de_juego_original[2:numero_jugadores], orden_de_juego_original[1])
    orden_de_juego <<- orden_de_juego_original}
  
  # Se crea en el entorno global un objeto carteras, que se va a reiniciar si se indica en la función, y en caso contrario permanece constante hasta que termine el juego.
  if (argumentos_jugar$reiniciar_carteras) {
    carteras <- data.frame(jugador = 1:numero_jugadores, cartera = rep(cantidad_carteras, numero_jugadores))
    carteras <<- carteras
  } else {
    if (!exists("carteras", where = .GlobalEnv)) {
      carteras <- data.frame(jugador = 1:numero_jugadores, cartera = rep(cantidad_carteras, numero_jugadores))
      carteras <<- carteras
    }
  } 
  
  # Ahora se crea en el entorno global un vector de eliminados (los que se quedan con menos de 4 en la cartera), distinto del de retirados (los retirados se actualizan en cada partida, mientras que los eliminados no).
  if (!exists("eliminados", where = .GlobalEnv)) eliminados <<- rep(FALSE, numero_jugadores)
  eliminados[carteras$cartera <= 4] <<- TRUE
  if(sum(eliminados) == 3){
    rm(list = setdiff(ls(envir = .GlobalEnv), lsf.str(envir = .GlobalEnv)), envir = .GlobalEnv)
    stop("Sólo queda un jugador. Fin de la partida.")}
  
  # Los eliminados se guardan automáticamente como retirados y se quitan del orden de juego
  orden_de_juego <<- orden_de_juego[!(orden_de_juego %in% which(eliminados))]
  retirados <<- rep(FALSE, numero_jugadores)
  retirados[eliminados] <<- TRUE
  
  # Hay dos variables nuevas para el manejo de los all-in: allin y chequeo_allin. Allin indica si el jugador está haciendo un allin y chequeo_allin sirve para que cada jugador pueda elegir qué hacer en caso de que otro haga allin: igualar (si puede), hacer otro all-in (si no puede igualar), o retirarse. Cuando alguien hace allin, se bloquean las apuestas del resto de turnos (solo se sacan cartas). De esta manera, es imposible que haya apuestas con carteras negativas.
  chequeo_allin <- rep(FALSE, numero_jugadores)
  chequeo_allin[eliminados] <- TRUE
  allin <- rep(FALSE, numero_jugadores)
  
  # El turno empieza en 0 y luego se suma 1 por cada iteración
  turno <- 0                                                
  
  # Nueva partida
  cat("\n\n\n\n\n\n*************** NUEVA PARTIDA ***************\n")
  cat("\n\nTu turno:", which(orden_de_juego == 1))  
  
  # El bote empieza en 6 porque es la suma de las ciegas grande y pequeña (4 + 2)
  bote <<- 6                                                
  
  # Se decide qué jugador pone la ciega pequeña y la grande igual que en el poker
  ciega_peque <- orden_de_juego[1]                          
  ciega_grande <- orden_de_juego[2]
  cat("\nPone ciega grande el jugador", ciega_grande)
  cat("\nPone ciega pequeña el jugador", ciega_peque)
  
  # Se restan las ciegas de las respectivas carteras
  carteras$cartera[ciega_grande] <- carteras$cartera[ciega_grande] - 4        
  carteras$cartera[ciega_peque] <- carteras$cartera[ciega_peque] - 2
  
  
  
  #### Bucle que mantiene cada juego ####
  repeat{
    
    ## Comprobación de si hay retirados al inicio de cada turno
    
    # Chequeo de retirados
    if (sum(retirados) == (numero_jugadores-1)){ 
      # Si se retiran todos menos uno, gana quien no se retira y se le suma el bote a la cartera 
      carteras$cartera[which(retirados == FALSE)] <- carteras$cartera[which(retirados == FALSE)] + bote                                                                                         
      cat("\n--------------------------------------------------------\n", "Gana el Jugador", which(retirados == FALSE), "y se lleva un bote de:", bote, "\n--------------------------------------------------------\n\n")
      print(carteras)
      cat("\n\n")
      
      # Se da la opción de volver a jugar y se utiliza un switch para hacer una u otra cosa
      sigue <- readline("¿Sigues jugando? (s/n): ")
      while(!(sigue %in% c("s", "n"))){
        sigue <- readline("¿Sigues jugando? (s/n): ")}                                                             
      switch(sigue, 
             "s" = {
               carteras <<- carteras
               jugar(reiniciar_carteras = argumentos_jugar$reiniciar_carteras, numero_jugadores = argumentos_jugar$numero_jugadores, cantidad_carteras = argumentos_jugar$cantidad_carteras)
             }, 
             "n" = {
               # Si se decide no seguir jugando, se elimina todo menos las funciones: https://stackoverflow.com/questions/8305754/remove-all-variables-except-functions
               rm(list = setdiff(ls(envir = .GlobalEnv), lsf.str(envir = .GlobalEnv)), envir = .GlobalEnv) 
               stop("Fin del juego")
             })
    } 
    
    ## Preparación de las jugadas y la interfaz
    
    # Se crea el objeto jugadas, que ya no necesita guardar la probabilidad de apostar
    jugadas <- data.frame(jugador = 1:numero_jugadores, cantidad_apostada = rep(NA, numero_jugadores))
    cantidad_anterior <- rep(NA, numero_jugadores)
    turno <- turno + 1                                                                                       
    cat("\n\n\n\n\n##########     TURNO", turno, "    ##########\n")
    
    # Repartir adaptado a los turnos que no son el primero
    if (turno > 1){
      indice <- sample(partida$baraja$id, 1)  
      partida$mesa_i[[turno+1]] <- indice
      partida$mesa[[turno+1]] <- paste(baraja_consulta$nombre[baraja_consulta$id == indice], "de", baraja_consulta$palo[baraja_consulta$id == indice])
      partida$baraja <- partida$baraja[!(partida$baraja$id %in% partida$mesa_i), ]
    }
    cat("\nMesa:\n"); print(partida$mesa)
    cat("\nTu mano:\n"); print(t(partida$manos[1,]))      
    cat("\nTu cartera:", carteras[1,2], "\n\n-------------------------------------\n\n")
    
    ## Turnos de apuestas entre el 1 y el 3; no se llevan a cabo si alguien hace allin
    if (turno < 4 && !any(allin)){
      apuestas_a_revisar <- rep(0, numero_jugadores)
      revision <- 0                                         
      orden_de_juego_sub <- orden_de_juego                  
      apuestas_maximas <- rep(0, 100)       # Se usan 0 porque asi el primer elemento es un 0 y se puede usar de referencia. Admite 100 revisiones.
      carteras_sub <- carteras
      # carteras_sub es una variable que se va a utilizar para: 
      # 1. Que la interfaz tenga más sentido: muestra la apuesta acumulada que hace cada jugador en el turno, en vez de la apuesta en cada jugada.
      # 2. Permitir la evaluación de las carteras por parte de los bots.
      # 3. No modificar el objeto carteras hasta que todo el mundo haya decidido su apuesta final para el turno.

      # Bucle de las apuestas - mientras haya apuestas a revisar y no todos hayan hecho una jugada válida en caso de all-in, se hace un bucle de la ronda de apuestas
      while(length(apuestas_a_revisar) > 0 && !all(chequeo_allin)){                
        for (jugador in orden_de_juego_sub){
          
          # Chequeo de retirados dentro de cada turno
          # Es el mismo código que el anterior, sólo que este evalúa dentro de cada turno. Según he comprobado, deben mantenerse ambos para evitar errores. Podría haberlo modularizado y externalizarlo en otra función, pero como son 3 líneas de código no me parece conveniente.
          if (sum(retirados) == (numero_jugadores-1)){                                                 
            carteras$cartera[which(retirados == FALSE)] <- carteras$cartera[which(retirados == FALSE)] + bote         
            cat("\n--------------------------------------------------------\n", "Gana el Jugador", which(retirados == FALSE), "y se lleva un bote de:", bote, "\n--------------------------------------------------------\n\n")
            print(carteras)
            cat("\n\n")
            
            # Se da la opción de volver a jugar. Es el mismo caso que el anterior, está repetido el código, pero como es tan breve, no me resulta conveniente externalizarlo en una función.
            sigue <- readline("¿Sigues jugando? (s/n): ")
            while(!(sigue %in% c("s", "n"))){
              sigue <- readline("¿Sigues jugando? (s/n): ")}                                                             
            switch(sigue,
                   "s" = {
                     carteras <<- carteras
                     jugar(reiniciar_carteras = argumentos_jugar$reiniciar_carteras, numero_jugadores = argumentos_jugar$numero_jugadores, cantidad_carteras = argumentos_jugar$cantidad_carteras)
                   },
                   "n" = {
                     rm(list = setdiff(ls(envir = .GlobalEnv), lsf.str(envir = .GlobalEnv)), envir = .GlobalEnv) # https://stackoverflow.com/questions/8305754/remove-all-variables-except-functions
                     stop("Fin del juego")
                   })
          }
          cat("Turno de Jugador", jugador, "\n")
          
          # Turno del usuario
          if (jugador == 1){                                                                                  
            if (((orden_de_juego_sub[1] == 1) && (revision == 0)) | all(is.na(jugadas$cantidad_apostada))){
              decide <- readline("Eres el primero en apostar. ¿Qué vas a hacer? [a]postar, [r]etirarse: ")    
              while(!(decide %in% c("a", "r"))) {   # Ahora en lugar de detenerse, sigue buscando una respuesta válida
                cat("\nTienes que dar una respuesta válida\n")
                decide <- readline("Eres el primero en apostar. ¿Qué vas a hacer? [a]postar, [r]etirarse: ")}   
            } else {
              decide <- readline("Es tu turno. ¿Qué vas a hacer? [i]r, [s]ubir, [r]etirarse: ")
              while(!(decide %in% c("i", "s", "r"))) {                                                        
                cat("\nTienes que dar una respuesta válida\n")
                decide <- readline("Es tu turno. ¿Qué vas a hacer? [i]r, [s]ubir, [r]etirarse: ")
              }
            }
            switch(decide,    # Al utilizar carteras_sub, se pueden hacer evaluaciones de las carteras cada turno sin modificar las carteras hasta que termine el turno.
                   "a" = {
                     jugadas$cantidad_apostada[1] <- as.numeric(readline("¿Cuánto apuestas?: "))
                     # Si el usuario indica más cantidad de lo que hay en su cartera, hace un all-in. Esta misma lógica se aplica a la opción de subir.
                     if (carteras$cartera[1] <= jugadas$cantidad_apostada[1]){
                       allin[1] <- TRUE
                       jugadas$cantidad_apostada[1] <- carteras$cartera[1]
                       carteras_sub$cartera[1] <- 0
                       cat("Jugador 1 va con todo:", jugadas$cantidad_apostada[1], "\n\n")
                     } else {
                       carteras_sub$cartera[1] <- (carteras$cartera[1] - jugadas$cantidad_apostada[1])
                       cat("Jugador 1 apuesta", jugadas$cantidad_apostada[1], "\n\n")
                     }
                   },
                   "i" = {
                     # Si el usuario indica "ir", pero la apuesta que hay es mayor que su cartera, hace un all-in.
                     if(carteras$cartera[1] <= max(jugadas$cantidad_apostada, na.rm = TRUE)){
                       allin[1] <- TRUE
                       jugadas$cantidad_apostada[1] <- carteras$cartera[1]
                       carteras_sub$cartera[1] <- 0
                       cat("Jugador 1 va con todo:", jugadas$cantidad_apostada[1], "\n\n")
                     } else {
                       jugadas$cantidad_apostada[1] <- max(jugadas$cantidad_apostada, na.rm = TRUE)
                       carteras_sub$cartera[1] <- (carteras$cartera[1] - jugadas$cantidad_apostada[1])
                       cat("Jugador 1 apuesta", jugadas$cantidad_apostada[1], "\n\n")
                     }
                   },
                   "s" = {
                     jugadas$cantidad_apostada[1] <- as.numeric(readline(paste("¿Cuánto apuestas? - Mínimo de", max(jugadas$cantidad_apostada, na.rm = TRUE), ": ")))
                     if (carteras$cartera[1] <= jugadas$cantidad_apostada[1]){
                       allin[1] <- TRUE
                       jugadas$cantidad_apostada[1] <- carteras$cartera[1]
                       carteras_sub$cartera[1] <- 0
                       cat("Jugador 1 va con todo:", jugadas$cantidad_apostada[1], "\n\n")
                     } else {
                       cat("Jugador 1 apuesta", jugadas$cantidad_apostada[1], "\n\n")}
                   },
                   "r" = {
                     cantidad_anterior[1] <- jugadas$cantidad_apostada[1]
                     jugadas$cantidad_apostada[1] <- NA
                     retirados[1] <<- TRUE
                     cat("Jugador 1 se retira\n\n")
                   })
            
            
            # Turno de los bots
          } else {
            eva_jugadas_jugador <- eva_jugadas(jugador, turno, jugadas, partida, carteras_sub, allin)
            jugadas$cantidad_apostada[jugador] <- eva_jugadas_jugador$cantidad
            if(eva_jugadas_jugador$decision != FALSE){
              if(eva_jugadas_jugador$decision == "allin"){
                allin[jugador] <- TRUE
                carteras_sub$cartera[jugador] <- 0
                cat("Jugador", jugador, "va con todo:", jugadas$cantidad_apostada[jugador], "\n\n")
              } else {
                carteras_sub$cartera[jugador] <- carteras$cartera[jugador] - jugadas$cantidad_apostada[jugador]     # Se actualiza la cartera restando la cantidad apostada
                cat("Jugador", jugador, "apuesta", jugadas$cantidad_apostada[jugador], "\n\n")
              }
            } else {
              if (revision > 0) cantidad_anterior[jugador] <- jugadas$cantidad_apostada[jugador]  # Si se retira en la primera revisión, quiere decir que no hay cantidad apostada antes de retirarse, por lo que va a ser NA
              jugadas$cantidad_apostada[jugador] <- NA
              retirados[jugador] <<- TRUE
              cat("Jugador", jugador, "se retira\n\n")
            }
          }
        }
        
        # Chequeo all in - verifica que cada jugador haya hecho una jugada válida si hay all-in (esto sirve para los casos donde el all-in se hace después de que ya hayan apostado algunos, para revisar las apuestas de los mismos). El chequeo está manteniendo activo el bucle del mismo modo que si hay apuestas a revisar.
        if(any(allin)){
          for (jugador in 1:numero_jugadores){
            if (jugadas$cantidad_apostada[jugador] >= max(jugadas$cantidad_apostada, na.rm = TRUE) || allin[jugador] == TRUE || retirados[jugador] == TRUE){
              chequeo_allin[jugador] <- TRUE
            }
          }
        }
        
        # Se ha creado un vector apuestas_máximas para solucionar un error sobre cómo se actualizaba orden_de_juego_sub en caso de subirse la apuesta en distintas revisiones (ahora no vale con apostar la máxima una vez, tiene que apostar la máxima de la última revisión)
        revision <- revision + 1                                                                                
        apuesta_actual <- ifelse(all(is.na(jugadas$cantidad_apostada)), 0, max(jugadas$cantidad_apostada, na.rm = TRUE))
        apuestas_maximas[revision+1] <- apuesta_actual
        apuestas_a_revisar <- which(jugadas$cantidad_apostada < apuesta_actual)
        if (apuestas_maximas[revision+1] > apuestas_maximas[revision]){
          orden_de_juego_sub <- orden_de_juego[!(orden_de_juego %in% which(jugadas$cantidad_apostada == max(jugadas$cantidad_apostada, na.rm = TRUE)))]
        } else {
          orden_de_juego_sub <- orden_de_juego_sub[which(orden_de_juego_sub %in% apuestas_a_revisar)]
        }
        
        # Se eliminan de la revisión los retirados y los que superan el chequeo_allin
        orden_de_juego_sub <- orden_de_juego_sub[!(orden_de_juego_sub %in% which(retirados))]                   
        orden_de_juego_sub <- orden_de_juego_sub[!(orden_de_juego_sub %in% which(chequeo_allin))]
        cat("- - - - -\n\nLa apuesta está en", apuesta_actual, "\n\n")
      }
      
      # Ahora es cuando se actualizan las carteras, al final de cada turno, de esta manera no hay problemas de jugadores que tienen que apostar dos veces
      carteras$cartera[!retirados] <- carteras$cartera[!retirados] - jugadas$cantidad_apostada[!retirados]   
      no_son_na <- !is.na(cantidad_anterior[retirados])
      carteras$cartera[retirados][no_son_na] <- carteras$cartera[retirados][no_son_na] - cantidad_anterior[retirados][no_son_na]
      carteras$cartera[allin] <- 0   # Un allin significa que apuesta todo lo que tiene
      
      
      
      ## Último turno
    } else {                                                                                                                                                                              
      cat("SE DESCUBREN LAS MANOS\n\n")
      if (sum(retirados) == 0){
        quienes_retirados <- "Ninguno"
      } else {
        quienes_retirados <- which(retirados)
      }
      cat("\nRetirados:\n", quienes_retirados) 
      cat("\n\nMesa:\n"); print(partida$mesa)
      cat("\nManos:\n"); print(partida$manos)
      cat("\n--------------------------------------------------------\n")
      quien_gana <- quien_gana(partida, numero_jugadores)  
      quien <- quien_gana$ganador
      por_que <- quien_gana$razon
      cat(quien, "gana por", por_que, "y se lleva un bote de:", bote)
      cat("\n--------------------------------------------------------\n\n")
      
      carteras$cartera[as.numeric(sub(".*\\s","", quien))] <- carteras$cartera[as.numeric(sub(".*\\s","", quien))] + bote/length(as.numeric(sub(".*\\s","", quien)))     
      print(carteras)
      cat("\n\n")
      
      sigue <- readline("¿Sigues jugando? (s/n): ")
      while(!(sigue %in% c("s", "n"))){
        sigue <- readline("¿Sigues jugando? (s/n): ")}                           
      switch(sigue, 
             "s" = {
               carteras <<- carteras
               jugar(reiniciar_carteras = argumentos_jugar$reiniciar_carteras, numero_jugadores = argumentos_jugar$numero_jugadores, cantidad_carteras = argumentos_jugar$cantidad_carteras)
             }, 
             "n" = {
               rm(list = setdiff(ls(envir = .GlobalEnv), lsf.str(envir = .GlobalEnv)), envir = .GlobalEnv) # https://stackoverflow.com/questions/8305754/remove-all-variables-except-functions
               stop("Fin del juego")
             })
      
    }
    
    cat("--- Final del turno", turno, "---\n")
    orden_de_juego <<- orden_de_juego[!(orden_de_juego %in% which(retirados))]          # Se quitan los jugadores retirados de la variable orden_de_juego para que dejen de aparecer en las consecutivas iteraciones del repeat
    
    # Al bote se suman las jugadas de los que no se han retirado, y las jugadas de aquellos que se han retirado en mitad de un turno
    bote <<- bote + sum(jugadas$cantidad_apostada[orden_de_juego]) + sum(cantidad_anterior[retirados], na.rm = TRUE)                      
    print(data.frame(jugador = orden_de_juego,                                          # Se va devolviendo un data.frame con las jugadas al final de cada turno
                     cantidad_apostada = jugadas$cantidad_apostada[orden_de_juego],     # No devuelve la probabilidad de apostar ni los datos de los retirados
                     cartera = carteras$cartera[orden_de_juego]))
  }
}

#### Jugar ####
jugar()
