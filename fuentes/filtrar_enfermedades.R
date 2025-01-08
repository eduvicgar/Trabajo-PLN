filtrar_enfermedades <- function(txt) {
  spacy_initialize(model = "es_core_news_sm")
  
  # Definimos patrones de enfermedades
  patrones <- list(
    dolor = "\\b\\w*algia\\b",
    alteracion = "\\b([H|h]ipo|[H|h]iper)\\w*\\b",
    infeccion = "\\b\\w*itis\\b",  
    cancer = "\\b\\w*oma\\b"
  )
  
  clasificar_enfermedades_nouns <- function(patron, columna) {
    # Vector para almacenar las coincidencias encontradas
    enfermedades <- c()
    
    # Itera sobre cada elemento de la columna
    for (texto in columna) {
      # Busca todas las coincidencias del patrón en el texto
      coincidencias <- regmatches(texto, gregexpr(patron, texto, 
                                                  ignore.case = TRUE))[[1]]
      # Agrega las coincidencias encontradas
      if (length(coincidencias) > 0) {
        enfermedades <- c(enfermedades, coincidencias)
      }
    }
    
    # Devuelve las coincidencias
    return(enfermedades)
  }
  
  clasificar_enfermedades <- function(patron, columna) {
    # Filtra los elementos que contienen al menos una palabra que coincida 
    # con el patrón
    textos_filtrados <- columna[grep(patron, columna, ignore.case = TRUE)]
    
    # Devuelve los textos completos que cumplen con el criterio
    return(textos_filtrados)
  }
  
  # Se separa el texto por sintagmas nominales.
  sn <- spacy_extract_nounphrases(txt)$text
  
  # Se filtran los sintagmas nominales que no contengan alguna de las palabras 
  # que buscamos.
  # Inicializamos un vector para almacenar los sintagmas nominales válidos.
  sn_filtrado <- c()
  
  # Por cada sintagma nominal, analizamos sus palabras con spaCy
  for (text in sn) {
    # Parseamos el texto
    parsed_text <- spacy_parse(text)
    
    # Buscamos las palabras que coincidan con el patrón y sean sustantivos 
    # o nombres propios
    palabras_validas <- subset(
      parsed_text,
      grepl("\\b([Hh]ipo\\w*|[Hh]iper\\w*|\\w*itis|\\w*algia|\\w*oma)\\b", token, ignore.case = TRUE)
      & pos %in% c("NOUN", "PROPN")
    )
    
    # Si hay al menos una palabra válida, añadimos el sintagma nominal al filtro
    if (nrow(palabras_validas) > 0) {
      sn_filtrado <- c(sn_filtrado, text)
    }
  }
  
  # Por cada sintagma nominal filtrado, lo parseamos con spacy y nos quedamos 
  # con aquellas palabras que sean sustantivos o nombres propios (a veces spacy 
  # interpreta una enfermedad como nombre propio).
  sn_nouns_filtrado <- c()
  for (text in sn_filtrado) {
    parsed_text <- spacy_parse(text)
    filtrado <- subset(parsed_text, pos %in% c("NOUN", "PROPN"))
    sn_nouns_filtrado <- c(sn_nouns_filtrado, paste(filtrado$token, 
                                                    collapse = " "))
  }
  
  # Clasificamos el vector con los sustantivos según el tipo de enfermedad que 
  # corresponda.
  enfermedades_nouns_clasificadas <- lapply(patrones, function(patron) {
    clasificar_enfermedades_nouns(patron, sn_nouns_filtrado)
  })
  
  # Clasificamos el vector con los sintagmas nominales según el tipo de 
  # enfermedad que corresponda.
  enfermedades_clasificadas <- lapply(patrones, function(patron) {
    clasificar_enfermedades(patron, sn_filtrado)
  })
  
  # Buscamos las definiciones de las enfermedades encontradas para cada 
  # enfermedad con la función buscar_definicion_diccionario
  definiciones <- list()
  
  for (tipo in names(enfermedades_nouns_clasificadas)) {
    definiciones[[tipo]] <- sapply(enfermedades_nouns_clasificadas[[tipo]], 
                                   buscar_definicion_diccionario, 
                                   USE.NAMES = FALSE)
  }
  
  
  # Pasamos las listas a vectores
  enfermedades_nouns_combinadas <- unlist(enfermedades_nouns_clasificadas)
  enfermedades_combinadas <- unlist(enfermedades_clasificadas)
  definiciones_combinadas <- unlist(definiciones)
  
  # Hallamos los lemas de las enfermedades
  lemmas_enfermedades <- spacy_parse(paste(enfermedades_nouns_combinadas, 
                                           collapse = " "))$lemma
  
  spacy_finalize()
  
  # Creamos un dataframe con toda la información obtenida
  df <- data.frame(
    Enfermedad = enfermedades_nouns_combinadas,
    Sintagma_nominal_completo = enfermedades_combinadas,
    Definicion = definiciones_combinadas,
    Lema = lemmas_enfermedades
  )
  
  return(df)
}