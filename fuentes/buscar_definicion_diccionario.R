# Función para buscar definiciones en el diccionario médico
buscar_definicion_diccionario <- function(enfermedad) {
  url <- paste0("https://www.cun.es/diccionario-medico/terminos/", enfermedad)
  
  # Se lee la página web.
  pagina <- read_html(url)
  
  # Usando XPath se obtiene el elemento del HTML que contiene el texto con 
  # la definición.
  html_xpath = '//*[@class="textImageComponent textImageComponent"]'
  elemento <- html_element(pagina, xpath = html_xpath)
  
  # Lee el texto del elemento.
  definicion <- html_text2(elemento)
  
  # Si no se encuentra la definición en el diccionario (en ese caso se muestra 
  # el texto de la condicion) se devuelve un string vacío.
  not_found = paste0("ENFERMEDADES Y TRATAMIENTOS\n\nEnfermedades\nPruebas",
                     " diagnósticas\nTratamientos\nCuidados en casa\n",
                     "Chequeos y salud") # Dividimos el texto y lo concatenamos
  # para que no se salga del PDF al
  # compilar el R Markdown
  
  if (definicion == not_found) {
    definicion <- ""
  }
  
  return(definicion)
}