# Librerias ---------------------------------------------------------------

library(tidyverse)
library(xlsx)

# Elegir tipo de prueba ---------------------------------------------------

ID_de_prueba = "MLOps"

# Definir ruta del dataset ------------------------------------------------

if (ID_de_prueba == "DS") {
    dataset_file = "files/datasets/input/Prueba teórica para Data Scientists.xlsx"
} else if (ID_de_prueba == "DA") {
    dataset_file = "files/datasets/input/Assessment Data Analyst V 0.2.xlsx"
} else if (ID_de_prueba == "MLOps") {
    dataset_file = "files/datasets/input/Prueba de selección MLOps Engineer.xlsx"
} else if (ID_de_prueba == "BA") {
    dataset_file = "files/datasets/input/Prueba de conocimientos generales de Analytics.xlsx"
} else if (ID_de_prueba == "test") {
    dataset_file = "files/datasets/input/prueba_de_test.xlsx"
} else if (ID_de_prueba == "ejecucion_especial") {
    dataset_file = "files/datasets/input/Evaluación módulo 2 y 3 DA Training.xlsx"
}

# Leer datos --------------------------------------------------------------

respuestas_prueba =
    xlsx::read.xlsx(file = dataset_file, sheetName = "Respuestas")
variables_prueba =
    xlsx::read.xlsx(file = dataset_file, sheetName = "Variables")

# Variables ---------------------------------------------------------------

obtener_valor_de_variable =
    function(nombre_variable,
             dataset_variables_prueba = variables_prueba) {
        dataset_variables_prueba |>
            filter(variable == nombre_variable) |>
            select(valor) |>
            unlist() |>
            unname() |>
            toolkitEntel::cleanString() |>
            tolower() |>
            rlang::sym()
    }

col_nombre_sym = obtener_valor_de_variable(nombre_variable = "nombre")
col_primera_sym = obtener_valor_de_variable(nombre_variable = "primera_pregunta")
col_ultima_sym = obtener_valor_de_variable(nombre_variable = "ultima_pregunta")
col_e_mail_sym = obtener_valor_de_variable(nombre_variable = "e_mail")
col_start_time_sym = obtener_valor_de_variable(nombre_variable = "start_time")
col_completion_time_sym = obtener_valor_de_variable(nombre_variable = "completion_time")
col_last_modification_time_sym = obtener_valor_de_variable(nombre_variable = "last_modification_time")

nombre_pauta_sym = obtener_valor_de_variable(nombre_variable = "nombre_pauta")
nombre_todas_las_respuestas_sym = obtener_valor_de_variable(nombre_variable = "nombre_todas_las_respuestas")

# Limpiar columnas --------------------------------------------------------

respuestas_prueba_colnames =
    respuestas_prueba |>
    colnames() |>
    toolkitEntel::cleanString() |>
    tolower() |>
    str_remove_all("^x_")

colnames(respuestas_prueba) = respuestas_prueba_colnames

colnames_duplicates =
    respuestas_prueba |>
    colnames() |>
    duplicated()

exist_colnames_duplicates =
    colnames_duplicates |>
    any()

while (exist_colnames_duplicates) {
    for (column_id in (1:length(colnames_duplicates))[colnames_duplicates]) {
        colnames(respuestas_prueba)[column_id] =
            paste0(colnames(respuestas_prueba)[column_id], "_1")
        colnames_duplicates =
            respuestas_prueba |>
            colnames() |>
            duplicated()
        
        exist_colnames_duplicates =
            colnames_duplicates |>
            any()
    }
}

# Definir cuál es la pauta y columnas a evaluar ---------------------------

pauta =
    respuestas_prueba |>
    as.data.frame() |>
    filter(!!col_nombre_sym == nombre_pauta_sym)

todas_las_respuestas =
    respuestas_prueba |>
    as.data.frame() |>
    filter(!!col_nombre_sym == nombre_todas_las_respuestas_sym)

columnas_a_evaluar =
    respuestas_prueba |>
    select(!!col_primera_sym:!!col_ultima_sym) |>
    colnames()

cantidad_de_preguntas =
    columnas_a_evaluar |>
    length()

# Función para evaluar a un postulante ------------------------------------

# id_postulante_a_evaluar = 3 # DEBUG
# column = columnas_a_evaluar[[25]] # DEBUG

evaluar_postulante = function(id_postulante_a_evaluar,
                              respuestas_prueba) {
    respuestas_correctas = 0
    respuestas_omitidas = 0
    suma_de_porcentajes_de_respuesta_correcta = 0
    postulante_a_evaluar = respuestas_prueba[id_postulante_a_evaluar, ]
    
    # Función para calcular porcentaje de acierto por pregunta ----------------
    
    calcular_porcentaje_de_acierto_en_pregunta =
        function(column) {
            # Respuestas seleccionadas ------------------------------------------------
            
            obtener_alternativas_sin_vacios =
                function(fila_del_dataset) {
                    alternativas_con_posibles_vacios =
                        fila_del_dataset[[column]] |>
                        str_remove(";$") |>
                        str_split(pattern = ";") |>
                        unlist()
                    
                    alternativas_sin_vacios =
                        alternativas_con_posibles_vacios[alternativas_con_posibles_vacios != ""]
                    
                    return(alternativas_sin_vacios)
                }
            
            vector_respuestas_postulante = obtener_alternativas_sin_vacios(fila_del_dataset = postulante_a_evaluar)
            vector_respuestas_pauta = obtener_alternativas_sin_vacios(fila_del_dataset = pauta)
            vector_todas_las_respuestas = obtener_alternativas_sin_vacios(fila_del_dataset = todas_las_respuestas)
            
            total_de_alternativas_en_pregunta =
                vector_todas_las_respuestas |>
                length()
            
            total_de_alternativas_correctas_en_pregunta =
                vector_respuestas_pauta |>
                length()
            total_de_alternativas_incorrectas_en_pregunta = total_de_alternativas_en_pregunta - total_de_alternativas_correctas_en_pregunta
            
            alternativas_correctamente_seleccionadas =
                vector_respuestas_postulante |>
                intersect(vector_respuestas_pauta) |>
                length()
            alternativas_incorrectamente_seleccionadas =
                vector_respuestas_postulante |>
                setdiff(vector_respuestas_pauta) |>
                length()
            
            # Respuestas no seleccionadas ---------------------------------------------
            
            if (total_de_alternativas_en_pregunta > 1) {
                # Las alternativas no seleccionadas solo importan cuando la pregunta es de selección múltiple. Cuando el total de alternativas posibles a enviar es 1, no nos interesan las alternativas no marcadas
                
                vector_respuestas_no_seleccionadas_pauta =
                    vector_todas_las_respuestas |>
                    setdiff(vector_respuestas_pauta)
                
                vector_respuestas_no_seleccionadas_postulante =
                    vector_todas_las_respuestas |>
                    setdiff(vector_respuestas_postulante)
                
                alternativas_correctamente_no_seleccionadas =
                    vector_respuestas_no_seleccionadas_postulante |>
                    intersect(vector_respuestas_no_seleccionadas_pauta) |>
                    length()
                alternativas_incorrectamente_no_seleccionadas =
                    vector_respuestas_no_seleccionadas_postulante |>
                    setdiff(vector_respuestas_no_seleccionadas_pauta) |>
                    length()
                
                
            } else{
                alternativas_correctamente_no_seleccionadas = 0
            }
            
            
            # Porcentaje de acierto en pregunta ---------------------------------------
            
            if (length(vector_respuestas_postulante) == 0) {
                porcentaje_de_acierto_en_pregunta = 0
            } else{
                porcentaje_de_acierto_en_pregunta = (
                    alternativas_correctamente_seleccionadas + alternativas_correctamente_no_seleccionadas
                ) /
                    total_de_alternativas_en_pregunta
            }
            
            return(porcentaje_de_acierto_en_pregunta)
            
        }
    
    
    
    
    # Iteracion por pregunta --------------------------------------------------
    
    for (column in columnas_a_evaluar) {
        if (is.na(postulante_a_evaluar[column])) {
            postulante_a_evaluar[column] = "" # Esto es para evitar comparar un string con NA, lo que produce NA y produce una cantidad de respuestas correctas NA
            respuestas_omitidas = respuestas_omitidas + 1
        }
        respuesta_correcta = postulante_a_evaluar[column] ==  pauta[column]
        respuestas_correctas = respuestas_correctas + respuesta_correcta[1]
        
        suma_de_porcentajes_de_respuesta_correcta = suma_de_porcentajes_de_respuesta_correcta + calcular_porcentaje_de_acierto_en_pregunta(column)
    }
    
    respuestas_respondidas = cantidad_de_preguntas - respuestas_omitidas
    
    porcentaje_de_acierto_absoluto = respuestas_correctas / cantidad_de_preguntas
    porcentaje_de_acierto_entre_las_respondidas = respuestas_correctas / respuestas_respondidas
    nota_de_1_a_7 = 1 + porcentaje_de_acierto_absoluto * 6
    porcentaje_de_acierto_parcial = suma_de_porcentajes_de_respuesta_correcta / cantidad_de_preguntas
    
    
    
    respuestas_prueba[id_postulante_a_evaluar, col_nombre_sym |> as.character()]
    
    col_nombre_sym
    
    
    nombre = respuestas_prueba[id_postulante_a_evaluar, col_nombre_sym |> as.character()]
    e_mail = respuestas_prueba[id_postulante_a_evaluar, col_e_mail_sym |> as.character()]
    start_time = respuestas_prueba[id_postulante_a_evaluar, col_start_time_sym |> as.character()]
    completion_time = respuestas_prueba[id_postulante_a_evaluar, col_completion_time_sym |> as.character()]
    last_modified_time = respuestas_prueba[id_postulante_a_evaluar, col_last_modification_time_sym |> as.character()]
    
    delta_time = difftime(completion_time, start_time, units =
                              'mins')
    
    return_list = list(
        nombre = nombre,
        e_mail = e_mail,
        nota_de_1_a_7 = nota_de_1_a_7,
        start_time = start_time,
        completion_time = completion_time,
        delta_time = delta_time,
        last_modified_time = last_modified_time,
        porcentaje_de_acierto_absoluto = porcentaje_de_acierto_absoluto,
        porcentaje_de_acierto_parcial = porcentaje_de_acierto_parcial,
        porcentaje_de_acierto_entre_las_respondidas = porcentaje_de_acierto_entre_las_respondidas,
        respuestas_correctas = respuestas_correctas,
        respuestas_respondidas = respuestas_respondidas,
        respuestas_omitidas = respuestas_omitidas
        
    )
    
    return(return_list)
}

# Aplicación de evaluación y guardado de output ---------------------------

tabla_de_evaluaciones =
    1:dim(respuestas_prueba)[1] |>
    map_df(evaluar_postulante,
           respuestas_prueba = respuestas_prueba,
           .progress = T) |>
    as.data.frame()

tabla_de_evaluaciones

tabla_de_evaluaciones |>
    data.table::fwrite(paste0(
        "files/datasets/output/tabla_de_evaluaciones_",
        ID_de_prueba,
        ".csv"
    ))

# Obtener respuestas erradas ----------------------------------------------

respuestas_erradas_prueba = respuestas_prueba
respuestas_separadas_en_vector =
    respuestas_prueba |>
    mutate_at(.vars = columnas_a_evaluar, .funs = ~ strsplit(., ";"))

for (fila in 1:nrow(respuestas_separadas_en_vector)) {
    for (pregunta in columnas_a_evaluar) {
        
        respuestas_correctas_de_fila_columna =
            respuestas_separadas_en_vector |>
            filter(!!col_nombre_sym == nombre_pauta_sym) |>
            select(all_of(pregunta)) |>
            unlist() |>
            unname()
        
        respuestas_respondidas_de_fila_columna =
            respuestas_separadas_en_vector[fila , pregunta] |>
            unlist() |>
            unname()
        
        respuestas_incorrectas =
            union(
                setdiff(
                    respuestas_respondidas_de_fila_columna,
                    respuestas_correctas_de_fila_columna
                ),
                setdiff(
                    respuestas_correctas_de_fila_columna,
                    respuestas_respondidas_de_fila_columna
                )
            )
        
        respuestas_erradas_prueba[fila , pregunta] =
            respuestas_incorrectas |>
            paste(collapse = ", ")
        
    }
}

# Conteo de preguntas totales ---------------------------------------------

conteo_de_preguntas_totales = 
    respuestas_separadas_en_vector |> 
    filter(!!col_nombre_sym == nombre_todas_las_respuestas_sym) |> 
    mutate_at(.vars = columnas_a_evaluar, .funs = ~ length(unlist(.))) |> 
    mutate(!!col_nombre_sym := "conteo_de_preguntas_totales")

respuestas_erradas_prueba_vectorizadas =
    respuestas_erradas_prueba |> 
    mutate_at(.vars = columnas_a_evaluar, .funs = ~ strsplit(., ",")) 

# Conteo de preguntas erradas ---------------------------------------------

cantidad_de_preguntas_erradas = respuestas_erradas_prueba_vectorizadas
porcentaje_de_preguntas_erradas = respuestas_erradas_prueba_vectorizadas 

for (fila in 1:nrow(respuestas_separadas_en_vector)) {
    for (pregunta in columnas_a_evaluar) {
        
        cantidad_de_preguntas_erradas[fila , pregunta] =
            respuestas_erradas_prueba_vectorizadas[fila , pregunta] |>
            unlist() |>
            unname() |> 
            length()
    }
}

cantidad_de_preguntas_erradas =
    cantidad_de_preguntas_erradas |> 
    mutate_at(columnas_a_evaluar, unlist)


for (fila in 1:nrow(respuestas_separadas_en_vector)) {
    for (pregunta in columnas_a_evaluar) {

        porcentaje_de_preguntas_erradas[fila , pregunta] = 
            cantidad_de_preguntas_erradas[fila , pregunta] / conteo_de_preguntas_totales[1 , pregunta]
    }
}

# Guardar datasets --------------------------------------------------------

respuestas_erradas_prueba |>
    data.table::fwrite(paste0(
        "files/datasets/output/respuestas_erradas_prueba_",
        ID_de_prueba,
        ".csv"
    ))

cantidad_de_preguntas_erradas |>
    data.table::fwrite(paste0(
        "files/datasets/output/cantidad_de_preguntas_erradas_",
        ID_de_prueba,
        ".csv"
    ))

porcentaje_de_preguntas_erradas |>
    data.table::fwrite(paste0(
        "files/datasets/output/porcentaje_de_preguntas_erradas_",
        ID_de_prueba,
        ".csv"
    ))
