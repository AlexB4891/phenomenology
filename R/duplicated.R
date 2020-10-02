#' Creación de la tabla de muestras
#'
#' @param base_prec Base después de precisión
#' @param identificador_unico Nombre del identificador único
#'
#' @return Lista con dos bases:
#' * `1` Para los que tienen muestra
#' * `0` Para los que no tienen muestra
#'
#' @import tidyverse
#' @examples
#'
#' f_cubo_muestras(viepi_prec)
f_cubo_muestras <- function(base_prec = NULL,
                             identificador_unico = character()){

  col_ini <- c("M_C_ademuestra",
               "M_C_lab_tomo",
               "M_C_resulcovid",
               "M_C_tmuestra",
               "M_F_entmuestra",
               "M_F_promuestra",
               "M_F_recmuestra",
               "M_F_tomamuestra",
               "M_I_lab_proc")

 cubo_muestras <-  col_ini %>%
    map(~{

      expr <- .x

      reducida <- base_prec %>%
        select(identificador_unico,
               P_C_muestra,
               P_S_diagnos1,
               # "M_N_numero",
               starts_with(!!expr)) %>%
        mutate_all(as.character)

      if(ncol(reducida) > 3){
        reducida <- reducida %>%
          gather(key = "M_N_numero",
                 value = !!expr,
                 -identificador_unico,
                 -P_C_muestra,
                 -P_S_diagnos1) %>%
          mutate(M_N_numero = str_extract(M_N_numero,"[:digit:]"))
      }else{
        reducida <- NULL
      }

      return(reducida)

    }) %>%
   discard(~is.null(.))

 cubo_muestras <- cubo_muestras %>%
   reduce(inner_join)

 cubo_muestras <- cubo_muestras %>%
   split(.$P_C_muestra)


 return(cubo_muestras)
}


#'  Construcción del id de muestra utilizando ventanas temporales
#'
#' @param lista elemento resultado de la función `f_cubo_muestras`
#'
#' @return Base con identificador de casos unicos por ventanas temporales:
#' * Muestra t:
#'   * Resultado: Negativo.
#'     * Ventana: 14
#'   * Resultado: Positivo.
#'     * Ventana: 30
#' * Muestra t + 1:
#'   * Nueva variable: `variable_fin_caso` = Fecha toma muestra t + Ventana
#'   * Condición: Fecha toma muestra t+1 <= `variable_fin_caso` o mismo caso epidemiologico.
#'     * TRUE:
#'       * Resultado: Negativo.
#'         * Ventana: 14
#'       * Resultado: Positivo.
#'         * Ventana: 30
#'       * ID: Se mantiene el mismo `id` de caso.
#'     * FALSE:
#'       * ID: Se actualiza el `id` de caso.
#'       * Se actualizan las ventanas de acuerdo al resultado de la prueba
#'       * Se recalculan las fechas de fin de caso.
#'   * Resultado: Positivo.
#'     * Ventana: 30
#' @export
#'
#' @examples
construct_id_caso <- function(lista){

  base_muestras <- lista[["1"]]

  estado_1 <- base_muestras %>%
    mutate(M_N_numero = as.numeric(M_N_numero)) %>%
    group_by(variable_ident,P_S_diagnos1) %>%
    arrange(M_N_numero) %>%
    mutate_at(.vars = vars(matches(".*_F_.*")),
              list(~ymd(.))) %>%
    mutate(ventana = case_when(M_C_resulcovid == "1" ~ 30L,
                               M_C_resulcovid == "0" ~ 14L,
                               TRUE ~ NA_integer_),
           fin_caso = M_F_tomamuestra + ventana,
           id_caso = case_when(M_N_numero == min(M_N_numero) ~ 1,
                               TRUE ~ NA_real_ ))

  estado_2 <- estado_1 %>%
    mutate(
      nuevo_caso = (M_F_tomamuestra < lag(fin_caso,n = 1)),
      id_caso = case_when(nuevo_caso ~ lag(id_caso),
                          TRUE ~ lag(id_caso) + 1 ),
      ventana = case_when(nuevo_caso & M_C_resulcovid == "1" ~ 30L,
                          nuevo_caso & M_C_resulcovid == "0" ~ 14L,
                          TRUE ~ NA_integer_ ),
      fin_caso = M_F_tomamuestra + ventana
    )

  result <- list(estado_1,estado_2)

  return(result)
}

construct_id_caso(lista = lista)

