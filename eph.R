library(tidyverse)
library(Hmisc)
library(funModeling)
summarize = dplyr::summarise

individual = read_delim('usu_individual_T318.txt', delim = ';')

mercosur_codes=read_csv('mercosur_codes.csv')
caracter_ocupacional_codes=read_csv('caracter_ocupacional.csv')

simple_boolean = function(x) {
  case_when(
    x==1 ~ TRUE,
    x==2 ~ FALSE,
    TRUE ~ NA
  )
}

individual_transf = select(individual,
                           entrevista=H15,
                           trimestre=TRIMESTRE,
                           region=REGION,
                           aglomerado=AGLOMERADO,
                           sexo=CH04,
                           edad=CH06,
                           sistema_salud=CH08,
                           alfabeto=CH09,
                           asistio_escuela=CH10,
                           tipo_escuela=CH11,
                           nivel_educacion=CH12,
                           finalizo_educacion=CH13,
                           ultimoanyo=CH14,
                           nacimiento=CH15,
                           nivel_educativo=NIVEL_ED,
                           tipo_actividad=CAT_OCUP,
                           tipo_inactividad=CAT_INAC,
                           imputados=IMPUTA,
                           estado_actividad=ESTADO,
                           estado_civil=CH07,
                           trabajo_12m=PP02I,
                           trabajo_busco_12m=PP02H,
                           trabajo_busqueda_contactos=PP02C1,
                           trabajo_busqueda_cv_avisos=PP02C2,
                           trabajo_busqueda_sepresento=PP02C3,
                           trabajo_busqueda_emprendimiento=PP02C4,
                           trabajo_busqueda_carteles=PP02C5,
                           trabajo_busqueda_conocidos=PP02C6,
                           trabajo_busqueda_bolsas_trabajo=PP02C7,
                           trabajo_busqueda_otros=PP02C8,
                           trabajo_no_busco_motivo=PP02E,
                           ingreso=PP06C,
                           horas=PP3E_TOT,
                           horas_otros=PP3F_TOT,
                           queria_mas=PP03G,
                           busco_mas_horas=PP03I,
                           busco_otro_trabajo=PP03J,
                           intensidad=INTENSI,
                           tipo_empresa=PP04A,
                           actividad_empresa=PP04B_COD,
                           servicio_domestico_casas=PP04B2,
                           desde_trabaja_anyo=PP04B3_ANO,
                           desde_trabaja_mes=PP04B3_MES,
                           desde_trabaja_dia=PP04B3_DIA,
                           desde_trabaja_indep_anyo=PP05B2_ANO,
                           desde_trabaja_indep_mes=PP05B2_MES,
                           desde_trabaja_indep_dia=PP05B2_DIA,
                           cantidad_empleados_est1=PP04C,
                           cantidad_empleados_est2=PP04C99,
                           asalariado_antiguedad=PP07A,
                           asalariado_temporal=PP07C,
                           asalariado_temporal_tiempo=PP07D,
                           asalariado_tipo=PP07E,
                           trabajo_incluye_comida=PP07F1,
                           trabajo_incluye_vivienda=PP07F2,
                           trabajo_incluye_mercaderia=PP07F3,
                           trabajo_incluye_otros_beneficios=PP07F4,
                           trabajo_incluye_vacaciones_pagas=PP07G1,
                           trabajo_incluye_aguinaldo=PP07G2,
                           trabajo_incluye_dias_enfermedad=PP07G3,
                           trabajo_incluye_obra_social=PP07G4,
                           trabajo_incluye_jubilacion=PP07H,
                           aporta_independiente_jubilacion=PP07I,
                           trabajo_turno_habitual=PP07J,
                           trabajo_forma_pago=PP07K,
                           salario=P21,
                           cantidad_ocupaciones=PP03D,
                           codigo_ocupacion=PP04D_COD,
                           ponderador_ocup_principal=PONDIIO
) %>%
  mutate_at(vars(queria_mas, busco_otro_trabajo, busco_mas_horas, entrevista, trabajo_12m, trabajo_busco_12m, trabajo_busqueda_contactos, trabajo_busqueda_cv_avisos, trabajo_busqueda_sepresento, trabajo_busqueda_emprendimiento, trabajo_busqueda_carteles, trabajo_busqueda_conocidos, trabajo_busqueda_bolsas_trabajo, trabajo_busqueda_otros, asalariado_temporal,
                 trabajo_incluye_comida, trabajo_incluye_vivienda, trabajo_incluye_mercaderia, trabajo_incluye_otros_beneficios, trabajo_incluye_vacaciones_pagas, trabajo_incluye_aguinaldo, trabajo_incluye_dias_enfermedad, trabajo_incluye_obra_social, trabajo_incluye_jubilacion, aporta_independiente_jubilacion), simple_boolean) %>%
  mutate(
    sexo=case_when(
      sexo==1 ~ "hombre",
      sexo==2 ~ "mujer"
    ),
    estado_actividad=as_factor(case_when(
      estado_actividad == 0 ~ "Entrevista individual no realizada",
      estado_actividad == 1 ~ "Ocupado",
      estado_actividad == 2 ~ "Desocupado",
      estado_actividad == 3 ~ "Inactivo",
      estado_actividad == 4 ~ "Menor de 10 años"
    )),
    tipo_actividad=as_factor(case_when(
      tipo_actividad == 1 ~ "Patrón",
      tipo_actividad == 2 ~ "Cuenta propia",
      tipo_actividad == 3 ~ "Obrero o empleado",
      tipo_actividad == 4 ~ "Trabajador familiar sin remuneración",
      TRUE ~ "otro"
    )),
    tipo_inactividad=as_factor(case_when(
      tipo_inactividad==1 ~ "Jubilado/ Pensionado",
      tipo_inactividad==2 ~ "Rentista",
      tipo_inactividad==3 ~ "Estudiante",
      tipo_inactividad==4 ~ "Ama de casa",
      tipo_inactividad==5 ~ "Menor de 6 años",
      tipo_inactividad==6 ~ "Discapacitado",
      tipo_inactividad==7 ~ "Otros"
    )),
    
    intensidad=as_factor(case_when(
      intensidad==1 ~ "Subocupado por insuficiencia horaria",
      intensidad==2 ~ "Ocupado pleno",
      intensidad==3 ~ "Sobreocupado",
      intensidad==4 ~ "Ocupado que no trabajó en la semana",
      TRUE ~ "otro"
    )),
    tipo_empresa=as_factor(case_when(
      tipo_empresa==1 ~ "Estatal",
      tipo_empresa==2 ~ "Privada",
      tipo_empresa==3 ~ "Otra",
      TRUE ~ "otro"
    )),
    nivel_educacion=case_when(
      nivel_educacion==1 ~ "Jardín/Preescolar",
      nivel_educacion==2 ~ "Primario",
      nivel_educacion==3 ~ "EGB",
      nivel_educacion==4 ~ "Secundario",
      nivel_educacion==5 ~ "Polimodal",
      nivel_educacion==6 ~ "Terciario",
      nivel_educacion==7 ~ "Universitario",
      nivel_educacion==8 ~ "Posgrado Univ.",
      nivel_educacion==9 ~ "Educación especial (discapacitado)",
      TRUE ~ "otro"
    ),
    # Antiguedad de trabajador en relacion de dependencia
    desde_trabaja_anyo = as.numeric(ifelse(desde_trabaja_anyo==99, as.integer(NA),desde_trabaja_anyo)),
    desde_trabaja_mes = as.numeric(ifelse(desde_trabaja_mes==99, as.integer(NA),desde_trabaja_mes)),
    desde_trabaja_dia = as.numeric(ifelse(desde_trabaja_dia==99, as.integer(NA),desde_trabaja_dia)),
    # Antiguedad de trabajador independiente
    antiguedad_trabajo_meses = (coalesce(desde_trabaja_anyo, 0) * 12) + (coalesce(desde_trabaja_mes, 0)) + round(desde_trabaja_dia/30),
    desde_trabaja_indep_anyo = as.numeric(ifelse(desde_trabaja_indep_anyo==99, as.integer(NA),desde_trabaja_indep_anyo)),
    desde_trabaja_indep_mes = as.numeric(ifelse( desde_trabaja_indep_mes==99, as.integer(NA), desde_trabaja_indep_mes)),
    desde_trabaja_indep_dia = as.numeric(ifelse( desde_trabaja_indep_dia==99, as.integer(NA), desde_trabaja_indep_dia)),
    antiguedad_trabajo_meses = (coalesce( desde_trabaja_indep_anyo, 0) * 12) + (coalesce( desde_trabaja_indep_mes, 0)) + round(desde_trabaja_indep_dia/30),
    cant_empleados_min=case_when(
      cantidad_empleados_est1==1 ~ 1,
      cantidad_empleados_est1==2 ~ 2,
      cantidad_empleados_est1==3 ~ 3,
      cantidad_empleados_est1==4 ~ 4,
      cantidad_empleados_est1==5 ~ 5,
      cantidad_empleados_est1==6 ~ 6,
      cantidad_empleados_est1==7 ~ 11,
      cantidad_empleados_est1==8 ~ 26,
      cantidad_empleados_est1==9 ~ 41,
      cantidad_empleados_est1==10 ~ 101,
      cantidad_empleados_est1==11 ~ 201,
      cantidad_empleados_est1==12 ~ 500,
      cantidad_empleados_est2==1 ~ 1,
      cantidad_empleados_est2==2 ~ 6,
      cantidad_empleados_est2==3 ~ 40,
      TRUE ~ as.numeric(NA)
    ),
    cant_empleados_max=case_when(
      cantidad_empleados_est1==1 ~ 1,
      cantidad_empleados_est1==2 ~ 2,
      cantidad_empleados_est1==3 ~ 3,
      cantidad_empleados_est1==4 ~ 4,
      cantidad_empleados_est1==5 ~ 5,
      cantidad_empleados_est1==6 ~ 10,
      cantidad_empleados_est1==7 ~ 25,
      cantidad_empleados_est1==8 ~ 40,
      cantidad_empleados_est1==9 ~ 100,
      cantidad_empleados_est1==10 ~ 200,
      cantidad_empleados_est1==11 ~ 500,
      cantidad_empleados_est2==1 ~ 5,
      cantidad_empleados_est2==2 ~ 40,
      TRUE ~ as.numeric(NA)
    ),
    # Si hay dos puntas (Ej: Entre 100 y 200) tomamos el punto medio (Ej: 150), si no esta la punta superior, tomamos la cota inferior multiplicado por 1.5, ya que es "desde", para representar que hay casos superiores a tal cota inferior.
    cant_empleados_estimados=ifelse(
      is.na(cant_empleados_max),cant_empleados_min*1.5,
      round((cant_empleados_max+cant_empleados_min)/2)
    ),
    asalariado_antiguedad = case_when(
      asalariado_antiguedad == 1 ~ "menos de 1 mes",
      asalariado_antiguedad == 2 ~ "1 a 3 meses",
      asalariado_antiguedad == 3 ~ "de 3 a 6 meses",
      asalariado_antiguedad == 4 ~ "de 6 a 12 meses",
      asalariado_antiguedad == 5 ~ "de 1 a 5 años",
      asalariado_antiguedad == 6 ~ "más de 5 años",
      TRUE ~ "otro"
    ),
    asalariado_temporal_tiempo = case_when(
      asalariado_temporal_tiempo == 1 ~ "solo fue esa vez/sólo cuando lo llaman",
      asalariado_temporal_tiempo == 2  ~ "hasta 3 meses",
      asalariado_temporal_tiempo == 3  ~ "de 3 a 6 meses",
      asalariado_temporal_tiempo == 4  ~ "de 6 a 12 meses",
      asalariado_temporal_tiempo == 5  ~ "más de 1 año",
      TRUE ~ "otro"
    ),
    asalariado_tipo = case_when(
      asalariado_tipo == 1 ~ "Plan de empleo",
      asalariado_tipo == 2  ~ "Periodo de prueba",
      asalariado_tipo == 3  ~ "Beca/Pasantia/Aprendizaje",
      TRUE  ~ "Ninguno de estos"
    ),
    trabajo_turno_habitual=case_when(
      trabajo_turno_habitual == 1 ~ "De dia",
      trabajo_turno_habitual == 2  ~ "De noche",
      trabajo_turno_habitual == 3  ~ "Otro (Guardia, rotativo, etc)",
      TRUE ~ "otro"
    ),
    trabajo_forma_pago=case_when(
      trabajo_forma_pago == 1 ~ "Recibo del empleador",
      trabajo_forma_pago == 2  ~ "Papel/Recibo precario",
      trabajo_forma_pago == 3  ~ "Entrega factura",
      trabajo_forma_pago == 4  ~ "Sin papeles",
      trabajo_forma_pago == 5  ~ "Ad-honorem",
      TRUE ~ "otro"
    ),
    codigo_ocupacion=ifelse(is.na(codigo_ocupacion), '     ', as.character(codigo_ocupacion)),
    ocupacion_jerarquia=as_factor(case_when(
      substring(codigo_ocupacion, 3,3) == '0' ~ "Director",
      substring(codigo_ocupacion, 3,3) == '1' ~ "Cuenta propia",
      substring(codigo_ocupacion, 3,3) == '2' ~ "Jefe",
      substring(codigo_ocupacion, 3,3) == '3' ~ "Trabajador asalariado",
      TRUE  ~ "otro"
      
    )),
    ocupacion_tecnologia=as_factor(case_when(
      substring(codigo_ocupacion, 4,4) == '0' ~ "Sin maquinaria",
      substring(codigo_ocupacion, 4,4) == '1' ~ "Operacion de maquinaria y equipos electromecanicos",
      substring(codigo_ocupacion, 4,4) == '2' ~ "Operacion de sistemas y equipos informatizados",
      TRUE ~ "otro"
    )),
    ocupacion_calificacion=as_factor(case_when(
      substring(codigo_ocupacion, 5,5) == '0' ~ "Profesionales",
      substring(codigo_ocupacion, 5,5) == '1' ~ "Tecnicos",
      substring(codigo_ocupacion, 5,5) == '2' ~ "Operativos",
      substring(codigo_ocupacion, 5,5) == '3' ~ "No calificados",
      TRUE ~ "otro"
    )),
    
    alfabeto=case_when(
      alfabeto == 1 ~ "Si",
      alfabeto == 2  ~ "No",
      alfabeto == 3  ~ "menor 2 años",
      TRUE  ~ "otro" 
    ),
    
    asistio_escuela=case_when(
      asistio_escuela == 1 ~ "Si",
      asistio_escuela == 2  ~ "No (pero asistió)",
      asistio_escuela == 3  ~ "nunca",
      TRUE  ~ "otro" 
    ),
    
    finalizo_educacion=case_when(
      asistio_escuela == 1 ~ "Si",
      asistio_escuela == 2  ~ "No (pero asistió)",
      asistio_escuela == 9  ~ "Ns/Nr",
      TRUE  ~ "otro" 
    ),
    
    sistema_salud=case_when(
      sistema_salud == 1 ~ "Obra social (incluye PAMI)",
      sistema_salud == 2  ~ "Mutual / Prepaga / Servicio de emergencia",
      sistema_salud == 3  ~ "Planes y seguros públicos",
      sistema_salud == 4  ~ "No paga ni le descuentan",
      sistema_salud == 9  ~ "Ns./Nr.",
      sistema_salud == 12  ~ "Obra social y mutual/prepaga/servicio de emergencia.",
      sistema_salud == 13  ~ "Obra social y Planes y Seguros Públicos",
      sistema_salud == 23  ~ "Mutual/prepaga/servicio de emergencia/ Planes y Seguros Públicos",
      sistema_salud == 123  ~ "obra social, mutual/prepaga/servicio de emergencia y Planes y Seguros Públicos",
      TRUE  ~ "otro" 
    ),
    
    
    nivel_educativo=case_when(
      nivel_educativo == 1 ~ "Primaria Incompleta(incluye educación especial)",
      nivel_educativo == 2  ~ "Primaria Completa",
      nivel_educativo == 3  ~ "Secundaria Incompleta",
      nivel_educativo == 4  ~ "Secundaria Completa",
      nivel_educativo == 5  ~ "Superior Universitaria Incompleta",
      nivel_educativo == 6  ~ "Superior Universitaria Completa",
      nivel_educativo == 7  ~ "Sin instrucción",
      nivel_educativo == 9  ~ "Ns./ Nr.",
      TRUE  ~ "otro" 
    ),
    
    estado_civil=case_when(
      estado_civil == 1 ~ "unido",
      estado_civil == 2  ~ "casado",
      estado_civil == 3  ~ "separado/a ó divorciado/a",
      estado_civil == 4  ~ "viudo/a",
      estado_civil == 5  ~ "soltero/a",
      TRUE  ~ "otro" 
    ),
    
    
    ocupacion_caracter_codigo=substring(codigo_ocupacion, 1,2),
    aglomerado=as_factor(case_when(
      aglomerado == 02 ~ "Gran La Plata",
      aglomerado == 03 ~ "Bahía Blanca - Cerri",
      aglomerado == 04 ~ "Gran Rosario",
      aglomerado == 05 ~ "Gran Santa Fé",
      aglomerado == 06 ~ "Gran Paraná",
      aglomerado == 07 ~ "Posadas",
      aglomerado == 08 ~ "Gran Resistencia",
      aglomerado == 09 ~ "Cdro. Rivadavia - R.Tilly",
      aglomerado == 10 ~ "Gran Mendoza",
      aglomerado == 12 ~ "Corrientes",
      aglomerado == 13 ~ "Gran Córdoba",
      aglomerado == 14 ~ "Concordia",
      aglomerado == 15 ~ "Formosa",
      aglomerado == 17 ~ "Neuquén – Plottier",
      aglomerado == 18 ~ "S.del Estero - La Banda",
      aglomerado == 19 ~ "Jujuy - Palpalá",
      aglomerado == 20 ~ "Río Gallegos",
      aglomerado == 22 ~ "Gran Catamarca",
      aglomerado == 23 ~ "Salta",
      aglomerado == 25 ~ "La Rioja",
      aglomerado == 26 ~ "San Luis - El Chorrillo",
      aglomerado == 27 ~ "Gran San Juan",
      aglomerado == 29 ~ "Gran Tucumán - T. Viejo",
      aglomerado == 30 ~ "Santa Rosa - Toay",
      aglomerado == 31 ~ "Ushuaia - Río Grande",
      aglomerado == 32 ~ "Ciudad de Bs As",
      aglomerado == 33 ~ "Partidos del GBA",
      aglomerado == 34 ~ "Mar del Plata - Batán",
      aglomerado == 36 ~ "Río Cuarto",
      aglomerado == 38 ~ "San Nicolás – Villa Constitución",
      aglomerado == 91 ~ "Rawson – Trelew",
      aglomerado == 93 ~ "Viedma – Carmen de Patagones"
    ))
    
  ) %>%
  left_join(select(mercosur_codes, clase_code, seccion, division, clase), by=c("actividad_empresa"="clase_code")) %>%
  left_join(caracter_ocupacional_codes, by=c("ocupacion_caracter_codigo"="caracter_codigo")) %>%
  select(-starts_with('desde_trabaja_'),
         -cantidad_empleados_est1, -cantidad_empleados_est2,
         -actividad_empresa, -ocupacion_caracter_codigo) %>%
  rename(actividad_empresa=clase, seccion_empresa=seccion, division_empresa=division) %>%
  mutate(actividad_empresa=as_factor(actividad_empresa))



############################################################
write_delim(individual_transf, "eph_individual.csv", delim = ";")
