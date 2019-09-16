---
author: "Laboratorio Social - INEC"
date: "16/7/2019"
output:
  html_document:
    toc: true
    toc_collapsed: false
    toc_depth: 6
---


***
## Ficha Metodológica
***

#### Nombre del Indicador:
Número de muertes neonatales

***
#### Definición:
El Número de muertes neonatales se define como el número de casos de defunciones de neonatos, niña/os menores de un 27 de días de edad en un periodo determinado. 

***
#### Periodo:
2013 - 2017

***

#### Fuente:
- Base de datos de Defunciones Generales
- 

***

***
#### Fórmula:

$$ MN^t = \sum_i^n{d_{i<=27d}^t} $$

- $TMI^t$    : Número de muertes neonatales en el periodo **t**.

- $d_{i<=27d}^t$: Defunción de neonato **i**, niña/o menor de 27 díasde edad, en el periodo **t**.

- $i$        :  Neonato **i**.


***

#### Desagregaciones:





<table class="table table-striped table-hover table-condensed table-responsive" style="font-size: 10px; width: auto !important; margin-left: auto; margin-right: auto;">
<caption style="font-size: initial !important;">Variables de Grupo</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Variable </th>
   <th style="text-align:left;"> Descripcion </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> area_fall </td>
   <td style="text-align:left;"> Área de fallecimiento </td>
  </tr>
  <tr>
   <td style="text-align:left;"> area_res </td>
   <td style="text-align:left;"> Área de residencia habitual del fallecido </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cant_fall </td>
   <td style="text-align:left;"> Cantón de fallecimiento </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cant_res </td>
   <td style="text-align:left;"> Cantón de residencia habitual del fallecido </td>
  </tr>
  <tr>
   <td style="text-align:left;"> etnia </td>
   <td style="text-align:left;"> Autoidentificación étnica </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prov_fall </td>
   <td style="text-align:left;"> Provincia de fallecimiento </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prov_res </td>
   <td style="text-align:left;"> Provincia de residencia habitual del fallecido </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sexo </td>
   <td style="text-align:left;"> Sexo </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tiempo </td>
   <td style="text-align:left;"> Mes-anio </td>
  </tr>
</tbody>
</table>

***


***

##### Tags:


   **Objetivos de Desarrollo Sostenible:**

    Objetivo 3. Salud y Bienestar

  **Plan Nacional de Desarrollo:**

    Objetivo 1. Garantizar una vida digna con iguales oportunidades para todas las personas.


  **Convención sobre los Derechos del Nino:**
  
    Artículo 6. Los Estados Partes garantizarán en la máxima medida posible la supervivencia y el desarrollo del niño.

***

##### Código
###### Funciones de Preámbulo:

###### Argumentos:



```r
# Funciones

#Argumentos: 

# f. Identificacion de Muertes neonatos
# crea la variable 'neonato'

neonatos <- function(data, varnac, varfall){
  
  varnac <- enquo(varnac)
  varfall <- enquo(varfall)
  
  
  data %>% 
    
    mutate(neonato=if_else(condition = (!!varfall-!!varnac)<=27,
                           true = T, 
                           false = F))
  
  }



#data* : `r params$bdd1`
#vars* : vector de variables de grupo en character (eg. c('var1','var2'))

#f2. funcion de conteo de casos del indicador

fgrupos <- 

  function(data, vars=c(), varindic=NULL) {
    
    if(is.null(varindic)){
      
    data %>% group_by_at(.vars = vars) %>% count() %>% ungroup()
      
    } else {
      
    data %>% filter_at(.vars = varindic, .vars_predicate =  all_vars(.==1)) %>% 
      
      group_by_at(.vars = c(vars)) %>% count() %>% ungroup()
      
    }
    
  }

#f3. funcion que agrega los datos en funcion de las variables de agrupacion 

summarizador <- function(tabla, vars=NULL, varindicad=NULL) {
  
  varst <- c(varindicad, vars, 'anio_bs')
  
  
  t <- tabla  %>% group_by_at(.vars = varst) %>% 
    
    summarise(casos=sum(n, na.rm = T)) %>% ungroup() 
  
  if(!is.null(varindicad)){
    
    t %>% 
      
      filter_at(.vars = varindicad, .vars_predicate = all_vars(!is.na(.)))
    
  }
  
  t
}
```

###### Función del Indicador:



```r
  #Argumentos
  
  #tabla  : resultado de la funcion fgrupos()
  #vars   : vector de variables de grupo en character (eg. c('var1','var2'))
  #varindicad: variable del indicador creada con la funcion neonatos


f_n_mor_neonat <- function(tabla, 
                             vars=NULL,
                             varindicad=NULL){
  #numerador
  
  tabla %>% 
    
    summarizador(vars = vars)
  
}
```
