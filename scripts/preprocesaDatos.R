library(readr)
library(tidyverse)

# Funcion para preprocesar un dataframe
preprocesa = function(data, type){ #type=0 --> horas, type=1 --> 10minutos
  
  agrupacion = 1
  pattern = ".* "
  
  if (type == 0){
    agrupacion = 6
  }
  
  # Unimos los consumos añadiendo un índice que nos permita usar
  # group by
  
  data = data %>% mutate(index = c(0, rep(1:(nrow(data)-1)%/%agrupacion))) %>%
    group_by(index) %>%
    summarise(Orig = Orig[1],Real = mean(Real)) %>%
    select(-1) %>%
    mutate(Orig =  sub(pattern,"",Orig)) %>%
    arrange(Orig)
  
  # Calculamos cuantos valores hay en cada hora/10min
  valores = unname(table(data$Orig)[1])
  
  # Hacemos un reshape de filas a columnas
  data_prepro = data.frame(0)
  indice = 1
  
  for (i in 1:nrow(data)){
    
    if (i %% valores == 0){
      data_prepro = cbind(data_prepro,data[indice:i,2])
      indice = i
      indice = indice + 1
    }  
    
  }
  
  data_prepro[1] = NULL
  colnames(data_prepro) = unique(data$Orig)
  data_prepro
}


# Lectura de datos
training = read_csv("../datos/raw/training.csv", 
                    col_types = cols(Orig = col_character()))

validation = read_csv("../datos/raw/validation.csv", 
                    col_types = cols(Orig = col_character()))

test = read_csv("../datos/raw/test.csv", 
                    col_types = cols(Orig = col_character()))

# Preprocesamiento de los mismos
training_horas = preprocesa(training,0)
validation_horas = preprocesa(validation,0)
test_horas = preprocesa(test,0)

training_minutos = preprocesa(training,1)
validation_minutos = preprocesa(validation,1)
test_minutos = preprocesa(test,1)

# Guardamos los datos
write.csv(training_horas,"../datos/preprocesados/training_horas.csv", row.names = F)
write.csv(validation_horas,"../datos/preprocesados/validation_horas.csv", row.names = F)
write.csv(test_horas,"../datos/preprocesados/test_horas.csv", row.names = F)
write.csv(training_minutos,"../datos/preprocesados/training_minutos.csv", row.names = F)
write.csv(validation_minutos,"../datos/preprocesados/validation_minutos.csv", row.names = F)
write.csv(test_minutos,"../datos/preprocesados/test_minutos.csv", row.names = F)

