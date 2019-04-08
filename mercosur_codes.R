library(tidyverse)
library(stringr)

f=file('mercosur_codes_raw.txt')

codes = readLines(f)

close(f)

res = data.frame(
  seccion_code=as.character(),
  seccion=as.character(),
  division_code=as.integer(),
  division=as.character(),
  clase_code=as.integer(),
  clase=as.character())

seccion=NA
seccion_desc=NA
division=NA
division_desc=NA
for(c in codes){
  code = word(c, 1)
  if(is.numeric(code)){
    if(as.integer(code) < 100){
      division=as.integer(code)
      division_desc=sub(paste0("^", code, ' '), '', c)
    }else{
      clase=as.integer(code)
      clase_desc=sub(paste0("^", code, ' '), '', c)
      
      r=data.frame(
      seccion_code=seccion,
      seccion=seccion_desc,
      division_code=division,
      division=division_desc,
      clase_code=clase,
      clase=clase_desc)
      
      res=rbind(res,r)
    }
  }else{
    seccion=code
    seccion_desc=sub(paste0("^", code, ' '), '', c)
  }
}

write_csv(res, path='mercosur_codes.csv')
