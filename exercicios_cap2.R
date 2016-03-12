naturaisCrescentes <- function(n){
  
  if(n < 0)
  {
    return('')
  }
  naturaisCrescentes(n-1)
  cat(paste0(n, ' '))
  
}

naturaisDecrescentes <- function(n){

  cat(paste0(n, ' '))
  if(n == 0)
  {
    return('')
  }
  naturaisDecrescentes(n-1)
}

maximoEmVetor <- function(vetor){
  
  if(length(vetor) == 1){
    return(vetor[1])
  }else{
    aux <- maximoEmVetor(vetor[2:(length(vetor))])
    if(aux > vetor[1]){
      return(aux)
    }else{
      return(vetor[1])
    }
  }
}

estaEmVetor <- function(vetor, valor){
    
  if(vetor[1] == valor){
    return(TRUE)
  }else if(length(vetor) > 1){
    estaEmVetor(vetor[2:length(vetor)], valor)    
  }else{
    return(FALSE)
  }
}

somaTodoVetor <- function(vetor){
  
  if(length(vetor) == 1){
    return(vetor[1])
  }else {
    return(vetor[1] + somaTodoVetor(vetor[2:length(vetor)]))
  }
}

inverteVetor <- function(vetor){
  
  if(length(vetor) == 1){
    return(vetor[1])
  }else {
    return(c(inverteVetor(vetor[2:length(vetor)]), vetor[1]))
  }
}

#outros exercicios

imprimirBinario <- function(numero){
  #em R o resto da divisão é o operador %/%
  resto <- numero %% 2
  #em R a parte inteira da divisão é o operador %%
  quociente <- numero %/% 2
  if(quociente > 0){
    imprimirBinario(quociente)
  }
  cat(paste0(resto, ' '))
}


naturaisCrescentes(10)
cat('\n')
naturaisDecrescentes(10)
cat('\n')
a <- c(1,2,3,4,5,6,7,8,9,10)
print(maximoEmVetor(a))
a <- c(1,2,30,4,5,6,7,8,9,10)
print(maximoEmVetor(a))
print(estaEmVetor(a,5))
print(estaEmVetor(a,99))
print(somaTodoVetor(a))
imprimirBinario(2)
cat('\n')
imprimirBinario(4)
cat('\n')
imprimirBinario(9)
cat('\n')
imprimirBinario(1024)
cat('\n')
