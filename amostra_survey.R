#

# Amostra aleatória simples

# Função para calcular

calculo_amostra_aleatoria_simples <-
  function(margem_erro, nivel_confianca, p = 0.5) {
    me <- margem_erro / 100

    if (nivel_confianca == 90) {
      z <- 1.645
    } else if (nivel_confianca == 95) {
      z <- 1.96
    } else if (nivel_confianca == 99) {
      z <- 2.575

    } else{
      return("Erro: Insira um dos seguintes valores para nível de confiança: 90, 95 ou 99.")
    }


    n <- (z ^ 2) * p * (1 - p) / (me ^ 2)

    paste0(
      "Para um nível de confiança de ",
      nivel_confianca,
      "% e margem de erro máxima de ",
      margem_erro,
      "%, a amostra é de ",
      round(n),
      " pessoas!"
    )
  }


# Exemplo de uso:

calculo_amostra_aleatoria_simples(margem_erro = 5, nivel_confianca = 95)


# Aplicando várias vezes para escolher

library(purrr)

x <- c(5, 3, 1) # margem erro

purrr::map(x,  .f = calculo_amostra_aleatoria_simples, nivel_confianca = 90)
purrr::map(x,  .f = calculo_amostra_aleatoria_simples, nivel_confianca = 95)
purrr::map(x,  .f = calculo_amostra_aleatoria_simples, nivel_confianca = 99)


