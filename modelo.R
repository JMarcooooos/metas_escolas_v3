print(paste("Diretório de trabalho atual:", getwd()))

arquivo_dados <- "dados_longit.RData"

if (file.exists(arquivo_dados)) {
  print(paste("Arquivo encontrado! Carregando:", arquivo_dados))
  load(arquivo_dados)
} else {
  print("ERRO CRÍTICO: Arquivo não encontrado.")
  print("Arquivos disponíveis nesta pasta:")
  print(list.files())
  stop("Parando execução pois os dados não foram encontrados.")
}

# GEMINI
library(brms)
library(tidyverse)

df_modelo <- dados_longit %>%
  filter(!is.na(VL_PROFICIENCIA_SAEB_LP), 
         !is.na(VL_PROFICIENCIA_SAEB_MT),
         !is.na(VL_PROFICIENCIA_LP),     
         !is.na(VL_PROFICIENCIA_ERRO_LP)) %>% 
  mutate(
    tempo = EDICAO - min(EDICAO) 
  )

# Fórmula Língua Portuguesa
bf_lp <- bf(
  VL_PROFICIENCIA_SAEB_LP ~ tempo + me(VL_PROFICIENCIA_LP, VL_PROFICIENCIA_ERRO_LP) + 
    (1 + tempo |p| CD_ESCOLA) +  
    (1 |q| CD_REGIONAL)         
)

# Fórmula Matemática
bf_mt <- bf(
  VL_PROFICIENCIA_SAEB_MT ~ tempo + me(VL_PROFICIENCIA_MT, VL_PROFICIENCIA_ERRO_MT) + 
    (1 + tempo |p| CD_ESCOLA) + 
    (1 |q| CD_REGIONAL)
)

is_github <- Sys.getenv("GITHUB_ACTIONS") == "true"

# Configuração Dinâmica
if (is_github) {
  n_chains  <- 4
  n_cores   <- 2 
  n_threads <- 1
  backend_opt <- "cmdstanr"
  msg <- "Rodando modo GitHub (Recursos Limitados)"
} else {
  n_chains  <- 4
  n_cores   <- 4
  n_threads <- 2
  backend_opt <- "cmdstanr"
  msg <- "Rodando modo Local (Full Power)"
}

print(msg)


modelo_me_brms_t <- brm(
  formula = bf_lp + bf_mt + set_rescor(TRUE),
  data = df_modelo,
  family = student(), 
  prior = c(
    prior(lkj(2), class = cor),
    prior(gamma(2, 0.1), class = nu) 
  ),
  backend = backend_opt,
  chains = n_chains,
  cores = n_cores, 
  threads = threading(n_threads),
  iter = 2000,
  warmup = 1000,
  control = list(adapt_delta = 0.98, max_treedepth = 15),
  file = "modelo_saeb_saego_me_v2" 
)

# modelo_me_brms <- brm(
#   formula = bf_lp + bf_mt + set_rescor(TRUE),
#   data = df_modelo,
#   family = gaussian(),
#   prior = prior(lkj(2), class = cor),
#   backend = backend_opt,
#   chains = n_chains,
#   cores = n_cores, 
#   threads = threading(n_threads),
#   iter = 4000,
#   warmup = 2000,
#   refresh = 250, 
#   silent = 0,    
#   control = list(adapt_delta = 0.98, max_treedepth = 12),
#   file = "modelo_saeb_saego_me_v1" 
# )
