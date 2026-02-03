# 1. Imprime onde o R acha que está (para debug no log)
print(paste("Diretório de trabalho atual:", getwd()))

# 2. Verifica se o arquivo existe antes de tentar carregar
arquivo_dados <- "dados_longit.RData" # <--- VERIFIQUE A ESCRITA AQUI EXATAMENTE IGUAL AO GITHUB

if (file.exists(arquivo_dados)) {
  print(paste("Arquivo encontrado! Carregando:", arquivo_dados))
  load(arquivo_dados)
} else {
  # Se não achou, lista o que tem na pasta para te ajudar a descobrir o erro
  print("ERRO CRÍTICO: Arquivo não encontrado.")
  print("Arquivos disponíveis nesta pasta:")
  print(list.files())
  stop("Parando execução pois os dados não foram encontrados.")
}

# GEMINI

library(brms)
library(tidyverse)

# Supondo que seu dataframe se chama 'df_completo'

df_modelo <- dados_longit %>%
  filter(!is.na(VL_PROFICIENCIA_SAEB_LP), 
         !is.na(VL_PROFICIENCIA_SAEB_MT),
         !is.na(VL_PROFICIENCIA_LP),     
         !is.na(VL_PROFICIENCIA_ERRO_LP)) %>% # Garanta que não há NA no erro
  mutate(
    tempo = EDICAO - min(EDICAO) # 0, 2, 4... (Anos a partir do início)
  )

# Fórmula Língua Portuguesa
bf_lp <- bf(
  VL_PROFICIENCIA_SAEB_LP ~ tempo + me(VL_PROFICIENCIA_LP, VL_PROFICIENCIA_ERRO_LP) + 
    (1 + tempo |p| CD_ESCOLA) +  # ID p para correlacionar escola entre matérias
    (1 |q| CD_REGIONAL)          # ID q para correlacionar regional entre matérias
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
  # CONFIGURAÇÃO SEGURA PARA GITHUB ACTIONS (FREE TIER)
  # A máquina tem 2 vCPUs. Não podemos usar threading agressivo.
  n_chains  <- 4
  n_cores   <- 2 # Roda 2 cadeias por vez (serializado)
  n_threads <- 1 # Sem threading intra-chain para economizar RAM
  backend_opt <- "cmdstanr"
  msg <- "Rodando modo GitHub (Recursos Limitados)"
} else {
  # SUA CONFIGURAÇÃO POTENTE LOCAL
  n_chains  <- 4
  n_cores   <- 4
  n_threads <- 2
  backend_opt <- "cmdstanr"
  msg <- "Rodando modo Local (Full Power)"
}

print(msg)


# 4. Rodando o Modelo
modelo_me_brms <- brm(
  formula = bf_lp + bf_mt + set_rescor(TRUE),
  data = df_modelo,
  family = gaussian(),
  
  # --- Configurações de Backend e Performance ---
  backend = backend_opt,
  chains = n_chains,
  cores = n_cores, 
  threads = threading(n_threads),
  
  # --- Configurações de Amostragem ---
  iter = 2000,
  warmup = 1000,
  refresh = 250,  # Imprime no log a cada 250 iterações
  silent = 0,     # Garante que não vai silenciar (0 = verboso)
  # Aumentar o adapt_delta é vital para modelos com 'me()', pois a geometria é complexa
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  
  # Salvar o modelo compilado em arquivo para não perder horas de processamento
  file = "modelo_saeb_saego_me_v1" 
)

summary(modelo_me_brms)
