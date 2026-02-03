load(dados_longit.RData)

# GEMINI

library(brms)

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

# 4. Rodando o Modelo
modelo_me_brms <- brm(
  formula = bf_lp + bf_mt + set_rescor(TRUE),
  data = df_modelo,
  family = gaussian(),
  
  # --- Configurações de Backend e Performance ---
  backend = "cmdstanr",
  threads = threading(2), # Habilita Within-chain parallelization
  chains = 4,
  cores = 4, # O brms usa 'cores' para rodar as cadeias em paralelo
  
  # --- Configurações de Amostragem ---
  iter = 2000,
  warmup = 1000,
  
  # Aumentar o adapt_delta é vital para modelos com 'me()', pois a geometria é complexa
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  
  # Salvar o modelo compilado em arquivo para não perder horas de processamento
  file = "modelo_saeb_saego_me_v1" 
)

summary(modelo_me_brms)