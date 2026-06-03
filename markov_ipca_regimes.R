# ================================================================
# Trabalho de Estatistica - Pos-graduacao em Economia
# Tema: Persistencia dos regimes inflacionarios no Brasil
# Metodo: Cadeias de Markov de tempo discreto
# Dados: IPCA mensal - Banco Central do Brasil, serie SGS 433
# ================================================================

# ------------------------------------------------
# 1. Preparacao do ambiente
# ------------------------------------------------

# O objetivo deste bloco e garantir que o pacote necessario esteja instalado.
# Para facilitar a reproducao, o script cria uma biblioteca local dentro da
# propria pasta do trabalho, evitando depender da biblioteca global do R.

biblioteca_local <- file.path(getwd(), "biblioteca_R")

if (!dir.exists(biblioteca_local)) {
  dir.create(biblioteca_local, recursive = TRUE)
}

.libPaths(c(biblioteca_local, .libPaths()))

pacotes_necessarios <- c("markovchain", "nnet")

for (pacote in pacotes_necessarios) {
  if (!requireNamespace(pacote, quietly = TRUE)) {
    install.packages(pacote, repos = "https://cloud.r-project.org")
  }
}

library(markovchain)

# ------------------------------------------------
# 2. Definicao do problema de pesquisa
# ------------------------------------------------

# Pergunta central:
# A inflacao brasileira apresenta persistencia de regime?

# Neste trabalho, a inflacao mensal medida pelo IPCA sera classificada em
# tres estados definidos por tercis da propria distribuicao historica:
# 1) Baixa:    terco inferior da distribuicao do IPCA mensal
# 2) Moderada: terco intermediario da distribuicao do IPCA mensal
# 3) Alta:     terco superior da distribuicao do IPCA mensal
#
# Essa escolha evita aplicar ao periodo pre-Real os mesmos cortes usados no
# periodo pos-Real, quando a escala da inflacao brasileira era muito diferente.

# A cadeia de Markov estimara a probabilidade de transicao entre esses
# regimes de um mes para o mes seguinte.

# ------------------------------------------------
# 3. Coleta dos dados reais do IPCA
# ------------------------------------------------

# A serie 433 do Sistema Gerenciador de Series Temporais do Banco Central
# corresponde ao IPCA - variacao mensal em percentual.

data_inicial <- "01/01/1980"

# A data final fica fixa para reproduzir exatamente a amostra usada no
# relatorio. Para atualizar o estudo no futuro, altere manualmente esta data.
data_final <- "30/04/2026"

url_bcb <- paste0(
  "https://api.bcb.gov.br/dados/serie/bcdata.sgs.433/dados?",
  "formato=csv",
  "&dataInicial=", utils::URLencode(data_inicial, reserved = TRUE),
  "&dataFinal=", utils::URLencode(data_final, reserved = TRUE)
)

# Leitura dos dados diretamente da API publica do Banco Central.
# O formato csv do BCB usa ponto e virgula como separador e virgula decimal.

ipca_bruto <- read.csv2(url_bcb, stringsAsFactors = FALSE)

# Padronizacao dos nomes das colunas.

names(ipca_bruto) <- c("data", "ipca_mensal")

# Conversao da data e do valor numerico.

ipca <- ipca_bruto
ipca$data <- as.Date(ipca$data, format = "%d/%m/%Y")
ipca$ipca_mensal <- as.numeric(gsub(",", ".", as.character(ipca$ipca_mensal)))

# Ordenacao cronologica e remocao de observacoes incompletas, se houver.

ipca <- ipca[order(ipca$data), ]
ipca <- ipca[!is.na(ipca$data) & !is.na(ipca$ipca_mensal), ]
rownames(ipca) <- NULL

# Coleta adicional da meta Selic para a extensao TVTP.
# A serie 432 do Banco Central corresponde a meta Selic definida pelo Copom.
# Ela sera usada como variavel macroeconomica auxiliar para modelar
# probabilidades de transicao variaveis no tempo.
#
# Algumas series do SGS podem recusar consultas muito longas. Por isso, a
# funcao abaixo baixa os dados em janelas menores e depois agrega os trechos.

baixar_sgs_em_janelas <- function(codigo, data_inicial, data_final, anos_janela = 6) {
  inicio_total <- as.Date(data_inicial, format = "%d/%m/%Y")
  fim_total <- as.Date(data_final, format = "%d/%m/%Y")
  inicio_janela <- inicio_total
  dados <- data.frame()

  while (inicio_janela <= fim_total) {
    proximo_inicio <- seq(inicio_janela, by = paste(anos_janela, "years"), length.out = 2)[2]
    fim_janela <- min(proximo_inicio - 1, fim_total)

    url <- paste0(
      "https://api.bcb.gov.br/dados/serie/bcdata.sgs.", codigo, "/dados?",
      "formato=csv",
      "&dataInicial=", format(inicio_janela, "%d/%m/%Y"),
      "&dataFinal=", format(fim_janela, "%d/%m/%Y")
    )

    trecho <- tryCatch(
      suppressWarnings(read.csv2(url, stringsAsFactors = FALSE)),
      error = function(e) NULL
    )

    if (!is.null(trecho) && nrow(trecho) > 0 && ncol(trecho) >= 2) {
      trecho <- trecho[, 1:2]
      names(trecho) <- c("data", "valor")
      dados <- rbind(dados, trecho)
    }

    inicio_janela <- fim_janela + 1
  }

  return(dados)
}

selic_bruto <- baixar_sgs_em_janelas(
  codigo = 432,
  data_inicial = data_inicial,
  data_final = data_final,
  anos_janela = 6
)

ipca$selic_meta <- NA_real_

if (!is.null(selic_bruto) && nrow(selic_bruto) > 0) {
  names(selic_bruto) <- c("data", "selic_meta")

  selic <- selic_bruto
  selic$data <- as.Date(selic$data, format = "%d/%m/%Y")
  selic$selic_meta <- as.numeric(gsub(",", ".", as.character(selic$selic_meta)))
  selic <- selic[order(selic$data), ]
  selic <- selic[!is.na(selic$data) & !is.na(selic$selic_meta), ]

  for (i in seq_len(nrow(ipca))) {
    posicao_selic <- which(selic$data <= ipca$data[i])

    if (length(posicao_selic) > 0) {
      ipca$selic_meta[i] <- selic$selic_meta[max(posicao_selic)]
    }
  }
}

# Conferencia inicial dos dados.

cat("\n============================================================\n")
cat("DADOS DO IPCA\n")
cat("============================================================\n")
cat("Fonte: Banco Central do Brasil - SGS 433\n")
cat("Periodo inicial:", format(min(ipca$data), "%d/%m/%Y"), "\n")
cat("Periodo final:  ", format(max(ipca$data), "%d/%m/%Y"), "\n")
cat("Numero de observacoes mensais:", nrow(ipca), "\n\n")
cat("Primeiras observacoes:\n")
print(head(ipca))
cat("\nUltimas observacoes:\n")
print(tail(ipca))

if (sum(!is.na(ipca$selic_meta)) > 0) {
  cat("\nSerie auxiliar Selic carregada para a extensao TVTP.\n")
  cat("Observacoes mensais com Selic disponivel:", sum(!is.na(ipca$selic_meta)), "\n")
} else {
  cat("\nAviso: a serie auxiliar Selic nao foi carregada. A extensao TVTP usara apenas o IPCA defasado.\n")
}

# ------------------------------------------------
# 4. Construcao dos estados da cadeia de Markov
# ------------------------------------------------

# Classificacao do IPCA mensal em regimes inflacionarios.
#
# Como a amostra agora comeca em 1980, os cortes fixos de 0,3% e 0,7% ao mes
# deixam de ser adequados: eles fazem sentido para a inflacao pos-Real, mas
# nao para a inflacao muito elevada dos anos 1980 e inicio dos anos 1990.
#
# Por isso, os estados principais da cadeia observavel serao definidos por
# tercis da distribuicao historica do IPCA mensal. Assim, "Baixa", "Moderada"
# e "Alta" passam a ser regimes relativos ao periodo analisado.

limiares_tercis <- quantile(
  ipca$ipca_mensal,
  probs = c(1 / 3, 2 / 3),
  na.rm = TRUE
)

limiar_baixa_moderada <- as.numeric(limiares_tercis[1])
limiar_moderada_alta <- as.numeric(limiares_tercis[2])

limiares_regimes <- data.frame(
  Metodo = "Tercis da distribuicao historica",
  Limite = c("Baixa-Moderada", "Moderada-Alta"),
  Valor_IPCA_mensal = c(limiar_baixa_moderada, limiar_moderada_alta)
)

ipca$regime <- ifelse(
  ipca$ipca_mensal < limiar_baixa_moderada,
  "Baixa",
  ifelse(ipca$ipca_mensal <= limiar_moderada_alta, "Moderada", "Alta")
)

# Ordem economica desejada dos estados.

estados <- c("Baixa", "Moderada", "Alta")
ipca$regime <- factor(ipca$regime, levels = estados)

# Frequencia absoluta e relativa dos regimes.

frequencia_regimes <- table(ipca$regime)
participacao_regimes <- prop.table(frequencia_regimes)

cat("\n============================================================\n")
cat("FREQUENCIA DOS REGIMES INFLACIONARIOS\n")
cat("============================================================\n")
cat("\nLimiares definidos por tercis da distribuicao historica:\n")
cat("Baixa    < ", round(limiar_baixa_moderada, 4), "% ao mes\n", sep = "")
cat(
  "Moderada entre ",
  round(limiar_baixa_moderada, 4),
  "% e ",
  round(limiar_moderada_alta, 4),
  "% ao mes\n",
  sep = ""
)
cat("Alta     > ", round(limiar_moderada_alta, 4), "% ao mes\n", sep = "")
cat("\nFrequencia absoluta:\n")
print(frequencia_regimes)
cat("\nFrequencia relativa:\n")
print(round(participacao_regimes, 4))

# ------------------------------------------------
# 5. Funcao para calcular a matriz de transicao
# ------------------------------------------------

# Esta funcao recebe uma sequencia de regimes e calcula:
# a) matriz de contagens de transicoes
# b) matriz de probabilidades de transicao

calcular_matriz_transicao <- function(regimes, estados) {
  origem <- factor(head(regimes, -1), levels = estados)
  destino <- factor(tail(regimes, -1), levels = estados)

  matriz_contagens <- as.matrix(table(Origem = origem, Destino = destino))

  matriz_probabilidades <- matrix(
    NA_real_,
    nrow = length(estados),
    ncol = length(estados),
    dimnames = list(Origem = estados, Destino = estados)
  )

  for (i in seq_along(estados)) {
    total_linha <- sum(matriz_contagens[i, ])

    if (total_linha > 0) {
      matriz_probabilidades[i, ] <- matriz_contagens[i, ] / total_linha
    }
  }

  return(list(
    contagens = matriz_contagens,
    probabilidades = matriz_probabilidades
  ))
}

arredondar_colunas_numericas <- function(base, digitos = 4) {
  base[] <- lapply(
    base,
    function(coluna) {
      if (is.numeric(coluna)) {
        return(round(coluna, digitos))
      }

      return(coluna)
    }
  )

  return(base)
}

# Aplicacao da funcao para a amostra completa.

resultado_transicao <- calcular_matriz_transicao(ipca$regime, estados)

matriz_contagens <- resultado_transicao$contagens
matriz_transicao <- resultado_transicao$probabilidades

cat("\n============================================================\n")
cat("MATRIZ DE CONTAGENS DE TRANSICAO\n")
cat("============================================================\n")
print(matriz_contagens)

cat("\n============================================================\n")
cat("MATRIZ DE PROBABILIDADES DE TRANSICAO\n")
cat("============================================================\n")
print(round(matriz_transicao, 4))

# ------------------------------------------------
# 6. Criacao e ajuste da cadeia com o pacote markovchain
# ------------------------------------------------

# O objeto abaixo representa a cadeia de Markov estimada manualmente a partir
# das frequencias observadas nos dados.

cadeia_ipca <- new(
  "markovchain",
  states = estados,
  transitionMatrix = matriz_transicao,
  name = "Cadeia de Markov - Regimes do IPCA"
)

cat("\n============================================================\n")
cat("OBJETO MARKOVCHAIN ESTIMADO\n")
cat("============================================================\n")
print(cadeia_ipca)

# Tambem podemos ajustar a cadeia diretamente pela funcao markovchainFit().
# O resultado deve ser muito proximo da matriz calculada acima, pois ambos
# usam as transicoes observadas na sequencia historica.

modelo_ajustado <- markovchainFit(data = as.character(ipca$regime))

cat("\n============================================================\n")
cat("ESTIMACAO COM markovchainFit()\n")
cat("============================================================\n")
print(modelo_ajustado$estimate)

# ------------------------------------------------
# 7. Analise da persistencia dos regimes
# ------------------------------------------------

# A diagonal principal da matriz de transicao mostra a probabilidade de o
# regime permanecer o mesmo no mes seguinte.

persistencia <- diag(matriz_transicao)
names(persistencia) <- estados

# A duracao esperada de cada regime e dada por 1 / (1 - p_ii),
# onde p_ii e a probabilidade de permanecer no mesmo estado.

duracao_esperada <- ifelse(
  persistencia < 1,
  1 / (1 - persistencia),
  Inf
)

resumo_regimes <- data.frame(
  Regime = estados,
  Probabilidade_de_permanencia = as.numeric(persistencia),
  Duracao_esperada_em_meses = as.numeric(duracao_esperada)
)

cat("\n============================================================\n")
cat("PERSISTENCIA E DURACAO ESPERADA DOS REGIMES\n")
cat("============================================================\n")
print(resumo_regimes)

# ------------------------------------------------
# 8. Distribuicao estacionaria
# ------------------------------------------------

# A distribuicao estacionaria indica a proporcao de tempo que a economia
# tenderia a passar em cada regime no longo prazo, caso a matriz de transicao
# permanecesse constante.

distribuicao_estacionaria <- steadyStates(cadeia_ipca)

cat("\n============================================================\n")
cat("DISTRIBUICAO ESTACIONARIA\n")
cat("============================================================\n")
print(round(distribuicao_estacionaria, 4))

# ------------------------------------------------
# 9. Diagnosticos formais da matriz empirica
# ------------------------------------------------

# Esta secao operacionaliza propriedades teoricas da cadeia no caso empirico:
# irreducibilidade, aperiodicidade, reversibilidade, autovalores, gap espectral
# e um limite superior aproximado de tempo de mistura. Esses diagnosticos nao
# substituem a interpretacao economica, mas mostram se a matriz estimada possui
# as propriedades matematicas usualmente discutidas em Cadeias de Markov.

calcular_mdc <- function(x) {
  x <- abs(as.integer(x))
  x <- x[x > 0]

  if (length(x) == 0) {
    return(NA_integer_)
  }

  mdc_dois <- function(a, b) {
    while (b != 0) {
      resto <- a %% b
      a <- b
      b <- resto
    }

    return(a)
  }

  Reduce(mdc_dois, x)
}

calcular_alcancabilidade <- function(matriz, tolerancia = 1e-12) {
  adjacencia <- (matriz > tolerancia) * 1
  alcance <- adjacencia
  potencia <- adjacencia
  k <- nrow(matriz)

  if (k > 1) {
    for (passo in 2:k) {
      potencia <- ((potencia %*% adjacencia) > 0) * 1
      alcance <- ((alcance + potencia) > 0) * 1
    }
  }

  rownames(alcance) <- rownames(matriz)
  colnames(alcance) <- colnames(matriz)

  return(alcance)
}

calcular_periodos_estados <- function(matriz, max_passo = 60, tolerancia = 1e-12) {
  k <- nrow(matriz)
  potencia <- diag(k)
  periodos <- rep(NA_integer_, k)
  names(periodos) <- rownames(matriz)

  for (passo in seq_len(max_passo)) {
    potencia <- potencia %*% matriz

    for (i in seq_len(k)) {
      if (potencia[i, i] > tolerancia) {
        if (is.na(periodos[i])) {
          periodos[i] <- passo
        } else {
          periodos[i] <- calcular_mdc(c(periodos[i], passo))
        }
      }
    }
  }

  return(periodos)
}

diagnosticar_matriz_markov <- function(matriz, distribuicao_estacionaria, epsilon = 0.05) {
  alcance <- calcular_alcancabilidade(matriz)
  irredutivel <- all(alcance == 1)

  periodos <- calcular_periodos_estados(matriz)
  aperiodica <- all(periodos == 1, na.rm = TRUE)

  pi_est <- as.numeric(distribuicao_estacionaria[1, rownames(matriz)])
  names(pi_est) <- rownames(matriz)

  fluxo_direto <- sweep(matriz, 1, pi_est, "*")
  fluxo_reverso <- t(fluxo_direto)
  desvio_reversibilidade <- max(abs(fluxo_direto - fluxo_reverso), na.rm = TRUE)

  autovalores <- eigen(matriz, only.values = TRUE)$values
  autovalores_modulo <- Mod(autovalores)
  lambda2 <- sort(autovalores_modulo, decreasing = TRUE)[2]
  gap_espectral <- 1 - lambda2

  tempo_mistura_limite <- NA_real_

  if (is.finite(gap_espectral) && gap_espectral > 0 && min(pi_est) > 0) {
    tempo_mistura_limite <- ceiling(
      log(1 / (epsilon * sqrt(min(pi_est)))) / gap_espectral
    )
  }

  list(
    resumo = data.frame(
      Propriedade = c(
        "Irredutivel",
        "Aperiodica",
        "Reversivel_aproximadamente",
        "Desvio_maximo_reversibilidade",
        "Segundo_maior_autovalor_em_modulo",
        "Gap_espectral",
        "Tempo_mistura_limite_superior_epsilon_0_05"
      ),
      Valor = c(
        as.character(irredutivel),
        as.character(aperiodica),
        as.character(desvio_reversibilidade < 1e-8),
        as.character(desvio_reversibilidade),
        as.character(lambda2),
        as.character(gap_espectral),
        as.character(tempo_mistura_limite)
      )
    ),
    periodos = data.frame(
      Estado = names(periodos),
      Periodo = as.integer(periodos)
    ),
    autovalores = data.frame(
      Ordem = seq_along(autovalores),
      Parte_real = Re(autovalores),
      Parte_imaginaria = Im(autovalores),
      Modulo = autovalores_modulo
    ),
    alcance = alcance
  )
}

diagnosticos_markov <- diagnosticar_matriz_markov(
  matriz = matriz_transicao,
  distribuicao_estacionaria = distribuicao_estacionaria
)

diagnosticos_matriz <- diagnosticos_markov$resumo
periodos_estados <- diagnosticos_markov$periodos
autovalores_matriz <- diagnosticos_markov$autovalores
matriz_alcancabilidade <- diagnosticos_markov$alcance

cat("\n============================================================\n")
cat("DIAGNOSTICOS FORMAIS DA MATRIZ DE TRANSICAO\n")
cat("============================================================\n")
print(diagnosticos_matriz)
cat("\nPeriodos estimados dos estados:\n")
print(periodos_estados)
cat("\nAutovalores da matriz de transicao:\n")
print(arredondar_colunas_numericas(autovalores_matriz, 4))

# ------------------------------------------------
# 10. Probabilidades apos varios meses
# ------------------------------------------------

# Esta funcao calcula potencias da matriz de transicao.
# Por exemplo, P^6 mostra probabilidades de transicao em seis meses.

potencia_matriz <- function(matriz, n) {
  if (n == 1) {
    return(matriz)
  }

  resultado <- matriz

  for (i in 2:n) {
    resultado <- resultado %*% matriz
  }

  return(resultado)
}

matriz_6_meses <- potencia_matriz(matriz_transicao, 6)
matriz_12_meses <- potencia_matriz(matriz_transicao, 12)

cat("\n============================================================\n")
cat("MATRIZ DE TRANSICAO EM 6 MESES - P^6\n")
cat("============================================================\n")
print(round(matriz_6_meses, 4))

cat("\n============================================================\n")
cat("MATRIZ DE TRANSICAO EM 12 MESES - P^12\n")
cat("============================================================\n")
print(round(matriz_12_meses, 4))

# ------------------------------------------------
# 10. Comparacao entre periodos historicos
# ------------------------------------------------

# Esta etapa amplia a analise e compara a persistencia dos regimes em tres
# periodos:
# 1) Pre-pandemia: ate dezembro de 2019
# 2) Pandemia: janeiro de 2020 a dezembro de 2021
# 3) Pos-pandemia: a partir de janeiro de 2022

analisar_periodo <- function(base, nome_periodo, estados) {
  if (nrow(base) < 2) {
    return(NULL)
  }

  resultado <- calcular_matriz_transicao(base$regime, estados)
  matriz_p <- resultado$probabilidades
  diag_p <- diag(matriz_p)

  data.frame(
    Periodo = nome_periodo,
    Regime = estados,
    Probabilidade_de_permanencia = as.numeric(diag_p),
    Duracao_esperada_em_meses = as.numeric(ifelse(diag_p < 1, 1 / (1 - diag_p), Inf))
  )
}

ipca_pre_pandemia <- ipca[ipca$data <= as.Date("2019-12-01"), ]
ipca_pandemia <- ipca[ipca$data >= as.Date("2020-01-01") & ipca$data <= as.Date("2021-12-01"), ]
ipca_pos_pandemia <- ipca[ipca$data >= as.Date("2022-01-01"), ]

resumo_periodos <- rbind(
  analisar_periodo(ipca_pre_pandemia, "Pre-pandemia", estados),
  analisar_periodo(ipca_pandemia, "Pandemia", estados),
  analisar_periodo(ipca_pos_pandemia, "Pos-pandemia", estados)
)

cat("\n============================================================\n")
cat("COMPARACAO DA PERSISTENCIA POR PERIODO\n")
cat("============================================================\n")
print(resumo_periodos)

# ------------------------------------------------
# 11. Analise estrutural: pre-Real e pos-Real
# ------------------------------------------------

# A economia brasileira passou por uma mudanca estrutural importante com o
# Plano Real. Por isso, alem da amostra completa, estimamos duas cadeias
# separadas:
#
# 1) Periodo inflacionario / pre-Real: janeiro de 1980 a junho de 1994
# 2) Periodo pos-estabilizacao / pos-Real: julho de 1994 em diante
#
# Em cada subperiodo, os tercis sao recalculados dentro da propria amostra.
# Assim, "Alta" no pre-Real significa alta inflacao dentro de um ambiente
# inflacionario; "Alta" no pos-Real significa pressao inflacionaria dentro
# de um ambiente estabilizado.

corte_plano_real <- as.Date("1994-07-01")

analisar_subperiodo_com_tercis <- function(base, nome_periodo, estados) {
  if (nrow(base) < 3) {
    stop("Subperiodo com observacoes insuficientes para calcular tercis.")
  }

  limiares <- quantile(
    base$ipca_mensal,
    probs = c(1 / 3, 2 / 3),
    na.rm = TRUE
  )

  limiar_1 <- as.numeric(limiares[1])
  limiar_2 <- as.numeric(limiares[2])

  base$regime_tercis_periodo <- ifelse(
    base$ipca_mensal < limiar_1,
    "Baixa",
    ifelse(base$ipca_mensal <= limiar_2, "Moderada", "Alta")
  )

  base$regime_tercis_periodo <- factor(base$regime_tercis_periodo, levels = estados)
  base$periodo_plano_real <- nome_periodo

  resultado <- calcular_matriz_transicao(base$regime_tercis_periodo, estados)
  matriz_p <- resultado$probabilidades
  diag_p <- diag(matriz_p)

  limiares_tabela <- data.frame(
    Periodo = nome_periodo,
    Limite = c("Baixa-Moderada", "Moderada-Alta"),
    Valor_IPCA_mensal = c(limiar_1, limiar_2)
  )

  resumo <- data.frame(
    Periodo = nome_periodo,
    Regime = estados,
    Frequencia = as.numeric(table(base$regime_tercis_periodo)[estados]),
    Probabilidade_de_permanencia = as.numeric(diag_p),
    Duracao_esperada_em_meses = as.numeric(ifelse(diag_p < 1, 1 / (1 - diag_p), Inf))
  )

  return(list(
    base = base,
    limiares = limiares_tabela,
    contagens = resultado$contagens,
    matriz = matriz_p,
    resumo = resumo
  ))
}

ipca_pre_real_base <- ipca[ipca$data < corte_plano_real, ]
ipca_pos_real_base <- ipca[ipca$data >= corte_plano_real, ]

analise_pre_real <- analisar_subperiodo_com_tercis(
  base = ipca_pre_real_base,
  nome_periodo = "Pre-Real",
  estados = estados
)

analise_pos_real <- analisar_subperiodo_com_tercis(
  base = ipca_pos_real_base,
  nome_periodo = "Pos-Real",
  estados = estados
)

limiares_pre_pos_real <- rbind(
  analise_pre_real$limiares,
  analise_pos_real$limiares
)

resumo_pre_pos_real <- rbind(
  analise_pre_real$resumo,
  analise_pos_real$resumo
)

ipca$periodo_plano_real <- ifelse(
  ipca$data < corte_plano_real,
  "Pre-Real",
  "Pos-Real"
)

ipca$regime_tercis_periodo <- NA_character_
ipca$regime_tercis_periodo[ipca$data < corte_plano_real] <-
  as.character(analise_pre_real$base$regime_tercis_periodo)
ipca$regime_tercis_periodo[ipca$data >= corte_plano_real] <-
  as.character(analise_pos_real$base$regime_tercis_periodo)
ipca$regime_tercis_periodo <- factor(ipca$regime_tercis_periodo, levels = estados)

cat("\n============================================================\n")
cat("ANALISE ESTRUTURAL PRE-REAL E POS-REAL\n")
cat("============================================================\n")
cat("\nLimiares por subperiodo:\n")
print(limiares_pre_pos_real)
cat("\nResumo da persistencia por subperiodo:\n")
print(resumo_pre_pos_real)
cat("\nMatriz de transicao - Pre-Real:\n")
print(round(analise_pre_real$matriz, 4))
cat("\nMatriz de transicao - Pos-Real:\n")
print(round(analise_pos_real$matriz, 4))

# ------------------------------------------------
# 12. Inferencia estatistica da matriz de transicao
# ------------------------------------------------

# Nesta etapa, o trabalho deixa de apresentar apenas estimativas pontuais.
# Sao calculados intervalos de confianca, um teste LR contra independencia
# temporal dos regimes e testes de estabilidade temporal da matriz.

# 12.1 Intervalos de confianca assintoticos
#
# Cada linha da matriz de transicao pode ser vista como uma distribuicao
# multinomial condicional ao estado de origem. Assim, uma aproximacao simples
# para o erro-padrao de p_ij e:
#
# se(p_ij) = sqrt[p_ij(1 - p_ij) / n_i]
#
# onde n_i e o numero de transicoes observadas a partir do estado i.

calcular_ic_multinomial <- function(contagens, nivel = 0.95) {
  z <- qnorm(1 - (1 - nivel) / 2)
  estados_linhas <- rownames(contagens)
  estados_colunas <- colnames(contagens)
  resultados <- data.frame()

  for (i in seq_along(estados_linhas)) {
    total_linha <- sum(contagens[i, ])

    for (j in seq_along(estados_colunas)) {
      p_estimado <- ifelse(total_linha > 0, contagens[i, j] / total_linha, NA_real_)
      erro_padrao <- ifelse(
        total_linha > 0,
        sqrt(p_estimado * (1 - p_estimado) / total_linha),
        NA_real_
      )

      limite_inferior <- max(0, p_estimado - z * erro_padrao)
      limite_superior <- min(1, p_estimado + z * erro_padrao)

      resultados <- rbind(
        resultados,
        data.frame(
          Origem = estados_linhas[i],
          Destino = estados_colunas[j],
          Probabilidade = p_estimado,
          Erro_padrao = erro_padrao,
          IC_inferior = limite_inferior,
          IC_superior = limite_superior
        )
      )
    }
  }

  return(resultados)
}

ic_transicoes_assintotico <- calcular_ic_multinomial(matriz_contagens)

resumo_contagens_transicao <- data.frame(
  Total_transicoes = sum(matriz_contagens),
  Total_celulas = length(matriz_contagens),
  Celulas_com_zero = sum(matriz_contagens == 0),
  Menor_contagem_celula = min(matriz_contagens),
  Maior_contagem_celula = max(matriz_contagens),
  Menor_total_por_origem = min(rowSums(matriz_contagens)),
  Maior_total_por_origem = max(rowSums(matriz_contagens))
)

cat("\n============================================================\n")
cat("INTERVALOS DE CONFIANCA ASSINTOTICOS - MATRIZ DE TRANSICAO\n")
cat("============================================================\n")
print(arredondar_colunas_numericas(ic_transicoes_assintotico, 4))
cat("\nResumo das contagens usadas na inferencia:\n")
print(resumo_contagens_transicao)

# 12.2 Bootstrap parametrico de Cadeia de Markov
#
# O bootstrap abaixo simula varias trajetorias artificiais a partir de uma
# matriz suavizada. A estimativa pontual reportada continua sendo a MLE, mas a
# matriz de simulacao recebe uma pequena massa alpha em todas as celulas. Isso
# evita que zeros amostrais sejam tratados como transicoes teoricamente
# impossiveis durante o bootstrap.

suavizar_matriz_transicao <- function(contagens, alpha = 0.5) {
  k <- ncol(contagens)
  contagens_suavizadas <- contagens + alpha
  totais_suavizados <- rowSums(contagens) + alpha * k
  matriz_suavizada <- sweep(contagens_suavizadas, 1, totais_suavizados, "/")

  rownames(matriz_suavizada) <- rownames(contagens)
  colnames(matriz_suavizada) <- colnames(contagens)

  return(matriz_suavizada)
}

alpha_bootstrap <- 0.5
matriz_transicao_suavizada_bootstrap <- suavizar_matriz_transicao(
  matriz_contagens,
  alpha = alpha_bootstrap
)

simular_cadeia_markov <- function(n, estados, matriz, estado_inicial = NULL) {
  cadeia_simulada <- character(n)

  if (is.null(estado_inicial)) {
    estado_inicial <- estados[1]
  }

  cadeia_simulada[1] <- estado_inicial

  for (t in 2:n) {
    estado_anterior <- cadeia_simulada[t - 1]
    probabilidades <- as.numeric(matriz[estado_anterior, ])
    cadeia_simulada[t] <- sample(estados, size = 1, prob = probabilidades)
  }

  return(factor(cadeia_simulada, levels = estados))
}

bootstrap_matriz_transicao <- function(
    regimes,
    estados,
    matriz_simulacao,
    matriz_pontual,
    B = 1000,
    seed = 123) {
  set.seed(seed)

  n <- length(regimes)
  k <- length(estados)
  resultados <- array(NA_real_, dim = c(B, k, k))

  for (b in seq_len(B)) {
    regimes_boot <- simular_cadeia_markov(
      n = n,
      estados = estados,
      matriz = matriz_simulacao,
      estado_inicial = as.character(regimes[1])
    )

    matriz_boot <- calcular_matriz_transicao(regimes_boot, estados)$probabilidades
    resultados[b, , ] <- matriz_boot
  }

  saida <- data.frame()

  for (i in seq_along(estados)) {
    for (j in seq_along(estados)) {
      distribuicao_boot <- resultados[, i, j]

      saida <- rbind(
        saida,
        data.frame(
          Origem = estados[i],
          Destino = estados[j],
          Probabilidade_MLE = matriz_pontual[i, j],
          Probabilidade_suavizada_simulacao = matriz_simulacao[i, j],
          IC_boot_inferior = as.numeric(quantile(distribuicao_boot, 0.025, na.rm = TRUE)),
          IC_boot_superior = as.numeric(quantile(distribuicao_boot, 0.975, na.rm = TRUE))
        )
      )
    }
  }

  return(saida)
}

ic_transicoes_bootstrap <- bootstrap_matriz_transicao(
  regimes = ipca$regime,
  estados = estados,
  matriz_simulacao = matriz_transicao_suavizada_bootstrap,
  matriz_pontual = matriz_transicao,
  B = 1000,
  seed = 123
)

cat("\n============================================================\n")
cat("INTERVALOS DE CONFIANCA VIA BOOTSTRAP PARAMETRICO\n")
cat("============================================================\n")
cat("Matriz de simulacao suavizada com alpha = ", alpha_bootstrap, "\n", sep = "")
print(arredondar_colunas_numericas(ic_transicoes_bootstrap, 4))

# 12.3 Teste LR contra independencia temporal dos regimes
#
# Hipotese nula H0:
# O regime no periodo t independe do regime em t-1. Isto equivale a um modelo
# de ordem zero, no qual as probabilidades dos destinos sao iguais para todas
# as origens.
#
# Hipotese alternativa H1:
# O regime em t depende do regime em t-1.
#
# Importante: este teste mostra dependencia temporal entre regimes. Ele nao
# prova que a cadeia seja exatamente de primeira ordem, pois nao compara o
# modelo estimado com uma cadeia de segunda ordem.

log_verossimilhanca_markov <- function(contagens) {
  totais_linha <- rowSums(contagens)
  probabilidades <- sweep(contagens, 1, totais_linha, "/")
  termos_validos <- contagens > 0 & probabilidades > 0
  sum(contagens[termos_validos] * log(probabilidades[termos_validos]))
}

teste_lr_dependencia_temporal <- function(contagens) {
  k <- nrow(contagens)

  ll_markov <- as.numeric(log_verossimilhanca_markov(contagens))

  probabilidades_destino <- colSums(contagens) / sum(contagens)
  ll_independente <- 0

  for (j in seq_len(k)) {
    if (probabilidades_destino[j] > 0) {
      ll_independente <- ll_independente +
        sum(contagens[, j]) * log(probabilidades_destino[j])
    }
  }

  ll_independente <- as.numeric(ll_independente)
  estatistica_lr <- as.numeric(2 * (ll_markov - ll_independente))
  graus_liberdade <- (k - 1)^2
  valor_p <- as.numeric(pchisq(estatistica_lr, df = graus_liberdade, lower.tail = FALSE))

  data.frame(
    Modelo_nulo = "Independencia temporal",
    Modelo_alternativo = "Dependencia temporal de primeira defasagem",
    LogLik_nulo = ll_independente,
    LogLik_alternativo = ll_markov,
    Estatistica_LR = estatistica_lr,
    Graus_de_liberdade = graus_liberdade,
    Valor_p = valor_p
  )
}

teste_dependencia_temporal <- teste_lr_dependencia_temporal(matriz_contagens)

monte_carlo_dependencia_temporal <- function(regimes, estados, B = 1000, seed = 321) {
  set.seed(seed)

  regimes <- factor(regimes, levels = estados)
  estatistica_observada <- teste_lr_dependencia_temporal(
    calcular_matriz_transicao(regimes, estados)$contagens
  )$Estatistica_LR

  probabilidades_marginais <- as.numeric(prop.table(table(regimes)))
  estatisticas_simuladas <- numeric(B)

  for (b in seq_len(B)) {
    regimes_simulados <- factor(
      sample(estados, size = length(regimes), replace = TRUE, prob = probabilidades_marginais),
      levels = estados
    )

    contagens_simuladas <- calcular_matriz_transicao(regimes_simulados, estados)$contagens
    estatisticas_simuladas[b] <- teste_lr_dependencia_temporal(
      contagens_simuladas
    )$Estatistica_LR
  }

  data.frame(
    Teste = "Dependencia temporal contra independencia",
    Estatistica_LR_observada = estatistica_observada,
    Replicacoes_Monte_Carlo = B,
    Seed = seed,
    Valor_p_Monte_Carlo = (1 + sum(estatisticas_simuladas >= estatistica_observada)) / (B + 1)
  )
}

teste_dependencia_temporal_monte_carlo <- monte_carlo_dependencia_temporal(
  regimes = ipca$regime,
  estados = estados,
  B = 1000,
  seed = 321
)

cat("\n============================================================\n")
cat("TESTE LR - DEPENDENCIA TEMPORAL DOS REGIMES\n")
cat("============================================================\n")
print(teste_dependencia_temporal)
cat("\nP-valor por Monte Carlo:\n")
print(teste_dependencia_temporal_monte_carlo)

# 12.4 Teste LR de ordem da cadeia
#
# O teste anterior rejeita independencia temporal, mas nao verifica se uma
# cadeia de primeira ordem e suficiente. Para isso, comparamos a
# log-verossimilhanca do modelo de primeira ordem com a de um modelo de
# segunda ordem, no qual o destino depende do par de estados anteriores.
#
# O teste deve ser calculado sobre a mesma amostra de triplas:
#   H0: P(X_t | X_{t-1}, X_{t-2}) = P(X_t | X_{t-1})
#   H1: P(X_t | X_{t-1}, X_{t-2}) depende do par (X_{t-2}, X_{t-1})

teste_lr_ordem_markov <- function(regimes, estados) {
  regimes <- as.character(regimes)
  n <- length(regimes)
  k <- length(estados)

  if (n < 3) {
    stop("A sequencia precisa ter ao menos tres observacoes para o teste de ordem.")
  }

  origem_primeira_ordem <- factor(regimes[2:(n - 1)], levels = estados)
  destino <- factor(regimes[3:n], levels = estados)

  contagens_primeira_ordem <- as.matrix(table(
    Origem = origem_primeira_ordem,
    Destino = destino
  ))

  contexto_segunda_ordem <- interaction(
    factor(regimes[1:(n - 2)], levels = estados),
    factor(regimes[2:(n - 1)], levels = estados),
    sep = "|",
    drop = FALSE
  )

  contagens_segunda_ordem <- as.matrix(table(
    Contexto = contexto_segunda_ordem,
    Destino = destino
  ))

  ll_primeira_ordem <- as.numeric(log_verossimilhanca_markov(contagens_primeira_ordem))
  ll_segunda_ordem <- as.numeric(log_verossimilhanca_markov(contagens_segunda_ordem))
  estatistica_lr <- as.numeric(2 * (ll_segunda_ordem - ll_primeira_ordem))
  graus_liberdade <- k * (k - 1)^2
  valor_p <- as.numeric(pchisq(estatistica_lr, df = graus_liberdade, lower.tail = FALSE))

  list(
    resumo = data.frame(
      Modelo_nulo = "Cadeia de primeira ordem",
      Modelo_alternativo = "Cadeia de segunda ordem",
      Numero_de_triplas = n - 2,
      LogLik_primeira_ordem = ll_primeira_ordem,
      LogLik_segunda_ordem = ll_segunda_ordem,
      Estatistica_LR = estatistica_lr,
      Graus_de_liberdade = graus_liberdade,
      Valor_p = valor_p,
      Contextos_segunda_ordem_observados = sum(rowSums(contagens_segunda_ordem) > 0),
      Contextos_segunda_ordem_possiveis = k^2
    ),
    contagens_segunda_ordem = contagens_segunda_ordem
  )
}

teste_ordem_markov <- teste_lr_ordem_markov(ipca$regime, estados)
teste_ordem_markov_resumo <- teste_ordem_markov$resumo
contagens_segunda_ordem <- teste_ordem_markov$contagens_segunda_ordem

monte_carlo_ordem_markov <- function(
    regimes,
    estados,
    matriz_simulacao,
    B = 1000,
    seed = 654) {
  set.seed(seed)

  regimes <- factor(regimes, levels = estados)
  estatistica_observada <- teste_lr_ordem_markov(regimes, estados)$resumo$Estatistica_LR
  estatisticas_simuladas <- numeric(B)

  for (b in seq_len(B)) {
    regimes_simulados <- simular_cadeia_markov(
      n = length(regimes),
      estados = estados,
      matriz = matriz_simulacao,
      estado_inicial = as.character(regimes[1])
    )

    estatisticas_simuladas[b] <- teste_lr_ordem_markov(
      regimes_simulados,
      estados
    )$resumo$Estatistica_LR
  }

  data.frame(
    Teste = "Primeira ordem contra segunda ordem",
    Estatistica_LR_observada = estatistica_observada,
    Replicacoes_Monte_Carlo = B,
    Seed = seed,
    Alpha_suavizacao_matriz_simulacao = alpha_bootstrap,
    Valor_p_Monte_Carlo = (1 + sum(estatisticas_simuladas >= estatistica_observada)) / (B + 1)
  )
}

teste_ordem_markov_monte_carlo <- monte_carlo_ordem_markov(
  regimes = ipca$regime,
  estados = estados,
  matriz_simulacao = matriz_transicao_suavizada_bootstrap,
  B = 1000,
  seed = 654
)

cat("\n============================================================\n")
cat("TESTE LR - ORDEM DA CADEIA DE MARKOV\n")
cat("============================================================\n")
print(teste_ordem_markov_resumo)
cat("\nP-valor por Monte Carlo:\n")
print(teste_ordem_markov_monte_carlo)
cat("\nContagens de segunda ordem por contexto observado:\n")
print(contagens_segunda_ordem)

# 12.5 Teste LR de estabilidade temporal das matrizes de transicao
#
# Hipotese nula H0:
# Os periodos analisados possuem a mesma matriz de transicao.
#
# Hipotese alternativa H1:
# Ao menos um periodo possui matriz de transicao distinta.

teste_estabilidade_transicoes <- function(lista_regimes, nomes_periodos, estados) {
  lista_contagens <- lapply(
    lista_regimes,
    function(regimes) calcular_matriz_transicao(regimes, estados)$contagens
  )

  contagens_agregadas <- Reduce("+", lista_contagens)

  ll_restrito <- as.numeric(log_verossimilhanca_markov(contagens_agregadas))
  ll_irrestrito <- as.numeric(sum(sapply(lista_contagens, log_verossimilhanca_markov)))

  k <- length(estados)
  g <- length(lista_contagens)

  estatistica_lr <- as.numeric(2 * (ll_irrestrito - ll_restrito))
  graus_liberdade <- (g - 1) * k * (k - 1)
  valor_p <- as.numeric(pchisq(estatistica_lr, df = graus_liberdade, lower.tail = FALSE))

  data.frame(
    Periodos_comparados = paste(nomes_periodos, collapse = " / "),
    LogLik_restrito = ll_restrito,
    LogLik_irrestrito = ll_irrestrito,
    Estatistica_LR = estatistica_lr,
    Graus_de_liberdade = graus_liberdade,
    Valor_p = valor_p
  )
}

monte_carlo_estabilidade_transicoes <- function(
    lista_regimes,
    nomes_periodos,
    estados,
    B = 1000,
    seed = 987,
    alpha = 0.5) {
  set.seed(seed)

  teste_observado <- teste_estabilidade_transicoes(
    lista_regimes = lista_regimes,
    nomes_periodos = nomes_periodos,
    estados = estados
  )

  lista_contagens <- lapply(
    lista_regimes,
    function(regimes) calcular_matriz_transicao(regimes, estados)$contagens
  )

  contagens_agregadas <- Reduce("+", lista_contagens)
  matriz_simulacao <- suavizar_matriz_transicao(contagens_agregadas, alpha = alpha)
  estatisticas_simuladas <- numeric(B)

  for (b in seq_len(B)) {
    lista_simulada <- lapply(
      lista_regimes,
      function(regimes) {
        regimes <- factor(regimes, levels = estados)

        simular_cadeia_markov(
          n = length(regimes),
          estados = estados,
          matriz = matriz_simulacao,
          estado_inicial = as.character(regimes[1])
        )
      }
    )

    estatisticas_simuladas[b] <- teste_estabilidade_transicoes(
      lista_regimes = lista_simulada,
      nomes_periodos = nomes_periodos,
      estados = estados
    )$Estatistica_LR
  }

  data.frame(
    Periodos_comparados = teste_observado$Periodos_comparados,
    Estatistica_LR_observada = teste_observado$Estatistica_LR,
    Replicacoes_Monte_Carlo = B,
    Seed = seed,
    Alpha_suavizacao_matriz_simulacao = alpha,
    Valor_p_Monte_Carlo = (1 + sum(estatisticas_simuladas >= teste_observado$Estatistica_LR)) / (B + 1)
  )
}

teste_estabilidade <- teste_estabilidade_transicoes(
  lista_regimes = list(
    ipca_pre_pandemia$regime,
    ipca_pandemia$regime,
    ipca_pos_pandemia$regime
  ),
  nomes_periodos = c("Pre-pandemia", "Pandemia", "Pos-pandemia"),
  estados = estados
)

teste_estabilidade_monte_carlo <- monte_carlo_estabilidade_transicoes(
  lista_regimes = list(
    ipca_pre_pandemia$regime,
    ipca_pandemia$regime,
    ipca_pos_pandemia$regime
  ),
  nomes_periodos = c("Pre-pandemia", "Pandemia", "Pos-pandemia"),
  estados = estados,
  B = 1000,
  seed = 987,
  alpha = alpha_bootstrap
)

cat("\n============================================================\n")
cat("TESTE LR - ESTABILIDADE TEMPORAL DAS MATRIZES\n")
cat("============================================================\n")
print(teste_estabilidade)
cat("\nP-valor por Monte Carlo:\n")
print(teste_estabilidade_monte_carlo)

# Teste formal de estabilidade pre-Real versus pos-Real.
#
# Para esse teste, usamos os regimes da amostra completa, pois o teste de
# igualdade de matrizes exige que os estados tenham a mesma definicao nos dois
# periodos. A analise descritiva pre/pos-Real com tercis proprios continua
# sendo a principal para interpretacao economica de cada subperiodo.

teste_estabilidade_pre_pos_real_global <- teste_estabilidade_transicoes(
  lista_regimes = list(
    ipca_pre_real_base$regime,
    ipca_pos_real_base$regime
  ),
  nomes_periodos = c("Pre-Real", "Pos-Real"),
  estados = estados
)

teste_estabilidade_pre_pos_real_monte_carlo <- monte_carlo_estabilidade_transicoes(
  lista_regimes = list(
    ipca_pre_real_base$regime,
    ipca_pos_real_base$regime
  ),
  nomes_periodos = c("Pre-Real", "Pos-Real"),
  estados = estados,
  B = 1000,
  seed = 988,
  alpha = alpha_bootstrap
)

cat("\n============================================================\n")
cat("TESTE LR - ESTABILIDADE PRE-REAL VS POS-REAL\n")
cat("============================================================\n")
print(teste_estabilidade_pre_pos_real_global)
cat("\nP-valor por Monte Carlo:\n")
print(teste_estabilidade_pre_pos_real_monte_carlo)

# ------------------------------------------------
# 13. Endogeneizacao dos regimes e modelo latente
# ------------------------------------------------

# A classificacao fixa Baixa/Moderada/Alta e simples e interpretavel, mas usa
# limites escolhidos pelo pesquisador. Para reduzir a arbitrariedade, esta
# secao adiciona duas estrategias:
#
# 1) Regimes endogenos observaveis por k-means, nos quais os grupos sao
#    determinados pela propria distribuicao do IPCA.
# 2) Um modelo latente tipo Markov-Switching/HMM gaussiano, estimado por EM,
#    no qual o regime nao e observado diretamente.

# 13.1 Regimes endogenos observaveis por k-means

set.seed(123)

kmeans_ipca <- kmeans(ipca$ipca_mensal, centers = 3, nstart = 100)
centros_kmeans <- as.numeric(kmeans_ipca$centers)
ordem_kmeans <- order(centros_kmeans)
regime_kmeans_num <- match(kmeans_ipca$cluster, ordem_kmeans)

ipca$regime_kmeans <- factor(estados[regime_kmeans_num], levels = estados)

centros_kmeans_ordenados <- centros_kmeans[ordem_kmeans]
limites_kmeans <- c(
  mean(centros_kmeans_ordenados[1:2]),
  mean(centros_kmeans_ordenados[2:3])
)

resultado_transicao_kmeans <- calcular_matriz_transicao(ipca$regime_kmeans, estados)
matriz_contagens_kmeans <- resultado_transicao_kmeans$contagens
matriz_transicao_kmeans <- resultado_transicao_kmeans$probabilidades

persistencia_kmeans <- diag(matriz_transicao_kmeans)
names(persistencia_kmeans) <- estados

resumo_kmeans <- data.frame(
  Regime = estados,
  Centro_estimado_IPCA = centros_kmeans_ordenados,
  Probabilidade_de_permanencia = as.numeric(persistencia_kmeans),
  Duracao_esperada_em_meses = as.numeric(ifelse(
    persistencia_kmeans < 1,
    1 / (1 - persistencia_kmeans),
    Inf
  ))
)

limites_kmeans_tabela <- data.frame(
  Limite = c("Baixa-Moderada", "Moderada-Alta"),
  Valor_IPCA_mensal = limites_kmeans
)

cat("\n============================================================\n")
cat("REGIMES ENDOGENOS OBSERVAVEIS POR K-MEANS\n")
cat("============================================================\n")
cat("\nCentros estimados dos regimes:\n")
print(resumo_kmeans)
cat("\nLimites implicitos entre regimes:\n")
print(limites_kmeans_tabela)
cat("\nMatriz de transicao - k-means:\n")
print(round(matriz_transicao_kmeans, 4))

# 13.2 Regimes endogenos por k-means em escala logaritmica
#
# Como a amostra desde 1980 tem valores extremos de inflacao, o k-means em
# nivel pode ser dominado pelos anos de inflacao muito alta. Como verificacao
# de robustez, repetimos o agrupamento usando log(1 + IPCA), que comprime a
# escala sem alterar a ordem das observacoes.

if (all(ipca$ipca_mensal > -1, na.rm = TRUE)) {
  ipca$ipca_log1p <- log1p(ipca$ipca_mensal)

  set.seed(123)

  kmeans_ipca_log <- kmeans(ipca$ipca_log1p, centers = 3, nstart = 100)
  centros_kmeans_log <- as.numeric(kmeans_ipca_log$centers)
  ordem_kmeans_log <- order(centros_kmeans_log)
  regime_kmeans_log_num <- match(kmeans_ipca_log$cluster, ordem_kmeans_log)

  ipca$regime_kmeans_log1p <- factor(estados[regime_kmeans_log_num], levels = estados)

  centros_kmeans_log_ordenados <- centros_kmeans_log[ordem_kmeans_log]
  centros_kmeans_log_escala_original <- expm1(centros_kmeans_log_ordenados)

  limites_kmeans_log <- c(
    mean(centros_kmeans_log_ordenados[1:2]),
    mean(centros_kmeans_log_ordenados[2:3])
  )

  limites_kmeans_log_escala_original <- expm1(limites_kmeans_log)

  resultado_transicao_kmeans_log <- calcular_matriz_transicao(ipca$regime_kmeans_log1p, estados)
  matriz_contagens_kmeans_log <- resultado_transicao_kmeans_log$contagens
  matriz_transicao_kmeans_log <- resultado_transicao_kmeans_log$probabilidades

  persistencia_kmeans_log <- diag(matriz_transicao_kmeans_log)
  names(persistencia_kmeans_log) <- estados

  resumo_kmeans_log <- data.frame(
    Regime = estados,
    Centro_estimado_log1p = centros_kmeans_log_ordenados,
    Centro_estimado_IPCA_aproximado = centros_kmeans_log_escala_original,
    Probabilidade_de_permanencia = as.numeric(persistencia_kmeans_log),
    Duracao_esperada_em_meses = as.numeric(ifelse(
      persistencia_kmeans_log < 1,
      1 / (1 - persistencia_kmeans_log),
      Inf
    ))
  )

  limites_kmeans_log_tabela <- data.frame(
    Limite = c("Baixa-Moderada", "Moderada-Alta"),
    Valor_log1p = limites_kmeans_log,
    Valor_IPCA_mensal_aproximado = limites_kmeans_log_escala_original
  )

  cat("\n============================================================\n")
  cat("ROBUSTEZ - K-MEANS EM ESCALA LOG(1 + IPCA)\n")
  cat("============================================================\n")
  cat("\nCentros estimados dos regimes:\n")
  print(resumo_kmeans_log)
  cat("\nLimites implicitos entre regimes:\n")
  print(limites_kmeans_log_tabela)
  cat("\nMatriz de transicao - k-means log1p:\n")
  print(round(matriz_transicao_kmeans_log, 4))
}

# 13.3 Modelo latente tipo Markov-Switching/HMM gaussiano
#
# O modelo assume que cada observacao do IPCA mensal vem de um dos tres
# regimes latentes. Condicional ao regime s_t, a inflacao segue uma distribuicao
# normal com media e desvio-padrao especificos daquele regime. A transicao entre
# regimes e governada por uma matriz de Markov nao observada diretamente.
#
# Esta e uma versao didatica e autocontida de um modelo Markov-Switching,
# implementada sem depender de pacotes externos adicionais.

forward_backward_hmm <- function(y, pi_inicial, matriz, medias, desvios) {
  n <- length(y)
  k <- length(pi_inicial)

  densidades <- matrix(0, nrow = n, ncol = k)

  for (j in seq_len(k)) {
    densidades[, j] <- dnorm(
      y,
      mean = medias[j],
      sd = max(desvios[j], 1e-6)
    )
  }

  densidades <- pmax(densidades, 1e-300)

  alpha <- matrix(0, nrow = n, ncol = k)
  beta <- matrix(0, nrow = n, ncol = k)
  escala <- numeric(n)

  alpha[1, ] <- pi_inicial * densidades[1, ]
  escala[1] <- sum(alpha[1, ])
  alpha[1, ] <- alpha[1, ] / escala[1]

  for (t in 2:n) {
    alpha[t, ] <- as.numeric(alpha[t - 1, ] %*% matriz) * densidades[t, ]
    escala[t] <- sum(alpha[t, ])

    if (!is.finite(escala[t]) || escala[t] <= 0) {
      escala[t] <- 1e-300
    }

    alpha[t, ] <- alpha[t, ] / escala[t]
  }

  beta[n, ] <- rep(1, k)

  for (t in (n - 1):1) {
    beta[t, ] <- as.numeric(
      matriz %*% (densidades[t + 1, ] * beta[t + 1, ])
    ) / escala[t + 1]
  }

  gamma <- alpha * beta
  gamma <- gamma / rowSums(gamma)

  xi <- array(0, dim = c(n - 1, k, k))

  for (t in seq_len(n - 1)) {
    numerador <- matriz * outer(alpha[t, ], densidades[t + 1, ] * beta[t + 1, ])
    denominador <- sum(numerador)

    if (is.finite(denominador) && denominador > 0) {
      xi[t, , ] <- numerador / denominador
    }
  }

  loglik <- sum(log(escala))

  return(list(
    alpha = alpha,
    beta = beta,
    gamma = gamma,
    xi = xi,
    loglik = loglik
  ))
}

ajustar_hmm_gaussiano <- function(y, estados, max_iter = 500, tolerancia = 1e-8) {
  k <- length(estados)
  n <- length(y)

  inicial_kmeans <- kmeans(y, centers = k, nstart = 100)
  centros <- as.numeric(inicial_kmeans$centers)
  ordem <- order(centros)
  estado_inicial_num <- match(inicial_kmeans$cluster, ordem)

  medias <- centros[ordem]
  desvios <- numeric(k)

  for (j in seq_len(k)) {
    desvio_j <- sd(y[estado_inicial_num == j])
    desvios[j] <- ifelse(is.na(desvio_j) || desvio_j == 0, sd(y), desvio_j)
  }

  contagens_iniciais <- calcular_matriz_transicao(
    factor(estados[estado_inicial_num], levels = estados),
    estados
  )$contagens

  suavizacao <- 0.5
  matriz <- sweep(
    contagens_iniciais + suavizacao,
    1,
    rowSums(contagens_iniciais) + k * suavizacao,
    "/"
  )

  pi_inicial <- rep(1 / k, k)
  loglik_anterior <- -Inf
  desvio_minimo <- max(sd(y) * 0.05, 0.01)

  for (iteracao in seq_len(max_iter)) {
    fb <- forward_backward_hmm(y, pi_inicial, matriz, medias, desvios)

    gamma <- fb$gamma
    xi <- fb$xi

    pi_inicial <- gamma[1, ] / sum(gamma[1, ])

    for (i in seq_len(k)) {
      denominador <- sum(gamma[1:(n - 1), i])

      if (denominador > 0) {
        matriz[i, ] <- colSums(xi[, i, ]) / denominador
      } else {
        matriz[i, ] <- rep(1 / k, k)
      }
    }

    matriz <- matriz / rowSums(matriz)

    pesos <- colSums(gamma)

    for (j in seq_len(k)) {
      medias[j] <- sum(gamma[, j] * y) / pesos[j]
      desvios[j] <- sqrt(sum(gamma[, j] * (y - medias[j])^2) / pesos[j])
      desvios[j] <- max(desvios[j], desvio_minimo)
    }

    if (abs(fb$loglik - loglik_anterior) < tolerancia) {
      break
    }

    loglik_anterior <- fb$loglik
  }

  ordem_final <- order(medias)

  pi_inicial <- pi_inicial[ordem_final]
  matriz <- matriz[ordem_final, ordem_final]
  medias <- medias[ordem_final]
  desvios <- desvios[ordem_final]

  fb_final <- forward_backward_hmm(y, pi_inicial, matriz, medias, desvios)

  rownames(matriz) <- estados
  colnames(matriz) <- estados
  colnames(fb_final$gamma) <- estados

  numero_parametros <- (k - 1) + k * (k - 1) + 2 * k

  return(list(
    pi = pi_inicial,
    matriz = matriz,
    medias = medias,
    desvios = desvios,
    probabilidades_suavizadas = fb_final$gamma,
    loglik = fb_final$loglik,
    iteracoes = iteracao,
    numero_parametros = numero_parametros,
    AIC = -2 * fb_final$loglik + 2 * numero_parametros,
    BIC = -2 * fb_final$loglik + log(n) * numero_parametros
  ))
}

set.seed(123)
modelo_hmm <- ajustar_hmm_gaussiano(ipca$ipca_mensal, estados)

matriz_transicao_hmm <- modelo_hmm$matriz
probabilidades_hmm <- modelo_hmm$probabilidades_suavizadas
regime_hmm <- estados[max.col(probabilidades_hmm, ties.method = "first")]

ipca$regime_hmm <- factor(regime_hmm, levels = estados)

for (estado in estados) {
  ipca[[paste0("prob_hmm_", estado)]] <- probabilidades_hmm[, estado]
}

persistencia_hmm <- diag(matriz_transicao_hmm)
names(persistencia_hmm) <- estados

resumo_hmm <- data.frame(
  Regime = estados,
  Media_IPCA = modelo_hmm$medias,
  Desvio_padrao_IPCA = modelo_hmm$desvios,
  Probabilidade_de_permanencia = as.numeric(persistencia_hmm),
  Duracao_esperada_em_meses = as.numeric(ifelse(
    persistencia_hmm < 1,
    1 / (1 - persistencia_hmm),
    Inf
  ))
)

criterios_hmm <- data.frame(
  LogLik = modelo_hmm$loglik,
  Numero_parametros = modelo_hmm$numero_parametros,
  AIC = modelo_hmm$AIC,
  BIC = modelo_hmm$BIC,
  Iteracoes_EM = modelo_hmm$iteracoes
)

especificacao_hmm <- data.frame(
  Modelo = "HMM gaussiano em nivel",
  Estados_latentes = length(estados),
  Algoritmo = "EM com filtro forward-backward escalado",
  Inicializacao = "k-means com 3 grupos, nstart = 100",
  Ordenacao_estados = "Medias estimadas em ordem crescente",
  Seed_global = 123,
  Max_iter = 500,
  Tolerancia = 1e-8,
  Iteracoes_EM = modelo_hmm$iteracoes,
  Observacao = "Extensao auxiliar; em nivel, tende a capturar fortemente a quebra entre inflacao cronica e estabilizacao."
)

cat("\n============================================================\n")
cat("MODELO LATENTE TIPO MARKOV-SWITCHING/HMM GAUSSIANO\n")
cat("============================================================\n")
cat("\nResumo dos regimes latentes:\n")
print(resumo_hmm)
cat("\nMatriz de transicao latente:\n")
print(round(matriz_transicao_hmm, 4))
cat("\nCriterios de ajuste:\n")
print(criterios_hmm)

# 13.4 Robustez: HMM gaussiano em escala log(1 + IPCA)
#
# O HMM em nivel tende a capturar fortemente a diferenca entre inflacao
# cronica pre-Real e estabilidade pos-Real. A estimacao em log(1 + IPCA)
# reduz a influencia dos valores extremos e funciona como verificacao de
# robustez. A interpretacao dos estados continua sendo auxiliar.

if (all(ipca$ipca_mensal > -1, na.rm = TRUE)) {
  set.seed(123)
  modelo_hmm_log <- ajustar_hmm_gaussiano(ipca$ipca_log1p, estados)

  matriz_transicao_hmm_log <- modelo_hmm_log$matriz
  probabilidades_hmm_log <- modelo_hmm_log$probabilidades_suavizadas
  regime_hmm_log <- estados[max.col(probabilidades_hmm_log, ties.method = "first")]

  ipca$regime_hmm_log1p <- factor(regime_hmm_log, levels = estados)

  for (estado in estados) {
    ipca[[paste0("prob_hmm_log1p_", estado)]] <- probabilidades_hmm_log[, estado]
  }

  persistencia_hmm_log <- diag(matriz_transicao_hmm_log)
  names(persistencia_hmm_log) <- estados

  resumo_hmm_log <- data.frame(
    Regime = estados,
    Media_log1p = modelo_hmm_log$medias,
    Media_IPCA_aproximada = expm1(modelo_hmm_log$medias),
    Desvio_padrao_log1p = modelo_hmm_log$desvios,
    Probabilidade_de_permanencia = as.numeric(persistencia_hmm_log),
    Duracao_esperada_em_meses = as.numeric(ifelse(
      persistencia_hmm_log < 1,
      1 / (1 - persistencia_hmm_log),
      Inf
    ))
  )

  criterios_hmm_log <- data.frame(
    LogLik = modelo_hmm_log$loglik,
    Numero_parametros = modelo_hmm_log$numero_parametros,
    AIC = modelo_hmm_log$AIC,
    BIC = modelo_hmm_log$BIC,
    Iteracoes_EM = modelo_hmm_log$iteracoes
  )

  especificacao_hmm_log <- data.frame(
    Modelo = "HMM gaussiano em log(1 + IPCA)",
    Estados_latentes = length(estados),
    Algoritmo = "EM com filtro forward-backward escalado",
    Inicializacao = "k-means com 3 grupos, nstart = 100",
    Ordenacao_estados = "Medias estimadas em ordem crescente",
    Seed_global = 123,
    Max_iter = 500,
    Tolerancia = 1e-8,
    Iteracoes_EM = modelo_hmm_log$iteracoes,
    Observacao = "Robustez auxiliar para reduzir a influencia de valores extremos da inflacao pre-Real."
  )

  cat("\n============================================================\n")
  cat("ROBUSTEZ - HMM GAUSSIANO EM ESCALA LOG(1 + IPCA)\n")
  cat("============================================================\n")
  cat("\nResumo dos regimes latentes em escala log1p:\n")
  print(resumo_hmm_log)
  cat("\nMatriz de transicao latente - HMM log1p:\n")
  print(round(matriz_transicao_hmm_log, 4))
  cat("\nCriterios de ajuste - HMM log1p:\n")
  print(criterios_hmm_log)
}

# 13.5 Probabilidades de transicao variaveis no tempo - TVTP empirico
#
# Uma extensao natural em macroeconomia e permitir que as probabilidades de
# transicao dependam de variaveis de estado. Aqui usamos uma aproximacao
# aplicada por logit multinomial com interacoes entre a origem e as covariaveis:
#
# Destino_{t+1} = f(Origem_t, IPCA_t, Selic_t, Origem_t x IPCA_t,
#                   Origem_t x Selic_t)
#
# Isso nao substitui um modelo TVTP estrutural completo estimado diretamente
# sobre todos os elementos p_ij(t). A saida deve ser lida como probabilidades
# condicionais previstas pelo logit, e nao como uma matriz de transicao fixa.
#
# Como a Selic e os regimes inflacionarios estabilizados sao mais coerentes no
# periodo pos-Real, esta extensao usa a subamostra pos-Real e os tercis proprios
# desse subperiodo. Assim, "Alta" significa pressao inflacionaria relativa ao
# ambiente de estabilizacao, e nao hiperinflacao historica.

base_tvtp_ipca <- ipca[ipca$data >= corte_plano_real, ]

transicoes_tvtp <- data.frame(
  data_origem = head(base_tvtp_ipca$data, -1),
  data_destino = tail(base_tvtp_ipca$data, -1),
  origem = factor(head(base_tvtp_ipca$regime_tercis_periodo, -1), levels = estados),
  destino = factor(tail(base_tvtp_ipca$regime_tercis_periodo, -1), levels = estados),
  ipca_origem = head(base_tvtp_ipca$ipca_mensal, -1),
  selic_origem = head(base_tvtp_ipca$selic_meta, -1)
)

transicoes_tvtp$ipca_origem_padronizado <- as.numeric(scale(transicoes_tvtp$ipca_origem))

usar_selic_tvtp <- sum(!is.na(transicoes_tvtp$selic_origem)) > 30 &&
  sd(transicoes_tvtp$selic_origem, na.rm = TRUE) > 0

if (usar_selic_tvtp) {
  transicoes_tvtp$selic_origem_padronizada <- as.numeric(scale(transicoes_tvtp$selic_origem))
  base_tvtp <- transicoes_tvtp[complete.cases(
    transicoes_tvtp[, c("origem", "destino", "ipca_origem_padronizado", "selic_origem_padronizada")]
  ), ]

  formula_tvtp <- destino ~ origem * (ipca_origem_padronizado + selic_origem_padronizada)
} else {
  base_tvtp <- transicoes_tvtp[complete.cases(
    transicoes_tvtp[, c("origem", "destino", "ipca_origem_padronizado")]
  ), ]

  formula_tvtp <- destino ~ origem * ipca_origem_padronizado
}

modelo_tvtp <- nnet::multinom(
  formula = formula_tvtp,
  data = base_tvtp,
  trace = FALSE,
  maxit = 1000
)

modelo_tvtp_nulo <- nnet::multinom(
  formula = destino ~ origem,
  data = base_tvtp,
  trace = FALSE,
  maxit = 1000
)

probabilidades_tvtp <- predict(modelo_tvtp, type = "probs")

if (is.vector(probabilidades_tvtp)) {
  probabilidades_tvtp <- cbind(
    1 - probabilidades_tvtp,
    probabilidades_tvtp
  )
}

probabilidades_tvtp <- as.matrix(probabilidades_tvtp)

coeficientes_tvtp <- summary(modelo_tvtp)$coefficients
erros_tvtp <- summary(modelo_tvtp)$standard.errors

coeficientes_tvtp_tabela <- data.frame()

for (i in seq_len(nrow(coeficientes_tvtp))) {
  for (j in seq_len(ncol(coeficientes_tvtp))) {
    z_valor <- coeficientes_tvtp[i, j] / erros_tvtp[i, j]

    coeficientes_tvtp_tabela <- rbind(
      coeficientes_tvtp_tabela,
      data.frame(
        Destino = rownames(coeficientes_tvtp)[i],
        Variavel = colnames(coeficientes_tvtp)[j],
        Coeficiente = coeficientes_tvtp[i, j],
        Erro_padrao = erros_tvtp[i, j],
        Estatistica_z = z_valor,
        Valor_p = 2 * pnorm(abs(z_valor), lower.tail = FALSE)
      )
    )
  }
}

loglik_tvtp <- as.numeric(logLik(modelo_tvtp))
loglik_tvtp_nulo <- as.numeric(logLik(modelo_tvtp_nulo))
df_tvtp <- attr(logLik(modelo_tvtp), "df")
df_tvtp_nulo <- attr(logLik(modelo_tvtp_nulo), "df")

teste_tvtp <- data.frame(
  Modelo_nulo = "Logit multinomial com origem apenas",
  Modelo_alternativo = "Logit multinomial com origem, variaveis macro e interacoes",
  LogLik_nulo = loglik_tvtp_nulo,
  LogLik_alternativo = loglik_tvtp,
  Estatistica_LR = 2 * (loglik_tvtp - loglik_tvtp_nulo),
  Graus_de_liberdade = df_tvtp - df_tvtp_nulo,
  Valor_p = pchisq(
    2 * (loglik_tvtp - loglik_tvtp_nulo),
    df = df_tvtp - df_tvtp_nulo,
    lower.tail = FALSE
  )
)

probabilidades_tvtp_tabela <- data.frame(
  data_origem = base_tvtp$data_origem,
  data_destino = base_tvtp$data_destino,
  origem = base_tvtp$origem,
  destino_observado = base_tvtp$destino,
  probabilidades_tvtp
)

probabilidades_medias_tvtp <- aggregate(
  probabilidades_tvtp,
  by = list(Origem = base_tvtp$origem),
  FUN = mean
)

nomes_colunas_prob <- colnames(probabilidades_tvtp)

if (is.null(nomes_colunas_prob)) {
  nomes_colunas_prob <- estados
}

colnames(probabilidades_medias_tvtp) <- c("Origem", nomes_colunas_prob)

contagens_tvtp <- as.matrix(table(
  Origem = base_tvtp$origem,
  Destino = base_tvtp$destino
))

max_abs_coef_tvtp <- max(abs(coeficientes_tvtp), na.rm = TRUE)
min_prob_prevista_tvtp <- min(probabilidades_tvtp, na.rm = TRUE)
max_prob_prevista_tvtp <- max(probabilidades_tvtp, na.rm = TRUE)

diagnostico_tvtp <- data.frame(
  Amostra_efetiva = nrow(base_tvtp),
  Numero_parametros = df_tvtp,
  LogLik = loglik_tvtp,
  AIC = AIC(modelo_tvtp),
  BIC = BIC(modelo_tvtp),
  Codigo_convergencia = modelo_tvtp$convergence,
  Convergiu = modelo_tvtp$convergence == 0,
  Menor_contagem_celula = min(contagens_tvtp),
  Celulas_com_zero = sum(contagens_tvtp == 0),
  Maior_coeficiente_absoluto = max_abs_coef_tvtp,
  Menor_probabilidade_prevista = min_prob_prevista_tvtp,
  Maior_probabilidade_prevista = max_prob_prevista_tvtp,
  Criterio_quase_separacao = "Alerta se maior |coef| > 10, menor probabilidade prevista < 1e-6 ou celulas com zero > 0.",
  Alerta_quase_separacao = max_abs_coef_tvtp > 10 ||
    min_prob_prevista_tvtp < 1e-6 ||
    sum(contagens_tvtp == 0) > 0
)

especificacao_tvtp <- data.frame(
  Modelo = "Logit multinomial aproximado para TVTP",
  Formula = paste(deparse(formula_tvtp), collapse = " "),
  Modelo_nulo = "destino ~ origem",
  Amostra = "Pos-Real com tercis proprios do subperiodo",
  Amostra_efetiva = nrow(base_tvtp),
  Data_inicial = as.character(min(base_tvtp$data_origem)),
  Data_final = as.character(max(base_tvtp$data_origem)),
  Usa_Selic = usar_selic_tvtp,
  Covariaveis = ifelse(
    usar_selic_tvtp,
    "IPCA mensal de origem e meta Selic de origem, ambas padronizadas",
    "IPCA mensal de origem padronizado"
  ),
  Defasagem = "Covariaveis observadas no periodo de origem t; destino em t+1",
  Interpretacao = "Probabilidades condicionais previstas; nao interpretar como matriz de transicao fixa nem como efeito causal."
)

cat("\n============================================================\n")
cat("TVTP EMPIRICO - LOGIT MULTINOMIAL COM INTERACOES\n")
cat("============================================================\n")
cat("\nFormula estimada:\n")
print(formula_tvtp)
cat("\nTeste LR do modelo TVTP contra modelo homogeneo com origem apenas:\n")
print(teste_tvtp)
cat("\nDiagnostico do ajuste TVTP:\n")
print(arredondar_colunas_numericas(diagnostico_tvtp, 4))
cat("\nProbabilidades medias previstas por origem, nao uma matriz fixa:\n")
print(arredondar_colunas_numericas(probabilidades_medias_tvtp, 4))

# Notas metodologicas para acompanhar os resultados exportados.
#
# Essas notas ajudam a evitar interpretacoes fortes demais no texto final do
# trabalho. Em especial, a comparacao pre-Real/pos-Real com tercis proprios e
# uma comparacao relativa dentro de cada ambiente macroeconomico, nao uma
# comparacao de niveis absolutos de inflacao.

notas_metodologicas <- data.frame(
  Topico = c(
    "Amostra completa",
    "Tercis por subperiodo",
    "Teste pre-Real vs pos-Real",
    "Transicoes estimadas como zero",
    "Diagnosticos da matriz",
    "Bootstrap parametrico",
    "Testes LR",
    "K-means e HMM",
    "Teste LR de dependencia temporal",
    "Teste LR de ordem da cadeia",
    "TVTP empirico",
    "Validacao preditiva",
    "Sensibilidade de escala"
  ),
  Nota = c(
    "A amostra completa de 1980 em diante e uma referencia historica agregada; sua interpretacao exige cautela porque combina regimes monetarios distintos.",
    "Nas subamostras pre-Real e pos-Real, Baixa, Moderada e Alta sao categorias relativas a distribuicao interna de cada periodo, nao os mesmos niveis absolutos de inflacao.",
    "O teste formal pre-Real versus pos-Real usa regimes globais para garantir que os estados tenham a mesma definicao nas duas matrizes comparadas.",
    "Probabilidades estimadas como zero indicam que a transicao nao foi observada na amostra; nao significam impossibilidade teorica.",
    "Irredutibilidade, aperiodicidade, reversibilidade, autovalores, gap espectral e tempo de mistura foram calculados para a matriz empirica da amostra completa.",
    "O bootstrap parametrico usa uma matriz suavizada com alpha = 0,5 apenas na simulacao, preservando a matriz MLE como estimativa pontual.",
    "Os testes LR assintoticos foram mantidos, mas p-valores de Monte Carlo foram adicionados para reduzir dependencia exclusiva da aproximacao qui-quadrado em amostras com celulas raras.",
    "K-means, HMM e TVTP devem ser lidos como extensoes e verificacoes de robustez, enquanto o nucleo interpretativo e a analise por tercis e a separacao pre/pos-Real.",
    "O teste LR compara dependencia temporal contra independencia; ele mostra que os regimes consecutivos nao se comportam como observacoes independentes.",
    "O teste LR de primeira versus segunda ordem compara verossimilhancas sobre a mesma amostra de triplas; sua leitura deve considerar a baixa frequencia de alguns contextos de segunda ordem.",
    "O TVTP foi aproximado por logit multinomial no pos-Real, usando tercis proprios e interacoes entre origem e covariaveis; as probabilidades previstas nao devem ser interpretadas como matriz fixa nem como relacao causal.",
    "O script nao realiza validacao preditiva fora da amostra; os resultados descrevem persistencia e dinamica historica dentro da amostra.",
    "Resultados em nivel podem ser influenciados pela escala extrema da inflacao pre-Real; por isso k-means e HMM em log(1 + IPCA) sao reportados como robustez."
  )
)

# ------------------------------------------------
# 14. Graficos e arquivos de saida
# ------------------------------------------------

# Criacao de uma pasta para guardar os resultados.

pasta_resultados <- file.path(getwd(), "resultados_markov_ipca")

if (!dir.exists(pasta_resultados)) {
  dir.create(pasta_resultados, recursive = TRUE)
}

# Salvamento das tabelas principais.

write.csv2(
  limiares_regimes,
  file = file.path(pasta_resultados, "00_limiares_regimes_tercis.csv"),
  row.names = FALSE
)

write.csv2(
  ipca,
  file = file.path(pasta_resultados, "01_ipca_classificado_regimes.csv"),
  row.names = FALSE
)

write.csv2(
  round(matriz_contagens, 0),
  file = file.path(pasta_resultados, "02_matriz_contagens.csv")
)

write.csv2(
  round(matriz_transicao, 4),
  file = file.path(pasta_resultados, "03_matriz_transicao.csv")
)

write.csv2(
  resumo_regimes,
  file = file.path(pasta_resultados, "04_resumo_persistencia.csv"),
  row.names = FALSE
)

write.csv2(
  as.data.frame(distribuicao_estacionaria),
  file = file.path(pasta_resultados, "05_distribuicao_estacionaria.csv"),
  row.names = FALSE
)

write.csv2(
  diagnosticos_matriz,
  file = file.path(pasta_resultados, "37_diagnosticos_matriz_markov.csv"),
  row.names = FALSE
)

write.csv2(
  periodos_estados,
  file = file.path(pasta_resultados, "38_periodos_estados_markov.csv"),
  row.names = FALSE
)

write.csv2(
  autovalores_matriz,
  file = file.path(pasta_resultados, "39_autovalores_matriz_transicao.csv"),
  row.names = FALSE
)

write.csv2(
  matriz_alcancabilidade,
  file = file.path(pasta_resultados, "40_matriz_alcancabilidade.csv")
)

write.csv2(
  resumo_contagens_transicao,
  file = file.path(pasta_resultados, "41_resumo_contagens_transicao.csv"),
  row.names = FALSE
)

write.csv2(
  round(matriz_transicao_suavizada_bootstrap, 4),
  file = file.path(pasta_resultados, "42_matriz_transicao_suavizada_bootstrap.csv")
)

write.csv2(
  resumo_periodos,
  file = file.path(pasta_resultados, "06_comparacao_periodos.csv"),
  row.names = FALSE
)

write.csv2(
  ic_transicoes_assintotico,
  file = file.path(pasta_resultados, "07_ic_transicoes_assintotico.csv"),
  row.names = FALSE
)

write.csv2(
  ic_transicoes_bootstrap,
  file = file.path(pasta_resultados, "08_ic_transicoes_bootstrap.csv"),
  row.names = FALSE
)

write.csv2(
  teste_dependencia_temporal,
  file = file.path(pasta_resultados, "09_teste_lr_dependencia_temporal.csv"),
  row.names = FALSE
)

write.csv2(
  teste_dependencia_temporal_monte_carlo,
  file = file.path(pasta_resultados, "43_teste_lr_dependencia_temporal_monte_carlo.csv"),
  row.names = FALSE
)

write.csv2(
  teste_ordem_markov_resumo,
  file = file.path(pasta_resultados, "09b_teste_lr_ordem_markov.csv"),
  row.names = FALSE
)

write.csv2(
  contagens_segunda_ordem,
  file = file.path(pasta_resultados, "09c_contagens_segunda_ordem.csv")
)

write.csv2(
  teste_ordem_markov_monte_carlo,
  file = file.path(pasta_resultados, "44_teste_lr_ordem_markov_monte_carlo.csv"),
  row.names = FALSE
)

write.csv2(
  teste_estabilidade,
  file = file.path(pasta_resultados, "10_teste_lr_estabilidade_temporal.csv"),
  row.names = FALSE
)

write.csv2(
  teste_estabilidade_monte_carlo,
  file = file.path(pasta_resultados, "45_teste_lr_estabilidade_temporal_monte_carlo.csv"),
  row.names = FALSE
)

write.csv2(
  limiares_pre_pos_real,
  file = file.path(pasta_resultados, "22_limiares_pre_pos_real_tercis_proprios.csv"),
  row.names = FALSE
)

write.csv2(
  resumo_pre_pos_real,
  file = file.path(pasta_resultados, "23_resumo_pre_pos_real_tercis_proprios.csv"),
  row.names = FALSE
)

write.csv2(
  round(analise_pre_real$contagens, 0),
  file = file.path(pasta_resultados, "24_matriz_contagens_pre_real_tercis_proprios.csv")
)

write.csv2(
  round(analise_pos_real$contagens, 0),
  file = file.path(pasta_resultados, "25_matriz_contagens_pos_real_tercis_proprios.csv")
)

write.csv2(
  round(analise_pre_real$matriz, 4),
  file = file.path(pasta_resultados, "26_matriz_transicao_pre_real_tercis_proprios.csv")
)

write.csv2(
  round(analise_pos_real$matriz, 4),
  file = file.path(pasta_resultados, "27_matriz_transicao_pos_real_tercis_proprios.csv")
)

write.csv2(
  teste_estabilidade_pre_pos_real_global,
  file = file.path(pasta_resultados, "28_teste_lr_estabilidade_pre_pos_real_regimes_globais.csv"),
  row.names = FALSE
)

write.csv2(
  teste_estabilidade_pre_pos_real_monte_carlo,
  file = file.path(pasta_resultados, "46_teste_lr_estabilidade_pre_pos_real_monte_carlo.csv"),
  row.names = FALSE
)

write.csv2(
  notas_metodologicas,
  file = file.path(pasta_resultados, "29_notas_metodologicas.csv"),
  row.names = FALSE
)

write.csv2(
  limites_kmeans_tabela,
  file = file.path(pasta_resultados, "11_limites_endogenos_kmeans.csv"),
  row.names = FALSE
)

write.csv2(
  resumo_kmeans,
  file = file.path(pasta_resultados, "12_resumo_regimes_kmeans.csv"),
  row.names = FALSE
)

write.csv2(
  round(matriz_transicao_kmeans, 4),
  file = file.path(pasta_resultados, "13_matriz_transicao_kmeans.csv")
)

if (exists("resumo_kmeans_log")) {
  write.csv2(
    limites_kmeans_log_tabela,
    file = file.path(pasta_resultados, "30_limites_kmeans_log1p.csv"),
    row.names = FALSE
  )

  write.csv2(
    resumo_kmeans_log,
    file = file.path(pasta_resultados, "31_resumo_kmeans_log1p.csv"),
    row.names = FALSE
  )

  write.csv2(
    round(matriz_transicao_kmeans_log, 4),
    file = file.path(pasta_resultados, "32_matriz_transicao_kmeans_log1p.csv")
  )
}

write.csv2(
  resumo_hmm,
  file = file.path(pasta_resultados, "14_resumo_regimes_hmm.csv"),
  row.names = FALSE
)

write.csv2(
  round(matriz_transicao_hmm, 4),
  file = file.path(pasta_resultados, "15_matriz_transicao_hmm.csv")
)

write.csv2(
  criterios_hmm,
  file = file.path(pasta_resultados, "16_criterios_ajuste_hmm.csv"),
  row.names = FALSE
)

write.csv2(
  especificacao_hmm,
  file = file.path(pasta_resultados, "47_especificacao_hmm.csv"),
  row.names = FALSE
)

probabilidades_hmm_tabela <- data.frame(
  data = ipca$data,
  ipca_mensal = ipca$ipca_mensal,
  regime_hmm = ipca$regime_hmm,
  probabilidades_hmm
)

write.csv2(
  probabilidades_hmm_tabela,
  file = file.path(pasta_resultados, "17_probabilidades_suavizadas_hmm.csv"),
  row.names = FALSE
)

if (exists("resumo_hmm_log")) {
  write.csv2(
    resumo_hmm_log,
    file = file.path(pasta_resultados, "33_resumo_hmm_log1p.csv"),
    row.names = FALSE
  )

  write.csv2(
    round(matriz_transicao_hmm_log, 4),
    file = file.path(pasta_resultados, "34_matriz_transicao_hmm_log1p.csv")
  )

  write.csv2(
    criterios_hmm_log,
    file = file.path(pasta_resultados, "35_criterios_ajuste_hmm_log1p.csv"),
    row.names = FALSE
  )

  write.csv2(
    especificacao_hmm_log,
    file = file.path(pasta_resultados, "48_especificacao_hmm_log1p.csv"),
    row.names = FALSE
  )

  probabilidades_hmm_log_tabela <- data.frame(
    data = ipca$data,
    ipca_mensal = ipca$ipca_mensal,
    ipca_log1p = ipca$ipca_log1p,
    regime_hmm_log1p = ipca$regime_hmm_log1p,
    probabilidades_hmm_log
  )

  write.csv2(
    probabilidades_hmm_log_tabela,
    file = file.path(pasta_resultados, "36_probabilidades_suavizadas_hmm_log1p.csv"),
    row.names = FALSE
  )
}

write.csv2(
  especificacao_tvtp,
  file = file.path(pasta_resultados, "49_especificacao_tvtp.csv"),
  row.names = FALSE
)

write.csv2(
  contagens_tvtp,
  file = file.path(pasta_resultados, "50_contagens_tvtp_pos_real.csv")
)

write.csv2(
  diagnostico_tvtp,
  file = file.path(pasta_resultados, "51_diagnostico_tvtp.csv"),
  row.names = FALSE
)

write.csv2(
  coeficientes_tvtp_tabela,
  file = file.path(pasta_resultados, "18_coeficientes_tvtp_logit_multinomial.csv"),
  row.names = FALSE
)

write.csv2(
  teste_tvtp,
  file = file.path(pasta_resultados, "19_teste_lr_tvtp.csv"),
  row.names = FALSE
)

write.csv2(
  probabilidades_medias_tvtp,
  file = file.path(pasta_resultados, "20_probabilidades_medias_tvtp.csv"),
  row.names = FALSE
)

write.csv2(
  probabilidades_tvtp_tabela,
  file = file.path(pasta_resultados, "21_probabilidades_previstas_tvtp.csv"),
  row.names = FALSE
)

# Graficos em formato academico.
#
# Em artigos cientificos, e comum usar graficos vetoriais em PDF,
# com fontes padronizadas, fundo branco, tons de cinza e poucos elementos
# decorativos. Isso facilita a impressao, melhora a nitidez e deixa as
# figuras apropriadas para relatorios, monografias e artigos.

graphics.off()

configurar_grafico <- function() {
  par(
    mfrow = c(1, 1),
    family = "serif",
    mar = c(4.2, 4.6, 2.2, 1.2),
    oma = c(0, 0, 0, 0),
    las = 1,
    bty = "l",
    cex = 0.95,
    cex.axis = 0.9,
    cex.lab = 0.95,
    cex.main = 1.0
  )
}

abrir_pdf <- function(nome_arquivo, largura = 7.2, altura = 4.8) {
  pdf(
    file = file.path(pasta_resultados, nome_arquivo),
    width = largura,
    height = altura,
    family = "serif",
    paper = "special",
    useDingbats = FALSE
  )
}

tons_regime <- c(
  "Baixa" = "gray25",
  "Moderada" = "gray55",
  "Alta" = "gray5"
)

simbolos_regime <- c(
  "Baixa" = 1,
  "Moderada" = 16,
  "Alta" = 17
)

# Grafico 1: serie historica do IPCA e classificacao dos regimes.

abrir_pdf("grafico_01_ipca_regimes.pdf", largura = 7.5, altura = 4.8)

configurar_grafico()

plot(
  ipca$data,
  ipca$ipca_mensal,
  type = "l",
  col = "gray35",
  lwd = 0.9,
  xlab = "Ano",
  ylab = "IPCA mensal (%)",
  main = ""
)

grid(nx = NA, ny = NULL, col = "gray90", lty = "dotted")

for (estado in estados) {
  dados_estado <- ipca[ipca$regime == estado, ]

  points(
    dados_estado$data,
    dados_estado$ipca_mensal,
    pch = simbolos_regime[estado],
    col = tons_regime[estado],
    cex = 0.65
  )
}

abline(h = limiar_baixa_moderada, lty = 2, col = "gray25", lwd = 0.8)
abline(h = limiar_moderada_alta, lty = 3, col = "gray25", lwd = 0.8)

legend(
  "topright",
  legend = estados,
  col = tons_regime[estados],
  pch = simbolos_regime[estados],
  bty = "n",
  cex = 0.85
)

dev.off()

# Grafico 2: frequencia dos regimes.

abrir_pdf("grafico_02_frequencia_regimes.pdf", largura = 6.5, altura = 4.6)

configurar_grafico()

bp_frequencia <- barplot(
  frequencia_regimes[estados],
  col = c("gray85", "gray60", "gray35"),
  border = "gray20",
  main = "",
  xlab = "Regime",
  ylab = "Numero de meses",
  ylim = c(0, max(frequencia_regimes[estados]) * 1.18)
)

text(
  x = bp_frequencia,
  y = frequencia_regimes[estados],
  labels = as.numeric(frequencia_regimes[estados]),
  pos = 3,
  cex = 0.85
)

dev.off()

# Grafico 3: probabilidade de permanencia em cada regime.

abrir_pdf("grafico_03_persistencia_regimes.pdf", largura = 6.5, altura = 4.6)

configurar_grafico()

bp_persistencia <- barplot(
  persistencia[estados],
  ylim = c(0, 1),
  col = c("gray85", "gray60", "gray35"),
  border = "gray20",
  main = "",
  xlab = "Regime",
  ylab = "Probabilidade de permanencia"
)

abline(h = seq(0, 1, by = 0.2), col = "gray85", lty = "dotted")

text(
  x = bp_persistencia,
  y = persistencia[estados],
  labels = paste0(round(100 * persistencia[estados], 1), "%"),
  pos = 3,
  cex = 0.85
)

dev.off()

# Grafico 4: matriz de transicao em escala de cinza.

abrir_pdf("grafico_04_matriz_transicao.pdf", largura = 6.2, altura = 5.4)

configurar_grafico()

plot(
  NA,
  xlim = c(0.5, length(estados) + 0.5),
  ylim = c(0.5, length(estados) + 0.5),
  xaxt = "n",
  yaxt = "n",
  xlab = "Destino",
  ylab = "Origem",
  main = ""
)

axis(1, at = seq_along(estados), labels = estados)

for (i in seq_along(estados)) {
  for (j in seq_along(estados)) {
    valor <- matriz_transicao[i, j]
    intensidade <- ifelse(is.na(valor), 0, valor)
    cor <- gray(0.95 - 0.55 * intensidade)

    rect(
      xleft = j - 0.5,
      ybottom = length(estados) - i + 0.5,
      xright = j + 0.5,
      ytop = length(estados) - i + 1.5,
      col = cor,
      border = "gray95"
    )

    text(
      x = j,
      y = length(estados) - i + 1,
      labels = ifelse(is.na(valor), "NA", paste0(round(100 * valor, 1), "%")),
      cex = 0.95
    )
  }
}

axis(2, at = rev(seq_along(estados)), labels = estados, las = 1)

dev.off()

# Grafico 5: intervalos de confianca bootstrap para a persistencia.

ic_persistencia_boot <- ic_transicoes_bootstrap[
  ic_transicoes_bootstrap$Origem == ic_transicoes_bootstrap$Destino,
]

ic_persistencia_boot <- ic_persistencia_boot[
  match(estados, ic_persistencia_boot$Origem),
]

abrir_pdf("grafico_05_ic_persistencia_bootstrap.pdf", largura = 6.5, altura = 4.6)

configurar_grafico()

plot(
  seq_along(estados),
  ic_persistencia_boot$Probabilidade_MLE,
  ylim = c(0, 1),
  xaxt = "n",
  xlab = "Regime",
  ylab = "Probabilidade de permanencia",
  main = "",
  pch = 16,
  col = "gray10"
)

axis(1, at = seq_along(estados), labels = estados)
abline(h = seq(0, 1, by = 0.2), col = "gray85", lty = "dotted")

arrows(
  x0 = seq_along(estados),
  y0 = ic_persistencia_boot$IC_boot_inferior,
  x1 = seq_along(estados),
  y1 = ic_persistencia_boot$IC_boot_superior,
  angle = 90,
  code = 3,
  length = 0.05,
  col = "gray20"
)

points(
  seq_along(estados),
  ic_persistencia_boot$Probabilidade_MLE,
  pch = 16,
  col = "gray10"
)

dev.off()

# Grafico 6: comparacao da persistencia entre abordagens.

persistencia_tvtp_media <- sapply(
  estados,
  function(estado) {
    linha <- probabilidades_medias_tvtp[probabilidades_medias_tvtp$Origem == estado, ]

    if (nrow(linha) == 0 || !(estado %in% colnames(probabilidades_medias_tvtp))) {
      return(NA_real_)
    }

    return(as.numeric(linha[[estado]]))
  }
)

comparacao_persistencia <- rbind(
  "Tercis" = persistencia[estados],
  "K-means" = persistencia_kmeans[estados],
  "HMM latente" = persistencia_hmm[estados],
  "TVTP medio" = persistencia_tvtp_media
)

if (exists("persistencia_kmeans_log")) {
  comparacao_persistencia <- rbind(
    comparacao_persistencia,
    "K-means log1p" = persistencia_kmeans_log[estados]
  )
}

if (exists("persistencia_hmm_log")) {
  comparacao_persistencia <- rbind(
    comparacao_persistencia,
    "HMM log1p" = persistencia_hmm_log[estados]
  )
}

colnames(comparacao_persistencia) <- estados

abrir_pdf("grafico_06_comparacao_persistencia_modelos.pdf", largura = 8.4, altura = 4.8)

configurar_grafico()

bp_comparacao <- barplot(
  t(comparacao_persistencia),
  beside = TRUE,
  ylim = c(0, 1),
  col = c("gray85", "gray60", "gray35"),
  border = "gray20",
  names.arg = rownames(comparacao_persistencia),
  ylab = "Probabilidade de permanencia",
  main = "",
  legend.text = estados,
  args.legend = list(bty = "n", x = "topright", cex = 0.8)
)

abline(h = seq(0, 1, by = 0.2), col = "gray85", lty = "dotted")

dev.off()

# Grafico 7: probabilidades suavizadas do modelo HMM.

abrir_pdf("grafico_07_probabilidades_suavizadas_hmm.pdf", largura = 7.5, altura = 4.8)

configurar_grafico()

matplot(
  ipca$data,
  probabilidades_hmm[, estados],
  type = "l",
  lty = c(1, 2, 3),
  lwd = c(1.2, 1.2, 1.2),
  col = c("gray10", "gray45", "gray70"),
  ylim = c(0, 1),
  xlab = "Ano",
  ylab = "Probabilidade suavizada",
  main = ""
)

legend(
  "topright",
  legend = estados,
  lty = c(1, 2, 3),
  col = c("gray10", "gray45", "gray70"),
  bty = "n",
  cex = 0.85
)

dev.off()

# Grafico 7b: probabilidades suavizadas do HMM em escala log1p.

if (exists("probabilidades_hmm_log")) {
  abrir_pdf("grafico_07b_probabilidades_suavizadas_hmm_log1p.pdf", largura = 7.5, altura = 4.8)

  configurar_grafico()

  matplot(
    ipca$data,
    probabilidades_hmm_log[, estados],
    type = "l",
    lty = c(1, 2, 3),
    lwd = c(1.2, 1.2, 1.2),
    col = c("gray10", "gray45", "gray70"),
    ylim = c(0, 1),
    xlab = "Ano",
    ylab = "Probabilidade suavizada",
    main = ""
  )

  legend(
    "topright",
    legend = estados,
    lty = c(1, 2, 3),
    col = c("gray10", "gray45", "gray70"),
    bty = "n",
    cex = 0.85
  )

  dev.off()
}

# Grafico 8: probabilidade TVTP prevista para destino em inflacao alta.

if ("Alta" %in% colnames(probabilidades_tvtp_tabela)) {
  abrir_pdf("grafico_08_tvtp_probabilidade_alta.pdf", largura = 7.5, altura = 4.8)

  configurar_grafico()

  prob_alta_media_movel <- as.numeric(stats::filter(
    probabilidades_tvtp_tabela$Alta,
    rep(1 / 12, 12),
    sides = 2
  ))

  plot(
    probabilidades_tvtp_tabela$data_origem,
    probabilidades_tvtp_tabela$Alta,
    type = "l",
    col = "gray15",
    lwd = 1,
    ylim = c(0, 1),
    xlab = "Ano",
    ylab = "Probabilidade prevista",
    main = ""
  )

  grid(nx = NA, ny = NULL, col = "gray90", lty = "dotted")

  lines(
    probabilidades_tvtp_tabela$data_origem,
    prob_alta_media_movel,
    col = "gray5",
    lwd = 2
  )

  pontos_alta_observada <- probabilidades_tvtp_tabela[
    probabilidades_tvtp_tabela$destino_observado == "Alta",
  ]

  points(
    pontos_alta_observada$data_origem,
    rep(0.035, nrow(pontos_alta_observada)),
    pch = 16,
    col = "gray55",
    cex = 0.35
  )

  legend(
    "topright",
    legend = c("Prevista mensal", "Media movel 12 meses", "Destino observado Alta"),
    col = c("gray15", "gray5", "gray30"),
    lty = c(1, 1, NA),
    lwd = c(1, 2, NA),
    pch = c(NA, NA, 16),
    bty = "n",
    cex = 0.78
  )

  dev.off()
}

# Grafico 9: IPCA em escala log1p para visualizar 1980-presente.
#
# A classificacao principal continua sendo feita por tercis em escala original.
# O log1p e usado aqui apenas para visualizacao, pois a amostra desde 1980
# inclui valores muito altos de inflacao mensal no periodo pre-Real.

if (all(ipca$ipca_mensal > -1, na.rm = TRUE)) {
  abrir_pdf("grafico_09_ipca_log1p_regimes.pdf", largura = 7.5, altura = 4.8)

  configurar_grafico()

  plot(
    ipca$data,
    log1p(ipca$ipca_mensal),
    type = "l",
    col = "gray35",
    lwd = 0.9,
    xlab = "Ano",
    ylab = "log(1 + IPCA mensal)",
    main = ""
  )

  grid(nx = NA, ny = NULL, col = "gray90", lty = "dotted")

  for (estado in estados) {
    dados_estado <- ipca[ipca$regime == estado, ]

    points(
      dados_estado$data,
      log1p(dados_estado$ipca_mensal),
      pch = simbolos_regime[estado],
      col = tons_regime[estado],
      cex = 0.65
    )
  }

  abline(h = log1p(limiar_baixa_moderada), lty = 2, col = "gray25", lwd = 0.8)
  abline(h = log1p(limiar_moderada_alta), lty = 3, col = "gray25", lwd = 0.8)

  legend(
    "topright",
    legend = estados,
    col = tons_regime[estados],
    pch = simbolos_regime[estados],
    bty = "n",
    cex = 0.85
  )

  dev.off()
}

# Grafico 10: persistencia comparada entre pre-Real e pos-Real.

persistencia_pre_pos_real <- rbind(
  "Pre-Real" = diag(analise_pre_real$matriz),
  "Pos-Real" = diag(analise_pos_real$matriz)
)

colnames(persistencia_pre_pos_real) <- estados

abrir_pdf("grafico_10_persistencia_pre_pos_real.pdf", largura = 7.2, altura = 4.8)

configurar_grafico()

barplot(
  t(persistencia_pre_pos_real),
  beside = TRUE,
  ylim = c(0, 1),
  col = c("gray85", "gray60", "gray35"),
  border = "gray20",
  names.arg = rownames(persistencia_pre_pos_real),
  ylab = "Probabilidade de permanencia",
  main = "",
  legend.text = estados,
  args.legend = list(bty = "n", x = "topright", cex = 0.8)
)

abline(h = seq(0, 1, by = 0.2), col = "gray85", lty = "dotted")

dev.off()

# Grafico 11: matrizes pre-Real e pos-Real lado a lado.

desenhar_matriz_transicao <- function(matriz, titulo, estados) {
  plot(
    NA,
    xlim = c(0.5, length(estados) + 0.5),
    ylim = c(0.5, length(estados) + 0.5),
    xaxt = "n",
    yaxt = "n",
    xlab = "Destino",
    ylab = "Origem",
    main = titulo
  )

  axis(1, at = seq_along(estados), labels = estados)

  for (i in seq_along(estados)) {
    for (j in seq_along(estados)) {
      valor <- matriz[i, j]
      intensidade <- ifelse(is.na(valor), 0, valor)
      cor <- gray(0.95 - 0.55 * intensidade)

      rect(
        xleft = j - 0.5,
        ybottom = length(estados) - i + 0.5,
        xright = j + 0.5,
        ytop = length(estados) - i + 1.5,
        col = cor,
        border = "gray95"
      )

      text(
        x = j,
        y = length(estados) - i + 1,
        labels = ifelse(is.na(valor), "NA", paste0(round(100 * valor, 1), "%")),
        cex = 0.82
      )
    }
  }

  axis(2, at = rev(seq_along(estados)), labels = estados, las = 1)
}

abrir_pdf("grafico_11_matrizes_pre_pos_real.pdf", largura = 10, altura = 4.8)

par(
  mfrow = c(1, 2),
  family = "serif",
  mar = c(4.2, 4.6, 2.2, 1.2),
  oma = c(0, 0, 0, 0),
  las = 1,
  bty = "l",
  cex = 0.85,
  cex.axis = 0.8,
  cex.lab = 0.85,
  cex.main = 0.95
)

desenhar_matriz_transicao(analise_pre_real$matriz, "Pre-Real", estados)
desenhar_matriz_transicao(analise_pos_real$matriz, "Pos-Real", estados)

dev.off()

# ------------------------------------------------
# 15. Conclusao automatica para apoiar a interpretacao
# ------------------------------------------------

regime_mais_persistente <- names(which.max(persistencia))
prob_mais_persistente <- max(persistencia, na.rm = TRUE)

regime_mais_persistente_kmeans <- names(which.max(persistencia_kmeans))
prob_mais_persistente_kmeans <- max(persistencia_kmeans, na.rm = TRUE)

regime_mais_persistente_hmm <- names(which.max(persistencia_hmm))
prob_mais_persistente_hmm <- max(persistencia_hmm, na.rm = TRUE)

if (exists("persistencia_kmeans_log")) {
  regime_mais_persistente_kmeans_log <- names(which.max(persistencia_kmeans_log))
  prob_mais_persistente_kmeans_log <- max(persistencia_kmeans_log, na.rm = TRUE)
}

if (exists("persistencia_hmm_log")) {
  regime_mais_persistente_hmm_log <- names(which.max(persistencia_hmm_log))
  prob_mais_persistente_hmm_log <- max(persistencia_hmm_log, na.rm = TRUE)
}

persistencia_pre_real <- diag(analise_pre_real$matriz)
persistencia_pos_real <- diag(analise_pos_real$matriz)

regime_mais_persistente_pre_real <- names(which.max(persistencia_pre_real))
prob_mais_persistente_pre_real <- max(persistencia_pre_real, na.rm = TRUE)

regime_mais_persistente_pos_real <- names(which.max(persistencia_pos_real))
prob_mais_persistente_pos_real <- max(persistencia_pos_real, na.rm = TRUE)

cat("\n============================================================\n")
cat("CONCLUSAO SUGERIDA\n")
cat("============================================================\n")
cat(
  "O resultado principal deve ser lido a partir da separacao pre-Real e pos-Real,\n",
  "pois os dois periodos pertencem a ambientes monetarios distintos.\n",
  sep = ""
)
cat(
  "Na analise com tercis proprios por subperiodo, o regime mais persistente no pre-Real foi:",
  regime_mais_persistente_pre_real,
  "com probabilidade de permanencia de",
  round(100 * prob_mais_persistente_pre_real, 2),
  "%.\n"
)
cat(
  "No pos-Real, com tercis recalculados dentro do periodo estabilizado, o regime mais persistente foi:",
  regime_mais_persistente_pos_real,
  "com probabilidade de permanencia de",
  round(100 * prob_mais_persistente_pos_real, 2),
  "%.\n"
)
cat(
  "Essas categorias sao relativas a cada subperiodo: Alta no pre-Real nao representa o mesmo nivel absoluto\n",
  "de inflacao que Alta no pos-Real, mas o terco superior dentro de cada ambiente historico.\n",
  sep = ""
)
cat(
  "Na amostra completa, mantida apenas como referencia historica agregada, o regime mais persistente foi:",
  regime_mais_persistente,
  "com probabilidade de permanencia de",
  round(100 * prob_mais_persistente, 2),
  "%.\n"
)
cat(
  "As abordagens k-means, HMM e TVTP devem ser interpretadas como extensoes e verificacoes de robustez.\n"
)
cat(
  "O teste LR contra independencia temporal apresentou p-valor assintotico de ",
  signif(teste_dependencia_temporal$Valor_p, 4),
  " e p-valor Monte Carlo de ",
  signif(teste_dependencia_temporal_monte_carlo$Valor_p_Monte_Carlo, 4),
  ", indicando evidencia estatistica de dependencia temporal dos regimes.\n",
  sep = ""
)
cat(
  "O teste LR de primeira versus segunda ordem apresentou p-valor assintotico de ",
  signif(teste_ordem_markov_resumo$Valor_p, 4),
  " e p-valor Monte Carlo de ",
  signif(teste_ordem_markov_monte_carlo$Valor_p_Monte_Carlo, 4),
  ". A diferenca entre os dois resultados recomenda cautela, pois alguns contextos de segunda ordem sao pouco frequentes.\n",
  sep = ""
)
cat(
  "O teste LR pre-Real versus pos-Real, usando os regimes globais para comparabilidade formal, apresentou p-valor assintotico de ",
  signif(teste_estabilidade_pre_pos_real_global$Valor_p, 4),
  " e p-valor Monte Carlo de ",
  signif(teste_estabilidade_pre_pos_real_monte_carlo$Valor_p_Monte_Carlo, 4),
  ".\n",
  sep = ""
)
cat(
  "O teste LR de estabilidade temporal entre pre-pandemia, pandemia e pos-pandemia apresentou p-valor assintotico de ",
  signif(teste_estabilidade$Valor_p, 4),
  " e p-valor Monte Carlo de ",
  signif(teste_estabilidade_monte_carlo$Valor_p_Monte_Carlo, 4),
  ". Assim, a evidencia formal de quebra entre os periodos depende do nivel de significancia adotado.\n",
  sep = ""
)
cat(
  "A extensao TVTP pos-Real por logit multinomial, comparada a um modelo com origem apenas, apresentou p-valor de ",
  signif(teste_tvtp$Valor_p, 4),
  ", sugerindo que variaveis de estado ajudam a explicar as probabilidades de transicao.\n",
  sep = ""
)
if (exists("regime_mais_persistente_kmeans_log")) {
  cat(
    "Na robustez por k-means em log1p, o regime mais persistente foi:",
    regime_mais_persistente_kmeans_log,
    "com probabilidade de permanencia de",
    round(100 * prob_mais_persistente_kmeans_log, 2),
    "%.\n"
  )
}
if (exists("regime_mais_persistente_hmm_log")) {
  cat(
    "Na robustez por HMM em log1p, o regime mais persistente foi:",
    regime_mais_persistente_hmm_log,
    "com probabilidade de permanencia de",
    round(100 * prob_mais_persistente_hmm_log, 2),
    "%.\n"
  )
}
cat(
  "Probabilidades estimadas como zero devem ser lidas como transicoes nao observadas na amostra,\n",
  "e nao como impossibilidades teoricas.\n",
  sep = ""
)
cat(
  "A distribuicao estacionaria mostra a participacao esperada de cada regime no longo prazo,\n",
  "caso o padrao historico de transicoes permanecesse constante.\n",
  sep = ""
)

cat("\nArquivos salvos em:\n")
cat(pasta_resultados, "\n")

# ================================================================
# Fim do script
# ================================================================
