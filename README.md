# Persistência dos regimes inflacionários no Brasil

Este repositório contém o código em R usado na aplicação empírica do trabalho de Estatística Aplicada sobre Cadeias de Markov e regimes inflacionários do IPCA no Brasil.

## Objetivo

Estimar e analisar uma cadeia de Markov de tempo discreto para regimes mensais de inflação no Brasil, classificados em:

* Baixa inflação;
* Inflação moderada;
* Alta inflação.

A aplicação usa dados mensais do IPCA e compara a persistência dos regimes na amostra completa, no período pré-Real e no período pós-Real.

## Dados

* Série principal: IPCA mensal, SGS 433, Banco Central do Brasil.
* Série auxiliar: Meta Selic, SGS 432, Banco Central do Brasil.
* Período usado no relatório: janeiro de 1980 a abril de 2026.

As séries são baixadas automaticamente das APIs públicas do Banco Central pelo script R.

## Arquivo principal

* `markov_ipca_regimes.R`: baixa os dados, classifica os regimes, estima as matrizes de transição, calcula diagnósticos, testes estatísticos, extensões exploratórias e salva tabelas/figuras.

## Como reproduzir

1. Instale o R.
2. Abra a pasta do repositório no RStudio.
3. Execute:

```r
source("markov_ipca_regimes.R")

```

O script instala automaticamente os pacotes necessários em uma biblioteca local chamada `biblioteca_R/`, que não é versionada no GitHub.

## Pacotes usados

* `markovchain`
* `nnet`

## Saídas

As tabelas e figuras são salvas automaticamente na pasta:

```text
resultados_markov_ipca/
```

Entre os principais resultados estão:

* matriz de transição dos regimes;
* persistência e duração esperada;
* distribuição estacionária;
* diagnósticos formais da cadeia;
* intervalos bootstrap;
* testes LR e p-valores de Monte Carlo;
* comparação pré-Real e pós-Real;
* extensões exploratórias com k-means, HMM e TVTP.

## Observação de reprodutibilidade

A data final da amostra está fixada em `30/04/2026` no script para reproduzir exatamente os resultados usados no relatório. Para atualizar o estudo com dados mais recentes, altere manualmente o objeto `data_final` no início do script.
