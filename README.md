# Persistencia dos regimes inflacionarios no Brasil

Este repositorio contem o codigo em R usado na aplicacao empirica do trabalho de Estatistica Aplicada sobre Cadeias de Markov e regimes inflacionarios do IPCA no Brasil.

## Objetivo

Estimar e analisar uma cadeia de Markov de tempo discreto para regimes mensais de inflacao no Brasil, classificados em:

- Baixa inflacao;
- Inflacao moderada;
- Alta inflacao.

A aplicacao usa dados mensais do IPCA e compara a persistencia dos regimes na amostra completa, no periodo pre-Real e no periodo pos-Real.

## Dados

- Serie principal: IPCA mensal, SGS 433, Banco Central do Brasil.
- Serie auxiliar: Meta Selic, SGS 432, Banco Central do Brasil.
- Periodo usado no relatorio: janeiro de 1980 a abril de 2026.

As series sao baixadas automaticamente das APIs publicas do Banco Central pelo script R.

## Arquivo principal

- `markov_ipca_regimes.R`: baixa os dados, classifica os regimes, estima as matrizes de transicao, calcula diagnosticos, testes estatisticos, extensoes exploratorias e salva tabelas/figuras.

## Como reproduzir

1. Instale o R.
2. Abra a pasta do repositorio no RStudio.
3. Execute:

```r
source("markov_ipca_regimes.R")
```

O script instala automaticamente os pacotes necessarios em uma biblioteca local chamada `biblioteca_R/`, que nao e versionada no GitHub.

## Pacotes usados

- `markovchain`
- `nnet`

## Saidas

As tabelas e figuras sao salvas automaticamente na pasta:

```text
resultados_markov_ipca/
```

Entre os principais resultados estao:

- matriz de transicao dos regimes;
- persistencia e duracao esperada;
- distribuicao estacionaria;
- diagnosticos formais da cadeia;
- intervalos bootstrap;
- testes LR e p-valores de Monte Carlo;
- comparacao pre-Real e pos-Real;
- extensoes exploratorias com k-means, HMM e TVTP.

## Observacao de reprodutibilidade

A data final da amostra esta fixada em `30/04/2026` no script para reproduzir exatamente os resultados usados no relatorio. Para atualizar o estudo com dados mais recentes, altere manualmente o objeto `data_final` no inicio do script.
