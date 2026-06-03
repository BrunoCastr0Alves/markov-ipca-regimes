# Selecao de tabelas e figuras para a aplicacao empirica

Objetivo editorial: manter o corpo do relatorio com padrao de pos-graduacao, evitando excesso de saidas computacionais. A aplicacao deve privilegiar a matriz principal, a inferencia, a comparacao pre-Real/pos-Real e uma extensao TVTP bem documentada. Robustez por k-means e HMM pode aparecer de forma resumida no texto e, se necessario, em apendice.

## Tabelas recomendadas no corpo do relatorio

| Nova tabela | Arquivo/base | Decisao | Justificativa |
|---|---|---|---|
| Tabela 1 - Limiares globais por tercis | `00_limiares_regimes_tercis.csv` | Manter | Define a operacionalizacao dos estados na amostra completa. |
| Tabela 2 - Matriz de transicao com contagens | `02_matriz_contagens.csv` + `03_matriz_transicao.csv` | Manter, combinando probabilidade e contagem | Responde a critica sobre falta de contagens e torna a matriz mais transparente. |
| Tabela 3 - Persistencia e duracao esperada | `04_resumo_persistencia.csv` | Manter | Resultado substantivo central da matriz agregada, com ressalva historica. |
| Tabela 4 - Diagnosticos formais da matriz | `37_diagnosticos_matriz_markov.csv` | Manter | Conecta teoria e aplicacao: irreducibilidade, aperiodicidade, gap espectral e tempo de mistura. |
| Tabela 5 - Bootstrap da persistencia | `08_ic_transicoes_bootstrap.csv` | Manter apenas diagonal | Mostra incerteza dos principais parametros sem sobrecarregar com todas as celulas. |
| Tabela 6 - Testes LR e Monte Carlo | `09`, `09b`, `43`, `44`, `45`, `46` | Manter como tabela sintese | Substitui varias tabelas soltas e melhora o rigor inferencial. |
| Tabela 7 - Limiares pre-Real e pos-Real | `22_limiares_pre_pos_real_tercis_proprios.csv` | Manter | Fundamental para explicar que os estados sao relativos dentro de cada periodo. |
| Tabela 8 - Persistencia pre-Real e pos-Real | `23_resumo_pre_pos_real_tercis_proprios.csv` | Manter | Principal resultado economico do trabalho. |
| Tabela 9 - Coeficientes do TVTP pos-Real | `18_coeficientes_tvtp_logit_multinomial.csv` | Manter no corpo, em fonte pequena | Corrige a critica de que probabilidades medias nao substituem a apresentacao do modelo estimado. |
| Tabela 10 - Diagnostico do TVTP | `51_diagnostico_tvtp.csv` | Manter no corpo | Informa convergencia, numero de parametros, AIC/BIC e criterio de quase-separacao. |
| Tabela 11 - TVTP pos-Real: probabilidades medias previstas | `20_probabilidades_medias_tvtp.csv` | Manter, depois dos coeficientes | Deve ser apresentada como media de probabilidades previstas, nao como matriz fixa. |

## Tabelas recomendadas para apendice

| Arquivo | Decisao | Justificativa |
|---|---|---|
| `01_ipca_classificado_regimes.csv` | Apendice/dados reprodutiveis | Muito grande para o corpo do texto. |
| `07_ic_transicoes_assintotico.csv` | Apendice | Util para transparencia, mas o corpo deve priorizar bootstrap suavizado. |
| `09c_contagens_segunda_ordem.csv` | Apendice | Importante para justificar cautela no teste de ordem. |
| `10_teste_lr_estabilidade_temporal.csv` | Apendice ou tabela sintese | No corpo, use apenas o resultado resumido. |
| `11` a `13` k-means em nivel | Apendice | Resultado auxiliar e sensivel a escala. |
| `30` a `32` k-means log1p | Apendice | Robustez util, mas nao deve competir com a analise principal. |
| `14` a `17` HMM em nivel | Apendice ou excluir figura em nivel | O HMM em nivel captura fortemente a quebra historica e pode ser interpretado em excesso. |
| `33` a `36` HMM log1p | Apendice | Melhor versao do HMM para robustez. |
| `21_probabilidades_previstas_tvtp.csv` | Apendice/dados reprodutiveis | Serie completa prevista, grande demais para corpo. |
| `38` a `40` diagnosticos detalhados | Apendice | Periodos, autovalores e matriz de alcancabilidade complementam a Tabela 4. |
| `41` e `42` contagens/suavizacao bootstrap | Apendice | Importante para reprodutibilidade. |
| `47` a `50` especificacoes HMM/TVTP e contagens TVTP | Apendice metodologico | Fortalece reproducibilidade sem carregar o texto principal. |

## Figuras recomendadas no corpo do relatorio

| Nova figura | Arquivo | Decisao | Justificativa |
|---|---|---|---|
| Figura 1 - IPCA em log1p e regimes | `grafico_09_ipca_log1p_regimes.pdf` | Manter e usar no lugar do grafico bruto | Para 1980-presente, a escala logaritmica comunica melhor a serie sem achatar o pos-Real. |
| Figura 2 - IC bootstrap da persistencia | `grafico_05_ic_persistencia_bootstrap.pdf` | Manter | Visualiza a incerteza dos parametros mais importantes. O codigo foi corrigido para plotar `Probabilidade_MLE`. |
| Figura 3 - Persistencia pre-Real e pos-Real | `grafico_10_persistencia_pre_pos_real.pdf` | Manter | Resume o principal resultado economico. |
| Figura 4 - Matrizes pre-Real e pos-Real | `grafico_11_matrizes_pre_pos_real.pdf` | Manter | Mostra a mudanca estrutural na matriz de transicao. |
| Figura 5 - TVTP probabilidade de destino Alta | `grafico_08_tvtp_probabilidade_alta.pdf` | Manter como extensao | Agora tem amostra pos-Real, covariaveis e media movel; e defensavel no corpo se a secao TVTP permanecer. |

## Figuras para apendice ou exclusao

| Arquivo | Decisao | Justificativa |
|---|---|---|
| `grafico_01_ipca_regimes.pdf` | Substituir por `grafico_09` | A escala bruta fica pouco informativa para o pos-Real. |
| `grafico_02_frequencia_regimes.pdf` | Excluir do corpo | Frequencias sao implicitas pelos tercis e podem ser descritas em texto. |
| `grafico_03_persistencia_regimes.pdf` | Excluir do corpo | Redundante com Tabela 3 e Figura 2/IC bootstrap. |
| `grafico_04_matriz_transicao.pdf` | Opcional/apendice | A matriz com contagens na tabela e mais cientifica; a figura pode ficar no apendice se houver espaco. |
| `grafico_06_comparacao_persistencia_modelos.pdf` | Apendice ou excluir | Mistura modelos principais e auxiliares; pode confundir a hierarquia dos resultados. |
| `grafico_07_probabilidades_suavizadas_hmm.pdf` | Excluir do corpo | HMM em nivel e dominado pela escala extrema pre-Real. |
| `grafico_07b_probabilidades_suavizadas_hmm_log1p.pdf` | Apendice | Boa robustez, mas nao precisa competir com o resultado principal pre/pos-Real. |

## Hierarquia recomendada da aplicacao

1. Classificacao por tercis na amostra completa.
2. Matriz de transicao agregada, persistencia e diagnosticos formais.
3. Inferencia: bootstrap suavizado, LR e Monte Carlo.
4. Resultado principal: comparacao pre-Real vs pos-Real com tercis proprios.
5. Extensoes exploratorias: k-means e HMM como apendice; TVTP pos-Real no corpo com coeficientes, diagnostico e probabilidades previstas.
6. Limitacoes: tercis relativos, quebra estrutural, zeros amostrais, ausencia de validacao fora da amostra e carater nao causal do TVTP.

## Ajuste de integracao teoria-aplicacao

Para evitar a critica de que a parte teorica e mais ampla do que a aplicacao, o corpo do relatorio deve explicitar a seguinte hierarquia:

| Conceito | Tratamento recomendado |
|---|---|
| Matriz de transicao, classificacao dos estados, distribuicao estacionaria, periodicidade, autovalores, gap espectral e tempo de mistura | Manter e aplicar diretamente aos dados do IPCA. |
| Inferencia por verossimilhanca, bootstrap, LR e Monte Carlo | Manter obrigatoriamente no corpo da aplicacao. |
| Acoplamento, tempo continuo e dependencia fraca | Manter apenas como extensoes conceituais ou reduzir na teoria se o relatorio precisar ficar mais enxuto. Nao apresentar como resultado empirico. |

Se o PDF final nao mostrar distribuicao estacionaria, diagnosticos formais, intervalos bootstrap e tabela LR/Monte Carlo, entao o problema nao e mais de conteudo: e de integracao/recompilacao da versao correta da secao `secao_7_aplicacao_atualizada.tex`.
