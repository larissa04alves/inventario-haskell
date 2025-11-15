# Inventário Minecraft — Haskell

Instituição: `PUCPR - Curitiba`

Disciplina: `Programação Lógica e Funcional`

Professor: `Frank Coelho de Alcantara`

Aluno(a): [`Larissa Alves da Silva`](https://github.com/larissa04alves)

## Objetivos do trabalho

- Receber comandos no terminal para adicionar, remover, atualizar e listar itens.

- Manter a lógica de negócio 100% pura em módulo separado do I/O.

- Persistir o inventário em Inventario.dat (sobrescrito apenas em sucesso).

- Auditar toda tentativa (sucesso e falha) em Auditoria.log (append-only).

- Carregar inventário e auditoria ao iniciar (com tratamento de erro).

- Implementar report que chama funções puras de análise e exibe o resultado.

## Funcionalidades

- Adicionar item (perguntas interativas: ID, nome, quantidade, categoria).

- Remover quantidade de um item (remove a chave se zerar).

- Atualizar a quantidade absoluta de um item.

- Listar itens ordenados por ItemID.

- Reportar estatísticas de auditoria: totais, sucessos, falhas e frequência por ação.

- Persistência: Inventario.dat (estado atual) e Auditoria.log (todas as ações).

## Como Executar

### Requisitos

- GHC (inclui `ghc`, `ghci`, `runghc`).

### Executar utilizando scripts

```
# Popular inventário
runghc -isrc Main.hs < src/scripts/popular.txt

# Checar persistência (deve listar itens e sair)
runghc -isrc Main.hs < src/scripts/cenario1_persistencia.txt

# Testar falha + sucesso (remoção maior que disponível, depois remoção válida)
runghc -isrc Main.hs < src/scripts/cenario2_erro.txt
```

### Executar interativamente

```
runghc -isrc Main.hs
```

## Comandos Disponíveis (no prompt)

`adicionar` — guia interativo pergunta ItemID, Nome, Quantidade, Categoria.

`remover` — lista itens, pergunta ItemID e Quantidade.

`update` — lista itens, pergunta ItemID e nova Quantidade.

`listar` — lista todos os itens do baú.

`report` — imprime estatísticas de auditoria.

`ajuda` — mostra os comandos.

`sair` — encerra o programa.

# Persistência e Auditoria

`Inventario.dat`: salvo com show, lido com read/reads.
É sobrescrito apenas quando a operação tem sucesso.

`Auditoria.log`: append-only (uma linha por LogEntry, via show).
Todas as operações (sucesso/falha) geram registro com timestamp.

### Execução Online

- Replit: o projeto já está configurado com Cabal.

  Link do ambiente: https://replit.com/join/tclykvfoxj-alves04larissa

Clique em Run para compilar/executar.

Para rodar scripts, abra o Shell do Replit e use:

```
cabal run -- < src/scripts/popular.txt
cabal run -- < src/scripts/cenario1_persistencia.txt
cabal run -- < src/scripts/cenario2_erro.txt
```

## Cenários de teste (manuais) — resultados documentados

### Cenário 1 — Persistência básica

Entrada: `src/scripts/cenario1_persistencia.txt`
Esperado:

O sistema `inicia`, `carrega` (ou cria) Inventario.dat.

Executa `listar`

`sair`
Resultado observado: itens listados conforme popularização; Inventario.dat permanece com os dados.

### Cenário 2 — Erro + Sucesso na remoção

Entrada: `src/scripts/cenario2_erro.txt`
Esperado:

`listar` exibe itens.

`remover` tenta remover mais do que disponível → `falha` (mensagem de quantidade insuficiente) e auditoria registrada.

`remover` com quantidade válida → `sucesso` (estoque atualizado e/ou item removido se zerar).

`sair`
Resultado observado:

`Mensagem`: Erro ao remover item: Quantidade insuficiente...

`Depois`: Item removido com sucesso: OURO e listar mostra OURO com quantidade reduzida.

A execução do report mostra totais, sucessos/falhas e frequência de ações com base em Auditoria.log.
