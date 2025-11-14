-- Etapa 2: Funções puras de regra de negócio do baú
-- cada função recebe o estado atual (baú atual), os parametros das operações + UTCTime

-- Retorno é:
-- Sucesso: Right (baú atualizado, logEntry de sucesso)
-- Falha de lógica: Left msgErro (o módulo de I/O criará o LogEntry de falha)

module Core.Logic (
  adicionarItem,
  removerItem,
  atualizarQuantidade,
  listarItens
) where

import qualified Data.Map.Strict as Map
import Data.Time (UTCTime)

import Core.Data
  ( Item(..)
  , ItemID
  , Inventario
  , AcaoLog(..)
  , StatusLog(..)
  , LogEntry(..)
  , normalizarQuantidade
  )

-----------------------------------------------
-- Adicionar item ao baú
-- Adiciona um novo item ou incrementa a quantidade se já existir
-----------------------------------------------
-- Regras:
-- - Se não existe: cria com quantidade >= 0 (normalizada)
-- - Se existe: incrementa a quantidade (resultado sempre >= 0)
-- - Esta operação nunca falha

adicionarItem ::
  Inventario ->
  UTCTime -> -- timestamp
  ItemID ->
  String -> -- nome
  Int -> -- quantidade a adicionar
  String -> -- categoria
  Either String (Inventario, LogEntry)

adicionarItem inventarioAtual agora itemID nomeItem qtdEntrada categoriaItem =
  let quantidadeOk = normalizarQuantidade qtdEntrada
  in case Map.lookup itemID inventarioAtual of
        Nothing ->
          -- Item não existe: criar novo
          let novoItem = Item { itemID = itemID,
                                nome = nomeItem,
                                quantidade = quantidadeOk,
                                categoria = categoriaItem }
              inventarioNovo = Map.insert itemID novoItem inventarioAtual
              logEntry = LogEntry
                { timestamp = agora,
                  acao = Adicionar,
                  detalhes = "Novo item adicionado ao seu baú: " ++ nomeItem ++ " (quantidade: " ++ show quantidadeOk ++ ")",
                  status = Sucesso
                }
          in Right (inventarioNovo, logEntry)

        Just itemExistente ->
          -- Item existe: incrementar quantidade
          let novaQuantidade = normalizarQuantidade (quantidade itemExistente + quantidadeOk)
              itemAtualizado = itemExistente { quantidade = novaQuantidade }
              inventarioNovo = Map.insert itemID itemAtualizado inventarioAtual
              logEntry = LogEntry
                { timestamp = agora,
                  acao = Adicionar,
                  detalhes = "Item do seu baú atualizado " ++ nome itemExistente ++ " (nova quantidade: " ++ show novaQuantidade ++ ")",
                  status = Sucesso
                }
          in Right (inventarioNovo, logEntry)
  -- não há falhas possíveis nesta operação

------------------------------------------------
-- Remover item do baú
-- Remove uma quantidade do item, se zerar remove o item completamente
-----------------------------------------------
-- Regras de falha:
-- - Item não existe no baú
-- - Quantidade a remover maior que a disponível

removerItem ::
  Inventario ->
  UTCTime -> -- timestamp
  ItemID ->
  Int -> -- quantidade a remover
  Either String (Inventario, LogEntry)

removerItem inventarioAtual agora itemID qtdRemover =
  case Map.lookup itemID inventarioAtual of
    Nothing ->
      Left $ "Item '" ++ itemID ++ "' não existe no seu baú"

    Just itemExistente ->
      let quantidadeOk = normalizarQuantidade qtdRemover
          quantidadeAtual = quantidade itemExistente
      in if quantidadeOk > quantidadeAtual
         then Left $ "Quantidade insuficiente em seu baú. Disponível: " ++ show quantidadeAtual ++
                    ", tentando remover: " ++ show quantidadeOk
         else
            let novaQuantidade = quantidadeAtual - quantidadeOk
                inventarioNovo = if novaQuantidade == 0
                                  then Map.delete itemID inventarioAtual
                                  else Map.insert itemID (itemExistente { quantidade = novaQuantidade }) inventarioAtual
                logEntry = LogEntry
                  { timestamp = agora,
                    acao = Remover,
                    detalhes = "Removido do seu baú " ++ show quantidadeOk ++ " unidades de " ++ nome itemExistente ++
                              " (restante: " ++ show novaQuantidade ++ ")",
                    status = Sucesso
                  }
            in Right (inventarioNovo, logEntry)

------------------------------------------------
-- Atualizar quantidade de um item no baú
-- Define a quantidade absoluta de um item existente
-----------------------------------------------
-- Regra de falha:
-- - Item não existe no baú

atualizarQuantidade ::
  Inventario ->
  UTCTime -> -- timestamp
  ItemID ->
  Int -> -- nova quantidade
  Either String (Inventario, LogEntry)

atualizarQuantidade inventarioAtual agora itemID novaQtd =
    case Map.lookup itemID inventarioAtual of
      Nothing ->
        Left $ "Item '" ++ itemID ++ "' não existe no seu baú"

      Just itemExistente ->
        let quantidadeOk = normalizarQuantidade novaQtd
            inventarioNovo = if quantidadeOk == 0
                              then Map.delete itemID inventarioAtual
                              else Map.insert itemID (itemExistente { quantidade = quantidadeOk }) inventarioAtual
            logEntry = LogEntry
              { timestamp = agora,
                acao = Atualizar,
                detalhes = "Quantidade de " ++ nome itemExistente ++ " atualizada para " ++ show quantidadeOk ++ " no seu baú",
                status = Sucesso
              }
        in Right (inventarioNovo, logEntry)

------------------------------------------------
-- Listar todos os itens do baú
-- Retorna a lista de itens ordenada por ItemId
-----------------------------------------------
-- Esta função nunca falha, sempre retorna uma lista (pode ser vazia)

listarItens :: Inventario -> [Item]
listarItens inventarioAtual =
  -- toAscList retorna lista de pares (ItemId, Item) ordenados por ItemId
  -- map snd pega apenas os Items
  map snd (Map.toAscList inventarioAtual)