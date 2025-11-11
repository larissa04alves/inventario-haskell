-- Define os tipos de dados principais do sistema
-- Todos derivam de Show e Read

module Core.Data
  ( -- * Tipos de dados
    Item (..),
    Inventario,
    ItemId,
    AcaoLog (..),
    LogEntry (..),
    StatusLog (..),

    -- * Helpers
    normalizarQuantidade,
  )
where

-- Map para armazenar itens por chave (itemId)
import Data.Map.Strict qualified as Map
-- Tipo para representar um timestamp
import Data.Time (UTCTime)

type ItemId = String -- identificador único do item

type Inventario = Map.Map ItemId Item -- mapa de itens no inventário

----------------------------------------------
-- Registrar tipo item
----------------------------------------------

data Item = Item
  { itemId :: ItemId,
    nome :: String,
    quantidade :: Int,
    categoria :: String
  }
  deriving (Show, Read, Eq) -- derivando de Show, Read e Eq para facilitar a impressão e comparação, permitem salvar e carregar

----------------------------------------------
-- Registrar tipo log
-- Ação registrada no log (o que o usuário tentou fazer)
----------------------------------------------

data AcaoLog
  = Adicionar
  | Remover
  | Atualizar
  | ConsultaFalha
  deriving (Show, Read, Eq)

-- Resultado da tentativa (sucesso ou falha com motivo)

data StatusLog
  = Sucesso -- operacao realizada com sucesso
  | Falha String -- operacao negada + mensagem de erro
  deriving (Show, Read, Eq)

------------------------------------------------
-- Entrada de log (registro completo)
------------------------------------------------

data LogEntry = LogEntry
  { timestamp :: UTCTime,
    acao :: AcaoLog,
    status :: StatusLog,
    detalhes :: String
  }
  deriving (Show, Read, Eq)

-- Também serializamos o log (um por linha no arquivo de auditoria)

------------------------------------------------
-- Funções auxiliares
------------------------------------------------

normalizarQuantidade :: Int -> Int
normalizarQuantidade quantidade
  | quantidade < 0 = 0
  | otherwise = quantidade -- otherwise é o caso padrão, não é necessário escrever e significa true

-- ^ Garante que quantidade nunca fique negativa dentro da lógica pura.
