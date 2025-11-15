module Core.Data
  (
    Item (..),
    Inventario,
    ItemID,
    AcaoLog (..),
    LogEntry (..),
    StatusLog (..),

    normalizarQuantidade,
  )
where

import Data.Map.Strict qualified as Map
import Data.Time (UTCTime)

type ItemID = String

type Inventario = Map.Map ItemID Item

----------------------------------------------
-- Registrar tipo item
----------------------------------------------

data Item = Item
  { itemID :: ItemID,
    nome :: String,
    quantidade :: Int,
    categoria :: String
  }
  deriving (Show, Read, Eq)

----------------------------------------------
-- Registrar tipo log
-- Ação registrada no log (o que o usuário tentou fazer)
----------------------------------------------

data AcaoLog
  = Adicionar
  | Remover
  | Atualizar
  | ConsultaFalha
  deriving (Show, Read, Eq, Ord)


data StatusLog
  = Sucesso
  | Falha String
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


------------------------------------------------
-- Funções auxiliares
------------------------------------------------

normalizarQuantidade :: Int -> Int
normalizarQuantidade quantidade
  | quantidade < 0 = 0
  | otherwise = quantidade

