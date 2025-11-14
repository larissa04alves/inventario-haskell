-- Objetivo: Funções PURAS para analisar logs (sem IO)
-- Entrada: [LogEntry]
-- Saída: filtros, contagens e resumo pronto para imprimir

module Relatorio
  ( -- * Filtros e consultas
    historicoPorItem
  , logsDeErro
  , totalSucessoFalha
  , totalPorAcao

    -- * Resumo completo
  , ResumoRelatorio(..)
  , resumoRelatorio

    -- * Helpers puros
  , extrairItemId
  ) where

import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)

import Core.Data
  ( ItemId
  , AcaoLog(..)
  , StatusLog(..)
  , LogEntry(..)
  )

-----------------------------------------------
-- Extração de informações dos logs
-----------------------------------------------

-- Extrai ItemId do campo detalhes do LogEntry se existir
-- Procura padrão como "id:ESPADA01" ou similar
extrairItemId :: LogEntry -> Maybe ItemId
extrairItemId LogEntry{detalhes = d} =
  case procurarPrefixo "id:" d of
    Nothing -> Nothing
    Just resto ->
      let itemId = takeWhile (\c -> c /= ' ' && c /= '/' && c /= ')' && c /= ',') resto
      in if null itemId then Nothing else Just itemId
  where
    -- Procura um prefixo e retorna a string após ele
    procurarPrefixo :: Eq a => [a] -> [a] -> Maybe [a]
    procurarPrefixo _ [] = Nothing
    procurarPrefixo pref xs
      | comecaCom pref xs = Just (drop (length pref) xs)
      | otherwise = procurarPrefixo pref (tail xs)

    comecaCom :: Eq a => [a] -> [a] -> Bool
    comecaCom [] _ = True
    comecaCom _ [] = False
    comecaCom (p:ps) (y:ys) = p == y && comecaCom ps ys

-----------------------------------------------
-- Filtros
-----------------------------------------------

-- Todo o histórico de logs de um item específico
historicoPorItem :: ItemId -> [LogEntry] -> [LogEntry]
historicoPorItem itemIdBusca =
  filter (\le -> extrairItemId le == Just itemIdBusca)

-- Apenas as entradas com erro (StatusLog = Falha ...)
logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro = filter ehFalha
  where
    ehFalha LogEntry{status = Falha _} = True
    ehFalha _ = False

-----------------------------------------------
-- Totais gerais
-----------------------------------------------

-- Retorna (totalSucessos, totalFalhas)
totalSucessoFalha :: [LogEntry] -> (Int, Int)
totalSucessoFalha = foldr contarStatus (0, 0)
  where
    contarStatus LogEntry{status = Sucesso} (ok, er) = (ok + 1, er)
    contarStatus LogEntry{status = Falha _} (ok, er) = (ok, er + 1)

-- Contagem por tipo de ação (Adicionar, Remover, Atualizar, ConsultaFalha)
totalPorAcao :: [LogEntry] -> Map.Map AcaoLog Int
totalPorAcao = foldr adicionarAcao Map.empty
  where
    adicionarAcao LogEntry{acao = a} = Map.insertWith (+) a 1

-----------------------------------------------
-- Resumo completo
-----------------------------------------------

-- Estrutura para exibir um resumo do relatório
data ResumoRelatorio = ResumoRelatorio
  { totalRegistros :: Int
  , totalSucessos :: Int
  , totalFalhas :: Int
  , porAcao :: Map.Map AcaoLog Int
  }
  deriving (Show, Read, Eq)

-- Monta um resumo completo dos logs
resumoRelatorio :: [LogEntry] -> ResumoRelatorio
resumoRelatorio logs =
  let (sucessos, falhas) = totalSucessoFalha logs
  in ResumoRelatorio
      { totalRegistros = length logs
      , totalSucessos = sucessos
      , totalFalhas = falhas
      , porAcao = totalPorAcao logs
      }