module Relatorio
  ( historicoPorItem
  , logsDeErro
  , totalSucessoFalha
  , totalPorAcao
  , ResumoRelatorio(..)
  , resumoRelatorio
  , extrairItemId
  ) where

import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)

import Core.Data
  ( ItemID
  , AcaoLog(..)
  , StatusLog(..)
  , LogEntry(..)
  )

-----------------------------------------------
-- Extração de informações dos logs
-----------------------------------------------

extrairItemId :: LogEntry -> Maybe ItemID
extrairItemId LogEntry{detalhes = d} =
  case procurarPrefixo "id:" d of
    Nothing -> Nothing
    Just resto ->
      let itemID = takeWhile (\c -> c /= ' ' && c /= '/' && c /= ')' && c /= ',') resto
      in if null itemID then Nothing else Just itemID
  where
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

historicoPorItem :: ItemID -> [LogEntry] -> [LogEntry]
historicoPorItem itemIDBusca =
  filter (\le -> extrairItemId le == Just itemIDBusca)

logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro = filter ehFalha
  where
    ehFalha LogEntry{status = Falha _} = True
    ehFalha _ = False

-----------------------------------------------
-- Totais gerais
-----------------------------------------------

totalSucessoFalha :: [LogEntry] -> (Int, Int)
totalSucessoFalha = foldr contarStatus (0, 0)
  where
    contarStatus LogEntry{status = Sucesso} (ok, er) = (ok + 1, er)
    contarStatus LogEntry{status = Falha _} (ok, er) = (ok, er + 1)

totalPorAcao :: [LogEntry] -> Map.Map AcaoLog Int
totalPorAcao = foldr adicionarAcao Map.empty
  where
    adicionarAcao LogEntry{acao = a} = Map.insertWith (+) a 1

-----------------------------------------------
-- Resumo completo
-----------------------------------------------
data ResumoRelatorio = ResumoRelatorio
  { totalRegistros :: Int
  , totalSucessos :: Int
  , totalFalhas :: Int
  , porAcao :: Map.Map AcaoLog Int
  }
  deriving (Show, Read, Eq)

resumoRelatorio :: [LogEntry] -> ResumoRelatorio
resumoRelatorio logs =
  let (sucessos, falhas) = totalSucessoFalha logs
  in ResumoRelatorio
      { totalRegistros = length logs
      , totalSucessos = sucessos
      , totalFalhas = falhas
      , porAcao = totalPorAcao logs
      }