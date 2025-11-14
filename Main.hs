-- Papel: ponto de entrada, loop de comandos, persistencia

module Main where

import qualified Data.Map.Strict as Map
import Data.Time (getCurrentTime, UTCTime)
import System.IO (hFlush, stdout)
import System.Directory (doesFileExist)
import Control.Exception (try, IOException)
import Text.Read (readMaybe)

-- módulos puros
import Core.Data
  ( Item(..)
  , ItemID
  , Inventario
  , AcaoLog(..)
  , StatusLog(..)
  , LogEntry(..)
  , normalizarQuantidade
  )

import qualified Core.Logic as Logic
import Relatorio (resumoRelatorio, ResumoRelatorio(..))

-----------------------------------------------
-- Constantes de arquivos
-----------------------------------------------

arquivoInventario :: FilePath
arquivoInventario = "Inventario.dat"

arquivoAuditoria :: FilePath
arquivoAuditoria = "Auditoria.log"

-----------------------------------------------
-- Função de persistencia (IO) - Inventario
-----------------------------------------------
-- Carrega o inventario do arquivo se existir, senão retorna inventario vazio

carregarInventario :: IO Inventario
carregarInventario = do
  existe <- doesFileExist arquivoInventario
  if not existe
    then pure Map.empty
    else do
      conteudoOuErro <- try (readFile arquivoInventario) :: IO (Either IOException String)
      case conteudoOuErro of
        Left _ -> pure Map.empty
        Right txt ->
         case safeRead txt of
            Just inv -> pure inv
            Nothing  -> pure Map.empty
  where
    -- safeRead usa reads para evitar exceções
    safeRead :: Read a => String -> Maybe a
    safeRead s = case reads s of
      [(x, "")] -> Just x
      _         -> Nothing

-- Salva o inventario no arquivo
salvarInventario :: Inventario -> IO ()
salvarInventario inventario = writeFile arquivoInventario (show inventario)

-----------------------------------------------
-- Função de persistencia (IO) - Auditoria
-----------------------------------------------
-- Adiciona uma entrada de log ao arquivo de auditoria

registrarAuditoria :: LogEntry -> IO ()
registrarAuditoria le = appendFile arquivoAuditoria (show le <> "\n")

-- carrega todas as linhas de auditoria como logEntry

carregarAuditoria :: IO [LogEntry]
carregarAuditoria = do
  existe <- doesFileExist arquivoAuditoria
  if not existe
    then pure []
    else do
      conteudoOuErro <- try (readFile arquivoAuditoria) :: IO (Either IOException String)
      case conteudoOuErro of
        Left _ -> pure []
        Right txt -> pure (mapMaybeRead (lines txt))

  where
    mapMaybeRead :: Read a => [String] -> [a]
    mapMaybeRead = foldr (\ln acc -> case reads ln of
                                        [(x, "")] -> x : acc
                                        _ -> acc) []

-----------------------------------------------
-- Parseamento de comandos (texto -> ação)
-----------------------------------------------
-- Tipo simples para command
data Comando
  = ComandoAdicionar ItemID String Int String
  | ComandoRemover ItemID Int
  | ComandoUpdate ItemID Int
  | ComandoListar
  | ComandoReport
  | ComandoAjuda
  | ComandoSair
  | ComandoDesconhecido String

-- transforma uma linha de texto em um comando
-- regra: nome/categoria usam _ para espaços

interpretarComando :: String -> Comando
interpretarComando linha =
  case words linha of
    ("adicionar":itemID:nome:qtd:categoria:[]) ->
      case readMaybe qtd of
        Just n -> ComandoAdicionar itemID (replaceUnderscores nome) n (replaceUnderscores categoria)
        Nothing -> ComandoDesconhecido "Quantidade inválida"

    ("remover":itemID:qtd:[]) ->
      case readMaybe qtd of
        Just n -> ComandoRemover itemID n
        Nothing -> ComandoDesconhecido "Quantidade inválida"

    ("update":itemID:novaQtd:[]) ->
      case readMaybe novaQtd of
        Just n -> ComandoUpdate itemID n
        Nothing -> ComandoDesconhecido "Quantidade inválida"

    ["listar"] -> ComandoListar
    ["report"] -> ComandoReport
    ["ajuda"] -> ComandoAjuda
    ["sair"] -> ComandoSair
    [] -> ComandoDesconhecido "Digite um comando"
    _ -> ComandoDesconhecido "Comando não reconhecido"

  where
    -- troca _ por espaço
    replaceUnderscores :: String -> String
    replaceUnderscores = map (\c -> if c == '_' then ' ' else c)

-----------------------------------------------
-- Execução de comandos
-----------------------------------------------

-- executa um comando e devolve o novo inventario

executarComando :: Inventario -> Comando -> IO Inventario
executarComando inventario cmd = case cmd of
  ComandoAdicionar itemID nome qtd categoria -> do
    agora <- getCurrentTime
    case Logic.adicionarItem inventario agora itemID nome qtd categoria of
      Left msgErro -> do
        registrarAuditoria (mensagemErro agora Adicionar msgErro)
        putStrLn $ "Erro ao adicionar item: " ++ msgErro
        pure inventario
      Right (inventarioNovo, logEntry) -> do
        salvarInventario inventarioNovo
        registrarAuditoria logEntry
        putStrLn $ "Item adicionado com sucesso: " ++ nome
        pure inventarioNovo

  ComandoRemover itemID qtd -> do
    agora <- getCurrentTime
    case Logic.removerItem inventario agora itemID qtd of
      Left msgErro -> do
        registrarAuditoria (mensagemErro agora Remover msgErro)
        putStrLn $ "Erro ao remover item: " ++ msgErro
        pure inventario
      Right (inventarioNovo, logEntry) -> do
        salvarInventario inventarioNovo
        registrarAuditoria logEntry
        putStrLn $ "Item removido com sucesso: " ++ itemID
        pure inventarioNovo

  ComandoUpdate itemID novaQtd -> do
    agora <- getCurrentTime
    case Logic.atualizarQuantidade inventario agora itemID novaQtd of
      Left msgErro -> do
        registrarAuditoria (mensagemErro agora Atualizar msgErro)
        putStrLn $ "Erro ao atualizar item: " ++ msgErro
        pure inventario
      Right (inventarioNovo, logEntry) -> do
        salvarInventario inventarioNovo
        registrarAuditoria logEntry
        putStrLn $ "Item atualizado com sucesso: " ++ itemID
        pure inventarioNovo

  ComandoListar -> do
    let itens = Logic.listarItens inventario
    if null itens
      then putStrLn "Seu baú está vazio."
      else mapM_ printItem itens
    pure inventario

  ComandoReport -> do
    auditoria <- carregarAuditoria
    imprimirRelatorio auditoria
    pure inventario

  ComandoAjuda -> do
    imprimirAjuda
    pure inventario

  ComandoSair -> do
    putStrLn "Fechando o seu baú!"
    pure inventario

  ComandoDesconhecido msg -> do
    putStrLn $ "Comando desconhecido: " ++ msg
    pure inventario

-----------------------------------------------
-- Funções de ajuda de log e impressão
-----------------------------------------------

-- Constrói um LogEntry de erro
mensagemErro :: UTCTime -> AcaoLog -> String -> LogEntry
mensagemErro agora acao detalhesTxt =
  LogEntry
    { timestamp = agora,
      acao = acao,
      detalhes = "Falha na ação: " ++ detalhesTxt,
      status = Falha detalhesTxt
    }

-- Imprime um item formatado
printItem :: Item -> IO ()
printItem item = putStrLn $
  "ID: " ++ itemID item ++
  ", Nome: " ++ nome item ++
  ", Quantidade: " ++ show (quantidade item) ++
  ", Categoria: " ++ categoria item

-- Imprime o relatório de auditoria
imprimirRelatorio :: [LogEntry] -> IO ()
imprimirRelatorio logs = do
  let resumo = resumoRelatorio logs

  putStrLn "\n=== RELATORIO DO BAU - MINECRAFT ==="

  if totalRegistros resumo == 0
    then putStrLn "\nNenhuma atividade registrada no bau ainda.\n"
    else do
      putStrLn "\nEstatisticas Gerais:"
      putStrLn "----------------------------------"
      putStrLn $ "Total de operacoes: " ++ show (totalRegistros resumo)
      putStrLn $ "Operacoes bem-sucedidas: " ++ show (totalSucessos resumo) ++
                 " (" ++ porcentagem (totalSucessos resumo) (totalRegistros resumo) ++ ")"
      putStrLn $ "Operacoes com falha: " ++ show (totalFalhas resumo) ++
                 " (" ++ porcentagem (totalFalhas resumo) (totalRegistros resumo) ++ ")"

      putStrLn "\nFrequencia de Acoes:"
      putStrLn "----------------------------------"
      if Map.null (porAcao resumo)
        then putStrLn "Nenhuma acao registrada."
        else mapM_ imprimirAcao (Map.toList (porAcao resumo))

      putStrLn "\n==================================\n"

  where
    porcentagem :: Int -> Int -> String
    porcentagem parte total' =
      if total' == 0
        then "0%"
        else show (parte * 100 `div` total') ++ "%"

    imprimirAcao :: (AcaoLog, Int) -> IO ()
    imprimirAcao (acaoLog, freq) =
      putStrLn $ "  " ++ show acaoLog ++ ": " ++ show freq ++ " vez(es)"

-- Imprime a ajuda de comandos
imprimirAjuda :: IO ()
imprimirAjuda = do
  putStrLn "\n=== MANUAL DO BAU - MINECRAFT ==="
  putStrLn "\nComandos disponiveis:"
  putStrLn "----------------------------------"
  putStrLn "  adicionar <itemID> <nome> <quantidade> <categoria>"
  putStrLn "    Adiciona itens ao seu bau"
  putStrLn "    Exemplo: adicionar ESPADA01 Espada_de_Diamante 1 Armas"
  putStrLn ""
  putStrLn "  remover <itemID> <quantidade>"
  putStrLn "    Remove itens do seu bau"
  putStrLn "    Exemplo: remover ESPADA01 1"
  putStrLn ""
  putStrLn "  update <itemID> <novaQuantidade>"
  putStrLn "    Atualiza a quantidade de um item"
  putStrLn "    Exemplo: update ESPADA01 5"
  putStrLn ""
  putStrLn "  listar"
  putStrLn "    Mostra todos os itens no bau"
  putStrLn ""
  putStrLn "  report"
  putStrLn "    Gera relatorio completo de atividades"
  putStrLn ""
  putStrLn "  ajuda"
  putStrLn "    Mostra este manual"
  putStrLn ""
  putStrLn "  sair"
  putStrLn "    Fecha o programa"
  putStrLn "\nDICA: Use _ (underscore) no lugar de espacos em nomes"
  putStrLn "==================================\n"

-----------------------------------------------
-- Loop principal
-----------------------------------------------
main :: IO ()
main = do
  putStrLn "\n=== Sistema de Inventario - Minecraft Bau ==="
  inventarioInicial <- carregarInventario
  let totalItens = Map.size inventarioInicial
  if totalItens == 0
    then putStrLn "Bau vazio carregado. Comece adicionando itens!"
    else putStrLn $ "Bau carregado: " ++ show totalItens ++ " tipo(s) de item(ns) encontrado(s)."
  putStrLn "Digite 'ajuda' para ver os comandos disponiveis.\n"
  loopComandos inventarioInicial

loopComandos :: Inventario -> IO ()
loopComandos inventarioAtual = do
  putStr "> "
  hFlush stdout
  linha <- getLine
  let comando = interpretarComando linha
  inv <- executarComando inventarioAtual comando
  case comando of
    ComandoSair -> pure ()
    _           -> loopComandos inv

