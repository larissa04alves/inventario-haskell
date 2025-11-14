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
  , ItemId
  , Inventario
  , AcaoLog(..)
  , StatusLog(..)
  , LogEntry(..)
  , normalizarQuantidade
  )

import qualified Core.Logic as Logic

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
  = ComandoAdicionar ItemId String Int String
  | ComandoRemover ItemId Int
  | ComandoUpdate ItemId Int
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
    ("adicionar":itemId:nome:qtd:categoria:[]) ->
      case readMaybe qtd of
        Just n -> ComandoAdicionar itemId (replaceUnderscores nome) n (replaceUnderscores categoria)
        Nothing -> ComandoDesconhecido "Quantidade inválida"

    ("remover":itemId:qtd:[]) ->
      case readMaybe qtd of
        Just n -> ComandoRemover itemId n
        Nothing -> ComandoDesconhecido "Quantidade inválida"

    ("update":itemId:novaQtd:[]) ->
      case readMaybe novaQtd of
        Just n -> ComandoUpdate itemId n
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
  ComandoAdicionar itemId nome qtd categoria -> do
    agora <- getCurrentTime
    case Logic.adicionarItem inventario agora itemId nome qtd categoria of
      Left msgErro -> do
        registrarAuditoria (mensagemErro agora Adicionar msgErro)
        putStrLn $ "Erro ao adicionar item: " ++ msgErro
        pure inventario
      Right (inventarioNovo, logEntry) -> do
        salvarInventario inventarioNovo
        registrarAuditoria logEntry
        putStrLn $ "Item adicionado com sucesso: " ++ nome
        pure inventarioNovo

  ComandoRemover itemId qtd -> do
    agora <- getCurrentTime
    case Logic.removerItem inventario agora itemId qtd of
      Left msgErro -> do
        registrarAuditoria (mensagemErro agora Remover msgErro)
        putStrLn $ "Erro ao remover item: " ++ msgErro
        pure inventario
      Right (inventarioNovo, logEntry) -> do
        salvarInventario inventarioNovo
        registrarAuditoria logEntry
        putStrLn $ "Item removido com sucesso: " ++ itemId
        pure inventarioNovo

  ComandoUpdate itemId novaQtd -> do
    agora <- getCurrentTime
    case Logic.atualizarQuantidade inventario agora itemId novaQtd of
      Left msgErro -> do
        registrarAuditoria (mensagemErro agora Atualizar msgErro)
        putStrLn $ "Erro ao atualizar item: " ++ msgErro
        pure inventario
      Right (inventarioNovo, logEntry) -> do
        salvarInventario inventarioNovo
        registrarAuditoria logEntry
        putStrLn $ "Item atualizado com sucesso: " ++ itemId
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
  "ID: " ++ itemId item ++
  ", Nome: " ++ nome item ++
  ", Quantidade: " ++ show (quantidade item) ++
  ", Categoria: " ++ categoria item

-- Imprime o relatório de auditoria
imprimirRelatorio :: [LogEntry] -> IO ()
imprimirRelatorio logs = do
  let total = length logs
      sucessos = length [ () | log <- logs, status log == Sucesso ]
      falhas = total - sucessos
      frequenciaAcoes = contarAcoes logs

  putStrLn "Relatório de Auditoria:"
  putStrLn $ "Total de ações: " ++ show total
  putStrLn $ "Ações bem-sucedidas: " ++ show sucessos
  putStrLn $ "Ações com falhas: " ++ show falhas
  putStrLn "Frequência de ações:"
  mapM_ (\(acao, freq) -> putStrLn $ "  " ++ show acao ++ ": " ++ show freq) (Map.toList frequenciaAcoes)

  where
    contarAcoes :: [LogEntry] -> Map.Map AcaoLog Int
    contarAcoes = foldr add Map.empty
      where
        add logEntry acc = Map.insertWith (+) (acao logEntry) 1 acc

-- Imprime a ajuda de comandos
imprimirAjuda :: IO ()
imprimirAjuda = putStrLn $
  "Comandos disponíveis:\n" ++
  "  adicionar <itemId> <nome> <quantidade> <categoria> - Adiciona um item ao baú\n" ++
  "  remover <itemId> <quantidade> - Remove uma quantidade de um item do baú\n" ++
  "  update <itemId> <novaQuantidade> - Atualiza a quantidade de um item no baú\n" ++
  "  listar - Lista todos os itens no baú\n" ++
  "  report - Gera um relatório de auditoria\n" ++
  "  ajuda - Mostra esta ajuda\n" ++
  "  sair - Fecha o programa\n"

-----------------------------------------------
-- Loop principal
-----------------------------------------------
main :: IO ()
main = do
  putStrLn "---- Inventário MineCraft ----"
  inventarioInicial <- carregarInventario
  putStrLn $ "Baú carregado com sucesso: " ++ show (Map.size inventarioInicial) ++ " itens."
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

