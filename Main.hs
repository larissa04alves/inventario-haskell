module Main where

import Control.Exception (IOException, try)
import Core.Data
  ( AcaoLog (..),
    Inventario,
    Item (..),
    ItemID,
    LogEntry (..),
    StatusLog (..),
    normalizarQuantidade,
  )
import Core.Logic qualified as Logic
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime, getCurrentTime)
import Relatorio (ResumoRelatorio (..), logsDeErro, resumoRelatorio)
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

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
            Nothing -> pure Map.empty
  where
    safeRead :: (Read a) => String -> Maybe a
    safeRead s = case reads s of
      [(x, "")] -> Just x
      _ -> Nothing

salvarInventario :: Inventario -> IO ()
salvarInventario inventario = writeFile arquivoInventario (show inventario)

-----------------------------------------------
-- Função de persistencia (IO) - Auditoria
-----------------------------------------------

registrarAuditoria :: LogEntry -> IO ()
registrarAuditoria le = appendFile arquivoAuditoria (show le <> "\n")

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
    mapMaybeRead :: (Read a) => [String] -> [a]
    mapMaybeRead =
      foldr
        ( \ln acc -> case reads ln of
            [(x, "")] -> x : acc
            _ -> acc
        )
        []

-----------------------------------------------
-- Parseamento de comandos (texto -> ação)
-----------------------------------------------
data Comando
  = ComandoAdicionar
  | ComandoRemover
  | ComandoUpdate
  | ComandoListar
  | ComandoReport
  | ComandoAjuda
  | ComandoSair
  | ComandoDesconhecido String

interpretarComando :: String -> Comando
interpretarComando linha =
  case words (map toLower linha) of
    ["adicionar"] -> ComandoAdicionar
    ["remover"] -> ComandoRemover
    ["update"] -> ComandoUpdate
    ["listar"] -> ComandoListar
    ["report"] -> ComandoReport
    ["ajuda"] -> ComandoAjuda
    ["sair"] -> ComandoSair
    [] -> ComandoDesconhecido "Digite um comando"
    _ -> ComandoDesconhecido "Comando não reconhecido"
  where
    toLower :: Char -> Char
    toLower c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c

-----------------------------------------------
-- Execução de comandos
-----------------------------------------------

perguntarTexto :: String -> IO String
perguntarTexto prompt = do
  putStr $ prompt ++ ": "
  hFlush stdout
  resposta <- getLine
  let respostaSemEspacos = unwords (words resposta)
  if null respostaSemEspacos
    then do
      putStrLn "Entrada vazia. Tente novamente."
      perguntarTexto prompt
    else pure respostaSemEspacos

perguntarInteiro :: String -> IO Int
perguntarInteiro prompt = do
  putStr $ prompt ++ ": "
  hFlush stdout
  linha <- getLine
  case readMaybe linha of
    Just n | n >= 0 -> pure n
    _ -> do
      putStrLn "Número inválido. Digite um número inteiro não-negativo."
      perguntarInteiro prompt

executarComando :: Inventario -> Comando -> IO Inventario
executarComando inventario cmd = case cmd of
  ComandoAdicionar -> do
    putStrLn "\n--- Adicionar Item ao Baú ---"
    itemID <- perguntarTexto "ItemID (ex: FERRO01)"
    nomeItem <- perguntarTexto "Nome do item (ex: Minério de Ferro)"
    qtd <- perguntarInteiro "Quantidade"
    categoriaItem <- perguntarTexto "Categoria (ex: Minérios)"

    agora <- getCurrentTime
    case Logic.adicionarItem inventario agora itemID nomeItem qtd categoriaItem of
      Left msgErro -> do
        registrarAuditoria (mensagemErro agora Adicionar msgErro)
        putStrLn $ "Erro ao adicionar item: " ++ msgErro
        pure inventario
      Right (inventarioNovo, logEntry) -> do
        salvarInventario inventarioNovo
        registrarAuditoria logEntry
        putStrLn $ "Item adicionado com sucesso: " ++ nomeItem
        pure inventarioNovo
  ComandoRemover -> do
    putStrLn "\n--- Remover Item do Baú ---"
    let itens = Logic.listarItens inventario
    if null itens
      then do
        putStrLn "Seu baú está vazio."
        pure inventario
      else do
        putStrLn "Itens disponíveis:"
        mapM_ printItem itens
        putStrLn ""
        itemID <- perguntarTexto "ItemID do item a remover"
        qtd <- perguntarInteiro "Quantidade a remover"

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
  ComandoUpdate -> do
    putStrLn "\n--- Atualizar Quantidade do Item ---"
    let itens = Logic.listarItens inventario
    if null itens
      then do
        putStrLn "Seu baú está vazio."
        pure inventario
      else do
        putStrLn "Itens disponíveis:"
        mapM_ printItem itens
        putStrLn ""
        itemID <- perguntarTexto "ItemID do item a atualizar"
        novaQtd <- perguntarInteiro "Nova quantidade"

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

mensagemErro :: UTCTime -> AcaoLog -> String -> LogEntry
mensagemErro agora acao detalhesTxt =
  LogEntry
    { timestamp = agora,
      acao = acao,
      detalhes = "Falha na ação: " ++ detalhesTxt,
      status = Falha detalhesTxt
    }

printItem :: Item -> IO ()
printItem item =
  putStrLn $
    "ID: "
      ++ itemID item
      ++ ", Nome: "
      ++ nome item
      ++ ", Quantidade: "
      ++ show (quantidade item)
      ++ ", Categoria: "
      ++ categoria item

imprimirRelatorio :: [LogEntry] -> IO ()
imprimirRelatorio logs = do
  let resumo = resumoRelatorio logs
  let falhas = logsDeErro logs

  putStrLn "\n=== RELATORIO DO BAU - MINECRAFT ==="

  if totalRegistros resumo == 0
    then putStrLn "\nNenhuma atividade registrada no bau ainda.\n"
    else do
      putStrLn "\nEstatisticas Gerais:"
      putStrLn "----------------------------------"
      putStrLn $ "Total de operacoes: " ++ show (totalRegistros resumo)
      putStrLn $
        "Operacoes bem-sucedidas: "
          ++ show (totalSucessos resumo)
          ++ " ("
          ++ porcentagem (totalSucessos resumo) (totalRegistros resumo)
          ++ ")"
      putStrLn $
        "Operacoes com falha: "
          ++ show (totalFalhas resumo)
          ++ " ("
          ++ porcentagem (totalFalhas resumo) (totalRegistros resumo)
          ++ ")"

      putStrLn "\nFrequencia de Acoes:"
      putStrLn "----------------------------------"
      if Map.null (porAcao resumo)
        then putStrLn "Nenhuma acao registrada."
        else mapM_ imprimirAcao (Map.toList (porAcao resumo))

      if null falhas
        then do
          putStrLn "\nFalhas:"
          putStrLn "----------------------------------"
          putStrLn "Nenhuma falha registrada."
        else do
          putStrLn "\nFalhas:"
          putStrLn "----------------------------------"
          mapM_ imprimirFalha (zip [1 ..] falhas)

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

    imprimirFalha :: (Int, LogEntry) -> IO ()
    imprimirFalha (num, logEntry) =
      putStrLn $
        "  "
          ++ show num
          ++ ". ["
          ++ show (acao logEntry)
          ++ "] "
          ++ detalhes logEntry
          ++ " ("
          ++ show (timestamp logEntry)
          ++ ")"

imprimirAjuda :: IO ()
imprimirAjuda = do
  putStrLn "\n=== MANUAL DO BAU - MINECRAFT ==="
  putStrLn "\nComandos disponiveis:"
  putStrLn "----------------------------------"
  putStrLn "  adicionar"
  putStrLn "    Adiciona itens ao seu bau"
  putStrLn ""
  putStrLn "  remover"
  putStrLn "    Remove itens do seu bau"
  putStrLn ""
  putStrLn "  update"
  putStrLn "    Atualiza a quantidade de um item"
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
    _ -> loopComandos inv
