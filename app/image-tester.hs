{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude
import Data.Text (Text)
import Data.Function ((&))
import Glot.Language (Language)
import Data.List.NonEmpty( NonEmpty( (:|) ) )

import qualified System.Process as Process
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Maybe as Maybe
import qualified Data.Either as Either
import qualified Data.Text.Encoding as Encoding
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson as Aeson
import qualified Glot.Language as Language
import qualified Glot.DockerRun as DockerRun
import qualified Glot.Snippet as Snippet
import qualified System.Environment as Env


main :: IO ()
main =
    let
        prepareIds :: [String] -> [Language.Id]
        prepareIds languageIds =
            languageIds
                & map Text.pack
                & map Language.idFromText
                & Maybe.catMaybes

        filterLanguages :: [Language.Id] -> [Language] -> [Language]
        filterLanguages languageIds languages =
            if null languageIds then
                languages
            else
                languages
                    & filter (\lang -> elem (Language.identifier lang) languageIds)

        prepareLanguages :: [Language] -> [LanguageData]
        prepareLanguages languages =
            languages
                & map languageDataFromLanguage
                & Maybe.catMaybes
    in do
    languageIdsToRun <- Env.getArgs
    languages <- Language.readLanguages
    languages
        & filterLanguages (prepareIds languageIdsToRun)
        & prepareLanguages
        & mapM_ runLanguage


data LanguageData = LanguageData
    { languageName :: Text
    , container :: Text
    , runRequest :: DockerRun.RunRequestPayload
    }
    deriving (Show)

languageDataFromLanguage :: Language.Language -> Maybe LanguageData
languageDataFromLanguage lang = do
    runConfig <- Language.runConfig lang
    Just $ LanguageData
      { languageName = Language.idToText (Language.identifier lang)
      , container = Language.containerImage runConfig
      , runRequest = DockerRun.RunRequestPayload
        { language = Language.identifier lang
        , stdin = Nothing
        , command = Nothing
        , files =
            singleton $ Snippet.FilePayload
                { name =
                    lang
                        & Language.editorConfig
                        & Language.defaultFilename
                        & Snippet.titleFromText
                        & Either.fromRight (error "Invalid filename")
                , content =
                    lang
                        & Language.editorConfig
                        & Language.exampleCode
                        & Snippet.fileContentFromText
                        & Either.fromRight (error "Invalid file content")
                }
        }
      }


runLanguage :: LanguageData -> IO ()
runLanguage languageData = do
    runOutput <- runContainer languageData
    printResult languageData (checkRunOutput runOutput)


data RunOutput = RunOutput
    { stdout :: Text
    , stderr :: Text
    }

runContainer :: LanguageData -> IO RunOutput
runContainer LanguageData{..} =
    let
        cmd =
            "docker run --rm -i --read-only --tmpfs /tmp:rw,noexec,nosuid,size=65536k --tmpfs /home/glot:rw,exec,nosuid,uid=1000,gid=1000,size=131072k -u glot -w /home/glot " <> Text.unpack container

        stdinPayload =
            runRequest
                & Aeson.encode
                & BSL.toStrict
                & Encoding.decodeUtf8
                & Text.unpack
    in do
    (_, stdout, stderr) <- Process.readCreateProcessWithExitCode (Process.shell cmd) stdinPayload
    pure $ RunOutput
        { stdout = Text.pack stdout
        , stderr = Text.pack stderr
        }


checkRunOutput :: RunOutput -> Either Error ()
checkRunOutput runOutput = do
    runResult <- decodeRunResult runOutput
    _ <- checkRunResult runResult
    pure ()

decodeRunResult :: RunOutput -> Either Error DockerRun.RunResult
decodeRunResult runOutput@RunOutput{..} =
    stdout
        & Encoding.encodeUtf8
        & Aeson.eitherDecodeStrict'
        & mapErr (FailedToDecodeResult runOutput)


checkRunResult :: DockerRun.RunResult -> Either Error ()
checkRunResult runResult@DockerRun.RunResult{..} =
    if Text.null error && stderrIsOk stderr && isHelloWorld stdout then
        Right ()

    else
        Left (InvalidHelloWorld runResult)




printResult :: LanguageData -> Either Error () -> IO ()
printResult languageData@LanguageData{..} result =
    case result of
        Right () ->
            putStrLn $ Text.unpack $ mconcat
                [ "OK: "
                , languageName
                , " ["
                , container
                , "]"
                ]

        Left err ->
            printError languageData err


data Error
    = FailedToDecodeResult RunOutput String
    | InvalidHelloWorld DockerRun.RunResult

-- TODO: print cmd to run container manually echo 'foo' | docker run ...
printError :: LanguageData -> Error -> IO ()
printError LanguageData{..} err =
    case err of
        FailedToDecodeResult RunOutput{..} _decodeErr ->
            putStrLn $ Text.unpack $ mconcat
                [ "Failed: "
                , languageName
                , " ["
                , container
                , "] failed to decode result, stdout: «"
                , Text.stripEnd stdout
                , "», stderr: «"
                , Text.stripEnd stderr
                , "»"
                ]

        InvalidHelloWorld DockerRun.RunResult{..} ->
            putStrLn $ Text.unpack $ mconcat
                [ "Failed: "
                , languageName
                , " ["
                ,  container
                , "] stdout: «"
                ,  Text.stripEnd stdout
                , "», stderr: «"
                ,  Text.stripEnd stderr
                , "», error: «"
                ,  Text.stripEnd error
                , "»"
                ]


isHelloWorld :: Text -> Bool
isHelloWorld text =
    let
        normalizedText =
            text
                & Text.stripEnd
                & Text.replace "\"" ""
                & Text.toLower
    in
    normalizedText == "hello world!"


stderrIsOk :: Text -> Bool
stderrIsOk err =
    let
        expectedErrors =
            [ "Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.\n"
            ]
    in
    Text.null err || elem err expectedErrors




-- TODO: Use newer version of NonEmpty
singleton :: a -> NonEmpty a
singleton a = a :| []


-- TODO: Use Bifunctor.first
mapErr :: (e -> b) -> Either e a -> Either b a
mapErr mapper either =
    case either of
        Left err ->
            Left (mapper err)

        Right value ->
            Right value