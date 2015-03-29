module Model.Language where

import Prelude (reads, readsPrec)
import ClassyPrelude.Yesod
import Util.Multiline (multiline)

data Language = Bash |
                C |
                Cpp |
                Erlang |
                Go |
                Haskell |
                Java |
                Javascript |
                Perl |
                Php |
                Python |
                Ruby
                deriving Eq


instance PathPiece Language where
    toPathPiece = pack . show
    fromPathPiece s =
        case reads $ unpack s of
            (lang, _):_ -> Just $ lang
            [] -> Nothing

instance Show Language where
    show Bash = "bash"
    show C = "c"
    show Cpp = "cpp"
    show Erlang = "erlang"
    show Go = "go"
    show Haskell = "haskell"
    show Java = "java"
    show Javascript = "javascript"
    show Perl = "perl"
    show Php = "php"
    show Python = "python"
    show Ruby = "ruby"

instance Read Language where
    readsPrec _ value =
        tryParse [
            ("bash", Bash),
            ("cpp", Cpp),
            ("c", C),
            ("erlang", Erlang),
            ("go", Go),
            ("haskell", Haskell),
            ("javascript", Javascript),
            ("java", Java),
            ("perl", Perl),
            ("php", Php),
            ("python", Python),
            ("ruby", Ruby)
        ] where
            tryParse [] = []
            tryParse ((attempt, result):xs) =
                if (take (length attempt) value) == attempt
                then [(result, drop (length attempt) value)]
                else tryParse xs


allLanguages :: [Language]
allLanguages = [Bash, C, Cpp, Erlang, Go, Haskell, Java, Javascript, Perl, Php, Python, Ruby]

languageFileExt :: Language -> Text
languageFileExt Bash = "sh"
languageFileExt C = "c"
languageFileExt Cpp = "cpp"
languageFileExt Erlang = "erl"
languageFileExt Go = "go"
languageFileExt Haskell = "hs"
languageFileExt Java = "java"
languageFileExt Javascript = "js"
languageFileExt Perl = "pl"
languageFileExt Php = "php"
languageFileExt Python = "py"
languageFileExt Ruby = "rb"

languageDefaultFname :: Language -> Text
languageDefaultFname Bash = "main." ++ languageFileExt Bash
languageDefaultFname C = "main." ++ languageFileExt C
languageDefaultFname Cpp = "main." ++ languageFileExt Cpp
languageDefaultFname Erlang = "main." ++ languageFileExt Erlang
languageDefaultFname Go = "main." ++ languageFileExt Go
languageDefaultFname Haskell = "main." ++ languageFileExt Haskell
languageDefaultFname Java = "Main." ++ languageFileExt Java
languageDefaultFname Javascript = "main." ++ languageFileExt Javascript
languageDefaultFname Perl = "main." ++ languageFileExt Perl
languageDefaultFname Php = "main." ++ languageFileExt Php
languageDefaultFname Python = "main." ++ languageFileExt Python
languageDefaultFname Ruby = "main." ++ languageFileExt Ruby

languageIconClass :: Language -> Text
languageIconClass Bash = "icon-prog-bash02"
languageIconClass C = "icon-prog-c"
languageIconClass Cpp = "icon-prog-cplusplus"
languageIconClass Erlang = "icon-prog-erlang"
languageIconClass Go = "icon-prog-golang02"
languageIconClass Haskell = "icon-prog-haskell"
languageIconClass Java = "icon-prog-java"
languageIconClass Javascript = "icon-prog-nodejs02"
languageIconClass Perl = "icon-prog-perl"
languageIconClass Php = "icon-prog-php02"
languageIconClass Python = "icon-prog-python"
languageIconClass Ruby = "icon-prog-ruby"

languageAceMode :: Language -> Text
languageAceMode Bash = "ace/mode/sh"
languageAceMode C = "ace/mode/c_cpp"
languageAceMode Cpp = "ace/mode/c_cpp"
languageAceMode Erlang = "ace/mode/erlang"
languageAceMode Go = "ace/mode/golang"
languageAceMode Haskell = "ace/mode/haskell"
languageAceMode Java = "ace/mode/java"
languageAceMode Javascript = "ace/mode/javascript"
languageAceMode Perl = "ace/mode/perl"
languageAceMode Php = "ace/mode/php"
languageAceMode Python = "ace/mode/python"
languageAceMode Ruby = "ace/mode/ruby"

languageName :: Language -> Text
languageName Bash = "Bash"
languageName C = "C"
languageName Cpp = "C++"
languageName Erlang = "Erlang"
languageName Go = "Go"
languageName Haskell = "Haskell"
languageName Java = "Java"
languageName Javascript = "Javascript"
languageName Perl = "Perl"
languageName Php = "Php"
languageName Python = "Python"
languageName Ruby = "Ruby"

languageDefaultContent :: Language -> String
languageDefaultContent Bash = [multiline|echo Hello World|]
languageDefaultContent C = [multiline|#include <stdio.h>

int main() {
    printf("Hello World!\n");
    return 0;
}|]
languageDefaultContent Cpp = [multiline|#include <iostream>
using namespace std;

int main() {
    cout << "Hello World!";
    return 0;
}|]
languageDefaultContent Erlang = [multiline|% escript will ignore the first line

main(_) ->
    io:format("Hello World!~n").|]
languageDefaultContent Go = [multiline|package main

import (
    "fmt"
)

func main() {
    fmt.Println("Hello World!")
}|]
languageDefaultContent Haskell = [multiline|main = putStrLn "Hello World!"|]
languageDefaultContent Java = [multiline|class Main {
    public static void main(String[] args) {
        System.out.println("Hello World!");
    }
}|]
languageDefaultContent Javascript = [multiline|console.log("Hello World!");|]
languageDefaultContent Perl = [multiline|print "Hello World!\n";|]
languageDefaultContent Php = [multiline|<?php

echo "Hello World\n";|]
languageDefaultContent Python = [multiline|print("Hello World!")|]
languageDefaultContent Ruby = [multiline|puts "Hello World!"|]
