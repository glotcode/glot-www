module Model.Language where

import Prelude (readsPrec)
import ClassyPrelude.Yesod
import Util.Multiline (multiline)
import Settings.StaticFiles
import qualified Data.Aeson as Aeson

data Language
    = Assembly
    | Ats
    | Bash
    | C
    | Clojure
    | Cobol
    | Coffeescript
    | Cpp
    | Crystal
    | Csharp
    | D
    | Elixir
    | Elm
    | Erlang
    | Fsharp
    | Go
    | Groovy
    | Haskell
    | Idris
    | Java
    | Javascript
    | Julia
    | Kotlin
    | Lua
    | Mercury
    | Nim
    | Ocaml
    | Perl
    | Php
    | Python
    | Raku
    | Ruby
    | Rust
    | Scala
    | Swift
    | TypeScript
    | Plaintext
    deriving (Eq)


instance Aeson.FromJSON Language where
    parseJSON = Aeson.withText "Language" $ \text ->
        pure (toLanguage text)

instance Aeson.ToJSON Language where
    toJSON language =
        Aeson.String (pack $ show language)


instance PathPiece Language where
    toPathPiece = pack . show
    fromPathPiece s = Just $ toLanguage s

-- Language type to lowercase ascii name, used in urls, etc
instance Show Language where
    show Assembly = "assembly"
    show Ats = "ats"
    show Bash = "bash"
    show C = "c"
    show Clojure = "clojure"
    show Cobol = "cobol"
    show Coffeescript = "coffeescript"
    show Cpp = "cpp"
    show Crystal = "crystal"
    show Csharp = "csharp"
    show D = "d"
    show Elixir = "elixir"
    show Elm = "elm"
    show Erlang = "erlang"
    show Fsharp = "fsharp"
    show Go = "go"
    show Groovy = "groovy"
    show Haskell = "haskell"
    show Idris = "idris"
    show Java = "java"
    show Javascript = "javascript"
    show Julia = "julia"
    show Kotlin = "kotlin"
    show Lua = "lua"
    show Mercury = "mercury"
    show Nim = "nim"
    show Ocaml = "ocaml"
    show Perl = "perl"
    show Php = "php"
    show Python = "python"
    show Raku = "raku"
    show Ruby = "ruby"
    show Rust = "rust"
    show Scala = "scala"
    show Swift = "swift"
    show TypeScript = "typescript"
    show Plaintext = "plaintext"

instance Read Language where
    readsPrec _ value = [(toLanguage $ pack value, value)]

-- Lowercase ascii name to language type
toLanguage :: Text -> Language
toLanguage "assembly" = Assembly
toLanguage "ats" = Ats
toLanguage "bash" = Bash
toLanguage "clojure" = Clojure
toLanguage "cobol" = Cobol
toLanguage "coffeescript" = Coffeescript
toLanguage "cpp" = Cpp
toLanguage "c" = C
toLanguage "crystal" = Crystal
toLanguage "csharp" = Csharp
toLanguage "d" = D
toLanguage "elixir" = Elixir
toLanguage "elm" = Elm
toLanguage "erlang" = Erlang
toLanguage "fsharp" = Fsharp
toLanguage "go" = Go
toLanguage "groovy" = Groovy
toLanguage "haskell" = Haskell
toLanguage "idris" = Idris
toLanguage "javascript" = Javascript
toLanguage "julia" = Julia
toLanguage "kotlin" = Kotlin
toLanguage "lua" = Lua
toLanguage "mercury" = Mercury
toLanguage "nim" = Nim
toLanguage "ocaml" = Ocaml
toLanguage "java" = Java
toLanguage "perl" = Perl
toLanguage "php" = Php
toLanguage "python" = Python
toLanguage "raku" = Raku
toLanguage "ruby" = Ruby
toLanguage "rust" = Rust
toLanguage "scala" = Scala
toLanguage "swift" = Swift
toLanguage "typescript" = TypeScript
toLanguage _ = Plaintext

-- List of all languages, languages in this list will show up on the homepage
allLanguages :: [Language]
allLanguages = [
        Assembly,
        Ats,
        Bash,
        C,
        Clojure,
        Cobol,
        Coffeescript,
        Cpp,
        Crystal,
        Csharp,
        D,
        Elixir,
        Elm,
        Erlang,
        Fsharp,
        Go,
        Groovy,
        Haskell,
        Idris,
        Java,
        Javascript,
        Julia,
        Kotlin,
        Lua,
        Mercury,
        Nim,
        Ocaml,
        Perl,
        Php,
        Plaintext,
        Python,
        Raku,
        Ruby,
        Rust,
        Scala,
        Swift,
        TypeScript
    ]

-- Default file extensions
languageFileExt :: Language -> Text
languageFileExt Assembly = "asm"
languageFileExt Ats = "dats"
languageFileExt Bash = "sh"
languageFileExt C = "c"
languageFileExt Clojure = "clj"
languageFileExt Cobol = "cob"
languageFileExt Coffeescript = "coffee"
languageFileExt Cpp = "cpp"
languageFileExt Crystal = "cr"
languageFileExt Csharp = "cs"
languageFileExt D = "d"
languageFileExt Elixir = "ex"
languageFileExt Elm = "elm"
languageFileExt Erlang = "erl"
languageFileExt Fsharp = "fs"
languageFileExt Go = "go"
languageFileExt Groovy = "groovy"
languageFileExt Kotlin = "kt"
languageFileExt Haskell = "hs"
languageFileExt Idris = "idr"
languageFileExt Java = "java"
languageFileExt Javascript = "js"
languageFileExt Julia = "jl"
languageFileExt Lua = "lua"
languageFileExt Mercury = "m"
languageFileExt Nim = "nim"
languageFileExt Ocaml = "ml"
languageFileExt Perl = "pl"
languageFileExt Php = "php"
languageFileExt Python = "py"
languageFileExt Raku = "raku"
languageFileExt Ruby = "rb"
languageFileExt Rust = "rs"
languageFileExt Scala = "scala"
languageFileExt Swift = "swift"
languageFileExt TypeScript = "ts"
languageFileExt Plaintext = "txt"

-- Default file name
languageDefaultFname :: Language -> Text
languageDefaultFname Java = "Main." ++ languageFileExt Java
languageDefaultFname Elm = "Main." ++ languageFileExt Elm
languageDefaultFname lang = "main." ++ languageFileExt lang

-- Route to svg image in /static/img/ use img_generic_svg if image is not available
languageLogo :: Language -> StaticRoute
languageLogo Assembly = img_generic_svg
languageLogo Ats = img_ats_svg
languageLogo Bash = img_bash_svg
languageLogo C = img_c_svg
languageLogo Clojure = img_clojure_svg
languageLogo Cobol = img_generic_svg
languageLogo Coffeescript = img_coffeescript_svg
languageLogo Cpp = img_cpp_svg
languageLogo Crystal = img_crystal_svg
languageLogo Csharp = img_csharp_svg
languageLogo D = img_d_svg
languageLogo Elixir = img_elixir_svg
languageLogo Elm = img_elm_svg
languageLogo Erlang = img_erlang_svg
languageLogo Fsharp = img_fsharp_svg
languageLogo Go = img_go_svg
languageLogo Groovy = img_groovy_svg
languageLogo Haskell = img_haskell_svg
languageLogo Idris = img_idris_svg
languageLogo Java = img_java_svg
languageLogo Javascript = img_javascript_svg
languageLogo Julia = img_julia_svg
languageLogo Kotlin = img_kotlin_svg
languageLogo Lua = img_lua_svg
languageLogo Mercury = img_mercury_svg
languageLogo Nim = img_nim_svg
languageLogo Ocaml = img_ocaml_svg
languageLogo Perl = img_perl_svg
languageLogo Php = img_php_svg
languageLogo Python = img_python_svg
languageLogo Raku = img_perl6_svg
languageLogo Ruby = img_ruby_svg
languageLogo Rust = img_rust_svg
languageLogo Scala = img_scala_svg
languageLogo Swift = img_swift_svg
languageLogo TypeScript = img_typescript_svg
languageLogo Plaintext = img_plaintext_svg

-- Route to png image in /static/img/ use img_generic_svg_png if image is not available
languageLogoPng :: Language -> StaticRoute
languageLogoPng Assembly = img_generic_svg_png
languageLogoPng Ats = img_ats_svg_png
languageLogoPng Bash = img_bash_svg_png
languageLogoPng C = img_c_svg_png
languageLogoPng Clojure = img_clojure_svg_png
languageLogoPng Cobol = img_generic_svg_png
languageLogoPng Coffeescript = img_coffeescript_svg_png
languageLogoPng Cpp = img_cpp_svg_png
languageLogoPng Crystal = img_crystal_svg_png
languageLogoPng Csharp = img_csharp_svg_png
languageLogoPng D = img_d_svg_png
languageLogoPng Elixir = img_elixir_svg_png
languageLogoPng Elm = img_elm_svg_png
languageLogoPng Erlang = img_erlang_svg_png
languageLogoPng Fsharp = img_fsharp_svg_png
languageLogoPng Go = img_go_svg_png
languageLogoPng Groovy = img_groovy_svg_png
languageLogoPng Haskell = img_haskell_svg_png
languageLogoPng Idris = img_idris_svg_png
languageLogoPng Java = img_java_svg_png
languageLogoPng Javascript = img_javascript_svg_png
languageLogoPng Julia = img_julia_svg_png
languageLogoPng Kotlin = img_kotlin_svg_png
languageLogoPng Lua = img_lua_svg_png
languageLogoPng Mercury = img_generic_svg_png
languageLogoPng Nim = img_nim_svg_png
languageLogoPng Ocaml = img_ocaml_svg_png
languageLogoPng Perl = img_perl_svg_png
languageLogoPng Php = img_php_svg_png
languageLogoPng Python = img_python_svg_png
languageLogoPng Raku = img_perl6_svg_png
languageLogoPng Ruby = img_ruby_svg_png
languageLogoPng Rust = img_rust_svg_png
languageLogoPng Scala = img_scala_svg_png
languageLogoPng Swift = img_swift_svg_png
languageLogoPng TypeScript = img_typescript_svg_png
languageLogoPng Plaintext = img_plaintext_svg_png

-- Mode to use in the ace editor, check the /static/lib/ace directory for available modes
languageAceMode :: Language -> Text
languageAceMode Assembly = "ace/mode/assembly_x86"
languageAceMode Ats = "ace/mode/ats"
languageAceMode Bash = "ace/mode/sh"
languageAceMode C = "ace/mode/c_cpp"
languageAceMode Clojure = "ace/mode/clojure"
languageAceMode Cobol = "ace/mode/cobol"
languageAceMode Coffeescript = "ace/mode/coffee"
languageAceMode Cpp = "ace/mode/c_cpp"
languageAceMode Crystal = "ace/mode/crystal"
languageAceMode Csharp = "ace/mode/csharp"
languageAceMode D = "ace/mode/d"
languageAceMode Elixir = "ace/mode/elixir"
languageAceMode Elm = "ace/mode/elm"
languageAceMode Erlang = "ace/mode/erlang"
languageAceMode Fsharp = "ace/mode/fsharp"
languageAceMode Go = "ace/mode/golang"
languageAceMode Groovy = "ace/mode/groovy"
languageAceMode Haskell = "ace/mode/haskell"
languageAceMode Idris = "ace/mode/plain_text"
languageAceMode Java = "ace/mode/java"
languageAceMode Javascript = "ace/mode/javascript"
languageAceMode Julia = "ace/mode/julia"
languageAceMode Kotlin = "ace/mode/kotlin"
languageAceMode Lua = "ace/mode/lua"
languageAceMode Mercury = "ace/mode/plain_text"
languageAceMode Nim = "ace/mode/nim"
languageAceMode Ocaml = "ace/mode/ocaml"
languageAceMode Perl = "ace/mode/perl"
languageAceMode Php = "ace/mode/php"
languageAceMode Python = "ace/mode/python"
languageAceMode Raku = "ace/mode/perl6"
languageAceMode Ruby = "ace/mode/ruby"
languageAceMode Rust = "ace/mode/rust"
languageAceMode Scala = "ace/mode/scala"
languageAceMode Swift = "ace/mode/swift"
languageAceMode TypeScript = "ace/mode/typescript"
languageAceMode Plaintext = "ace/mode/plain_text"

-- Language name, used in titles, etc
languageName :: Language -> Text
languageName Assembly = "Assembly"
languageName Ats = "ATS"
languageName Bash = "Bash"
languageName C = "C"
languageName Clojure = "Clojure"
languageName Cobol = "COBOL"
languageName Coffeescript = "Coffeescript"
languageName Cpp = "C++"
languageName Crystal = "Crystal"
languageName Csharp = "C#"
languageName D = "D"
languageName Elixir = "Elixir"
languageName Elm = "Elm"
languageName Erlang = "Erlang"
languageName Fsharp = "F#"
languageName Go = "Go"
languageName Groovy = "Groovy"
languageName Haskell = "Haskell"
languageName Idris = "Idris"
languageName Java = "Java"
languageName Javascript = "Javascript"
languageName Julia = "Julia"
languageName Kotlin = "Kotlin"
languageName Lua = "Lua"
languageName Mercury = "Mercury"
languageName Nim = "Nim"
languageName Ocaml = "Ocaml"
languageName Perl = "Perl"
languageName Php = "PHP"
languageName Python = "Python"
languageName Raku = "Raku"
languageName Ruby = "Ruby"
languageName Rust = "Rust"
languageName Scala = "Scala"
languageName Swift = "Swift"
languageName TypeScript = "TypeScript"
languageName Plaintext = "Plaintext"

-- If the language is runnable or not
languageIsRunnable :: Language -> Bool
languageIsRunnable Plaintext = False
languageIsRunnable _ = True

languageDockerImage :: Language -> Text
languageDockerImage Assembly = "glot/assembly:latest"
languageDockerImage Ats = "glot/ats:latest"
languageDockerImage Bash = "glot/bash:latest"
languageDockerImage C = "glot/clang:latest"
languageDockerImage Clojure = "glot/clojure:latest"
languageDockerImage Cobol = "glot/cobol:latest"
languageDockerImage Coffeescript = "glot/coffeescript:latest"
languageDockerImage Cpp = "glot/clang:latest"
languageDockerImage Crystal = "glot/crystal:latest"
languageDockerImage Csharp = "glot/csharp:latest"
languageDockerImage D = "glot/dlang:latest"
languageDockerImage Elixir = "glot/elixir:latest"
languageDockerImage Elm = "glot/elm:latest"
languageDockerImage Erlang = "glot/erlang:latest"
languageDockerImage Fsharp = "glot/fsharp:latest"
languageDockerImage Go = "glot/golang:latest"
languageDockerImage Groovy = "glot/groovy:latest"
languageDockerImage Haskell = "glot/haskell:latest"
languageDockerImage Idris = "glot/idris:latest"
languageDockerImage Java = "glot/java:latest"
languageDockerImage Javascript = "glot/javascript:latest"
languageDockerImage Julia = "glot/julia:latest"
languageDockerImage Kotlin = "glot/kotlin:latest"
languageDockerImage Lua = "glot/lua:latest"
languageDockerImage Mercury = "glot/mercury:latest"
languageDockerImage Nim = "glot/nim:latest"
languageDockerImage Ocaml = "glot/ocaml:latest"
languageDockerImage Perl = "glot/perl:latest"
languageDockerImage Php = "glot/php:latest"
languageDockerImage Python = "glot/python:latest"
languageDockerImage Raku = "glot/raku:latest"
languageDockerImage Ruby = "glot/ruby:latest"
languageDockerImage Rust = "glot/rust:latest"
languageDockerImage Scala = "glot/scala:latest"
languageDockerImage Swift = "glot/swift:latest"
languageDockerImage TypeScript = "glot/typescript:latest"
languageDockerImage Plaintext = ""

-- Example of custom run command, not important, use "todo" if you are unsure what to add
languageRunCmdExample :: Language -> String
languageRunCmdExample Assembly = "nasm -f elf64 -o a.o main.asm && ld -o a.out a.o && ./a.out"
languageRunCmdExample Ats = "make"
languageRunCmdExample Bash = "bash main.sh"
languageRunCmdExample C = "clang main.c && ./a.out"
languageRunCmdExample Clojure = "java -cp /usr/share/java/clojure.jar clojure.main main.clj"
languageRunCmdExample Cobol = "cobc -x -o a.out main.cob && ./a.out"
languageRunCmdExample Coffeescript = "coffee main.coffee"
languageRunCmdExample Cpp = "clang++ main.cpp && ./a.out"
languageRunCmdExample Crystal = "crystal run main.cr"
languageRunCmdExample Csharp = "mcs -out:a.exe main.cs && mono a.exe"
languageRunCmdExample D = "dmd -ofa.out main.d && ./a.out"
languageRunCmdExample Elixir = "elixirc main.ex"
languageRunCmdExample Elm = "elm make --output a.js Main.elm && elm-runner a.js"
languageRunCmdExample Erlang = "escript main.erl"
languageRunCmdExample Fsharp = "fsharpc --out:a.exe main.fs > /dev/null && mono a.exe"
languageRunCmdExample Go = "go run main.go"
languageRunCmdExample Groovy = "groovy main.groovy"
languageRunCmdExample Haskell = "runghc main.hs"
languageRunCmdExample Idris = "idris -o a.out main.idr && ./a.out"
languageRunCmdExample Java = "javac Main.java && java Main"
languageRunCmdExample Javascript = "node main.js"
languageRunCmdExample Julia = "julia main.jl"
languageRunCmdExample Kotlin = "kotlinc main.kt && kotlin MainKt"
languageRunCmdExample Lua = "lua main.lua"
languageRunCmdExample Mercury = "mmc -o a.out main.m && ./a.out"
languageRunCmdExample Nim = "nim --verbosity:0 compile --run main.nim"
languageRunCmdExample Ocaml = "ocamlc -o a.out main.ml && ./a.out"
languageRunCmdExample Perl = "perl main.pl"
languageRunCmdExample Php = "php main.php"
languageRunCmdExample Python = "python main.py"
languageRunCmdExample Raku = "raku main.raku"
languageRunCmdExample Ruby = "ruby main.rb"
languageRunCmdExample Rust = "rustc -o a.out main.rs && ./a.out"
languageRunCmdExample Scala = "scalac main.scala && scala Main"
languageRunCmdExample Swift = "swift main.swift"
languageRunCmdExample TypeScript = "tsc main.ts && node main.js"
languageRunCmdExample _ = ""

-- Hello World example code, used as initial code when a new snippet is created
languageDefaultContent :: Language -> String
languageDefaultContent Assembly = [multiline|section .data
    msg db "Hello World!", 0ah

section .text
    global _start
_start:
    mov rax, 1
    mov rdi, 1
    mov rsi, msg
    mov rdx, 13
    syscall
    mov rax, 60
    mov rdi, 0
    syscall|]
languageDefaultContent Ats = [multiline|implement main0 () = print"Hello World!\n"|]
languageDefaultContent Bash = [multiline|echo Hello World|]
languageDefaultContent C = [multiline|#include <stdio.h>

int main(void) {
    printf("Hello World!\n");
    return 0;
}|]
languageDefaultContent Clojure = [multiline|(println "Hello World!")|]
languageDefaultContent Cobol = [multiline|       IDENTIFICATION DIVISION.
       PROGRAM-ID. hello.

       PROCEDURE DIVISION.
           DISPLAY 'Hello World!'
           GOBACK
           .
|]
languageDefaultContent Coffeescript = [multiline|console.log "Hello World!"|]
languageDefaultContent Cpp = [multiline|#include <iostream>
using namespace std;

int main() {
    cout << "Hello World!";
    return 0;
}|]
languageDefaultContent Csharp = [multiline|using System;
using System.Collections.Generic;
using System.Linq;

class MainClass {
    static void Main() {
        Console.WriteLine("Hello World!");
    }
}|]
languageDefaultContent Crystal = [multiline|puts "Hello World!"|]
languageDefaultContent D = [multiline|import std.stdio;

void main()
{
    writeln("Hello World!");
}|]
languageDefaultContent Elixir = [multiline|IO.puts "Hello World!"|]
languageDefaultContent Elm = [multiline|module Main exposing (main)

import Html exposing (..)

main =
    text "Hello World!"|]
languageDefaultContent Erlang = [multiline|% escript will ignore the first line

main(_) ->
    io:format("Hello World!~n").|]
languageDefaultContent Fsharp = [multiline|printfn "Hello World!"|]
languageDefaultContent Go = [multiline|package main

import (
    "fmt"
)

func main() {
    fmt.Println("Hello World!")
}|]
languageDefaultContent Groovy = [multiline|println "Hello World!"|]
languageDefaultContent Haskell = [multiline|main = putStrLn "Hello World!"|]
languageDefaultContent Idris = [multiline|module Main

main : IO ()
main = putStrLn "Hello World!"|]
languageDefaultContent Java = [multiline|class Main {
    public static void main(String[] args) {
        System.out.println("Hello World!");
    }
}|]
languageDefaultContent Javascript = [multiline|console.log("Hello World!");|]
languageDefaultContent Julia = [multiline|println("Hello world!")|]
languageDefaultContent Kotlin = [multiline|fun main(args : Array<String>){
    println("Hello World!")
}|]
languageDefaultContent Lua = [multiline|print("Hello World!");|]
languageDefaultContent Mercury = [multiline|:- module main.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
	io.write_string("Hello World!", !IO).|]
languageDefaultContent Nim = [multiline|echo("Hello World!")|]
languageDefaultContent Ocaml = [multiline|print_endline "Hello World!"|]
languageDefaultContent Perl = [multiline|print "Hello World!\n";|]
languageDefaultContent Php = [multiline|<?php

echo "Hello World\n";|]
languageDefaultContent Python = [multiline|print("Hello World!")|]
languageDefaultContent Raku = [multiline|say 'Hello World!';|]
languageDefaultContent Ruby = [multiline|puts "Hello World!"|]
languageDefaultContent Rust = [multiline|fn main() {
    println!("Hello World!");
}|]
languageDefaultContent Scala = [multiline|object Main extends App {
    println("Hello World!")
}|]
languageDefaultContent Swift = [multiline|print("Hello World!")|]
languageDefaultContent TypeScript = [multiline|const hello : string = "Hello World!"
console.log(hello)|]
languageDefaultContent Plaintext = [multiline|Hello World!|]
