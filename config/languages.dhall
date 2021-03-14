let Language = ./types/Language.dhall

let RunConfig = ./types/RunConfig.dhall

let LanguageConfig = ./types/LanguageConfig.dhall

in    [ { language = Language.Assembly
        , name = "Assembly"
        , logoName = "generic"
        , fileExtension = "asm"
        , editorConfig =
          { defaultFilename = "main.asm"
          , mode = "ace/mode/assembly_x86"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode =
              ''
              section .data
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
                  syscall''
          }
        , runConfig = Some
          { containerImage = "glot/assembly:latest"
          , runCommand =
              "nasm -f elf64 -o a.o main.asm && ld -o a.out a.o && ./a.out"
          }
        }
      , { language = Language.Ats
        , name = "ATS"
        , logoName = "ats"
        , fileExtension = "dats"
        , editorConfig =
          { defaultFilename = "main.dats"
          , mode = "ace/mode/ats"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode = "implement main0 () = print\"Hello World!\\n\""
          }
        , runConfig = Some
          { containerImage = "glot/ats:latest", runCommand = "make" }
        }
      , { language = Language.Bash
        , name = "Bash"
        , logoName = "bash"
        , fileExtension = "sh"
        , editorConfig =
          { defaultFilename = "main.sh"
          , mode = "ace/mode/sh"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode = "echo Hello World!"
          }
        , runConfig = Some
          { containerImage = "glot/bash:latest", runCommand = "bash main.sh" }
        }
      , { language = Language.C
        , name = "C"
        , logoName = "c"
        , fileExtension = "c"
        , editorConfig =
          { defaultFilename = "main.c"
          , mode = "ace/mode/c_cpp"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode =
              ''
              #include <stdio.h>

              int main(void) {
                  printf("Hello World!\n");
                  return 0;
              }''
          }
        , runConfig = Some
          { containerImage = "glot/clang:latest"
          , runCommand = "clang main.c && ./a.out"
          }
        }
      , { language = Language.Clojure
        , name = "Clojure"
        , logoName = "clojure"
        , fileExtension = "clj"
        , editorConfig =
          { defaultFilename = "main.clj"
          , mode = "ace/mode/clojure"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode = "(println \"Hello World!\")"
          }
        , runConfig = Some
          { containerImage = "glot/clojure:latest"
          , runCommand =
              "java -cp /usr/share/java/clojure.jar clojure.main main.clj"
          }
        }
      , { language = Language.Cobol
        , name = "COBOL"
        , logoName = "generic"
        , fileExtension = "cob"
        , editorConfig =
          { defaultFilename = "main.cob"
          , mode = "ace/mode/cobol"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode =
              ''
                     IDENTIFICATION DIVISION.
                     PROGRAM-ID. hello.

                     PROCEDURE DIVISION.
                         DISPLAY 'Hello World!'
                         GOBACK
                         .
              ''
          }
        , runConfig = Some
          { containerImage = "glot/cobol:latest"
          , runCommand = "cobc -x -o a.out main.cob && ./a.out"
          }
        }
      , { language = Language.CoffeeScript
        , name = "CoffeeScript"
        , logoName = "coffeescript"
        , fileExtension = "coffee"
        , editorConfig =
          { defaultFilename = "main.coffee"
          , mode = "ace/mode/coffee"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode = "console.log \"Hello World!\""
          }
        , runConfig = Some
          { containerImage = "glot/coffeescript:latest"
          , runCommand = "coffee main.coffee"
          }
        }
      , { language = Language.Cpp
        , name = "C++"
        , logoName = "cpp"
        , fileExtension = "cpp"
        , editorConfig =
          { defaultFilename = "main.cpp"
          , mode = "ace/mode/c_cpp"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode =
              ''
              #include <iostream>
              using namespace std;

              int main() {
                  cout << "Hello World!";
                  return 0;
              }''
          }
        , runConfig = Some
          { containerImage = "glot/clang:latest"
          , runCommand = "clang++ main.cpp && ./a.out"
          }
        }
      , { language = Language.Crystal
        , name = "Crystal"
        , logoName = "crystal"
        , fileExtension = "cr"
        , editorConfig =
          { defaultFilename = "main.cr"
          , mode = "ace/mode/crystal"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode = "puts \"Hello World!\""
          }
        , runConfig = Some
          { containerImage = "glot/crystal:latest"
          , runCommand = "crystal run main.cr"
          }
        }
      , { language = Language.Csharp
        , name = "C#"
        , logoName = "csharp"
        , fileExtension = "cs"
        , editorConfig =
          { defaultFilename = "main.cs"
          , mode = "ace/mode/csharp"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode =
              ''
              using System;
              using System.Collections.Generic;
              using System.Linq;

              class MainClass {
                  static void Main() {
                      Console.WriteLine("Hello World!");
                  }
              }''
          }
        , runConfig = Some
          { containerImage = "glot/csharp:latest"
          , runCommand = "mcs -out:a.exe main.cs && mono a.exe"
          }
        }
      , { language = Language.D
        , name = "D"
        , logoName = "d"
        , fileExtension = "d"
        , editorConfig =
          { defaultFilename = "main.d"
          , mode = "ace/mode/d"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode =
              ''
              import std.stdio;

              void main()
              {
                  writeln("Hello World!");
              }''
          }
        , runConfig = Some
          { containerImage = "glot/dlang:latest"
          , runCommand = "dmd -ofa.out main.d && ./a.out"
          }
        }
      , { language = Language.Elixir
        , name = "Elixir"
        , logoName = "elixir"
        , fileExtension = "ex"
        , editorConfig =
          { defaultFilename = "main.ex"
          , mode = "ace/mode/elixir"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode = "IO.puts \"Hello World!\""
          }
        , runConfig = Some
          { containerImage = "glot/elixir:latest"
          , runCommand = "elixirc main.ex"
          }
        }
      , { language = Language.Elm
        , name = "Elm"
        , logoName = "elm"
        , fileExtension = "elm"
        , editorConfig =
          { defaultFilename = "Main.elm"
          , mode = "ace/mode/elm"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode =
              ''
              module Main exposing (main)

              import Html exposing (..)

              main =
                  text "Hello World!"''
          }
        , runConfig = Some
          { containerImage = "glot/elm:latest"
          , runCommand = "elm make --output a.js Main.elm && elm-runner a.js"
          }
        }
      , { language = Language.Erlang
        , name = "Erlang"
        , logoName = "erlang"
        , fileExtension = "erl"
        , editorConfig =
          { defaultFilename = "main.erl"
          , mode = "ace/mode/erlang"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode =
              ''
              % escript will ignore the first line

              main(_) ->
                  io:format("Hello World!~n").''
          }
        , runConfig = Some
          { containerImage = "glot/erlang:latest"
          , runCommand = "escript main.erl"
          }
        }
      , { language = Language.Fsharp
        , name = "F#"
        , logoName = "fsharp"
        , fileExtension = "fs"
        , editorConfig =
          { defaultFilename = "main.fs"
          , mode = "ace/mode/fsharp"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode = "printfn \"Hello World!\""
          }
        , runConfig = Some
          { containerImage = "glot/fsharp:latest"
          , runCommand = "fsharpc --out:a.exe main.fs > /dev/null && mono a.exe"
          }
        }
      , { language = Language.Go
        , name = "Go"
        , logoName = "go"
        , fileExtension = "go"
        , editorConfig =
          { defaultFilename = "main.go"
          , mode = "ace/mode/golang"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode =
              ''
              package main

              import (
                  "fmt"
              )

              func main() {
                  fmt.Println("Hello World!")
              }''
          }
        , runConfig = Some
          { containerImage = "glot/golang:latest"
          , runCommand = "go run main.go"
          }
        }
      , { language = Language.Groovy
        , name = "Groovy"
        , logoName = "groovy"
        , fileExtension = "groovy"
        , editorConfig =
          { defaultFilename = "main.groovy"
          , mode = "ace/mode/groovy"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode = "println \"Hello World!\""
          }
        , runConfig = Some
          { containerImage = "glot/groovy:latest"
          , runCommand = "groovy main.groovy"
          }
        }
      , { language = Language.Haskell
        , name = "Haskell"
        , logoName = "haskell"
        , fileExtension = "hs"
        , editorConfig =
          { defaultFilename = "main.hs"
          , mode = "ace/mode/haskell"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode = "main = putStrLn \"Hello World!\""
          }
        , runConfig = Some
          { containerImage = "glot/haskell:latest"
          , runCommand = "runghc main.hs"
          }
        }
      , { language = Language.Idris
        , name = "Idris"
        , logoName = "idris"
        , fileExtension = "idr"
        , editorConfig =
          { defaultFilename = "main.idr"
          , mode = "ace/mode/plain_text"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode =
              ''
              module Main

              main : IO ()
              main = putStrLn "Hello World!"''
          }
        , runConfig = Some
          { containerImage = "glot/idris:latest"
          , runCommand = "idris -o a.out main.idr && ./a.out"
          }
        }
      , { language = Language.Java
        , name = "Java"
        , logoName = "java"
        , fileExtension = "java"
        , editorConfig =
          { defaultFilename = "Main.java"
          , mode = "ace/mode/java"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode =
              ''
              class Main {
                  public static void main(String[] args) {
                      System.out.println("Hello World!");
                  }
              }''
          }
        , runConfig = Some
          { containerImage = "glot/java:latest"
          , runCommand = "javac Main.java && java Main"
          }
        }
      , { language = Language.JavaScript
        , name = "JavaScript"
        , logoName = "javascript"
        , fileExtension = "js"
        , editorConfig =
          { defaultFilename = "main.js"
          , mode = "ace/mode/javascript"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode = "console.log(\"Hello World!\");"
          }
        , runConfig = Some
          { containerImage = "glot/javascript:latest"
          , runCommand = "node main.js"
          }
        }
      , { language = Language.Julia
        , name = "Julia"
        , logoName = "julia"
        , fileExtension = "jl"
        , editorConfig =
          { defaultFilename = "main.jl"
          , mode = "ace/mode/julia"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode = "println(\"Hello world!\")"
          }
        , runConfig = Some
          { containerImage = "glot/julia:latest", runCommand = "julia main.jl" }
        }
      , { language = Language.Kotlin
        , name = "Kotlin"
        , logoName = "kotlin"
        , fileExtension = "kt"
        , editorConfig =
          { defaultFilename = "main.kt"
          , mode = "ace/mode/kotlin"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode =
              ''
              fun main(args : Array<String>){
                  println("Hello World!")
              }''
          }
        , runConfig = Some
          { containerImage = "glot/kotlin:latest"
          , runCommand = "kotlinc main.kt && kotlin MainKt"
          }
        }
      , { language = Language.Lua
        , name = "Lua"
        , logoName = "lua"
        , fileExtension = "lua"
        , editorConfig =
          { defaultFilename = "main.lua"
          , mode = "ace/mode/lua"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode = "print(\"Hello World!\");"
          }
        , runConfig = Some
          { containerImage = "glot/lua:latest", runCommand = "lua main.lua" }
        }
      , { language = Language.Mercury
        , name = "Mercury"
        , logoName = "mercury"
        , fileExtension = "m"
        , editorConfig =
          { defaultFilename = "main.m"
          , mode = "ace/mode/plain_text"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode =
              ''
              :- module main.
              :- interface.
              :- import_module io.

              :- pred main(io::di, io::uo) is det.

              :- implementation.

              main(!IO) :-
                  io.write_string("Hello World!", !IO).''
          }
        , runConfig = Some
          { containerImage = "glot/mercury:latest"
          , runCommand = "mmc -o a.out main.m && ./a.out"
          }
        }
      , { language = Language.Nim
        , name = "Nim"
        , logoName = "nim"
        , fileExtension = "nim"
        , editorConfig =
          { defaultFilename = "main.nim"
          , mode = "ace/mode/nim"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode = "echo(\"Hello World!\")"
          }
        , runConfig = Some
          { containerImage = "glot/nim:latest"
          , runCommand = "nim --verbosity:0 compile --run main.nim"
          }
        }
      , { language = Language.Ocaml
        , name = "Ocaml"
        , logoName = "ocaml"
        , fileExtension = "ml"
        , editorConfig =
          { defaultFilename = "main.ml"
          , mode = "ace/mode/ocaml"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode = "print_endline \"Hello World!\""
          }
        , runConfig = Some
          { containerImage = "glot/ocaml:latest"
          , runCommand = "ocamlc -o a.out main.ml && ./a.out"
          }
        }
      , { language = Language.Perl
        , name = "Perl"
        , logoName = "perl"
        , fileExtension = "pl"
        , editorConfig =
          { defaultFilename = "main.pl"
          , mode = "ace/mode/perl"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode = "print \"Hello World!\\n\";"
          }
        , runConfig = Some
          { containerImage = "glot/perl:latest", runCommand = "perl main.pl" }
        }
      , { language = Language.Php
        , name = "PHP"
        , logoName = "php"
        , fileExtension = "php"
        , editorConfig =
          { defaultFilename = "main.php"
          , mode = "ace/mode/php"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode =
              ''
              <?php

              echo "Hello World\n";''
          }
        , runConfig = Some
          { containerImage = "glot/php:latest", runCommand = "php main.php" }
        }
      , { language = Language.Python
        , name = "Python"
        , logoName = "python"
        , fileExtension = "py"
        , editorConfig =
          { defaultFilename = "main.py"
          , mode = "ace/mode/python"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode = "print(\"Hello World!\")"
          }
        , runConfig = Some
          { containerImage = "glot/python:latest"
          , runCommand = "python main.py"
          }
        }
      , { language = Language.Raku
        , name = "Raku"
        , logoName = "perl6"
        , fileExtension = "raku"
        , editorConfig =
          { defaultFilename = "main.raku"
          , mode = "ace/mode/perl6"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode = "say 'Hello World!';"
          }
        , runConfig = Some
          { containerImage = "glot/raku:latest", runCommand = "raku main.raku" }
        }
      , { language = Language.Ruby
        , name = "Ruby"
        , logoName = "ruby"
        , fileExtension = "rb"
        , editorConfig =
          { defaultFilename = "main.rb"
          , mode = "ace/mode/ruby"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode = "puts \"Hello World!\""
          }
        , runConfig = Some
          { containerImage = "glot/ruby:latest", runCommand = "ruby main.rb" }
        }
      , { language = Language.Rust
        , name = "Rust"
        , logoName = "rust"
        , fileExtension = "rs"
        , editorConfig =
          { defaultFilename = "main.rs"
          , mode = "ace/mode/rust"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode =
              ''
              fn main() {
                  println!("Hello World!");
              }''
          }
        , runConfig = Some
          { containerImage = "glot/rust:latest"
          , runCommand = "rustc -o a.out main.rs && ./a.out"
          }
        }
      , { language = Language.Scala
        , name = "Scala"
        , logoName = "scala"
        , fileExtension = "scala"
        , editorConfig =
          { defaultFilename = "main.scala"
          , mode = "ace/mode/scala"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode =
              ''
              object Main extends App {
                  println("Hello World!")
              }''
          }
        , runConfig = Some
          { containerImage = "glot/scala:latest"
          , runCommand = "scalac main.scala && scala Main"
          }
        }
      , { language = Language.Swift
        , name = "Swift"
        , logoName = "swift"
        , fileExtension = "swift"
        , editorConfig =
          { defaultFilename = "main.swift"
          , mode = "ace/mode/swift"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode = "print(\"Hello World!\")"
          }
        , runConfig = Some
          { containerImage = "glot/swift:latest"
          , runCommand = "swift main.swift"
          }
        }
      , { language = Language.TypeScript
        , name = "TypeScript"
        , logoName = "typescript"
        , fileExtension = "ts"
        , editorConfig =
          { defaultFilename = "main.ts"
          , mode = "ace/mode/typescript"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode =
              ''
              const hello : string = "Hello World!"
              console.log(hello)''
          }
        , runConfig = Some
          { containerImage = "glot/typescript:latest"
          , runCommand = "tsc main.ts && node main.js"
          }
        }
      , { language = Language.Plaintext
        , name = "Plaintext"
        , logoName = "generic"
        , fileExtension = "txt"
        , editorConfig =
          { defaultFilename = "main.txt"
          , mode = "ace/mode/plain_text"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode = "Hello world!"
          }
        , runConfig = None RunConfig
        }
      ]
    : List LanguageConfig
