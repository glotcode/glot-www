let RunConfig = ./types/RunConfig.dhall

let Language = ./types/Language.dhall

in    [ { identifier = "assembly"
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
      , { identifier = "ats"
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
      , { identifier = "bash"
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
      , { identifier = "c"
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
      , { identifier = "clojure"
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
      , { identifier = "cobol"
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
      , { identifier = "coffeescript"
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
      , { identifier = "cpp"
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
      , { identifier = "crystal"
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
      , { identifier = "csharp"
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
      , { identifier = "d"
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
      , { identifier = "elixir"
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
      , { identifier = "elm"
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
      , { identifier = "erlang"
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
      , { identifier = "fsharp"
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
      , { identifier = "go"
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
      , { identifier = "groovy"
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
      , { identifier = "haskell"
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
      , { identifier = "idris"
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
      , { identifier = "java"
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
      , { identifier = "javascript"
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
      , { identifier = "julia"
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
      , { identifier = "kotlin"
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
      , { identifier = "lua"
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
      , { identifier = "mercury"
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
      , { identifier = "nim"
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
      , { identifier = "ocaml"
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
      , { identifier = "perl"
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
      , { identifier = "php"
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
      , { identifier = "python"
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
      , { identifier = "raku"
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
      , { identifier = "ruby"
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
      , { identifier = "rust"
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
      , { identifier = "scala"
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
      , { identifier = "swift"
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
      , { identifier = "typescript"
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
      , { identifier = "plaintext"
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
    : List Language
