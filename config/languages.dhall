let RunConfig = ./types/RunConfig.dhall

let Language = ./types/Language.dhall

let Book = ./types/Book.dhall

in    [ { id = "assembly"
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
          , versionCommand = "nasm --version"
          }
        , books =
            [ { purchaseLink = "https://www.amazon.com/Art-64-Bit-Assembly-Language/dp/1718501080?_encoding=UTF8&psc=1&refRID=W4Q0JX99S7MPZ4TCAY8P&linkCode=li3&tag=glot06-20&linkId=90bd558a8d5b707f298624be26ca60ff&language=en_US&ref_=as_li_ss_il"
              , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1718501080&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
              , bookTitle = "The Art of 64-Bit Assembly"
              }
            , { purchaseLink = "https://www.amazon.com/Modern-X86-Assembly-Language-Programming/dp/1484240626?_encoding=UTF8&psc=1&refRID=W4Q0JX99S7MPZ4TCAY8P&linkCode=li3&tag=glot06-20&linkId=b96aa897f7b812878a5a034fad51384a&language=en_US&ref_=as_li_ss_il"
              , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1484240626&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
              , bookTitle = "Modern X86 Assembly Language Programming"
              }
            , { purchaseLink = "https://www.amazon.com/xchg-rax-xorpd/dp/1502958082?_encoding=UTF8&psc=1&refRID=W4Q0JX99S7MPZ4TCAY8P&linkCode=li3&tag=glot06-20&linkId=ff9389b29e7100f07951af12180d2f36&language=en_US&ref_=as_li_ss_il"
              , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1502958082&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
              , bookTitle = "xchg rax,rax"
              }
            ] : List Book
        }
      , { id = "ats"
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
          { containerImage = "glot/ats:latest"
          , runCommand = "make"
          , versionCommand = "patscc --version"
          }
        , books = [] : List Book
        }
      , { id = "bash"
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
          { containerImage = "glot/bash:latest"
          , runCommand = "bash main.sh"
          , versionCommand = "bash --version"
          }
        , books =
          [ { purchaseLink = "https://www.amazon.com/Linux-Command-Line-2nd-Introduction/dp/1593279523?_encoding=UTF8&psc=1&refRID=S4EBY2GK1KE0GSDTTGC8&linkCode=li3&tag=glot06-20&linkId=5405bdf8362076ee739b8f8fd6c94c77&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1593279523&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "The Linux Command Line"
            }
          , { purchaseLink = "https://www.amazon.com/Wicked-Cool-Shell-Scripts-2nd/dp/1593276028?_encoding=UTF8&psc=1&refRID=S4EBY2GK1KE0GSDTTGC8&linkCode=li3&tag=glot06-20&linkId=8f64430b7214c7d8c5cf5c9d8f006a95&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1593276028&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Wicked Cool Shell Scripts"
            }
          , { purchaseLink = "https://www.amazon.com/Learning-bash-Shell-Programming-Nutshell/dp/0596009658?dchild=1&keywords=bash&qid=1628535278&sr=8-1&linkCode=li3&tag=glot06-20&linkId=0b2f41a23e25abc83107b1f26e48c9b4&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=0596009658&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Learning the bash Shell"
            }
          ] : List Book
        }
      , { id = "c"
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
          , versionCommand = "clang --version"
          }
        , books =
            [ { purchaseLink = "https://www.amazon.com/gp/product/1718501048?ie=UTF8&linkCode=li3&tag=glot06-20&linkId=69c65136c9167d29dd8ea3408c7891f8&language=en_US&ref_=as_li_ss_il"
              , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1718501048&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
              , bookTitle = "Effective C"
              }
            , { purchaseLink = "https://www.amazon.com/Programming-Language-2nd-Brian-Kernighan/dp/0131103628?_encoding=UTF8&psc=1&refRID=F96TAQ2KD9M11C046DSC&linkCode=li3&tag=glot06-20&linkId=f6266a35acac68f29a58cd06fe954317&language=en_US&ref_=as_li_ss_il"
              , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=0131103628&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
              , bookTitle = "The C Programming Language"
              }
            ] : List Book
        }
      , { id = "clojure"
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
          , versionCommand = "clj --version"
          }
        , books =
          [ { purchaseLink = "https://www.amazon.com/Clojure-Brave-True-Ultimate-Programmer/dp/1593275919?pd_rd_w=GPBWG&pf_rd_p=016e3697-91be-4dc2-9533-ef9350e7e73d&pf_rd_r=XCYC213587F3DVQWRQ8Y&pd_rd_r=4fee5800-ae25-4ebf-b7ad-9c965655554c&pd_rd_wg=Tbdhl&pd_rd_i=1593275919&psc=1&linkCode=li3&tag=glot06-20&linkId=14b1c1cc7a8a85e66d5640fb0d9f30dd&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1593275919&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Clojure for the Brave and True"
            }
          , { purchaseLink = "https://www.amazon.com/Joy-Clojure-Michael-Fogus/dp/1617291412?pd_rd_w=GPBWG&pf_rd_p=016e3697-91be-4dc2-9533-ef9350e7e73d&pf_rd_r=XCYC213587F3DVQWRQ8Y&pd_rd_r=4fee5800-ae25-4ebf-b7ad-9c965655554c&pd_rd_wg=Tbdhl&pd_rd_i=1617291412&psc=1&linkCode=li3&tag=glot06-20&linkId=29aaa623adadddd73dfe87082aa2450f&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1617291412&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "The Joy of Clojure"
            }
          ] : List Book
        }
      , { id = "cobol"
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
          , versionCommand = "cobc --version"
          }
        , books =
          [ { purchaseLink = "https://www.amazon.com/Beginning-COBOL-Programmers-Michael-Coughlan/dp/1430262532?dchild=1&keywords=cobol&qid=1628536294&s=books&sr=1-2&linkCode=li3&tag=glot06-20&linkId=d35e630e95c2a3abd79717a5f7f15c47&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1430262532&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Beginning COBOL for Programmers"
            }
          , { purchaseLink = "https://www.amazon.com/COBOL-Basic-Training-Using-VSAM/dp/1734584726?dchild=1&keywords=cobol&qid=1628536506&s=books&sr=1-3&linkCode=li3&tag=glot06-20&linkId=4ab96724263438a880e6de7af6ed6131&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1734584726&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "COBOL Basic Training"
            }
          , { purchaseLink = "https://www.amazon.com/COBOL-21st-Century-Nancy-Stern/dp/1118739531?dchild=1&keywords=cobol&qid=1628536294&s=books&sr=1-4&linkCode=li3&tag=glot06-20&linkId=6917c6f9b00e89344009e3abf7d9f9aa&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1118739531&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "COBOL for the 21st Century"
            }
          ] : List Book
        }
      , { id = "coffeescript"
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
          , versionCommand = "coffee --version"
          }
        , books =
          [ { purchaseLink = "https://www.amazon.com/CoffeeScript-Action-Patrick-Lee-dp-1617290629/dp/1617290629?_encoding=UTF8&me=&qid=1628536743&linkCode=li3&tag=glot06-20&linkId=094602c0bcd00b44672ec45a5ccb1895&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1617290629&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "CoffeeScript in Action"
            }
          , { purchaseLink = "https://www.amazon.com/Little-Book-CoffeeScript-JavaScript-Developers/dp/1449321054?dchild=1&keywords=coffeescript&qid=1628536743&s=books&sr=1-4&linkCode=li3&tag=glot06-20&linkId=947976ea83cef8a014bbb96b9dd7f818&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1449321054&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "The Little Book on CoffeeScript"
            }
          ] : List Book
        }
      , { id = "cpp"
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
          , versionCommand = "clang --version"
          }
        , books =
          [ { purchaseLink = "https://www.amazon.com/C-Crash-Course-Josh-Lospinoso/dp/1593278888?_encoding=UTF8&psc=1&refRID=TRZ40JZX0TJC99RWRNC3&linkCode=li3&tag=glot06-20&linkId=9cd26766aa6c95c953d27ef1315e6094&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1593278888&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "C++ Crash Course"
            }
          , { purchaseLink = "https://www.amazon.com/Starting-Out-Control-Structures-Objects/dp/0134498372?_encoding=UTF8&psc=1&refRID=TRZ40JZX0TJC99RWRNC3&linkCode=li3&tag=glot06-20&linkId=dfb888b61e18e4e9fba439db00e908e7&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=0134498372&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Starting Out with C++"
            }
          , { purchaseLink = "https://www.amazon.com/Effective-Modern-Specific-Ways-Improve/dp/1491903996?_encoding=UTF8&psc=1&refRID=TRZ40JZX0TJC99RWRNC3&linkCode=li3&tag=glot06-20&linkId=0db816902a23107fa0b82619eff8ec09&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1491903996&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Effective Modern C++"
            }
          ] : List Book
        }
      , { id = "crystal"
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
          , versionCommand = "crystal --version"
          }
        , books =
          [ { purchaseLink = "https://www.amazon.com/Programming-Crystal-Create-High-Performance-Concurrent/dp/1680502867?dchild=1&keywords=crystal+programming&qid=1628537031&s=books&sr=1-1&linkCode=li3&tag=glot06-20&linkId=1bd258ef044713bfa0b53f85994e27e1&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1680502867&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Programming Crystal"
            }
          ] : List Book
        }
      , { id = "csharp"
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
          , versionCommand = "mcs --version"
          }
        , books =
          [ { purchaseLink = "https://www.amazon.com/Head-First-Learners-Real-World-Programming/dp/1491976705?_encoding=UTF8&psc=1&refRID=38VQ15A9W6GCP70CM4XZ&linkCode=li3&tag=glot06-20&linkId=4d5638a8b80d87c1bd000bac08553d41&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1491976705&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Head First C#"
            }
          , { purchaseLink = "https://www.amazon.com/NET-Cross-Platform-Development-intelligent-Framework/dp/180056810X?_encoding=UTF8&psc=1&refRID=38VQ15A9W6GCP70CM4XZ&linkCode=li3&tag=glot06-20&linkId=a921bd41a6224bb4975a13d5b104ab98&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=180056810X&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "C# 9 and .NET 5"
            }
          , { purchaseLink = "https://www.amazon.com/C-9-0-Nutshell-Definitive-Reference-dp-1098100964/dp/1098100964?_encoding=UTF8&me=&qid=&linkCode=li3&tag=glot06-20&linkId=d779db86705a99cd59d4260010dafbef&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1098100964&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "C# 9.0 in a Nutshell"
            }
          , { purchaseLink = "https://www.amazon.com/C-Depth-Jon-Skeet/dp/1617294535?_encoding=UTF8&psc=1&refRID=38VQ15A9W6GCP70CM4XZ&linkCode=li3&tag=glot06-20&linkId=36528b0a884047b94d78dc7f2657d589&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1617294535&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "C# in Depth"
            }
          ] : List Book
        }
      , { id = "d"
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
          , versionCommand = "dmd --version"
          }
        , books =
          [ { purchaseLink = "https://www.amazon.com/Learn-Programming-Designed-individuals-starting/dp/B096LPVFRV?crid=2N3L2UVNJTXPR&keywords=d+programming+language&qid=1628537854&sprefix=d+programming%2Cstripbooks-intl-ship%2C230&sr=8-2&linkCode=li3&tag=glot06-20&linkId=4139afc8480f81b0deba542c3c053d26&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=B096LPVFRV&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Learn D Programming"
            }
          , { purchaseLink = "https://www.amazon.com/D-Programming-Language-Andrei-Alexandrescu/dp/0321635361?crid=2N3L2UVNJTXPR&dchild=1&keywords=d+programming+language&qid=1628537854&sprefix=d+programming%2Cstripbooks-intl-ship%2C230&sr=8-1&linkCode=li3&tag=glot06-20&linkId=894e60001cf7b8e334c9fc0d53950156&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=0321635361&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "The D Programming Language"
            }
          ] : List Book
        }
      , { id = "elixir"
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
          , versionCommand = "elixirc main.ex"
          , runCommand = "elixirc --version | tail -n 1"
          }
        , books =
          [ { purchaseLink = "https://www.amazon.com/Programming-Elixir-1-6-Functional-Concurrent/dp/1680502999?crid=UMBVK47J54SP&dchild=1&keywords=elixir+programming&qid=1628538085&sprefix=elixir+pro%2Caps%2C252&sr=8-1&linkCode=li3&tag=glot06-20&linkId=5f9081f924492b5e576aa0b7ff1b9778&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1680502999&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Programming Elixir ≥ 1.6"
            }
          , { purchaseLink = "https://www.amazon.com/Elixir-Action-Sa%C5%A1a-Juri-cacute/dp/1617295027?pd_rd_w=iYtF5&pf_rd_p=3676f086-9496-4fd7-8490-77cf7f43f846&pf_rd_r=TKW3H30X9FY59C7JVZS4&pd_rd_r=381ebbaf-c2ca-4a55-9463-99ae7244b5ce&pd_rd_wg=ZfLpr&pd_rd_i=1617295027&psc=1&linkCode=li3&tag=glot06-20&linkId=af785e3f77f2814a7461b52540631648&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1617295027&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Elixir in Action"
            }
          , { purchaseLink = "https://www.amazon.com/Learn-Functional-Programming-Elixir-Foundations-dp-168050245X/dp/168050245X?_encoding=UTF8&me=&qid=1628538085&linkCode=li3&tag=glot06-20&linkId=04463a251292850b9d7a63c157b3f478&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=168050245X&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Learn Functional Programming with Elixir"
            }
          , { purchaseLink = "https://www.amazon.com/Metaprogramming-Elixir-Write-Less-Code/dp/1680500414?crid=UMBVK47J54SP&dchild=1&keywords=elixir+programming&qid=1628538085&sprefix=elixir+pro%2Caps%2C252&sr=8-11&linkCode=li3&tag=glot06-20&linkId=bb875a0ca68c68f88c152e56d4152862&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1680500414&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Metaprogramming Elixir"
            }
          , { purchaseLink = "https://www.amazon.com/Programming-Phoenix-LiveView-Interactive-JavaScript/dp/1680508210?crid=UMBVK47J54SP&dchild=1&keywords=elixir+programming&qid=1628538085&sprefix=elixir+pro%2Caps%2C252&sr=8-4&linkCode=li3&tag=glot06-20&linkId=1efed79475def7b1e47f5202bb9399da&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1680508210&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Programming Phoenix LiveView"
            }
          ] : List Book
        }
      , { id = "elm"
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
          , versionCommand = "elm --version"
          }
        , books =
          [ { purchaseLink = "https://www.amazon.com/Elm-Action-Richard-Feldman/dp/1617294047?dchild=1&keywords=elm+programming&qid=1628706527&sr=8-2&linkCode=li3&tag=glot06-20&linkId=b6b92c7a4d93eea5aeb0db4e0711600f&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1617294047&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Elm in Action"
            }
          , { purchaseLink = "https://www.amazon.com/Programming-Elm-Maintainable-Front-End-Applications/dp/1680502859?dchild=1&keywords=elm+programming&qid=1628706527&sr=8-1&linkCode=li3&tag=glot06-20&linkId=7d4623f41b78c6f24282d9e6eb5398a2&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1680502859&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Programming Elm"
            }
          ] : List Book
        }
      , { id = "erlang"
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
          , versionCommand = "erl -version"
          }
        , books =
          [ { purchaseLink = "https://www.amazon.com/Learn-Some-Erlang-Great-Good/dp/1593274351?dchild=1&keywords=erlang&qid=1628707667&sr=8-3&linkCode=li3&tag=glot06-20&linkId=fd3bfc4884b0b89c6c0d6bb09b479223&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1593274351&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Learn You Some Erlang for Great Good!"
            }
          , { purchaseLink = "https://www.amazon.com/Introducing-Erlang-Getting-Functional-Programming/dp/1491973374?dchild=1&keywords=erlang&qid=1628707667&sr=8-4&linkCode=li3&tag=glot06-20&linkId=ee2d76d4a6f491ebe2f06dd53d22d4fb&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1491973374&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Introducing Erlang"
            }
          , { purchaseLink = "https://www.amazon.com/Designing-Scalability-Erlang-OTP-Fault-Tolerant/dp/1449320732?dchild=1&keywords=erlang&qid=1628707667&sr=8-2&linkCode=li3&tag=glot06-20&linkId=1a37cdf70112ac0970b0b6c391a0cd96&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1449320732&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Designing for Scalability with Erlang/OTP"
            }
          ] : List Book
        }
      , { id = "fsharp"
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
          , versionCommand = "fsharpc --version 2>/dev/null | head -n 1"
          }
        , books =
          [ { purchaseLink = "https://www.amazon.com/Book-Breaking-Managed-Functional-Programming-dp-1593275528/dp/1593275528?_encoding=UTF8&me=&qid=1628708061&linkCode=li3&tag=glot06-20&linkId=a24070f56054af8cc7a752b95730d371&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1593275528&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "The Book of F#"
            }
          , { purchaseLink = "https://www.amazon.com/Domain-Modeling-Made-Functional-Domain-Driven/dp/1680502549?dchild=1&keywords=f%23+programming&qid=1628708061&sr=8-1&linkCode=li3&tag=glot06-20&linkId=fc824a7033f0067ff786deaf121e0048&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1680502549&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Domain Modeling Made Functional"
            }
          , { purchaseLink = "https://www.amazon.com/Stylish-Crafting-Elegant-Functional-Code/dp/1484239997?dchild=1&keywords=f%23+programming&qid=1628708061&sr=8-3&linkCode=li3&tag=glot06-20&linkId=8ba2b90256e9cb9726771e8bb66dc38b&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1484239997&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Stylish F#"
            }
          , { purchaseLink = "https://www.amazon.com/Real-World-Functional-Programming-Tomas-Petricek/dp/1933988924?dchild=1&keywords=f%23+programming&qid=1628708061&sr=8-4&linkCode=li3&tag=glot06-20&linkId=dc5af751322ff389745c87edf23986a1&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1933988924&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Real-World Functional Programming"
            }
          ] : List Book
        }
      , { id = "go"
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
          , versionCommand = "go version"
          }
        , books =
          [ { purchaseLink = "https://www.amazon.com/Learning-Go-Idiomatic-Real-World-Programming/dp/1492077216?dchild=1&keywords=go+programming&qid=1628708371&sr=8-1&linkCode=li3&tag=glot06-20&linkId=7fe6d90c38d824f16540be09465fd618&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1492077216&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Learning Go"
            }
          , { purchaseLink = "https://www.amazon.com/Head-First-Go-Jay-McGavren/dp/1491969555?dchild=1&keywords=go+programming&qid=1628708371&sr=8-4&linkCode=li3&tag=glot06-20&linkId=5b9a056d65c48cb6d53f9092dc4bcafe&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1491969555&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Head First Go"
            }
          , { purchaseLink = "https://www.amazon.com/Programming-Language-Addison-Wesley-Professional-Computing/dp/0134190440?dchild=1&keywords=go+programming&qid=1628708371&sr=8-2&linkCode=li3&tag=glot06-20&linkId=de7587b84e0009df8d1cd000acc59464&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=0134190440&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "The Go Programming Language"
            }
          , { purchaseLink = "https://www.amazon.com/Black-Hat-Go-Programming-Pentesters/dp/1593278659?crid=6KTCNR314RYK&dchild=1&keywords=black+hat+go&qid=1628708469&sprefix=black+hat+go%2Caps%2C248&sr=8-2&linkCode=li3&tag=glot06-20&linkId=be2e9bfcef56d1ad07579f11801a6586&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1593278659&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Black Hat Go"
            }
          ] : List Book
        }
      , { id = "groovy"
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
          , versionCommand = "groovy --version"
          }
        , books =
          [ { purchaseLink = "https://www.amazon.com/Groovy-Action-Covers-2-4/dp/1935182447?dchild=1&keywords=groovy+programming&qid=1628791946&sr=8-1&linkCode=li3&tag=glot06-20&linkId=e72d94d55a8ee745816b9ff5801be120&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1935182447&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Groovy in Action"
            }
          , { purchaseLink = "https://www.amazon.com/Groovy-Domain-Specific-Languages-Fergal-Dearle-dp-1849695407/dp/1849695407?_encoding=UTF8&me=&qid=1628791946&linkCode=li3&tag=glot06-20&linkId=5525f72fa10209fc796d0151677b3f8b&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1849695407&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Groovy for Domain-specific Languages"
            }
          ] : List Book
        }
      , { id = "hare"
        , name = "Hare"
        , logoName = "hare"
        , fileExtension = "hare"
        , editorConfig =
          { defaultFilename = "main.ha"
          , mode = "ace/mode/plain_text"
          , useSoftTabs = False
          , softTabSize = 8
          , exampleCode =
              ''
              use fmt;

              export fn main() void = {
              	fmt::println("Hello World!")!;
              };
              ''
          }
        , runConfig = Some
          { containerImage = "glot/hare:latest"
          , runCommand = "hare run main.ha"
          , versionCommand = "hare version"
          }
        , books = [] : List Book
        }
      , { id = "haskell"
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
          , versionCommand = "ghc --version"
          }
        , books =
          [ { purchaseLink = "https://www.amazon.com/Learn-You-Haskell-Great-Good/dp/1593272839?crid=2XXTJEG1T25RZ&dchild=1&keywords=haskell+programming&qid=1628793572&sprefix=haskell+programming%2Caps%2C248&sr=8-2&linkCode=li3&tag=glot06-20&linkId=132e2be928817a68937b2843e46ff46a&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1593272839&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Learn You a Haskell for Great Good!"
            }
          , { purchaseLink = "https://www.amazon.com/Haskell-Very-Beginning-John-Whitington/dp/095767113X?crid=2XXTJEG1T25RZ&dchild=1&keywords=haskell+programming&qid=1628793572&sprefix=haskell+programming%2Caps%2C248&sr=8-25&linkCode=li3&tag=glot06-20&linkId=b9ef94ef794ddb57ab117ff7d463214e&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=095767113X&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Haskell from the Very Beginning"
            }
          , { purchaseLink = "https://www.amazon.com/Haskell-Depth-Vitaly-Bragilevsky/dp/161729540X?crid=2XXTJEG1T25RZ&dchild=1&keywords=haskell+programming&qid=1628793572&sprefix=haskell+programming%2Caps%2C248&sr=8-24&linkCode=li3&tag=glot06-20&linkId=00224a454cdb670de686efd4d5d75490&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=161729540X&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Haskell in Depth"
            }
          , { purchaseLink = "https://www.amazon.com/Practical-Haskell-World-Guide-Programming/dp/1484244796?crid=2XXTJEG1T25RZ&dchild=1&keywords=haskell+programming&qid=1628793572&sprefix=haskell+programming%2Caps%2C248&sr=8-6&linkCode=li3&tag=glot06-20&linkId=240eb9915a87d3b3314755a5ab2a815c&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1484244796&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Practical Haskell"
            }
          ] : List Book
        }
      , { id = "idris"
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
          , runCommand = "idris2 -o a.out --output-dir . main.idr && ./a.out"
          , versionCommand = "idris2 --version"
          }
        , books =
          [ { purchaseLink = "https://www.amazon.com/Type-driven-Development-Idris-Edwin-Brady/dp/1617293024?dchild=1&keywords=idris+programming&qid=1628794133&sr=8-9&linkCode=li3&tag=glot06-20&linkId=c4a34168c6d0e221131eb437484a2e43&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1617293024&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Type-Driven Development with Idris"
            }
          , { purchaseLink = "https://www.amazon.com/Gentle-Introduction-Dependent-Types-Idris/dp/1723139416?dchild=1&keywords=idris+programming&qid=1628794133&sr=8-1&linkCode=li3&tag=glot06-20&linkId=d430a4f633723dee453b2f2b9f44d31a&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1723139416&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Gentle Introduction to Dependent Types with Idris"
            }
          ] : List Book
        }
      , { id = "java"
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
          , versionCommand = "javac --version"
          }
        , books =
            [ { purchaseLink = "https://www.amazon.com/gp/product/0133957055/ref=as_li_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0133957055&linkCode=as2&tag=glot06-20&linkId=20f61a0cc1750b41d670ca1de188bd36"
              , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&MarketPlace=US&ASIN=0133957055&ServiceVersion=20070822&ID=AsinImage&WS=1&Format=_SL250_&tag=glot06-20"
              , bookTitle = "Starting Out with Java"
              }
            , { purchaseLink = "https://www.amazon.com/gp/product/0596009208/ref=as_li_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0596009208&linkCode=as2&tag=glot06-20&linkId=d288a782d8a00b3f48a9915d02bcee41"
              , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&MarketPlace=US&ASIN=0596009208&ServiceVersion=20070822&ID=AsinImage&WS=1&Format=_SL250_&tag=glot06-20"
              , bookTitle = "Head First Java"
              }
            ] : List Book
        }
      , { id = "javascript"
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
          , versionCommand = "node --version"
          }
        , books =
            [ { purchaseLink = "https://www.amazon.com/gp/product/1593279507/ref=as_li_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=1593279507&linkCode=as2&tag=glot06-20&linkId=2ae36f5e284f23d54c7c7cfe4fd9e71f"
              , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&MarketPlace=US&ASIN=1593279507&ServiceVersion=20070822&ID=AsinImage&WS=1&Format=_SL250_&tag=glot06-20"
              , bookTitle = "Eloquent JavaScript"
              }
            , { purchaseLink = "https://www.amazon.com/gp/product/0596517742/ref=as_li_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0596517742&linkCode=as2&tag=glot06-20&linkId=4fa53e02bddfe56560787ded1356f2f9"
              , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&MarketPlace=US&ASIN=0596517742&ServiceVersion=20070822&ID=AsinImage&WS=1&Format=_SL250_&tag=glot06-20"
              , bookTitle = "JavaScript: The Good Parts"
              }
            , { purchaseLink = "https://www.amazon.com/gp/product/1491952024/ref=as_li_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=1491952024&linkCode=as2&tag=glot06-20&linkId=c376c0df092259a46f1dc6cb1b43054b"
              , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&MarketPlace=US&ASIN=1491952024&ServiceVersion=20070822&ID=AsinImage&WS=1&Format=_SL250_&tag=glot06-20"
              , bookTitle = "JavaScript: The Definitive Guide"
              }
            , { purchaseLink = "https://www.amazon.com/gp/product/0596806752/ref=as_li_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0596806752&linkCode=as2&tag=glot06-20&linkId=b4b26f58b9c76f93741280e2d85d8fa2"
              , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&MarketPlace=US&ASIN=0596806752&ServiceVersion=20070822&ID=AsinImage&WS=1&Format=_SL250_&tag=glot06-20"
              , bookTitle = "JavaScript Patterns"
              }
            , { purchaseLink = "https://www.amazon.com/gp/product/1617296201/ref=as_li_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=1617296201&linkCode=as2&tag=glot06-20&linkId=a037b8a769b33560f278e3da18c904d5"
              , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&MarketPlace=US&ASIN=1617296201&ServiceVersion=20070822&ID=AsinImage&WS=1&Format=_SL250_&tag=glot06-20"
              , bookTitle = "Grokking Simplicity"
              }
            ] : List Book
        }
      , { id = "julia"
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
          { containerImage = "glot/julia:latest"
          , runCommand = "julia main.jl"
          , versionCommand = "julia --version"
          }
        , books =
          [ { purchaseLink = "https://www.amazon.com/Think-Julia-Like-Computer-Scientist/dp/1492045039?dchild=1&keywords=julia+programming&qid=1628794356&sr=8-2&linkCode=li3&tag=glot06-20&linkId=eaa64ef339d522e9e056efcfcaad01f1&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1492045039&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Think Julia"
            }
          , { purchaseLink = "https://www.amazon.com/Julia-1-0-Example-Adrian-Salceanu/dp/178829274X?dchild=1&keywords=julia+programming&qid=1628794356&sr=8-4&linkCode=li3&tag=glot06-20&linkId=035b09173d70c4234d20bdbf9d5ce04e&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=178829274X&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Julia Programming Projects"
            }
          , { purchaseLink = "https://www.amazon.com/Hands-Design-Patterns-Julia-comprehensive/dp/183864881X?dchild=1&keywords=julia+programming&qid=1628794356&sr=8-3&linkCode=li3&tag=glot06-20&linkId=d5fa624fcfee0e19a4bf3be8f4c7d653&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=183864881X&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Hands-On Design Patterns and Best Practices with Julia"
            }
          ] : List Book
        }
      , { id = "kotlin"
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
          , versionCommand = "kotlinc -version"
          }
        , books =
          [ { purchaseLink = "https://www.amazon.com/Kotlin-Programming-Nerd-Ranch-Guide-dp-0135161630/dp/0135161630?_encoding=UTF8&me=&qid=1628794627&linkCode=li3&tag=glot06-20&linkId=25e5e39ee5039db9c2477a3dd66a0ee6&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=0135161630&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Kotlin Programming"
            }
          , { purchaseLink = "https://www.amazon.com/Head-First-Kotlin-Brain-Friendly-Guide/dp/1491996692?dchild=1&keywords=kotlin+programming&qid=1628794627&sr=8-4&linkCode=li3&tag=glot06-20&linkId=df8b6ea02b830f94180cd3fdb6b7ad66&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1491996692&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Head First Kotlin"
            }
          , { purchaseLink = "https://www.amazon.com/Joy-Kotlin-Pierre-Yves-Saumont/dp/1617295361?pd_rd_w=E7MT6&pf_rd_p=3676f086-9496-4fd7-8490-77cf7f43f846&pf_rd_r=7WC4MJR8WEG3AM93ZZRY&pd_rd_r=78e84c17-1e21-4282-b716-4673287f2250&pd_rd_wg=vaEY0&pd_rd_i=1617295361&psc=1&linkCode=li3&tag=glot06-20&linkId=5bbc4f7d833773f54713fd5d7d70d071&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1617295361&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "The Joy of Kotlin"
            }
          , { purchaseLink = "https://www.amazon.com/Kotlin-Action-Dmitry-Jemerov/dp/1617293296?pd_rd_w=E7MT6&pf_rd_p=3676f086-9496-4fd7-8490-77cf7f43f846&pf_rd_r=7WC4MJR8WEG3AM93ZZRY&pd_rd_r=78e84c17-1e21-4282-b716-4673287f2250&pd_rd_wg=vaEY0&pd_rd_i=1617293296&psc=1&linkCode=li3&tag=glot06-20&linkId=e7e04f4a0a8c781d4b1d06e51250a101&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1617293296&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Kotlin in Action"
            }
          ] : List Book
        }
      , { id = "lua"
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
          { containerImage = "glot/lua:latest"
          , runCommand = "lua main.lua"
          , versionCommand = "lua -v"
          }
        , books = 
          [ { purchaseLink = "https://www.amazon.com/Programming-Lua-Fourth-Roberto-Ierusalimschy/dp/8590379868?dchild=1&keywords=lua+programming&qid=1628798275&sr=8-1&linkCode=li3&tag=glot06-20&linkId=4481a5b6ec6ec9e24399112aa76a911e&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=8590379868&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Programming in Lua"
            }
          , { purchaseLink = "https://www.amazon.com/Lua-Quick-Start-Guide-programming/dp/1789343224?dchild=1&keywords=lua+programming&qid=1628798275&sr=8-3&linkCode=li3&tag=glot06-20&linkId=50ea4bcd4aa1288bf4df3d0f41bee530&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1789343224&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Lua Quick Start Guide"
            }
          , { purchaseLink = "https://www.amazon.com/Coding-Roblox-Games-Made-Easy/dp/1800561997?dchild=1&keywords=lua+programming&qid=1628798275&sr=8-2&linkCode=li3&tag=glot06-20&linkId=d82b25452a369e67ed4e854d8f85f24c&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1800561997&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Coding Roblox Games Made Easy"
            }
          ] : List Book
        }
      , { id = "mercury"
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
          , versionCommand = "mmc --version | head -n 1"
          }
        , books =
          [
          ] : List Book
        }
      , { id = "nim"
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
          , versionCommand = "nim --version | head -n 1"
          }
        , books =
          [ { purchaseLink = "https://www.amazon.com/Nim-Action-Dominik-Picheta/dp/1617293431?dchild=1&keywords=nim+programming&qid=1628796010&sr=8-1&linkCode=li3&tag=glot06-20&linkId=590c24fdaaf5685cbc35123f1830dc81&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1617293431&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Nim in Action"
            }
          ] : List Book
        }
      , { id = "nix"
        , name = "Nix"
        , logoName = "nix"
        , fileExtension = "nix"
        , editorConfig =
          { defaultFilename = "main.nix"
          , mode = "ace/mode/nix"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode =
            ''
            let
                hello = "Hello World!";
            in
            hello''
          }
        , runConfig = Some
          { containerImage = "glot/nix:latest"
          , runCommand = "nix-instantiate --eval main.nix"
          , versionCommand = "nix --version"
          }
        , books = [] : List Book
        }
      , { id = "ocaml"
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
          , versionCommand = "ocamlc --version"
          }
        , books =
          [ { purchaseLink = "https://www.amazon.com/OCaml-Very-Beginning-John-Whitington/dp/0957671105?dchild=1&keywords=ocaml+programming&qid=1628796089&sr=8-2&linkCode=li3&tag=glot06-20&linkId=634e3d2d801abff055b6cd37ed22b348&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=0957671105&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "OCaml from the Very Beginning"
            }
          , { purchaseLink = "https://www.amazon.com/Real-World-OCaml-Functional-programming/dp/144932391X?dchild=1&keywords=ocaml+programming&qid=1628796089&sr=8-3&linkCode=li3&tag=glot06-20&linkId=c1cfd59cb45caf05e24dc5702b284650&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=144932391X&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Real World OCaml"
            }
          ] : List Book
        }
      , { id = "perl"
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
          { containerImage = "glot/perl:latest"
          , runCommand = "perl main.pl"
          , versionCommand = "perl --version | head -n 2 | tail -n 1"
          }
        , books =
          [
            { purchaseLink = "https://www.amazon.com/Modern-Perl-chromatic/dp/1680500880?dchild=1&keywords=perl+programming&qid=1628796261&sr=8-4&linkCode=li3&tag=glot06-20&linkId=eae8370312860449bd63e03a9cdbd4b5&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1680500880&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Modern Perl"
            }
          , { purchaseLink = "https://www.amazon.com/Programming-Perl-Unmatched-processing-scripting/dp/0596004923?dchild=1&keywords=perl+programming&qid=1628796261&sr=8-1&linkCode=li3&tag=glot06-20&linkId=a76ba78bc8637be88fe56387326b51a0&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=0596004923&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Programming Perl"
            }
          ] : List Book
        }
      , { id = "php"
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

              echo "Hello World!\n";''
          }
        , runConfig = Some
          { containerImage = "glot/php:latest"
          , runCommand = "php main.php"
          , versionCommand = "php --version | head -n 1"
          }
        , books =
          [ { purchaseLink = "https://www.amazon.com/Programming-PHP-Creating-Dynamic-Pages/dp/1492054135?dchild=1&keywords=php+programming&qid=1628796402&sr=8-1&linkCode=li3&tag=glot06-20&linkId=e561bea66f79cacd6d211e4bde676e3a&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1492054135&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Programming PHP"
            }
          , { purchaseLink = "https://www.amazon.com/Murachs-PHP-MySQL-Joel-Murach/dp/1943872384?_encoding=UTF8&psc=1&refRID=RYPZQA4XQ3K6S4WEJ5AY&linkCode=li3&tag=glot06-20&linkId=404f73e50197f5500aa724e46ec52028&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1943872384&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Murach's PHP and MySQL"
            }
          ] : List Book
        }
      , { id = "python"
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
          , versionCommand = "python --version"
          }
        , books =
            [ { purchaseLink = "https://www.amazon.com/gp/product/1593279280/ref=as_li_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=1593279280&linkCode=as2&tag=glot06-20&linkId=b93c6d5810ce111c2137299b3498c3c2"
              , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&MarketPlace=US&ASIN=1593279280&ServiceVersion=20070822&ID=AsinImage&WS=1&Format=_SL250_&tag=glot06-20"
              , bookTitle = "Python Crash Course"
              }
            , { purchaseLink = "https://www.amazon.com/gp/product/1449355730/ref=as_li_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=1449355730&linkCode=as2&tag=glot06-20&linkId=ed62041c275a4298598cd3be6d38c0f9"
              , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&MarketPlace=US&ASIN=1449355730&ServiceVersion=20070822&ID=AsinImage&WS=1&Format=_SL250_&tag=glot06-20"
              , bookTitle = "Learning Python"
              }
            ] : List Book
        }
      , { id = "raku"
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
          { containerImage = "glot/raku:latest"
          , runCommand = "raku main.raku"
          , versionCommand = "raku --version | head -n 1"
          }
        , books =
          [ { purchaseLink = "https://www.amazon.com/Learning-Perl-Keeping-Impossible-Within/dp/149197768X?dchild=1&keywords=raku+programming&qid=1628796842&s=books&sr=1-3&linkCode=li3&tag=glot06-20&linkId=eeaaa6de0b55ca2ac9e5d5e2108e8afc&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=149197768X&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Learning Perl 6"
            }
          , { purchaseLink = "https://www.amazon.com/Raku-Fundamentals-Examples-Projects-Studies/dp/1484261089?dchild=1&keywords=raku+programming&qid=1628796842&s=books&sr=1-4&linkCode=li3&tag=glot06-20&linkId=8eee6aa95abbddecc981681449d1f6f3&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1484261089&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Raku Fundamentals"
            }
          , { purchaseLink = "https://www.amazon.com/Raku-Recipes-Problem-Solution-J-J-Merelo/dp/1484262573?dchild=1&keywords=raku+programming&qid=1628796842&s=books&sr=1-2&linkCode=li3&tag=glot06-20&linkId=88957ebd308e1712f2e87f52cffcff50&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1484262573&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Raku Recipes"
            }
          ] : List Book
        }
      , { id = "ruby"
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
          { containerImage = "glot/ruby:latest"
          , runCommand = "ruby main.rb"
          , versionCommand = "ruby --version"
          }
        , books =
          [ { purchaseLink = "https://www.amazon.com/Well-Grounded-Rubyist-David-Black/dp/1617295213?dchild=1&keywords=ruby+programming&qid=1628797009&sr=8-4&linkCode=li3&tag=glot06-20&linkId=e85f7916dca0153c1e57c5dfbcde4639&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1617295213&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "The Well-Grounded Rubyist"
            }
          , { purchaseLink = "https://www.amazon.com/Beginning-Ruby-3-Beginner-Pro/dp/1484263235?dchild=1&keywords=ruby+programming&qid=1628797009&sr=8-3&linkCode=li3&tag=glot06-20&linkId=63bbca1f489a31fa1259801fabba62d5&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1484263235&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Beginning Ruby 3"
            }
          , { purchaseLink = "https://www.amazon.com/Head-First-Ruby-Brain-Friendly-Guide/dp/1449372651?dchild=1&keywords=ruby+programming&qid=1628797009&sr=8-9&linkCode=li3&tag=glot06-20&linkId=423fa098006c415181df5eb88a742e5f&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1449372651&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Head First Ruby"
            }
          ] : List Book
        }
      , { id = "rust"
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
          , versionCommand = "rustc --version"
          }
        , books =
          [ { purchaseLink = "https://www.amazon.com/Rust-Programming-Language-Covers-2018/dp/1718500440?dchild=1&keywords=rust+programming&qid=1628797278&sr=8-2&linkCode=li3&tag=glot06-20&linkId=01ca357d12026b1921c0279402c3c6de&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1718500440&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "The Rust Programming Language"
            }
          , { purchaseLink = "https://www.amazon.com/Programming-Rust-Fast-Systems-Development/dp/1492052590?dchild=1&keywords=rust+programming&qid=1628797278&sr=8-1&linkCode=li3&tag=glot06-20&linkId=a11b2841a6ef9e1d428265cb8dd086a5&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1492052590&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Programming Rust"
            }
          , { purchaseLink = "https://www.amazon.com/Rust-Rustaceans-Programming-Experienced-Developers-dp-1718501854/dp/1718501854?_encoding=UTF8&me=&qid=1628797278&linkCode=li3&tag=glot06-20&linkId=766c196896bdc58c3fdca822b514735e&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1718501854&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Rust for Rustaceans"
            }
          , { purchaseLink = "https://www.amazon.com/Programming-WebAssembly-Rust-Development-Applications/dp/1680506366?dchild=1&keywords=rust+programming&qid=1628797278&sr=8-13&linkCode=li3&tag=glot06-20&linkId=c27d7e395468b0ffe17b2fe2fe81fee2&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1680506366&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Programming WebAssembly with Rust"
            }
          ] : List Book
        }
      , { id = "sac"
        , name = "SaC"
        , logoName = "sac"
        , fileExtension = "sac"
        , editorConfig =
          { defaultFilename = "main.sac"
          , mode = "ace/mode/sac"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode =
            ''
            int main () {
                StdIO::printf ("Hello World!");
                return 0;
            }''
          }
        , runConfig = Some
          { containerImage = "glot/sac:latest"
          , runCommand = "sac2c -t seq -o a.out main.sac && ./a.out"
          , versionCommand = "sac2c -V | head -n 1"
          }
        , books = [] : List Book
        }
      , { id = "scala"
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
          , versionCommand = "scalac --version"
          }
        , books =
          [ { purchaseLink = "https://www.amazon.com/Programming-Scala-Fifth-Odersky/dp/0997148004?dchild=1&keywords=scala+programming&qid=1628797515&sr=8-1&linkCode=li3&tag=glot06-20&linkId=5b2b1fa15a903f9544b00a32ffee3a5e&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=0997148004&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Programming in Scala"
            }
          , { purchaseLink = "https://www.amazon.com/Programming-Scala-Scalability-Functional-Objects/dp/1492077895?dchild=1&keywords=scala+programming&qid=1628797515&sr=8-2&linkCode=li3&tag=glot06-20&linkId=798154dc10418594e743e7c01c1ebde2&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1492077895&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Programming Scala"
            }
          , { purchaseLink = "https://www.amazon.com/Hands-Scala-Programming-Practical-Project-Based/dp/9811456933?dchild=1&keywords=scala+programming&qid=1628797515&sr=8-4&linkCode=li3&tag=glot06-20&linkId=4302476ff2d1a2e994164aec20a256b8&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=9811456933&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Hands-on Scala Programming"
            }
          ] : List Book
        }
      , { id = "swift"
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
          , versionCommand = "swift --version | head -n 1"
          }
        , books =
          [ { purchaseLink = "https://www.amazon.com/Swift-Programming-Ranch-Guide-Guides/dp/0135264200?dchild=1&keywords=swift+programming&qid=1628797723&sr=8-1&linkCode=li3&tag=glot06-20&linkId=bdeaf1be97d16725c840525b75eaf813&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=0135264200&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Swift Programming"
            }
          , { purchaseLink = "https://www.amazon.com/iOS-14-Programming-Beginners-building/dp/1800209746?dchild=1&keywords=swift+programming&qid=1628797723&sr=8-2&linkCode=li3&tag=glot06-20&linkId=412065324bace2d5899dea57b29a8b7e&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1800209746&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "iOS 14 Programming for Beginners"
            }
          , { purchaseLink = "https://www.amazon.com/Swift-Depth-Tjeerd-t-Veen/dp/1617295183?dchild=1&keywords=swift+programming&qid=1628797723&sr=8-6&linkCode=li3&tag=glot06-20&linkId=eee408e75f2e0db5c02671e57283352f&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1617295183&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Swift in Depth"
            }
          ] : List Book
        }
      , { id = "typescript"
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
          , versionCommand = "tsc --version"
          }
        , books =
          [ { purchaseLink = "https://www.amazon.com/Programming-TypeScript-Making-JavaScript-Applications/dp/1492037656?dchild=1&keywords=typescript+programming&qid=1628797879&sr=8-1&linkCode=li3&tag=glot06-20&linkId=9074045b5b2933f9e857434dbf4b241a&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1492037656&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Programming TypeScript"
            }
          , { purchaseLink = "https://www.amazon.com/Essential-TypeScript-Beginner-Adam-Freeman/dp/148424978X?dchild=1&keywords=typescript+programming&qid=1628797879&sr=8-3&linkCode=li3&tag=glot06-20&linkId=d316caeacd6efdc8b4bca5c2fac9f080&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=148424978X&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Essential TypeScript"
            }
          , { purchaseLink = "https://www.amazon.com/Effective-TypeScript-Specific-Ways-Improve/dp/1492053740?dchild=1&keywords=typescript+programming&qid=1628797879&sr=8-2&linkCode=li3&tag=glot06-20&linkId=52e058d7cd385b55e641dfeb659b3725&language=en_US&ref_=as_li_ss_il"
            , imgLink = "https://ws-na.amazon-adsystem.com/widgets/q?_encoding=UTF8&ASIN=1492053740&Format=_SL250_&ID=AsinImage&MarketPlace=US&ServiceVersion=20070822&WS=1&tag=glot06-20&language=en_US"
            , bookTitle = "Effective TypeScript"
            }
          ] : List Book
        }
      , { id = "zig"
        , name = "Zig"
        , logoName = "zig"
        , fileExtension = "zig"
        , editorConfig =
          { defaultFilename = "main.zig"
          , mode = "ace/mode/plain_text"
          , useSoftTabs = True
          , softTabSize = 4
          , exampleCode =
              ''
              const std = @import("std");

              pub fn main() !void {
                  const stdout = std.io.getStdOut().writer();
                  try stdout.print("{s}\n", .{"Hello World!"});
              }''
          }
        , runConfig = Some
          { containerImage = "glot/zig:latest"
          , runCommand = "zig run main.zig"
          , versionCommand = "zig version"
          }
        , books = [] : List Book
        }
      , { id = "plaintext"
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
        , books = [] : List Book
        }
      ]
    : List Language
