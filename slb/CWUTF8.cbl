       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWUTF8.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  07/02/2008.
       SECURITY.      *************************************************
                      *                                               *
                      *  Grava na codifica��o UTF-8                   *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT BMPWORK ASSIGN TO DISK
                  ORGANIZATION  IS BINARY SEQUENTIAL
                  LOCK MODE     IS EXCLUSIVE.
      *           FILE STATUS   IS FS.

      $Set IdxFormat"14" DataCompress"1" KeyCompress"7"
           SELECT OBJETOS ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS OBJETOS-FILE
                  LOCK MODE     IS EXCLUSIVE
                  RESERVE NO ALTERNATE AREA
                  FILE STATUS   IS FS-OBJETOS.

       DATA DIVISION.
       FILE SECTION.

       FD  BMPWORK
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS CWUTF8-FILE.

       01  BMPWORK-REG               PIC X(001).

       FD  OBJETOS
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-OBJETOS.

       01  OBJETOS-REG.
           05 OBJETOS-FILE        PIC  X(255).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 ER-OBJETOS.
              10 FS-OBJETOS       PIC X(002) VALUE "00".
              10 LB-OBJETOS       PIC X(255) VALUE "$TEMP/cwutf8###".
           05 UTF8-FILE           PIC X(255) VALUE SPACES.
           05 BIN          COMP-X PIC 9(002) VALUE 0.
           05 NUM                 PIC 9(003) VALUE 0.
           05 I PIC 9(4) VALUE 0.
           05 T PIC 9(4) VALUE 0.
           05 U PIC 9(1) VALUE 0.
           05 TABELA.
              06 PIC X(3) VALUE SPACE.
              06 PIC X(3) VALUE "☺".
              06 PIC X(3) VALUE "☻".
              06 PIC X(3) VALUE "♥".
              06 PIC X(3) VALUE "♦".
              06 PIC X(3) VALUE "♣".
              06 PIC X(3) VALUE "♠".
              06 PIC X(3) VALUE "•".
              06 PIC X(3) VALUE "◘".
              06 PIC X(3) VALUE "○".
              06 PIC X(3) VALUE "◙".
              06 PIC X(3) VALUE "♂".
              06 PIC X(3) VALUE "♀".
              06 PIC X(3) VALUE "♪".
              06 PIC X(3) VALUE "♫".
              06 PIC X(3) VALUE "☼".
              06 PIC X(3) VALUE "►".
              06 PIC X(3) VALUE "◄".
              06 PIC X(3) VALUE "↕".
              06 PIC X(3) VALUE "‼".
              06 PIC X(3) VALUE "¶".
              06 PIC X(3) VALUE "§".
              06 PIC X(3) VALUE "▬".
              06 PIC X(3) VALUE "↨".
              06 PIC X(3) VALUE "↑".
              06 PIC X(3) VALUE "↓".
              06 PIC X(3) VALUE "→".
              06 PIC X(3) VALUE "←".
              06 PIC X(3) VALUE "∟".
              06 PIC X(3) VALUE "↔".
              06 PIC X(3) VALUE "▲".
              06 PIC X(3) VALUE "▼".
              06 PIC X(3) VALUE SPACE.
              06 PIC X(3) VALUE "!".
              06 PIC X(3) VALUE '"'.
              06 PIC X(3) VALUE "#".
              06 PIC X(3) VALUE "$".
              06 PIC X(3) VALUE "%".
              06 PIC X(3) VALUE "&".
              06 PIC X(3) VALUE "'".
              06 PIC X(3) VALUE "(".
              06 PIC X(3) VALUE ")".
              06 PIC X(3) VALUE "*".
              06 PIC X(3) VALUE "+".
              06 PIC X(3) VALUE ",".
              06 PIC X(3) VALUE "-".
              06 PIC X(3) VALUE ".".
              06 PIC X(3) VALUE "/".
              06 PIC X(3) VALUE "0".
              06 PIC X(3) VALUE "1".
              06 PIC X(3) VALUE "2".
              06 PIC X(3) VALUE "3".
              06 PIC X(3) VALUE "4".
              06 PIC X(3) VALUE "5".
              06 PIC X(3) VALUE "6".
              06 PIC X(3) VALUE "7".
              06 PIC X(3) VALUE "8".
              06 PIC X(3) VALUE "9".
              06 PIC X(3) VALUE ":".
              06 PIC X(3) VALUE ";".
              06 PIC X(3) VALUE "<".
              06 PIC X(3) VALUE "=".
              06 PIC X(3) VALUE ">".
              06 PIC X(3) VALUE "?".
              06 PIC X(3) VALUE "@".
              06 PIC X(3) VALUE "A".
              06 PIC X(3) VALUE "B".
              06 PIC X(3) VALUE "C".
              06 PIC X(3) VALUE "D".
              06 PIC X(3) VALUE "E".
              06 PIC X(3) VALUE "F".
              06 PIC X(3) VALUE "G".
              06 PIC X(3) VALUE "H".
              06 PIC X(3) VALUE "I".
              06 PIC X(3) VALUE "J".
              06 PIC X(3) VALUE "K".
              06 PIC X(3) VALUE "L".
              06 PIC X(3) VALUE "M".
              06 PIC X(3) VALUE "N".
              06 PIC X(3) VALUE "O".
              06 PIC X(3) VALUE "P".
              06 PIC X(3) VALUE "Q".
              06 PIC X(3) VALUE "R".
              06 PIC X(3) VALUE "S".
              06 PIC X(3) VALUE "T".
              06 PIC X(3) VALUE "U".
              06 PIC X(3) VALUE "V".
              06 PIC X(3) VALUE "W".
              06 PIC X(3) VALUE "X".
              06 PIC X(3) VALUE "Y".
              06 PIC X(3) VALUE "Z".
              06 PIC X(3) VALUE "[".
              06 PIC X(3) VALUE "\".
              06 PIC X(3) VALUE "]".
              06 PIC X(3) VALUE "^".
              06 PIC X(3) VALUE "_".
              06 PIC X(3) VALUE "`".
              06 PIC X(3) VALUE "a".
              06 PIC X(3) VALUE "b".
              06 PIC X(3) VALUE "c".
              06 PIC X(3) VALUE "d".
              06 PIC X(3) VALUE "e".
              06 PIC X(3) VALUE "f".
              06 PIC X(3) VALUE "g".
              06 PIC X(3) VALUE "h".
              06 PIC X(3) VALUE "i".
              06 PIC X(3) VALUE "j".
              06 PIC X(3) VALUE "k".
              06 PIC X(3) VALUE "l".
              06 PIC X(3) VALUE "m".
              06 PIC X(3) VALUE "n".
              06 PIC X(3) VALUE "o".
              06 PIC X(3) VALUE "p".
              06 PIC X(3) VALUE "q".
              06 PIC X(3) VALUE "r".
              06 PIC X(3) VALUE "s".
              06 PIC X(3) VALUE "t".
              06 PIC X(3) VALUE "u".
              06 PIC X(3) VALUE "v".
              06 PIC X(3) VALUE "w".
              06 PIC X(3) VALUE "x".
              06 PIC X(3) VALUE "y".
              06 PIC X(3) VALUE "z".
              06 PIC X(3) VALUE "{".
              06 PIC X(3) VALUE "|".
              06 PIC X(3) VALUE "}".
              06 PIC X(3) VALUE "~".
              06 PIC X(3) VALUE "⌂".
              06 PIC X(3) VALUE "Ç".
              06 PIC X(3) VALUE "ü".
              06 PIC X(3) VALUE "é".
              06 PIC X(3) VALUE "â".
              06 PIC X(3) VALUE "ä".
              06 PIC X(3) VALUE "à".
              06 PIC X(3) VALUE "å".
              06 PIC X(3) VALUE "ç".
              06 PIC X(3) VALUE "ê".
              06 PIC X(3) VALUE "ë".
              06 PIC X(3) VALUE "è".
              06 PIC X(3) VALUE "ï".
              06 PIC X(3) VALUE "î".
              06 PIC X(3) VALUE "ì".
              06 PIC X(3) VALUE "Ä".
              06 PIC X(3) VALUE "Å".
              06 PIC X(3) VALUE "É".
              06 PIC X(3) VALUE "æ".
              06 PIC X(3) VALUE "Æ".
              06 PIC X(3) VALUE "ô".
              06 PIC X(3) VALUE "ö".
              06 PIC X(3) VALUE "ò".
              06 PIC X(3) VALUE "û".
              06 PIC X(3) VALUE "ù".
              06 PIC X(3) VALUE "ÿ".
              06 PIC X(3) VALUE "Ö".
              06 PIC X(3) VALUE "Ü".
              06 PIC X(3) VALUE "ø".
              06 PIC X(3) VALUE "£".
              06 PIC X(3) VALUE "Ø".
              06 PIC X(3) VALUE "×".
              06 PIC X(3) VALUE "ƒ".
              06 PIC X(3) VALUE "á".
              06 PIC X(3) VALUE "í".
              06 PIC X(3) VALUE "ó".
              06 PIC X(3) VALUE "ú".
              06 PIC X(3) VALUE "ñ".
              06 PIC X(3) VALUE "Ñ".
              06 PIC X(3) VALUE "ª".
              06 PIC X(3) VALUE "º".
              06 PIC X(3) VALUE "¿".
              06 PIC X(3) VALUE "®".
              06 PIC X(3) VALUE "¬".
              06 PIC X(3) VALUE "½".
              06 PIC X(3) VALUE "¼".
              06 PIC X(3) VALUE "¡".
              06 PIC X(3) VALUE "«".
              06 PIC X(3) VALUE "»".
              06 PIC X(3) VALUE "░".
              06 PIC X(3) VALUE "▒".
              06 PIC X(3) VALUE "▓".
              06 PIC X(3) VALUE "│".
              06 PIC X(3) VALUE "┤".
              06 PIC X(3) VALUE "Á".
              06 PIC X(3) VALUE "Â".
              06 PIC X(3) VALUE "À".
              06 PIC X(3) VALUE "©".
              06 PIC X(3) VALUE "╣".
              06 PIC X(3) VALUE "║".
              06 PIC X(3) VALUE "╗".
              06 PIC X(3) VALUE "╝".
              06 PIC X(3) VALUE "¢".
              06 PIC X(3) VALUE "¥".
              06 PIC X(3) VALUE "┐".
              06 PIC X(3) VALUE "└".
              06 PIC X(3) VALUE "┴".
              06 PIC X(3) VALUE "┬".
              06 PIC X(3) VALUE "├".
              06 PIC X(3) VALUE "─".
              06 PIC X(3) VALUE "┼".
              06 PIC X(3) VALUE "ã".
              06 PIC X(3) VALUE "Ã".
              06 PIC X(3) VALUE "╚".
              06 PIC X(3) VALUE "╔".
              06 PIC X(3) VALUE "╩".
              06 PIC X(3) VALUE "╦".
              06 PIC X(3) VALUE "╠".
              06 PIC X(3) VALUE "═".
              06 PIC X(3) VALUE "╬".
              06 PIC X(3) VALUE "¤".
              06 PIC X(3) VALUE "ð".
              06 PIC X(3) VALUE "Ð".
              06 PIC X(3) VALUE "Ê".
              06 PIC X(3) VALUE "Ë".
              06 PIC X(3) VALUE "È".
              06 PIC X(3) VALUE "ı".
              06 PIC X(3) VALUE "Í".
              06 PIC X(3) VALUE "Î".
              06 PIC X(3) VALUE "Ï".
              06 PIC X(3) VALUE "┘".
              06 PIC X(3) VALUE "┌".
              06 PIC X(3) VALUE "█".
              06 PIC X(3) VALUE "▄".
              06 PIC X(3) VALUE "¦".
              06 PIC X(3) VALUE "Ì".
              06 PIC X(3) VALUE "▀".
              06 PIC X(3) VALUE "Ó".
              06 PIC X(3) VALUE "ß".
              06 PIC X(3) VALUE "Ô".
              06 PIC X(3) VALUE "Ò".
              06 PIC X(3) VALUE "õ".
              06 PIC X(3) VALUE "Õ".
              06 PIC X(3) VALUE "µ".
              06 PIC X(3) VALUE "þ".
              06 PIC X(3) VALUE "Þ".
              06 PIC X(3) VALUE "Ú".
              06 PIC X(3) VALUE "Û".
              06 PIC X(3) VALUE "Ù".
              06 PIC X(3) VALUE "ý".
              06 PIC X(3) VALUE "Ý".
              06 PIC X(3) VALUE "¯".
              06 PIC X(3) VALUE "´".
              06 PIC X(3) VALUE "­".
              06 PIC X(3) VALUE "±".
              06 PIC X(3) VALUE "‗".
              06 PIC X(3) VALUE "¾".
              06 PIC X(3) VALUE "¶".
              06 PIC X(3) VALUE "§".
              06 PIC X(3) VALUE "÷".
              06 PIC X(3) VALUE "¸".
              06 PIC X(3) VALUE "°".
              06 PIC X(3) VALUE "¨".
              06 PIC X(3) VALUE "·".
              06 PIC X(3) VALUE "¹".
              06 PIC X(3) VALUE "³".
              06 PIC X(3) VALUE "²".
              06 PIC X(3) VALUE "■".
              06 PIC X(3) VALUE " ".
           05 REDEFINES TABELA.
              06 STRING-UTF8 OCCURS 256 PIC X(3).

       COPY CWUNIX.

       LINKAGE SECTION.

       COPY CWUTF8.

       PROCEDURE DIVISION USING PARAMETROS-CWUTF8.

       000-INICIO.

           ON  1
               DISPLAY "CWSAVE" UPON ENVIRONMENT-NAME
               DISPLAY "ON"     UPON ENVIRONMENT-VALUE
               CALL "CWUNIX" USING PARAMETROS-CWUNIX
               ON EXCEPTION CONTINUE END-CALL
               MOVE SPACES TO OBJETOS-FILE
               OPEN I-O OBJETOS.

           IF  CWUTF8-OLDFILE NOT = SPACES
               MOVE CWUTF8-OLDFILE TO UTF8-FILE
           ELSE
               MOVE CWUTF8-FILE    TO UTF8-FILE
           END-IF

           IF  UTF8-FILE = SPACES
               GOBACK
           END-IF

           IF  CWUTF8-CLOSE
               IF  OBJETOS-FILE NOT = SPACES
                   CLOSE BMPWORK
               END-IF
               MOVE UTF8-FILE TO OBJETOS-FILE
               READ OBJETOS
               DELETE OBJETOS RECORD
               MOVE SPACES TO OBJETOS-FILE
               GOBACK
           END-IF

           IF   UTF8-FILE NOT = OBJETOS-FILE
                IF  OBJETOS-FILE NOT = SPACES
                    CLOSE BMPWORK
                    MOVE SPACES TO OBJETOS-FILE
                END-IF
                MOVE UTF8-FILE TO OBJETOS-FILE
                READ OBJETOS
                IF  FS-OBJETOS = '23'
                    WRITE OBJETOS-REG
                    OPEN OUTPUT BMPWORK
                    WRITE BMPWORK-REG FROM X"EF"
                    WRITE BMPWORK-REG FROM X"BB"
                    WRITE BMPWORK-REG FROM X"BF"
                    CLOSE BMPWORK
                END-IF
                OPEN EXTEND BMPWORK
           END-IF

           PERFORM VARYING T FROM LENGTH OF CWUTF8-RECORD
                               BY -1
                            UNTIL T = 0
                               OR CWUTF8-RECORD(T: 1) <> SPACE
                    CONTINUE
           END-PERFORM
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > T
                   PERFORM 010-GRAVA-STRING THRU 010-99-FIM
           END-PERFORM
           IF   CWUNIX-OFF
                WRITE BMPWORK-REG FROM X"0D"
           END-IF
           WRITE BMPWORK-REG FROM X"0A".

       000-99-FIM. GOBACK.

       010-GRAVA-STRING.

           IF  CWUTF8-RECORD (I: 1) = X"00"
               CONTINUE
           ELSE
               IF  CWUTF8-RECORD (I: 1) = SPACE
                   WRITE BMPWORK-REG FROM SPACE
               ELSE
                   MOVE CWUTF8-RECORD (I: 1) TO BIN(1:1)
                   COMPUTE NUM = BIN + 1
                   PERFORM VARYING U FROM 1 BY 1
                             UNTIL U > 3
                                OR STRING-UTF8(NUM)(U:1) = SPACE
                           WRITE BMPWORK-REG FROM STRING-UTF8(NUM)(U:1)
                   END-PERFORM
               END-IF
            END-IF.

       010-99-FIM. EXIT.
       END PROGRAM CWUTF8.
