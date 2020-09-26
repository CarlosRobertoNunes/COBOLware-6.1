       IDENTIFICATION DIVISION.
       PROGRAM-ID.    FROMUTF8.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  02/06/2017.
       SECURITY.      *************************************************
                      *                                               *
                      * Converte codifica��o UTF-8 Para ASCII         *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 FROMUTF8            PIC X(5).
           05 BUFFER              PIC X(32768).
           05 B            COMP-5 PIC S9(4) VALUE 0.
           05 I            COMP-5 PIC S9(4) VALUE 0.
           05 T            COMP-5 PIC S9(4) VALUE 0..
           05 T-CHAR       COMP-X PIC 9(002) VALUE 0.
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

       LINKAGE SECTION.

       01  CAMPO           PIC X.
       01  TAMANHO         PIC 9(008) COMP-X.

       PROCEDURE DIVISION USING CAMPO TAMANHO.

       000-INICIO.

           ON 1
               DISPLAY 'FROMUTF8' UPON ENVIRONMENT-NAME
               ACCEPT   FROMUTF8  FROM ENVIRONMENT-VALUE
               INSPECT FROMUTF8 CONVERTING 'onf' TO 'ONF'.

           IF FROMUTF8 = 'ON'
              MOVE SPACES TO BUFFER
              MOVE 0      TO B
              INSPECT CAMPO (1:TAMANHO) CONVERTING LOW-VALUES TO SPACES
              IF CAMPO(1:TAMANHO) = SPACES
                 GOBACK
              END-IF
              PERFORM VARYING I FROM 1 BY 1 UNTIL I > TAMANHO
                  ADD  1 TO B
                  IF  CAMPO(I:1) = SPACE
                      EXIT PERFORM CYCLE
                  END-IF
                  MOVE CAMPO(I:1) TO BUFFER(B:1)
                  PERFORM VARYING T FROM 1 BY 1 UNTIL T > 256
                       IF  CAMPO (I:1) NOT = SPACE
                           IF  (I + 2) NOT > TAMANHO
                           AND (STRING-UTF8(T)(2:1) NOT = SPACE)
                           AND (STRING-UTF8(T)(3:1) NOT = SPACE)
                           AND CAMPO (I:3) = STRING-UTF8(T)(1:3)
                               COMPUTE T-CHAR = T - 1
                               MOVE T-CHAR(1:1) TO BUFFER(B:1)
                               ADD 2 TO I
                               EXIT PERFORM
                           END-IF
                       END-IF
                  END-PERFORM
                  IF T > 256
                     PERFORM VARYING T FROM 1 BY 1 UNTIL T > 256
                          MOVE CAMPO(I:1) TO BUFFER(B:1)
                          IF  CAMPO (I:1) NOT = SPACE
                              IF  (I + 1) NOT > TAMANHO
                              AND (STRING-UTF8(T)(2:1) NOT = SPACE)
                              AND CAMPO (I:2) = STRING-UTF8(T)(1:2)
                                  COMPUTE T-CHAR = T - 1
                                  MOVE T-CHAR(1:1) TO BUFFER(B:1)
                                  ADD 1 TO I
                                  EXIT PERFORM
                              END-IF
                          END-IF
                     END-PERFORM
                  END-IF
              END-PERFORM
              MOVE BUFFER (1: TAMANHO) TO CAMPO (1: TAMANHO)
           END-IF.

       000-99-FIM. GOBACK.

       END PROGRAM FROMUTF8.
