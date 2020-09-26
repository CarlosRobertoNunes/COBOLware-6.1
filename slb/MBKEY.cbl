       IDENTIFICATION DIVISION.
       PROGRAM-ID.    MBKEY.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  19/05/2009.
       SECURITY.      *************************************************
                      *                                               *
                      * ConversÆo de c¢digos de tecla do padrÆo       *
                      * CRT-STATUS Microsoft para ESCAPE KEY Microbase*
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 I                   PIC 9(001) VALUE 0.
           05 Y                   PIC 9(001) VALUE 0.
           05 KEY-STATUS.
              06 KEY-TYPE         PIC X.
              06 KEY-CODE-1       PIC 9(2) COMP-X.
              06 KEY-CODE-2       PIC 9(2) COMP-X.
          05  TECLA               PIC 9(003).
              88 ENTR                                VALUE 00.
              88 ESC                                 VALUE 01.
              88 F1                                  VALUE 01.
              88 F2                                  VALUE 02.
              88 F3                                  VALUE 03.
              88 F4                                  VALUE 04.
              88 F5                                  VALUE 05.
              88 F6                                  VALUE 06.
              88 F7                                  VALUE 07.
              88 F8                                  VALUE 08.
              88 F9                                  VALUE 09.
              88 F10                                 VALUE 10.
              88 F99                                 VALUE 99.
              88 HELP                                VALUE 11.
              88 F11                                 VALUE 11.
              88 F12                                 VALUE 12.
              88 F13                                 VALUE 13.
              88 F14                                 VALUE 14.
              88 F15                                 VALUE 15.
              88 F16                                 VALUE 16.
              88 CIMA                                VALUE 17.
              88 BAIXO                               VALUE 18.
              88 PGUP                                VALUE 19.
              88 PGDW                                VALUE 20.

       LINKAGE SECTION.

       01  LK-TECLA      PIC X(018).
       01  TAMANHO-TECLA PIC 9(008) COMP-X.

       PROCEDURE DIVISION USING LK-TECLA
                                TAMANHO-TECLA.

       000-INICIO.

           CALL "CWCRTS" USING "G" KEY-STATUS
           EVALUATE KEY-STATUS
               WHEN X"320B0D"  SET  ENTR       TO TRUE
               WHEN X"31001B"  SET  ESC        TO TRUE
               WHEN X"310100"  SET  F1         TO TRUE
               WHEN X"310200"  SET  F2         TO TRUE
               WHEN X"321901"  SET  F2         TO TRUE *> CTRL-A
               WHEN X"310300"  SET  F3         TO TRUE
               WHEN X"32131A"  SET  F3         TO TRUE *> CTRL-Z
               WHEN X"310400"  SET  F4         TO TRUE
               WHEN X"331111"  SET  F4         TO TRUE *> CTRL-Q
               WHEN X"310500"  SET  F5         TO TRUE
               WHEN X"331717"  SET  F5         TO TRUE *> CTRL-W
               WHEN X"310600"  SET  F6         TO TRUE
               WHEN X"330505"  SET  F6         TO TRUE *> CTRL-E
               WHEN X"310700"  SET  F7         TO TRUE
               WHEN X"321212"  SET  F7         TO TRUE *> CTRL-R
               WHEN X"310800"  SET  F8         TO TRUE
               WHEN X"331414"  SET  F8         TO TRUE *> CTRL-T
               WHEN X"310900"  SET  F9         TO TRUE
               WHEN X"320F19"  SET  F9         TO TRUE *> CTRL-Y
               WHEN X"310A00"  SET  F10        TO TRUE
               WHEN X"32100F"  SET  F10        TO TRUE *> CTRL-O
               WHEN X"310100"  SET  F11        TO TRUE
               WHEN X"311500"  SET  F11        TO TRUE *> CTRL-F1
               WHEN X"310200"  SET  F12        TO TRUE
               WHEN X"311600"  SET  F12        TO TRUE *> CTRL-F2
               WHEN X"311700"  SET  F13        TO TRUE *> CTRL-F3
               WHEN X"311800"  SET  F14        TO TRUE *> CTRL-F4
               WHEN X"310500"  SET  F15        TO TRUE
               WHEN X"310600"  SET  F16        TO TRUE
EASLEO         WHEN X"320C00"  SET  CIMA       TO TRUE
EASLEO*        WHEN X"320C00"  SET  F99        TO TRUE
               WHEN X"321400"  SET  CIMA       TO TRUE *> CTRL-END
               WHEN X"313500"  SET  PGUP       TO TRUE
               WHEN X"320B00"  SET  BAIXO      TO TRUE
               WHEN X"313600"  SET  PGDW       TO TRUE
               WHEN X"315B00"  SET  F11        TO TRUE
               WHEN X"315C00"  SET  F12        TO TRUE
               WHEN X"320300"  SET  F99        TO TRUE
               WHEN X"314100"  SET  PGUP       TO TRUE
           END-EVALUATE

           MOVE TAMANHO-TECLA TO Y
           MOVE ALL "0"       TO LK-TECLA (1: Y)
           MOVE 3             TO I
           PERFORM UNTIL Y = 0
                      OR I = 0
                   MOVE TECLA (I: 1) TO LK-TECLA (Y: 1)
                   SUBTRACT 1 FROM I Y
           END-PERFORM.

       000-99-FIM. GOBACK.

       900-99-FIM. EXIT.

       END PROGRAM MBKEY.
