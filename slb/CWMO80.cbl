       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMO80.
       DATE-WRITTEN.  29/02/2008.
       SECURITY.      *************************************************
                      *                                               *
                      *  Exibe vari veis de mais de 80 bytes          *
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
           05 S                   PIC 9(004) COMP-X.
           05 RESTO               PIC 9(004) COMP-X.
       01  ELEMENTO.
           10 POS-E.
              15 LIN-E              PIC  9(002).
              15 COL-E              PIC  9(002).
           10 REDEFINES POS-E.
              15 PLUS-LIN           PIC  X(001).
              15 LIN-X              PIC  9(002) COMP-X.
              15 PLUS-COL           PIC  X(001).
              15 COL-X              PIC  9(002) COMP-X.
           10 LEN-E                 PIC  9(002).
           10 ATTR.
              15 MODE-E             PIC  X(001).
                 88 ACCEPT-E             VALUE "U" "u" "T" "t".
                 88 VARIAVEL             VALUE "U" "u" "T" "t"
                                               "F" "f".
                 88 no-update            VALUE "T" "t".
                 88 ERASE-EOL            VALUE "l".
                 88 ERASE-EOS            VALUE "s".
                 88 BLANK-LINE           VALUE "L".
                 88 BLANK-SCREEN         VALUE "S".
                 88 TEXTO                VALUE "V" "v".
              15 FORE-E.
                 20 FORE-N              PIC  9(001).
              15 BACK-E.
                 20 BACK-N          PIC  9(001).
              15 SECURE-E           PIC  X(001).
              15 BLINK-E            PIC  X(001).
              15 BZERO-E            PIC  X(001).
              15 EMPTY-E            PIC  X(001).
                 88 RM-EOL          VALUE 'L'.
                 88 RM-EOS          VALUE 'S'.
              15 BEEP-E             PIC  X(001).
              15 REVERSE-E          PIC  X(001).
              15 AUTO-E             PIC  X(001).
              15 HIGH-E             PIC  X(001).
              15 UPPLOW-E           PIC  X(001).
              15 ADVANCE-E          PIC  X(001).
                 88 AD-EOL          VALUE 'L'.
                 88 AD-EOS          VALUE 'S'.
           10 PLUS-E                PIC  X(004).
           10                       PIC  X(006).
           10 FIELD-GUIA-E          PIC  X(008).
           10 ATTR-INIT             PIC  X(001).
           10 COM-SINAL             PIC  9(001).
           10 MODO-ACCEPT           PIC  X(001).
              88 INDEFINIDO                 VALUE "?".
              88 VER-MODO-ACCEPT            VALUE SPACE.
              88 NUMERICO                   VALUE "N" "b" "/".
              88 NUMERICO-BARRA             VALUE "/".
              88 ALFABETICO                 VALUE "A" "B" "?".
              88 COM-BARRA                  VALUE "B" "b".
              88 COM-BARRA1                 VALUE "B".
              88 COM-BARRA2                 VALUE "b".
           10 PIC-E                 PIC  X(080).
           10 DATANAME-E            PIC  X(030).
           10 DATA-E                PIC  X(080).
           10 CTRL-E                PIC  X(080).

       LINKAGE SECTION.

       01  TELA              PIC X(2000).
       01  TAMANHO           PIC 9(08) COMP-X.
       01  CWUSER-LK         PIC X(310).

       PROCEDURE DIVISION USING TELA TAMANHO CWUSER-LK.

       000-INICIO.

           MOVE 1         TO S
           MOVE TAMANHO   TO RESTO
           MOVE CWUSER-LK TO ELEMENTO
           IF TAMANHO > 80
              MOVE 80        TO LEN-E
           ELSE
              MOVE TAMANHO   TO LEN-E
           END-IF
           PERFORM UNTIL RESTO = 0
                   MOVE TELA(S: LEN-E) TO DATA-E
                   ADD  LEN-E          TO S
                   CALL "CWUSER" USING "DCWMO80  ."
                         ELEMENTO LENGTH OF ELEMENTO
                   MOVE 0 TO LIN-E
                             COL-E
                   SUBTRACT LEN-E FROM RESTO
                   IF RESTO < 80
                      MOVE RESTO TO LEN-E
                   END-IF
           END-PERFORM.

       000-99-FIM. GOBACK.

       END PROGRAM CWMO80.
