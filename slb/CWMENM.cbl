       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMENM.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  21/06/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Verifica existencia de programas para CWMEN3 *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 MENSAGEM-ERRO       PIC X(060) VALUE SPACES.
           05 MINUSCULAS PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
           05 MAIUSCULAS PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05 X91-RESULT          PIC  9(002) COMP-X VALUE 0.
           05 X91-F15             PIC  9(002) COMP-X VALUE 15.
           05 X91-PROVEDOR.
              10 LEN-PROVEDOR     PIC  9(002) VALUE 0 COMP-X.
              10 PROG             PIC  X(065) VALUE SPACES.

       LINKAGE SECTION.

       01   ERRO           PIC 9 VALUE 0.
       01   PROGRAMA       PIC X(008).
       01   FIELD          PIC X(030).
            88 FIELD-OK VALUE
               "CWCONF-MASTER"
               "CWCONF-END"
               "CWCONF-LOGIN"
               "CWCONF-LOGOUT"
               "CWCONF-CALLIN".

       PROCEDURE DIVISION USING ERRO PROGRAMA FIELD.

       000-INICIO.

           MOVE 0 TO ERRO
           IF  (PROGRAMA NOT = SPACES)
           IF  (PROGRAMA NOT = LOW-VALUES)
           AND  FIELD-OK
                MOVE PROGRAMA TO PROG
                INSPECT PROG CONVERTING MINUSCULAS TO MAIUSCULAS
                PERFORM VARYING LEN-PROVEDOR
                                FROM 8 BY -1
                          UNTIL LEN-PROVEDOR = 0
                          OR (PROG (LEN-PROVEDOR: 1)
                                     NOT = SPACE)
                       CONTINUE
                END-PERFORM
                CALL X"91" USING X91-RESULT X91-F15
                                 X91-PROVEDOR
                IF   X91-RESULT NOT = 0
                     STRING "Programa " DELIMITED BY SIZE
                                   PROG DELIMITED BY SPACE
                     " n∆o encontrado " DELIMITED BY SIZE
                     INTO MENSAGEM-ERRO
                     EXEC COBOLware SEND
                          MSG MENSAGEM-ERRO
                     END-EXEC
                     MOVE 1 TO ERRO
                END-IF
           END-IF.

       000-99-FIM. GOBACK.

       END PROGRAM CWMENM.
