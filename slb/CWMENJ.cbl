       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMENJ.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  21/06/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Valida nome de programa para CWMEN6          *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 MINUSCULAS PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
           05 MAIUSCULAS PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".

       LINKAGE SECTION.

       01   erro           pic 9 value 0.
       01   programa       pic x(008).

       PROCEDURE DIVISION using  erro programa.

       000-INICIO.

           INSPECT PROGRAMA CONVERTING MINUSCULAS TO MAIUSCULAS
           IF   PROGRAMA = "CWMENU"
                MOVE 1   TO ERRO
                EXEC COBOLware SEND
                          MSG "CWMENU em uso recursivo"
                END-EXEC
           ELSE
                MOVE 0   TO ERRO
           END-IF.

       000-99-FIM. GOBACK.

       END PROGRAM CWMENJ.
