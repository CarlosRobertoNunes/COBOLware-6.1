       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TENV.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  10/11/2016.
       SECURITY.      *************************************************
                      *                                               *
                      *  Testa gasto de tempo em leitura de variavel  *
                      *  de ambiente                                  *
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
           05 I                   PIC 9(008) VALUE 0.
           05 X                   PIC X(006) VALUE SPACES.
           05 A                   PIC X(006) VALUE SPACES.

       PROCEDURE DIVISION.

       000-INICIO.

           ACCEPT A FROM TIME DISPLAY A
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 1000000
                    CONTINUE
           END-PERFORM
           ACCEPT A FROM TIME DISPLAY A
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 1000000
                   DISPLAY 'NOME' UPON ENVIRONMENT-NAME
                   ACCEPT  X      FROM ENVIRONMENT-VALUE
           END-PERFORM
           ACCEPT A FROM TIME DISPLAY A.

       000-99-FIM. GOBACK.

       END PROGRAM TENV.
