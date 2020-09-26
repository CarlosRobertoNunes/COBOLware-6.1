       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LUCAS.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  20/08/2019.
       SECURITY.      *************************************************
                      *                                               *
                      *  Descreva a finalidade do seu novo programa   *
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
           05 TECLA               PIC 9(003) VALUE 0. COPY CWKEYS.

       LINKAGE SECTION.

       SCREEN SECTION.

       PROCEDURE DIVISION.

       000-INICIO.

           PERFORM 800-INICIAIS      THRU 800-99-FIM
           PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
           PERFORM 900-FINAIS        THRU 900-99-FIM.

       000-99-FIM. GOBACK.

       100-PROCESSAMENTO.

       100-99-FIM. EXIT.

       800-INICIAIS.

       800-99-FIM. EXIT.

       900-FINAIS.

       900-99-FIM. EXIT.

       END PROGRAM LUCAS.
