       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWSQLBT.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  09/06/2006.
       SECURITY.      *************************************************
                      *                                               *
                      *  Ativa COMMIT Manual                          *
                      *                                               *
                      *************************************************
       PROCEDURE DIVISION.

       000-INICIO.

           DISPLAY "COMMIT" UPON ENVIRONMENT-NAME
           DISPLAY "MANUAL" UPON ENVIRONMENT-VALUE.

       000-99-FIM. GOBACK.

       100-PROCESSAMENTO.
       END PROGRAM CWSQLBT.
