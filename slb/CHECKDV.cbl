       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CHECKDV.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  25/05/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  DV para teste de Objeto de critica           *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.

       01   erro           pic 9 value 0.
       01   parametros-dv.
            05 matricula pic 9(004).
            05 dv        pic X(001).

       PROCEDURE DIVISION using  erro
                                 parametros-dv.

       000-INICIO.

            IF MATRICULA = 0
               EXEC COBOLware SEND
                         MSG "Matr¡cula inv lida"
               END-EXEC
               MOVE 1 TO ERRO
            else
               EXEC COBOLware SEND
                         MSG "Matr¡cula OK"
               END-EXEC
            end-if.

       000-99-FIM. GOBACK.

       END PROGRAM CHECKDV.
