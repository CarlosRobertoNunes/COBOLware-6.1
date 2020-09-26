       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWRESE.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  18/01/2006.
       SECURITY.      *************************************************
                      *                                               *
                      *  Reserva mem¢ria para PANELS do SP2           *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       78  SP2 value "SP2".
       COPY SP2.

       PROCEDURE DIVISION.

       000-INICIO.

      *    PERFORM 10 TIMES
      *    CALL SP2    USING SP2-RESERVE-MEMORY SP2-NULL-PARM.

       000-99-FIM. GOBACK.

       END PROGRAM CWRESE.
