       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMENN.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  27/02/2006.
       SECURITY.      *************************************************
                      *                                               *
                      *  Verifica permissÆo de Impressora para o grupo*
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. call-convention 74 is staticWINAPI.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY CWCONF.

       LINKAGE SECTION.

       01  GRUPO-ID   PIC X(005).
       01  IMPRESSORA PIC X(030).

       PROCEDURE DIVISION USING GRUPO-ID IMPRESSORA.

       000-INICIO.

           ON 1
              SET CWSQLC-OPEN TO TRUE
              CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO.

           MOVE "GI"       TO CWCONF-REGGI
           MOVE GRUPO-ID   TO CWCONF-GU-ID
           MOVE IMPRESSORA TO CWCONF-IMPRESSORA
           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO

           IF  FS-CWCONF > "09"
               MOVE SPACES TO IMPRESSORA
           END-IF.

       000-99-FIM. GOBACK.

       END PROGRAM CWMENN.
