       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWPGIR.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  17/11/2009.
       SECURITY.      *************************************************
                      *                                               *
                      *  Atualiza nome do programa no rodap‚ (GUI)    *
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

       78  SP2 value "SP2".
       01  AREAS-DE-TRABALHO.
           05 I             COMP-5 PIC S9(004) VALUE 0.

       COPY CWSPWS.

       LINKAGE SECTION.

       01   PROGRAMA PIC X(8).

       PROCEDURE DIVISION USING PROGRAMA.

       000-INICIO.

           ON 1
              COPY CWSPPD.
              continue.

           MOVE "CWMENU"   TO SP2-WD-NAME
           MOVE LOW-VALUES TO SP2-PD-DATA
           MOVE "CWMENU"   TO SP2-PD-NAME
           CALL SP2   USING SP2-GET-PANEL-DEF  SP2-PANEL-DEF
           PERFORM VARYING I FROM LENGTH OF SP2-PD-MSG-TEXT BY -1
                   UNTIL SP2-PD-MSG-TEXT (I: 1) = '/'
                   CONTINUE
           END-pERFORM
           SUBTRACT 1 FROM I
           PERFORM VARYING I FROM I BY -1
                   UNTIL SP2-PD-MSG-TEXT (I: 1) = '/'
                   CONTINUE
           END-pERFORM
           ADD  1        TO I
           MOVE PROGRAMA TO SP2-PD-MSG-TEXT (I: 8)
           CALL SP2   USING SP2-SET-PANEL-DEF  SP2-PANEL-DEF
           CALL SP2   USING SP2-DISPLAY-WINDOW SP2-NULL-PARM.

       000-99-FIM. GOBACK.

       END PROGRAM CWPGIR.
