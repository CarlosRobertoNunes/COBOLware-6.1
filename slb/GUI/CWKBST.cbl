       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWKBST.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  31/05/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula CBL_GET_KBD_STATUS via SP2            *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       78  SP2 value "SP2".
       01  AREAS-DE-TRABALHO.
           05  ATIVO      PIC  9(001) VALUE 0.
           05  MODO       PIC  9(001) VALUE 0.
           05  CHEIO      PIC  9(001) VALUE 0.

      *01   CURPOS.
      *     10 CURPOS-LIN           PIC  9(002) VALUE 1.
      *     10 CURPOS-COL           PIC  9(002) VALUE 1.
      *
       COPY CWSPWS.

       LINKAGE SECTION.

       01  KEY-STATUS               PIC  9(002) COMP-X.

       PROCEDURE DIVISION USING KEY-STATUS.

       000-INICIO.

           DISPLAY "NEWOBJECTS" UPON ENVIRONMENT-NAME
           DISPLAY "X"          UPON ENVIRONMENT-VALUE

           IF KEY-STATUS (1: 1) = 'C'
              CANCEL 'CWKBBF'
              GOBACK
           END-IF

           CALL "CWKBBF" USING "?" SP2-CD-CTRL-FIELD-KEY
                                   SP2-CD-KEY

           IF (SP2-CD-CTRL-FIELD-KEY NOT = 0)
           OR (SP2-CD-KEY            NOT = 0)
               IF SP2-CD-CTRL-FIELD-KEY = 500
                  MOVE 0 TO SP2-CD-CTRL-FIELD-KEY
               END-IF
               PERFORM 100-CLOSE-FIELD THRU 100-99-FIM
               MOVE 1 TO KEY-STATUS
               GOBACK
           END-IF

           ON   1
                COPY CWSPPD.
                MOVE 1 TO MODO.

           IF   MODO = 0
                CALL "CWUSER" USING "R" *> Refresh screen
                MOVE 1 TO MODO
           END-IF

           IF  (SP2-CD-CTRL-FIELD-KEY NOT = 0)
           OR  (SP2-CD-KEY            NOT = 0)
                PERFORM 100-CLOSE-FIELD THRU 100-99-FIM
                MOVE 1           TO KEY-STATUS
                GOBACK
           END-IF

           IF   ATIVO = 0
                MOVE LOW-VALUES      TO SP2-FD-DATA
                                        SP2-FD-VAR-LENS
                CALL "CWSPID" USING SP2-FD-ID "FInsert"
                MOVE 1               TO SP2-FD-FLD-NUM
                                        SP2-FD-TAB-NUM
                MOVE X"00"           TO SP2-FD-CTRL-TYPE
                MOVE 1               TO SP2-FD-PROG-LEN
                                        SP2-FD-MAX-LEN
                                        SP2-FD-INITIAL-LEN
                                        SP2-FD-VAR-LEN
                MOVE "a"             TO SP2-FD-PROG-CTRL
                MOVE "y"             TO SP2-FD-CURS-SKIP
      *         call "CWCURS"  USING "G" CURPOS
      *         beto (1, 1) CURPOS
      *         IF CURPOS NOT = ZEROS
      *            compute SP2-FD-ROW = CURPOS-LIN - 1
      *            compute SP2-FD-COL = CURPOS-COL - 1
      *         else
      *            compute SP2-FD-ROW = 10
      *            compute SP2-FD-COL = 10
      *         END-IF
                CALL SP2   USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
                MOVE 1               TO ATIVO
           END-IF

RDIG       MOVE "CWMENU" TO SP2-PD-NAME
RDIG       CALL SP2   USING SP2-GET-PANEL-DEF SP2-PANEL-DEF
RDIG       MOVE SP2-KEY-INSERT TO SP2-PD-CTRL-KEY (16)
RDIG       MOVE SP2-KEY-DELETE TO SP2-PD-CTRL-KEY (37)
RDIG       CALL SP2   USING SP2-SET-PANEL-DEF SP2-PANEL-DEF
           MOVE "k"      TO SP2-CD-WAIT-SW
           CALL SP2   USING SP2-GET-INPUT SP2-CONVERSE-DATA
RDIG       MOVE SP2-KEY-ALT-EQUAL TO SP2-PD-CTRL-KEY (16)
RDIG       MOVE SP2-KEY-ALT-MINUS TO SP2-PD-CTRL-KEY (37)
RDIG       CALL SP2   USING SP2-SET-PANEL-DEF SP2-PANEL-DEF

           IF   SP2-CD-KEY = SP2-KEY-SYS-SHUTDOWN
           OR                SP2-KEY-APP-CLOSE
           OR                SP2-KEY-CLOSE
                CALL "CWCRTS" USING "S" X"FFFFFF"
           END-IF

           IF   SP2-CD-KEY = -1 OR 0
                MOVE 0 TO KEY-STATUS
           ELSE
                MOVE 1           TO KEY-STATUS
                MOVE 0           TO MODO
                CALL "CWKBBF" USING "S" SP2-CD-CTRL-FIELD-KEY
                                        SP2-CD-KEY
                PERFORM 100-CLOSE-FIELD THRU 100-99-FIM
           END-IF.

       000-99-FIM. GOBACK.

       100-CLOSE-FIELD.

           IF   ATIVO = 1
                CALL SP2   USING SP2-DELETE-FIELD SP2-FIELD-DEF
                CALL "CWSPID" USING SP2-FD-ID "FDelete"
                MOVE 0 TO ATIVO
           END-IF.

       100-99-FIM. EXIT.

       END PROGRAM CWKBST.

