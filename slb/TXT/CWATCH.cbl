       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWATCH.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  06/03/1988.
       SECURITY.      *************************************************
                      *                                               *
                      *   Atualiza data e hora no video               *
                      *                                               *
                      *************************************************

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 GRHORA                   PIC  X(010) VALUE SPACES.
           05 CRON                     PIC  X(003) VALUE SPACES.
           05 LASTDATE                 PIC  9(008) VALUE 0.
           05 TODAY.
              10 WEEK                  PIC  X(003) VALUE SPACES.
              10 FILLER                PIC  X(001) VALUE SPACES.
              10 DATE-ED               PIC  X(010) VALUE ZEROS.
           05 TEMPO                    PIC  9(006) VALUE ZEROS.
           05 TEMPO-A                  PIC  9(006) VALUE ZEROS.
           05 TIME-ED                  PIC  X(008) VALUE SPACES.
           05 SALVA-CURSOR             PIC  X(002) VALUE SPACES.
           05 TEST-TIME                PIC  X(008) VALUE SPACES.
           05 TEST-DATE                PIC  X(014) VALUE SPACES.

       COPY CWTIME.
       COPY CWUNIX.

       PROCEDURE DIVISION.

       000-INICIO.

           ON 1
              DISPLAY "CWATCH" UPON ENVIRONMENT-NAME
              ACCEPT GRHORA  FROM ENVIRONMENT-VALUE
              DISPLAY "CWCRON" UPON ENVIRONMENT-NAME
              ACCEPT  CRON   FROM ENVIRONMENT-VALUE
              INSPECT CRON
              CONVERTING MINUSCULAS TO MAIUSCULAS
              CALL "CWUNIX" USING PARAMETROS-CWUNIX.

           IF   CRON = 'ON'
                GOBACK
           END-IF

           IF GRHORA NOT = SPACES
              CALL GRHORA
              GOBACK
           END-IF

           SET CWTIME-NORMAL       TO TRUE
           SET CWTIME-TODAY        TO TRUE
           CALL "CWTIME"        USING PARAMETROS-CWTIME
           MOVE CWTIME-TIME-FINAL         TO TEMPO
           IF  TEMPO = TEMPO-A
               GOBACK
           END-IF
           MOVE TEMPO                     TO TEMPO-A

           SET CWTIME-EDIT                TO TRUE
           MOVE CWTIME-DATE-FINAL         TO CWTIME-DATE
           MOVE CWTIME-TIME-FINAL         TO CWTIME-TIME
           CALL "CWTIME"               USING PARAMETROS-CWTIME
           MOVE CWTIME-DATE-EDITED        TO DATE-ED
           MOVE CWTIME-TIME-EDITED        TO TIME-ED
           IF   CWTIME-DATE NOT = LASTDATE
                SET CWTIME-WEEK       TO TRUE
                CALL "CWTIME"      USING PARAMETROS-CWTIME
                MOVE CWTIME-WEEK-CHAR TO WEEK
                MOVE CWTIME-DATE      TO LASTDATE
           END-IF
           CALL "CBL_READ_SCR_CHARS" USING X"0140"
                                           TEST-DATE
                                           X"000E"
           IF   TEST-DATE (07: 1) = "/"
           AND  TEST-DATE (10: 1) = "/"
           AND (TEST-DATE NOT = TODAY)
                IF   CWUNIX-ON
                     CALL "CBL_GET_CSR_POS" USING SALVA-CURSOR
                     CALL "CBL_SET_CSR_POS" USING X"FFFF"
                END-IF
                CALL "CBL_WRITE_SCR_CHARS" USING X"0140"
                                                 TODAY
                                                 X"000E"
                IF   CWUNIX-ON
                     CALL "CBL_SET_CSR_POS" USING SALVA-CURSOR
                END-IF
           END-IF
           CALL "CBL_READ_SCR_CHARS" USING X"0246"
                                           TEST-TIME
                                           X"0008"
           IF   TEST-TIME (03: 1) = ":"
           AND  TEST-TIME (06: 1) = ":"
           AND (TEST-TIME NOT = TIME-ED)
                IF   CWUNIX-ON
                     CALL "CBL_GET_CSR_POS" USING SALVA-CURSOR
                     CALL "CBL_SET_CSR_POS" USING X"FFFF"
                END-IF
                CALL "CBL_WRITE_SCR_CHARS" USING X"0246"
                                                 TIME-ED
                                                 X"0008"
                IF   CWUNIX-ON
                     CALL "CBL_SET_CSR_POS" USING SALVA-CURSOR
                END-IF
           END-IF.

       000-99-FIM. GOBACK.
       END PROGRAM CWATCH.
