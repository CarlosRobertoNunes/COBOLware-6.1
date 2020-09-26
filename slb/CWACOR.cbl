       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWACOR.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  03/09/1991.
       SECURITY.      *************************************************
                      *                                               *
                      *   Consulta atributos de cor                   *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
MOGI++     05 A7-TEST-MONITOR-FUNCTION  PIC X(01) COMP-X VALUE 25.
MOGI++     05 A7-TEST-MONITOR-PARAMETER PIC X(01) COMP-X VALUE 1.
MOGI++        88 video-mono                       VALUE 0.
MOGI++        88 video-color                      VALUE 1.
MOGI++        88 video-error                      VALUE 3.
MOGI++     05 ESQUEMA                 PIC  X(040) VALUE SPACES.
           05 CWACORAF                PIC  X(004) VALUE SPACES.
           05 CRON                    PIC  X(003) VALUE SPACES.
           05 TASK                    PIC  9(006) VALUE ZERO.
           05 PROGRAMA                PIC  X(008) VALUE "CWMENU".
           05 SAVE-NOME               PIC  X(030) VALUE ALL X"FF".
           05 NOME                    PIC  X(030) VALUE SPACES.
           05 PAI                     PIC  X(030) VALUE SPACES.
           05 SET-LOG                 PIC  X(001) VALUE "?".
           05 I                COMP-X PIC  9(004) VALUE 0.
           05 Y                COMP-X PIC  9(004) VALUE 0.
           05 CX               COMP-X PIC  9(004) VALUE 0.
           05 LX               COMP-X PIC  9(004) VALUE 0.
           05 CBL-READ-SCR-CHARS.
              10 SCREEN-POSITION.
                 15 ROW-NUMBER        PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER     PIC  9(002) COMP-X VALUE 0.
              10 ATTRIBUTE            PIC  9(002) COMP-X VALUE 0.

MOGI++ COPY CWUNIX.
       COPY CWCONF.

       LINKAGE SECTION.

       01   PARAMETROS-CWACOR.
            05 CORES OCCURS 20.
               10 CWACOR-F                      PIC  9(001).
               10 CWACOR-B                      PIC  9(001).

       PROCEDURE DIVISION USING PARAMETROS-CWACOR.

       000-INICIO.

           DISPLAY "CWCRON" UPON ENVIRONMENT-NAME
           ACCEPT  CRON   FROM ENVIRONMENT-VALUE
           INSPECT CRON
                   CONVERTING MINUSCULAS TO MAIUSCULAS
           IF   CRON = 'ON'
                GOBACK
           END-IF
           DISPLAY "CWACORAF" UPON ENVIRONMENT-NAME
           MOVE SPACES TO CWACORAF
           ACCEPT CWACORAF FROM ENVIRONMENT-VALUE
           INSPECT CWACORAF CONVERTING MINUSCULAS TO MAIUSCULAS
           IF   CWACORAF = 'OFF'
                GO TO END-MOGI
           END-IF
MOGI++     ON 1
MOGI++        CALL "CWUNIX" USING PARAMETROS-CWUNIX
MOGI++        IF NOT CWUNIX-GUI
MOGI++           CALL X"A7" USING A7-TEST-MONITOR-FUNCTION
MOGI++                            A7-TEST-MONITOR-PARAMETER
MOGI++           ON OVERFLOW
MOGI++              SET video-error TO TRUE
MOGI++           END-CALL
MOGI++           IF video-error
MOGI++              SET video-color TO TRUE
MOGI++           ELSE
MOGI++              DIVIDE A7-TEST-MONITOR-PARAMETER BY 2 GIVING TASK
MOGI++                            REMAINDER TASK
MOGI++              IF TASK       = 1
MOGI++                 SET video-color TO TRUE
MOGI++              ELSE
MOGI++                 SET video-mono TO TRUE
MOGI++              END-IF
MOGI++           END-IF
MOGI++        END-IF.
MOGI++     IF   (not video-color)
MOGI++           MOVE ALL "0700" TO PARAMETROS-CWACOR
MOGI++           GOBACK
MOGI++     END-IF.
       END-MOGI.
           MOVE "?"         TO SET-LOG
           CALL "CWGETU" USING NOME TASK PROGRAMA SET-LOG
           IF   NOME = "LOGON" OR LOW-VALUES
                MOVE SPACES TO NOME
           END-IF
           IF   NOME NOT = SAVE-NOME
                MOVE NOME TO SAVE-NOME
                SET CWSQLC-OPEN TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                IF   FS-CWCONF NOT = "00"
                     EXIT PROGRAM
                END-IF
                MOVE "PS"  TO CWCONF-REGAT
                MOVE NOME  TO CWCONF-ELEMENTO
                SET CWSQLC-READ        TO TRUE
                SET CWSQLC-IGNORE-LOCK TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                IF   FS-CWCONF < "10"
                AND  CWCONF-PAI NOT = SPACES
                     MOVE CWCONF-PAI TO PAI
                END-IF
                MOVE "AT"  TO CWCONF-REGAT
                MOVE NOME  TO CWCONF-ELEMENTO
                SET CWSQLC-READ        TO TRUE
                SET CWSQLC-IGNORE-LOCK TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                IF   FS-CWCONF = "00"
                     IF   CWCONF-REGAT (33: 1) = X"00"
                          MOVE ALL "71" TO CWCONF-REGAT (33: )
                     END-IF
                     MOVE CWCONF-REGAT (33: ) TO PARAMETROS-CWACOR
                END-IF
                IF   FS-CWCONF = "23"
                     MOVE "AT"   TO CWCONF-REGAT
                     DISPLAY "CWUSRDEF" UPON ENVIRONMENT-NAME
                     ACCEPT  CWCONF-ELEMENTO FROM ENVIRONMENT-VALUE
                     IF   CWCONF-ELEMENTO = SPACES
                          MOVE PAI TO CWCONF-ELEMENTO
                     END-IF
                     IF   CWCONF-ELEMENTO NOT = SPACES
                          INSPECT CWCONF-ELEMENTO
                          CONVERTING MINUSCULAS TO MAIUSCULAS
                     END-IF
                     SET CWSQLC-READ        TO TRUE
                     SET CWSQLC-IGNORE-LOCK TO TRUE
                     CALL "CWCONF" USING CWSQLC CWCONF-REG
                                                FS-CWCONF KCO PCO
                     IF   FS-CWCONF < "10"
                          MOVE CWCONF-REGAT (33: ) TO PARAMETROS-CWACOR
                     ELSE
                          MOVE ALL "71"            TO PARAMETROS-CWACOR
                     END-IF
                     SET CWSQLC-CLOSE TO TRUE
                     CALL "CWCONF" USING CWSQLC CWCONF-REG
                                         FS-CWCONF KCO PCO
                     SET CWSQLC-UPDATE TO TRUE
                     CALL "CWCONF" USING CWSQLC CWCONF-REG
                                         FS-CWCONF KCO PCO
                     MOVE "AT"   TO CWCONF-REGAT
                     MOVE NOME   TO CWCONF-ELEMENTO
                     MOVE PARAMETROS-CWACOR TO CWCONF-REGAT (33: )
                     SET CWSQLC-WRITE TO TRUE
                     CALL "CWCONF" USING CWSQLC CWCONF-REG
                                         FS-CWCONF KCO PCO
                     MOVE "AT"   TO CWCONF-REGAT
                     MOVE NOME   TO CWCONF-ELEMENTO
                     SET CWSQLC-READ        TO TRUE
                     SET CWSQLC-IGNORE-LOCK TO TRUE
                     CALL "CWCONF" USING CWSQLC CWCONF-REG
                                         FS-CWCONF KCO PCO
                END-IF
                SET CWSQLC-CLOSE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                MOVE PARAMETROS-CWACOR TO CWCONF-REGAT (33: )
           END-IF

           MOVE CWCONF-REGAT (33: ) TO PARAMETROS-CWACOR
MOGI++     DISPLAY "CWACOR" UPON ENVIRONMENT-NAME
MOGI++     MOVE SPACES TO ESQUEMA
MOGI++     ACCEPT ESQUEMA FROM ENVIRONMENT-VALUE
MOGI++     IF ESQUEMA NOT = SPACES
MOGI++        PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH ESQUEMA
MOGI++          IF ESQUEMA (I:1) NUMERIC
MOGI++             MOVE ESQUEMA (I:1) TO PARAMETROS-CWACOR (I:1)
MOGI++          END-IF
MOGI++        END-PERFORM
MOGI++     END-IF
           GOBACK.

       FIM-0500-3000. EXIT.

       END PROGRAM CWACOR.
