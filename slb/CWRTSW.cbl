       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWRTSW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  23/04/1989.
       SECURITY.      *************************************************
                      *                                               *
                      *   Trata erros de runtime                      *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT OPTIONAL CWHELP ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-CWHELP
                  RESERVE NO ALTERNATE AREA
                  LOCK MODE     IS EXCLUSIVE.

           SELECT FSINI  ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  FILE STATUS   IS FS-FSINI.

       DATA DIVISION.
       FILE SECTION.

       FD  CWHELP
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS CWHELP-FILE.

       01  CWHELP-REG PIC X(80).

       FD  FSINI
           VALUE OF FILE-ID IS LB-FSINI.

       01  FSINI-REG                   PIC X(5080).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 CWWIND-ACTIVE  PIC X(003) VALUE SPACES.
           05 CRITICA        PIC X(002) VALUE SPACES.
           05 OLD-DRIVE      PIC X(001) VALUE SPACE.
           05 OLD-DIRECTORY  PIC X(050) VALUE SPACES.
           05 SIZE-OLD-DIR   PIC 9(002) COMP-X VALUE 50.
           05 PRINTER-STATUS           PIC  9(002) COMP-X VALUE 0.
           05 CRON                     PIC  X(003) VALUE SPACES.
           05 CWLOGW                   PIC  X(003) VALUE SPACES.
           05 RESTO                    PIC  X(080) VALUE SPACES.
           05 SHOW                     PIC  X(255) VALUE SPACES.
           05 COMANDO                  PIC  X(255) VALUE SPACES.
           05 CWMENU-END               PIC  X(008) VALUE SPACES.
           05 FATOR-W           COMP-X PIC  9(002) VALUE 0.
           05 FIM                      PIC  X(002) VALUE SPACES.
           05 I                        PIC  9(003) VALUE 0.
           05 Y                        PIC  9(003) VALUE 0.
           05 M                        PIC  9(003) VALUE 0.
           05 F                        PIC  9(003) VALUE 0.
           05 DATA-DE-HOJE             PIC  X(010) VALUE SPACES.
           05 HORA                     PIC  X(008) VALUE SPACES.
           05 TEMP                     PIC  X(050) VALUE SPACES.
           05 LB-MAIL                  PIC  X(255) VALUE SPACES.
           05 LB-TELA                  PIC  X(255) VALUE SPACES.
           05 MAIL                     PIC  X(050) VALUE SPACES.
           05 FS-CWHELP                PIC  X(002) VALUE "00".
           05 USUARIO                  PIC  X(030).
           05 TASK                     PIC  9(006) VALUE 0.
           05 LOOPING                  PIC  9(006) VALUE 0.
           05 PROGRAMA                 PIC  X(008).
           05 LOG                      PIC  X(001).
              88 LOG-ON                            VALUE "1".
           05 MENSAGENS                            VALUE SPACES.
              10 MSG OCCURS 10         PIC  X(080).
           05 X91-RESULT        COMP-X PIC  9(002) VALUE 0.
           05 X91-FUNCTION      COMP-X PIC  9(002) VALUE 16.
           05 X91-PARAMETER     COMP-X PIC  9(002) VALUE 0.
           05 CNT                      PIC  9(002) COMP-X.
           05 SCNT                     PIC  9(002) COMP-X.
           05 LCNT                     PIC  9(002) COMP-X.
           05 LBUFFER                  PIC  X(160).
           05 ETYPE                    PIC  X(001).
           05 CBL-READ-WRITE-SCR-CHARS-ATTR.
              10 SCREEN-POSITION.
                 15 ROW-NUMBER         PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER      PIC  9(002) COMP-X VALUE 0.
              10 CHARACTER-BUFFER      PIC X(2000) VALUE SPACES.
              10 REDEFINES CHARACTER-BUFFER.
                 15 LINHA-TELA         PIC  X(080) OCCURS 25.
              10 ATTRIBUTE-BUFFER      PIC X(2000) VALUE SPACES.
              10 STRING-LENGTH         PIC  9(004) COMP-X VALUE 2000.
           05 ER-FSINI.
              10 FS-FSINI          PIC  X(002) VALUE "00".
              10 LB-FSINI          PIC  X(255) VALUE SPACES.

       COPY CWTIME.
       COPY CWSEND.
       COPY CWHELP.
       COPY CWGETL.
       COPY CWUNIX.

       01  LINHAS-DE-IMPRESSAO-CLIC.
       02  LINHA-01.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 FILLER                         PIC  X(009) VALUE
              "Empresa: ".
           05 CLIC-EMPRESA                   PIC  X(030) VALUE SPACES.
       02  LINHA-02.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 FILLER                         PIC  X(009) VALUE
              "Sistema: ".
           05 CLIC-SISTEMA                   PIC  X(030) VALUE SPACES.
       02  LINHA-03.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 FILLER                         PIC  X(009) VALUE
              "Usuario: ".
           05 CLIC-USUARIO                   PIC  X(030) VALUE SPACES.
       02  LINHA-04.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 FILLER                         PIC  X(010) VALUE
              "Programa: ".
           05 CLIC-PROGRAMA                  PIC  X(008) VALUE SPACES.
       02  LINHA-05.
           05 FILLER                         PIC  X(001) VALUE SPACE.
           05 FILLER                         PIC  X(016) VALUE
              "Pasta corrente: ".
           05 CLIC-PASTA                     PIC  X(050) VALUE SPACES.

       LINKAGE SECTION.

       01   ERR-MSG PIC X(325).

       PROCEDURE DIVISION USING ERR-MSG.

       000-INICIO.

           CALL "CWUNIX" USING PARAMETROS-CWUNIX
           CALL "CWGETU" USING USUARIO TASK PROGRAMA "?"
           DISPLAY "TEMP" UPON ENVIRONMENT-NAME
           ACCEPT  TEMP   FROM ENVIRONMENT-VALUE

           PERFORM 010-FORMATA-ERRO THRU 010-99-FIM

           DISPLAY "CWLOGW"     UPON ENVIRONMENT-NAME
           ACCEPT  CWLOGW       FROM ENVIRONMENT-VALUE
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > M
                                               OR CWLOGW = 'ON'
                   IF   MSG (I) NOT = SPACES
                        MOVE MSG (I) TO RESTO
                        IF   RESTO (66: 6) = SPACES
                             MOVE TASK TO RESTO (66: 6)
                        END-IF
                        CALL "CWLOGW" USING "^" RESTO
                   END-IF
           END-PERFORM

           MOVE SPACES TO CWHELP-FILE
           IF   CWUNIX-ON
                STRING TEMP    DELIMITED BY SPACE
                       "/cw" TASK ".hlp" DELIMITED BY SIZE
                  INTO CWHELP-FILE
           ELSE
                STRING TEMP    DELIMITED BY SPACE
                      "\cw" TASK ".hlp" DELIMITED BY SIZE
                  INTO CWHELP-FILE
           END-IF
           DISPLAY "CWRTSW" UPON ENVIRONMENT-NAME
           ACCEPT SHOW FROM ENVIRONMENT-VALUE
           IF SHOW NOT = SPACES
              PERFORM VARYING I FROM 1 BY 1
                UNTIL I > LENGTH OF SHOW
                    OR SHOW (I:1) = "$"
                    CONTINUE
              END-PERFORM
              PERFORM VARYING I FROM 1 BY 1
                      UNTIL I > LENGTH SHOW
                      OR SHOW (I:1) = '$'
                      CONTINUE
              END-PERFORM
              IF SHOW (I:1) = '$'
                 COMPUTE Y = I + 1
                 SUBTRACT 1 FROM I
                 STRING SHOW (1:I) DELIMITED BY '$'
                        CWHELP-FILE DELIMITED BY SPACE
                     ' ' DELIMITED BY SIZE
                     SHOW (Y:) DELIMITED BY SIZE
                  INTO COMANDO
              ELSE
                 PERFORM VARYING Y FROM LENGTH SHOW BY -1
                         UNTIL SHOW(Y:1) NOT = SPACE
                         CONTINUE
                 END-PERFORM
                 STRING SHOW(1:Y) DELIMITED BY SIZE
                     ' ' DELIMITED BY SIZE
                        CWHELP-FILE DELIMITED BY SPACE
                     ' ' DELIMITED BY SIZE
                  INTO COMANDO
              END-IF
           END-IF
           OPEN OUTPUT CWHELP
           MOVE  SPACES  TO CWHELP-REG
           MOVE 0        TO CWHELP-VERTICAL-LENGTH Y
           MOVE 0        TO CWHELP-VERTICAL-LENGTH Y
                            CWHELP-HORIZONTAL-LENGTH
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > M
                                            OR I > 80
                   IF  (MSG (I) NOT = SPACES)
                   AND (MSG (I) NOT = CWHELP-REG)
                        MOVE MSG (I) TO CWHELP-REG (2: )
                        WRITE CWHELP-REG
                        ADD 1 TO CWHELP-VERTICAL-LENGTH
Zepama*                 PERFORM VARYING Y FROM 80 BY -1
                        PERFORM VARYING Y FROM 78 BY -1
                                UNTIL Y = 1
                                   OR MSG (I) (Y: 1) NOT = SPACE
                                CONTINUE
                        END-PERFORM
                        IF   Y > CWHELP-HORIZONTAL-LENGTH
                             MOVE Y TO CWHELP-HORIZONTAL-LENGTH
                        END-IF
                   END-IF
           END-PERFORM
           CLOSE CWHELP
           DISPLAY "CWERRORMAIL" UPON ENVIRONMENT-NAME
           ACCEPT  MAIL          FROM ENVIRONMENT-VALUE
           IF  MAIL NOT = SPACES
               IF  CWUNIX-ON
                   STRING TEMP DELIMITED BY SPACE
                           "/CWrun" TASK(5:2) ".txt" DELIMITED BY SIZE
                     INTO LB-TELA
                   STRING TEMP DELIMITED BY SPACE
                           "/CWmail" TASK(5:2) ".txt" DELIMITED BY SIZE
                     INTO LB-MAIL
               ELSE
                   STRING TEMP DELIMITED BY SPACE
                           "\CWrun" TASK(5:2) ".txt" DELIMITED BY SIZE
                     INTO LB-TELA
                   STRING TEMP DELIMITED BY SPACE
                           "\CWmail" TASK(5:2) ".txt" DELIMITED BY SIZE
                     INTO LB-MAIL
               END-IF
               CALL "CBL_COPY_FILE" USING CWHELP-FILE LB-MAIL
               MOVE LB-MAIL     TO CWHELP-FILE
               OPEN EXTEND CWHELP
               WRITE CWHELP-REG FROM SPACES
               WRITE CWHELP-REG FROM "Imagem da tela em anexo"
               CALL "CBL_READ_SCR_CHATTRS" USING SCREEN-POSITION
                                                 CHARACTER-BUFFER
                                                 ATTRIBUTE-BUFFER
                                                 STRING-LENGTH
               WRITE CWHELP-REG FROM SPACES
               EXEC COBOLware GetSystem
                    SCREEN-COMPANY   ;CLIC-EMPRESA
                    SCREEN-APLICATION;CLIC-SISTEMA
                    USER             ;CLIC-USUARIO
                    PROGRAM          ;CLIC-PROGRAMA
               END-EXEC
               WRITE CWHELP-REG FROM LINHA-01
               WRITE CWHELP-REG FROM LINHA-02
               WRITE CWHELP-REG FROM LINHA-03
               WRITE CWHELP-REG FROM LINHA-04
               MOVE SPACES TO CLIC-PASTA OLD-DIRECTORY
               CALL "CBL_READ_DIR"   USING OLD-DIRECTORY
                                           SIZE-OLD-DIR
               IF   CWUNIX-OFF
                    CALL "PC_READ_DRIVE"  USING OLD-DRIVE
                                                PRINTER-STATUS
                    STRING OLD-DRIVE DELIMITED BY SIZE
                           ":\" DELIMITED BY SIZE
                           OLD-DIRECTORY DELIMITED BY SPACE
                    INTO CLIC-PASTA
               ELSE
                    MOVE OLD-DIRECTORY TO CLIC-PASTA
               END-IF
               INSPECT CLIC-PASTA CONVERTING X"00" TO SPACE
               WRITE CWHELP-REG FROM LINHA-05
               WRITE CWHELP-REG FROM SPACES
               DISPLAY "CWINI" UPON ENVIRONMENT-NAME
               ACCEPT LB-FSINI FROM ENVIRONMENT-VALUE
               IF LB-FSINI NOT = SPACES
                  OPEN INPUT FSINI
                  MOVE SPACES TO CWHELP-REG
                  STRING "[" DELIMITED BY SIZE
                          LB-FSINI DELIMITED BY SPACE
                          "]" DELIMITED BY SIZE
                         INTO CWHELP-REG
                  WRITE CWHELP-REG
                  PERFORM UNTIL FS-FSINI > "09"
                          READ FSINI
                          IF FS-FSINI < "10"
                             WRITE CWHELP-REG FROM FSINI-REG
                          END-IF
                  END-PERFORM
                  CLOSE FSINI
               END-IF
               CLOSE CWHELP
               STRING "Erro de execu‡Æo do programa "
                                DELIMITED BY SIZE
                       PROGRAMA DELIMITED BY SPACE
                       "."      DELIMITED BY SIZE
                 INTO CWSEND-MSG
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > 25
                       EXEC COBOLware UTF8 UTF-8
                            FILE LB-TELA
                            RECORD LINHA-TELA(I)
                       END-EXEC
               END-PERFORM
               EXEC COBOLware UTF8
                    FILE LB-TELA
                    CLOSE
               END-EXEC
               EXEC COBOLware Mail
                    TO MAIL
                    SUBJECT CWSEND-MSG
                    TEXT LB-MAIL
                    ATTACH(1) LB-TELA
               END-EXEC
               MOVE SPACES TO CWHELP-FILE
               IF  CWUNIX-ON
                   STRING TEMP DELIMITED BY space
                          "/cw" TASK ".hlp" DELIMITED BY SIZE
                      INTO CWHELP-FILE
               ELSE
                   STRING TEMP DELIMITED BY space
                          "\cw" TASK ".hlp" DELIMITED BY SIZE
                      INTO CWHELP-FILE
               END-IF
           END-IF

           DISPLAY 'CWUSER-MSG' UPON ENVIRONMENT-NAME
           MOVE    SPACES         TO CRITICA
           ACCEPT  CRITICA      FROM ENVIRONMENT-VALUE
           EVALUATE TRUE
           WHEN CRITICA = 'ON'
                PERFORM 030-NOTAS THRU 030-99-FIM
           WHEN SHOW = SPACES
              MOVE 1              TO CWSEND-OPTION
              MOVE SPACES         TO CWSEND-SCREENS
              MOVE " ~Exibir"     TO CWSEND-SCREEN (1)
              MOVE " ~Fechar_"    TO CWSEND-SCREEN (2)
              MOVE "Aten‡Æo: Erro no sistema." TO CWSEND-MSG
              PERFORM UNTIL CWSEND-OPTION = 2
                      MOVE 1 TO CWSEND-OPTION
                      CALL "CWSEND" USING PARAMETROS-CWSEND
                      IF   CWSEND-OPTION = 1
                           move spaces to comando
                           DISPLAY 'CWHELP-RUN' UPON ENVIRONMENT-NAME
                           ACCEPT  comando      FROM ENVIRONMENT-VALUE
                           DISPLAY 'CWWIND-ACTIVE' UPON ENVIRONMENT-NAME
                           ACCEPT CWWIND-ACTIVE FROM ENVIRONMENT-VALUE
      *                    IF (comando NOT = 'ON')
      *                    AND (CWWIND-ACTIVE NOT = 'ON')
      *                       MOVE '?0' TO comando
      *                       CALL 'CWWIND' USING COMANDO
      *                       If comando(2:1) = '1'
      *                          move 'ON' to comando
      *                       end-if
      *                    END-IF
                           IF (comando NOT = 'ON')
                           AND (CWWIND-ACTIVE NOT = 'ON')
                              MOVE 13 TO CWHELP-LINE
                              MOVE 10 TO CWHELP-COLUMN
                              move 'Erro de sistema' TO CWHELP-TITLE
                              ADD 3 TO CWHELP-HORIZONTAL-LENGTH
      *                       IF CWHELP-VERTICAL-LENGTH < 4
      *                          MOVE  4  TO CWHELP-VERTICAL-LENGTH
      *                       END-if
                              CALL "CWHELP" USING PARAMETROS-CWHELP
                           ELSE
                              PERFORM 030-NOTAS THRU 030-99-FIM
                           END-IF
                      END-IF
              END-PERFORM
           WHEN OTHER
             EXEC COBOLware ExecSystem
                  COMMAND comando Hide
             END-EXEC
           END-EVALUATE
           DELETE FILE CWHELP

           GOBACK.

       000-99-FIM.

       010-FORMATA-ERRO.

           MOVE SPACES TO MENSAGENS

           IF  ERR-MSG (1: 1) = "0" OR "1" OR "2"
               PERFORM 020-FORMATA-16BIT THRU 020-99-FIM
               GO TO 010-99-FIM
           END-IF

pep        MOVE 1      TO M
           PERFORM VARYING F FROM 325 BY -1
                            UNTIL F = 1
                        OR ERR-MSG (F: 1) NOT = SPACE
                   CONTINUE
           END-PERFORM

           PERFORM VARYING I FROM 1 BY 1 UNTIL I NOT < F
                        OR ERR-MSG (I: 1) > X"7A"
                   IF  (ERR-MSG (I: 1) NOT = X"0D")
                   AND (ERR-MSG (I: 1) NOT = X"00")
                   AND (ERR-MSG (I: 2) NOT = "\0")
                        IF   ERR-MSG (I: 1) = X"0A"
                             ADD  1 TO M
                             MOVE 0 TO Y
                        ELSE
nena                         IF   ERR-MSG (I: 1) > X"1F"
nena                         AND  ERR-MSG (I: 1) <> X"60"
                                  ADD 1 TO Y
                                  MOVE ERR-MSG (I: 1) TO MSG (M) (Y: 1)
                             END-IF
                        END-IF
                   END-IF
           END-PERFORM.

       010-99-FIM. EXIT.

       020-FORMATA-16BIT.

           MOVE SPACES TO MENSAGENS
           MOVE ERR-MSG(1:1) TO ETYPE
           ADD  1            TO M
           STRING "Error type     : " ETYPE DELIMITED BY SIZE
                  INTO  MSG (M)
           MOVE SPACES TO LBUFFER
           MOVE 1 TO LCNT
           PERFORM VARYING CNT FROM 3 BY 1
                     UNTIL CNT = 160 OR ERR-MSG(CNT:1) = X"00"
               MOVE ERR-MSG(CNT:1) TO LBUFFER(LCNT:1)
               ADD 1 TO LCNT
           END-PERFORM
           ADD  1            TO M
           STRING "Error number   : " LBUFFER DELIMITED BY SIZE
                  INTO  MSG (M)
           MOVE CNT TO SCNT
           ADD  1      TO SCNT
           MOVE SPACES TO LBUFFER
           MOVE  1     TO LCNT
           PERFORM VARYING CNT FROM SCNT BY 1
                     UNTIL CNT = 160 OR ERR-MSG(CNT:1) = X"00"
               MOVE ERR-MSG(CNT:1) TO LBUFFER(LCNT:1)
               ADD 1 TO LCNT
           END-PERFORM
           ADD  1            TO M
           STRING "Current program: " LBUFFER DELIMITED BY SIZE
                  INTO  MSG (M)
           MOVE CNT TO SCNT
           ADD 1 TO SCNT
           MOVE SPACES TO LBUFFER
           MOVE 1 TO LCNT
           PERFORM VARYING CNT FROM SCNT BY 1
                     UNTIL CNT = 160 OR ERR-MSG(CNT:1) = X"00"
               MOVE ERR-MSG(CNT:1) TO LBUFFER(LCNT:1)
               ADD 1 TO LCNT
           END-PERFORM
           ADD  1            TO M
           STRING "Current segment: " LBUFFER DELIMITED BY SIZE
                  INTO  MSG (M)
           MOVE CNT TO SCNT
           ADD 1 TO SCNT
           MOVE SPACES TO LBUFFER
           MOVE 1 TO LCNT
           PERFORM VARYING CNT FROM SCNT BY 1
                     UNTIL CNT = 160 OR ERR-MSG(CNT:1) = X"00"
               MOVE ERR-MSG(CNT:1) TO LBUFFER(LCNT:1)
               ADD 1 TO LCNT
           END-PERFORM
           ADD  1            TO M
           STRING "COBOL PC       : " LBUFFER DELIMITED BY SIZE
                  INTO  MSG (M)
           MOVE CNT    TO SCNT
           ADD  1      TO SCNT
           MOVE SPACES TO LBUFFER
           MOVE 1      TO LCNT
           PERFORM VARYING CNT FROM SCNT BY 1
                     UNTIL CNT = 160 OR ERR-MSG(CNT:1) = X"00"
                                     OR ERR-MSG(CNT:2) = SPACES
               MOVE ERR-MSG(CNT:1) TO LBUFFER(LCNT:1)
               ADD 1 TO LCNT
           END-PERFORM
           IF LBUFFER NOT = SPACES
              ADD  1 TO M
              STRING "Error text     : " LBUFFER DELIMITED BY SIZE
               INTO  MSG (M)
           END-IF
           MOVE CNT TO SCNT
           ADD 1 TO SCNT
           IF ETYPE = "1" OR "2"
               ADD  1 TO M
               MOVE SPACES TO LBUFFER
               MOVE 1 TO LCNT
               PERFORM VARYING CNT FROM SCNT BY 1
                         UNTIL CNT = 160 OR ERR-MSG(CNT:1) = X"00"
                                         OR ERR-MSG(CNT:2) = SPACES
                   MOVE ERR-MSG(CNT:1) TO LBUFFER(LCNT:1)
                   ADD 1 TO LCNT
               END-PERFORM
               IF ETYPE = "1"
                   STRING "Filename       : " LBUFFER DELIMITED BY SIZE
                     INTO  MSG (M)
               ELSE
                   STRING "Program name   : " LBUFFER DELIMITED BY SIZE
                     INTO  MSG (M)
               END-IF
           END-IF.

       020-99-FIM. EXIT.

       030-NOTAS.

           move spaces to comando
           IF CWUNIX-ON
              CALL 'system' using z'clear'
              STRING "cat " delimited by size
                cwhelp-file delimited by space
                      x'00' delimited by size
                       into comando
               CALL 'system' using comando
               CALL 'system' using
                         z'echo Tecle "q" para fechar...>$TEMP/rtserror'
               CALL 'system' using
               z'less $TEMP/rtserror'
               CALL 'system' using
               z'rm $TEMP/rtserror'
           ELSE
              STRING "notepad " delimited by size
                            cwhelp-file
                            delimited by space
                           x'00' delimited by size
                            into comando
           END-IF
           CALL 'system' using comando.

       030-99-FIM. EXIT.
       END PROGRAM CWRTSW.
