      $Set CallFH"FHREDIR"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWPICT.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  06/05/2005.
       SECURITY.      *************************************************
                      *                                               *
                      *  Mantem, figuras                              *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT BMPFILES ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS BMPFILES-CHAVE
                  LOCK MODE     IS MANUAL
                  RESERVE NO ALTERNATE AREA
                  FILE STATUS   IS FS-BMPFILES.

       DATA DIVISION.
       FILE SECTION.

       FD  BMPFILES
           LABEL RECORD IS STANDARD
           RECORD VARYING FROM 58 TO 32826 DEPENDING ON SZ-BMPFILES
           VALUE OF FILE-ID IS LB-BMPFILES.

       01  BMPFILES-REG.
           05 BMPFILES-CHAVE.
              10 BMPFILES-RECORD     PIC X(50).
              10 BMPFILES-SEQUENCE   PIC 9(05).
           05 BMPFILES-TYPE          PIC X(03).
           05 BMPFILES-BLOCK.
              10 BMPFILES-BYTE       PIC X(001)
                 OCCURS 0 TO 32768 DEPENDING ON TAMANHO.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
frango     05 JANPOS.
frango        10 JANLIN             PIC  9(002) VALUE 0.
frango        10 JANCOL             PIC  9(002) VALUE 0.
           05 WS-STATUS             PIC  X(002) VALUE SPACE.
           05 DTASK                 PIC  9(006) VALUE 0.
           05 SHOWFAX               PIC  X(003) VALUE SPACE.
           05 COPYCMD               PIC  X(200) VALUE SPACES.
           05 DELETAR               PIC  9(001) VALUE 0.
           05 DELETAR-ARQUIVO       PIC  X(080) VALUE SPACES.
           05 EXT                   PIC  X(004) VALUE ".BMP".
           05 BMPWORK-REG           PIC  X(001) VALUE SPACE.
           05 TYPE-TESTE            PIC  X(005) VALUE SPACE.
              88 JPEG       value "jpg" "jif" "jpe" "jpeg" "jfif".
              88 TARGA      value "tga" "targa".
              88 TIFF       value "tif" "tiff".
              88 PICT       value "pct" "pict" "pic".
              88 WBMP       value "wbmp".
           05 K                     PIC  9(002) VALUE 0.
           05 I                     PIC  9(005) VALUE 0.
           05 TAMANHO               PIC  9(005) VALUE 0.
           05 SZ-BMPFILES           PIC  9(008) VALUE 55.
           05 IMAGE                 PIC  X(255) VALUE SPACES.
           05 ER-BMPFILES.
              10 FS-BMPFILES        PIC  X(002) VALUE "00".
              10 LB-BMPFILES        PIC  X(255) VALUE SPACES.
           05 ER-BMPWORK.
              10 FS-BMPWORK         PIC  X(002) VALUE "00".
              10 LB-BMPWORK         PIC  X(100) VALUE SPACES.
              10 TEMP               PIC  X(100) VALUE SPACES.
              10 BMPWORK-LABEL                  VALUE "CW000000.BMP".
                 15                 PIC  X(002).
                 15 TASK            PIC  9(006).
                 15                 PIC  X(001).
                 15 EXTENSAO        PIC  X(003).

       COPY CWUNIX.

       LINKAGE SECTION.

       COPY CWPICT.

       PROCEDURE DIVISION USING PARAMETROS-CWPICT.

       000-INICIO.

           ON 1
           CALL "CWUNIX" USING PARAMETROS-CWUNIX
           IF   NOT CWUNIX-ON
                DISPLAY 'CWFAXWORK' UPON ENVIRONMENT-NAME
                ACCEPT TEMP FROM ENVIRONMENT-VALUE
                IF TEMP = SPACES
                   DISPLAY 'TEMP' UPON ENVIRONMENT-NAME
                   ACCEPT TEMP   FROM ENVIRONMENT-VALUE
                   IF TEMP = SPACES
                      DISPLAY 'TMP' UPON ENVIRONMENT-NAME
                      ACCEPT TEMP   FROM ENVIRONMENT-VALUE
                   END-IF
                END-IF
                DISPLAY "CWSHOWFAX" UPON ENVIRONMENT-NAME
                ACCEPT SHOWFAX FROM ENVIRONMENT-VALUE
                INSPECT SHOWFAX CONVERTING MINUSCULAS TO MAIUSCULAS.

           IF  (CWPICT-DISPLAY
           OR   CWPICT-ERASE
           OR   CWPICT-FUNCION = X'00')
           AND (DELETAR-ARQUIVO NOT = SPACES)
               CALL "CBL_DELETE_FILE" USING DELETAR-ARQUIVO
               MOVE SPACES TO DELETAR-ARQUIVO
           END-IF

           MOVE '00'   TO WS-STATUS

           IF   CWPICT-CURPOS NOT NUMERIC
                MOVE '0000' TO CWPICT-CURPOS
           END-IF

           IF   CWPICT-FUNCION = X'00'
                GOBACK
           END-IF

           IF  CWPICT-LINE = 0
               MOVE 1 TO CWPICT-LINE
           END-IF
           IF  CWPICT-COLUMN = 0
               MOVE 1 TO CWPICT-COLUMN
           END-IF

frango     DISPLAY 'WINPOS'      UPON ENVIRONMENT-NAME
frango     ACCEPT  JANPOS        FROM ENVIRONMENT-VALUE
frango     add JANLIN to CWPICT-LINE
frango     add JANCOL to CWPICT-COLUMN
frango     DISPLAY '0000'        UPON ENVIRONMENT-VALUE

           MOVE SPACES TO IMAGE
                          LB-BMPWORK

           IF  CWPICT-RECORD NOT = SPACES
               IF   CWPICT-OLDFILE = LOW-VALUES
                    IF   CWPICT-FILE NOT = SPACES
                         MOVE CWPICT-FILE   TO LB-BMPFILES
                    ELSE
                         MOVE "cwbmpf"      TO LB-BMPFILES
                    END-IF
               ELSE
                    IF   CWPICT-OLDFILE NOT = SPACES
                         MOVE CWPICT-OLDFILE TO LB-BMPFILES
                    ELSE
                         MOVE "cwbmpf"       TO LB-BMPFILES
                    END-IF
               END-IF
               EVALUATE TRUE
                   WHEN CWPICT-UPDATE
                        OPEN I-O BMPFILES
                        IF   FS-BMPFILES > '09'
                             MOVE FS-BMPFILES TO WS-STATUS
                        END-IF
                        IF   CWPICT-IMAGE NOT = SPACES
                             MOVE CWPICT-IMAGE TO LB-BMPWORK
                             CALL "CWBINF" USING "I" FS-BMPWORK
                                                     LB-BMPWORK
                             IF  FS-BMPWORK = X"3904"
                                 PERFORM VARYING I
                                         FROM LENGTH OF LB-BMPWORK
                                           BY -1
                                              UNTIL I = 0
                                              OR LB-BMPWORK (I:1) = '.'
                                         CONTINUE
                                 END-PERFORM
                                 ADD 1 TO I
                                 MOVE LB-BMPWORK(I:) TO EXTENSAO
                                 CALL "CWTASK" USING "4" TASK
                                 MOVE SPACES TO COPYCMD
                                 STRING "COPY " DELIMITED BY SIZE
                                        LB-BMPWORK DELIMITED BY SPACE
                                        ' ' DELIMITED BY SIZE
                                        BMPWORK-LABEL DELIMITED BY SIZE
                                        Z">NUL:" DELIMITED BY SIZE
                                        INTO  COPYCMD
                                 MOVE BMPWORK-LABEL TO LB-BMPWORK
                                 CALL "system" USING COPYCMD
                                 CALL "CWBINF" USING "I"
                                                     FS-BMPWORK
                                                     LB-BMPWORK
                                 MOVE 1 TO DELETAR
                             END-IF
                             IF  FS-BMPWORK NOT = "00"
                                 MOVE SPACES TO CWPICT-IMAGE
                             END-IF
                        END-IF
                        IF   CWPICT-IMAGE = SPACES
                             MOVE ".JPG" TO EXT
                             EXEC COBOLware Path
                                  TITLE "Carregar_figura:"
                                  FILE;LB-BMPWORK
                                  DEFAULT ".JPG"
                             END-EXEC
                             CALL "CWBINF" USING "I" FS-BMPWORK
                                                     LB-BMPWORK
                        END-IF
                        IF  FS-BMPWORK = "00"
                            PERFORM 010-EXCLUIR THRU 010-99-FIM
                            MOVE CWPICT-RECORD TO BMPFILES-RECORD
                            MOVE 1             TO BMPFILES-SEQUENCE
                            PERFORM VARYING I
                                    FROM LENGTH OF LB-BMPWORK
                                    BY -1
                                    UNTIL I = 1
                                    IF LB-BMPWORK (I: 1) = "."
                                       ADD 1 TO I
                                       MOVE LB-BMPWORK (I: )
                                         TO TYPE-TESTE
                                       MOVE LB-BMPWORK (I: 3)
                                         TO BMPFILES-TYPE
                                       EXIT PERFORM
                                    END-IF
                            END-PERFORM
                            inspect BMPFILES-TYPE
                                       converting maiusculas
                                               to minusculas
                            EVALUATE TRUE
                                WHEN JPEG  MOVE "jpg" TO BMPFILES-TYPE
                                WHEN TARGA MOVE "tga" TO BMPFILES-TYPE
                                WHEN TIFF  MOVE "tif" TO BMPFILES-TYPE
                                WHEN PICT  MOVE "pct" TO BMPFILES-TYPE
                                WHEN WBMP  MOVE "wbm" TO BMPFILES-TYPE
                            END-EVALUATE
                            MOVE 0  TO TAMANHO
                            MOVE 58 TO SZ-BMPFILES
                            PERFORM UNTIL FS-BMPWORK > "09"
                              CALL "CWBINF" USING "R" FS-BMPWORK
                                                   BMPWORK-REG(1:1)
                              IF   FS-BMPWORK < "10"
                                   ADD 1 TO TAMANHO
                                            SZ-BMPFILES
                                   MOVE BMPWORK-REG
                                     TO BMPFILES-BYTE (TAMANHO)
                                   IF TAMANHO = 32768
                                      WRITE BMPFILES-REG
                                      MOVE 0  TO TAMANHO
                                      MOVE 58 TO SZ-BMPFILES
                                      ADD 1   TO BMPFILES-SEQUENCE
                                   END-IF
                              END-IF
                            END-PERFORM
                            IF  TAMANHO NOT = 0
                                WRITE BMPFILES-REG
                            END-IF
                            CALL "CWBINF" USING "C" FS-BMPWORK
                            IF DELETAR = 1
                               MOVE 0 TO DELETAR
                               CALL "CWBINF" USING "D" FS-BMPWORK
                            END-IF
                        ELSE
                            MOVE FS-BMPWORK TO WS-STATUS
                        END-IF
                        CLOSE BMPFILES
                   WHEN CWPICT-REMOVE
                        OPEN I-O BMPFILES
                        PERFORM 010-EXCLUIR THRU 010-99-FIM
                        CLOSE BMPFILES
                   WHEN CWPICT-EXTRACT
                     OR(CWPICT-DISPLAY AND SHOWFAX = 'ON')
                        OPEN INPUT BMPFILES
                        MOVE FS-BMPFILES TO WS-STATUS
                        MOVE CWPICT-RECORD TO BMPFILES-RECORD
                        MOVE 1             TO BMPFILES-SEQUENCE
                        IF WS-STATUS < '10'
                           READ BMPFILES IGNORE LOCK
                           MOVE FS-BMPFILES TO WS-STATUS
                        END-IF
                        IF  FS-BMPFILES = "00"
                            MOVE BMPFILES-TYPE TO EXTENSAO
                            IF  CWPICT-IMAGE NOT = SPACES
                                MOVE CWPICT-IMAGE TO LB-BMPWORK
                            ELSE
                                IF CWPICT-DISPLAY
                                AND DTASK > 0
                                   MOVE DTASK TO TASK
                                ELSE
                                   CALL "CWTASK" USING "4" TASK
                                   IF DTASK = 0
                                      MOVE TASK TO DTASK
                                   END-IF
                                END-IF
                                MOVE SPACES TO LB-BMPWORK
                                STRING TEMP DELIMITED BY SPACE
                                       "\"  DELIMITED BY SIZE
                                       BMPWORK-LABEL DELIMITED BY SPACE
                                  INTO LB-BMPWORK
                             END-IF
                             CALL "CWBINF" USING "O" FS-BMPWORK
                                                     LB-BMPWORK
                             PERFORM UNTIL FS-BMPFILES > "09"
                                     COMPUTE TAMANHO = SZ-BMPFILES - 58
                                     MOVE 0 TO I
                                     PERFORM TAMANHO TIMES
                                             ADD 1 TO I
                                             CALL "CWBINF"
                                             USING "W" FS-BMPWORK
                                                   BMPFILES-BYTE (I)
                                     END-PERFORM
                                     ADD 1 TO BMPFILES-SEQUENCE
                                     READ BMPFILES IGNORE LOCK
                             END-PERFORM
                             CALL "CWBINF" USING "C" FS-BMPWORK
                             MOVE LB-BMPWORK   TO IMAGE
                             IF  CWPICT-DISPLAY
                             AND CWPICT-IMAGE = SPACES
                                MOVE IMAGE TO DELETAR-ARQUIVO
                             END-IF
                        ELSE
                             MOVE CWPICT-IMAGE TO IMAGE
                        END-IF
                        CLOSE BMPFILES
                        IF  CWPICT-DISPLAY
                        AND IMAGE NOT = SPACES
                            MOVE SPACES TO COPYCMD
                            STRING 'START ' DELIMITED BY SIZE
                                   IMAGE DELIMITED BY SPACE
                                   INTO COPYCMD
                            EXEC COBOLware EXECsystem ASSYNCRONE HIDE
                                 COMMAND COPYCMD
                            END-EXEC
                        END-IF
               END-EVALUATE
           ELSE
                MOVE CWPICT-IMAGE TO IMAGE
           END-IF

           IF  CWPICT-EXTRACT
               INITIALIZE PARAMETROS-CWPICT
               MOVE IMAGE TO CWPICT-IMAGE
           ELSE
               INITIALIZE PARAMETROS-CWPICT
           END-IF

           MOVE WS-STATUS TO CWPICT-STATUS
           MOVE '**'      TO CWPICT-FLAG
frango     subtract JANLIN from CWPICT-LINE
frango     subtract JANCOL from CWPICT-COLUMN
frango     DISPLAY 'WINPOS'      UPON ENVIRONMENT-NAME
frango     DISPLAY JANPOS        UPON ENVIRONMENT-VALUE.

       000-99-FIM. GOBACK.

       010-EXCLUIR.

           MOVE CWPICT-RECORD TO BMPFILES-RECORD
           MOVE 1             TO BMPFILES-SEQUENCE
           MOVE '23'          TO WS-STATUS
           PERFORM UNTIL FS-BMPFILES > "09"
                   READ BMPFILES
                   IF   FS-BMPFILES < "10"
                        MOVE '00' TO WS-STATUS
                        DELETE BMPFILES RECORD
                   END-IF
                   ADD  1 TO BMPFILES-SEQUENCE
           END-PERFORM.

       010-99-FIM. EXIT.

       END PROGRAM CWPICT.

