       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWBOXC.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  29/03/1993.
       SECURITY.      *************************************************
                      *                                               *
                      *  Abre janela-menu e retorna a opcao           *
                      *                                               *
                      *  Versao complexa, permite:                    *
                      *                                               *
                      *         1 - Opcoes ilimitadas                 *
                      *         2 - Rolagem e paginacao               *
                      *         3 - Help durante a escolha            *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT OPTIONAL BOXWRK ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS BOXWRK-CHAVE
                  ALTERNATE RECORD KEY BOXWRK-STRING-1 WITH DUPLICATES
                  ALTERNATE RECORD KEY BOXWRK-STRING-2 WITH DUPLICATES
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-BOXWRK.

       DATA DIVISION.
       FILE SECTION.

       FD  BOXWRK
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-BOXWRK.

       01  BOXWRK-REG.
           05 BOXWRK-CHAVE.
              10 BOXWRK-SEQUENCIA  COMP-3 PIC  9(018).
           05 BOXWRK-STRING-1             PIC  X(080).
           05 BOXWRK-STRING-2             PIC  X(080).
           05 BOXWRK-COLOR-ITEM           PIC  X(010).
           05 BOXWRK-H-FILE               PIC  X(050).
           05 BOXWRK-H-LINE               PIC  9(002).
           05 BOXWRK-H-COLUMN             PIC  9(002).
           05 BOXWRK-H-TYPE               PIC  9(001).
           05 BOXWRK-H-VERTICAL-LENGTH    PIC  9(002).
           05 BOXWRK-H-HORIZONTAL-LENGTH  PIC  9(002).
           05 BOXWRK-H-COLOR-FRAME        PIC  9(002) COMP-X.
           05 BOXWRK-H-COLOR-BORDER       PIC  9(002) COMP-X.
           05 BOXWRK-H-COLOR-SHADE        PIC  9(002) COMP-X.
              66 BOXWRK-HELP
                 RENAMES BOXWRK-H-FILE THRU BOXWRK-H-COLOR-SHADE.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 SEQUENCIA         COMP-3 PIC  9(018) VALUE 0.
           05 LEN-TMP                  PIC  9(002) VALUE 0.
           05 Z                        PIC  9(003) VALUE 0.
           05 ZZ                       PIC  9(003) VALUE 0.
           05 VEZ                      PIC  9(001) VALUE 1.
           05 KEEPTMP                  PIC  X(003) VALUE SPACES.
           05 TMP                      PIC  X(050) VALUE SPACES.
           05 TMP-LB                   PIC  X(012) VALUE "CW300000.tmp".
           05 TASK                     PIC  9(006) VALUE 0.
           05 BOXWRK-ABERTO            PIC  X(001) VALUE "N".
           05 BOXWRK-FIM               PIC  X(001) VALUE ">".
           05 VARNAME                  PIC  X(050) VALUE SPACES.
           05 VARDATA                  PIC  X(050) VALUE SPACES.
           05 WORK-NAME-A              PIC  X(050) VALUE SPACES.
           05 ER-BOXWRK.
              10 FS-BOXWRK             PIC  X(002) VALUE "00".
              10 LB-BOXWRK                         VALUE "CW300000.tmp".
                 15 WORK-NAME.
                    20 WORK-BYTE       PIC  X(001) OCCURS 8.
                 15 FILLER             PIC  X(042).

       COPY CWHELP.
       COPY CWSEND.
       COPY CWBOXF.

       LINKAGE SECTION.

       01  PARAMETROS-CWBOXC.
           05 CWBOXC-FUNCTION                PIC  X(002).
              88 CWBOXC-LOAD                             VALUE "L" "l".
              88 CWBOXC-SHOW                             VALUE "S" "s"
                                                               "P" "p".
              88 CWBOXC-POP-UP                           VALUE "P" "p".
              88 CWBOXC-DELETE                           VALUE "D" "d".
              88 CWBOXC-VIEW                             VALUE "V" "v".
           05 CWBOXC-LINE                    PIC  9(002).
           05 CWBOXC-COLUMN                  PIC  9(002).
           05 CWBOXC-TYPE                    PIC  9(001).
           05 CWBOXC-VERTICAL-LENGTH         PIC  9(002).
           05 CWBOXC-HORIZONTAL-LENGTH       PIC  9(002).
           05 CWBOXC-OPTION                  PIC  X(076).
           05 CWBOXC-KEY-ON                  PIC  X(001).
           05 CWBOXC-KEY                     PIC  9(002).
           05 CWBOXC-EDIT REDEFINES CWBOXC-KEY
                                      COMP-5 PIC S9(004).
           05 CWBOXC-START-CURSOR            PIC  9(002) COMP-X.
           05 CWBOXC-TITLE                   PIC  X(078).
           05 CWBOXC-COLOR-FRAME             PIC  9(002) COMP-X.
           05 CWBOXC-COLOR-BORDER            PIC  9(002) COMP-X.
           05 CWBOXC-COLOR-SHADE             PIC  9(002) COMP-X.
           05 CWBOXC-COLOR-BARR-MENU         PIC  9(002) COMP-X.
           05 CWBOXC-ORDER                   PIC  9(001).
           05 CWBOXC-RETURN                  PIC  9(001).
           05 CWBOXC-TIMEOUT-STATUS          PIC  9(001).
              88 CWBOXC-TIMEOUT-ENABLE                   VALUE 1.
              88 CWBOXC-TIMEOUT-DISABLE                  VALUE 0.
           05 CWBOXC-TIMEOUT-RETURN          PIC  9(001).
              88 CWBOXC-TIMEOUT-ON                       VALUE 1.
              88 CWBOXC-TIMEOUT-OFF                      VALUE 0.
           05 CWBOXC-RECORD.
              10 CWBOXC-WORK-NAME            PIC  X(050).
              10 CWBOXC-STRING-1             PIC  X(080).
              10 CWBOXC-STRING-1-LENGTH      PIC  9(002).
              10 CWBOXC-STRING-2             PIC  X(080).
              10 CWBOXC-STRING-2-LENGTH      PIC  9(002).
              10 CWBOXC-H-FILE               PIC  X(050).
              10 CWBOXC-H-LINE               PIC  9(002).
              10 CWBOXC-H-COLUMN             PIC  9(002).
              10 CWBOXC-H-TYPE               PIC  9(001).
              10 CWBOXC-H-VERTICAL-LENGTH    PIC  9(002).
              10 CWBOXC-H-HORIZONTAL-LENGTH  PIC  9(002).
              10 CWBOXC-H-COLOR-FRAME        PIC  9(002) COMP-X.
              10 CWBOXC-H-COLOR-BORDER       PIC  9(002) COMP-X.
              10 CWBOXC-H-COLOR-SHADE        PIC  9(002) COMP-X.

       PROCEDURE DIVISION USING PARAMETROS-CWBOXC.

       000-INICIO.

           IF   CWBOXC-ORDER = 0
                MOVE 1 TO CWBOXC-ORDER
           END-IF

           IF   VEZ = 1
                MOVE 2            TO VEZ
                DISPLAY "CWKEEPTMP"    UPON ENVIRONMENT-NAME
                ACCEPT  KEEPTMP        FROM ENVIRONMENT-VALUE
                INSPECT KEEPTMP CONVERTING MINUSCULAS
                                      TO MAIUSCULAS
                DISPLAY "TEMP"         UPON ENVIRONMENT-NAME
                ACCEPT  TMP            FROM ENVIRONMENT-VALUE
                IF   TMP = SPACES
                     DISPLAY "TMP"     UPON ENVIRONMENT-NAME
                     ACCEPT  TMP       FROM ENVIRONMENT-VALUE
                END-IF
           END-IF

           IF   CWBOXC-LOAD
           AND  BOXWRK-ABERTO = "N"
           OR  (CWBOXC-WORK-NAME NOT = WORK-NAME-A)
                IF BOXWRK-ABERTO NOT = 'N'
                   CLOSE BOXWRK
                   MOVE "@" TO BOXWRK-ABERTO
                END-IF
                IF   CWBOXC-WORK-NAME (1:1) = "$"
                     PERFORM VARYING LEN-TMP FROM 1 BY 1
                               UNTIL CWBOXC-WORK-NAME (LEN-TMP:1)
                                     = SPACE OR "/" OR "\"
                             CONTINUE
                     END-PERFORM
                     IF  CWBOXC-WORK-NAME (LEN-TMP:1) = "\"
                     OR  CWBOXC-WORK-NAME (LEN-TMP:1) = "/"
                         SUBTRACT 1 FROM LEN-TMP
                     END-IF
                     SUBTRACT 1 FROM LEN-TMP
                     MOVE CWBOXC-WORK-NAME(2:LEN-TMP) TO VARNAME
                     ADD 1 TO LEN-TMP
                     DISPLAY VARNAME UPON ENVIRONMENT-NAME
                     MOVE SPACES TO VARNAME VARDATA
                     ACCEPT VARDATA FROM ENVIRONMENT-VALUE
                     IF  VARDATA NOT = SPACES
                         ADD 1 TO LEN-TMP
                         IF  (CWBOXC-WORK-NAME (LEN-TMP:1) NOT = "\")
                         AND (CWBOXC-WORK-NAME (LEN-TMP:1) NOT = "/")
                             ADD 1 TO LEN-TMP
                             MOVE VARDATA TO CWBOXC-WORK-NAME
                         ELSE
                             STRING VARDATA DELIMITED BY SPACE
                                    CWBOXC-WORK-NAME(LEN-TMP:)
                                    DELIMITED BY SPACE
                               INTO VARNAME
                             MOVE VARNAME TO CWBOXC-WORK-NAME
                         END-IF
                     END-IF
                END-IF
      *         IF   CWBOXC-WORK-NAME = "CWBOXC##"
                IF   CWBOXC-WORK-NAME (7: 2) = "##"
                     CALL "CWTASK" USING "2" TASK
                     MOVE TASK (2: 5) TO TMP-LB (4: 5)
                     IF   TMP NOT = SPACE
                          MOVE SPACES TO LB-BOXWRK
                          STRING TMP    DELIMITED BY SPACE
                                 "/"    DELIMITED BY SIZE
                                 TMP-LB DELIMITED BY SPACE
                            INTO LB-BOXWRK
                     ELSE
                          MOVE TMP-LB TO LB-BOXWRK
                     END-IF
                     MOVE LB-BOXWRK TO CWBOXC-WORK-NAME WORK-NAME-A
                ELSE
                     IF   CWBOXC-WORK-NAME NOT = WORK-NAME-A
                          MOVE CWBOXC-WORK-NAME TO LB-BOXWRK
                          CALL "CWTASK" USING "2" TASK
                          IF   BOXWRK-ABERTO = "S"
                               CLOSE BOXWRK
                               MOVE "N" TO BOXWRK-ABERTO
                          END-IF
                          MOVE    6                TO Z
                          PERFORM VARYING ZZ FROM 50 BY -1
                                    UNTIL ZZ = 1 OR Z = 0
                                  IF   WORK-BYTE (ZZ) = "#" OR "$"
                                       MOVE TASK (Z: 1)
                                         TO WORK-BYTE (ZZ)
                                       SUBTRACT 1 FROM Z
                                  END-IF
                          END-PERFORM
                          MOVE LB-BOXWRK TO CWBOXC-WORK-NAME WORK-NAME-A
                     END-IF
                END-IF
           END-IF

           IF   BOXWRK-ABERTO = "N" OR "@"
      *         IF  CWBOXC-LOAD
      *             DELETE FILE BOXWRK
      *         END-IF
                IF  (TMP NOT = SPACE)
                AND  CWBOXC-LOAD
                AND (KEEPTMP NOT = 'ON')
                AND (BOXWRK-ABERTO NOT = '@')
                     PERFORM VARYING LEN-TMP FROM LENGTH OF TMP BY -1
                                     UNTIL TMP (LEN-TMP: 1) NOT = SPACE
                             CONTINUE
                     END-PERFORM
                     IF   LB-BOXWRK (1: LEN-TMP) = TMP (1: LEN-TMP)
                          DELETE FILE BOXWRK
                     END-IF
                END-IF
                OPEN I-O BOXWRK
                IF   FS-BOXWRK > "09"
                     CALL "CWISAM" USING ER-BOXWRK
                ELSE
                     MOVE "S" TO BOXWRK-ABERTO
                END-IF
           END-IF

           EVALUATE TRUE
               WHEN CWBOXC-LOAD   PERFORM 100-LOAD   THRU 100-99-FIM
               WHEN CWBOXC-SHOW
                 OR CWBOXC-VIEW
                    PERFORM 200-SHOW   THRU 200-99-FIM
               WHEN CWBOXC-DELETE PERFORM 300-DELETE THRU 300-99-FIM
               WHEN OTHER
                    MOVE "Erro de l¢gica usando a CWBOXC" TO CWSEND-MSG
                    CALL "CWSEND" USING PARAMETROS-CWSEND
           END-EVALUATE.
           CALL "CWATCH".

       000-99-FIM. GOBACK.

       100-LOAD.

           ADD  1                          TO SEQUENCIA
           MOVE SEQUENCIA                  TO BOXWRK-SEQUENCIA
           MOVE CWBOXC-STRING-1            TO BOXWRK-STRING-1
           MOVE CWBOXC-STRING-2            TO BOXWRK-STRING-2
           MOVE CWBOXC-H-FILE              TO BOXWRK-H-FILE
           MOVE CWBOXC-H-LINE              TO BOXWRK-H-LINE
           MOVE CWBOXC-H-COLUMN            TO BOXWRK-H-COLUMN
           MOVE CWBOXC-H-TYPE              TO BOXWRK-H-TYPE
           MOVE CWBOXC-H-VERTICAL-LENGTH   TO BOXWRK-H-VERTICAL-LENGTH
           MOVE CWBOXC-H-HORIZONTAL-LENGTH TO BOXWRK-H-HORIZONTAL-LENGTH
           MOVE CWBOXC-H-COLOR-FRAME       TO BOXWRK-H-COLOR-FRAME
           MOVE CWBOXC-H-COLOR-BORDER      TO BOXWRK-H-COLOR-BORDER
           MOVE CWBOXC-H-COLOR-SHADE       TO BOXWRK-H-COLOR-SHADE
           MOVE SPACES                     TO BOXWRK-COLOR-ITEM
           DISPLAY "CWBOXC-COLOR-ITEM"     UPON ENVIRONMENT-NAME
           ACCEPT   BOXWRK-COLOR-ITEM      FROM ENVIRONMENT-VALUE
           DISPLAY  SPACES                 UPON ENVIRONMENT-VALUE
           CALL "CWVARX" USING BOXWRK-STRING-1 LENGTH OF BOXWRK-STRING-1
           CALL "CWVARX" USING BOXWRK-STRING-2 LENGTH OF BOXWRK-STRING-2

           WRITE BOXWRK-REG
                 IF   FS-BOXWRK > "09"
                      CALL "CWISAM" USING ER-BOXWRK
                 END-IF

           MOVE SPACES TO CWBOXC-OPTION.

       100-99-FIM. EXIT.

       200-SHOW.

           move cwboxc-FUNCTION            to cwboxf-FUNCTION
           MOVE "CWBOXM"                   TO CWBOXF-PROGRAM
           move LB-BOXWRK                  to cwboxf-WORK-AREA
           move cwboxc-LINE                to cwboxf-LINE
           move cwboxc-COLUMN              to cwboxf-COLUMN
           move cwboxc-TYPE                to cwboxf-TYPE
           move cwboxc-VERTICAL-LENGTH     to cwboxf-VERTICAL-LENGTH
           move cwboxc-HORIZONTAL-LENGTH   to cwboxf-HORIZONTAL-LENGTH
           move cwboxc-OPTION              to cwboxf-OPTION
           MOVE CWBOXC-KEY-ON              TO CWBOXF-KEY-ON
           MOVE CWBOXC-KEY                 TO CWBOXF-KEY
           move cwboxc-START-CURSOR        to cwboxf-START-CURSOR
           move cwboxc-TITLE               to cwboxf-TITLE
           move cwboxc-COLOR-FRAME         to cwboxf-COLOR-FRAME
           move cwboxc-COLOR-BORDER        to cwboxf-COLOR-BORDER
           move cwboxc-COLOR-SHADE         to cwboxf-COLOR-SHADE
           move cwboxc-COLOR-BARR-MENU     to cwboxf-COLOR-BARR-MENU
           move cwboxc-ORDER               to cwboxf-ORDER
           move cwboxc-RETURN              to cwboxf-RETURN
           move cwboxc-STRING-1-LENGTH     to cwboxf-STRING-1-LENGTH
           move cwboxc-STRING-2-LENGTH     to cwboxf-STRING-2-LENGTH
           move cwboxc-TIMEOUT-STATUS      to cwboxf-TIMEOUT-STATUS
           move cwboxc-TIMEOUT-RETURN      to cwboxf-TIMEOUT-RETURN
           CALL "CWBOXF" USING PARAMETROS-CWBOXF
           move cwboxF-KEY                 to cwboxC-KEY
           move cwboxF-OPTION              to cwboxC-OPTION
           move cwboxF-EDIT                to cwboxC-EDIT
           CLOSE BOXWRK
           MOVE "N"                      TO BOXWRK-ABERTO.

       200-99-FIM. EXIT.

       300-DELETE.

           CLOSE BOXWRK
           MOVE "N" TO BOXWRK-ABERTO
           DELETE FILE BOXWRK.

       300-99-FIM. EXIT.

       END PROGRAM CWBOXC.

