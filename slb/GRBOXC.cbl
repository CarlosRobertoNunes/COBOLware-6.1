       WORKING-STORAGE SECTION.

       77  I                        PIC  9(003) VALUE 0.
       77  T                        PIC  9(003) VALUE 0.
       77  NOME                     PIC  X(030) VALUE SPACES.
       77  TASK                     PIC  9(006) VALUE 0.
       77  GRUPO                    PIC  X(022) VALUE SPACES.
       77  PROGRAMA                 PIC  X(008) VALUE SPACES.

       COPY CWBOXC.cpy.

       LINKAGE SECTION.

       01  PARAMETROS-GRBOXC.
           05 GRBOXC-FUNCTION                 PIC X(002) VALUE SPACE.
           05 GRBOXC-LINE                     PIC 9(002) VALUE 1.
           05 GRBOXC-COLUMN                   PIC 9(002) VALUE 1.
           05 GRBOXC-TYPE                     PIC 9(001) VALUE 0.
           05 GRBOXC-VERTICAL-LENGTH          PIC 9(002) VALUE 0.
           05 GRBOXC-HORIZONTAL-LENGTH        PIC 9(002) VALUE 0.
           05 GRBOXC-OPTION                   PIC X(040) VALUE SPACE.
           05 GRBOXC-TITLE                    PIC X(078) VALUE SPACES.
           05 GRBOXC-COLOR-SCREENS     COMP-X PIC 9(002) VALUE 110.
           05 GRBOXC-COLOR-FRAME       COMP-X PIC 9(002) VALUE 110.
           05 GRBOXC-COLOR-SHADE       COMP-X PIC 9(002) VALUE 008.
           05 GRBOXC-COLOR-BARR-MENU   COMP-X PIC 9(002) VALUE 112.
           05 GRBOXC-ORDER                    PIC 9(001) VALUE 0.
           05 GRBOXC-RETURN                   PIC 9(001) VALUE 1.
           05 GRBOXC-RECORD.
              10 GRBOXC-WORK-NAME             PIC X(008) VALUE
                                                         "GRBOXC##".
              10 GRBOXC-STRING-1              PIC X(040) VALUE SPACES.
              10 GRBOXC-STRING-1-LENGTH       PIC 9(002) VALUE 0.
              10 GRBOXC-STRING-2              PIC X(040) VALUE SPACES.
              10 GRBOXC-STRING-2-LENGTH       PIC 9(002) VALUE 0.
              10 GRBOXC-H-FILE                PIC X(050) VALUE SPACES.
              10 GRBOXC-H-LINE                PIC 9(002) VALUE 1.
              10 GRBOXC-H-COLUMN              PIC 9(002) VALUE 1.
              10 GRBOXC-H-TYPE                PIC 9(001) VALUE 0.
              10 GRBOXC-H-VERTICAL-LENGTH     PIC 9(002) VALUE 22.
              10 GRBOXC-H-HORIZONTAL-LENGTH   PIC 9(002) VALUE 78.
              10 GRBOXC-H-COLOR-SCREEN COMP-X PIC 9(002) VALUE 078.
              10 GRBOXC-H-COLOR-FRAME  COMP-X PIC 9(002) VALUE 078.
              10 GRBOXC-H-COLOR-SHADE  COMP-X PIC 9(002) VALUE 008.

       PROCEDURE DIVISION USING PARAMETROS-GRBOXC.

           ON 1
              CALL "CWGETU" USING NOME TASK PROGRAMA "?".
           MOVE GRBOXC-FUNCTION            TO CWBOXC-FUNCTION
           MOVE GRBOXC-LINE                TO CWBOXC-LINE
           MOVE GRBOXC-COLUMN              TO CWBOXC-COLUMN
           MOVE GRBOXC-TYPE                TO CWBOXC-TYPE
           MOVE GRBOXC-VERTICAL-LENGTH     TO CWBOXC-VERTICAL-LENGTH
           MOVE GRBOXC-HORIZONTAL-LENGTH   TO CWBOXC-HORIZONTAL-LENGTH
           MOVE GRBOXC-OPTION              TO CWBOXC-OPTION
           MOVE GRBOXC-TITLE               TO CWBOXC-TITLE
           MOVE GRBOXC-COLOR-SCREENS       TO CWBOXC-COLOR-FRAME
           MOVE GRBOXC-COLOR-FRAME         TO CWBOXC-COLOR-BORDER
           MOVE GRBOXC-COLOR-SHADE         TO CWBOXC-COLOR-SHADE
           MOVE GRBOXC-COLOR-BARR-MENU     TO CWBOXC-COLOR-BARR-MENU
           MOVE GRBOXC-ORDER               TO CWBOXC-ORDER
           MOVE GRBOXC-RETURN              TO CWBOXC-RETURN
           MOVE SPACES                     TO CWBOXC-WORK-NAME
           MOVE 7                          TO T
           PERFORM VARYING I FROM LENGTH GRBOXC-WORK-NAME BY -1
                     UNTIL I = 0
                   IF GRBOXC-WORK-NAME (I: 1) = '#' OR '$'
                      IF T = 0
                         MOVE '0' TO GRBOXC-WORK-NAME (I: 1)
                      ELSE
                         SUBTRACT 1 FROM T
                         MOVE TASK (T:1) TO GRBOXC-WORK-NAME (I: 1)
                      END-IF
                   END-IF
           END-PERFORM
           STRING '$TEMP/' GRBOXC-WORK-NAME DELIMITED BY SIZE
                                         INTO CWBOXC-WORK-NAME
           MOVE GRBOXC-STRING-1            TO CWBOXC-STRING-1
           MOVE GRBOXC-STRING-1-LENGTH     TO CWBOXC-STRING-1-LENGTH
           MOVE GRBOXC-STRING-2            TO CWBOXC-STRING-2
           MOVE GRBOXC-STRING-2-LENGTH     TO CWBOXC-STRING-2-LENGTH
           MOVE GRBOXC-H-FILE              TO CWBOXC-H-FILE
           MOVE GRBOXC-H-LINE              TO CWBOXC-H-LINE
           MOVE GRBOXC-H-COLUMN            TO CWBOXC-H-COLUMN
           MOVE GRBOXC-H-TYPE              TO CWBOXC-H-TYPE
           MOVE GRBOXC-H-VERTICAL-LENGTH   TO CWBOXC-H-VERTICAL-LENGTH
           MOVE GRBOXC-H-HORIZONTAL-LENGTH TO CWBOXC-H-HORIZONTAL-LENGTH
           MOVE GRBOXC-H-COLOR-SCREEN      TO CWBOXC-H-COLOR-FRAME
           MOVE GRBOXC-H-COLOR-FRAME       TO CWBOXC-H-COLOR-BORDER
           MOVE GRBOXC-H-COLOR-SHADE       TO CWBOXC-H-COLOR-SHADE
           CALL "CWBOXC"                USING PARAMETROS-CWBOXC
           MOVE CWBOXC-OPTION              TO GRBOXC-OPTION
           GOBACK.
