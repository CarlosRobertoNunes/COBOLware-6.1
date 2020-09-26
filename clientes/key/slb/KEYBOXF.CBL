       IDENTIFICATION DIVISION.
       PROGRAM-ID.    KEYBOXF INITIAL.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  29/03/1993.
       SECURITY.      *************************************************
                      *                                               *
                      * Lista de sele‡ao de arquivo derivada da CWBOXF*
                      * especial para a KEYSYSTEM                     *
                      *                                               *
                      *************************************************
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY g:\cobware\cpy\CWCASE.
           05 tab-mouse.
              10 rm pic 9(2).
              10 im pic 9(2).
              10 tb occurs 25.
                 15 e-mouse pic 9(2).
                 15 r-mouse pic 9(2).
           05 COR-ITEM                   PIC  9(001) VALUE 0.
           05 MAX                        PIC  9(002) VALUE 0.
           05 MULTI-COLOR                PIC  X(098) VALUE SPACEs.
           05 COLOR-CHAR                 PIC  X(080) VALUE SPACEs.
           05 REDEFINES COLOR-CHAR.
              06 TESTE-SAME COMP-X PIC 9(2).
              88 SAME-FB2 VALUE 000 017 034 051 068 085 102 119 136 153
                                170 187 204 221 238 255.
              06 filler pic x(79).
           05 COLOR-TAB.
              10 COR-TAB                 PIC  9(003) OCCURS 21.
           05 DIGITADO                   PIC  X(001) VALUE SPACE.
           05 T-PRE                      PIC  X(080) VALUE SPACES.
           05 P                          PIC  9(003) VALUE 0.
           05 SKIP-LER                   PIC  9(002) VALUE 0.
           05 MAPA-HOT.
              10 OCCURS 25.
                 15 HOT-POSIT            PIC  X(002).
                 15 HOT-POSIT-HIGH       PIC  X(002).
                 15 HOT-CHAR             PIC  X(001).
           05 HOT-KEYS                   PIC  X(034) VALUE SPACES.
           05 ENV                        PIC  X(120) VALUE SPACES.
           05 ROW-MOUSE-A         COMP-X PIC  9(004) VALUE 0.
           05 TESTCHAR                   PIC  x(001) VALUE space.
           05 LETRA                      PIC  x(001) VALUE space.
           05 KP                         PIC  9(002) VALUE 0.
           05 tup                        PIC  x(002) VALUE space.
           05 ret-end                    PIC  9(001) VALUE 0.
           05 case-char                  PIC  X(001) VALUE space.
           05 SAVE-VERT                  PIC  9(002) VALUE 0.
           05 LIDOX-1                    PIC  X(120) VALUE SPACES.
           05 LIDOX-2                    PIC  X(120) VALUE SPACES.
           05 MENOR                      PIC  9(001) VALUE 0.
           05 STRINGX.
              10 STRING-1X               PIC  X(120).
              10 STRING-2X               PIC  X(120).
           05 VIEW-OPTION                PIC  X(076) VALUE SPACES.
           05 OFFSET                     PIC  9(003) VALUE 0.
           05 LEITURAS                   PIC  9(002) VALUE 0.
           05 BARRA                      PIC  9(001) VALUE 0.
           05 SAVE-RETURN                PIC  9(001) VALUE 0.
           05 SAVE-ORDER                 PIC  9(001) VALUE 0.
           05 FLAG-FIRST                 PIC  9(001) VALUE 0.
           05 SEM-DADOS                  PIC  9(001) VALUE 1.
           05 MULTI-USER                 PIC  9(001) VALUE 0.
           05 TMP                        PIC  X(003) VALUE SPACE.
           05 X91-RESULT          COMP-X PIC  9(002) VALUE 0.
           05 X91-FUNCTION        COMP-X PIC  9(002) VALUE 16.
           05 X91-PARAMETER       COMP-X PIC  9(002) VALUE 0.
           05 REVERSO                    PIC  9(002) COMP-X VALUE 0.
           05 WRITE-STRING.
              10 STRING-POS.
                 15 STRING-ROW-NUMBER    PIC  9(002) COMP-X VALUE 0.
                 15 STRING-COLUMN-NUMBER PIC  9(002) COMP-X VALUE 0.
              10 STRING-CHAR             PIC  X(120) VALUE SpACES.
              10 STRING-SIZE             PIC  9(004) COMP-X VALUE 0.
           05 TITULO                     PIC  X(078) VALUE SPACES.
           05 VAZIO                      PIC  9(003) VALUE 0.
           05 CC                         PIC  9(002) COMP-X VALUE 0.
           05 HORA-A                     PIC  X(006) VALUE SPACES.
           05 HORA-B                     PIC  X(006) VALUE SPACES.
           05 SEGUNDOS                   PIC  9(004) VALUE 0.
           05 SAVE-OPT                   PIC  X(120) VALUE SPACES.
           05 CWBOXF-STRING-1            PIC  X(120) VALUE SPACES.
           05 CWBOXF-STRING-2            PIC  X(120) VALUE SPACES.
           05 END-STRING-1               PIC  X(120) VALUE SPACES.
           05 END-STRING-2               PIC  X(120) VALUE SPACES.
           05 MSG                        PIC  X(030) VALUE
              "Imposs¡vel executar XXXXXXXX  ".
           05 L-UP                       PIC  9(002) COMP-X VALUE 0.
           05 L-DN                       PIC  9(002) COMP-X VALUE 0.
           05 C-UPDN                     PIC  9(002) COMP-X VALUE 0.
           05 NADA                       PIC  X(001) VALUE SPACE.
           05 ROLA                       PIC  X(001) VALUE "S".
           05 CURPOS.
              10 CURPOS-LIN              PIC  9(002) VALUE ZERO.
              10 CURPOS-COL              PIC  9(002) VALUE ZERO.
           05 CARACTER                   PIC  X(001).
           05 CARACTER-X REDEFINES CARACTER
                                       PIC  9(002) COMP-X.
           05 TECLA                    PIC  9(003) VALUE 0. COPY CWEDIT.
           05 TECLA2                   PIC  9(003) VALUE 0.
           05 VEZ                 COMP PIC  9(001) VALUE 1.
           05 VOLTA                    PIC  9(002) VALUE 0.
           05 C-1                      PIC  9(003) VALUE 0.
           05 C                        PIC  9(003) VALUE 0.
           05 S                        PIC  9(003) VALUE 0.
           05 L                        PIC  9(002) VALUE 0.
           05 L2                       PIC  9(002) VALUE 0.
           05 M-1                      PIC  9(002) VALUE 0.
           05 I                        PIC  9(003) VALUE 0.
           05 J                        PIC  9(002) VALUE 0.
           05 X                        PIC  9(002) VALUE 0.
           05 X2                       PIC  9(002) VALUE 0.
           05 TF                       PIC  9(003) VALUE 0.
           05 T                        PIC  9(003) VALUE 0.
           05 T2                       PIC  9(003) VALUE 0.
           05 Z                        PIC  9(002) VALUE 0.
           05 ZZ                       PIC  9(002) VALUE 0.
           05 Y                        PIC  9(002) VALUE 0.
           05 M                        PIC  9(002) VALUE 0.
           05 M2                       PIC  9(002) VALUE 0.
           05 OPERADOR                 PIC  X(030) VALUE SPACES.
           05 TASK                     PIC  9(006) VALUE 0.
           05 PROGRAMA                 PIC  X(008) VALUE SPACES.
           05 SET-LOG                  PIC  X(001) VALUE "?".
           05 ABERTO                   PIC  X(001) VALUE "N".
           05 FIM                      PIC  X(001) VALUE ">".
           05 WORK-NAME-A              PIC  X(008) VALUE SPACES.
           05 TEXTO-SAVE               PIC  X(120) VALUE SPACES.
           05 TEXTO-X                  PIC  X(120) VALUE SPACES.
           05 TEXTO-A                              VALUE SPACES.
              10 BYTE-A                PIC  X(001) OCCURS 120.
           05 TEXTO-B                              VALUE SPACES.
              10 BYTE-B                PIC  X(001) OCCURS 120.
           05 MATRIZ                               VALUE SPACES.
              07         OCCURS 26.
                10 TEXTO-1             PIC  X(120).
                10 TEXTO-2             PIC  X(120).
                10 TEXTO-L             PIC  X(200).
           05 TABELA-CORES.
              10 COR PIC 9(002) COMP-X OCCURS 256.
           05 TABELA-MOLDURA.
              10 BASE-MOLDURA          PIC  X(008) OCCURS 9.
           05 M-186                    PIC  X(001) VALUE SPACE.
           05 CBL-READ-WRITE-SCR-CHARS-ATTR.
              10 SCREEN-POSITION.
                 15 ROW-NUMBER         PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER      PIC  9(002) COMP-X VALUE 0.
              10 SCREEN-POSITION-X.
                 15 ROW-NUMBER-X       PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER-X    PIC  9(002) COMP-X VALUE 0.
              10 SCREEN-POSITION-X1.
                 15 ROW-NUMBER-X1      PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER-X1   PIC  9(002) COMP-X VALUE 0.
              10 SCREEN-POSITION-X2.
                 15 ROW-NUMBER-X2      PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER-X2   PIC  9(002) COMP-X VALUE 0.
              10 SCREEN-POSITION-H.
                 15 ROW-NUMBER-H       PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER-H    PIC  9(002) COMP-X VALUE 0.
              10 CARACTER-BUFFER       PIC X(2000) VALUE SPACES.
              10 ATTRIBUTE-BARR        PIC  X(001) VALUE SPACES.
              10 ATTRIBUTE-BARR-X REDEFINES
                 ATTRIBUTE-BARR        PIC  9(002) COMP-X.
              10 ATTRIBUTE-BUFFER      PIC X(2000) VALUE SPACES.
              10 STRING-LENGTH         PIC  9(004) COMP-X VALUE 0.
              10 STRING-LENGTH-X       PIC  9(004) COMP-X VALUE 0.
              10 STRING-START          PIC  9(004) COMP-X VALUE 0.
              10 CURSOR-POSITION.
                 15 ROW-CURSOR         PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-CURSOR      PIC  9(002) COMP-X VALUE 0.
              10 SCREEN-POSITION-MOUSE.
                 15 ROW-NUMBER-2       PIC  9(002) COMP-X VALUE 0.
                 15 COLUMN-NUMBER-2    PIC  9(002) COMP-X VALUE 0.
           05 SIZE-BARR-MENU           PIC  9(004) COMP-X VALUE 0.
           05 MOUSE-HANDLE             PIC  9(008) COMP-X VALUE 1.
           05 MOUSE-BUTTONS            PIC  9(002) COMP-X VALUE 3.
           05 MOUSE-POSITION-S         PIC  X(004).
           05 MOUSE-POSITION-A         PIC  X(004).
           05 MOUSE-POSITION.
              10 ROW-MOUSE             PIC  9(004) COMP-X.
              10 COLUMN-MOUSE          PIC  9(004) COMP-X.
           05 SAVE-MOUSE-POSITION      PIC  X(004) VALUE LOW-VALUES.
           05 MOUSE-DATA.
              10 MOUSE-EVENT-TYPE      PIC  9(004) COMP-X VALUE 0.
              10 MOUSE-EVENT-TIME      PIC  9(008) COMP-X VALUE 0.
              10 MOUSE-EVENT-ROW       PIC  9(004) COMP-X VALUE 0.
              10 MOUSE-EVENT-COL       PIC  9(004) COMP-X VALUE 0.
           05 SAVE-MOUSE-EVENT-TYPE    PIC  9(004) COMP-X VALUE 0.
           05 MOUSE-READ-TYPE          PIC  9(002) COMP-X VALUE 0.
           05 BYTE-MOUSE               PIC  X(001) VALUE SPACE.
           05 BYTE-MOUSE-LENGTH        PIC  9(004) COMP-X VALUE 1.
           05 KEY-STATUS               PIC  9(002) COMP-X VALUE 0.
           05 COLUMN-MAX               PIC  9(002) COMP-X VALUE 0.
           05 COLUMN-MIN               PIC  9(002) COMP-X VALUE 0.
           05 ROW-MIN                  PIC  9(002) COMP-X VALUE 0.
           05 ROW-MAX                  PIC  9(002) COMP-X VALUE 0.
           05 E                 COMP-X PIC  9(002) VALUE ZERO.
           05 EX                COMP-X PIC  9(002) VALUE ZERO.
           05 E-ANT             COMP-X PIC  9(002) VALUE ZERO.
           05 COLOR-FRAME       COMP-X PIC  9(002) VALUE ZERO.
           05 COLOR-BARR-MENU   COMP-X PIC  9(002) VALUE ZERO.
           05 COLOR-BARR-MENU-BLINK.
              10 COLOR-BARR-MENU-BLINK-N PIC  9(002) COMP-X VALUE 0.
           05 MOLDURA-DEFAULT            PIC  9(001) VALUE 0.
           05 USER-IO                    PIC  X(001).
              88 TOP-BOT                             VALUE "B" "b"
                                                           "E" "e".
              88 BEGIN-FILE                          VALUE "B" "b".
              88 END-FILE                            VALUE "E" "e".
              88 AT-END                              VALUE "*".
              88 READ-NEXT                           VALUE "N" "n".
              88 READ-PREVIOUS                       VALUE "P" "p".
              88 NOT-LESS                            VALUE ">".
              88 NOT-GREATER                         VALUE "<".
              88 STARTED                             VALUE "<" ">".
           05 COLOR-HOT     PIC  9(002) COMP-X.
           05 COLOR-HOT-BAR PIC  9(002) COMP-X.
           05 ATT-H         PIC  X(001) VALUE X"6A".
           05 ATT-HN REDEFINES ATT-H  PIC  9(002) COMP-X.
              88 SAME-FB VALUE 000 008 017 025 034 042 051 059
                               068 076 085 093 102 110 119 127.
              88 LOW-FB VALUE  000 001 002 003 004 005 006 007
                               016 017 018 019 020 021 022 023
                               032 033 034 035 036 037 038 039
                               048 049 050 051 052 053 054 055
                               064 065 066 067 068 069 070 071
                               080 081 082 083 084 085 086 087
                               096 097 098 099 100 101 102 103
                               112 113 114 115 116 117 118 119.

       COPY CWBOXW.
       COPY CWGETL.
       COPY CWSEND.
       COPY CWUNIX.

       LINKAGE SECTION.

       01  PARAMETROS-CWBOXF.
           05 CWBOXF-FUNCTION                PIC  X(001).
              88 CWBOXF-SHOW                             VALUE "S" "s"
                                                               "P" "p".
              88 CWBOXF-VIEW                             VALUE "V" "v".
              88 CWBOXF-POP-UP                           VALUE "P" "p".
              88 CWBOXF-DISPLAY                          VALUE "D" "d".
              88 CWBOXF-NOLIGTH                          VALUE "d".
              88 CWBOXF-GET                              VALUE "G" "g".
           05 CWBOXF-PROGRAM                 PIC  X(008).
           05 CWBOXF-WORK-AREA               PIC  X(050).
           05 CWBOXF-LINE                    PIC  9(002).
           05 CWBOXF-COLUMN                  PIC  9(002).
           05 CWBOXF-TYPE                    PIC  9(001).
           05 CWBOXF-VERTICAL-LENGTH         PIC  9(002).
           05 CWBOXF-HORIZONTAL-LENGTH       PIC  9(002).
           05 CWBOXF-OPTION                  PIC  X(076).
           05 CWBOXF-KEY-ON                  PIC  X(001).
           05 CWBOXF-EDIT REDEFINES CWBOXF-KEY-ON
                                      COMP-X PIC  9(002).
           05 CWBOXF-KEY                     PIC  9(002).
           05 CWBOXF-START-CURSOR            PIC  9(002) COMP-X.
           05 CWBOXF-TITLE.
              10 CWBOXF-TITLE-BYTE           PIC  X(001) OCCURS 78.
           05 CWBOXF-COLOR-FRAME             PIC  9(002) COMP-X.
           05 CWBOXF-COLOR-BORDER            PIC  9(002) COMP-X.
           05 CWBOXF-COLOR-SHADE             PIC  9(002) COMP-X.
           05 CWBOXF-COLOR-BARR-MENU         PIC  9(002) COMP-X.
           05 CWBOXF-ORDER                   PIC  9(001).
           05 CWBOXF-RETURN                  PIC  9(001).
           05 CWBOXF-STRING-1-LENGTH         PIC  9(002).
           05 CWBOXF-STRING-2-LENGTH         PIC  9(002).
           05 CWBOXF-TIMEOUT-STATUS          PIC  9(001).
              88 CWBOXF-TIMEOUT-ENABLE                   VALUE 1.
              88 CWBOXF-TIMEOUT-DISABLE                  VALUE 0.
           05 CWBOXF-TIMEOUT-RETURN          PIC  9(001).
              88 CWBOXF-TIMEOUT-ON                       VALUE 1.
              88 CWBOXF-TIMEOUT-OFF                      VALUE 0.

       01  MOUSE-POSITION-2                  PIC  X(004).
       01  MOUSE-EVENT-TYPE-2         COMP-X PIC  9(004).
       01  OBJECT-TYPE                       PIC  X(001).
           88 LIST-BOX                            VALUE "l".
           88 AJUSTED                             VALUE "x".
           88 COMBO-BOX                           VALUE "c" "x".
           88 BOX                                 VALUE "c" "l" "x".
       01  CWUSER-KEY                        PIC  9(003).
       01  CONTEXT-ID                        PIC  X(038).
       01  POS-E.
           15 LIN-E                          PIC  9(002).
           15 COL-E                          PIC  9(002).
       01  PIC-E                             PIC  X(080).
       01  ct-lines                          PIC  9(002).
       PROCEDURE DIVISION USING PARAMETROS-CWBOXF
                                MOUSE-POSITION-2
                                MOUSE-EVENT-TYPE-2 OBJECT-TYPE
                                CWUSER-KEY
                                CONTEXT-ID POS-E PIC-E
                                ct-lines.

       000-INICIO.

           INSPECT CWBOXF-OPTION CONVERTING LOW-VALUE TO SPACE
           MOVE 0        TO T
           MOVE 1        TO SEM-DADOS
           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER
           CALL "CWGETL" USING PARAMETROS-CWGETL
           DISPLAY "CWBOXS" UPON ENVIRONMENT-NAME
           MOVE SPACES TO HOT-KEYS LETRA
           ACCEPT  HOT-KEYS FROM ENVIRONMENT-VALUE
           DISPLAY "CWBOXS-COLORS" UPON ENVIRONMENT-NAME
           INITIALIZE MULTI-COLOR COLOR-TAB
           ACCEPT  MULTI-COLOR FROM ENVIRONMENT-VALUE
           IF MULTI-COLOR NOT = SPACES
              MOVE 1        TO T
              PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH MULTI-COLOR
                      IF MULTI-COLOR (I:1) NOT NUMERIC
                         IF COR-TAB (T) NOT = 0
                            ADD  1 TO T
                         END-IF
                      ELSE
                         IF COR-TAB (T) NOT = 0
                            MOVE COR-TAB (T) (2: 2)
                              TO COR-TAB (T) (1: 2)
                         END-IF
                         MOVE MULTI-COLOR (I:1) TO COR-TAB (T) (3:1)
                      END-IF
              END-PERFORM
              MOVE 0        TO T
           END-IF

           IF   VEZ = 1
                DISPLAY "CWMULTI"      UPON ENVIRONMENT-NAME
                ACCEPT  TMP            FROM ENVIRONMENT-VALUE
                INSPECT TMP (1: 2) CONVERTING MINUSCULAS TO MAIUSCULAS
                IF   TMP (1: 2) = "ON"
                     MOVE 1 TO MULTI-USER
                END-IF
                CALL "CWUNIX" USING PARAMETROS-CWUNIX
                IF   CWUNIX-ON
                     MOVE 1 TO MULTI-USER
                END-IF
                CALL "CWMOLD" USING TABELA-CORES TABELA-MOLDURA
txt   *         CALL "CWTEXT" USING MSG LENGTH OF MSG
                MOVE 2            TO VEZ
                MOVE "m"          TO SET-LOG
                CALL  "CWGETU" USING OPERADOR
                                     TASK
                                     PROGRAMA
                                     SET-LOG
                MOVE TASK         TO MOLDURA-DEFAULT
           END-IF

           DISPLAY "CWBOXF"      UPON ENVIRONMENT-NAME
           DISPLAY SPACES        UPON ENVIRONMENT-VALUE
           COMPUTE CC = CWBOXF-COLOR-FRAME + 1
           MOVE    COR (CC) TO COLOR-FRAME
           IF   CWBOXF-TYPE = 0
                MOVE MOLDURA-DEFAULT TO CWBOXF-TYPE
           END-IF

           MOVE "O" TO USER-IO
           MOVE SPACES TO CWBOXF-STRING-1
                          CWBOXF-STRING-2
           ADD  1      TO CWBOXF-VERTICAL-LENGTH
           MOVE CWBOXF-VERTICAL-LENGTH TO SAVE-VERT
           CALL CWBOXF-PROGRAM USING USER-IO CWBOXF-ORDER
                                             CWBOXF-STRING-1
                                             CWBOXF-STRING-2
                                             CWBOXF-VERTICAL-LENGTH
                                             CWBOXF-WORK-AREA
                ON EXCEPTION
                   MOVE SPACES TO CWSEND-MSG CWSEND-SCREENS
                   STRING "Imposs¡vel executar " CWBOXF-PROGRAM
                                                 DELIMITED BY SIZE
                               INTO CWSEND-MSG
                   CALL "CWSEND" USING PARAMETROS-CWSEND END-CALL
                   GOBACK
                NOT EXCEPTION
                    IF AT-END
                    AND ((CWBOXF-STRING-1 NOT = SPACES)
                      OR (CWBOXF-STRING-2 NOT = SPACES))
                       MOVE "O" TO USER-IO
                    END-IF
                    IF AT-END
                       IF   CWBOXF-PROGRAM NOT = "CWREAD"
                            MOVE "C" TO USER-IO
                            CALL CWBOXF-PROGRAM USING USER-IO
                                                      CWBOXF-ORDER
                                                      CWBOXF-STRING-1
                                                      CWBOXF-STRING-2
                                                CWBOXF-VERTICAL-LENGTH
                                                      CWBOXF-WORK-AREA
                            END-CALL
                            CANCEL CWBOXF-PROGRAM
                       END-IF
                       MOVE SPACES TO CWSEND-MSG CWSEND-SCREENS
                       MOVE "Consulta sem informa‡äes" TO CWSEND-MSG
                       CALL "CWSEND" USING PARAMETROS-CWSEND END-CALL
                       GOBACK
                    END-IF
           END-CALL
lula       move spaces to CWBOXF-STRING-1
                          CWBOXF-STRING-2
           IF   CWBOXF-VERTICAL-LENGTH < SAVE-VERT
                MOVE 1 TO MENOR
           ELSE
                SUBTRACT 1 FROM CWBOXF-VERTICAL-LENGTH
           END-IF
           IF   CWBOXF-DISPLAY
           OR   CWBOXF-GET
                PERFORM 203-DISPLAY-COMBO-BOX THRU 203-99-FIM
                GO TO BOX-CLOSE
           ELSE
                PERFORM 200-SHOW    THRU 200-99-FIM
           END-IF

           IF   ROLA = '@'
                GO TO BOX-CLOSE
           END-IF
           IF   CWBOXF-POP-UP
           AND  CWBOXF-OPTION = SPACES
           AND  X91-PARAMETER > 3
           AND  LIST-BOX
                MOVE MOUSE-POSITION   TO SAVE-MOUSE-POSITION
                MOVE MOUSE-EVENT-TYPE TO SAVE-MOUSE-EVENT-TYPE
                SET  CWBOXF-VIEW      TO TRUE
                MOVE VIEW-OPTION      TO CWBOXF-OPTION
                ADD  1                TO CWBOXF-LINE
                PERFORM 200-SHOW THRU 200-99-FIM
                SET  CWBOXF-POP-UP         TO TRUE
                MOVE SAVE-MOUSE-POSITION   TO MOUSE-POSITION
                MOVE SAVE-MOUSE-EVENT-TYPE TO MOUSE-EVENT-TYPE
                move spaces to cwboxf-option
           END-IF

           IF   X91-PARAMETER > 3
           AND  BOX
                ADD 1 TO CWBOXF-LINE
           END-IF.

       BOX-CLOSE.

           IF  HOT-KEYS NOT = SPACES
           AND LETRA = '<' OR '>'
               MOVE LETRA TO CWBOXF-OPTION
           END-IF

           IF   CWBOXF-PROGRAM NOT = "CWREAD"
                CALL CWBOXF-PROGRAM USING "C" CWBOXF-ORDER
                                              CWBOXF-STRING-1
                                              CWBOXF-STRING-2
                                              CWBOXF-VERTICAL-LENGTH
                                              CWBOXF-WORK-AREA
                CANCEL CWBOXF-PROGRAM
           END-IF

           CALL "CWATCH"

           IF   X91-PARAMETER > 1
                MOVE MOUSE-POSITION TO MOUSE-POSITION-2
           END-IF

           IF   X91-PARAMETER > 2
                MOVE MOUSE-EVENT-TYPE TO MOUSE-EVENT-TYPE-2
           END-IF

           IF   ROLA = '@'
                MOVE 'S' TO ROLA
                GO TO 000-99-FIM
           END-IF

           IF   X91-PARAMETER > 3
           AND  COMBO-BOX
                SUBTRACT 1 FROM CWBOXF-LINE
           END-IF.

           IF (CWBOXF-KEY-ON = "*" OR "^")
             IF  TECLA2 > 0
                 move tecla2 to cwboxf-edit
             ELSE
                 IF CWBOXF-OPTION NOT = SPACES
                    MOVE 13 TO CWBOXF-EDIT
                    MOVE 0  TO CWBOXF-KEY
                 END-IF
             END-IF
           END-IF

>>>        IF   X91-PARAMETER > 3
>>>        AND  COMBO-BOX
>>>             SUBTRACT 1 FROM CWBOXF-HORIZONTAL-LENGTH
>>>        END-IF.

       000-99-FIM. GOBACK.

       200-SHOW.

           IF   X91-PARAMETER > 3
           AND  COMBO-BOX
                ADD  1 TO CWBOXF-LINE
                MOVE 8 TO CWBOXF-TYPE
           END-IF
           MOVE CWBOXF-LINE              TO CWBOXW-LINE   L
           MOVE CWBOXF-COLUMN            TO CWBOXW-COLUMN C
           MOVE CWBOXF-TYPE              TO CWBOXW-TYPE
           COMPUTE S      = CWBOXW-TYPE   + 1
           MOVE BASE-MOLDURA (S) (4: 1)  TO M-186
           IF  CWBOXF-HORIZONTAL-LENGTH = 0
               COMPUTE CWBOXF-HORIZONTAL-LENGTH = CWBOXF-STRING-1-LENGTH
                                                + CWBOXF-STRING-2-LENGTH
           END-IF

>>>        IF   X91-PARAMETER > 3
>>>        AND  COMBO-BOX
>>>             ADD 1 TO CWBOXF-HORIZONTAL-LENGTH
>>>        END-IF

           COMPUTE CWBOXW-VERTICAL-LENGTH = CWBOXF-VERTICAL-LENGTH * 3
           MOVE CWBOXF-HORIZONTAL-LENGTH TO CWBOXW-HORIZONTAL-LENGTH
           MOVE CWBOXF-COLOR-FRAME       TO CWBOXW-COLOR-FRAME
           MOVE CWBOXF-COLOR-BORDER      TO CWBOXW-COLOR-BORDER
           MOVE CWBOXF-COLOR-SHADE       TO CWBOXW-COLOR-SHADE
           IF CWBOXF-TITLE = 'CWUSER'
>>>        AND  COMBO-BOX
              MOVE 'I'    TO CWBOXW-TYPE(1:1)
           END-IF
           SET   CWBOXW-OPEN             TO TRUE

           IF   X91-PARAMETER > 3
                MOVE 255  TO CWBOXW-COLOR-SHADE
                IF   LIST-BOX
                     SUBTRACT 1 FROM CWBOXW-LINE
                     MOVE     255 TO CWBOXW-COLOR-SHADE
                END-IF
                IF   COMBO-BOX
                     SUBTRACT 2 FROM CWBOXW-VERTICAL-LENGTH
                     SUBTRACT 1 FROM CWBOXW-HORIZONTAL-LENGTH
                     ADD      1   TO CWBOXW-COLUMN
                END-IF
           END-IF

           CALL "CWBOXW" USING PARAMETROS-CWBOXW

           IF  (CWBOXF-TITLE NOT = SPACES)
           AND (CWBOXF-TITLE NOT = "CWUSER")
      *    AND (CWBOXF-COLOR-SHADE NOT = 0)
                MOVE CWBOXF-TITLE TO TITULO
                COMPUTE Y = C + 2
                COMPUTE STRING-ROW-NUMBER    = L - 1
                COMPUTE STRING-COLUMN-NUMBER = Y - 1
                CALL "CWTEXT" USING TITULO LENGTH OF TITULO
                PERFORM VARYING Y FROM 78 BY -1
                                 UNTIL Y = 0
                                    OR TITULO (Y: 1) NOT = SPACE
                        CONTINUE
                END-PERFORM
                IF   Y NOT = 0
                     MOVE Y TO STRING-SIZE
                     CALL "CBL_READ_SCR_CHARS" USING STRING-POS
                                                     STRING-CHAR
                                                     STRING-SIZE
                END-IF
                MOVE 0           TO STRING-SIZE
                PERFORM VARYING I FROM 1 BY 1 UNTIL I > Y
                   ADD 1 TO STRING-SIZE
                   IF   TITULO (I: 1) NOT = SPACE
                        IF   TITULO (I: 1) = "_"
                             MOVE SPACE TO STRING-CHAR (STRING-SIZE: 1)
                        ELSE
                             MOVE TITULO (I: 1) TO STRING-CHAR
                                                   (STRING-SIZE: 1)
                        END-IF
                   END-IF
                END-PERFORM
                CALL "CBL_WRITE_SCR_CHARS" USING STRING-POS
                                                 STRING-CHAR
                                                 STRING-SIZE
                IF HOT-KEYS NOT = SPACES
                   CALL "CBL_SET_CSR_POS" USING X"FFFF"
                   IF   CWUNIX-OFF
                        MOVE X"1B"   TO STRING-CHAR
                   ELSE
                        MOVE "<"     TO STRING-CHAR
                   END-IF
                   SUBTRACT 1 FROM STRING-COLUMN-NUMBER
                   CALL "CBL_WRITE_SCR_CHARS" USING STRING-POS
                                                    STRING-CHAR
                                                    X"0001"
                   IF   CWUNIX-OFF
                        MOVE X"1A"  TO STRING-CHAR
                   ELSE
                        MOVE ">"    TO STRING-CHAR
                   END-IF
                   ADD CWBOXW-HORIZONTAL-LENGTH TO STRING-COLUMN-NUMBER
                   SUBTRACT 1 FROM STRING-COLUMN-NUMBER
                   CALL "CBL_WRITE_SCR_CHARS" USING STRING-POS
                                                    STRING-CHAR
                                                    X"0001"
                END-IF
           END-IF

           IF   CWBOXF-COLOR-BARR-MENU = 112
                COMPUTE REVERSO = CWBOXF-COLOR-FRAME - 16
                COMPUTE CC = REVERSO + 1
           ELSE
                COMPUTE CC = CWBOXF-COLOR-BARR-MENU + 1
           END-IF

           IF   CWGETL-HIGH NOT = 1
                MOVE 112 TO COR (CC)
           END-IF

           IF   X91-PARAMETER > 3
           AND  BOX
                SUBTRACT 1 FROM CWBOXF-LINE
           END-IF
           MOVE    COR (CC) TO COLOR-BARR-MENU
           IF   CWUNIX-OFF
                ADD 128 TO CC
           END-IF
           MOVE    COR (CC) TO COLOR-BARR-MENU-BLINK-N
           COMPUTE ROW-MIN    = CWBOXF-LINE - 1
           COMPUTE ROW-MAX    = ROW-MIN + (CWBOXF-VERTICAL-LENGTH * 3)
           + 1
           COMPUTE COLUMN-MIN = CWBOXF-COLUMN + 1
           COMPUTE COLUMN-MAX = COLUMN-MIN
                              + CWBOXF-HORIZONTAL-LENGTH - 1

           IF   CWGETL-MOUSE = 1
                CALL "CBL_INIT_MOUSE" USING MOUSE-HANDLE
                                            MOUSE-BUTTONS
                     ON exception
                        CONTINUE
                END-CALL
                CALL "CBL_GET_MOUSE_POSITION"
                      USING MOUSE-HANDLE
                            MOUSE-POSITION
                  ON   exception
                      CONTINUE
                END-CALL
                MOVE MOUSE-POSITION   TO MOUSE-POSITION-A
                                         MOUSE-POSITION-S
           END-IF

           IF HOT-KEYS NOT = SPACES
              INSPECT HOT-KEYS CONVERTING MINUSCULAS TO MAIUSCULAS
              IF CWGETL-HIGH = 1
                 MOVE COLOR-FRAME TO ATT-HN
                 IF  LOW-FB
                     ADD 8 TO ATT-HN
                 ELSE
                      SUBTRACT 8 FROM ATT-HN
                 END-IF
                 IF SAME-FB ADD 1 TO ATT-HN END-IF
                 MOVE ATT-HN TO COLOR-HOT

                 MOVE COLOR-BARR-MENU TO ATT-HN
                 IF  LOW-FB
                     ADD 8 TO ATT-HN
                 ELSE
                      SUBTRACT 8 FROM ATT-HN
                 END-IF
                 IF SAME-FB ADD 1 TO ATT-HN END-IF
                 MOVE ATT-HN TO COLOR-HOT-BAR
              ELSE
                 MOVE COLOR-FRAME     TO COLOR-HOT
                 MOVE COLOR-BARR-MENU TO COLOR-HOT-BAR
              END-IF
           END-IF

           PERFORM 205-INICIO-BOXWRK THRU 205-99-FIM

           MOVE ">" TO FIM
           ADD  1   TO C

           IF   CWBOXF-STRING-1-LENGTH NOT = 0
                COMPUTE I = 2 + CWBOXF-STRING-1-LENGTH
                COMPUTE Y = I - 3
           ELSE
                MOVE 2 TO I
           END-IF

           PERFORM 210-MONTA-TELA THRU 210-99-FIM

           COMPUTE L-UP   = CWBOXF-LINE   + 1
           COMPUTE L-DN   = CWBOXF-LINE   + (CWBOXF-VERTICAL-LENGTH * 3)
           COMPUTE C-UPDN = CWBOXF-COLUMN + CWBOXF-HORIZONTAL-LENGTH + 1

           SET CWBOXF-TIMEOUT-OFF TO TRUE
           IF   CWGETL-TIMEOUT NOT = 0
           AND  CWBOXF-TIMEOUT-ENABLE
                ACCEPT HORA-A FROM TIME
                MOVE 0 TO SEGUNDOS
           END-IF

           IF  SEM-DADOS = 1
SC             IF X91-PARAMETER > 4 AND (NOT  CWBOXF-VIEW)
SC                CONTINUE
SC             ELSE
                  GO TO FIM-SHOW
SC             END-IF
           END-IF
           MOVE CWBOXF-OPTION TO VIEW-OPTION
           MOVE 0 TO ROW-MOUSE-A
           PERFORM TEST AFTER UNTIL EDIT-ESC OR EDIT-ENTER
                                             OR CWBOXF-VIEW
                   PERFORM UNTIL M = 1
                              OR TEXTO-L (M) NOT = SPACES
                              SUBTRACT 1 FROM M
                              SUBTRACT 3 FROM   L
                   END-PERFORM
                   EVALUATE TRUE
SC                    WHEN SEM-DADOS = 1
SC                         CONTINUE
                      WHEN MENOR = 1
                           MOVE "*" TO FIM
                      WHEN FIM = "N"
                           IF   CWUNIX-ON
                                MOVE "^" TO STRING-CHAR
                           ELSE
                                MOVE X"18" TO STRING-CHAR
                           END-IF
                           COMPUTE STRING-ROW-NUMBER    = L-UP   - 1
                           COMPUTE STRING-COLUMN-NUMBER = C-UPDN - 1
                           MOVE    1 TO STRING-SIZE
                           CALL "CBL_WRITE_SCR_CHARS" USING STRING-POS
                                                            STRING-CHAR
                                                            STRING-SIZE
                           COMPUTE STRING-ROW-NUMBER    = L-DN   - 1
                           IF   CWUNIX-ON
                                MOVE "v" TO STRING-CHAR
                           ELSE
                                MOVE X"19" TO STRING-CHAR
                           END-IF
                           CALL "CBL_WRITE_SCR_CHARS" USING STRING-POS
                                                            STRING-CHAR
                                                            STRING-SIZE
                      WHEN FIM = ">"
                           MOVE M-186 TO STRING-CHAR
                           IF   M > 1
                                IF   CWUNIX-ON
                                     MOVE "^"   TO STRING-CHAR
                                ELSE
                                     MOVE X"18" TO STRING-CHAR
                                END-IF
                           END-IF
                           COMPUTE STRING-ROW-NUMBER    = L-UP   - 1
                           COMPUTE STRING-COLUMN-NUMBER = C-UPDN - 1
                           MOVE    1 TO STRING-SIZE
                           CALL "CBL_WRITE_SCR_CHARS" USING STRING-POS
                                                            STRING-CHAR
                                                            STRING-SIZE
                           COMPUTE STRING-ROW-NUMBER    = L-DN   - 1
                           IF   CWUNIX-ON
                                MOVE "v" TO STRING-CHAR
                           ELSE
                                MOVE X"19" TO STRING-CHAR
                           END-IF
                           CALL "CBL_WRITE_SCR_CHARS" USING STRING-POS
                                                            STRING-CHAR
                                                            STRING-SIZE
                      WHEN FIM = "<"
                           IF   CWUNIX-ON
                                MOVE "^" TO STRING-CHAR
                           ELSE
                                MOVE X"18" TO STRING-CHAR
                           END-IF
                           COMPUTE STRING-ROW-NUMBER    = L-UP   - 1
                           COMPUTE STRING-COLUMN-NUMBER = C-UPDN - 1
                           MOVE    1 TO STRING-SIZE
                           CALL "CBL_WRITE_SCR_CHARS" USING STRING-POS
                                                            STRING-CHAR
                                                            STRING-SIZE
                           IF   X91-PARAMETER > 3
                           AND  COMBO-BOX
                                CONTINUE
                           ELSE
                                COMPUTE STRING-ROW-NUMBER = L-DN - 1
                                MOVE M-186 TO STRING-CHAR
                                CALL "CBL_WRITE_SCR_CHARS" USING
                                                           STRING-POS
                                                           STRING-CHAR
                                                           STRING-SIZE
                           END-IF
                   END-EVALUATE
                   MOVE CWBOXF-HORIZONTAL-LENGTH TO SIZE-BARR-MENU
                   MOVE 0 TO TECLA BARRA
                   MOVE E TO E-ANT
                   PERFORM TEST AFTER UNTIL KEY-STATUS = 1
                                         OR MOUSE-EVENT-TYPE > 1
                                         OR CWBOXF-VIEW

                           IF  MULTI-USER = 1
                               MOVE 1 TO KEY-STATUS
                           ELSE
                               CALL "CBL_GET_KBD_STATUS"
                                USING KEY-STATUS
                           END-IF
                           IF   CWGETL-MOUSE = 1
                                CALL "CBL_GET_MOUSE_POSITION"
                                                   USING MOUSE-HANDLE
                                                         MOUSE-POSITION
                                ON   exception
                                     CONTINUE
                                END-CALL
                                IF MOUSE-POSITION-A NOT = MOUSE-POSITION
*********************************************
                                   MOVE 0 TO SEGUNDOS
                                END-IF
                                MOVE MOUSE-POSITION TO MOUSE-POSITION-A
                                CALL "CBL_READ_MOUSE_EVENT"
                                     USING MOUSE-HANDLE
                                           MOUSE-DATA
                                           MOUSE-READ-TYPE
                                ON   exception
                                     CONTINUE
                                END-CALL
                           END-IF
                           CALL "CWATCH"
                           IF   MOUSE-EVENT-TYPE = 2 OR 3
                                MOVE ROW-MOUSE    TO ROW-NUMBER-2
                                MOVE COLUMN-MOUSE TO COLUMN-NUMBER-2
                                CALL "CBL_READ_SCR_CHARS"
                                 USING SCREEN-POSITION-MOUSE
                                              BYTE-MOUSE
                                              BYTE-MOUSE-LENGTH
                                   ON   exception
                                        CONTINUE
                                   END-CALL
                           END-IF
                           IF (((COLUMN-MOUSE NOT < (COLUMN-MIN - 1))
                           AND  (COLUMN-MOUSE NOT > (COLUMN-MAX - 1))
                           AND  (ROW-MOUSE    NOT < ROW-MIN)
                           AND  (ROW-MOUSE    NOT > ROW-MAX))
                           OR  (BYTE-MOUSE = X"18" OR X"19")
                           OR  (BYTE-MOUSE = "^" OR "v"))
                           AND((MOUSE-POSITION NOT = MOUSE-POSITION-S)
                            OR MOUSE-EVENT-TYPE = 2 OR 3)
                                initialize tab-mouse
                                move 1       to e
                                move ROW-MIN to rm im
                                add 1 to rm
                                perform until im > row-mouse
                                        move e  to e-mouse (im)
                                        move rm to r-mouse (im)
                                        add  1  to im
                                        move e  to e-mouse (im)
                                        move rm to r-mouse (im)
                                        add  1  to im
                                        move e  to e-mouse (im)
                                        move rm to r-mouse (im)
                                        add  1  to im
                                        add  1  to e
                                        add  3  to rm
                                end-perform
                                move e-mouse (row-mouse) to e
      *                         COMPUTE E = ROW-MOUSE - ROW-MIN
                                MOVE E TO EX
                                IF  (E NOT = E-ANT)
                                AND (BYTE-MOUSE NOT = "^")
                                AND (BYTE-MOUSE NOT = "v")
                                AND (BYTE-MOUSE NOT = X"18")
                                AND (BYTE-MOUSE NOT = X"19")
                                AND (TEXTO-L (E) NOT = SPACES)
                                AND (ROW-MOUSE   NOT = ROW-MIN)
                                AND (ROW-MOUSE   NOT = ROW-MAX)
                                    MOVE E TO E-ANT
                                    MOVE 0 TO SEGUNDOS
                                    COMPUTE CC = CWBOXF-COLOR-FRAME  + 1
                                    IF   X91-PARAMETER > 3
                                    AND  COMBO-BOX
                                         MOVE SCREEN-POSITION
                                           TO SCREEN-POSITION-X
                                         ADD  1  TO COLUMN-NUMBER-X
                                         COMPUTE STRING-SIZE =
                                                 SIZE-BARR-MENU - 1
                                          CALL "CBL_WRITE_SCR_N_ATTR"
                                                USING SCREEN-POSITION-X
                                                      COR (CC)
                                                      STRING-SIZE
                                    ELSE
                                         MOVE SCREEN-POSITION TO
                                              SCREEN-POSITION-X
                                              SCREEN-POSITION-X1
                                          ADD 1 TO ROW-NUMBER-X
                                          ADD 2 TO ROW-NUMBER-X1
                                         CALL "CBL_WRITE_SCR_N_ATTR"
                                               USING SCREEN-POSITION
                                                     COR (CC)
                                                     SIZE-BARR-MENU
                                         CALL "CBL_WRITE_SCR_N_ATTR"
                                               USING SCREEN-POSITION-X
                                                     COR (CC)
                                                     SIZE-BARR-MENU
                                         CALL "CBL_WRITE_SCR_N_ATTR"
                                               USING SCREEN-POSITION-X1
caca                                                 COR (CC)
                                                     SIZE-BARR-MENU
                                         PERFORM 320-REACENDE
                                            THRU 320-99-FIM
                                    END-IF
                                    move r-mouse (row-mouse)
                                      to ROW-NUMBER
                                    COMPUTE L          = ROW-NUMBER + 1
                                    move e to m
      *                             COMPUTE L          = ROW-MOUSE + 1
      *                             MOVE    ROW-MOUSE TO ROW-NUMBER
      *                             COMPUTE M          = L - CWBOXF-LINE
                                    PERFORM 202-BARRA THRU 202-99-FIM
                                END-IF
                                EVALUATE TRUE
                                    WHEN BYTE-MOUSE = "^"
                                         SET EDIT-CURSOR-UP   TO TRUE
                                    WHEN BYTE-MOUSE = "v"
                                         SET EDIT-CURSOR-DOWN TO TRUE
                                    WHEN BYTE-MOUSE = X"18"
                                         SET EDIT-CURSOR-UP   TO TRUE
                                    WHEN BYTE-MOUSE = X"19"
                                         SET EDIT-CURSOR-DOWN TO TRUE
                                    WHEN ROW-MOUSE = ROW-MIN
                                         SET EDIT-PAGE-UP     TO TRUE
                                    WHEN ROW-MOUSE = ROW-MAX
                                         SET EDIT-PAGE-DOWN   TO TRUE
                                    WHEN EX = E-ANT
                                     AND MOUSE-EVENT-TYPE > 1
                                     AND X91-PARAMETER < 3
      *                                  IF  CWBOXF-RETURN = 1
      *                                      MOVE TEXTO-1 (EX)
      *                                        TO CWBOXF-OPTION
      *                                  ELSE
      *                                      MOVE TEXTO-2 (EX)
      *                                        TO CWBOXF-OPTION
      *                                  END-IF
                                         MOVE EX TO M
                                         IF  MOUSE-EVENT-TYPE = 2
                                             SET EDIT-ENTER TO TRUE
                                         ELSE
                                             SET EDIT-ESC TO TRUE
                                         END-IF
                                         EXIT PERFORM
                                END-EVALUATE
                            ELSE
                                IF   MOUSE-EVENT-TYPE > 1
                                     MOVE 4 TO MOUSE-EVENT-TYPE
                                     IF  CWBOXF-RETURN = 1
                                         MOVE TEXTO-1 (M) TO ENV
                                     ELSE
                                         MOVE TEXTO-2 (M) TO ENV
                                     END-IF
                                     DISPLAY "CWBOXF"
                                             UPON ENVIRONMENT-NAME
                                     DISPLAY ENV
                                             UPON ENVIRONMENT-VALUE
                                ELSE
                                     MOVE 0 TO MOUSE-EVENT-TYPE
                                END-IF
                           END-IF
                           move MOUSE-POSITION to MOUSE-POSITION-s
                   IF   CWBOXF-OPTION NOT = SPACES
                        IF   CWBOXF-OPTION = SAVE-OPT
                             MOVE 1 TO FLAG-FIRST
                        ELSE
                             MOVE 0 TO FLAG-FIRST
                        END-IF
                        PERFORM VARYING X2 FROM LENGTH OF CWBOXF-OPTION
                                             BY -1
                                UNTIL CWBOXF-OPTION (X2: 1) NOT = SPACE
                                CONTINUE
                        END-PERFORM
                        PERFORM VARYING X
                                   FROM 1 BY 1 UNTIL X > X2
                                MOVE CWBOXF-OPTION (X: 1) TO CARACTER
                                PERFORM 201-LER-TECLA THRU 201-99-FIM
                        END-PERFORM
                        IF   CWBOXF-START-CURSOR = 32
                             MOVE 0             TO T
                        ELSE
                             IF   CWBOXF-START-CURSOR < 81
                                  MOVE CWBOXF-START-CURSOR TO T
md                                if ret-end = 0
md                                and t > 0
bug                                   SUBTRACT 1 FROM T
md                                else
md                                    if ret-end = 2
md                                    and t > 0
md                                    and m > 0
md                                        move cwboxf-option(t:1)
                                            to tup(1:1)
                                           compute t2 = t + 1
md                                         move texto-l(m)(t2: 1)
                                            to tup(2:1)
                                          inspect tup converting
                                                  minusculas
                                               to maiusculas
                                          if tup(1:1) not = tup(2:1)
md                                           subtract 1 from t
md                                        end-if
md                                    end-if
md                                end-if
                             END-IF
                        END-IF
md                      move 0 to ret-end
                        PERFORM 310-POSIT-END THRU 310-99-FIM
                        MOVE SPACES TO CWBOXF-OPTION
                   ELSE
                        IF   CWGETL-TIMEOUT NOT = 0
                        AND  CWBOXF-TIMEOUT-ENABLE
                             ACCEPT HORA-B FROM TIME
                             IF   HORA-B NOT = HORA-A
                                  MOVE HORA-B TO HORA-A
                                  ADD 1 TO SEGUNDOS
                                  IF   SEGUNDOS NOT < CWGETL-TIMEOUT
                                       MOVE 1 TO KEY-STATUS
                                       SET CWBOXF-TIMEOUT-ON TO TRUE
                                       SET EDIT-ESC TO TRUE
                                       IF   X91-PARAMETER > 4
                                       AND (NOT CWBOXF-VIEW)
                                            MOVE TECLA TO CWUSER-KEY
                                       END-IF
                                  END-IF
                             END-IF
                        END-IF
                   END-IF
                   IF  BARRA = 0
                       MOVE L                        TO ROW-NUMBER
                       MOVE C                        TO COLUMN-NUMBER
                       SUBTRACT 1                  FROM COLUMN-NUMBER
                                                        ROW-NUMBER
                       PERFORM 202-BARRA THRU 202-99-FIM
                       MOVE "S" TO ROLA
                       MOVE ROW-NUMBER TO ROW-CURSOR
                       EVALUATE TRUE
                         WHEN CWBOXF-ORDER = 1
                              COMPUTE COLUMN-CURSOR = C + T
                         WHEN CWBOXF-ORDER = 2
                              COMPUTE COLUMN-CURSOR = I + C - 2 + T
                         WHEN OTHER
                              MOVE 255 TO ROW-CURSOR
                                          COLUMN-CURSOR
                       END-EVALUATE
                       IF HOT-KEYS = SPACES
                       OR CURSOR-POSITION = X"FFFF"
                          CALL "CBL_SET_CSR_POS" USING CURSOR-POSITION
                       END-IF
                       MOVE 1 TO BARRA
                   END-IF
                   END-PERFORM
                   IF  (MOUSE-EVENT-TYPE = 0
                   AND  CWBOXF-TIMEOUT-OFF)
                   OR   CWBOXF-VIEW
                        MOVE 0 TO SEGUNDOS
                        COMPUTE CURPOS-LIN = ROW-CURSOR    + 1
                        COMPUTE CURPOS-COL = COLUMN-CURSOR + 1
                        IF   CWBOXF-VIEW
                             MOVE X"00" TO CARACTER
                             IF  VIEW-OPTION NOT = SPACES
                                 SET EDIT-ENTER TO TRUE
                             ELSE
                                 SET EDIT-CONTROL-PAGE-DOWN TO TRUE
                             END-IF
                        ELSE
                             IF HOT-KEYS = SPACES
                                IF   X91-PARAMETER > 3
                                     CALL "CWKBDC" USING CURPOS
                                                    CARACTER TECLA
                                ELSE
                                     CALL "CWKBDC" USING "0000"
                                                    CARACTER TECLA
                                END-IF
                             ELSE
                                CALL "CWKBDC" USING "0000"
                                                    CARACTER TECLA
                                PERFORM 030-ALT THRU 030-99-FIM
                             END-IF
                             IF   MENOR = 1
                             AND (
                                      EDIT-CONTROL-PAGE-DOWN
                                   OR EDIT-CONTROL-PAGE-UP
                           OR (EDIT-END  AND M = CWBOXF-VERTICAL-LENGTH)
                                   OR (EDIT-HOME AND M = 1)
                                   OR EDIT-PAGE-DOWN
                                   OR EDIT-PAGE-UP
                                  )
                                   MOVE 0 TO TECLA
                                   EXIT PERFORM CYCLE
                             END-IF
BUG                          IF   M > 1
                             OR (
                                  EDIT-CONTROL-PAGE-UP   OR
                                  EDIT-HOME              OR
                                  EDIT-CURSOR-UP         OR
                                  EDIT-PAGE-UP
                             )
BUG                          AND  FIM = ">"
BUG                               MOVE "N" TO FIM
BUG                          END-IF
                             IF   X91-PARAMETER > 6
                             AND  (EDIT-F1 OR EDIT-ALT-H)
                                   CALL "CWEHLP" USING CONTEXT-ID
                                                       TECLA
                                                       POS-E "C"
                             END-IF
                             IF   X91-PARAMETER > 4
                             AND (NOT CWBOXF-VIEW)
                                  MOVE TECLA TO CWUSER-KEY
                                  IF  (EDIT-CURSOR-LEFT
ANA                               OR   EDIT-SHIFT-TAB
                                  OR   EDIT-BACKSPACE)
                                  AND  T = 0
                                       SET EDIT-CURSOR-UP TO TRUE
                                       MOVE TECLA TO CWUSER-KEY
                                       SET EDIT-ESC TO TRUE
                                  END-IF
                                  IF  EDIT-TAB
                                      SET EDIT-ESC TO TRUE
                                  END-IF
                             END-IF
                        END-IF
                        IF   EDIT-BACKSPACE
                             MOVE X"08" TO CARACTER
                        END-IF
                        IF   CARACTER NOT = X"00"
                             PERFORM 201-LER-TECLA THRU 201-99-FIM
                        ELSE
                             MOVE SPACE      TO CARACTER
                             IF   CWBOXF-KEY-ON = "Y" OR "y"
                                               OR "S" OR "s"
                                  EVALUATE TRUE
                                     WHEN EDIT-F1  MOVE 02 TO CWBOXF-KEY
                                     WHEN EDIT-F2  MOVE 03 TO CWBOXF-KEY
                                     WHEN EDIT-F3  MOVE 04 TO CWBOXF-KEY
                                     WHEN EDIT-F4  MOVE 05 TO CWBOXF-KEY
                                     WHEN EDIT-F5  MOVE 06 TO CWBOXF-KEY
                                     WHEN EDIT-F6  MOVE 07 TO CWBOXF-KEY
                                     WHEN EDIT-F7  MOVE 08 TO CWBOXF-KEY
                                     WHEN EDIT-F8  MOVE 09 TO CWBOXF-KEY
                                     WHEN EDIT-F9  MOVE 10 TO CWBOXF-KEY
                                     WHEN EDIT-F10 MOVE 11 TO CWBOXF-KEY
                                  END-EVALUATE
                                  IF   CWBOXF-KEY NOT = 0
                                       SET EDIT-ENTER TO TRUE
                                  END-IF
                             ELSE
                                  IF  (CWBOXF-KEY-ON = "*" OR "^")
                                  AND (NOT EDIT-CURSOR-DOWN)
                                  AND (NOT EDIT-CURSOR-UP)
                                  AND (NOT EDIT-ESC)
                                  AND (NOT EDIT-CURSOR-LEFT)
                                  AND (NOT EDIT-CURSOR-RIGHT)
                                  AND (NOT EDIT-ENTER)
                                  AND (NOT EDIT-BACKSPACE)
                                  AND (NOT EDIT-CONTROL-PAGE-DOWN)
                                  AND (NOT EDIT-CONTROL-PAGE-UP)
                                  AND (NOT EDIT-END)
                                  AND (NOT EDIT-ESC)
                                  AND (NOT EDIT-HOME)
                                  AND (NOT EDIT-PAGE-DOWN)
                                  AND (NOT EDIT-PAGE-UP)
                                  MOVE TECLA     TO TECLA2
                                       IF  CWBOXF-KEY-ON = "*" OR "^"
                                       AND (NOT CWBOXF-VIEW)
                                           IF X91-PARAMETER > 4
                                               MOVE TECLA TO CWUSER-KEY
                                           END-IF
                                           SET EDIT-ENTER TO TRUE
                                       END-IF
                                       IF  CWBOXF-RETURN = 1
                                           MOVE TEXTO-1 (M)
                                             TO CWBOXF-OPTION
                                       ELSE
                                           MOVE TEXTO-2 (M)
                                             TO CWBOXF-OPTION
                                       END-IF
                                       CALL CWBOXF-PROGRAM USING "$"
                                            CWBOXF-ORDER
                                            CWBOXF-STRING-1
                                            CWBOXF-STRING-2
                                            CWBOXF-VERTICAL-LENGTH
                                            CWBOXF-WORK-AREA
                                            TECLA2
                                            CWBOXF-OPTION
                                       END-IF
                                       IF  CWBOXF-KEY-ON = "^"
                                           MOVE SPACES TO CWBOXF-OPTION
                                       END-IF
                             END-IF
                        END-IF
                   ELSE
                        IF   MOUSE-EVENT-TYPE = 2 OR 3
                             IF  (BYTE-MOUSE NOT = X"18")
                             AND (BYTE-MOUSE NOT = X"19")
                             AND (BYTE-MOUSE NOT = X"5E")
                             AND (BYTE-MOUSE NOT = X"76")
                             AND (ROW-MOUSE  NOT = ROW-MIN)
                             AND (ROW-MOUSE  NOT = ROW-MAX)
                                  IF ROW-MOUSE = ROW-MOUSE-A
                                     SET  EDIT-ENTER TO TRUE
                                     MOVE 0 TO ROW-MOUSE-A
                                  ELSE
                                     MOVE ROW-MOUSE TO ROW-MOUSE-A
                                  END-IF
                             END-IF
                             MOVE SPACE TO BYTE-MOUSE
                        ELSE
                             IF   MOUSE-EVENT-TYPE NOT = 9
                                  SET EDIT-ESC TO TRUE
                             END-IF
                        END-IF
                   END-IF
                   IF COR-TAB (M) = 0
                      COMPUTE CC = CWBOXF-COLOR-FRAME + 1
                   ELSE
                      COMPUTE CC = COR-TAB (M) + 1
                   END-IF
                   IF   X91-PARAMETER > 3
                   AND  COMBO-BOX
                        MOVE SCREEN-POSITION TO SCREEN-POSITION-X
                        ADD  1               TO COLUMN-NUMBER-X
pep                                             STRING-SIZE
                        COMPUTE STRING-SIZE = SIZE-BARR-MENU - 1
                        CALL "CBL_WRITE_SCR_N_ATTR" USING
                                                    SCREEN-POSITION-X
                                                    COR (CC)
                                                    STRING-SIZE
pep                     SUBTRACT 1 FROM STRING-SIZE
                   ELSE
                        IF   X91-PARAMETER > 3
                        AND  LIST-BOX
                        AND  MOUSE-EVENT-TYPE = 4
                             CONTINUE
                        ELSE
                             MOVE SCREEN-POSITION TO
                                  SCREEN-POSITION-X
                                  SCREEN-POSITION-X1
                             ADD 1 TO ROW-NUMBER-X
                             ADD 2 TO ROW-NUMBER-X1
                             CALL "CBL_WRITE_SCR_N_ATTR" USING
                                                         SCREEN-POSITION
                                                         COR (CC)
                                                         SIZE-BARR-MENU
                             CALL "CBL_WRITE_SCR_N_ATTR"
                                   USING SCREEN-POSITION-X
                                         COR (CC)
                                         SIZE-BARR-MENU
                             CALL "CBL_WRITE_SCR_N_ATTR"
                                   USING SCREEN-POSITION-X1
caca                                     COR (CC)
                                         SIZE-BARR-MENU
                             PERFORM 320-REACENDE
                                THRU 320-99-FIM
                        END-IF
                   END-IF
                   CALL "CWATCH"
                   EVALUATE TRUE
                      WHEN X91-PARAMETER > 3
                       AND LIST-BOX
                       AND MOUSE-EVENT-TYPE = 4
                           MOVE "@" TO ROLA
                      WHEN EDIT-CURSOR-RIGHT
                       AND CWBOXF-ORDER = 1
                       AND T < CWBOXF-STRING-1-LENGTH
                           ADD 1 TO T
                      WHEN EDIT-CURSOR-RIGHT
                       AND CWBOXF-ORDER = 2
                       AND T < CWBOXF-STRING-2-LENGTH
                           ADD 1 TO T
                      WHEN EDIT-CURSOR-LEFT
                       AND T > 0
                           SUBTRACT 1 FROM T
                   END-EVALUATE
                   IF  ROLA = "S"
                       EVALUATE TRUE
                          WHEN EDIT-HOME
                           AND T > 0
                               MOVE    0 TO T
                          WHEN EDIT-HOME
                           AND M > 1
                               MOVE    1 TO M
                               COMPUTE L  = CWBOXF-LINE + 1
                          WHEN EDIT-END
                           AND (L NOT =
                                 (CWBOXF-LINE + CWBOXF-VERTICAL-LENGTH))
                               MOVE CWBOXF-VERTICAL-LENGTH TO M
                               COMPUTE L = CWBOXF-LINE
                                         + CWBOXF-VERTICAL-LENGTH
                                   + ((CWBOXF-VERTICAL-LENGTH - 1) * 3)
                          WHEN EDIT-CURSOR-DOWN
                           AND M < CWBOXF-VERTICAL-LENGTH
                               ADD 1 TO M
                               ADD 3 TO   L
                          WHEN EDIT-CURSOR-UP
                           AND M > 1
                               SUBTRACT 1 FROM M
                               SUBTRACT 3 FROM   L
                          WHEN EDIT-CURSOR-DOWN
                           AND FIM NOT = "<"
                           AND M = CWBOXF-VERTICAL-LENGTH
                               MOVE "N"  TO FIM
                               SET READ-NEXT TO TRUE
                               PERFORM 250-LER THRU 250-99-FIM
                               IF  AT-END
                                   MOVE "<" TO FIM
                                   SET READ-PREVIOUS TO TRUE
                                   PERFORM 250-LER THRU 250-99-FIM
                               ELSE
                                   MOVE "N" TO FIM
                                   PERFORM VARYING M FROM 2 BY 1
                                      UNTIL M > CWBOXF-VERTICAL-LENGTH
                                      COMPUTE M-1 = M - 1
                                      MOVE TEXTO-1 (M) TO TEXTO-1 (M-1)
                                      MOVE TEXTO-2 (M) TO TEXTO-2 (M-1)
                                      MOVE TEXTO-L (M) TO TEXTO-L (M-1)
                                      IF COR-ITEM = 1
                                         MOVE COR-TAB (M)
                                           TO COR-TAB (M-1)
                                      END-IF
                                   END-PERFORM
                                   MOVE CWBOXF-VERTICAL-LENGTH TO M
                                   PERFORM 215-DISCO-MEMORIA
                                   THRU    215-99-FIM
                                   PERFORM 220-EXIBE-TELA
                                   THRU    220-99-FIM
                               END-IF
                          WHEN EDIT-CONTROL-PAGE-UP
                            OR EDIT-HOME
                               PERFORM 205-INICIO-BOXWRK THRU 205-99-FIM
                               PERFORM 210-MONTA-TELA    THRU 210-99-FIM
                               MOVE ">" TO FIM
                          WHEN EDIT-CONTROL-PAGE-DOWN
                            OR EDIT-END
                               MOVE    "N"              TO FIM
                               PERFORM 206-FIM-BOXWRK THRU 206-99-FIM
                          WHEN ((EDIT-CURSOR-UP AND M = 1)
                            OR EDIT-PAGE-UP)
                           AND FIM NOT = ">"
                               MOVE "N"  TO FIM
                               IF   EDIT-PAGE-UP
                                    ADD CWBOXF-VERTICAL-LENGTH  TO VOLTA
                               ELSE
                                    ADD 1 TO VOLTA
                               END-IF
                               MOVE TEXTO-1 (1) TO LIDOX-1
                               MOVE TEXTO-2 (1) TO LIDOX-2
                               PERFORM VOLTA TIMES
                                       IF   FIM NOT = ">"
                                            SET READ-PREVIOUS TO TRUE
                                            PERFORM 250-LER
                                               THRU 250-99-FIM
                                            IF AT-END
                                               PERFORM 205-INICIO-BOXWRK
                                               THRU 205-99-FIM
                                            END-IF
                                       END-IF
                               END-PERFORM
                               PERFORM 210-MONTA-TELA THRU 210-99-FIM
                               IF  X91-PARAMETER > 4
                               AND EDIT-CURSOR-UP
                               AND FIM = ">"
                               AND LIDOX-1 = TEXTO-1 (1)
                               AND LIDOX-2 = TEXTO-2 (1)
                                   MOVE TECLA   TO CWUSER-KEY
                                   SET EDIT-ESC TO TRUE
                               END-IF
                          WHEN EDIT-PAGE-DOWN
                           AND FIM NOT = "<"
                               MOVE "N" TO FIM
                               PERFORM 210-MONTA-TELA THRU 210-99-FIM
                               MOVE CWBOXF-VERTICAL-LENGTH TO M
                               IF   M = 1
                                    COMPUTE L = CWBOXF-LINE
                                              + CWBOXF-VERTICAL-LENGTH
                               ELSE
                                    COMPUTE L = CWBOXF-LINE
                                     + (CWBOXF-VERTICAL-LENGTH * 3) - 2
                               END-IF
                               IF   TEXTO-L (1) = SPACE
                                    MOVE    "N" TO FIM
                                    PERFORM 206-FIM-BOXWRK
                                       THRU 206-99-FIM
                               END-IF
                       END-EVALUATE
                       IF   CWBOXF-VIEW
                       AND (NOT CWBOXF-VIEW)
                            SET EDIT-ENTER TO TRUE
                            MOVE 1 TO M
                       END-IF
                   END-IF
           END-PERFORM.

       FIM-SHOW.

           IF   EDIT-ENTER
                IF CWBOXF-RETURN = 1
                   MOVE TEXTO-1 (M) TO CWBOXF-OPTION
                ELSE
                   MOVE TEXTO-2 (M) TO CWBOXF-OPTION
                END-IF
           ELSE
                MOVE SPACES TO CWBOXF-OPTION
           END-IF

           IF   CWBOXF-POP-UP
           OR   CWBOXF-VIEW
                SET CWBOXW-POPUP        TO TRUE
                IF  (CWBOXF-OPTION NOT = SPACES)
      *         AND (NOT CWBOXF-VIEW)
                    IF   CWBOXF-VIEW
                    AND  X91-PARAMETER > 1
                         ADD 1 TO COLUMN-NUMBER
                         SUBTRACT 1 FROM SIZE-BARR-MENU
                    END-IF
                             MOVE SCREEN-POSITION TO
                                  SCREEN-POSITION-X
                                  SCREEN-POSITION-X1
                             ADD 1 TO ROW-NUMBER-X
                             ADD 2 TO ROW-NUMBER-X1
                    CALL "CBL_WRITE_SCR_N_ATTR" USING SCREEN-POSITION
                                                COLOR-BARR-MENU-BLINK
                                                      SIZE-BARR-MENU
                    CALL "CBL_WRITE_SCR_N_ATTR" USING SCREEN-POSITION-X
                                                COLOR-BARR-MENU-BLINK
                                                      SIZE-BARR-MENU
                    CALL "CBL_WRITE_SCR_N_ATTR" USING SCREEN-POSITION-X1
                                                COLOR-BARR-MENU-BLINK
                                                      SIZE-BARR-MENU
                    IF   CWBOXF-VIEW
                    AND  X91-PARAMETER > 1
                         SUBTRACT 1 FROM COLUMN-NUMBER
                         ADD      1   TO SIZE-BARR-MENU
                    END-IF
                END-IF
           ELSE
                SET  CWBOXW-CLOSE             TO TRUE
           END-IF

           IF   CWGETL-MOUSE = 1
                CALL "CBL_TERM_MOUSE"         USING MOUSE-HANDLE
                                                    MOUSE-BUTTONS
                     ON exception
                        CONTINUE
                END-CALL
           END-IF

           CALL "CWBOXW" USING PARAMETROS-CWBOXW.

       200-99-FIM. EXIT.

       201-LER-TECLA.

           EVALUATE TRUE
             WHEN  CWBOXF-OPTION NOT = SPACES
md    *           perform varying t from length of CWBOXF-OPTION by -1
md    *                   until T = 1
md    *                      or cwboxf-option(T:1) not = space
md    *                   continue
md    *           end-perform
md                if   CWBOXF-OPTION(2:) = SPACES
md                     move 1 to ret-end t
md                end-if
                  MOVE  CWBOXF-OPTION TO TEXTO-A
                  compute x = x2 + 1
                  IF CWBOXF-ORDER = 1
                     MOVE CWBOXF-OPTION TO CWBOXF-STRING-1
                  ELSE
                     MOVE CWBOXF-OPTION TO CWBOXF-STRING-2
                  END-IF
                   SET NOT-LESS TO TRUE
                   PERFORM 250-LER THRU 250-99-FIM
                   IF   AT-END
                        PERFORM 206-FIM-BOXWRK
                        THRU    206-99-FIM
                   ELSE
                        PERFORM 210-MONTA-TELA
                        THRU    210-99-FIM
                   END-IF
md    *            move 0 to ret-end
             WHEN(CWBOXF-ORDER = 1
              AND CWBOXF-STRING-1-LENGTH NOT = 0)
               OR CWBOXF-ORDER > 1
              AND(CWBOXF-STRING-2-LENGTH NOT = 0)
                  IF CWBOXF-ORDER = 1
                     IF   T < CWBOXF-STRING-1-LENGTH
                          ADD 1 TO T
                     END-IF
                     MOVE TEXTO-L (M) (2: CWBOXF-STRING-1-LENGTH)
                       TO TEXTO-A TEXTO-B
                     COMPUTE C-1 = C + 1
                  ELSE
                     IF   T < 120
                          ADD 1 TO T
                     END-IF
                     MOVE TEXTO-L (M) (I: )
                       TO TEXTO-A TEXTO-B
                     COMPUTE C-1 = I + C - 1
                  END-IF
Oscar             MOVE TEXTO-A  TO T-PRE
                  IF   CARACTER = X"08"
                       IF   T > 2
                            SUBTRACT 1 FROM T
                            MOVE SPACE TO TEXTO-A (T: 1) CARACTER
                            SUBTRACT 1 FROM T
                       END-IF
                  ELSE
                       MOVE CARACTER TO TEXTO-A (T: 1)
                  END-IF
                  MOVE    TEXTO-A TO STRING-CHAR
                  COMPUTE STRING-ROW-NUMBER    = L   - 1
                  COMPUTE STRING-COLUMN-NUMBER = C-1 - 1
                  IF CWBOXF-ORDER = 1
                     COMPUTE MAX = STRING-COLUMN-NUMBER
                            + CWBOXF-STRING-1-LENGTH
                  ELSE
                     COMPUTE MAX = STRING-COLUMN-NUMBER
                            + CWBOXF-STRING-2-LENGTH
                  END-IF
                  subtract 1 from STRING-COLUMN-NUMBER
                  PERFORM T TIMES
                      ADD 1 TO STRING-COLUMN-NUMBER
                      IF STRING-COLUMN-NUMBER > max
                         add 1 to STRING-ROW-NUMBER
                         IF CWBOXF-ORDER = 1
                            SUBTRACT CWBOXF-STRING-1-LENGTH
                               FROM STRING-COLUMN-NUMBER
                         else
                            SUBTRACT CWBOXF-STRING-2-LENGTH
                               FROM STRING-COLUMN-NUMBER
                         end-if
                      end-if
                  END-PERFORM
                  MOVE    1 TO STRING-SIZE
                  CALL "CBL_WRITE_SCR_CHARS" USING STRING-POS
                                                   STRING-CHAR(T:1)
                                                   STRING-SIZE
                  IF  TEXTO-A NOT = TEXTO-B
                      MOVE "N"            TO FIM
                                             ROLA
                      IF CWBOXF-ORDER = 1
                         MOVE TEXTO-A (1: T) TO CWBOXF-STRING-1
                      ELSE
                         MOVE TEXTO-A (1: T) TO CWBOXF-STRING-2
                      END-IF
                      SET NOT-LESS TO TRUE
                      PERFORM 250-LER THRU 250-99-FIM
                      IF NOT  AT-END
                         SET READ-NEXT TO TRUE
                         PERFORM 250-LER THRU 250-99-FIM
                         MOVE 1 TO SKIP-LER
                      END-IF
                      COMPUTE P = T - 1
                      IF P = 0
                         MOVE 1 TO P
                      END-IF
                      IF  (CWBOXF-ORDER = 1 AND T > 1
                      AND  CWBOXF-STRING-1(1: P) > T-PRE(1: P))
                      OR ((CWBOXF-ORDER NOT = 1) AND T > 1
                      AND (CWBOXF-STRING-2(1: P) > T-PRE(1: P)))
                      OR  AT-END
                      OR  (CWBOXF-ORDER = 1
                      AND  CWBOXF-STRING-1(T: 1) <> TEXTO-A (T: 1))
                      OR ((CWBOXF-ORDER NOT = 1)
                      AND (CWBOXF-STRING-2(T: 1) <> TEXTO-A (T: 1)))
                          MOVE TEXTO-A (T: 1) TO CASE-CHAR DIGITADO
                          IF  CASE-CHAR ALPHABETIC-LOWER
                          OR  CASE-CHAR ALPHABETIC-UPPER
                              PERFORM 251-REVERSE-CASE THRU 251-99-FIM
                              SET NOT-LESS TO TRUE
                              PERFORM 250-LER THRU 250-99-FIM
                              IF NOT  AT-END
                                 SET READ-NEXT TO TRUE
                                 PERFORM 250-LER THRU 250-99-FIM
                                 MOVE 1 TO SKIP-LER
                              END-IF
                          END-IF
                          IF  (CWBOXF-ORDER = 1 AND T > 1
                          AND  CWBOXF-STRING-1(1: P) > T-PRE(1: P))
                          OR ((CWBOXF-ORDER NOT = 1) AND T > 1
                          AND (CWBOXF-STRING-2(1: P) > T-PRE(1: P)))
                          OR  AT-END
                          OR  (CWBOXF-ORDER = 1
                          AND  CWBOXF-STRING-1(T: 1) <> DIGITADO)
                          OR ((CWBOXF-ORDER NOT = 1)
                          AND (CWBOXF-STRING-2(T: 1) <> DIGITADO))
                              MOVE T-PRE     TO CWBOXF-STRING-1
                                                CWBOXF-STRING-2
                              SET NOT-LESS TO TRUE
                              PERFORM 250-LER THRU 250-99-FIM
                              IF T > 0
                              AND (NOT AT-END)
                                 SUBTRACT 1 FROM T
                              END-IF
                          END-IF
                      END-IF
                      IF   AT-END
                           PERFORM 206-FIM-BOXWRK
                           THRU    206-99-FIM
                      ELSE
                           PERFORM 210-MONTA-TELA
                           THRU    210-99-FIM
                      END-IF
                  END-IF
                  IF   CARACTER = X"08"
                       MOVE 0 TO T
                  END-IF
           END-EVALUATE.

       201-99-FIM. EXIT.

       202-BARRA.

           IF   X91-PARAMETER > 3
           AND  COMBO-BOX
                MOVE SCREEN-POSITION TO SCREEN-POSITION-X
                ADD  1               TO COLUMN-NUMBER-X
                COMPUTE STRING-SIZE = SIZE-BARR-MENU - 1
                CALL "CBL_WRITE_SCR_N_ATTR" USING SCREEN-POSITION-X
                                                  COLOR-BARR-MENU
                                                  STRING-SIZE
           ELSE
                MOVE SCREEN-POSITION TO
                     SCREEN-POSITION-X
                     SCREEN-POSITION-X1
                ADD 1 TO ROW-NUMBER-X
                ADD 2 TO ROW-NUMBER-X1
                IF COR-TAB (M) = 0
                   CALL "CBL_WRITE_SCR_N_ATTR" USING SCREEN-POSITION
                                                  COLOR-BARR-MENU
                                                  SIZE-BARR-MENU
                   CALL "CBL_WRITE_SCR_N_ATTR" USING SCREEN-POSITION-X
                                                  COLOR-BARR-MENU
                                                  SIZE-BARR-MENU
                   CALL "CBL_WRITE_SCR_N_ATTR" USING SCREEN-POSITION-X1
                                                  COLOR-BARR-MENU
                                                  SIZE-BARR-MENU
                ELSE
                    COMPUTE CC = COR-TAB (M) + 1 + 128
                    MOVE COR (CC) (1:1) TO COLOR-CHAR
                    IF SAME-FB2
                       PERFORM TEST AFTER UNTIL TESTE-SAME > 0
                               ADD 1 TO TESTE-SAME
                       END-PERFORM
                    END-IF
                    CALL "CBL_WRITE_SCR_N_ATTR" USING SCREEN-POSITION
                                                 COLOR-CHAR
                                                SIZE-BARR-MENU
                    CALL "CBL_WRITE_SCR_N_ATTR" USING SCREEN-POSITION-X
                                                 COLOR-CHAR
                                                SIZE-BARR-MENU
                    CALL "CBL_WRITE_SCR_N_ATTR" USING SCREEN-POSITION-X1
                                                 COLOR-CHAR
                                                SIZE-BARR-MENU
                END-IF
<WX>            PERFORM 320-REACENDE THRU 320-99-FIM
                IF HOT-KEYS NOT = SPACES
                   PERFORM VARYING Z FROM 1 BY 1 UNTIL Z > 25
                   IF SCREEN-POSITION = HOT-POSIT (Z)
                      CALL "CBL_WRITE_SCR_CHATTRS"
                            USING HOT-POSIT-HIGH (Z)
                                  HOT-CHAR(Z)
                                  COLOR-HOT-BAR
                                  X"0001"
                      EXIT PERFORM
                   END-IF
                   END-PERFORM
                END-IF
           END-IF
           IF   X91-PARAMETER > 3
           AND  LIST-BOX
                CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
                                                   M-186
                                                   COLOR-FRAME
                                                   X"0001"
           END-IF.

       202-99-FIM. EXIT.

       203-DISPLAY-COMBO-BOX.

           COMPUTE STRING-SIZE = CWBOXF-STRING-1-LENGTH +
                                 CWBOXF-STRING-2-LENGTH
foda       IF   CWBOXF-DISPLAY
foda            move CWBOXF-HORIZONTAL-LENGTH to STRING-SIZE
foda       END-IF
           COMPUTE STRING-ROW-NUMBER    = CWBOXF-LINE   - 1
           COMPUTE STRING-COLUMN-NUMBER = CWBOXF-COLUMN + 1

PEP        IF   CWBOXF-DISPLAY
           AND  CWBOXF-OPTION = SPACES
                IF   CWBOXF-ORDER < 2
                     MOVE CWBOXF-STRING-1 TO CWBOXF-OPTION
                ELSE
                     MOVE CWBOXF-STRING-2 TO CWBOXF-OPTION
                END-IF
           END-IF

rita       IF (CWBOXF-ORDER NOT = CWBOXF-RETURN)
           OR (CWBOXF-ORDER = 1
               AND CWBOXF-RETURN = 1
foda  *        AND CWBOXF-STRING-1-LENGTH = 0
               )
           OR (CWBOXF-ORDER = 2
               AND CWBOXF-RETURN = 2
foda  *        AND CWBOXF-STRING-2-LENGTH = 0
               )
               IF  CWBOXF-OPTION NOT = SPACES
                   MOVE CWBOXF-ORDER  TO SAVE-ORDER
                   MOVE CWBOXF-RETURN TO CWBOXF-ORDER SAVE-RETURN
                   IF  CWBOXF-ORDER = 1
                       MOVE CWBOXF-OPTION TO CWBOXF-STRING-1
                   ELSE
                       MOVE CWBOXF-OPTION TO CWBOXF-STRING-2
                   END-IF
                   SET NOT-LESS TO TRUE
                   PERFORM 250-LER THRU 250-99-FIM
ED                 IF NOT AT-END
                      SET READ-NEXT TO TRUE
                      PERFORM 250-LER THRU 250-99-FIM
ED                 END-IF
                   MOVE SAVE-RETURN TO CWBOXF-RETURN
                   MOVE SAVE-ORDER  TO CWBOXF-ORDER
                   IF   CWBOXF-GET
                        IF  CWBOXF-RETURN = 1
                            MOVE CWBOXF-STRING-2 TO CWBOXF-OPTION
                        ELSE
                            MOVE CWBOXF-STRING-1 TO CWBOXF-OPTION
                        END-IF
                   END-IF
               ELSE
                    MOVE SPACES TO CWBOXF-STRING-1
                                   CWBOXF-STRING-2
               END-IF
               MOVE SPACES TO STRING-CHAR
               MOVE 1      TO OFFSET
ML             IF  CWBOXF-DISPLAY
ML                 IF  CWBOXF-STRING-1-LENGTH NOT = 0
ML                     MOVE CWBOXF-STRING-1 TO STRING-CHAR
ML                     ADD CWBOXF-STRING-1-LENGTH TO OFFSET
ML                 END-IF
ML                 IF  CWBOXF-STRING-2-LENGTH NOT = 0
ML                     MOVE CWBOXF-STRING-2 TO STRING-CHAR (OFFSET: )
ML                 END-IF
easy  *        IF CWBOXF-RETURN = 2
ML             ELSE
return             IF CWBOXF-ORDER = 2
crise                 MOVE CWBOXF-STRING-2 TO STRING-CHAR
crise              ELSE
crise                 MOVE CWBOXF-STRING-1 TO STRING-CHAR
crise              END-IF
ML             END-IF
           ELSE
               MOVE CWBOXF-OPTION TO STRING-CHAR
           END-IF

           IF   CWBOXF-DISPLAY
                IF   X91-PARAMETER > 4
                     IF AJUSTED
      *                 NEXT SENTENCE
                        ADD ct-lines TO STRING-ROW-NUMBER
                        move 0 to ct-lines
                     END-IF
                     SUBTRACT 1 FROM STRING-SIZE
                END-IF
SC              IF   X91-PARAMETER > 7
SC                   PERFORM VARYING I FROM 1 BY 1
SC                             UNTIL I > STRING-SIZE
SC                               OR (PIC-E(I:1) NOT = 'Z')
SC                               OR (STRING-CHAR(I:1) NOT = '0')
SC                             MOVE SPACE TO STRING-CHAR(I:1)
SC                   END-PERFORM
                END-IF
                CALL "CBL_WRITE_SCR_CHARS" USING STRING-POS
                                                 STRING-CHAR
                                                 STRING-SIZE
                IF   NOT CWBOXF-NOLIGTH
DOLLAR               CALL "CBL_WRITE_SCR_N_ATTR" USING STRING-POS
VH1                                                    X"70"
VH1                                                    STRING-SIZE
                END-IF
                IF   X91-PARAMETER > 4
                     ADD 1 TO STRING-SIZE
                END-IF
           END-IF.

       203-99-FIM. EXIT.

       205-INICIO-BOXWRK.

           MOVE ">" TO FIM
           SET BEGIN-FILE TO TRUE
           PERFORM 250-LER THRU 250-99-FIM.

       205-99-FIM. EXIT.

       206-FIM-BOXWRK.

           SET END-FILE TO TRUE
           PERFORM 250-LER THRU 250-99-FIM

           COMPUTE VOLTA = CWBOXF-VERTICAL-LENGTH + 1

           PERFORM VOLTA TIMES
                IF   FIM NOT = ">"
                     SET READ-PREVIOUS TO TRUE
                     PERFORM 250-LER THRU 250-99-FIM
                     IF   AT-END
                          MOVE ">" TO FIM
                     END-IF
                END-IF
           END-PERFORM

           MOVE "N" TO FIM
           PERFORM 210-MONTA-TELA THRU 210-99-FIM
           MOVE CWBOXF-VERTICAL-LENGTH TO M
           COMPUTE L = CWBOXF-LINE
                     + CWBOXF-VERTICAL-LENGTH
           MOVE "<" TO FIM.

       206-99-FIM. EXIT.

       210-MONTA-TELA.

           IF   X91-PARAMETER > 3
           AND  COMBO-BOX
           AND  CWBOXF-VIEW
                GO TO 210-99-FIM
           END-IF

           MOVE CWBOXF-LINE TO L
           MOVE 0           TO M
                               VOLTA
           MOVE SPACES      TO MATRIZ
           move CWBOXF-VERTICAL-LENGTH to LEITURAS
dollar*    IF   X91-PARAMETER > 3
dollar*    and  COMBO-BOX
dollar*         add 1 to LEITURAS
dollar*    end-if
ANA        MOVE ">" TO FIM
           PERFORM LEITURAS TIMES
             ADD 1 TO M
             ADD 3 TO   L
             IF  FIM NOT = "<"
                 SET READ-NEXT TO TRUE
                 PERFORM 250-LER THRU 250-99-FIM
                 IF   AT-END
                      MOVE "<" TO FIM
                      SET READ-PREVIOUS TO TRUE
                      PERFORM 250-LER THRU 250-99-FIM
                 ELSE
                     ADD 1 TO VOLTA
                     PERFORM 215-DISCO-MEMORIA THRU 215-99-FIM
                END-IF
             END-IF
           END-PERFORM
           COMPUTE L = CWBOXF-LINE + 1
           MOVE    1 TO M
           PERFORM 220-EXIBE-TELA THRU 220-99-FIM.

       210-99-FIM. EXIT.

       215-DISCO-MEMORIA.

           MOVE CWBOXF-STRING-1  TO TEXTO-1 (M)
           MOVE CWBOXF-STRING-2  TO TEXTO-2 (M)

           IF  CWBOXF-STRING-1-LENGTH NOT = 0
               MOVE CWBOXF-STRING-1 TO TEXTO-L (M) (2: )
           END-IF
           IF  CWBOXF-STRING-2-LENGTH NOT = 0
               MOVE CWBOXF-STRING-2 TO TEXTO-L (M) (I: )
           END-IF

           DISPLAY "CWBOXF-COLOR-ITEM" UPON ENVIRONMENT-NAME
           INITIALIZE MULTI-COLOR
           ACCEPT  MULTI-COLOR FROM ENVIRONMENT-VALUE
           IF MULTI-COLOR NOT = SPACES
              MOVE 1 TO COR-ITEM
              MOVE 0 TO COR-TAB (M)
              PERFORM VARYING J FROM 1 BY 1 UNTIL J > 10
                      IF MULTI-COLOR (J:1) NUMERIC
                         IF COR-TAB (M) NOT = 0
                            MOVE COR-TAB (M) (2: 2)
                              TO COR-TAB (M) (1: 2)
                         END-IF
                         MOVE MULTI-COLOR (J:1) TO COR-TAB (M) (3:1)
                      END-IF
              END-PERFORM
              DISPLAY SPACES UPON ENVIRONMENT-VALUE
           END-IF.

       215-99-FIM. EXIT.

       220-EXIBE-TELA.

           MOVE LOW-VALUES TO MAPA-HOT
           MOVE CWBOXF-LINE TO L2
           MOVE 0           TO M2
           ADD 1 TO L2
           PERFORM CWBOXF-VERTICAL-LENGTH TIMES
             ADD 1 TO M2
             COMPUTE COLUMN-NUMBER-X = C  - 1
             MOVE CWBOXF-HORIZONTAL-LENGTH TO STRING-LENGTH-X
             MOVE 1      TO TF
             COMPUTE ROW-NUMBER-X    = L2 - 1
             PERFORM 3 TIMES
                IF COR-TAB (M2) = 0
                   CALL "CBL_WRITE_SCR_CHARS" USING SCREEN-POSITION-X
                            TEXTO-L (M2) (TF: STRING-LENGTH-X)
                                              STRING-LENGTH-X
                   IF COR-ITEM = 1 OR (COLOR-TAB NOT = ZEROS)
                          CALL "CBL_WRITE_SCR_N_ATTR"
                                 USING SCREEN-POSITION-X
                                       COLOR-FRAME
                                       STRING-LENGTH-X
                   END-IF
                ELSE
                   COMPUTE CC = COR-TAB (M2) + 1
                   MOVE LOW-VALUES TO COLOR-CHAR
                   INSPECT COLOR-CHAR
                     CONVERTING LOW-VALUES TO COR (CC) (1:1)
                   CALL  "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION-X
                    TEXTO-L (M2) (TF: STRING-LENGTH-X)
                                                       COLOR-CHAR
                                                       STRING-LENGTH-X
                END-IF
                ADD STRING-LENGTH-X TO TF
                ADD 1 TO ROW-NUMBER-X
                IF TF = (1 + STRING-LENGTH-X)
                   IF CWBOXF-STRING-1-LENGTH < 40
                      ADD      CWBOXF-STRING-1-LENGTH TO COLUMN-NUMBER-X
                      SUBTRACT CWBOXF-STRING-1-LENGTH
                          FROM STRING-LENGTH-X
                   ELSE
                      ADD      CWBOXF-STRING-2-LENGTH TO COLUMN-NUMBER-X
                      SUBTRACT CWBOXF-STRING-2-LENGTH
                          FROM STRING-LENGTH-X
                   END-IF
                   ADD 1 TO COLUMN-NUMBER-X
                   SUBTRACT 1 FROM STRING-LENGTH-X
                END-IF
             END-PERFORM
             IF   X91-PARAMETER > 3
             AND  LIST-BOX
                  CALL  "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION-X
                                                     M-186
                                                     COLOR-FRAME
                                                     X"0001"
             ELSE
                  IF   HOT-KEYS NOT = SPACES
                       CALL "CBL_WRITE_SCR_N_ATTR"
                              USING SCREEN-POSITION-X
                                    COLOR-FRAME
                                    STRING-LENGTH-X
                       MOVE TEXTO-1 (M2) (1: 2) TO KP (1: 2)
                       MOVE SCREEN-POSITION-X   TO SCREEN-POSITION-H
                       PERFORM VARYING Z FROM 1 BY 1
                                 UNTIL Z > STRING-LENGTH-X
                             MOVE TEXTO-L (M2) (Z:1) TO TESTCHAR
                             IF HOT-KEYS (KP: 1) NOT = SPACE
                                INSPECT TESTCHAR CONVERTING MINUSCULAS
                                        TO MAIUSCULAS
                                IF  TESTCHAR = HOT-KEYS (KP:1)
                                    MOVE SCREEN-POSITION-X TO
                                         HOT-POSIT(M2)
                                    MOVE SCREEN-POSITION-H TO
                                         HOT-POSIT-HIGH(M2)
                                    MOVE TESTCHAR TO HOT-CHAR (M2)
                                    CALL "CBL_WRITE_SCR_CHATTRS"
                                       USING SCREEN-POSITION-H
                                                      TESTCHAR
                                                      COLOR-HOT
                                                      X"0001"
                                    EXIT PERFORM
                               END-IF
                               ADD 1 TO COLUMN-NUMBER-H
                             END-IF
                       END-PERFORM
                  END-IF
             ADD 3 TO L2
             END-IF
           END-PERFORM.

       220-99-FIM. EXIT.

       250-LER.

           IF SKIP-LER = 1 AND READ-NEXT
              MOVE 0 TO SKIP-LER
              GO TO 250-99-FIM
           END-IF

           MOVE 0               TO SKIP-LER
           MOVE 0               TO VAZIO
           MOVE CWBOXF-STRING-1 TO END-STRING-1
           MOVE CWBOXF-STRING-2 TO END-STRING-2

           PERFORM TEST AFTER
                        UNTIL AT-END
                        OR ((NOT READ-NEXT) AND (NOT READ-PREVIOUS))
                OR (CWBOXF-RETURN = 1 AND (STRING-1X NOT = SPACE))
                OR (CWBOXF-RETURN > 1 AND (STRING-2X NOT = SPACE))
                    CALL CWBOXF-PROGRAM USING USER-IO
                                              CWBOXF-ORDER
                                              CWBOXF-STRING-1
                                              CWBOXF-STRING-2
                                              CWBOXF-VERTICAL-LENGTH
                                              CWBOXF-WORK-AREA
                    MOVE CWBOXF-STRING-1 TO STRING-1X
                    MOVE CWBOXF-STRING-2 TO STRING-2X
>>>>>>*             INSPECT STRINGX CONVERTING ZERO TO SPACE
                     IF   SPACES = STRING-1X AND STRING-2X
                     AND (READ-NEXT OR READ-PREVIOUS)
                          ADD 1 TO VAZIO
                          IF   VAZIO = 50
                               SET AT-END TO TRUE
                          END-IF
                     END-IF
           END-PERFORM

md         if  READ-NEXT
md         and ret-end = 2
md             if CWBOXF-order = 1
md                move CWBOXF-STRING-1 to texto-x
md             else
md                move CWBOXF-STRING-2 to texto-x
md             end-if
md         end-if

           IF   AT-END
           and  ret-end = 1
                move 2 to ret-end
           end-if

           IF   AT-END
                MOVE END-STRING-1 TO CWBOXF-STRING-1
                MOVE END-STRING-2 TO CWBOXF-STRING-2
           END-IF

           IF  SAVE-OPT = SPACES
               IF   CWBOXF-ORDER = 2
                    MOVE CWBOXF-STRING-2 TO SAVE-OPT
               ELSE
                    MOVE CWBOXF-STRING-1 TO SAVE-OPT
               END-IF
           END-IF

           IF   SEM-DADOS = 1
           AND (NOT AT-END)
                IF  (STRING-1X NOT = SPACES)
                AND  CWBOXF-RETURN = 1
                     MOVE 0 TO SEM-DADOS
                END-IF
                IF  (STRING-2X NOT = SPACES)
                AND (CWBOXF-RETURN NOT = 1)
                     MOVE 0 TO SEM-DADOS
                END-IF
           END-IF.

       250-99-FIM. EXIT.

       251-REVERSE-CASE.

           IF   CASE-CHAR ALPHABETIC-LOWER
                INSPECT CASE-CHAR CONVERTING MINUSCULAS
                                          TO MAIUSCULAS
           ELSE
                INSPECT CASE-CHAR CONVERTING MAIUSCULAS
                                          TO MINUSCULAS
           END-IF
           IF   CWBOXF-ORDER = 1
                MOVE TEXTO-A (1: T) TO CWBOXF-STRING-1
                MOVE CASE-CHAR      TO CWBOXF-STRING-1 (T:) DIGITADO
           ELSE
                MOVE TEXTO-A (1: T) TO CWBOXF-STRING-2
                MOVE CASE-CHAR      TO CWBOXF-STRING-2 (T:) DIGITADO
           END-IF.

       251-99-FIM. EXIT.

       310-POSIT-END.

           IF  (TEXTO-L (1) NOT = SPACES)
           AND  TEXTO-L (2) = SPACES
           AND  CWBOXF-VERTICAL-LENGTH > 1
                MOVE TEXTO-L (1) TO TEXTO-SAVE
                PERFORM CWBOXF-VERTICAL-LENGTH TIMES
                        IF   FIM NOT = ">"
                             SET READ-PREVIOUS TO TRUE
                             PERFORM 250-LER THRU 250-99-FIM
                             IF   AT-END
                                  PERFORM 205-INICIO-BOXWRK
                                  THRU 205-99-FIM
                             END-IF
                        END-IF
                END-PERFORM
                MOVE ">" TO FIM
                PERFORM 210-MONTA-TELA THRU 210-99-FIM
                PERFORM VARYING M FROM 1 BY 1
                           UNTIL TEXTO-L (M) = TEXTO-SAVE
                              OR M = CWBOXF-VERTICAL-LENGTH
                                 CONTINUE
                END-PERFORM
                COMPUTE L = CWBOXF-LINE + M
                IF   CWUNIX-ON
                     MOVE "^" TO STRING-CHAR
                ELSE
                     MOVE X"18" TO STRING-CHAR
                END-IF
                COMPUTE STRING-ROW-NUMBER    = L-UP   - 1
                COMPUTE STRING-COLUMN-NUMBER = C-UPDN - 1
                MOVE    1 TO STRING-SIZE
                CALL "CBL_WRITE_SCR_CHARS" USING STRING-POS
                                                 STRING-CHAR
                                                 STRING-SIZE
           END-IF

           IF  FLAG-FIRST = 0
               IF   CWUNIX-ON
                    MOVE "^" TO STRING-CHAR
               ELSE
                    MOVE X"18" TO STRING-CHAR
               END-IF
               COMPUTE STRING-ROW-NUMBER    = L-UP   - 1
               COMPUTE STRING-COLUMN-NUMBER = C-UPDN - 1
               MOVE    1 TO STRING-SIZE
               CALL "CBL_WRITE_SCR_CHARS" USING STRING-POS
                                                STRING-CHAR
                                                STRING-SIZE
           END-IF.

       310-99-FIM. EXIT.

       320-REACENDE.

           IF HOT-KEYS NOT = SPACES
              PERFORM VARYING Z FROM 1 BY 1 UNTIL Z > 25
                 IF SCREEN-POSITION = HOT-POSIT (Z)
                  CALL  "CBL_WRITE_SCR_CHATTRS" USING HOT-POSIT-HIGH (Z)
                                                      HOT-CHAR(Z)
                                                      COLOR-HOT
                                                      X"0001"
                  EXIT PERFORM
                 END-IF
              END-PERFORM
           END-IF.

       320-99-FIM. EXIT.

       030-ALT.

           MOVE SPACE TO LETRA
           EVALUATE TRUE
               WHEN EDIT-ALT-0           MOVE "0" TO LETRA
               WHEN EDIT-ALT-1           MOVE "1" TO LETRA
               WHEN EDIT-ALT-2           MOVE "2" TO LETRA
               WHEN EDIT-ALT-3           MOVE "3" TO LETRA
               WHEN EDIT-ALT-4           MOVE "4" TO LETRA
               WHEN EDIT-ALT-5           MOVE "5" TO LETRA
               WHEN EDIT-ALT-6           MOVE "6" TO LETRA
               WHEN EDIT-ALT-7           MOVE "7" TO LETRA
               WHEN EDIT-ALT-8           MOVE "8" TO LETRA
               WHEN EDIT-ALT-9           MOVE "9" TO LETRA
               WHEN EDIT-ALT-A           MOVE "A" TO LETRA
               WHEN EDIT-ALT-B           MOVE "B" TO LETRA
               WHEN EDIT-ALT-C           MOVE "C" TO LETRA
               WHEN EDIT-ALT-D           MOVE "D" TO LETRA
               WHEN EDIT-ALT-E           MOVE "E" TO LETRA
               WHEN EDIT-ALT-EQUAL       MOVE "=" TO LETRA
               WHEN EDIT-ALT-F           MOVE "F" TO LETRA
               WHEN EDIT-ALT-G           MOVE "G" TO LETRA
               WHEN EDIT-ALT-H           MOVE "H" TO LETRA
               WHEN EDIT-ALT-I           MOVE "I" TO LETRA
               WHEN EDIT-ALT-J           MOVE "J" TO LETRA
               WHEN EDIT-ALT-K           MOVE "K" TO LETRA
               WHEN EDIT-ALT-L           MOVE "L" TO LETRA
               WHEN EDIT-ALT-M           MOVE "M" TO LETRA
               WHEN EDIT-ALT-TRACE       MOVE "-" TO LETRA
               WHEN EDIT-ALT-N           MOVE "N" TO LETRA
               WHEN EDIT-ALT-O           MOVE "O" TO LETRA
               WHEN EDIT-ALT-P           MOVE "P" TO LETRA
               WHEN EDIT-ALT-Q           MOVE "Q" TO LETRA
               WHEN EDIT-ALT-R           MOVE "R" TO LETRA
               WHEN EDIT-ALT-S           MOVE "S" TO LETRA
               WHEN EDIT-ALT-T           MOVE "T" TO LETRA
               WHEN EDIT-ALT-U           MOVE "U" TO LETRA
               WHEN EDIT-ALT-V           MOVE "V" TO LETRA
               WHEN EDIT-ALT-W           MOVE "W" TO LETRA
               WHEN EDIT-ALT-X           MOVE "X" TO LETRA
               WHEN EDIT-ALT-Y           MOVE "Y" TO LETRA
               WHEN EDIT-ALT-Z           MOVE "Z" TO LETRA
               WHEN EDIT-CURSOR-LEFT     MOVE "<" TO LETRA
               WHEN EDIT-CURSOR-RIGHT    MOVE ">" TO LETRA
               WHEN (CARACTER NOT = SPACES)
                AND (CARACTER NOT = X'00')
                    MOVE CARACTER TO LETRA
                    INSPECT LETRA CONVERTING MINUSCULAS TO MAIUSCULAS
           END-EVALUATE

           IF LETRA NOT = SPACE
              MOVE SPACE TO CARACTER
              PERFORM VARYING Z FROM 1 BY 1 UNTIL Z > 34
                      IF LETRA = HOT-KEYS (Z:1) OR "<" OR ">"
                         SET EDIT-ENTER TO TRUE
                         IF LETRA = "<" OR ">"
                            MOVE LOW-VALUE TO CARACTER
                         ELSE
                            MOVE Z     TO M
                                          TEXTO-1(M)
                            MOVE LETRA TO CARACTER
                                          TEXTO-1(M)(3:1)
                         END-IF
                         EXIT PERFORM
                      END-IF
              END-PERFORM
              IF SPACE = CARACTER
                 CALL X"E5"
              END-IF
           END-IF.

       030-99-FIM. EXIT.
       END PROGRAM KEYBOXF.
