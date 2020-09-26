       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWBOXF INITIAL.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  29/03/1993.
       SECURITY.      *************************************************
                      *                                               *
                      *  Lista de sele‡ao de arquivo                  *
                      *                                               *
                      *  - Opcoes ilimitadas                          *
                      *  - Rolagem e paginacao                        *
                      *  - Acesso a arquivo                           *
                      *                                               *
                      *************************************************
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       78  SP2 value "SP2".
       01  OFF-SETS.
           05 SIZE-FIELDS PIC 9(004) VALUE 0.
           05 OFF-W       PIC 9(004) VALUE 0.
           05 FIELD-AREA.
              10 OCCURS 0 TO 2000 DEPENDING ON SIZE-FIELDS PIC X.
           05 END-W       PIC 9(004) VALUE 0.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 COLOR-ALERT         COMP-X PIC  9(002).
           05 LOADED                     PIC  9(001) VALUE 1.
           05 inverte                    PIC  9(003) VALUE 0.
           05 WD2                        PIC  9(002) VALUE 0.
           05 CWFIND                     PIC  X(003) VALUE SPACES.
           05 COR-N               COMP-X PIC  9(002) VALUE 0.
              88 SAME-FB2 VALUE 000 017 034 051 068 085 102 119 136 153
                                170 187 204 221 238 255.
           05 J                   COMP-X PIC  9(002) VALUE 0.
           05 COR-ITEM                   PIC  9(001) VALUE 0.
           05 COR-NOVA                   PIC  X(001) VALUE SPACE.
           05 MULTI-COLOR                PIC  X(098) VALUE SPACEs.
           05 COLOR-TAB.
              10 COR-TAB                PIC  9(003) OCCURS 21.
           05 DIGITADO                  PIC  X(001) VALUE SPACES.
           05 CARACTER                  PIC  X(001) VALUE SPACES.
           05 T-PRE                     PIC  X(080) VALUE SPACES.
           05 SKIP-LER                  PIC  9(001) VALUE 0.
           05 CWINDF                    PIC  X(010) VALUE SPACES.
           05 BRANCAO                   PIC  X(2000) VALUE SPACES.
           05 XCUR                      PIC  X(01) VALUE X'9E'.  *> 02
      * 158 224
           05 CWBOXS-CHARS.
              15 CHAR OCCURS 26        PIC  X(001).
           05 LETRA                    PIC  X(001) VALUE SPACE.
           05 VEZ                      PIC  9(001) VALUE 0.
           05 VEZ2                     PIC  9(001) VALUE 0.
           05 ALTS                     PIC  9(002) VALUE 0.
           05 CWBOXS-EXIT              PIC  X(002) VALUE SPACES.
           05 CWBOXS-EXIT-CHAR         PIC  X(001) VALUE SPACE.
           05 SP2-BUG                  PIC  9(001) VALUE 0.
           05 case-char                PIC  X(001) VALUE space.
           05 SAVE-VERT                PIC  9(002) VALUE 0.
           05 MENOR                    PIC  9(001) VALUE 0.
           05 K                        PIC 9(002) VALUE 0.
           05 EDIT-OK                  PIC 9(001) VALUE 0.
           05 MIL               COMP-X PIC 9(008) VALUE 1000.
           05 CORRENTE-ID       COMP-5 PIC S9(004) VALUE 0.
           05 CORRENTE                 PIC  X(010) VALUE SPACES.
           05 CWCASE                   PIC  X(003) VALUE SPACES.
           05 CWLITS                   PIC  X(003) VALUE SPACES.
           05 CWACCENT                 PIC  X(003) VALUE SPACES.
           05 OFF-X                    PIC  9(004) VALUE 0.
      *    05 CHAR.
      *       10 DIGITADO       COMP-X PIC  9(002) VALUE 0.
           05 SAVE-A                   PIC  X(001) VALUE SPACES.
           05 WAIT-SW                  PIC  X(001) VALUE SPACES.
           05 WAIT-SW2                 PIC  X(001) VALUE SPACES.
           05 SZ                COMP-X PIC  9(002) VALUE 0.
           05 KURSOR            COMP-X PIC  9(002) VALUE 0.
           05 CD-KEY            COMP-5 PIC S9(004) VALUE 0.
           05 MFF                      PIC  X(003) VALUE SPACES.
           05 STATIC                   PIC  X(001) VALUE X"70".
           05 ENTRY-ATTR               PIC  X(001) VALUE X"F0".
           05 DISABLE-ATTR             PIC  X(001) VALUE X"70".
           05 CURCOLOR                 PIC  X(001) VALUE X"00".
           05 SEGUNDOS          COMP-X PIC  9(002) VALUE 0.
           05 SAIR              COMP-X PIC  9(002) VALUE 0.
           05 D                 COMP-X PIC  9(002) VALUE 0.
           05 TECLA-EDIT               PIC  9(003) VALUE 0.
           05 OK                       PIC  X(001) VALUE SPACE.
           05 OK2                      PIC  X(001) VALUE X"02".
           05 GAP                      PIC  9(002) VALUE 0.
           05 CRT-STATUS               PIC  X(003) VALUE SPACES.
           05 COMPOSIT.
              10 COL-1                 PIC  9(003).
              10 COL-2                 PIC  9(003).
              10 COL-B                 PIC  9(003).
              10 WID-1                 PIC  9(003).
              10 WID-2                 PIC  9(003).
              10 WID-G                 PIC  9(003).
              10 RESTO                 PIC  9(003).
              10 WD                    PIC  9(002)V99.
           05 SEM-DADOS                PIC  9(001) VALUE 1.
           05 TECLA                    PIC  9(003) VALUE 0. COPY CWEDIT.
           05 TECLA2                   PIC  9(003) VALUE 0. COPY CWKEYS.
           05 BARRA                    PIC  9(005) VALUE 0.
           05 BARRPOS                  PIC  9(005) VALUE 0.
           05 TITLE-ID          COMP-5 PIC S9(004) VALUE 0.
           05 CURSOR-ID         COMP-5 PIC S9(004) VALUE 0.
           05 BARR-ID           COMP-5 PIC S9(004) VALUE 0.
           05 FD-ID             COMP-5 PIC S9(004) VALUE 0 OCCURS 60.
           05 K1                       PIC  9(005) VALUE 0.
           05 K2                       PIC  9(005) VALUE 0.
           05 KX                       PIC  9(005) VALUE 0.
           05 FIELD             COMP-5 PIC S9(004) VALUE 0.
           05 FIELDS                   PIC  9(002) VALUE 0.
           05 filler                   PIC  x(001) VALUE space.
           05 WS-BLOCKS                            VALUE SPACES.
              10 WS-BLOCK              PIC X(2000) OCCURS 2.
           05 MSGW-POSIT.
              10 L                     PIC  9(002) VALUE 0.
              10 C                     PIC  9(002) VALUE 0.
              10 ITEM-LEN              PIC  9(002) VALUE 0.
           05 MSGW-POSIT2.
              10 L2                    PIC  9(002) VALUE 0.
              10 Y                     PIC  9(002) VALUE 0.
              10 I                     PIC  9(002) VALUE 0.
           05 II                       PIC  9(002) VALUE 0.
           05 RETURN-ID                PIC  9(005) VALUE 0.
           05 STR                      PIC  9(001) VALUE 0.
           05 START-CURSOR             PIC  9(002) VALUE 0.
           05 LISTAS.
              06 OCCURS 25.
                 07 LISTA       COMP-5 PIC S9(004) VALUE 0.
                 07 LISTA2      COMP-5 PIC S9(004) VALUE 0.
           05 CD-SAVE                  PIC  X(999) VALUE SPACES.

           05 MULTI-USER               PIC  9(001) VALUE 0.
           05 TMP                      PIC  X(003) VALUE SPACE.
           05 TITULO                   PIC  X(078) VALUE SPACES.
           05 VAZIO                    PIC  9(003) VALUE 0.
           05 CC                       PIC  9(003) VALUE 0.
           05 CWBOXF-STRING-1          PIC  X(080) VALUE SPACES.
           05 CWBOXF-STRING-2          PIC  X(080) VALUE SPACES.
           05 END-STRING-1             PIC  X(080) VALUE SPACES.
           05 END-STRING-2             PIC  X(080) VALUE SPACES.
           05 L-UP                     PIC  9(002) COMP-X VALUE 0.
           05 L-DN                     PIC  9(002) COMP-X VALUE 0.
           05 C-UPDN                   PIC  9(002) COMP-X VALUE 0.
           05 VOLTA                    PIC  9(002) VALUE 0.
           05 C-1                      PIC  9(002) VALUE 0.
           05 STR-LEN                  PIC  9(002) VALUE 0.
           05 ST1                      PIC  9(002) VALUE 0.
           05 ST2                      PIC  9(002) VALUE 0.
           05 M-1                      PIC  9(002) VALUE 0.
           05 X                        PIC  9(002) VALUE 0.
           05 X2                       PIC  9(002) VALUE 0.
           05 ZZ                       PIC  9(002) VALUE 0.
           05 M                        PIC  9(002) VALUE 0.
           05 MK                       PIC  9(002) VALUE 0.
           05 MA                       PIC  9(002) VALUE 0.
           05 Mx                       PIC  9(002) VALUE 0.
           05 SAVE-m                   PIC  9(002) VALUE 0.
           05 SAVE-m2                  PIC  9(002) VALUE 0.
           05 SAVE-m3                  PIC  9(002) VALUE 0.
           05 M2                       PIC  9(002) VALUE 0.
           05 M3                       PIC  9(002) VALUE 0.
           05 M4                       PIC  9(002) VALUE 0.
           05 ABERTO                   PIC  X(001) VALUE "N".
           05 FIM                      PIC  X(001) VALUE ">".
           05 TEXTO-A                  PIC  X(080) VALUE SPACES.
           05 TEXTO-B                  PIC  X(080) VALUE SPACES.
           05 MATRIZ                               VALUE SPACES.
              10 TEXTO-1 OCCURS 25     PIC  X(080).
              10 TEXTO-2 OCCURS 25     PIC  X(080).
              10 TEXTO-L OCCURS 25     PIC  X(080).
           05 T                 COMP-X PIC  9(002) VALUE ZERO.
           05 P                 COMP-X PIC  9(002) VALUE ZERO.
           05 E                 COMP-X PIC  9(002) VALUE ZERO.
           05 E-ANT             COMP-X PIC  9(002) VALUE ZERO.
           05 USER-IO                  PIC  X(001).
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
           05 CHAR-X.
              10 CHAR-N         COMP-X PIC  9(002).

      *01  DIGITAVEIS.
      *    05 UPP-CASE PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
      *    05 LOW-CASE PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
      *    05 NUMEROS  PIC X(10) VALUE "1234567890".
      *    05 EXTRAS   PIC X(19) VALUE "!$%&*()-+<>[]'/?,.".
      *    05 ASPA     PIC X(01) VALUE '"'.
      *    05 EXTRAS2  PIC X(05) VALUE "{}_\ ".

       COPY CWGETL.
       COPY CWSEND.
       COPY CWUNIX.
       COPY CWBOXW.
       COPY CWSPWS.

       LINKAGE SECTION.

       01  PARAMETROS-CWBOXF.
           05 CWBOXF-FUNCTION          PIC  X(001).
              88 CWBOXF-SHOW                       VALUE "S" "s"
                                                         "P" "p".
              88 CWBOXF-VIEW                       VALUE "V" "v".
              88 CWBOXF-POP-UP                     VALUE "P" "p".
           05 CWBOXF-PROGRAM           PIC  X(008).
           05 CWBOXF-WORK-AREA.
              88 CWBOXS-NO                         VALUE X"0000FFFF".
              10 CWBOXF-CWBOXS         PIC  X(002).
                 88 CWBOXS                         VALUE X"0000".
              10                       PIC  X(001).
              10 CWBOXS-OPTION         PIC  9(002).
              10                       PIC  X(045).
           05 CWBOXF-LINE              PIC  9(002).
           05 CWBOXF-COLUMN            PIC  9(002).
           05 CWBOXF-TYPE              PIC  9(001).
           05 CWBOXF-VERTICAL-LENGTH   PIC  9(002).
           05 CWBOXF-HORIZONTAL-LENGTH PIC  9(002).
           05 CWBOXF-OPTION            PIC  X(076).
           05 CWBOXF-KEY-ON            PIC  X(001).
           05 CWBOXF-KEY               PIC  9(002).
           05 CWBOXF-EDIT REDEFINES CWBOXF-KEY
                                       PIC S9(004) COMP-5.
           05 CWBOXF-START-CURSOR      PIC  9(002) COMP-X.
           05 CWBOXF-TITLE             PIC  X(078).
           05 CWBOXF-COLOR-FRAME       PIC  9(002) COMP-X.
           05 CWBOXF-COLOR-BORDER      PIC  9(002) COMP-X.
           05 CWBOXF-COLOR-SHADE       PIC  9(002) COMP-X.
           05 CWBOXF-COLOR-BARR-MENU   PIC  9(002) COMP-X.
           05 CWBOXF-ORDER             PIC  9(001).
           05 CWBOXF-RETURN            PIC  9(001).
           05 CWBOXF-STRING-1-LENGTH   PIC  9(002).
           05 CWBOXF-STRING-2-LENGTH   PIC  9(002).
           05 CWBOXF-TIMEOUT-STATUS    PIC  9(001).
              88 CWBOXF-TIMEOUT-ENABLE             VALUE 1.
              88 CWBOXF-TIMEOUT-DISABLE            VALUE 0.
           05 CWBOXF-TIMEOUT-RETURN    PIC  9(001).
              88 CWBOXF-TIMEOUT-ON                 VALUE 1.
              88 CWBOXF-TIMEOUT-OFF                VALUE 0.

       PROCEDURE DIVISION USING PARAMETROS-CWBOXF.

       000-INICIO.

      *    CALL "CWUSER" USING X"FF"

           DISPLAY "CWFIND" UPON ENVIRONMENT-NAME
           ACCEPT  CWFIND   FROM ENVIRONMENT-VALUE
           INSPECT CWFIND CONVERTING MINUSCULAS TO MAIUSCULAS
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
           INSPECT CWBOXF-OPTION CONVERTING LOW-VALUE TO SPACE
           IF  CWBOXS
               DISPLAY "CWBOXS-EXIT" UPON ENVIRONMENT-NAME
               ACCEPT CWBOXS-EXIT FROM ENVIRONMENT-VALUE
               DISPLAY SPACES UPON ENVIRONMENT-VALUE
           END-IF
           DISPLAY "NEWOBJECTS" UPON ENVIRONMENT-NAME
           DISPLAY "X"          UPON ENVIRONMENT-VALUE
           DISPLAY "CWCASE" UPON ENVIRONMENT-NAME
           ACCEPT CWCASE FROM ENVIRONMENT-VALUE
           INSPECT CWCASE CONVERTING MINUSCULAS TO MAIUSCULAS
           DISPLAY "CWLITS" UPON ENVIRONMENT-NAME
           ACCEPT CWLITS FROM ENVIRONMENT-VALUE
           INSPECT CWLITS CONVERTING MINUSCULAS TO MAIUSCULAS
           DISPLAY "CWACCENT" UPON ENVIRONMENT-NAME
           ACCEPT CWACCENT FROM ENVIRONMENT-VALUE
           INSPECT CWACCENT CONVERTING MINUSCULAS TO MAIUSCULAS
           DISPLAY "CWFONT-FIELD-NAME"  UPON ENVIRONMENT-NAME
           ACCEPT  MFF             FROM ENVIRONMENT-VALUE
           CALL "CWATTR" USING STATIC ENTRY-ATTR DISABLE-ATTR CURCOLOR.
           IF    CURCOLOR > X"01"
                 MOVE CURCOLOR TO OK2
           END-IF
           CALL "CWKBST" USING "C"
           CALL "CWCRTS" USING "G" CRT-STATUS
           IF   CRT-STATUS = X"FFFFFF"
                ADD 1 TO SAIR
                IF  SAIR = 2
                    STOP RUN
                ELSE
                    GOBACK
                END-IF
           END-IF
           COPY CWSPPD.

           IF  CWBOXF-POP-UP
               DISPLAY "CWMULTI" UPON ENVIRONMENT-NAME
               DISPLAY "ON"      UPON ENVIRONMENT-VALUE
               CALL "TXBOXF" USING PARAMETROS-CWBOXF
               GOBACK
           END-IF

           IF   CWBOXF-LINE = 0
           OR   CWBOXF-LINE NOT NUMERIC
                MOVE 1 TO CWBOXF-LINE
           END-IF

           IF   CWBOXF-COLUMN = 0
           OR   CWBOXF-COLUMN NOT NUMERIC
                MOVE 1 TO CWBOXF-COLUMN
           END-IF

           IF   CWBOXF-VERTICAL-LENGTH = 0
           OR   CWBOXF-VERTICAL-LENGTH NOT NUMERIC
                MOVE 1 TO CWBOXF-VERTICAL-LENGTH
           END-IF

           IF   CWBOXF-HORIZONTAL-LENGTH = 0
           OR   CWBOXF-HORIZONTAL-LENGTH NOT NUMERIC
                MOVE 10 TO CWBOXF-HORIZONTAL-LENGTH
           END-IF
           MOVE 1 TO SEM-DADOS

           COMPUTE ITEM-LEN = CWBOXF-STRING-1-LENGTH
                            + CWBOXF-STRING-2-LENGTH
           IF   ITEM-LEN > CWBOXF-HORIZONTAL-LENGTH
                MOVE ITEM-LEN TO CWBOXF-HORIZONTAL-LENGTH
           END-IF

           IF  (CWBOXF-LINE   + CWBOXF-VERTICAL-LENGTH) > 26
           OR  (CWBOXF-COLUMN + CWBOXF-HORIZONTAL-LENGTH) > 81
               MOVE "Erro de limites no uso da CWBOXF" TO SP2-MS-TEXT
               PERFORM SENDMSG
               EXIT PROGRAM
           END-IF

           IF   CWBOXF-STRING-1-LENGTH = 0
                MOVE CWBOXF-HORIZONTAL-LENGTH TO CWBOXF-STRING-2-LENGTH
           ELSE
                IF   CWBOXF-STRING-2-LENGTH = 0
                     MOVE CWBOXF-HORIZONTAL-LENGTH
                       TO CWBOXF-STRING-1-LENGTH
                END-IF
           END-IF

           MOVE 0  TO COL-1
           MOVE 10 TO WD
           IF   CWBOXF-STRING-2-LENGTH = 0
                MOVE 1 TO RESTO
           ELSE
                MOVE CWBOXF-STRING-2-LENGTH TO RESTO
           END-IF
           COMPUTE COL-2 = COL-1 + CWBOXF-STRING-1-LENGTH
           COMPUTE COL-B = COL-2 + RESTO
           COMPUTE WID-1 = (COL-2 - COL-1) * WD
           COMPUTE WID-2 = (COL-B - COL-2) * WD
           COMPUTE WID-G = WID-1 + WID-2 + 20
           IF   CWBOXF-STRING-2-LENGTH = 0
                ADD WD TO WID-1
           END-IF

           CALL "CWGETL" USING PARAMETROS-CWGETL

           SET CWBOXF-TIMEOUT-OFF TO TRUE
           IF   CWGETL-TIMEOUT NOT = 0
           AND  CWBOXF-TIMEOUT-ENABLE
                IF   CWGETL-TIMEOUT > 255
                     MOVE 255 TO SEGUNDOS
                ELSE
                     MOVE CWGETL-TIMEOUT TO SEGUNDOS
                END-IF
           ELSE
                MOVE 0                   TO SEGUNDOS
           END-IF

           MOVE SEGUNDOS (1: 1) TO SP2-CD-TIMEOUT
           IF   SEGUNDOS = 0
                MOVE X"00" TO SP2-CD-WAIT-SW
           ELSE
                MOVE "a"   TO SP2-CD-WAIT-SW
           END-IF
           MOVE "O"    TO USER-IO
           MOVE SPACES TO CWBOXF-STRING-1
                          CWBOXF-STRING-2
           ADD 1 TO CWBOXF-VERTICAL-LENGTH
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
                   CALL "CWSEND" USING PARAMETROS-CWSEND
                   GOBACK
                NOT EXCEPTION
                    IF AT-END
                    AND ((CWBOXF-STRING-1 NOT = SPACES)
                      OR (CWBOXF-STRING-2 NOT = SPACES))
                       MOVE "O" TO USER-IO
                    END-IF
                    IF AT-END
                       MOVE "C" TO USER-IO
                       IF   CWBOXF-PROGRAM NOT = "CWREAD"
                            CALL CWBOXF-PROGRAM USING USER-IO
                                                 CWBOXF-ORDER
                                                 CWBOXF-STRING-1
                                                 CWBOXF-STRING-2
                                                 CWBOXF-VERTICAL-LENGTH
                                                 CWBOXF-WORK-AREA
                            CANCEL CWBOXF-PROGRAM
                       END-IF
                       MOVE SPACES TO CWSEND-MSG CWSEND-SCREENS
                       MOVE "Consulta sem informa‡äes" TO CWSEND-MSG
                       CALL "CWSEND" USING PARAMETROS-CWSEND
                       GOBACK
                    END-IF
           END-CALL
           IF   CWBOXF-VERTICAL-LENGTH < SAVE-VERT
                MOVE 1 TO MENOR
           ELSE
                SUBTRACT 1 FROM CWBOXF-VERTICAL-LENGTH
           END-IF
           MOVE "CWBOXF" TO SP2-ND-NAME
           CALL SP2   USING SP2-ACTIVATE-INTERNAL SP2-NAME-DEF
           PERFORM 200-SHOW THRU 200-99-FIM

           CALL CWBOXF-PROGRAM USING "C" CWBOXF-ORDER
                                         CWBOXF-STRING-1
                                         CWBOXF-STRING-2
                                         CWBOXF-VERTICAL-LENGTH
                                         CWBOXF-WORK-AREA
           CANCEL CWBOXF-PROGRAM
           MOVE "CWBOXF" TO SP2-ND-NAME
           CALL SP2   USING SP2-ACTIVATE-INTERNAL SP2-NAME-DEF
           MOVE LOW-VALUES TO SP2-FD-DATA
           PERFORM UNTIL FIELDS = 0
                   MOVE FD-ID (FIELDS) TO SP2-FD-ID
                   CALL "CWSPID" USING SP2-FD-ID "FDelete"
                   CALL SP2   USING SP2-DELETE-FIELD SP2-FIELD-DEF
                   SUBTRACT 1 FROM FIELDS
           END-PERFORM
           IF   TITLE-ID NOT = 0
                MOVE TITLE-ID TO SP2-SD-ID
                CALL "CWSPID" USING SP2-SD-ID "SDelete"
                CALL SP2   USING SP2-DELETE-STATIC SP2-STATIC-DEF
           END-IF

           CALL SP2   USING SP2-CLOSE-WINDOW SP2-WINDOW-DEF
           MOVE CORRENTE TO SP2-ND-NAME
           CALL SP2   USING SP2-ACTIVATE-INTERNAL SP2-NAME-DEF
           CALL SP2   USING SP2-DISPLAY-WINDOW SP2-NULL-PARM
           IF  CWBOXS
           AND (CWBOXS-EXIT-CHAR NOT = SPACES)
           AND  CWBOXS-OPTION(1:2) = SPACES
               MOVE CWBOXS-EXIT-CHAR TO CWBOXS-OPTION
               IF  (CWBOXF-KEY-ON = "*" OR "^")
               AND TECLA > 0
                   MOVE TECLA TO CWBOXF-EDIT
               END-IF
               IF  CWBOXF-KEY-ON = "Y"
               AND TECLA2 > 0
                   MOVE TECLA2    TO CWBOXF-KEY
              END-IF
           END-IF.

       000-99-FIM. GOBACK.

       200-SHOW.

           PERFORM 205-INICIO-BOXWRK THRU 205-99-FIM

           MOVE ">" TO FIM
           ADD  1   TO C
           INITIALIZE LISTAS

           IF  CWBOXF-STRING-1-LENGTH NOT = 0
               COMPUTE GAP = 2 + CWBOXF-STRING-1-LENGTH
           ELSE
               MOVE 2 TO GAP
           END-IF

           IF   CWBOXF-ORDER = 0
                MOVE 1 TO CWBOXF-ORDER
           END-IF
           MOVE CWBOXF-COLUMN  TO C

           perform open-panel thru fim-open-panel

      * Enty-Fields
           IF   CWBOXF-STRING-1-LENGTH NOT = 0
                MOVE 1                      TO STR
                MOVE 0                      TO L
                MOVE CWBOXF-STRING-1-LENGTH TO STR-LEN
                                               ST1
                PERFORM CREATE-FIELD CWBOXF-VERTICAL-LENGTH TIMES
           END-IF

           IF   CWBOXF-STRING-2-LENGTH NOT = 0
                MOVE 2                      TO STR
                MOVE 0                      TO L
                MOVE CWBOXF-STRING-2-LENGTH TO STR-LEN
                MOVE    STR-LEN             TO ST2
                PERFORM CREATE-FIELD CWBOXF-VERTICAL-LENGTH TIMES
           END-IF

      * Vertical scrow-bar
           MOVE LOW-VALUES       TO SP2-FD-DATA
                                    SP2-FD-VAR-LENS
           MOVE 0                TO SP2-FD-VAR-LEN
           ADD  1                TO FIELDS
           MOVE FIELDS           TO SP2-FD-FLD-NUM
                                    SP2-FD-TAB-NUM
           CALL "CWSPID"      USING SP2-FD-ID "FInsert"
           MOVE SP2-FD-ID        TO FD-ID (FIELDS)
                                    BARR-ID
           MOVE 0                TO SP2-FD-ROW
           COMPUTE SP2-FD-HEIGHT  = CWBOXF-VERTICAL-LENGTH * 10
           IF   TITULO NOT = SPACES
                ADD 10 TO SP2-FD-HEIGHT
           END-IF
           MOVE COL-B            TO SP2-FD-COL
           MOVE CURCOLOR         TO SP2-FD-CUR-COLR
           MOVE 25               TO SP2-FD-WIDTH
           MOVE 30               TO SP2-FD-WIDTH
290516     MOVE 20               TO SP2-FD-WIDTH
           MOVE "v"              TO SP2-FD-CTRL-TYPE
           MOVE SIZE-FIELDS      TO SP2-FD-PROG-OFF
                                    END-W
           COMPUTE BARRPOS = SIZE-FIELDS + 1
           ADD  5                TO SIZE-FIELDS
           MOVE 5                TO SP2-FD-MAX-LEN
                                    SP2-FD-PROG-LEN
           IF   CWBOXS
                IF   CWBOXF-STRING-1-LENGTH = 0
                OR   CWBOXF-STRING-2-LENGTH = 0
                     MOVE 10 TO WD2
                END-IF
           ELSE
               CALL SP2     USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
           END-IF

           PERFORM 210-MONTA-TELA THRU 210-99-FIM

           IF  SEM-DADOS = 1
               GO TO FIM-SHOW
           END-IF

           IF  VEZ2 = ZERO
               IF  CWBOXF-OPTION = SPACES
               AND (CWBOXF-PROGRAM NOT = "CWREAD")
                   MOVE X'01' TO CWBOXF-OPTION
               END-IF
               MOVE 1     TO VEZ2
           END-IF
           IF  CWBOXF-OPTION NOT = SPACES
               IF  CWBOXF-OPTION = X'01'
                   MOVE SPACES TO CWBOXF-OPTION
               END-IF
               MOVE 0 TO KURSOR
               MOVE 1 TO SP2-BUG
               PERFORM VARYING M FROM LENGTH OF CWBOXF-OPTION
                           BY -1
                           UNTIL CWBOXF-OPTION (M: 1) NOT = SPACES
                         CONTINUE
               END-PERFORM
               INSPECT CWBOXF-OPTION (M: )
                       CONVERTING SPACE TO LOW-VALUE
               IF LOADED = 1
                  move m to kursor
               END-IF
               MOVE 1 TO L M
               PERFORM FIND-CURSOR THRU FIM-FIND-CURSOR
               MOVE SPACES TO CWBOXF-OPTION
           END-IF
           MOVE "CWBOXF"  TO SP2-CD-NEXT-PANEL
           MOVE 0         TO SP2-CD-KEY TECLA
           MOVE 1         TO M L KURSOR
           move spaces    to CWBOXS-EXIT-CHAR
                   move 1 to vez
           PERFORM TEST AFTER UNTIL (SP2-CD-KEY = SP2-KEY-ESC
                                 OR SP2-KEY-ENTER OR CWBOXS-NO)
                                 or (CWBOXS-EXIT-CHAR not = space)
                   PERFORM UNTIL M = 1
                              OR TEXTO-L (M) NOT = SPACES
                                 SUBTRACT 1 FROM M L
                   END-PERFORM
                   IF   CWBOXS
                        PERFORM UNTIL M NOT < CWBOXS-OPTION
                                ADD 1 TO M
                        END-PERFORM
                        MOVE 0 TO CWBOXS-OPTION
                   END-IF
                   MOVE OK2 TO OK
                   PERFORM CURSOR-BARRA
                   CALL SP2   USING SP2-DISPLAY-WINDOW SP2-NULL-PARM
                   PERFORM UNTIL OK = X"01" OR CWBOXS-NO
                           MOVE "00050"    TO FIELD-AREA (BARRPOS: 5)
                           MOVE LOW-VALUES TO SP2-FD-DATA
                           MOVE CURSOR-ID  TO SP2-FD-ID
                           MOVE 2000       TO SP2-FD-VAR-LEN
                           CALL SP2   USING SP2-GET-FIELD-DEF
                                            SP2-FIELD-DEF
                           COMPUTE OFF-X = SP2-FD-PROG-OFF + 1
                           COMPUTE SP2-FD-COL = SP2-FD-COL + KURSOR - 1
                           MOVE SP2-FD-PROG-LEN TO SZ
                           MOVE 1  TO SP2-FD-INITIAL-LEN
                                      SP2-FD-VAR-LEN
                                      SP2-FD-MAX-LEN
                                      SP2-FD-PROG-LEN
                           ADD KURSOR TO SP2-FD-PROG-OFF
                           SUBTRACT 1 FROM SP2-FD-PROG-OFF
                           COMPUTE OFF-W = SP2-FD-PROG-OFF + 1
                           IF CWBOXS
                              MOVE LOW-VALUE TO SAVE-A
                           ELSE
                              MOVE FIELD-AREA (OFF-W: 1) TO SAVE-A
                           END-IF
                           MOVE 12         TO SP2-FD-WIDTH
                           CALL "CWSPID" USING SP2-FD-ID "FInsert"
                           MOVE SP2-FD-ID  TO SP2-CD-NEXT-FLD-ID
                           MOVE "y"        TO SP2-FD-CURS-SHOW
                           MOVE "b"        TO SP2-FD-CURS-SHOW
                           MOVE "y"        TO SP2-FD-CURS-SKIP
                           MOVE "o"        TO SP2-FD-PROG-CTRL
                           MOVE X"00"      TO SP2-FD-OUTPUT
                           MOVE m          to save-m
                           IF   CWCASE = "UPP"
                           OR  (CWBOXS
                           AND  CWBOXF-PROGRAM = "CWREAD")
                                MOVE "u" TO SP2-FD-CASE
                           END-IF
                           IF   CWCASE = "LOW"
                                MOVE "l" TO SP2-FD-CASE
                           END-IF
                           move OK2    to sp2-PD-CUR-FLD-COLR
                           CALL SP2   USING SP2-SET-FIELD-DEF
                                            SP2-FIELD-DEF
                           CALL SP2   USING SP2-DISPLAY-WINDOW
                                            SP2-NULL-PARM
                           IF    vez = 2
                           and   SP2-CD-WAIT-SW = 'k'
                                 MOVE x'00'      TO SP2-CD-WAIT-SW
                           end-if
                           if   vez = 1
                           and (not CWBOXS)
      *                          MOVE CURSOR-ID TO SP2-CD-NEXT-FLD-ID
                                 MOVE "k"       TO SP2-CD-WAIT-SW
                                 move 2         to vez
                           end-if
                           IF  SP2-BUG = 1
                               MOVE SP2-CD-WAIT-SW TO WAIT-SW
                               MOVE "k"            TO SP2-CD-WAIT-SW
                           END-IF
                           CALL SP2   USING SP2-GET-INPUT
                                            SP2-CONVERSE-DATA
                           IF CWBOXS
                              perform 030-ALT THRU 030-99-FIM
                           END-IF
                           IF CWBOXS-EXIT = 'ON'
                           AND CWBOXF-KEY-ON = '*'
                           AND (SP2-CD-KEY = 329 or 337)
                                MOVE X"01" TO OK
                                MOVE "*"   TO CWBOXS-EXIT-CHAR
                                move spaces to CWBOXS-OPTION(1:2)
                                exit perform cycle
                           END-IF
                           IF CWBOXS-EXIT = 'ON'
                              MOVE FIELD-AREA (OFF-W: 1)
                                TO CWBOXS-EXIT-CHAR
                           END-IF
                           IF  SP2-BUG = 1
                               MOVE WAIT-SW TO SP2-CD-WAIT-SW
                               MOVE 0       TO SP2-BUG
                           END-IF
                           IF  (SP2-CD-KEY = SP2-KEY-MOUSE
                            OR (SP2-CD-KEY = SP2-KEY-CTRL-FIELD
                                AND ALTS > 0))
                           AND (SP2-CD-NEXT-FLD-ID NOT = BARR-ID)
                           AND (SP2-CD-NEXT-FLD-ID  =
                                SP2-CD-LAST-FLD-ID OR Mx = 99)
                               MOVE SP2-KEY-ENTER TO SP2-CD-KEY
                           END-IF
                           IF Mx = 99
                              MOVE 0 TO Mx
                           END-IF
                           CALL SP2   USING SP2-DELETE-FIELD
                                            SP2-FIELD-DEF
                           CALL "CWSPID" USING SP2-FD-ID "FDelete"
                           IF   MENOR = 1
                           AND (      SP2-CD-KEY = SP2-KEY-CTRL-PGDN
                                   OR SP2-CD-KEY = SP2-KEY-CTRL-PGUP
                                   OR (SP2-CD-KEY = SP2-KEY-END
                                      AND M = CWBOXF-VERTICAL-LENGTH)
                                   OR (SP2-CD-KEY = SP2-KEY-HOME
                                      AND M = 1)
                                   OR SP2-CD-KEY = SP2-KEY-PGDN
                                   OR SP2-CD-KEY = SP2-KEY-PGUP
                                  )
                                   MOVE 0 TO SP2-CD-KEY
                           END-IF
                           IF   CWBOXS
                           AND  CWBOXF-PROGRAM = "CWREAD"
                           AND (SP2-CD-KEY = SP2-KEY-RIGHT
                            OR  SP2-CD-KEY = SP2-KEY-LEFT)
                                IF   SP2-CD-KEY = SP2-KEY-RIGHT
                                     MOVE ">" TO CWBOXF-OPTION(3:1)
                                END-IF
                                IF   SP2-CD-KEY = SP2-KEY-LEFT
                                     MOVE "<" TO CWBOXF-OPTION(3:1)
                                END-IF
                                MOVE "00"          TO CWBOXF-STRING-1
                                MOVE SP2-KEY-ENTER TO SP2-CD-KEY
                                MOVE 9             TO EDIT-OK
                                MOVE X"01"         TO OK
                                EXIT PERFORM CYCLE
                           END-IF
                           IF   SP2-CD-KEY = SP2-KEY-RIGHT
                                IF  KURSOR NOT > SZ
                                    ADD 1 TO KURSOR
                                END-IF
                                EXIT PERFORM CYCLE
                           END-IF
                           IF   SP2-CD-KEY = SP2-KEY-LEFT
                                IF  KURSOR > 1
                                    SUBTRACT 1 FROM KURSOR
                                END-IF
                                EXIT PERFORM CYCLE
                           END-IF
                           IF NOT CWBOXS
                              ADD 1 TO KURSOR
                              MOVE 0 TO MK
                           ELSE
                               IF Mx > 0
                                  MOVE X"01" TO OK
                                  PERFORM CURSOR-BARRA
                                  MOVE M  TO MK
                                  MOVE Mx TO M
                                  MOVE 0  TO Mx
                               END-IF
                           END-IF
                           IF  (SP2-CD-KEY = SP2-KEY-CTRL-FIELD)
                           AND ((FIELD-AREA (OFF-W: 1) NOT = SAVE-A)
                                OR CWBOXS)
                                IF  CWBOXS
                                AND FIELD-AREA (OFF-W: 1) = SAVE-A
                                    MOVE M TO SAVE-M2
                                    PERFORM VARYING M FROM 1 BY 1
                                        UNTIL M > 25
                                       OR SP2-CD-NEXT-FLD-ID = LISTA (M)
                                                            OR LISTA2(M)
                                           CONTINUE
                                    END-PERFORM
                                    IF SAVE-M NOT = M
                                       MOVE M       TO SAVE-M3
                                       MOVE SAVE-M2 TO M
                                       MOVE X"01"   TO OK
                                       PERFORM CURSOR-BARRA
                                       MOVE SAVE-M3 TO M
                                       MOVE OK2     TO OK
                                       PERFORM CURSOR-BARRA
                                    ELSE
                                       MOVE SP2-KEY-ENTER TO SP2-CD-KEY
                                       MOVE X"01"         TO OK
                                    END-IF
                                    EXIT PERFORM CYCLE
                                END-IF
060616*                         IF   M NOT = 1
060616*                         OR   MK > 1
060616                          IF   MK > 1
                                     MOVE X"01" TO OK
                                     PERFORM CURSOR-BARRA
                                     IF SP2-CD-KEY = -4
                                     AND (SP2-CD-NEXT-FLD-ID <>
                                         SP2-CD-LAST-FLD-ID)
                                          IF M <> MA
                                             MOVE M TO MA
                                             SUBTRACT 1 FROM M L
                                             MOVE SP2-KEY-DOWN
                                               TO SP2-CD-KEY
                                             MOVE SP2-CD-NEXT-FLD-ID
                                               TO SP2-CD-LAST-FLD-ID
                                             EXIT PERFORM CYCLE
                                          ELSE
                                             MOVE 0 TO MA
                                             EXIT PERFORM
                                          END-IF
      *                                   MOVE 99 TO Mx
                                     ELSE
                                          MOVE 1 TO L M
                                     END-IF
                                     IF  CWBOXF-ORDER < 2
                                         MOVE LISTA  (M)
                                           TO SP2-CD-NEXT-FLD-ID
                                     ELSE
                                         MOVE LISTA2 (M)
                                           TO SP2-CD-NEXT-FLD-ID
                                     END-IF
      *                              IF Mx = 99
      *                                 MOVE 0 TO Mx
      *                                 EXIT PERFORM CYCLE
      *                              END-IF
                                     MOVE SPACES TO CWBOXF-OPTION
                                     PERFORM FIND-CURSOR
                                        THRU FIM-FIND-CURSOR
                                     MOVE OK2 TO OK
                                     PERFORM CURSOR-BARRA
                                ELSE
                                     MOVE 0      TO MA
                                     MOVE SPACES TO CWBOXF-OPTION
                                     PERFORM FIND-CURSOR
                                        THRU FIM-FIND-CURSOR
                                END-IF
                                IF  CWBOXS
                                    MOVE X"01" TO OK
                                END-IF
                                EXIT PERFORM CYCLE
                           END-IF
                           IF   SP2-CD-KEY NOT = SP2-KEY-CTRL-FIELD
                                MOVE 1 TO KURSOR
                           END-IF
                           IF   NOT (SP2-CD-KEY = -1 OR 0)
                                MOVE X"01" TO OK
                           END-IF
                           IF   SP2-CD-KEY = SP2-KEY-TIMEOUT
                                MOVE SP2-KEY-ESC      TO SP2-CD-KEY
                                SET CWBOXF-TIMEOUT-ON TO TRUE
                           ELSE
                                IF   CWBOXF-KEY-ON = "Y" OR "y" OR "*"
                                     OR "^" OR "S" OR "s"
                                     MOVE 0 TO TECLA TECLA2
                                     PERFORM 100-CONVERTE-TECLA
                                        THRU 100-99-FIM
                                     IF (CWBOXF-KEY-ON = "*" OR "^")
                                     AND((TECLA NOT = 0)
                                     OR  (TECLA2 NOT = 0))
                                     OR  (SP2-CD-KEY = SP2-KEY-ESC)
                                     OR  (SP2-CD-KEY = SP2-KEY-ENTER)
                                         MOVE X"01" TO OK
                                         IF  TECLA2 NOT = 0
                                             MOVE TECLA2 TO CWBOXF-EDIT
                                         ELSE
                                             MOVE SP2-CD-KEY
                                               TO CWBOXF-EDIT
                                         END-IF
                                     ELSE
                                         IF CWBOXF-KEY-ON = "Y"
                                         AND (TECLA2 NOT = 0)
                                             MOVE SP2-KEY-ENTER
                                                         TO SP2-CD-KEY
                                             MOVE X"01"  TO OK
                                             MOVE TECLA2 TO CWBOXF-KEY
                                         END-IF
                                     END-IF
                                END-IF
                           END-IF
                   END-PERFORM
                   PERFORM CURSOR-BARRA
                   IF   SP2-CD-KEY = SP2-KEY-CLOSE
                        MOVE SP2-KEY-ESC TO SP2-CD-KEY
                   END-IF
                   IF   SP2-CD-KEY = SP2-KEY-SYS-SHUTDOWN
                   OR   SP2-KEY-APP-CLOSE
                        MOVE SP2-KEY-ESC TO SP2-CD-KEY
                        CALL "CWCRTS" USING "S" X"FFFFFF"
                        MOVE SP2-KEY-ESC TO SP2-CD-KEY
                   END-IF
                   IF  (FIELD-AREA (BARRPOS: 5) NOT = "00050"
                   OR   SP2-CD-NEXT-FLD-ID = BARR-ID)
                   AND (SP2-CD-KEY NOT = SP2-KEY-ESC)
                        MOVE 1 TO KURSOR
                        IF   FIELD-AREA (BARRPOS: 5) = "00050"
                        AND  BARRA = 51
                             MOVE "00049" TO FIELD-AREA (BARRPOS: 5)
                        ELSE
                             IF   FIELD-AREA (BARRPOS: 5) = "00050"
                             AND  BARRA = 49
                                  MOVE "00051" TO FIELD-AREA(BARRPOS: 5)
                            END-IF
                        END-IF
                        MOVE FIELD-AREA (BARRPOS: 5) TO BARRA
                        MOVE 0                       TO SP2-CD-KEY
                        IF   BARRA > 50
                             IF  BARRA = 51
                                 MOVE SP2-KEY-DOWN TO SP2-CD-KEY
                             ELSE
                                 MOVE SP2-KEY-PGDN TO SP2-CD-KEY
                                 IF  BARRA > 98
                                     MOVE SP2-KEY-CTRL-PGDN
                                       TO SP2-CD-KEY
                                 END-IF
                             END-IF
                        ELSE
                             IF  BARRA = 49
                                 MOVE SP2-KEY-UP   TO SP2-CD-KEY
                             ELSE
                                 MOVE SP2-KEY-PGUP TO SP2-CD-KEY
                                 IF  BARRA < 2
                                     MOVE SP2-KEY-CTRL-PGUP
                                       TO SP2-CD-KEY
                                 END-IF
                             END-IF
                        END-IF
                   END-IF
                   IF   SP2-CD-KEY = -4
                        IF SP2-CD-NEXT-FLD-ID <> 0
                           PERFORM VARYING X FROM 1 BY 1 UNTIL X > 25
                                   OR SP2-CD-NEXT-FLD-ID = LISTA (X)
                                   OR SP2-CD-NEXT-FLD-ID = LISTA2 (X)
                                      CONTINUE
                           END-PERFORM
                        ELSE
                            IF M = 0
                               MOVE 1 TO M
                            END-IF
                            IF STR = 1
                               MOVE LISTA (M) TO SP2-CD-NEXT-FLD-ID
                            ELSE
                               MOVE LISTA2(M) TO SP2-CD-NEXT-FLD-ID
                            END-IF
                        END-IF
                        IF   SP2-CD-CTRL-FIELD-KEY = SP2-KEY-MOUSE
                             IF   X = M
                                  MOVE SP2-KEY-ENTER TO SP2-CD-KEY
                                  IF  CWBOXF-KEY-ON = "*" OR "^"
                                      MOVE SP2-KEY-ENTER TO CWBOXF-EDIT
                                  END-IF
                             ELSE
                                  IF X < 26
                                     MOVE X TO M
                                  ELSE
                                     MOVE 1 TO KURSOR
                                  END-IF
                             END-IF
                        ELSE
                             MOVE SP2-CD-CTRL-FIELD-KEY TO SP2-CD-KEY
                        END-IF
                   END-IF
                   MOVE 0 TO TECLA
                   EVALUATE SP2-CD-KEY
                       WHEN SP2-KEY-CLOSE
                            MOVE SP2-KEY-ESC           TO SP2-CD-KEY
                       WHEN SP2-KEY-HOME
                            SET EDIT-HOME              TO TRUE
                       WHEN SP2-KEY-END
                            SET EDIT-END               TO TRUE
                       WHEN SP2-KEY-DOWN
                            SET EDIT-CURSOR-DOWN       TO TRUE
                       WHEN SP2-KEY-TAB
                            SET EDIT-CURSOR-DOWN       TO TRUE
                       WHEN SP2-KEY-UP
                            SET EDIT-CURSOR-UP         TO TRUE
                       WHEN SP2-KEY-BACKTAB
                            SET EDIT-CURSOR-UP         TO TRUE
                       WHEN SP2-KEY-CTRL-PGUP
                            SET EDIT-CONTROL-PAGE-UP   TO TRUE
                       WHEN SP2-KEY-CTRL-PGDN
                            SET EDIT-CONTROL-PAGE-DOWN TO TRUE
                       WHEN SP2-KEY-PGUP
                            SET EDIT-PAGE-UP           TO TRUE
                       WHEN SP2-KEY-PGDN
                            SET EDIT-PAGE-DOWN         TO TRUE
                   END-EVALUATE
                   IF  (TECLA NOT = 0)
                   AND CWBOXF-VERTICAL-LENGTH > 1
                        PERFORM ROLAGEM THRU FIM-ROLAGEM
                   END-IF
                   IF   SP2-CD-KEY NOT = SP2-KEY-HOME
                        MOVE SP2-CD-WAIT-SW TO WAIT-SW
                        MOVE SP2-CD-KEY     TO CD-KEY
                        MOVE "k"            TO SP2-CD-WAIT-SW
                        CALL SP2         USING SP2-GET-INPUT
                                               SP2-CONVERSE-DATA
                        MOVE WAIT-SW        TO SP2-CD-WAIT-SW
                        MOVE CD-KEY         TO SP2-CD-KEY
                   END-IF
                   IF  M NOT = SAVE-M
                       MOVE 1 TO KURSOR
                   END-IF
           END-PERFORM

           IF  SP2-CD-KEY = SP2-KEY-ESC
               SET ESC TO TRUE
               IF  (CWBOXF-KEY-ON = "*" OR "^")
                   MOVE SP2-KEY-ESC TO CWBOXF-EDIT
               END-IF
           END-IF

           CALL "CWAKEY" USING TECLA2 MIL.

       FIM-SHOW.

           IF   SP2-CD-KEY = SP2-KEY-ENTER
                EVALUATE TRUE
                    WHEN EDIT-OK = 9
                         MOVE CWBOXF-STRING-1(1: 2) TO CWBOXF-OPTION
                         MOVE CWBOXF-STRING-1(80:1) TO CWBOXF-OPTION(3:)
                    WHEN CWBOXF-RETURN = 1
                         MOVE TEXTO-1 (M) TO CWBOXF-OPTION
                         IF   CWBOXS
                         AND  CWBOXF-PROGRAM = "CWREAD"
                              MOVE TEXTO-1 (M) (80: 1)
                              TO CWBOXF-OPTION (3: )
                         END-IF
                    WHEN CWBOXF-RETURN = 2
                         MOVE TEXTO-2 (M) TO CWBOXF-OPTION
                END-EVALUATE
           ELSE
                MOVE SPACES TO CWBOXF-OPTION
           END-IF

           IF  CWBOXF-POP-UP
           OR  CWBOXF-VIEW
               MOVE CWBOXF-LINE              TO CWBOXW-LINE    L L2
               MOVE CWBOXF-COLUMN            TO CWBOXW-COLUMN  C
               MOVE CWBOXF-VERTICAL-LENGTH   TO CWBOXW-VERTICAL-LENGTH
               MOVE CWBOXF-HORIZONTAL-LENGTH TO CWBOXW-HORIZONTAL-LENGTH
               SET   CWBOXW-OPEN             TO TRUE
               CALL "CWBOXW"              USING PARAMETROS-CWBOXW
               SET CWBOXW-POPUP              TO TRUE
               CALL "CWBOXW"              USING PARAMETROS-CWBOXW
               ADD  1 TO C
               move C TO Y
               IF    TITULO = SPACES
               AND  (SP2-WD-TITLE NOT = LOW-VALUES)
                    MOVE SP2-WD-TITLE TO TITULO
               END-IF
               IF    TITULO NOT = SPACES
                     PERFORM VARYING I FROM LENGTH OF TITULO BY -1
                                     UNTIL I = 1
                                       OR (TITULO (I: 1) NOT = SPACE)
                             CONTINUE
                     END-PERFORM
                     INSPECT TITULO
                             CONVERTING ACENTOS-WINDOWS TO ACENTOS-850
                     CALL "CWMSGW" USING MSGW-POSIT2
                                         TITULO
               END-IF
               MOVE 1 TO X I
               ADD  1 TO L
               PERFORM CWBOXF-VERTICAL-LENGTH TIMES
                       CALL "CWMSGW" USING MSGW-POSIT
                                           TEXTO-L (X) (2: )
                       IF  X = M
                       AND (CWBOXF-OPTION NOT = SPACES)
                       AND (NOT CWBOXF-VIEW)
                           MOVE L TO L2
                           COMPUTE Y = C - 1
                           CALL "CWMSGW" USING MSGW-POSIT2 ">"
                       END-IF
                       ADD 1 TO L X
               END-PERFORM
           END-IF.

       200-99-FIM. EXIT.

       205-INICIO-BOXWRK.

           MOVE ">" TO FIM
           MOVE "B" TO USER-IO
           PERFORM 250-LER THRU 250-99-FIM.

       205-99-FIM. EXIT.

       206-FIM-BOXWRK.

           MOVE "E" TO USER-IO
           PERFORM 250-LER THRU 250-99-FIM

           COMPUTE VOLTA = CWBOXF-VERTICAL-LENGTH + 1
           PERFORM VOLTA TIMES
             IF  FIM NOT = ">"
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

           MOVE CWBOXF-LINE TO L
           MOVE 0           TO M
                               VOLTA
           MOVE SPACES      TO MATRIZ
           PERFORM CWBOXF-VERTICAL-LENGTH TIMES
             ADD 1 TO M L
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

           MOVE CWBOXF-STRING-1 TO TEXTO-1 (M)
           MOVE CWBOXF-STRING-2 TO TEXTO-2 (M)

           IF  CWBOXF-STRING-1-LENGTH NOT = 0
               MOVE CWBOXF-STRING-1 TO TEXTO-L (M) (2: )
           END-IF

           IF  CWBOXF-STRING-2-LENGTH NOT = 0
               MOVE CWBOXF-STRING-2 TO TEXTO-L (M) (GAP: )
           END-IF

           IF   MFF = "MF"
                INSPECT TEXTO-1(M) CONVERTING ACENTOS-850 TO ACENTOS-437
                INSPECT TEXTO-2(M) CONVERTING ACENTOS-850 TO ACENTOS-437
                INSPECT TEXTO-L(M) CONVERTING ACENTOS-850 TO ACENTOS-437
           ELSE
                INSPECT TEXTO-1(M)
                        CONVERTING ACENTOS-850 TO ACENTOS-WINDOWS
                INSPECT TEXTO-2(M)
                        CONVERTING ACENTOS-850 TO ACENTOS-WINDOWS
                INSPECT TEXTO-L(M)
                        CONVERTING ACENTOS-850 TO ACENTOS-WINDOWS
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

           move 128            to inverte
           MOVE CWBOXF-LINE    TO L2
           MOVE 0              TO M2
           MOVE SPACES         TO WS-BLOCKS
           MOVE 1              TO K1
           MOVE 1              TO K2
           PERFORM CWBOXF-VERTICAL-LENGTH TIMES
             ADD 1 TO M2 L2
             MOVE X"00"     TO COR-NOVA
             IF COR-TAB (M2) NOT = 0
                MOVE COR-TAB (M2) TO COR-N
                MOVE COR-N (1:1) TO COR-NOVA
             END-IF
             IF   CWBOXF-STRING-1-LENGTH NOT = 0
                  MOVE K1 TO KX
                  MOVE TEXTO-1 (M2) TO WS-BLOCK (1)
                                       (KX: CWBOXF-STRING-1-LENGTH)
                  ADD  ST1 TO K1
                  IF  CWBOXF-ORDER < 2
                      MOVE LISTA (M2) TO SP2-FD-ID
                      MOVE 2000       TO SP2-FD-VAR-LEN
                      CALL SP2   USING SP2-GET-FIELD-DEF SP2-FIELD-DEF
                      CALL SP2   USING SP2-delete-FIELD SP2-FIELD-DEF
                      IF  TEXTO-1 (M2) = SPACES
                          MOVE "g" TO SP2-FD-OUTPUT
                      ELSE
                          MOVE "p" TO SP2-FD-OUTPUT
                      END-IF
                      MOVE COR-NOVA     TO SP2-FD-COLR
                      move XCUR         to sp2-fd-cur-colr
<pop>                 if   texto-2 (m2) (79:1) = x'01'
                           move texto-2 (m2) (80:1) to color-alert(1:1)
                           move color-alert(1:1)    to SP2-FD-COLR
                           add inverte              to color-alert
                           move color-alert(1:1)    to sp2-fd-cur-colr
                      end-if
                      CALL SP2   USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
                      IF COR-ITEM = 1 AND LISTA2 (m2) NOT = 0
                         MOVE LISTA2 (M2) TO SP2-FD-ID
                         MOVE 2000        TO SP2-FD-VAR-LEN
                         CALL SP2   USING SP2-GET-FIELD-DEF
                                          SP2-FIELD-DEF
                         CALL SP2   USING SP2-delete-FIELD
                                          SP2-FIELD-DEF
                         MOVE COR-NOVA  TO SP2-FD-COLR
<pop>                 if   texto-2 (m2) (79:1) = x'01'
                           move texto-2 (m2) (80:1) to color-alert(1:1)
                           move color-alert(1:1)    to SP2-FD-COLR
                           add inverte              to color-alert
                           move color-alert(1:1)    to sp2-fd-cur-colr
                      end-if
                         CALL SP2   USING SP2-SET-FIELD-DEF
                                          SP2-FIELD-DEF
                      END-IF
                  END-IF
             END-IF
             IF   CWBOXF-STRING-2-LENGTH NOT = 0
                  MOVE K2 TO KX
                  MOVE TEXTO-2 (M2) TO WS-BLOCK (2)
                                       (KX: CWBOXF-STRING-2-LENGTH)
                  ADD  ST2 TO K2
                  IF  CWBOXF-ORDER > 1
                      MOVE LISTA2(M2) TO SP2-FD-ID
                      MOVE 2000       TO SP2-FD-VAR-LEN
                      CALL SP2   USING SP2-GET-FIELD-DEF SP2-FIELD-DEF
                      CALL SP2   USING SP2-delete-FIELD SP2-FIELD-DEF
                      IF  TEXTO-1 (M2) = SPACES
                          MOVE "g" TO SP2-FD-OUTPUT
                      ELSE
                          MOVE "p" TO SP2-FD-OUTPUT
                      END-IF
                      MOVE COR-NOVA     TO SP2-FD-COLR
                      move XCUR  to sp2-fd-cur-colr
<pop>                 if   texto-2 (m2) (79:1) = x'01'
                           move texto-2 (m2) (80:1) to color-alert(1:1)
                           move color-alert(1:1)    to SP2-FD-COLR
                           add inverte              to color-alert
                           move color-alert(1:1)    to sp2-fd-cur-colr
                      end-if
                      CALL SP2   USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
                      IF COR-ITEM = 1 AND LISTA (m2) NOT = 0
                         MOVE LISTA (M2) TO SP2-FD-ID
                         MOVE 2000       TO SP2-FD-VAR-LEN
                         CALL SP2   USING SP2-GET-FIELD-DEF
                                          SP2-FIELD-DEF
                         CALL SP2   USING SP2-delete-FIELD
                                          SP2-FIELD-DEF
                         MOVE COR-NOVA  TO SP2-FD-COLR
<pop>                 if   texto-2 (m2) (79:1) = x'01'
                           move texto-2 (m2) (80:1) to color-alert(1:1)
                           move color-alert(1:1)    to SP2-FD-COLR
                           add inverte              to color-alert
                           move color-alert(1:1)    to sp2-fd-cur-colr
                      end-if
                         CALL SP2   USING SP2-SET-FIELD-DEF
                                          SP2-FIELD-DEF
                      END-IF
                  END-IF
             END-IF
           END-PERFORM
           IF   CWBOXS
                INSPECT WS-BLOCKS CONVERTING "_" TO SPACE
           END-IF
           MOVE WS-BLOCK (1) TO FIELD-AREA (1: )
           MOVE WS-BLOCK (2) TO FIELD-AREA (K1: )
           MOVE "00050"      TO FIELD-AREA (BARRPOS: 5)
           CALL SP2   USING SP2-SET-PANEL-FIELDS FIELD-AREA.

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
                OR (CWBOXF-RETURN = 1 AND (CWBOXF-STRING-1 NOT = SPACE))
                OR (CWBOXF-RETURN > 1 AND (CWBOXF-STRING-2 NOT = SPACE))
                    CALL CWBOXF-PROGRAM USING USER-IO
                                              CWBOXF-ORDER
                                              CWBOXF-STRING-1
                                              CWBOXF-STRING-2
                                              CWBOXF-VERTICAL-LENGTH
                                              CWBOXF-WORK-AREA
                     MOVE "CWBOXF" TO SP2-ND-NAME
                     CALL SP2   USING SP2-ACTIVATE-INTERNAL
                                      SP2-NAME-DEF
                     IF   SPACES = CWBOXF-STRING-1 AND CWBOXF-STRING-2
                     AND (READ-NEXT OR READ-PREVIOUS)
                          ADD 1 TO VAZIO
                          IF   VAZIO = 50
                               SET AT-END TO TRUE
                          END-IF
                     END-IF
           END-PERFORM

           IF   AT-END
                MOVE END-STRING-1 TO CWBOXF-STRING-1
                MOVE END-STRING-2 TO CWBOXF-STRING-2
           END-IF

           IF   SEM-DADOS = 1
           AND (NOT AT-END)
                IF  (CWBOXF-STRING-1 NOT = SPACES)
                AND  CWBOXF-RETURN = 1
                     MOVE 0 TO SEM-DADOS
                END-IF
                IF  (CWBOXF-STRING-2 NOT = SPACES)
                AND  CWBOXF-RETURN > 1
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

       CREATE-FIELD.

           MOVE LOW-VALUES      TO SP2-FD-DATA
                                   SP2-FD-VAR-LENS
           MOVE "p"             TO SP2-FD-OUTPUT
           MOVE "s"             TO SP2-FD-PROG-CTRL
           ADD  1               TO FIELDS
           MOVE FIELDS          TO SP2-FD-FLD-NUM
                                   SP2-FD-TAB-NUM
           CALL "CWSPID" USING SP2-FD-ID "FInsert"
           MOVE SP2-FD-ID       TO FD-ID (FIELDS)
           MOVE L               TO SP2-FD-ROW
           IF   TITULO NOT = SPACES
                ADD 1 TO SP2-FD-ROW
           END-IF

           IF   STR = 2
                MOVE COL-2      TO SP2-FD-COL
                MOVE WID-2      TO SP2-FD-WIDTH
           ELSE
                MOVE COL-1      TO SP2-FD-COL
                MOVE WID-1      TO SP2-FD-WIDTH
           END-IF

           ADD      1           TO L
           IF   CWBOXF-ORDER = STR
           OR  (STR = 2 AND LISTA (L) = 0)
                MOVE "y"        TO SP2-FD-CURS-SHOW
      *****     MOVE "o"        TO SP2-FD-PROG-CTRL
                MOVE SP2-FD-COL TO START-CURSOR
           END-IF

           IF   STR = 1
                MOVE SP2-FD-ID  TO LISTA(L)
           ELSE
                MOVE SP2-FD-ID  TO LISTA2(L)
           END-IF
      *                            SP2-FD-CURS-SKIP
           MOVE 10              TO SP2-FD-HEIGHT
           MOVE STR-LEN         TO SP2-FD-MAX-LEN
                                   SP2-FD-PROG-LEN
                                   SP2-FD-VAR-LEN
           MOVE SIZE-FIELDS     TO SP2-FD-PROG-OFF
           ADD  STR-LEN         TO SIZE-FIELDS
           MOVE 2               TO SP2-FD-FONT-ID
           MOVE -1              TO SP2-FD-FONT-ID
           IF COR-TAB (L) = 0
              MOVE X"01"        TO SP2-FD-COLR
           ELSE
              MOVE COR-TAB (L)  TO COR-N
              MOVE COR-N (1:1)  TO SP2-FD-COLR
           END-IF
           CALL SP2   USING SP2-SET-FIELD-DEF SP2-FIELD-DEF.

       FIM-CREATE-FIELD. EXIT.

       ROLAGEM.

           EVALUATE TRUE
              WHEN EDIT-HOME
               AND M > 1
                   MOVE    1 TO M
                   COMPUTE L  = CWBOXF-LINE + 1
              WHEN EDIT-END
               AND (L NOT = (CWBOXF-LINE + CWBOXF-VERTICAL-LENGTH))
                   MOVE CWBOXF-VERTICAL-LENGTH TO M
                   COMPUTE L = CWBOXF-LINE
                             + CWBOXF-VERTICAL-LENGTH
              WHEN EDIT-CURSOR-DOWN
               AND M < CWBOXF-VERTICAL-LENGTH
                   ADD 1 TO M L
              WHEN EDIT-CURSOR-DOWN
               AND M = CWBOXF-VERTICAL-LENGTH
               AND CWBOXS
               AND CWBOXF-PROGRAM = "CWREAD"
                   MOVE    1 TO M
                   COMPUTE L  = CWBOXF-LINE + 1
              WHEN EDIT-CURSOR-UP
               AND M = 1
               AND CWBOXS
               AND CWBOXF-PROGRAM = "CWREAD"
                   MOVE CWBOXF-VERTICAL-LENGTH TO M
                   COMPUTE L = CWBOXF-LINE
                             + CWBOXF-VERTICAL-LENGTH
              WHEN EDIT-CURSOR-UP
               AND M > 1
                   SUBTRACT 1 FROM M L
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
                        ADD CWBOXF-VERTICAL-LENGTH
                         TO VOLTA
                   ELSE
                        ADD 1 TO VOLTA
                   END-IF
                   PERFORM VOLTA TIMES
                           IF   FIM NOT = ">"
                                SET READ-PREVIOUS TO TRUE
                                PERFORM 250-LER THRU 250-99-FIM
                                IF AT-END
                                   PERFORM 205-INICIO-BOXWRK
                                   THRU 205-99-FIM
                                END-IF
                           END-IF
                   END-PERFORM
                   PERFORM 210-MONTA-TELA THRU 210-99-FIM
              WHEN EDIT-PAGE-DOWN
               AND FIM NOT = "<"
                   MOVE "N" TO FIM
                   PERFORM 210-MONTA-TELA THRU 210-99-FIM
                   MOVE CWBOXF-VERTICAL-LENGTH TO M
                   COMPUTE L = CWBOXF-LINE
                             + CWBOXF-VERTICAL-LENGTH
                   IF   TEXTO-L (1) = SPACE
                        MOVE    "N" TO FIM
                        PERFORM 206-FIM-BOXWRK
                           THRU 206-99-FIM
                   END-IF
           END-EVALUATE
           IF   CWBOXF-VIEW
                SET EDIT-ENTER TO TRUE
                MOVE 1 TO M
           END-IF.

       FIM-ROLAGEM. EXIT.

       SET-CURSOR.

           MOVE SP2-KEY-ESC TO SP2-CD-KEY
           IF  (SP2-CD-NEXT-FLD-ID NOT = LISTA (M))
           AND (SP2-CD-NEXT-FLD-ID NOT = LISTA2 (M))
               PERFORM CURSOR-BARRA
               PERFORM VARYING M FROM 1 BY 1 UNTIL M > 25
                    OR  SP2-CD-NEXT-FLD-ID = LISTA  (M)
                    OR  SP2-CD-NEXT-FLD-ID = LISTA2 (M)
                        CONTINUE
               END-PERFORM
               IF   M > 25
                    MOVE 1  TO M
               ELSE
                    MOVE 0  TO SP2-CD-KEY
               END-IF
               MOVE OK2 TO OK
               PERFORM CURSOR-BARRA
           END-IF.

       FIM-SET-CURSOR. EXIT.

       CURSOR-BARRA.

           MOVE OK        TO COR-NOVA
           IF OK NOT = X'01'
              MOVE 128    TO INVERTE
           ELSE
              MOVE 0      TO INVERTE
           END-IF
           IF COR-TAB (M) NOT = 0
              MOVE COR-TAB (M) TO COR-N
              IF OK NOT = X'01'
                 ADD 128 TO COR-N
              END-IF
              IF SAME-FB2
                 PERFORM TEST AFTER UNTIL COR-N > 0
                         ADD 1 TO COR-N
                 END-PERFORM
              END-IF
              MOVE COR-N (1:1) TO COR-NOVA
           END-IF
           IF  M > 0 AND M < 26
               IF  (LISTA (M) NOT = 0)
               AND  CWBOXF-OPTION = SPACES
                    MOVE LOW-VALUES      TO SP2-FD-DATA
                                            SP2-FD-VAR-LENS
                    MOVE LISTA (M) TO SP2-FD-ID
                    MOVE 2000      TO SP2-FD-VAR-LEN
                    CALL SP2    USING SP2-GET-FIELD-DEF SP2-FIELD-DEF
                    CALL SP2    USING SP2-delete-FIELD SP2-FIELD-DEF
                    MOVE SP2-FD-INITIAL-LEN TO SP2-FD-VAR-LEN
                    MOVE COR-NOVA           TO SP2-FD-COLR
 pep                move XCUR               to sp2-fd-cur-colr
                    ADD  WD2                TO SP2-FD-WIDTH
<pop>               if   texto-2 (m) (79:1) = x'01'
                         move texto-2 (m) (80:1) to color-alert(1:1)
                         add inverte             to color-alert
                         move color-alert(1:1)   to SP2-FD-COLR
                         add inverte             to color-alert
                         move color-alert(1:1)   to sp2-fd-cur-colr
                    end-if
                    CALL SP2   USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
                    IF  OK = OK2
                    AND (CWBOXF-ORDER = 1
                         OR (LISTA2 (M) > 0 AND LISTA(M) = 0))
                        MOVE LISTA (M) TO CURSOR-ID
                    END-IF
               END-IF
               IF   LISTA2 (M) NOT = 0
                    MOVE LOW-VALUES      TO SP2-FD-DATA
                                            SP2-FD-VAR-LENS
                    MOVE LISTA2 (M) TO SP2-FD-ID
                    MOVE 2000       TO SP2-FD-VAR-LEN
                    CALL SP2     USING SP2-GET-FIELD-DEF SP2-FIELD-DEF
                    CALL SP2     USING SP2-delete-FIELD SP2-FIELD-DEF
                    MOVE SP2-FD-INITIAL-LEN TO SP2-FD-VAR-LEN
                    MOVE COR-NOVA           TO SP2-FD-COLR
pep                 move XCUR               to sp2-fd-cur-colr
                    ADD  WD2                TO SP2-FD-WIDTH
<pop>               if   texto-2 (m) (79:1) = x'01'
                         move texto-2 (m) (80:1) to color-alert(1:1)
                         add inverte             to color-alert
                         move color-alert(1:1)   to SP2-FD-COLR
                         add inverte             to color-alert
                         move color-alert(1:1)   to sp2-fd-cur-colr
                    end-if
                    CALL SP2   USING SP2-SET-FIELD-DEF SP2-FIELD-DEF
                    IF  OK = OK2
                    AND ((CWBOXF-ORDER NOT = 1)
                         OR (LISTA2 (M) > 0 AND LISTA(M) = 0))
                        MOVE LISTA2 (M) TO CURSOR-ID
                    END-IF
               END-IF
           END-IF.

       FIM-BARRA.

       SENDMSG.

           MOVE "b"      TO SP2-MS-ICON
           MOVE "o"      TO SP2-MS-BUTTON
           MOVE "Aviso:" TO SP2-MS-TITLE
           MOVE 1        TO SP2-MS-LINE-CNT
           CALL SP2   USING SP2-DISPLAY-MESSAGE
                            SP2-MESSAGE-DATA.

       FIM-SENDMSG. EXIT.

       OPEN-PANEL.

           CALL SP2   USING SP2-DISPLAY-WINDOW SP2-NULL-PARM
           MOVE LOW-VALUES       TO SP2-WD-DATA
           CALL SP2   USING SP2-GET-WINDOW-DEF SP2-WINDOW-DEF
           MOVE    SP2-WD-NAME   TO CORRENTE
           MOVE SP2-WD-WINDOW-ID TO CORRENTE-ID
           IF CORRENTE-ID NOT = 0
              CALL SP2   USING SP2-DISPLAY-WINDOW SP2-NULL-PARM
           END-IF

           MOVE LOW-VALUES      TO SP2-WD-DATA
           MOVE "t"             TO SP2-WD-BOR-TYPE *> Sem titulo e fixa
           MOVE "d"             TO SP2-WD-BOR-TYPE
           MOVE "CWBOXF"        TO SP2-WD-NAME
           MOVE X'02'           TO SP2-WD-OPTIONS-3
           DISPLAY 'CWBOXF-WD-OPTIONS-3' UPON ENVIRONMENT-NAME
           ACCEPT SP2-WD-OPTIONS-3  FROM ENVIRONMENT-VALUE
           COMPUTE SP2-WD-WIDTH  = CWBOXF-HORIZONTAL-LENGTH
      *    COMPUTE SP2-WD-WIDTH  = COL-B + 2
           COMPUTE SP2-WD-WIDTH  = COL-B + 3
           IF (CWBOXS
           AND  CWBOXF-PROGRAM = "CWREAD")
               SUBTRACT 3 FROM SP2-WD-WIDTH
           END-IF
           PERFORM UNTIL SP2-WD-WIDTH NOT < 15
                   ADD 1 TO SP2-WD-WIDTH
                            COL-B
                   IF   CWBOXF-STRING-2-LENGTH = 0
                        ADD WD TO WID-1
                   ELSE
                        ADD WD TO WID-2
                   END-IF
           END-PERFORM
200516     subtract 1 from SP2-WD-WIDTH
           COMPUTE SP2-WD-HEIGHT = CWBOXF-VERTICAL-LENGTH - 1
           COMPUTE SP2-WD-ROW    = (CWBOXF-LINE   - 1) * 7
           COMPUTE SP2-WD-COL    = (CWBOXF-COLUMN - 1) * 5
           MOVE -1              TO SP2-WD-TITLE-ROWS
           MOVE -1              TO SP2-WD-MENU-ROWS
           IF   CWBOXF-TITLE NOT = SPACE
                MOVE CWBOXF-TITLE              TO TITULO
                INSPECT TITULO CONVERTING "_"  TO SPACE
      *         INSPECT TITULO CONVERTING ACENTOS-850 TO ACENTOS-WINDOWS
                IF TITULO(1: 1) = X"B5"
                   MOVE TITULO(2:)             TO SP2-WD-TITLE
                ELSE
                    MOVE TITULO                TO SP2-WD-TITLE
                END-IF
      *         INSPECT SP2-WD-TITLE CONVERTING X"FFC6" TO "  "
                INSPECT SP2-WD-TITLE CONVERTING X"FF" TO " "
                MOVE SPACES                    TO TITULO
                PERFORM VARYING I FROM LENGTH OF TITULO BY -1
                        UNTIL SP2-WD-TITLE (I: 1) NOT = SPACES
                        CONTINUE
                END-PERFORM
                PERFORM VARYING Y FROM 1 BY 1
                          UNTIL Y = I OR SP2-WD-TITLE = SPACES
                        IF  SP2-WD-TITLE (Y: 1) = SPACE
                        AND SP2-WD-TITLE (Y + 1: 1) = SPACE
                            MOVE SP2-WD-TITLE TO TITULO
                            MOVE SPACES       TO SP2-WD-TITLE
                        END-IF
                END-PERFORM
                IF   TITULO NOT = SPACE
                     ADD 1 TO SP2-WD-HEIGHT
                END-IF
           END-IF
      *    MOVE -2               TO SP2-WD-OWNR-ID
           IF CORRENTE = 'CWMENU'
              MOVE CORRENTE-ID   TO SP2-WD-OWNR-ID
           END-IF
           MOVE X"10"            TO SP2-WD-MORE-OPTIONS
      *    MOVE X"11"            TO SP2-WD-MORE-OPTIONS
           MOVE 8                TO SP2-WD-CELL-WIDTH
           MOVE 16               TO SP2-WD-CELL-HEIGHT
           INSPECT SP2-WD-TITLE CONVERTING X"CDC4" TO X"2020"
           IF   CWLITS = "LOW"
                INSPECT SP2-WD-TITLE CONVERTING MAIUSCULAS TO MINUSCULAS
           END-IF
           IF   CWLITS = "UPP"
                INSPECT SP2-WD-TITLE CONVERTING MINUSCULAS TO MAIUSCULAS
           END-IF
           IF   CWACCENT = "OFF"
                INSPECT SP2-WD-TITLE CONVERTING ACENTOS-850
                                             TO ACENTOS-OFF
           ELSE
                INSPECT SP2-WD-TITLE CONVERTING ACENTOS-850
                                             TO ACENTOS-WINDOWS
           END-IF

           DISPLAY "CWINDF" UPON ENVIRONMENT-NAME
           ACCEPT   CWINDF  FROM ENVIRONMENT-VALUE
           IF CWINDF NOT = SPACES
              CALL CWINDF USING SP2-WINDOW-DEF
                ON EXCEPTION
                   CONTINUE
              END-CALL
           END-IF
           CALL SP2           USING SP2-OPEN-WINDOW SP2-WINDOW-DEF

           INITIALIZE SP2-PD-CTRL-KEYS
           MOVE LOW-VALUES        TO SP2-PD-DATA
           MOVE SP2-WD-WIDTH      TO SP2-PD-WIDTH
           MOVE SP2-WD-HEIGHT     TO SP2-PD-HEIGHT
           MOVE 0                 TO SP2-PD-ROW
           MOVE 0                 TO SP2-PD-COL
           MOVE "CWBOXF"          TO SP2-PD-NAME
           MOVE X'20'             TO SP2-PD-OPTIONS-3
           DISPLAY 'CWBOXF-PD-OPTIONS-3' UPON ENVIRONMENT-NAME
           ACCEPT  SP2-PD-OPTIONS-3      FROM ENVIRONMENT-VALUE
           MOVE SP2-KEY-HOME      TO SP2-PD-CTRL-KEY (01)
           MOVE SP2-KEY-END       TO SP2-PD-CTRL-KEY (02)
           MOVE SP2-KEY-DOWN      TO SP2-PD-CTRL-KEY (03)
           MOVE SP2-KEY-UP        TO SP2-PD-CTRL-KEY (04)
           MOVE SP2-KEY-CTRL-PGUP TO SP2-PD-CTRL-KEY (05)
           MOVE SP2-KEY-CTRL-PGDN TO SP2-PD-CTRL-KEY (06)
           MOVE SP2-KEY-PGUP      TO SP2-PD-CTRL-KEY (07)
           MOVE SP2-KEY-PGDN      TO SP2-PD-CTRL-KEY (08)
           MOVE SP2-KEY-TAB       TO SP2-PD-CTRL-KEY (09)
           MOVE SP2-KEY-BACKTAB   TO SP2-PD-CTRL-KEY (10)
           MOVE SP2-KEY-ENTER     TO SP2-PD-CTRL-KEY (11)
           MOVE SP2-KEY-ESC       TO SP2-PD-CTRL-KEY (12)
           MOVE SP2-KEY-TIMEOUT   TO SP2-PD-CTRL-KEY (13)
           MOVE SP2-KEY-RIGHT     TO SP2-PD-CTRL-KEY (14)
           MOVE SP2-KEY-BACKSPAC  TO SP2-PD-CTRL-KEY (15)
           MOVE SP2-KEY-DELETE    TO SP2-PD-CTRL-KEY (16)
           MOVE SP2-KEY-INSERT    TO SP2-PD-CTRL-KEY (17)
           MOVE SP2-KEY-CTRL-HOME TO SP2-PD-CTRL-KEY (18)
           MOVE SP2-KEY-CTRL-END  TO SP2-PD-CTRL-KEY (19)
           MOVE SP2-KEY-LEFT      TO SP2-PD-CTRL-KEY (20)
           MOVE 20                TO I
           IF   CWBOXF-KEY-ON = "Y" OR "y"
                             OR "S" OR "s" OR "*" OR "^"
                MOVE SP2-KEY-F1   TO SP2-PD-CTRL-KEY (21)
                MOVE SP2-KEY-F2   TO SP2-PD-CTRL-KEY (22)
                MOVE SP2-KEY-F3   TO SP2-PD-CTRL-KEY (23)
                MOVE SP2-KEY-F4   TO SP2-PD-CTRL-KEY (24)
                MOVE SP2-KEY-F5   TO SP2-PD-CTRL-KEY (25)
                MOVE SP2-KEY-F6   TO SP2-PD-CTRL-KEY (26)
                MOVE SP2-KEY-F7   TO SP2-PD-CTRL-KEY (27)
                MOVE SP2-KEY-F8   TO SP2-PD-CTRL-KEY (28)
                MOVE SP2-KEY-F9   TO SP2-PD-CTRL-KEY (29)
                MOVE SP2-KEY-F10  TO SP2-PD-CTRL-KEY (30)
                MOVE 30           TO I
                IF   CWBOXF-KEY-ON = "*" OR "^"
                     MOVE SP2-KEY-F11     TO SP2-PD-CTRL-KEY (31)
                     MOVE SP2-KEY-F12     TO SP2-PD-CTRL-KEY (32)
                     MOVE SP2-KEY-CTRL-F1 TO SP2-PD-CTRL-KEY (33)
                     MOVE SP2-KEY-CTRL-F2 TO SP2-PD-CTRL-KEY (34)
                     MOVE SP2-KEY-CTRL-A  TO SP2-PD-CTRL-KEY (35)
                     MOVE SP2-KEY-CTRL-B  TO SP2-PD-CTRL-KEY (36)
                     MOVE SP2-KEY-CTRL-C  TO SP2-PD-CTRL-KEY (37)
                     MOVE SP2-KEY-CTRL-D  TO SP2-PD-CTRL-KEY (38)
                     MOVE SP2-KEY-CTRL-E  TO SP2-PD-CTRL-KEY (39)
                     MOVE SP2-KEY-CTRL-F  TO SP2-PD-CTRL-KEY (40)
                     MOVE SP2-KEY-CTRL-G  TO SP2-PD-CTRL-KEY (41)
                     MOVE SP2-KEY-CTRL-J  TO SP2-PD-CTRL-KEY (42)
                     MOVE SP2-KEY-CTRL-K  TO SP2-PD-CTRL-KEY (43)
                     MOVE SP2-KEY-CTRL-L  TO SP2-PD-CTRL-KEY (44)
                     MOVE SP2-KEY-CTRL-N  TO SP2-PD-CTRL-KEY (45)
                     MOVE SP2-KEY-CTRL-O  TO SP2-PD-CTRL-KEY (46)
                     MOVE SP2-KEY-CTRL-P  TO SP2-PD-CTRL-KEY (47)
                     MOVE SP2-KEY-CTRL-Q  TO SP2-PD-CTRL-KEY (48)
                     MOVE SP2-KEY-CTRL-R  TO SP2-PD-CTRL-KEY (49)
                     MOVE SP2-KEY-CTRL-S  TO SP2-PD-CTRL-KEY (50)
                     MOVE SP2-KEY-CTRL-T  TO SP2-PD-CTRL-KEY (51)
                     MOVE SP2-KEY-CTRL-U  TO SP2-PD-CTRL-KEY (52)
                     MOVE SP2-KEY-CTRL-V  TO SP2-PD-CTRL-KEY (53)
                     MOVE SP2-KEY-CTRL-W  TO SP2-PD-CTRL-KEY (54)
                     MOVE SP2-KEY-CTRL-X  TO SP2-PD-CTRL-KEY (55)
                     MOVE SP2-KEY-CTRL-Y  TO SP2-PD-CTRL-KEY (56)
                     MOVE SP2-KEY-CTRL-Z  TO SP2-PD-CTRL-KEY (57)
                     MOVE SP2-KEY-ALT-A   TO SP2-PD-CTRL-KEY (58)
                     MOVE SP2-KEY-ALT-B   TO SP2-PD-CTRL-KEY (59)
                     MOVE SP2-KEY-ALT-C   TO SP2-PD-CTRL-KEY (60)
                     MOVE SP2-KEY-ALT-D   TO SP2-PD-CTRL-KEY (61)
                     MOVE SP2-KEY-ALT-E   TO SP2-PD-CTRL-KEY (62)
                     MOVE SP2-KEY-ALT-F   TO SP2-PD-CTRL-KEY (63)
                     MOVE SP2-KEY-ALT-G   TO SP2-PD-CTRL-KEY (64)
                     MOVE SP2-KEY-ALT-H   TO SP2-PD-CTRL-KEY (65)
                     MOVE SP2-KEY-ALT-I   TO SP2-PD-CTRL-KEY (66)
                     MOVE SP2-KEY-ALT-J   TO SP2-PD-CTRL-KEY (67)
                     MOVE SP2-KEY-ALT-K   TO SP2-PD-CTRL-KEY (68)
                     MOVE SP2-KEY-ALT-L   TO SP2-PD-CTRL-KEY (69)
                     MOVE SP2-KEY-ALT-M   TO SP2-PD-CTRL-KEY (70)
                     MOVE SP2-KEY-ALT-N   TO SP2-PD-CTRL-KEY (71)
                     MOVE SP2-KEY-ALT-O   TO SP2-PD-CTRL-KEY (72)
                     MOVE SP2-KEY-ALT-P   TO SP2-PD-CTRL-KEY (73)
                     MOVE SP2-KEY-ALT-Q   TO SP2-PD-CTRL-KEY (74)
                     MOVE SP2-KEY-ALT-R   TO SP2-PD-CTRL-KEY (75)
                     MOVE SP2-KEY-ALT-S   TO SP2-PD-CTRL-KEY (76)
                     MOVE SP2-KEY-ALT-T   TO SP2-PD-CTRL-KEY (77)
                     MOVE SP2-KEY-ALT-U   TO SP2-PD-CTRL-KEY (78)
                     MOVE SP2-KEY-ALT-V   TO SP2-PD-CTRL-KEY (79)
                     MOVE SP2-KEY-ALT-W   TO SP2-PD-CTRL-KEY (80)
                     MOVE SP2-KEY-ALT-X   TO SP2-PD-CTRL-KEY (81)
                     MOVE SP2-KEY-ALT-Y   TO SP2-PD-CTRL-KEY (82)
                     MOVE SP2-KEY-ALT-Z   TO SP2-PD-CTRL-KEY (83)
                     MOVE SP2-KEY-DELETE  TO SP2-PD-CTRL-KEY (84)
                     MOVE SP2-KEY-INSERT  TO SP2-PD-CTRL-KEY (85)
                     MOVE 0               TO SP2-PD-CTRL-KEY (16)
                                             SP2-PD-CTRL-KEY (17)
                     MOVE 85              TO I
                END-IF
           END-IF
           IF   CWBOXS
                MOVE SPACES       TO CWBOXS-CHARS
                DISPLAY "CWBOXS-CHARS" UPON ENVIRONMENT-NAME
                ACCEPT CWBOXS-CHARS FROM ENVIRONMENT-VALUE
                PERFORM 040-ATIVA-ALT THRU 040-99-FIM
                        VARYING D FROM 1 BY 1 UNTIL D > 21
           END-IF
           MOVE "n"               TO SP2-PD-CURS-SHOW
      *    move OK2             to sp2-PD-CUR-FLD-COLR
           CALL "CWRESE"
           CALL SP2            USING SP2-SET-PANEL-DEF SP2-PANEL-DEF.

           IF   TITULO = SPACES
                MOVE 0 TO TITLE-ID
           ELSE
                MOVE LOW-VALUES TO SP2-SD-DATA
                CALL "CWSPID" USING SP2-SD-ID "SInsert"
                MOVE SP2-SD-ID      TO TITLE-ID
                MOVE CWBOXF-HORIZONTAL-LENGTH
                                    TO SP2-SD-VAR-LEN
                                       SP2-SD-TEXT-LEN
                MOVE 10             TO SP2-SD-HEIGHT
                COMPUTE SP2-SD-WIDTH = (SP2-SD-TEXT-LEN + 1) * 10
                MOVE TITULO         TO SP2-SD-TEXT
                MOVE STATIC         TO SP2-SD-COLR
                MOVE 2              TO SP2-SD-FONT-ID
                MOVE 1              TO SP2-SD-FONT-ID
                MOVE -1             TO SP2-SD-FONT-ID
                CALL SP2   USING SP2-SET-STATIC-DEF SP2-STATIC-DEF
           END-IF.

       FIM-OPEN-PANEL. EXIT.

       FIND-CURSOR.

           IF  CWBOXS
           AND (FIELD-AREA (OFF-X: 1) NOT = SPACE)
           AND (CWBOXF-PROGRAM = "CWREAD")
               MOVE FIELD-AREA (OFF-X: 1) TO CWBOXF-WORK-AREA (3: 1)
               MOVE SPACES TO CWBOXF-STRING-1
                              CWBOXF-STRING-2
               CALL CWBOXF-PROGRAM USING "R"
                                         CWBOXF-ORDER
                                         CWBOXF-STRING-1
                                         CWBOXF-STRING-2
                                         CWBOXF-VERTICAL-LENGTH
                                         CWBOXF-WORK-AREA
               IF CWBOXF-STRING-1 NOT = SPACES
                  MOVE SP2-KEY-ENTER TO SP2-CD-KEY
                  MOVE 9             TO EDIT-OK
                  MOVE X"01"         TO OK
                  IF CWBOXF-WORK-AREA (1: 2) = LOW-VALUES
                  AND CWBOXS
                  AND CWBOXF-STRING-1 (1: 79) = SPACES
                     SET CWBOXS-NO TO TRUE
                  END-IF
               END-IF
               GO TO FIM-FIND-CURSOR
           END-IF

           IF   CWBOXF-OPTION = SPACES
                IF   CWACCENT = "OFF"
                     INSPECT FIELD-AREA (OFF-X: 1)
                             CONVERTING ACENTOS-WINDOWS
                                      TO ACENTOS-OFF
                ELSE
                     INSPECT FIELD-AREA (OFF-X: 1)
                             CONVERTING ACENTOS-WINDOWS
                                     TO ACENTOS-850
                END-IF
                IF  CWBOXF-ORDER < 2
                    MOVE FIELD-AREA (OFF-X: CWBOXF-STRING-1-LENGTH)
                      TO CWBOXF-STRING-1
                    MOVE SPACES TO CWBOXF-STRING-1 (KURSOR: )
                    MOVE LISTA  (M) TO SP2-CD-NEXT-FLD-NUM
                    MOVE SPACES TO CWBOXF-STRING-2
                ELSE
                    MOVE FIELD-AREA (OFF-X: CWBOXF-STRING-2-LENGTH)
                      TO CWBOXF-STRING-2
                    MOVE SPACES TO CWBOXF-STRING-2 (KURSOR: )
                    MOVE LISTA2 (M) TO SP2-CD-NEXT-FLD-NUM
                    MOVE SPACES TO CWBOXF-STRING-1
                END-IF
                IF  KURSOR > SZ
                    MOVE 1 TO KURSOR
                END-IF
           ELSE
                IF  CWBOXF-ORDER < 2
                    MOVE CWBOXF-OPTION TO CWBOXF-STRING-1
                    MOVE SPACES TO CWBOXF-STRING-2
                ELSE
                    MOVE CWBOXF-OPTION   TO CWBOXF-STRING-2
                    MOVE SPACES TO CWBOXF-STRING-1
                END-IF
           END-IF

           MOVE X"01" TO OK
           PERFORM CURSOR-BARRA

           MOVE "N" TO FIM
           IF  KURSOR > 0
               COMPUTE T = KURSOR - 1
               IF  CWBOXF-ORDER < 2
                   move CWBOXF-STRING-1 to texto-a
               else
                   move CWBOXF-STRING-2 to texto-a
               end-if
               inspect texto-a converting low-values to spaces
           END-IF
           IF    (CWBOXF-ORDER = 1
              AND CWBOXF-STRING-1-LENGTH NOT = 0)
               OR CWBOXF-ORDER > 1
              AND(CWBOXF-STRING-2-LENGTH NOT = 0)
                  IF CWBOXF-ORDER = 1
                     if kursor > 0
                     AND LOADED = 1
                        move texto-a to TEXTO-1 (1)
                     end-if
                     move 0               to LOADED
                     MOVE TEXTO-1 (1)     TO T-PRE
                                             TEXTO-A TEXTO-B
                     COMPUTE C-1 = C + 1
                  ELSE
                     if kursor > 0
                     AND LOADED = 1
                        move texto-a to TEXTO-2 (1)
                     end-if
                     move 0               to LOADED
                     MOVE TEXTO-2 (1)     TO T-PRE
                                             TEXTO-A TEXTO-B
                     COMPUTE C-1 = I + C - 1
                  END-IF
                  MOVE SP2-CD-CTRL-FIELD-KEY (1: 1) TO CARACTER
                  IF   CARACTER = X"08"
                       IF   T > 2
                            SUBTRACT 1 FROM T
                            MOVE SPACE TO TEXTO-A (T: 1) CARACTER
                            SUBTRACT 1 FROM T
                       END-IF
                  ELSE
                       MOVE CARACTER TO TEXTO-A (T: 1)
                  END-IF
                  IF  CWFIND = 'OFF'
                      MOVE TEXTO-A TO TEXTO-B
                  END-IF
                  IF  TEXTO-A NOT = TEXTO-B
                      MOVE "N"            TO FIM
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
                                 SUBTRACT 1 FROM KURSOR
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
           ELSE


           SET NOT-LESS TO TRUE
           IF NOT AT-END
              PERFORM 250-LER THRU 250-99-FIM
              SET READ-NEXT TO TRUE
           ELSE
              MOVE "<" TO FIM
           END-IF

           PERFORM 210-MONTA-TELA THRU 210-99-FIM
           END-IF
           MOVE OK2 TO OK
           PERFORM CURSOR-BARRA
           CALL SP2   USING SP2-DISPLAY-WINDOW SP2-NULL-PARM.

       FIM-FIND-CURSOR. EXIT.

       100-CONVERTE-TECLA.

           IF SP2-CD-KEY = SP2-KEY-CLOSE
           OR SP2-KEY-HOME
           OR SP2-KEY-END
           OR SP2-KEY-DOWN
           OR SP2-KEY-TAB
           OR SP2-KEY-UP
           OR SP2-KEY-BACKTAB
           OR SP2-KEY-CTRL-PGUP
           OR SP2-KEY-CTRL-PGDN
           OR SP2-KEY-PGUP
           OR SP2-KEY-PGDN
           OR SP2-KEY-ESC
              GO TO 100-99-FIM
           END-IF
           COPY CWSPKY.
           MOVE 0 TO EDIT-OK
           PERFORM VARYING K FROM 27 BY 1 UNTIL K > 89
                   IF   SP2-CD-KEY = SP2-PD-CTRL-KEY (K)
                        MOVE 1 TO EDIT-OK
                        MOVE 90 TO K
                   ELSE
                        IF   SP2-PD-CTRL-KEY (K) = 0
                             MOVE 90 TO K
                        END-IF
                   END-IF
           END-PERFORM
           IF  EDIT-OK = 0
               GO TO 100-99-FIM
           END-IF
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
                IF  CWBOXF-KEY-ON = "*" OR "^"
                    MOVE TECLA     TO TECLA2
                    IF (CWBOXF-KEY-ON = "*" OR "^")
                        MOVE TECLA     TO CWBOXF-EDIT
                        SET EDIT-ENTER TO TRUE
                        MOVE SP2-KEY-ENTER TO SP2-CD-KEY
                    END-IF
                    EVALUATE TRUE
                        WHEN CWBOXF-RETURN = 1
                             MOVE TEXTO-1 (M)
                               TO CWBOXF-OPTION
                        WHEN CWBOXF-RETURN = 2
                             MOVE TEXTO-2 (M)
                               TO CWBOXF-OPTION
                    END-EVALUATE
                    MOVE CORRENTE TO SP2-ND-NAME
                    CALL SP2   USING SP2-ACTIVATE-INTERNAL SP2-NAME-DEF
                    CALL CWBOXF-PROGRAM USING "$"
                         CWBOXF-ORDER
                         CWBOXF-STRING-1
                         CWBOXF-STRING-2
                         CWBOXF-VERTICAL-LENGTH
                         CWBOXF-WORK-AREA
                         TECLA2
                         CWBOXF-OPTION
                    MOVE "CWBOXF" TO SP2-ND-NAME
                    CALL SP2   USING SP2-ACTIVATE-INTERNAL SP2-NAME-DEF
                    IF  CWBOXF-KEY-ON = "^"
                        MOVE SPACES TO CWBOXF-OPTION
                    END-IF
                END-IF
           END-IF

           CALL "CWAKEY" USING TECLA2 MIL.

       100-99-FIM. EXIT.
       030-ALT.

           MOVE SPACE TO LETRA
           EVALUATE SP2-CD-KEY
               WHEN SP2-KEY-ALT-0           MOVE "0" TO LETRA
               WHEN SP2-KEY-ALT-1           MOVE "1" TO LETRA
               WHEN SP2-KEY-ALT-2           MOVE "2" TO LETRA
               WHEN SP2-KEY-ALT-3           MOVE "3" TO LETRA
               WHEN SP2-KEY-ALT-4           MOVE "4" TO LETRA
               WHEN SP2-KEY-ALT-5           MOVE "5" TO LETRA
               WHEN SP2-KEY-ALT-6           MOVE "6" TO LETRA
               WHEN SP2-KEY-ALT-7           MOVE "7" TO LETRA
               WHEN SP2-KEY-ALT-8           MOVE "8" TO LETRA
               WHEN SP2-KEY-ALT-9           MOVE "9" TO LETRA
               WHEN SP2-KEY-ALT-A           MOVE "A" TO LETRA
               WHEN SP2-KEY-ALT-B           MOVE "B" TO LETRA
               WHEN SP2-KEY-ALT-C           MOVE "C" TO LETRA
               WHEN SP2-KEY-ALT-D           MOVE "D" TO LETRA
               WHEN SP2-KEY-ALT-E           MOVE "E" TO LETRA
               WHEN SP2-KEY-ALT-EQUAL       MOVE "=" TO LETRA
               WHEN SP2-KEY-ALT-F           MOVE "F" TO LETRA
               WHEN SP2-KEY-ALT-G           MOVE "G" TO LETRA
               WHEN SP2-KEY-ALT-H           MOVE "H" TO LETRA
               WHEN SP2-KEY-ALT-I           MOVE "I" TO LETRA
               WHEN SP2-KEY-ALT-J           MOVE "J" TO LETRA
               WHEN SP2-KEY-ALT-K           MOVE "K" TO LETRA
               WHEN SP2-KEY-ALT-L           MOVE "L" TO LETRA
               WHEN SP2-KEY-ALT-M           MOVE "M" TO LETRA
               WHEN SP2-KEY-ALT-MINUS       MOVE "-" TO LETRA
               WHEN SP2-KEY-ALT-N           MOVE "N" TO LETRA
               WHEN SP2-KEY-ALT-O           MOVE "O" TO LETRA
               WHEN SP2-KEY-ALT-P           MOVE "P" TO LETRA
               WHEN SP2-KEY-ALT-Q           MOVE "Q" TO LETRA
               WHEN SP2-KEY-ALT-R           MOVE "R" TO LETRA
               WHEN SP2-KEY-ALT-S           MOVE "S" TO LETRA
               WHEN SP2-KEY-ALT-T           MOVE "T" TO LETRA
               WHEN SP2-KEY-ALT-U           MOVE "U" TO LETRA
               WHEN SP2-KEY-ALT-V           MOVE "V" TO LETRA
               WHEN SP2-KEY-ALT-W           MOVE "W" TO LETRA
               WHEN SP2-KEY-ALT-X           MOVE "X" TO LETRA
               WHEN SP2-KEY-ALT-Y           MOVE "Y" TO LETRA
               WHEN SP2-KEY-ALT-Z           MOVE "Z" TO LETRA
           END-EVALUATE

           IF LETRA = SPACE
           AND SP2-CD-CTRL-FIELD-KEY (2:1) = X'00'
           AND (SP2-CD-CTRL-FIELD-KEY (1:1) NOT = X'00')
               MOVE SP2-CD-CTRL-FIELD-KEY (1:1) TO LETRA
               INSPECT LETRA CONVERTING MINUSCULAS TO MAIUSCULAS
           END-IF

           IF LETRA NOT = SPACE
              PERFORM VARYING I FROM 1 BY 1 UNTIL I > 21
                      IF LETRA = CHAR (I)
                         MOVE I          TO M
                         MOVE 99         TO Mx
                         MOVE LETRA      TO FIELD-AREA (OFF-W: 1)
                                            SAVE-A
                         MOVE LISTA2 (I) TO SP2-CD-NEXT-FLD-ID
                         MOVE SP2-KEY-CTRL-FIELD TO SP2-CD-KEY
                         EXIT PERFORM
                      END-IF
              END-PERFORM
           ELSE
              IF SP2-CD-KEY = SP2-KEY-CTRL-FIELD OR SP2-KEY-MOUSE
                 MOVE SPACE TO SAVE-A
                 PERFORM VARYING I FROM 1 BY 1 UNTIL I > 21
                         IF LISTA2 (I) = SP2-CD-NEXT-FLD-ID
                            MOVE I TO Mx
231116                                m
                            EXIT PERFORM
                         END-IF
                 END-PERFORM
              END-IF
           END-IF.

       030-99-FIM. EXIT.

       040-ATIVA-ALT.

           ADD 1  TO I
           MOVE 0 TO SP2-PD-CTRL-KEY (I)
           EVALUATE CHAR (D)
               WHEN "0" MOVE  SP2-KEY-ALT-0     TO SP2-PD-CTRL-KEY (I)
               WHEN "1" MOVE  SP2-KEY-ALT-1     TO SP2-PD-CTRL-KEY (I)
               WHEN "2" MOVE  SP2-KEY-ALT-2     TO SP2-PD-CTRL-KEY (I)
               WHEN "3" MOVE  SP2-KEY-ALT-3     TO SP2-PD-CTRL-KEY (I)
               WHEN "4" MOVE  SP2-KEY-ALT-4     TO SP2-PD-CTRL-KEY (I)
               WHEN "5" MOVE  SP2-KEY-ALT-5     TO SP2-PD-CTRL-KEY (I)
               WHEN "6" MOVE  SP2-KEY-ALT-6     TO SP2-PD-CTRL-KEY (I)
               WHEN "7" MOVE  SP2-KEY-ALT-7     TO SP2-PD-CTRL-KEY (I)
               WHEN "8" MOVE  SP2-KEY-ALT-8     TO SP2-PD-CTRL-KEY (I)
               WHEN "9" MOVE  SP2-KEY-ALT-9     TO SP2-PD-CTRL-KEY (I)
               WHEN "A" MOVE  SP2-KEY-ALT-A     TO SP2-PD-CTRL-KEY (I)
               WHEN "B" MOVE  SP2-KEY-ALT-B     TO SP2-PD-CTRL-KEY (I)
               WHEN "C" MOVE  SP2-KEY-ALT-C     TO SP2-PD-CTRL-KEY (I)
               WHEN "D" MOVE  SP2-KEY-ALT-D     TO SP2-PD-CTRL-KEY (I)
               WHEN "E" MOVE  SP2-KEY-ALT-E     TO SP2-PD-CTRL-KEY (I)
               WHEN "=" MOVE  SP2-KEY-ALT-EQUAL TO SP2-PD-CTRL-KEY (I)
               WHEN "F" MOVE  SP2-KEY-ALT-F     TO SP2-PD-CTRL-KEY (I)
               WHEN "G" MOVE  SP2-KEY-ALT-G     TO SP2-PD-CTRL-KEY (I)
               WHEN "H" MOVE  SP2-KEY-ALT-H     TO SP2-PD-CTRL-KEY (I)
               WHEN "I" MOVE  SP2-KEY-ALT-I     TO SP2-PD-CTRL-KEY (I)
               WHEN "J" MOVE  SP2-KEY-ALT-J     TO SP2-PD-CTRL-KEY (I)
               WHEN "K" MOVE  SP2-KEY-ALT-K     TO SP2-PD-CTRL-KEY (I)
               WHEN "L" MOVE  SP2-KEY-ALT-L     TO SP2-PD-CTRL-KEY (I)
               WHEN "M" MOVE  SP2-KEY-ALT-M     TO SP2-PD-CTRL-KEY (I)
               WHEN "-" MOVE  SP2-KEY-ALT-MINUS TO SP2-PD-CTRL-KEY (I)
               WHEN "N" MOVE  SP2-KEY-ALT-N     TO SP2-PD-CTRL-KEY (I)
               WHEN "O" MOVE  SP2-KEY-ALT-O     TO SP2-PD-CTRL-KEY (I)
               WHEN "P" MOVE  SP2-KEY-ALT-P     TO SP2-PD-CTRL-KEY (I)
               WHEN "Q" MOVE  SP2-KEY-ALT-Q     TO SP2-PD-CTRL-KEY (I)
               WHEN "R" MOVE  SP2-KEY-ALT-R     TO SP2-PD-CTRL-KEY (I)
               WHEN "S" MOVE  SP2-KEY-ALT-S     TO SP2-PD-CTRL-KEY (I)
               WHEN "T" MOVE  SP2-KEY-ALT-T     TO SP2-PD-CTRL-KEY (I)
               WHEN "U" MOVE  SP2-KEY-ALT-U     TO SP2-PD-CTRL-KEY (I)
               WHEN "V" MOVE  SP2-KEY-ALT-V     TO SP2-PD-CTRL-KEY (I)
               WHEN "W" MOVE  SP2-KEY-ALT-W     TO SP2-PD-CTRL-KEY (I)
               WHEN "X" MOVE  SP2-KEY-ALT-X     TO SP2-PD-CTRL-KEY (I)
               WHEN "Y" MOVE  SP2-KEY-ALT-Y     TO SP2-PD-CTRL-KEY (I)
               WHEN "Z" MOVE  SP2-KEY-ALT-Z     TO SP2-PD-CTRL-KEY (I)
           END-EVALUATE

           IF SP2-PD-CTRL-KEY (I) = 0
              SUBTRACT 1 FROM I
           ELSE
              PERFORM VARYING II FROM 1 BY 1 UNTIL II > I
                      IF SP2-PD-CTRL-KEY (I) = SP2-PD-CTRL-KEY (II)
                         SUBTRACT 1 FROM I
                         EXIT PERFORM
                      END-IF
              END-PERFORM
              ADD 1 TO ALTS
           END-IF.

       040-99-FIM. EXIT.

       AJUSTA-ACENTOS.

           IF SP2-SD-FONT-ID NOT = 0
              MOVE SP2-SD-FONT-ID TO SP2-FO-ID
              MOVE SPACES TO SP2-FO-NAME
              CALL SP2   USING SP2-GET-FONT-DEF SP2-FONT-DEF
           ELSE
              MOVE SPACES TO SP2-FO-NAME
           END-IF
           IF   CWLITS = "UPP"
                INSPECT SP2-SD-TEXT (1: SP2-SD-TEXT-LEN)
                        CONVERTING MINUSCULAS
                                TO MAIUSCULAS
           END-IF
           IF   CWLITS = "LOW"
                INSPECT SP2-SD-TEXT (1: SP2-SD-TEXT-LEN)
                        CONVERTING MAIUSCULAS
                                TO MINUSCULAS
           END-IF
           IF   CWACCENT = "OFF"
                INSPECT SP2-SD-TEXT (1: SP2-SD-TEXT-LEN)
                        CONVERTING ACENTOS-850
                                TO ACENTOS-OFF
           ELSE
                IF   SP2-FO-NAME (1:2) = "MF"
                     INSPECT SP2-SD-TEXT (1: SP2-SD-TEXT-LEN)
                             CONVERTING ACENTOS-850
                                     TO ACENTOS-437
                ELSE
                     INSPECT SP2-SD-TEXT (1: SP2-SD-TEXT-LEN)
                          CONVERTING ACENTOS-850
                                  TO ACENTOS-WINDOWS
                END-IF
           END-IF.

       FIM-AJUSTA-ACENTOS. EXIT.

       COPY SLB\TXT\CWBOXF.CBL REPLACING ==CWBOXF== BY ==TXBOXF==
                  ==CWGETL-MOUSE = 1== BY ==CWGETL-MOUSE = 9==.
       END PROGRAM CWBOXF.

