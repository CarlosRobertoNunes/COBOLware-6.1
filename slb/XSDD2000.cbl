       IDENTIFICATION DIVISION.
       PROGRAM-ID.    XSDD2000.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  09/09/2017.
       SECURITY.      *************************************************
                      *                                               *
                      * Suporte a programas gerados pelo XSEED        *
                      * Subrotina de processamento de datas           *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1. COPY CWCASE.
           05 CC                  PIC  9(002) VALUE 20.
           05 MMM                 PIC  X(003) VALUE SPACES.
           05 CWDATE              PIC  9(008) VALUE ZEROS.
           05 DATA-TESTE          PIC  X(006) VALUE SPACES.
           05 HOJE.
              10 YY               PIC  9(002) VALUE ZEROS.
              10 MM               PIC  9(002) VALUE ZEROS.
              10 DD               PIC  9(002) VALUE ZEROS.
           05 REDEFINES HOJE.
              10 YY-X             PIC  X(002).
              10 MM-X             PIC  X(002).
              10 DD-X             PIC  X(002).
           05 HOJE-2000.
              10 CC-2000          PIC  9(002) VALUE ZEROS.
              10 YY-2000          PIC  9(002) VALUE ZEROS.
              10 MM-2000          PIC  9(002) VALUE ZEROS.
              10 DD-2000          PIC  9(002) VALUE ZEROS.
           05 REDEFINES HOJE-2000.
              10 CC-X-2000        PIC  X(002).
              10 YY-X-2000        PIC  X(002).
              10 MM-X-2000        PIC  X(002).
              10 DD-X-2000        PIC  X(002).
           05 VALUE "312831303130313130313031".
              10 ULTIMO-DIA          PIC  9(002) OCCURS 12.
           05 ANO-BI          COMP-3 PIC  9(002) VALUE ZERO.
           05 DIAS            COMP-3 PIC S9(010) VALUE ZERO.
           05 DIAS-2          COMP-3 PIC S9(010) VALUE ZERO.
           05 DDD                    PIC  9(003) VALUE ZERO.
           05 I               COMP-3 PIC S9(009) VALUE ZERO.
           05 RESTO           COMP-3 PIC  9(004)V9(6).
           05 W-DIA                  PIC S9(009) VALUE ZERO.
           05 M-DIA                  PIC  9(009) VALUE ZERO.
           05 TO-DATE                PIC  9(001) VALUE ZERO.
           05 LIXO                   PIC  9(002) VALUE ZERO.
           05 TABELA-DE-MESES.
              10                     PIC  X(010) VALUE "JANEIRO".
              10                     PIC  X(010) VALUE "FEVEREIRO".
              10                     PIC  X(010) VALUE "MARCO".
              10                     PIC  X(010) VALUE "ABRIL".
              10                     PIC  X(010) VALUE "MAIO".
              10                     PIC  X(010) VALUE "JUNHO".
              10                     PIC  X(010) VALUE "JULHO".
              10                     PIC  X(010) VALUE "AGOSTO".
              10                     PIC  X(010) VALUE "SETEMBRO".
              10                     PIC  X(010) VALUE "OUTUBRO".
              10                     PIC  X(010) VALUE "NOVEMBRO".
              10                     PIC  X(010) VALUE "DEZEMBRO".
           05 REDEFINES TABELA-DE-MESES.
              10 NOME-MES            PIC  X(010) OCCURS 12.
           05 SAVE-DCTYPE            PIC  X(002) VALUE SPACES.
           05 SAVE-DCEDIT            PIC  X(001) VALUE SPACE.

       01  WS-CURRENT-DATE-FIELDS.
           05  WS-CURRENT-DATE.
               10 WS-CURRENT-YEAR    PIC  9(004).
               10 WS-CURRENT-MONTH   PIC  9(002).
               10 WS-CURRENT-DATE    PIC  9(002).
           05  WS-CURRENT-TIME.
               10 WS-CURRENT-HOUR    PIC  9(002).
               10 WS-CURRENT-MINUTE  PIC  9(002).
               10 WS-CURRENT-SECOND  PIC  9(002).
               10 WS-CURRENT-MS      PIC  9(002).
               10 WS-GMT-SIGN        PIC  X(001).
               10 WS-GMT-TIME        PIC  X(004).

       LINKAGE SECTION.

       01  GLB-DCITENS.
           05 GLB-BASE               PIC  9(004).
           05 GLB-CENTURY            PIC  9(002).
           05 GLB-DCFORMAT           PIC  X(012).
              88 DATA-NUMERICA VALUE "DDMMYY" "DDYYMM"
                                     "MMDDYY" "MMYYDD"
                                     "YYDDMM" "YYMMDD".
              88 DATA-2000     VALUE "DDMMCCYY" "DDCCYYMM"
                                     "MMDDCCYY" "MMCCYYDD"
                                     "CCYYDDMM" "CCYYMMDD".
           05 GLB-DCTYPE             PIC  X(002).
           05 GLB-DCEDIT             PIC  X(001).
           05 glb-dclength           pic  9(001).
           05 GLB-DCSINAL            PIC  X(001).
           05 GLB-DCOPERAND          PIC S9(009).
           05 GLB-DCDATE             PIC  X(025).
           05 GLB-DCYEAR             PIC  9(004).
           05 GLB-DC-DAYNUM          PIC S9(009).
           05 GLB-DC-DD-MM-YY        PIC  X(008).
           05 GLB-DC-DD-MMM-YY       PIC  X(009).
           05 GLB-DC-DDMMMYY         PIC  X(007).
           05 GLB-DC-DDMMYY          PIC  9(006).
           05 GLB-DC-MM-DD-YY        PIC  X(008).
           05 GLB-DC-MMDDYY          PIC  9(006).
           05 GLB-DC-MMM-DD-YY       PIC  X(009).
           05 GLB-DC-MMMDDYY         PIC  X(007).
           05 GLB-DC-UK-ALPHA        PIC  X(025).
           05 GLB-DC-US-ALPHA        PIC  X(025).
           05 GLB-DC-IN-ALPHA        PIC  X(025).
           05 GLB-DC-YYDDD           PIC  9(005).
           05 GLB-DC-YYMMDD          PIC  9(006).
           05 GLB-DC-YYMMMDD         PIC  X(007).
           05 GLB-DC-TODAY           PIC  X(015).
           05 GLB-DC-WEEKNO          PIC  9(002).
           05 glb-dc-dd-mm-ccyy      pic  x(010).
           05 glb-dc-dd-mmm-ccyy     pic  x(011).
           05 glb-dc-ddmmmccyy       pic  x(009).
           05 glb-dc-ddmmccyy        pic  9(008).
           05 glb-dc-mm-dd-ccyy      pic  x(010).
           05 glb-dc-mmddccyy        pic  9(008).
           05 glb-dc-mmm-dd-ccyy     pic  x(011).
           05 glb-dc-mmmddccyy       pic  x(009).
           05 glb-dc-ccyy-mm-dd      pic  x(010).
           05 glb-dc-ccyy-mmm-dd     pic  x(011).
           05 glb-dc-ccyyddd         pic  9(007).
           05 glb-dc-ccyymmdd        pic  9(008).
           05 glb-dc-ccyymmmdd       pic  x(009).
           05 glb-dceditonly         pic  x(001).

       01  GLB-TOTAL                 PIC S9(010)V99.
       01  GLB-STATUS                PIC  X(005).

       PROCEDURE DIVISION USING GLB-DCITENS GLB-TOTAL GLB-STATUS.

       000-INICIO.

           ON 1
              MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS
              MOVE WS-CURRENT-YEAR(1:2)  TO CC(1:2).
           INSPECT GLB-DCFORMAT CONVERTING MINUSCULAS TO MAIUSCULAS
           MOVE SPACES TO GLB-STATUS
           MOVE 0      TO TO-DATE

           IF   GLB-DCFORMAT = "TO-DATE"
                MOVE "DDMMYY"   TO GLB-DCFORMAT
                MOVE 1          TO TO-DATE
                MOVE GLB-BASE   TO GLB-DCYEAR YY
                MOVE GLB-TOTAL  TO DIAS
                MOVE 01         TO MM DD
                MOVE GLB-DCTYPE TO SAVE-DCTYPE
                MOVE GLB-DCEDIT TO SAVE-DCEDIT
                MOVE "UK"       TO GLB-DCTYPE
                MOVE "9"        TO GLB-DCEDIT
                PERFORM UNTIL DIAS = 0
                        DIVIDE YY BY 4 GIVING LIXO REMAINDER RESTO
                        IF   RESTO EQUAL ZERO
                             MOVE 29 TO ULTIMO-DIA (2)
                        ELSE
                             MOVE 28 TO ULTIMO-DIA (2)
                        END-IF
                        IF   DIAS > ULTIMO-DIA (MM)
                             SUBTRACT ULTIMO-DIA (MM) FROM DIAS
                             ADD      1                 TO MM
                             IF   MM = 13
                                  MOVE 1 TO MM
                                  ADD  1 TO YY
                             END-IF
                        ELSE
                             ADD DIAS TO DD
                             MOVE 0   TO DIAS
                             IF  DD > ULTIMO-DIA (MM)
                                 MOVE 1 TO DD
                                 ADD 1  TO MM
                                 IF  MM = 13
                                     MOVE 1 TO MM
                                     ADD  1 TO YY
                                END-IF
                             END-IF
                        END-IF
                END-PERFORM
                MOVE SPACES TO GLB-DCDATE
                MOVE DD-X   TO GLB-DCDATE (1: 2)
                MOVE MM-X   TO GLB-DCDATE (3: 2)
                MOVE YY-X   TO GLB-DCDATE (5: 2)
           END-IF

           MOVE GLB-DCDATE TO DATA-TESTE

           IF   DATA-TESTE NUMERIC
                MOVE "9" TO GLB-DCEDIT
           END-IF

           IF  (DATA-NUMERICA
           OR   DATA-2000)
           AND  GLB-DCEDIT = "9"
                IF   DATA-2000
                     PERFORM VARYING I FROM 1 BY 2 UNTIL I > 8
                        EVALUATE GLB-DCFORMAT (I: 2)
                            WHEN "DD"
                                 MOVE GLB-DCDATE (I: 2) TO DD-X-2000
                            WHEN "MM"
                                 MOVE GLB-DCDATE (I: 2) TO MM-X-2000
                            WHEN "CC"
                                 MOVE GLB-DCDATE (I: 2) TO CC-X-2000
                            WHEN "YY"
                                 MOVE GLB-DCDATE (I: 2) TO YY-X-2000
                            WHEN OTHER
                                 MOVE "*****" TO GLB-STATUS
                        END-EVALUATE
                     END-PERFORM
                ELSE
                     PERFORM VARYING I FROM 1 BY 2 UNTIL I > 6
                             EVALUATE GLB-DCFORMAT (I: 2)
                                 WHEN "DD"
                                      MOVE GLB-DCDATE (I: 2) TO DD-X
                                 WHEN "MM"
                                      MOVE GLB-DCDATE (I: 2) TO MM-X
                                 WHEN "YY"
                                      MOVE GLB-DCDATE (I: 2) TO YY-X
                                 WHEN OTHER
                                      MOVE "*****" TO GLB-STATUS
                             END-EVALUATE
                     END-PERFORM
                END-IF
           ELSE
           IF  (GLB-DCFORMAT = "TO-DAYNUMBER"
           OR   GLB-DCFORMAT = "VERIFY")
           AND  GLB-DCEDIT = "9"
                EVALUATE TRUE
                    WHEN GLB-DCTYPE = "IN"
                         MOVE GLB-DCDATE (1: 2) TO YY-X
                         MOVE GLB-DCDATE (3: 2) TO MM-X
                         MOVE GLB-DCDATE (5: 2) TO DD-X
                    WHEN GLB-DCTYPE = "US"
                         MOVE GLB-DCDATE (1: 2) TO MM-X
                         MOVE GLB-DCDATE (3: 2) TO DD-X
                         MOVE GLB-DCDATE (5: 2) TO YY-X
                    WHEN GLB-DCTYPE = "UK" OR "  "
                         MOVE GLB-DCDATE (1: 2) TO DD-X
                         MOVE GLB-DCDATE (3: 2) TO MM-X
                         MOVE GLB-DCDATE (5: 2) TO YY-X
                    WHEN OTHER
                         MOVE "*****" TO GLB-STATUS
                END-EVALUATE
           ELSE
           IF  (GLB-DCFORMAT = "TO-DAYNUMBER"
           OR   GLB-DCFORMAT = "VERIFY")
           AND  GLB-DCEDIT = "X"
                MOVE GLB-DCDATE (1: 2) TO DD-X
                MOVE GLB-DCDATE (6: 2) TO YY-X
                EVALUATE GLB-DCDATE (3: 3)
                    WHEN "JAN" MOVE 01 TO MM-X
                    WHEN "FEV" MOVE 02 TO MM-X
                    WHEN "MAR" MOVE 03 TO MM-X
                    WHEN "ABR" MOVE 04 TO MM-X
                    WHEN "MAI" MOVE 05 TO MM-X
                    WHEN "JUN" MOVE 06 TO MM-X
                    WHEN "JUL" MOVE 07 TO MM-X
                    WHEN "AGO" MOVE 08 TO MM-X
                    WHEN "SET" MOVE 09 TO MM-X
                    WHEN "OUT" MOVE 10 TO MM-X
                    WHEN "NOV" MOVE 11 TO MM-X
                    WHEN "DEZ" MOVE 12 TO MM-X
                    WHEN OTHER MOVE "*****" TO GLB-STATUS
                END-EVALUATE
           ELSE
                MOVE "*****" TO GLB-STATUS
           END-IF

           DIVIDE YY BY 4 GIVING LIXO REMAINDER RESTO

           IF   RESTO EQUAL ZERO
                MOVE 29 TO ULTIMO-DIA (2)
           ELSE
                MOVE 28 TO ULTIMO-DIA (2)
           END-IF

           IF   ((HOJE NOT NUMERIC)
           OR   (MM EQUAL ZEROS) OR (MM GREATER 12)
           OR   (DD EQUAL ZEROS) OR (DD GREATER ULTIMO-DIA (MM)))
                MOVE "*****" TO GLB-STATUS
           END-IF

      *    DISPLAY " GLB-BASE  ----> " LINE 20 POSITION 01.
      *    DISPLAY GLB-BASE            LINE 20 POSITION 18.
      *    DISPLAY " DATA-HOJE ----> " LINE 21 POSITION 01.
      *    DISPLAY HOJE                LINE 21 POSITION 18.
      *    DISPLAY " GLB-DCYEAR ---> " LINE 22 POSITION 01.
      *    DISPLAY GLB-DCYEAR          LINE 22 POSITION 18.
      *    DISPLAY " GLB-CENTURY --> " LINE 23 POSITION 01.
      *    DISPLAY GLB-CENTURY         LINE 23 POSITION 18.


      *    Obtendo GLB-DCYEAR

           COMPUTE GLB-DCYEAR = (GLB-CENTURY * 100) + YY.

      *    DISPLAY " GLB-DCYEAR APOS ==> " LINE 24 POSITION 01.
      *    DISPLAY GLB-DCYEAR              LINE 24 POSITION 22.
      *    ACCEPT  LIXO                    LINE 24 POSITION 78.

      *    IF   GLB-BASE > GLB-DCYEAR
      *         MOVE "*****" TO GLB-STATUS.

           IF   GLB-STATUS = "*****"
                MOVE 0     TO GLB-DCOPERAND
                MOVE SPACE TO GLB-DCSINAL
                EXIT PROGRAM
           END-IF

      *    Soma dias

           MOVE GLB-DCOPERAND TO M-DIA.

           EVALUATE TRUE
               WHEN M-DIA NOT = 0
                   IF   GLB-DCSINAL = " "
                        IF   GLB-DCOPERAND NEGATIVE
                             MOVE "-" TO GLB-DCSINAL
                        ELSE
                             MOVE "+" TO GLB-DCSINAL
                        END-IF
                   ELSE
                        IF   GLB-DCSINAL = "+"
                        AND  GLB-DCOPERAND NEGATIVE
                             MOVE "-" TO GLB-DCSINAL
                        ELSE
                             IF   GLB-DCSINAL = "-"
                             AND  GLB-DCOPERAND POSITIVE
                                  MOVE "-" TO GLB-DCSINAL
                             END-IF
                        END-IF
                   END-IF
                   IF   GLB-DCSINAL = "+"
                        ADD DD TO M-DIA
                        PERFORM UNTIL M-DIA < ULTIMO-DIA (MM)
                                IF  M-DIA NOT < ULTIMO-DIA (MM)
                                    SUBTRACT ULTIMO-DIA (MM) FROM M-DIA
                                    ADD      1                 TO MM
                                    IF   MM EQUAL 13
                                         MOVE 1 TO MM
                                         ADD  1 TO YY GLB-DCYEAR
                                    END-IF
                                    DIVIDE YY BY 4
                                           GIVING LIXO
                                           REMAINDER RESTO
                                    IF   RESTO = 0
                                         MOVE 29 TO ULTIMO-DIA (2)
                                    ELSE
                                         MOVE 28 TO ULTIMO-DIA (2)
                                    END-IF
                                END-IF
                        END-PERFORM
                        MOVE M-DIA TO DD
                   ELSE
                        PERFORM UNTIL M-DIA = 0
                             IF  M-DIA NOT < ULTIMO-DIA (MM)
                                 SUBTRACT ULTIMO-DIA (MM) FROM M-DIA
                                 SUBTRACT 1               FROM MM
                                 IF   MM EQUAL 0
                                      MOVE 12 TO MM
                                      SUBTRACT 1 FROM YY GLB-DCYEAR
                                      DIVIDE YY BY 4 GIVING LIXO
                                             REMAINDER RESTO
                                      IF   RESTO = 0
                                           MOVE 29 TO ULTIMO-DIA (2)
                                      ELSE
                                           MOVE 28 TO ULTIMO-DIA (2)
                                      END-IF
                                 END-IF
                             ELSE
                                 SUBTRACT M-DIA FROM DD
                                 MOVE 0 TO M-DIA
                                 IF DD = 0
                                    SUBTRACT 1 FROM MM
                                    IF MM EQUAL 0
                                       MOVE 12 TO MM
                                       SUBTRACT 1 FROM YY
                                                       GLB-DCYEAR
                                       DIVIDE YY BY 4
                                              GIVING LIXO
                                              REMAINDER RESTO
                                       IF   RESTO = 0
                                            MOVE 29 TO ULTIMO-DIA (2)
                                       ELSE
                                            MOVE 28 TO ULTIMO-DIA (2)
                                       END-IF
                                    END-IF
                                    MOVE ULTIMO-DIA (MM) TO DD
                                 END-IF
                             END-IF
                        END-PERFORM
                   END-IF
               WHEN (DATA-NUMERICA OR DATA-2000)
                AND GLB-DCEDIT = "9"
                    IF DATA-2000
                    PERFORM VARYING I FROM 1 BY 2 UNTIL I > 8
                         EVALUATE GLB-DCFORMAT (I: 2)
                            WHEN "DD"  MOVE DD-2000 TO GLB-DCDATE (I: 2)
                            WHEN "MM"  MOVE MM-2000 TO GLB-DCDATE (I: 2)
                            WHEN "YY"  MOVE YY-2000 TO GLB-DCDATE (I: 2)
                            WHEN "CC"  MOVE CC-2000 TO GLB-DCDATE (I: 2)
                         END-EVALUATE
                    END-PERFORM
                    ELSE
                    PERFORM VARYING I FROM 1 BY 2 UNTIL I > 6
                            EVALUATE GLB-DCFORMAT (I: 2)
                              WHEN "DD"  MOVE DD TO GLB-DCDATE (I: 2)
                              WHEN "MM"  MOVE MM TO GLB-DCDATE (I: 2)
                              WHEN "YY"  MOVE YY TO GLB-DCDATE (I: 2)
                            END-EVALUATE
                    END-PERFORM
                    END-IF
               WHEN GLB-DCFORMAT = "TO-DAYNUMBER"
                    EVALUATE TRUE
                        WHEN GLB-DCTYPE = "IN"
                             MOVE YY TO GLB-DCDATE (1: 2)
                             MOVE MM TO GLB-DCDATE (3: 2)
                             MOVE DD TO GLB-DCDATE (5: 2)
                        WHEN GLB-DCTYPE = "US"
                             MOVE MM TO GLB-DCDATE (1: 2)
                             MOVE DD TO GLB-DCDATE (3: 2)
                             MOVE YY TO GLB-DCDATE (5: 2)
                        WHEN GLB-DCTYPE = "UK" OR " "
                             MOVE DD TO GLB-DCDATE (1: 2)
                             MOVE MM TO GLB-DCDATE (3: 2)
                             MOVE YY TO GLB-DCDATE (5: 2)
                        WHEN OTHER
                             MOVE "*****" TO GLB-STATUS
                    END-EVALUATE
               WHEN GLB-DCEDIT = "X"
                    MOVE DD            TO GLB-DCDATE (1: 2)
                    MOVE NOME-MES (MM) TO GLB-DCDATE (3: 3)
                    MOVE YY            TO GLB-DCDATE (6: 2)
           END-EVALUATE

           MOVE 0     TO GLB-DCOPERAND
           MOVE SPACE TO GLB-DCSINAL

      *    Obtendo GLB-DC-DAYNUM

           DIVIDE YY BY 4 GIVING LIXO REMAINDER RESTO
           IF   RESTO EQUAL ZERO
                MOVE 29 TO ULTIMO-DIA (2)
           ELSE
                MOVE 28 TO ULTIMO-DIA (2)
           END-IF

           MOVE ZERO TO DIAS
           PERFORM VARYING I FROM GLB-BASE BY 1
                     UNTIL I NOT LESS GLB-DCYEAR
                   MOVE I TO ANO-BI
                   DIVIDE ANO-BI BY 4 GIVING LIXO REMAINDER RESTO
                   IF   RESTO EQUAL ZERO
                        ADD 366 TO DIAS
                   ELSE
                        ADD 365 TO DIAS
                   END-IF
           END-PERFORM
           MOVE 0 TO DDD
           PERFORM VARYING I FROM 1 BY 1 UNTIL I EQUAL MM
                   ADD ULTIMO-DIA (I) TO DIAS DDD
           END-PERFORM
           ADD    DD            TO DIAS DDD
           COMPUTE GLB-DC-DAYNUM = DIAS - 1

           IF   GLB-DCFORMAT = "TO-DAYNUMBER"
                MOVE GLB-DC-DAYNUM TO GLB-TOTAL
           ELSE
                MOVE DD            TO GLB-DC-DD-MM-YY (1: 2)
                MOVE "/"           TO GLB-DC-DD-MM-YY (3: 1)
                MOVE MM            TO GLB-DC-DD-MM-YY (4: 2)
                MOVE "/"           TO GLB-DC-DD-MM-YY (6: 1)
                MOVE YY            TO GLB-DC-DD-MM-YY (7: 2)

                MOVE DD            TO GLB-DC-DD-MMM-YY (1: 2)
                MOVE NOME-MES (MM) TO GLB-DC-DD-MMM-YY (4: 3)
                MOVE YY            TO GLB-DC-DD-MMM-YY (8: 2)

                MOVE DD            TO GLB-DC-DDMMMYY (1: 2)
                MOVE NOME-MES (MM) TO GLB-DC-DDMMMYY (3: 3)
                MOVE YY            TO GLB-DC-DDMMMYY (6: 2)

                MOVE DD            TO GLB-DC-DDMMYY (1: 2)
                MOVE MM            TO GLB-DC-DDMMYY (3: 3)
                MOVE YY            TO GLB-DC-DDMMYY (5: 2)

                MOVE MM            TO GLB-DC-MM-DD-YY (1: 2)
                MOVE "/"           TO GLB-DC-MM-DD-YY (3: 1)
                MOVE DD            TO GLB-DC-MM-DD-YY (4: 2)
                MOVE "/"           TO GLB-DC-MM-DD-YY (6: 1)
                MOVE YY            TO GLB-DC-MM-DD-YY (7: 2)

                MOVE MM            TO GLB-DC-MMDDYY (1: 2)
                MOVE DD            TO GLB-DC-MMDDYY (3: 2)
                MOVE YY            TO GLB-DC-MMDDYY (5: 2)

                MOVE NOME-MES (MM) TO GLB-DC-MMM-DD-YY (1: 3)
                MOVE DD            TO GLB-DC-MMM-DD-YY (5: 2)
                MOVE YY            TO GLB-DC-MMM-DD-YY (8: 2)

                MOVE NOME-MES (MM) TO GLB-DC-MMMDDYY (1: 3)
                MOVE DD            TO GLB-DC-MMMDDYY (4: 2)
                MOVE YY            TO GLB-DC-MMMDDYY (6: 2)

                MOVE SPACES        TO GLB-DC-UK-ALPHA
                                      GLB-DC-US-ALPHA
                                      GLB-DC-IN-ALPHA

                STRING DD              DELIMITED SIZE
                       " "             DELIMITED SIZE
                       NOME-MES (MM)   DELIMITED " "
                       " "             DELIMITED SIZE
                       GLB-DCYEAR      DELIMITED SIZE
                                       INTO GLB-DC-UK-ALPHA

                STRING NOME-MES (MM)   DELIMITED " "
                       " "             DELIMITED SIZE
                       DD              DELIMITED SIZE
                       " "             DELIMITED SIZE
                       GLB-DCYEAR      DELIMITED SIZE
                                       INTO GLB-DC-US-ALPHA

                STRING GLB-DCYEAR      DELIMITED SIZE
                       " "             DELIMITED SIZE
                       NOME-MES (MM)   DELIMITED " "
                       " "             DELIMITED SIZE
                       DD              DELIMITED SIZE
                                       INTO GLB-DC-IN-ALPHA

                MOVE YY            TO GLB-DC-YYDDD (1: 2)
                MOVE DDD           TO GLB-DC-YYDDD (3: 3)

                MOVE YY            TO GLB-DC-YYMMDD (1: 2)
                MOVE MM            TO GLB-DC-YYMMDD (3: 2)
                MOVE DD            TO GLB-DC-YYMMDD (5: 2)

                MOVE YY            TO GLB-DC-YYMMMDD (1: 2)
                MOVE NOME-MES (MM) TO GLB-DC-YYMMMDD (3: 3)
                MOVE DD            TO GLB-DC-YYMMMDD (6: 2)

      *         MOVE    YY                TO AA-WEEK
      *         MOVE    MM                TO MM-WEEK
      *         MOVE    DD                TO DD-WEEK
      *         ADD     INC-WEEK(MM-WEEK) TO DD-WEEK
      *         ADD     2199              TO GRWEEK-DATA
      *         COMPUTE V-WEEK-5           = AA-WEEK * 125
      *         ADD     DM-WEEK           TO V-WEEK-5
      *         DIVIDE  V-WEEK-3 BY 7 GIVING DD-WEEK
      *         COMPUTE GRWEEK-DIA         = V-WEEK-3 - (DD-WEEK * 7)
      *         IF   GRWEEK-DIA = 0
      *              MOVE NM-SABADO            TO GLB-DC-TODAY
      *         ELSE
      *              MOVE NM-WEEK (GRWEEK-DIA) TO GLB-DC-TODAY
      *         END-IF
                COMPUTE CWDATE = 20000000 + GLB-DC-YYMMDD
                EXEC COBOLware Time Week
                     DATE CWDATE
                     WEEK-CHAR;GLB-DC-TODAY
                END-EXEC
                INSPECT GLB-DC-TODAY CONVERTING MINUSCULAS TO MAIUSCULAS
                COMPUTE GLB-DC-WEEKNO = DDD / 7
                IF   TO-DATE = 1
                     MOVE "DDMMYY" TO GLB-DCFORMAT
                     PERFORM VARYING I FROM 1 BY 2 UNTIL I > 6
                             EVALUATE GLB-DCFORMAT (I: 2)
                                 WHEN "DD"
                                      MOVE DD TO GLB-DCDATE (I: 2)
                                 WHEN "MM"
                                      MOVE MM TO GLB-DCDATE (I: 2)
                                 WHEN "YY"
                                      MOVE YY TO GLB-DCDATE (I: 2)
                             END-EVALUATE
                     END-PERFORM
                     MOVE SAVE-DCTYPE TO GLB-DCTYPE
                     MOVE SAVE-DCEDIT TO GLB-DCEDIT
                     MOVE "TO-DATE"   TO GLB-DCFORMAT
               END-IF
           END-IF

           initialize glb-dc-dd-mm-ccyy
                      glb-dc-dd-mmm-ccyy
                      glb-dc-ddmmmccyy
                      glb-dc-ddmmccyy
                      glb-dc-mm-dd-ccyy
                      glb-dc-mmddccyy
                      glb-dc-mmm-dd-ccyy
                      glb-dc-mmmddccyy
                      glb-dc-ccyy-mm-dd
                      glb-dc-ccyy-mmm-dd
                      glb-dc-ccyyddd
                      glb-dc-ccyymmdd
                      glb-dc-ccyymmmdd
            IF   MM NOT = ZERO
                 MOVE NOME-MES (MM) TO MMM
                 STRING DD '/' MM '/' CC YY
                    DELIMITED BY SIZE INTO glb-dc-dd-mm-ccyy
                 STRING DD '/' MMM '/' CC YY
                    DELIMITED BY SIZE INTO glb-dc-dd-mmm-ccyy
                 STRING DD MMM CC YY
                    DELIMITED BY SIZE INTO glb-dc-ddmmmccyy
                 STRING DD MM CC YY
                    DELIMITED BY SIZE INTO glb-dc-ddmmccyy
                 STRING MM '/' DD '/' CC YY
                    DELIMITED BY SIZE INTO glb-dc-mm-dd-ccyy
                 STRING MM DD CC YY
                    DELIMITED BY SIZE INTO glb-dc-mmddccyy
                 STRING MMM '/' DD '/' CC YY
                    DELIMITED BY SIZE INTO glb-dc-mmm-dd-ccyy
                 STRING MMM DD CC YY
                    DELIMITED BY SIZE INTO glb-dc-mmmddccyy
                 STRING CC YY '/' MM '/' DD
                    DELIMITED BY SIZE INTO glb-dc-ccyy-mm-dd
                 STRING CC YY '/' MMM '/' DD
                    DELIMITED BY SIZE INTO glb-dc-ccyy-mmm-dd
                 STRING CC YY DDD
                    DELIMITED BY SIZE INTO glb-dc-ccyyddd
                 STRING CC YY MM DD
                    DELIMITED BY SIZE INTO glb-dc-ccyymmdd
                 STRING CC YY MMM DD
                    DELIMITED BY SIZE INTO glb-dc-ccyymmmdd
            END-IF.

       000-99-FIM. GOBACK.
       END PROGRAM XSDD2000.
