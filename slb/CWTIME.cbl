       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWTIME.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  22/07/1996.
       SECURITY.      *************************************************
                      *                                               *
                      *  Tratamento de tempo (datas e horas)          *
                      *                                               *
                      *************************************************
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
      *    05 CENTURY                        PIC  9(002) VALUE 20.
           05 HORA                           PIC  9(006) VALUE 0.
           05 MIN-YEAR                       PIC  9(004) VALUE 0.
           05 MAX-YEAR                       PIC  9(004) VALUE 9999.
           05 WS-DATE.
              10 WS-ANO                      PIC  9(004).
              10 WS-MES                      PIC  9(002).
                 88 WS-MES-OK                      VALUE 1 THRU 12.
              10 WS-DIA                      PIC  9(002).
           05 WS-DATE2.
              10 WS-DIA2                     PIC  9(002).
              10 WS-MES2                     PIC  9(002).
              10 WS-ANO2                     PIC  9(004).
           05 WS-TIME                        PIC  X(006) VALUE SPACES.

           05 SEG-1                          PIC  9(009) VALUE ZEROS.
           05 SEG-2                          PIC  9(009) VALUE ZEROS.
           05 MM                             PIC  9(002) VALUE 0.
           05 SS                             PIC  9(009) VALUE 0.

           05 T-DMAX             VALUE "312831303130313130313031".
              10 DIA-MAX OCCURS 12           PIC  9(002).
           05 W-TP-ANO                       PIC  9(001).
              88 ANO-BISSEXTO                            VALUE  0.
           05 W-DIA                          PIC  9(007).
           05 W-NADA                         PIC  9(004).
           05 TABELA-DE-MESES.
              10 PIC  X(9) VALUE "Janeiro".
              10 PIC  X(9) VALUE "Fevereiro".
              10 PIC  X(9) VALUE "Marco".
              10 PIC  X(9) VALUE "Abril".
              10 PIC  X(9) VALUE "Maio".
              10 PIC  X(9) VALUE "Junho".
              10 PIC  X(9) VALUE "Julho".
              10 PIC  X(9) VALUE "Agosto".
              10 PIC  X(9) VALUE "Setembro".
              10 PIC  X(9) VALUE "Outubro".
              10 PIC  X(9) VALUE "Novembro".
              10 PIC  X(9) VALUE "Dezembro".
           05 REDEFINES TABELA-DE-MESES.
              10 NOME-MES OCCURS 12          PIC  X(009).
           05 WS-LONG.
              10 WS-DIA-LONG                 PIC  Z(002) VALUE ZEROS.
              10                             PIC  X(004) VALUE " de ".
              10 WS-MES-LONG                 PIC  X(009) VALUE SPACES.
              10                             PIC  X(004) VALUE " de ".
              10 WS-ANO-LONG                 PIC  9(004) VALUE ZEROS.
              10                             PIC  X(002) VALUE SPACES.
           05 REDEFINES WS-LONG.
              10 BYTE-LONG OCCURS 25         PIC  X(001).
           05 WS-LONG2                                   VALUE SPACES.
              10 BYTE-LONG2 OCCURS 25        PIC  X(001).
           05 LONG-I                         PIC  9(002) VALUE 0.
           05 LONG-Y                         PIC  9(002) VALUE 0.
           05 DIAS-X                  COMP-3 PIC  9(007) VALUE ZERO.
           05 DIAS-1                  COMP-3 PIC  9(007) VALUE ZERO.
           05 DIAS-2                  COMP-3 PIC  9(007) VALUE ZERO.
           05 LIXO                    COMP-3 PIC  9(007) VALUE ZERO.
           05 I                       COMP-3 PIC  9(004) VALUE ZERO.
           05 DATE-INICIAL                   PIC  9(008) VALUE ZERO.
           05 REDEFINES DATE-INICIAL.
              10 AA-I                        PIC  9(004).
              10 MMDD-I.
                 15 MM-I                     PIC  9(002).
                 15 DD-I                     PIC  9(002).
           05 DATE-FINAL                     PIC  9(008) VALUE ZERO.
           05 REDEFINES DATE-FINAL.
              10 AA-F                        PIC  9(004).
              10 MMDD-F.
                 15 MM-F                     PIC  9(002).
                 15 DD-F                     PIC  9(002).
           05 NUM-DIAS                       PIC  9(006).
           05 WS-SINAL                       PIC  X(001) VALUE "+".
           05 NDIA-WEEK                      PIC  9(001) VALUE ZERO.
           05 TAB-NM-WEEK.
              10 PIC X(007) VALUE "Sabado ".
              10 PIC X(007) VALUE "Domingo".
              10 PIC X(007) VALUE "Segunda".
              10 PIC X(007) VALUE "Terca  ".
              10 PIC X(007) VALUE "Quarta".
              10 PIC X(007) VALUE "Quinta".
              10 PIC X(007) VALUE "Sexta  ".
           05 REDEFINES TAB-NM-WEEK.
              10 NM-WEEK OCCURS 7 PIC X(007).

       01  WS-CURRENT-DATE-FIELDS.
           05  WS-CURRENT-DATE.
               10 WS-CURRENT-YEAR    PIC  9(004).
               10 WS-CURRENT-MONTH   PIC  9(002).
               10 WS-CURRENT-DAY     PIC  9(002).
           05  WS-CURRENT-TIME.
               10 WS-CURRENT-HOUR    PIC  9(002).
               10 WS-CURRENT-MINUTE  PIC  9(002).
               10 WS-CURRENT-SECOND  PIC  9(002).
               10 WS-CURRENT-MS      PIC  9(002).
               10 WS-GMT-SIGN        PIC  X(001).
               10 WS-GMT-TIME        PIC  X(004).

       LINKAGE SECTION.

       01  PARAMETROS-CWTIME.
           05 CWTIME-TYPE                    PIC  9(002).
              88 CWTIME-NORMAL                           VALUE 1.
              88 CWTIME-REVERSED                         VALUE 2.
           05 CWTIME-FUNCTION                PIC  9(002).
              88 CWTIME-ADD-DAYS                         VALUE 1.
              88 CWTIME-EDIT                             VALUE 2.
              88 CWTIME-INTERVAL                         VALUE 3.
              88 CWTIME-REVERSE                          VALUE 4.
              88 CWTIME-SUBTRACT-DAYS                    VALUE 5.
              88 CWTIME-TODAY                            VALUE 6.
              88 CWTIME-VALIDATE                         VALUE 7.
              88 CWTIME-WEEK                             VALUE 8.
              88 CWTIME-MIN-YEAR                         VALUE 9.
              88 CWTIME-MAX-YEAR                         VALUE 0.
           05 CWTIME-DATE                    PIC  9(008).
           05 CWTIME-TIME                    PIC  9(006).
           05 REDEFINES CWTIME-TIME.
              10 HH-1                        PIC  9(002).
              10 MM-1                        PIC  9(002).
              10 SS-1                        PIC  9(002).
           05 CWTIME-DAYS                    PIC  9(007).
           05 CWTIME-DATE-TIME-FINAL.
              10 CWTIME-DATE-FINAL           PIC  9(008).
              10 CWTIME-TIME-FINAL           PIC  X(006).
              10 REDEFINES CWTIME-TIME-FINAL.
                 15 HH-2                     PIC  9(002).
                 15 MM-2                     PIC  9(002).
                 15 SS-2                     PIC  9(002).
           05 CWTIME-DAYS-FINAL              PIC S9(007).
           05 CWTIME-TOTAL-TIME              PIC  9(012).
           05 CWTIME-TOTAL-HOURS             PIC  9(009).
           05 CWTIME-TOTAL-MINUTES           PIC  9(002).
           05 CWTIME-TOTAL-SECONDS           PIC  9(002).
           05 CWTIME-YEARS-OLD               PIC S9(004).
           05 CWTIME-MOUNTHS-OLD             PIC S9(002).
           05 CWTIME-DAYS-OLD                PIC S9(002).
           05 CWTIME-DATE-EDITED             PIC  X(010).
           05 CWTIME-DATE-EDITED-LONG        PIC  X(023).
           05 CWTIME-MOUNTH-EDITED           PIC  X(014).
           05 CWTIME-TIME-EDITED             PIC  X(008).
           05 CWTIME-WEEK-NUM                PIC  9(001).
           05 CWTIME-WEEK-CHAR               PIC  X(007).

       PROCEDURE DIVISION USING PARAMETROS-CWTIME.

       INICIO.

      *    DISPLAY "YYYY"        UPON ENVIRONMENT-NAME
      *    ACCEPT   CENTURY(1:2) FROM ENVIRONMENT-VALUE

           IF   CWTIME-MIN-YEAR
                MOVE CWTIME-YEARS-OLD TO MIN-YEAR
                GOBACK
           ELSE
                IF   CWTIME-MAX-YEAR
                     MOVE CWTIME-YEARS-OLD TO MAX-YEAR
                     GOBACK
                END-IF
           END-IF

           IF   CWTIME-NORMAL
                MOVE CWTIME-DATE TO WS-DATE2
                MOVE WS-ANO2     TO WS-ANO
                MOVE WS-MES2     TO WS-MES
                MOVE WS-DIA2     TO WS-DIA
           ELSE
                MOVE CWTIME-DATE TO WS-DATE
           END-IF

           EVALUATE TRUE
               WHEN CWTIME-ADD-DAYS
                    PERFORM CWADAY THRU FIM-CWADAY
               WHEN CWTIME-EDIT
                    MOVE WS-DIA TO CWTIME-DATE-EDITED (1: 2)
                                   WS-DIA-LONG
                    MOVE "/"    TO CWTIME-DATE-EDITED (3: 1)
                    MOVE WS-MES TO CWTIME-DATE-EDITED (4: 2)
                    MOVE "/"    TO CWTIME-DATE-EDITED (6: 1)
                    MOVE WS-ANO TO CWTIME-DATE-EDITED (7: 4)
                                   WS-ANO-LONG
                    IF   WS-MES-OK
                         MOVE NOME-MES (WS-MES) TO WS-MES-LONG
                                                   CWTIME-MOUNTH-EDITED
                    ELSE
                         MOVE WS-MES            TO WS-MES-LONG
                                                   CWTIME-MOUNTH-EDITED
                    END-IF
                    MOVE 0      TO LONG-I
                                   LONG-Y
                    IF  WS-DIA < 10
                        ADD 1   TO LONG-I
                    END-IF
                    PERFORM UNTIL LONG-I > 22
                            ADD 1 TO LONG-I
                            IF  BYTE-LONG (LONG-I) = SPACE
                            AND BYTE-LONG (LONG-I + 1) = SPACE
                                CONTINUE
                            ELSE
                                ADD  1 TO LONG-Y
                                MOVE BYTE-LONG  (LONG-I)
                                  TO BYTE-LONG2 (LONG-Y)
                            END-IF
                    END-PERFORM
                    MOVE WS-LONG2             TO CWTIME-DATE-EDITED-LONG
                    MOVE CWTIME-MOUNTH-EDITED TO WS-LONG2
                    PERFORM VARYING LONG-I FROM 1 BY 1
                                    UNTIL BYTE-LONG2 (LONG-I) = SPACE
                            CONTINUE
                    END-PERFORM
                    MOVE "/"      TO WS-LONG2 (LONG-I: )
                    ADD  1        TO LONG-I
                    MOVE WS-ANO   TO WS-LONG2 (LONG-I: )
                    MOVE WS-LONG2 TO CWTIME-MOUNTH-EDITED
                    MOVE HH-1     TO CWTIME-TIME-EDITED (1: 2)
                    MOVE ":"      TO CWTIME-TIME-EDITED (3: 1)
                    MOVE MM-1     TO CWTIME-TIME-EDITED (4: 2)
                    MOVE ":"      TO CWTIME-TIME-EDITED (6: 1)
                    MOVE SS-1     TO CWTIME-TIME-EDITED (7: 2)
               WHEN CWTIME-INTERVAL
                    IF   CWTIME-DATE-FINAL = 0
                    AND  CWTIME-DATE = 0
                         PERFORM HOJE THRU FIM-HOJE
                         MOVE CWTIME-DATE-FINAL TO CWTIME-DATE
                    ELSE
                         IF   CWTIME-DATE = 0
                              MOVE CWTIME-DATE-FINAL TO CWTIME-DATE
                         END-IF
                    END-IF
                    PERFORM CWDIAS THRU FIM-CWDIAS
                    COMPUTE CWTIME-TOTAL-HOURS = CWTIME-DAYS-FINAL * 24
                    COMPUTE SEG-1 = HH-1  * 3600
                    COMPUTE SEG-2 = HH-2  * 3600
                    COMPUTE SEG-1 = SEG-1 + (MM-1 * 60)
                    COMPUTE SEG-2 = SEG-2 + (MM-2 * 60)
                    COMPUTE SEG-1 = SEG-1 + SS-1
                    COMPUTE SEG-2 = SEG-2 + SS-2
                    IF   SEG-1 > SEG-2
                         SUBTRACT 24  FROM CWTIME-TOTAL-HOURS
                         ADD      86400 TO SEG-2
                    END-IF
                    COMPUTE SS = SEG-2 - SEG-1
                    MOVE 0 TO MM
                    PERFORM UNTIL SS < 60
                            SUBTRACT 60 FROM SS
                            ADD      1    TO MM
                            IF   MM = 60
                                 ADD  1 TO CWTIME-TOTAL-HOURS
                                 MOVE 0 TO MM
                            END-IF
                    END-PERFORM
                    MOVE MM TO CWTIME-TOTAL-MINUTES
                    MOVE SS TO CWTIME-TOTAL-SECONDS
                    COMPUTE CWTIME-TOTAL-TIME =
                            (CWTIME-TOTAL-HOURS * 3600) +
                            (MM                 * 60)    +
                            SS
               WHEN CWTIME-REVERSE
                    IF   CWTIME-NORMAL
                         MOVE WS-DATE     TO CWTIME-DATE-FINAL
                    ELSE
                         MOVE CWTIME-DATE TO WS-DATE
                         MOVE WS-ANO      TO WS-ANO2
                         MOVE WS-MES      TO WS-MES2
                         MOVE WS-DIA      TO WS-DIA2
                         MOVE WS-DATE2    TO CWTIME-DATE-FINAL
                    END-IF
               WHEN CWTIME-SUBTRACT-DAYS
                    PERFORM CWSDAY THRU FIM-CWSDAY
               WHEN CWTIME-TODAY
                    PERFORM HOJE THRU FIM-HOJE
      *             ACCEPT CWTIME-TIME-FINAL        FROM TIME
                    MOVE WS-CURRENT-HOUR   TO HH-2
                    MOVE WS-CURRENT-MINUTE TO MM-2
                    MOVE WS-CURRENT-SECOND TO SS-2
               WHEN CWTIME-VALIDATE
                    DIVIDE WS-ANO BY 4 GIVING W-NADA REMAINDER W-TP-ANO
                    IF   ANO-BISSEXTO
                         MOVE 29 TO DIA-MAX (2)
                    ELSE
                         MOVE 28 TO DIA-MAX (2)
                    END-IF
                    IF  (WS-DATE NOT NUMERIC)
                    OR   WS-MES EQUAL ZEROS
                    OR   WS-MES GREATER 12
                    OR   WS-DIA EQUAL ZEROS
                    OR   WS-DIA GREATER DIA-MAX (WS-MES)
                    OR   WS-ANO LESS    MIN-YEAR
                    OR   WS-ANO GREATER MAX-YEAR
                         MOVE ZEROS   TO CWTIME-DATE-FINAL
                    ELSE
                         IF   CWTIME-NORMAL
                              MOVE WS-ANO   TO WS-ANO2
                              MOVE WS-MES   TO WS-MES2
                              MOVE WS-DIA   TO WS-DIA2
                              MOVE WS-DATE2 TO CWTIME-DATE-FINAL
                         ELSE
                              MOVE WS-DATE  TO CWTIME-DATE-FINAL
                         END-IF
                    END-IF
               WHEN CWTIME-WEEK
                    MOVE    WS-DATE           TO CWTIME-DATE-FINAL
                    MOVE    "00000101"        TO WS-DATE
                    PERFORM CWDIAS          THRU FIM-CWDIAS
                    DIVIDE CWTIME-DAYS-FINAL BY 7 GIVING LIXO
                                   REMAINDER NDIA-WEEK
                    IF   NDIA-WEEK = 0
                         MOVE 7 TO NDIA-WEEK
                    END-IF
                    MOVE    NM-WEEK (NDIA-WEEK) TO CWTIME-WEEK-CHAR
                    COMPUTE CWTIME-WEEK-NUM = NDIA-WEEK - 1
           END-EVALUATE.

       FIM. GOBACK.

       HOJE.

      *    ACCEPT CWTIME-DATE-FINAL (3: 6) FROM DATE
      *    MOVE CENTURY                TO CWTIME-DATE-FINAL (1: 2)
      *    MOVE "20"                   TO CWTIME-DATE-FINAL (1: 2)
      *    IF   CWTIME-NORMAL
      *         MOVE CWTIME-DATE-FINAL TO WS-DATE
      *         MOVE WS-ANO            TO WS-ANO2
      *         MOVE WS-MES            TO WS-MES2
      *         MOVE WS-DIA            TO WS-DIA2
      *         MOVE WS-DATE2          TO CWTIME-DATE-FINAL
      *    END-IF.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS
           MOVE WS-CURRENT-YEAR  TO WS-ANO WS-ANO2
           MOVE WS-CURRENT-MONTH TO WS-MES WS-MES2
           MOVE WS-CURRENT-DAY   TO WS-DIA WS-DIA2
           IF   CWTIME-NORMAL
                MOVE WS-DATE2         TO CWTIME-DATE-FINAL
           ELSE
                MOVE WS-CURRENT-DATE  TO CWTIME-DATE-FINAL
           END-IF.

       FIM-HOJE. EXIT.

       CWADAY.

           COMPUTE W-DIA = WS-DIA + CWTIME-DAYS.

       CWADAY-CHECK-ANO.

           DIVIDE WS-ANO BY 4
                  GIVING W-NADA
                         REMAINDER W-TP-ANO

           IF   ANO-BISSEXTO
                MOVE 29 TO DIA-MAX (2)
           ELSE
                MOVE 28 TO DIA-MAX (2)
           END-IF.

       CWADAY-MAIS-DIA.

           IF   W-DIA GREATER DIA-MAX (WS-MES)
                SUBTRACT DIA-MAX (WS-MES) FROM W-DIA
                ADD      1                  TO WS-MES
                IF   WS-MES EQUAL 13
                     MOVE 1 TO WS-MES
                     ADD  1 TO WS-ANO
                     GO     TO CWADAY-CHECK-ANO
                ELSE
                     GO     TO CWADAY-MAIS-DIA
                END-IF
           ELSE
                MOVE W-DIA TO WS-DIA
           END-IF

           IF   CWTIME-NORMAL
                MOVE WS-ANO     TO WS-ANO2
                MOVE WS-MES     TO WS-MES2
                MOVE WS-DIA     TO WS-DIA2
                MOVE WS-DATE2   TO CWTIME-DATE-FINAL
           ELSE
                MOVE WS-DATE    TO CWTIME-DATE-FINAL
           END-IF.

       FIM-CWADAY. EXIT.

       CWSDAY.

           IF   WS-DIA > CWTIME-DAYS
                COMPUTE W-DIA = WS-DIA - CWTIME-DAYS
                MOVE W-DIA TO WS-DIA
                GO TO CWSDAY-CHECK
           END-IF

           COMPUTE W-DIA = CWTIME-DAYS - WS-DIA.

       CWSDAY-CHECK-MES-ANO.

           COMPUTE WS-MES = WS-MES - 1.
           IF   WS-MES = ZERO
                MOVE 12 TO WS-MES
                SUBTRACT 1 FROM WS-ANO
           END-IF.

       CWSDAY-CHECK-ANO.

           DIVIDE WS-ANO BY 4
                  GIVING W-NADA
                         REMAINDER W-TP-ANO.

           IF   ANO-BISSEXTO
                MOVE 29 TO DIA-MAX (2)
           ELSE
                MOVE 28 TO DIA-MAX (2)
           END-IF.

       CWSDAY-MENOS-DIA.

           IF   W-DIA GREATER DIA-MAX (WS-MES)
                COMPUTE W-DIA = W-DIA - DIA-MAX (WS-MES)
                GO     TO CWSDAY-CHECK-MES-ANO
           END-IF

           COMPUTE WS-DIA = DIA-MAX (WS-MES) - W-DIA

           IF   WS-DIA = 0
                SUBTRACT 1 FROM WS-MES
                IF   WS-MES = 0
                     MOVE     12   TO WS-MES
                     SUBTRACT  1 FROM WS-ANO
                END-IF
                MOVE DIA-MAX (WS-MES) TO WS-DIA
           END-IF.

       CWSDAY-CHECK.

           IF   CWTIME-NORMAL
                MOVE WS-ANO     TO WS-ANO2
                MOVE WS-MES     TO WS-MES2
                MOVE WS-DIA     TO WS-DIA2
                MOVE WS-DATE2   TO CWTIME-DATE-FINAL
           ELSE
                MOVE WS-DATE    TO CWTIME-DATE-FINAL
           END-IF.

       FIM-CWSDAY. EXIT.

       CWDIAS.

           MOVE WS-DATE TO DATE-INICIAL

           IF   NOT CWTIME-WEEK
           AND  CWTIME-NORMAL
                MOVE CWTIME-DATE-FINAL TO WS-DATE2
                MOVE WS-ANO2           TO WS-ANO
                MOVE WS-MES2           TO WS-MES
                MOVE WS-DIA2           TO WS-DIA
                MOVE WS-DATE           TO DATE-FINAL
           ELSE
                MOVE CWTIME-DATE-FINAL TO DATE-FINAL
           END-IF

           IF   DATE-INICIAL = DATE-FINAL
           OR   DATE-INICIAL = ZERO
           OR   DATE-FINAL   = ZERO
                MOVE ZERO TO CWTIME-DAYS-FINAL
                GO TO FIM-CWDIAS
           END-IF

           IF   DATE-FINAL < DATE-INICIAL
                MOVE DATE-FINAL   TO WS-DATE2
                MOVE DATE-INICIAL TO DATE-FINAL
                MOVE WS-DATE2     TO DATE-INICIAL
                MOVE "-"          TO WS-SINAL
           ELSE
                MOVE "+"          TO WS-SINAL
           END-IF

           DIVIDE AA-I BY 4 GIVING LIXO REMAINDER W-TP-ANO

           IF   ANO-BISSEXTO
                MOVE 29 TO DIA-MAX (2)
           ELSE
                MOVE 28 TO DIA-MAX (2)
           END-IF

           MOVE ZERO TO DIAS-1

           COMPUTE DIAS-1 = ((AA-I - 1) / 4) + ((AA-I - 1) * 365) + 366

           PERFORM VARYING I FROM 1 BY 1 UNTIL I EQUAL MM-I
                   ADD DIA-MAX (I) TO DIAS-1
           END-PERFORM

           ADD    DD-I          TO DIAS-1
           DIVIDE AA-F BY 4 GIVING LIXO REMAINDER W-TP-ANO

           IF   ANO-BISSEXTO
                MOVE 29 TO DIA-MAX (2)
           ELSE
                MOVE 28 TO DIA-MAX (2)
           END-IF

           COMPUTE DIAS-2 = ((AA-F - 1) / 4) + ((AA-F - 1) * 365) + 366

           PERFORM VARYING I FROM 1 BY 1 UNTIL I EQUAL MM-F
                   ADD DIA-MAX (I) TO DIAS-2
           END-PERFORM

           ADD DD-F TO DIAS-2

           COMPUTE CWTIME-DAYS-FINAL = DIAS-2 - DIAS-1

           COMPUTE CWTIME-YEARS-OLD = AA-F - AA-I
           EVALUATE TRUE
               WHEN MMDD-F = MMDD-I
                    MOVE 0 TO CWTIME-MOUNTHS-OLD
                              CWTIME-DAYS-OLD
               WHEN MMDD-F < MMDD-I
                    IF   CWTIME-YEARS-OLD NOT = 0
                         SUBTRACT 1 FROM CWTIME-YEARS-OLD
                    END-IF
                    COMPUTE CWTIME-MOUNTHS-OLD = 12 - MM-I + MM-F
                    EVALUATE TRUE
                        WHEN DD-F = DD-I
                             MOVE 0 TO CWTIME-DAYS-OLD
                        WHEN DD-F > DD-I
                             COMPUTE CWTIME-DAYS-OLD = DD-F - DD-I
                        WHEN DD-F < DD-I
                             SUBTRACT 1 FROM CWTIME-MOUNTHS-OLD
                             COMPUTE CWTIME-DAYS-OLD
                                   = DIA-MAX (MM-I)
                                   - DD-I
                                   + DD-F
                    END-EVALUATE
               WHEN MMDD-F > MMDD-I
                    COMPUTE CWTIME-MOUNTHS-OLD = MM-F - MM-I
                    EVALUATE TRUE
                        WHEN DD-F = DD-I
                             MOVE 0 TO CWTIME-DAYS-OLD
                        WHEN DD-F > DD-I
                             COMPUTE CWTIME-DAYS-OLD = DD-F - DD-I
                        WHEN DD-F < DD-I
                             SUBTRACT 1 FROM CWTIME-MOUNTHS-OLD
                             COMPUTE CWTIME-DAYS-OLD
                                   = DIA-MAX (MM-I)
                                   - DD-I
                                   + DD-F
                    END-EVALUATE
           END-EVALUATE

           IF   WS-SINAL = "-"
                COMPUTE CWTIME-DAYS-FINAL  = CWTIME-DAYS-FINAL  * -1
                COMPUTE CWTIME-YEARS-OLD   = CWTIME-YEARS-OLD   * -1
                COMPUTE CWTIME-MOUNTHS-OLD = CWTIME-MOUNTHS-OLD * -1
                COMPUTE CWTIME-DAYS-OLD    = CWTIME-DAYS-OLD    * -1
           END-IF.

       FIM-CWDIAS. EXIT.
       END PROGRAM CWTIME.
