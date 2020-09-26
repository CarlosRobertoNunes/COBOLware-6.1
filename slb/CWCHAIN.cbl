       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWCHAIN.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  02/01/2008.
       SECURITY.      *************************************************
                      *                                               *
                      *  Suporte a simula‡Æo do comando CHAIN         *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA
                      CALL-CONVENTION 66 IS WIN32.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           04 X91-RESULT    COMP-X     PIC  9(002) VALUE 0.
           04 X91-FUNCTION  COMP-X     PIC  9(002) VALUE 16.
           04 X91-PARAMETER COMP-X     PIC  9(002) VALUE 0.
           04 WORKS.
              05 WLENS.
                 06 WLEN01                PIC  9(008) COMP-X.
                 06 WLEN02                PIC  9(008) COMP-X.
                 06 WLEN03                PIC  9(008) COMP-X.
                 06 WLEN04                PIC  9(008) COMP-X.
                 06 WLEN05                PIC  9(008) COMP-X.
                 06 WLEN06                PIC  9(008) COMP-X.
                 06 WLEN07                PIC  9(008) COMP-X.
                 06 WLEN08                PIC  9(008) COMP-X.
                 06 WLEN09                PIC  9(008) COMP-X.
                 06 WLEN10                PIC  9(008) COMP-X.
              05 WORK01. 06 PIC X OCCURS 0 TO 32767 DEPENDING ON WLEN01.
              05 WORK02. 06 PIC X OCCURS 0 TO 32767 DEPENDING ON WLEN02.
              05 WORK03. 06 PIC X OCCURS 0 TO 32767 DEPENDING ON WLEN03.
              05 WORK04. 06 PIC X OCCURS 0 TO 32767 DEPENDING ON WLEN04.
              05 WORK05. 06 PIC X OCCURS 0 TO 32767 DEPENDING ON WLEN05.
              05 WORK06. 06 PIC X OCCURS 0 TO 32767 DEPENDING ON WLEN06.
              05 WORK07. 06 PIC X OCCURS 0 TO 32767 DEPENDING ON WLEN07.
              05 WORK08. 06 PIC X OCCURS 0 TO 32767 DEPENDING ON WLEN08.
              05 WORK09. 06 PIC X OCCURS 0 TO 32767 DEPENDING ON WLEN09.
              05 WORK10. 06 PIC X OCCURS 0 TO 32767 DEPENDING ON WLEN10.

       01  AREAS-DE-TRABALHO-2.
           05 I                        PIC  9(006) VALUE 0.
           05 TASK                     PIC  9(006) VALUE 0.
           05 PROGRAMA                 PIC  X(008) VALUE SPACES.
           05 CWPGM                    PIC  X(050) VALUE SPACES.
           05 CWPGM2                   PIC  X(050) VALUE SPACES.
           05 NOME                     PIC  X(030) VALUE SPACES.
           05 SET-LOG                  PIC  X(001) VALUE SPACE.
           05 DATA-DE-HOJE             PIC  X(010) VALUE SPACES.
           05 HORA                     PIC  X(008) VALUE SPACES.
           05 CWLOGP                   PIC  X(008) VALUE SPACES.
           05 FATOR-W           COMP-X PIC  9(002) VALUE 0.
           05 HOJE                     PIC  9(008) VALUE 0.
           05 funcao-PROGRAMA          PIC  X(035) VALUE SPACES.

       COPY CWGETL.
       COPY CWTIME.

       LINKAGE SECTION.

       01  OPT                      PIC X(0003).
       01  COMMAREA01               PIC  X(001).
       01  LEN01                    PIC  9(008) COMP-X.
       01  COMMAREA02               PIC  X(001).
       01  LEN02                    PIC  9(008) COMP-X.
       01  COMMAREA03               PIC  X(001).
       01  LEN03                    PIC  9(008) COMP-X.
       01  COMMAREA04               PIC  X(001).
       01  LEN04                    PIC  9(008) COMP-X.
       01  COMMAREA05               PIC  X(001).
       01  LEN05                    PIC  9(008) COMP-X.
       01  COMMAREA06               PIC  X(001).
       01  LEN06                    PIC  9(008) COMP-X.
       01  COMMAREA07               PIC  X(001).
       01  LEN07                    PIC  9(008) COMP-X.
       01  COMMAREA08               PIC  X(001).
       01  LEN08                    PIC  9(008) COMP-X.
       01  COMMAREA09               PIC  X(001).
       01  LEN09                    PIC  9(008) COMP-X.
       01  COMMAREA10               PIC  X(001).
       01  LEN10                    PIC  9(008) COMP-X.

       PROCEDURE DIVISION USING BY VALUE OPT
                   BY REFERENCE COMMAREA01 BY REFERENCE LEN01
                   BY REFERENCE COMMAREA02 BY REFERENCE LEN02
                   BY REFERENCE COMMAREA03 BY REFERENCE LEN03
                   BY REFERENCE COMMAREA04 BY REFERENCE LEN04
                   BY REFERENCE COMMAREA05 BY REFERENCE LEN05
                   BY REFERENCE COMMAREA06 BY REFERENCE LEN06
                   BY REFERENCE COMMAREA07 BY REFERENCE LEN07
                   BY REFERENCE COMMAREA08 BY REFERENCE LEN08
                   BY REFERENCE COMMAREA09 BY REFERENCE LEN09
                   BY REFERENCE COMMAREA10 BY REFERENCE LEN10.

       000-INICIO.

           ON 1
              DISPLAY "CWLOGP"     UPON ENVIRONMENT-NAME
              ACCEPT CWLOGP        FROM ENVIRONMENT-VALUE.
           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER
           IF   X91-PARAMETER = 0
                GOBACK
           ELSE
                CALL "CWGETL" USING PARAMETROS-CWGETL
                CALL "CWGETU" USING NOME TASK PROGRAMA "?"
                IF  OPT = "SET" OR "TES"
                    EXEC COBOLware CloseAllFiles END-EXEC
                    DISPLAY "CWCHAIN" UPON ENVIRONMENT-NAME
                    ACCEPT  CWPGM FROM ENVIRONMENT-VALUE
                    INSPECT CWPGM CONVERTING MINUSCULAS TO MAIUSCULAS
                    INSPECT CWPGM CONVERTING '.'        TO SPACE
                    MOVE SPACE TO CWPGM2
                    STRING CWPGM DELIMITED BY SPACE INTO CWPGM2
                    INSPECT CWPGM2 CONVERTING '/\:'     TO SPACE
                    PERFORM VARYING I FROM LENGTH OF CWPGM2
                            BY -1 UNTIL I = 1
                                  OR (CWPGM2(I:1) NOT = SPACE)
                            CONTINUE
                    END-PERFORM
                    PERFORM VARYING I FROM I
                            BY -1 UNTIL I = 1
                                  OR CWPGM2(I:1) = SPACE
                            CONTINUE
                    END-PERFORM
                    IF CWPGM2(I:1) = SPACE
                       ADD 1 TO I
                    END-IF
                    MOVE CWPGM2(I:) TO CWPGM
                    DISPLAY CWPGM UPON ENVIRONMENT-VALUE
                    IF X91-PARAMETER > 2
                       MOVE LEN01                 TO WLEN01
                       MOVE COMMAREA01 (1: LEN01) TO WORK01
                    END-IF
                    IF X91-PARAMETER > 4
                       MOVE LEN02                 TO WLEN02
                       MOVE COMMAREA02 (1: LEN02) TO WORK02
                    END-IF
                    IF X91-PARAMETER > 6
                       MOVE LEN03                 TO WLEN03
                       MOVE COMMAREA03 (1: LEN03) TO WORK03
                    END-IF
                    IF X91-PARAMETER > 8
                       MOVE LEN04                 TO WLEN04
                       MOVE COMMAREA04 (1: LEN04) TO WORK04
                    END-IF
                    IF X91-PARAMETER > 10
                       MOVE LEN05                 TO WLEN05
                       MOVE COMMAREA05 (1: LEN05) TO WORK05
                    END-IF
                    IF X91-PARAMETER > 12
                       MOVE LEN06                 TO WLEN06
                       MOVE COMMAREA06 (1: LEN06) TO WORK06
                    END-IF
                    IF X91-PARAMETER > 14
                       MOVE LEN07                 TO WLEN07
                       MOVE COMMAREA07 (1: LEN07) TO WORK07
                    END-IF
                    IF X91-PARAMETER > 16
                       MOVE LEN08                 TO WLEN08
                       MOVE COMMAREA08 (1: LEN08) TO WORK08
                    END-IF
                    IF X91-PARAMETER > 18
                       MOVE LEN09                 TO WLEN09
                       MOVE COMMAREA09 (1: LEN09) TO WORK09
                    END-IF
                    IF X91-PARAMETER > 20
                       MOVE LEN10                 TO WLEN10
                       MOVE COMMAREA10 (1: LEN10) TO WORK10
                    END-IF
                    MOVE "Processamento encerrado por CHAIN"
                      TO FUNCAO-PROGRAMA
                    PERFORM 130-GRAVA-CWLOGF THRU 130-99-FIM
                END-IF
                IF  OPT = "GET" OR "TEG"
                    IF X91-PARAMETER > 2
                       IF LEN01 = 0
                       OR LEN01 > WLEN01
                          MOVE WLEN01 TO LEN01
                       END-IF
                       MOVE WORK01 (1: LEN01) TO COMMAREA01 (1: LEN01)
                    END-IF
                    IF X91-PARAMETER > 4
                       IF LEN02 = 0
                       OR LEN02 > WLEN02
                          MOVE WLEN02 TO LEN02
                       END-IF
                       MOVE WORK02 (1: LEN02) TO COMMAREA02 (1: LEN02)
                    END-IF
                    IF X91-PARAMETER > 6
                       IF LEN03 = 0
                       OR LEN03 > WLEN03
                          MOVE WLEN03 TO LEN03
                       END-IF
                       MOVE WORK03 (1: LEN03) TO COMMAREA03 (1: LEN03)
                    END-IF
                    IF X91-PARAMETER > 8
                       IF LEN04 = 0
                       OR LEN04 > WLEN04
                          MOVE WLEN04 TO LEN04
                       END-IF
                       MOVE WORK04 (1: LEN04) TO COMMAREA04 (1: LEN04)
                    END-IF
                    IF X91-PARAMETER > 10
                       IF LEN05 = 0
                       OR LEN05 > WLEN05
                          MOVE WLEN05 TO LEN05
                       END-IF
                       MOVE WORK05 (1: LEN05) TO COMMAREA05 (1: LEN05)
                    END-IF
                    IF X91-PARAMETER > 12
                       IF LEN06 = 0
                       OR LEN06 > WLEN06
                          MOVE WLEN06 TO LEN06
                       END-IF
                       MOVE WORK06 (1: LEN06) TO COMMAREA06 (1: LEN06)
                    END-IF
                    IF X91-PARAMETER > 14
                       IF LEN07 = 0
                       OR LEN07 > WLEN07
                          MOVE WLEN07 TO LEN07
                       END-IF
                       MOVE WORK07 (1: LEN07) TO COMMAREA07 (1: LEN07)
                    END-IF
                    IF X91-PARAMETER > 16
                       IF LEN08 = 0
                       OR LEN08 > WLEN08
                          MOVE WLEN08 TO LEN08
                       END-IF
                       MOVE WORK08 (1: LEN08) TO COMMAREA08 (1: LEN08)
                    END-IF
                    IF X91-PARAMETER > 18
                       IF LEN09 = 0
                       OR LEN09 > WLEN09
                          MOVE WLEN09 TO LEN09
                       END-IF
                       MOVE WORK09 (1: LEN09) TO COMMAREA09 (1: LEN09)
                    END-IF
                    IF X91-PARAMETER > 20
                       IF LEN10 = 0
                       OR LEN10 > WLEN10
                          MOVE WLEN10 TO LEN10
                       END-IF
                       MOVE WORK10 (1: LEN10) TO COMMAREA10 (1: LEN10)
                    END-IF
                    DISPLAY "CWCHAIN" UPON ENVIRONMENT-NAME
                    ACCEPT   PROGRAMA FROM ENVIRONMENT-VALUE
                    MOVE "Chamada via CHAIN" TO funcao-PROGRAMA
                    MOVE  CWGETL-LOG TO SET-LOG
                    CALL "CWGETU" USING NOME TASK PROGRAMA SET-LOG
                    PERFORM 130-GRAVA-CWLOGF THRU 130-99-FIM
                    INITIALIZE WORKS
                END-IF
           END-IF.

       000-99-FIM. GOBACK.

       130-GRAVA-CWLOGF.

           DISPLAY "CWMENUPGM" UPON ENVIRONMENT-NAME
           DISPLAY  PROGRAMA   UPON ENVIRONMENT-VALUE
           CALL "CWPGIR" USING PROGRAMA
           CALL "CWGETU" USING NOME TASK PROGRAMA "#"
           CALL "CWLOGW" USING "#" FUNCAO-PROGRAMA.

       130-99-FIM. EXIT.

       END PROGRAM CWCHAIN.
