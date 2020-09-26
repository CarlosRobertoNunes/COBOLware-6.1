       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMOLD.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  03/04/1996.
       SECURITY.      *************************************************
                      *                                               *
                      *  Extrai/Moldura e cores do Unix               *
                      *                                               *
                      *  Input BOXCOLOR                               *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT BOXCOLOR ASSIGN TO DISK
                  ORGANIZATION  IS RELATIVE
                  RELATIVE KEY  IS RK-BOXCOLOR
                  ACCESS MODE   IS RANDOM
                  RESERVE NO ALTERNATE AREA
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-BOXCOLOR.

       DATA DIVISION.
       FILE SECTION.

       FD  BOXCOLOR
           VALUE OF FILE-ID LB-BOXCOLOR.

       01  BOXCOLOR-REG PIC X(256).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 COBWARE                  PIC  X(030) VALUE SPACES.
           05 TERM                     PIC  X(030) VALUE SPACES.
           05 MSG                      PIC  X(080) VALUE SPACES.
           05 MOLDURAS                 PIC  X(072) VALUE SPACES.
           05 CORES.
              10 COR OCCURS 256        PIC  9(002) COMP-X.
           05 ER-BOXCOLOR.
              10 FS-BOXCOLOR           PIC  X(002) VALUE "00".
              10 LB-BOXCOLOR           PIC  X(255) VALUE SPACES.
           05 RK-BOXCOLOR              PIC  9(002) VALUE 1.
           05 I                        PIC  9(003) VALUE 0.
           05 Y                        PIC  9(003) VALUE 0.
           05 X91-RESULT        COMP-X PIC  9(002) VALUE 0.
           05 X91-FUNCTION      COMP-X PIC  9(002) VALUE 16.
           05 X91-PARAMETER     COMP-X PIC  9(002) VALUE 0.
           05 MOLDURAS-DEFAULT.
              10 FILLER PIC X(8) VALUE X"C9CDBBBACCB9C8BC".
              10 FILLER PIC X(8) VALUE X"DAC4BFB3C3B4C0D9".
              10 FILLER PIC X(8) VALUE X"D5CDB8B3C6B5D4BE".
              10 FILLER PIC X(8) VALUE X"D6C4B7BAC7B6D3BD".
              10 FILLER PIC X(8) VALUE ALL X"B0".
              10 FILLER PIC X(8) VALUE ALL X"B1".
              10 FILLER PIC X(8) VALUE ALL X"B2".
              10 FILLER PIC X(8) VALUE "---|----".
              10 FILLER PIC X(8) VALUE "        ".

       COPY CWUNIX.

       LINKAGE SECTION.

       01  TABELA-CORES    PIC X(256).
       01  TABELA-MOLDURA  PIC X(072).

       PROCEDURE DIVISION USING TABELA-CORES TABELA-MOLDURA.

       000-INICIO.

           ON 1 CALL "CWUNIX" USING PARAMETROS-CWUNIX.
           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER

           IF   X91-PARAMETER = 0
                GOBACK
           END-IF

           IF   MOLDURAS = SPACES
                DISPLAY "COBOLWARE" UPON ENVIRONMENT-NAME
                ACCEPT  COBWARE     FROM ENVIRONMENT-VALUE
                DISPLAY "TERM"      UPON ENVIRONMENT-NAME
                ACCEPT  TERM        FROM ENVIRONMENT-VALUE
                IF   TERM = SPACES
                     MOVE "dos" TO TERM
                     IF  CWUNIX-WINDOWS
                         MOVE "win" TO TERM
                     END-IF
                END-IF
                STRING COBWARE      DELIMITED BY SPACE
                       "/boxcolor." DELIMITED BY SIZE
                       TERM         DELIMITED BY SPACE
                              INTO LB-BOXCOLOR
                OPEN INPUT BOXCOLOR
                IF   FS-BOXCOLOR < "10"
                     MOVE 1 TO RK-BOXCOLOR
                     READ BOXCOLOR INTO CORES
                     MOVE 2 TO RK-BOXCOLOR
                     READ BOXCOLOR INTO MOLDURAS
                     CLOSE BOXCOLOR
                ELSE
                     MOVE MOLDURAS-DEFAULT TO MOLDURAS
                     MOVE 0                TO I
                     PERFORM 255 TIMES
                             COMPUTE Y = I + 1
                             MOVE I TO COR (Y)
                             ADD  1 TO I
                     END-PERFORM
                END-IF
           END-IF

           MOVE MOLDURAS TO TABELA-MOLDURA
           MOVE CORES    TO TABELA-CORES.

       000-99-FIM. GOBACK.

       END PROGRAM CWMOLD.
