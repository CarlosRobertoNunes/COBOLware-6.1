       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWTEXT.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  25/01/2001.
       SECURITY.      *************************************************
                      *                                               *
                      *  Adapta acentos de acordo com a configuracao  *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *    SELECT BOXCOLOR ASSIGN TO DISK
      *           ORGANIZATION  IS RELATIVE
      *           RELATIVE KEY  IS RK-BOXCOLOR
      *           ACCESS MODE   IS RANDOM
      *           RESERVE NO ALTERNATE AREA
070519*           LOCK MODE     IS EXCLUSIVE
      *           FILE STATUS   IS FS-BOXCOLOR.
      *
       DATA DIVISION.
       FILE SECTION.

      *FD  BOXCOLOR
      *    VALUE OF FILE-ID LB-BOXCOLOR.
      *
      *01  BOXCOLOR-REG PIC X(256).
      *
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 FL-SAVE3                PIC X(001) VALUE "0".
      *    05 RK-BOXCOLOR             PIC 9(002) VALUE 1.
      *    05 ER-BOXCOLOR.
      *       10 FS-BOXCOLOR          PIC X(002) VALUE "00".
      *       10 LB-BOXCOLOR          PIC X(255) VALUE SPACES.
           05 COBWARE                 PIC X(030) VALUE SPACES.
           05 TERM                    PIC X(030) VALUE SPACES.
           05 ACENTOS                 PIC X(058) VALUE SPACES.

       COPY CWUNIX.

       LINKAGE SECTION.

       01  TXT             PIC X(32768).
       01  LEN             PIC 9(008) COMP-X.

       PROCEDURE DIVISION USING TXT LEN.

       000-INICIO.

           IF   LEN > 32768
                MOVE 32768 TO LEN
           END-IF

           ON   1
                MOVE ACENTOS-850 TO ACENTOS
                CALL "CWUNIX" USING PARAMETROS-CWUNIX
                DISPLAY "COBOLWARE" UPON ENVIRONMENT-NAME
                ACCEPT  COBWARE   FROM ENVIRONMENT-VALUE
                DISPLAY "TERM"      UPON ENVIRONMENT-NAME
                ACCEPT  TERM        FROM ENVIRONMENT-VALUE
                IF   TERM = SPACES
                     MOVE "dos" TO TERM
                     IF  CWUNIX-WINDOWS
                         MOVE "win" TO TERM
                     END-IF
                END-IF
      *         STRING COBWARE    DELIMITED BY SPACE
      *                "/boxcolor." DELIMITED BY SIZE
      *                TERM         DELIMITED BY SPACE
      *                       INTO LB-BOXCOLOR
      *         OPEN INPUT BOXCOLOR
      *         IF    FS-BOXCOLOR < "10"
      *               MOVE 3 TO RK-BOXCOLOR
      *               READ BOXCOLOR
      *               IF  FS-BOXCOLOR < "10"
      *                   MOVE "1" TO FL-SAVE3
      *                   MOVE BOXCOLOR-REG TO ACENTOS
      *               END-IF
      *               CLOSE BOXCOLOR
      *         END-IF
                IF   FL-SAVE3 = "0"
                     IF   CWUNIX-ON
                     AND  TERM NOT = "linux"
                          MOVE ACENTOS-OFF TO ACENTOS
                     END-IF
                     IF   CWUNIX-WINDOWS
                          MOVE ACENTOS-437 TO ACENTOS
                     END-IF
                END-IF.

           IF   ACENTOS NOT = ACENTOS-850
                INSPECT TXT (1: LEN)
                        CONVERTING ACENTOS-850 TO ACENTOS
           END-IF.

       000-99-FIM. GOBACK.

       END PROGRAM CWTEXT.
