       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWGETL.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  22/01/2001.
       SECURITY.      *************************************************
                      *                                               *
                      *   Optem opcoes de suporte                     *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1. COPY CWCASE.
           05 MULTI-USER               PIC  9(001) VALUE 0.
           05 TMP                      PIC  X(003) VALUE SPACE.
           05 WINDIR                   PIC  X(050) VALUE SPACES.

       COPY CWUNIX.
       COPY CWCONF.

       LINKAGE SECTION.

       01  PARAMETROS-CWGETL.
           05 CWGETL-MASTER     PIC  X(008).
           05 CWGETL-END        PIC  X(008).
           05 CWGETL-LOGIN      PIC  X(008).
           05 CWGETL-LOGOUT     PIC  X(008).
           05 CWGETL-CALLIN     PIC  X(008).
           05 CWGETL-CALLOUT    PIC  X(008).
           05 CWGETL-HELPDIR-D  PIC  X(050).
           05 CWGETL-HELPDIR-U  PIC  X(050).
           05 CWGETL-TIMEOUT    PIC  9(004).
           05 CWGETL-AUTOPASS   PIC  9(001).
           05 CWGETL-CLOCK      PIC  9(001).
           05 CWGETL-LOG        PIC  9(001).
           05 CWGETL-MOUSE      PIC  9(001).
           05 CWGETL-SPOOL-ROLL PIC  9(001).
           05 CWGETL-HIGH       PIC  9(001).
           05 CWGETL-SPOOL      PIC  9(001).
           05 CWGETL-CODELOG    PIC  9(001).
           05 CWGETL-PRTPOSIT   PIC  9(001).
           05 CWGETL-RETRY      PIC  9(004).
           05 FILLER            PIC  9(002).
           05 CWGETL-LOGDIR     PIC  X(040).

       PROCEDURE DIVISION USING PARAMETROS-CWGETL.

       000-INICIO.

           ON   1
                DISPLAY "CWMULTI"      UPON ENVIRONMENT-NAME
                ACCEPT  TMP            FROM ENVIRONMENT-VALUE
                INSPECT TMP (1: 2) CONVERTING MINUSCULAS TO MAIUSCULAS
                IF   TMP (1: 2) = "ON"
                     MOVE 1 TO MULTI-USER
                END-IF
                DISPLAY "CWIN"   UPON ENVIRONMENT-NAME
                ACCEPT   WINDIR  FROM ENVIRONMENT-VALUE
                CALL "CWUNIX" USING PARAMETROS-CWUNIX
                IF   CWUNIX-ON
                     MOVE 1 TO MULTI-USER
                END-IF
                SET CWSQLC-OPEN TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                MOVE "LG"              TO CWCONF-CHAVE
                SET CWSQLC-READ        TO TRUE
                SET CWSQLC-IGNORE-LOCK TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                IF   FS-CWCONF > "09"
                     MOVE "LG"       TO CWCONF-REGLG
                     INITIALIZE CWCONF-OPCOES-LOG
                     MOVE 1          TO CWCONF-LOG
                     MOVE 0          TO CWCONF-AUTOPASS
                     MOVE 1          TO CWCONF-CLOCK
                     MOVE 1          TO CWCONF-MOUSE
                     MOVE 1          TO CWCONF-DIR
                     MOVE 3          TO CWCONF-RETRY
                     MOVE 60         TO CWCONF-TIMEOUT
                     MOVE ALL "1"    TO CWCONF-FILLER (1: )
                     MOVE "."        TO CWCONF-LOGDIR
                     MOVE 1          TO CWCONF-PRTPOSIT
                END-IF
                IF ( CWCONF-RETRY NOT NUMERIC )
                OR   CWCONF-RETRY = 1111
                     MOVE 3          TO CWCONF-RETRY
                END-IF
                IF   CWCONF-LOGDIR = LOW-VALUES
                     MOVE "." TO CWCONF-LOGDIR
                     MOVE 0   TO CWCONF-CODELOG
                END-IF
                IF   CWCONF-LOGDIR = SPACE
                     MOVE "." TO CWCONF-LOGDIR
                END-IF
                SET CWSQLC-CLOSE TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG
                                    FS-CWCONF KCO PCO.

           MOVE CWCONF-OPCOES-LOG TO PARAMETROS-CWGETL
           IF   CWGETL-SPOOL NOT = "2"
                MOVE "1" TO CWGETL-SPOOL
           END-IF.

      *    IF   CWUNIX-ON
      *    OR   CWUNIX-WIN32
      *    OR  (WINDIR NOT = SPACES)
      *         MOVE "2" TO CWGETL-CLOCK
      *    END-IF
           IF   MULTI-USER = 1
                MOVE "2" TO CWGETL-MOUSE
                MOVE 0   TO CWGETL-TIMEOUT
           END-IF.
      *
       000-99-FIM. GOBACK.
       END PROGRAM CWGETL.
