       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWCLOG.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  23/02/1998.
       SECURITY.      *************************************************
                      *                                               *
                      *  Grava log de extra‡Æo e carga                *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT USELOG ASSIGN TO DISK
                  ORGANIZATION  IS LINE SEQUENTIAL
                  LOCK MODE IS EXCLUSIVE
                  FILE STATUS   IS FS-USELOG.

       DATA DIVISION.
       FILE SECTION.

       FD  USELOG
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-USELOG.

       01  USELOG-REG.
           05 USELOG-DATA-DE-HOJE       PIC  X(009).
           05 USELOG-HORA               PIC  X(009).
           05 USELOG-OBS                PIC  X(5000).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 PROGRAMA                PIC  X(008) VALUE "GRMENU".
           05 NOME                    PIC  X(030) VALUE "LOGON".
           05 DATA-DE-HOJE            PIC  X(008) VALUE SPACES.
           05 HORA                    PIC  X(008) VALUE SPACES.
           05 ER-USELOG.
              10 FS-USELOG             PIC  X(002) VALUE "00".
              10 LB-USELOG             PIC  X(255) VALUE "cwextcar.log".

       LINKAGE SECTION.

       01  FUNCAO                            PIC  X(001).
       01  OBS                               PIC  X(5000).

       PROCEDURE DIVISION USING FUNCAO OBS.

       000-INICIO.

           DISPLAY 'CWEXTCAR' UPON ENVIRONMENT-NAME
           ACCEPT LB-USELOG FROM ENVIRONMENT-VALUE
           CALL "GRDATA"   USING DATA-DE-HOJE HORA.

       RETRY.


           OPEN EXTEND USELOG

           IF   FS-USELOG = "00"
           OR   FS-USELOG = "05"
                CALL "GRDATA"     USING DATA-DE-HOJE HORA
                MOVE DATA-DE-HOJE    TO USELOG-DATA-DE-HOJE
                MOVE HORA            TO USELOG-HORA
                MOVE OBS             TO USELOG-OBS
                WRITE USELOG-REG
           ELSE
                GO TO RETRY
           END-IF

           CLOSE USELOG.

       000-99-FIM. EXIT PROGRAM.
       COPY GRDATA.cbl.
       END PROGRAM CWCLOG.
