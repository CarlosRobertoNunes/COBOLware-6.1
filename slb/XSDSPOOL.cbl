       IDENTIFICATION DIVISION.
       PROGRAM-ID.    XSDSPOOL INITIAL.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  09/09/2017.
       SECURITY.      *************************************************
                      *                                               *
                      *  Suporte a programas gerados pelo XSEED       *
                      *  Descarrega arquivo de SPOOL para PRINTER     *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

            SELECT XSEED-REP ASSIGN TO DISK
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS  IS GLB-STATUSCODE.

       DATA DIVISION.
       FILE SECTION.

       FD  XSEED-REP IS GLOBAL
           VALUE OF FILE-ID GLB-WTITLE.

       01  XSEED-REP-REG PIC X(260).

       WORKING-STORAGE SECTION.

       01  PARAMETROS-IMPRIME IS GLOBAL.
           05                 PIC  X(001) VALUE SPACES.
              88 IMPRIME-OPEN   VALUE 'O'.
              88 IMPRIME-WRITE  VALUE 'W'.
              88 IMPRIME-CLOSE  VALUE 'C'.
           05 FS-PRNTER       PIC  X(002) VALUE "00".
           05 LB-PRNTER       PIC  X(255) VALUE SPACES.
           05 LINHAS          PIC  9(018) VALUE ZERO.
           05 SALTAR          PIC  9(001) VALUE 0.
           05 PAGEMARK        PIC  X(003) VALUE SPACES.

       COPY CWUNIX.
       COPY CWEXEC.
       COPY CWCONF.

       LINKAGE SECTION.

       01  GLB-WTITLE         PIC  X(060).
       01  GLB-WSTATION       PIC  X(017).
       01  GLB-PAGEMARK       PIC  X(003).
       01  GLB-PAGESKIP       PIC  X(001).
       01  GLB-LSN            PIC  X(005).
       01  GLB-INITSTN        PIC  X(017).
       01  GLB-STATUSCODE.
           05 GLB-STATUSCHAR  PIC  X(001).
           05 GLB-STATUSSCAN  PIC  9(002) COMP-5.

       PROCEDURE DIVISION USING GLB-WTITLE
                                GLB-WSTATION
                                GLB-PAGEMARK
                                GLB-PAGESKIP
                                GLB-LSN
                                GLB-INITSTN
                                GLB-STATUSCODE.

       000-INICIO.

           PERFORM 800-INICIAIS      THRU 800-99-FIM
           PERFORM 100-PROCESSAMENTO THRU 100-99-FIM
           PERFORM 900-FINAIS        THRU 900-99-FIM.

       000-99-FIM. GOBACK.

       100-PROCESSAMENTO.

           PERFORM UNTIL GLB-STATUSCODE > "09"
                      OR FS-PRNTER > '00'
              READ XSEED-REP
                   AT END
                      CONTINUE
                   NOT AT END
                       CALL 'IMPRIME'
              END-READ
           END-PERFORM

           MOVE "00" TO GLB-STATUSCODE.

       100-99-FIM. EXIT.

       800-INICIAIS.

           MOVE GLB-PAGEMARK TO PAGEMARK
           IF  PAGEMARK = SPACES OR LOW-VALUES
               MOVE '.PA' TO PAGEMARK
           END-IF

           OPEN INPUT XSEED-REP

           IF   GLB-STATUSCODE > "00"
                GOBACK
           END-IF

           IF   GLB-WSTATION NOT = SPACES
                DISPLAY 'XSEED-PRINTER' UPON ENVIRONMENT-NAME
                DISPLAY GLB-WSTATION    UPON ENVIRONMENT-VALUE
           END-IF

           CALL "CWUNIX" USING PARAMETROS-CWUNIX
           IF   CWUNIX-ON
                MOVE "/dev/lp0" TO LB-PRNTER
           ELSE
                MOVE "LPT1:"    TO LB-PRNTER
           END-IF

           SET IMPRIME-OPEN TO TRUE
           CALL 'IMPRIME'.

       800-99-FIM. EXIT.

       900-FINAIS.

           CLOSE XSEED-REP
           IF   FS-PRNTER < '10'
                SET IMPRIME-CLOSE TO TRUE
                CALL "IMPRIME"
           END-IF

           IF   GLB-WSTATION NOT = SPACES
                DISPLAY 'XSEED-PRINTER' UPON ENVIRONMENT-NAME
                DISPLAY SPACES          UPON ENVIRONMENT-VALUE
           END-IF.

       900-99-FIM. EXIT.

      $Set CALLFH"CWSQLC"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IMPRIME.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

            SELECT PRNTER ASSIGN TO DISK
                   ORGANIZATION IS LINE SEQUENTIAL
                   FILE STATUS IS FS-PRNTER.

       DATA DIVISION.
       FILE SECTION.

       FD  PRNTER IS GLOBAL
           VALUE OF FILE-ID LB-PRNTER.

       01  PRNTER-REG PIC X(260).

       WORKING-STORAGE SECTION.

       PROCEDURE DIVISION.

           EVALUATE TRUE
                WHEN IMPRIME-OPEN
                     CALL "CWSQLC" USING "S"
                     OPEN OUTPUT PRNTER
                     SET IMPRIME-WRITE TO TRUE
                WHEN IMPRIME-WRITE
                     IF  SALTAR = 1
                         MOVE XSEED-REP-REG TO PRNTER-REG
                         ADD 1 TO LINHAS
                         WRITE PRNTER-REG AFTER PAGE
                         MOVE 0 TO SALTAR
                     ELSE
                         IF  XSEED-REP-REG = PAGEMARK
                             MOVE 1 TO SALTAR
                         ELSE
                             MOVE XSEED-REP-REG TO PRNTER-REG
                             ADD 1 TO LINHAS
                             IF   LINHAS = 1
                                  WRITE PRNTER-REG AFTER 0
                             ELSE
                                  WRITE PRNTER-REG
                             END-IF
                         END-IF
                     END-IF
                WHEN IMPRIME-CLOSE
                     CLOSE PRNTER
           END-EVALUATE.

       END PROGRAM IMPRIME.
       END PROGRAM XSDSPOOL.
