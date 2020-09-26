      $SET CALLFH"EXTFH"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWSQLS.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  06/05/2014.
       SECURITY.      *************************************************
                      *                                               *
                      *  Exibe "peso" do acesso a SQL via CWSQLC      *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT stat ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS stat-chave
                  ALTERNATE KEY IS stat-TOP =
                                   stat-TOTAL-TIME
                                   stat-HANDLER
                                   WITH DUPLICATES
                  FILE STATUS   IS FS-stat.

       DATA DIVISION.
       FILE SECTION.

       FD  stat
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-STAT.

       01  stat-REG.
           05 stat-CHAVE.
              10 stat-HANDLER    PIC X(015) VALUE SPACES.
           05 stat-DADOS.
              10 stat-TOTAL-TIME PIC 9(012) COMP-3.
              10 stat-OPEN       PIC 9(018) COMP-3.
              10 stat-CLOSE      PIC 9(018) COMP-3.
              10 stat-START      PIC 9(018) COMP-3.
              10 stat-READ       PIC 9(018) COMP-3.
              10 stat-NEXT       PIC 9(018) COMP-3.
              10 stat-PREVIOUS   PIC 9(018) COMP-3.
              10 stat-WRITE      PIC 9(018) COMP-3.
              10 stat-REWRITE    PIC 9(018) COMP-3.
              10 stat-DELETE     PIC 9(018) COMP-3.
              10 stat-OTHER      PIC 9(018) COMP-3.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.  COPY CWCASE.
           05 TECLA               PIC 9(003) VALUE 0. COPY CWKEYS.
           05 PERC                PIC 9(003)V9 VALUE 0.
           05 LL                  PIC 9(003) VALUE 0.
           05 TOTAL-TIME          PIC 9(012) VALUE 0 COMP-3.
           05 CTAC-TIME           PIC X(010) VALUE SPACES.
           05 CTAC-READ           PIC 9(018) VALUE 0.
           05 LIXO                PIC X(001) VALUE SPACE.
           05 FS-stat             PIC  X(002) VALUE "00".
           05 LB-STAT             PIC  X(255) VALUE SPACES.
           05 flag-stat           PIC  X(003) VALUE SPACES.

       SCREEN SECTION.

       01  CTAC-LIT-CWSQLS.
           05 LINE 01 COLUMN 01 VALUE "Peso".
           05 LINE 01 COLUMN 08 VALUE "%".
           05 LINE 01 COLUMN 10 VALUE "Handler".
           05 LINE 01 COLUMN 27 VALUE "Read".
           05 LINE 01 COLUMN 37 VALUE "Write".
           05 LINE 01 COLUMN 43 VALUE "Rewrite".
           05 LINE 01 COLUMN 51 VALUE "Delete".
           05 LINE 01 COLUMN 59 VALUE "Start".
           05 LINE 01 COLUMN 66 VALUE "Open".
           05 LINE 01 COLUMN 71 VALUE "Close".

       01  CTAC-VAR-CWSQLS.
           05 LINE ll COLUMN 01 PIC ZZZ9 from Stat-TOTAL-TIME.
           05 LINE ll COLUMN 06 PIC ZZ9 from PERC.
           05 LINE ll COLUMN 10 PIC X(015) from stat-HANDLER.
           05 LINE ll COLUMN 26 PIC ZZ.ZZZ.ZZ9 from CTAC-READ.
           05 LINE ll COLUMN 37 PIC ZZ.ZZZ.ZZ9 from stat-WRITE.
           05 LINE ll COLUMN 48 PIC ZZZ.ZZ9 from stat-REWRITE.
           05 LINE ll COLUMN 56 PIC ZZZZZ9 from stat-DELETE.
           05 LINE ll COLUMN 63 PIC ZZZZZ9 from stat-START.
           05 LINE ll COLUMN 70 PIC ZZZZ9 from stat-OPEN.
           05 LINE ll COLUMN 76 PIC ZZZZ9 from stat-CLOSE.

       PROCEDURE DIVISION.

       000-INICIO.

           DISPLAY 'CWSQLSTAT' UPON ENVIRONMENT-NAME
           ACCEPT  LB-STAT     FROM ENVIRONMENT-VALUE
           MOVE LB-STAT TO flag-stat
           INSPECT flag-stat   CONVERTING MINUSCULAS TO MAIUSCULAS
           IF FLAG-STAT = 'ON' OR 'OFF'
              MOVE 'CWSQLC_stat' TO LB-STAT
           END-IF
           OPEN input stat
           PERFORM TEST AFTER UNTIL ESC
                   DISPLAY (1, 1) ERASE
                   DISPLAY CTAC-LIT-CWSQLS
                   MOVE HIGH-VALUES TO stat-REG
                   START stat KEY NOT GREATER stat-TOP
                   MOVE 1 TO LL
                   Perform test after until LL > 24
                                            or fs-stat > '09'
                          read stat previous record ignore lock
                          if fs-stat < '10'
                             IF stat-HANDLER = HIGH-VALUES
                                MOVE 100             TO PERC
                                MOVE stat-total-time TO total-time
                             ELSE
                                COMPUTE PERC = stat-total-time
                                             / total-time * 100
                             END-IF
                             add 1 to LL
                             compute CTAC-READ = stat-READ
                                               + stat-NEXT
                                               + stat-PREVIOUS
                             display CTAC-VAR-CWSQLS
                          end-if
                   END-PERFORM
                   ACCEPT LIXO AT 0111
                   ACCEPT TECLA FROM ESCAPE KEY
           END-PERFORM
           CLOSE stat.

       000-99-FIM. GOBACK.

       END PROGRAM CWSQLS.
