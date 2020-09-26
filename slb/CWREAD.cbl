       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWREAD.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  04/01/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Aopio a CWBOXF via CWUSER                    *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      $Set IdxFormat"14" DataCompress"1" KeyCompress"7"
           SELECT LISTA ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD KEY    IS LISTA1 = LISTA-SEQUENCE
                                   LISTA-STRING-1 WITH DUPLICATES
                  ALTERNATE KEY IS LISTA2 = LISTA-SEQUENCE
                                   LISTA-STRING-2 WITH DUPLICATES
                  ALTERNATE KEY IS LISTA3 = LISTA-SEQUENCE
                                   LISTA-STRING-3 WITH DUPLICATES
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-LISTA.

       DATA DIVISION.
       FILE SECTION.

       FD  LISTA
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-LISTA.

       01  LISTA-REG.
           05 LISTA-SEQUENCE   COMP-X PIC  9(004).
           05 LISTA-STRING-1          PIC  X(080).
           05 LISTA-STRING-2          PIC  X(080).
           05 LISTA-STRING-3          PIC  X(001).

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1. COPY CWCASE.
      *    05 ANTERIOR            PIC  X(162) VALUE SPACES.
           05 REGISTROS           PIC  9(002) VALUE 0.
           05 CARREGADO           PIC  9(001) VALUE 0.
           05 ER-LISTA.
              10 FS-LISTA         PIC  X(002) VALUE "00".
              10 LB-LISTA         PIC  X(255) VALUE "$TEMP/cwread##".
           05 USUARIO             PIC  X(030) VALUE SPACES.
           05 TASK                PIC  9(006) VALUE 0.
           05 PROGRAMA            PIC  X(008) VALUE SPACES.

       LINKAGE SECTION.

       01  USER-IO                            PIC  X(001).
           88 OPEN-FILE                            VALUE "O" "o".
           88 CLOSE-FILE                           VALUE "C" "c".
           88 DELETE-FILE                          VALUE "D" "d".
           88 BEGIN-FILE                           VALUE "B" "b".
           88 END-FILE                             VALUE "E" "e".
           88 AT-END                               VALUE "*".
           88 READ-CHAR                            VALUE "R" "r".
           88 READ-NEXT                            VALUE "N" "n".
           88 READ-PREVIOUS                        VALUE "P" "p".
           88 NOT-LESS                             VALUE ">".
           88 NOT-GREATER                          VALUE "<".
           88 EDIT-KEY                             VALUE "$".
           88 EDIT-LOAD                            VALUE "L".
       01  ORDER-X                            PIC  9(001).
       01  STRING-1                           PIC  X(080).
       01  STRING-2                           PIC  X(080).
       01  VERTICAL-LENGTH                    PIC  9(002).
       01  WORK-AREA.
           05 WORK-SEQUENCE            COMP-X PIC  9(004).
           05 FILLER                          PIC  X(048).
       01  CWBOXF-EDIT                 COMP-X PIC  9(002).
       01  CWBOXF-OPTION                      PIC  X(076).

       PROCEDURE DIVISION USING USER-IO ORDER-X
                                        STRING-1
                                        STRING-2
                                        VERTICAL-LENGTH
                                        WORK-AREA
                                        CWBOXF-EDIT CWBOXF-OPTION.
       000-INICIO.

           ON 1
              CALL "CWGETU" USING USUARIO TASK PROGRAMA "?"
              MOVE TASK (5: 2) TO LB-LISTA (13: 2).

           EVALUATE TRUE
               WHEN READ-CHAR
                    MOVE WORK-SEQUENCE    TO LISTA-SEQUENCE
                    MOVE WORK-AREA (3: 1) TO LISTA-STRING-3
                    READ LISTA KEY IS LISTA3
                    IF   FS-LISTA = "23"
                         MOVE SPACES TO LISTA-STRING-1
                                        LISTA-STRING-2
                    END-IF
               WHEN EDIT-LOAD
                    IF   CARREGADO = 0
                         OPEN I-O LISTA
                         MOVE 1 TO CARREGADO
                    END-IF
                    MOVE WORK-SEQUENCE TO LISTA-SEQUENCE
                    MOVE STRING-1      TO LISTA-STRING-1
                    MOVE STRING-2      TO LISTA-STRING-2
                    IF   WORK-SEQUENCE = 0
                         MOVE WORK-AREA (3: 1) TO LISTA-STRING-3
                         INSPECT LISTA-STRING-3
                                 CONVERTING MINUSCULAS TO MAIUSCULAS
                    END-IF
                    WRITE LISTA-REG
               WHEN EDIT-KEY
                    CONTINUE *> Tecla de funçao em CWBOXF-EDIT
               WHEN OPEN-FILE
                    IF   CARREGADO = 0
                         OPEN I-O LISTA
                         MOVE 1 TO CARREGADO
                    END-IF
                    MOVE WORK-SEQUENCE TO LISTA-SEQUENCE
                    MOVE LOW-VALUES    TO LISTA-STRING-1
                    START LISTA KEY NOT LESS LISTA1
                    IF   FS-LISTA > "09"
                         CALL "CWISAM" USING ER-LISTA
                    END-IF
                    MOVE 0 TO REGISTROS
                    PERFORM TEST AFTER UNTIL FS-LISTA > "09"
                                          OR REGISTROS = VERTICAL-LENGTH
                            READ LISTA NEXT RECORD
                            IF   FS-LISTA < "10"
                            AND  WORK-SEQUENCE = LISTA-SEQUENCE
                                 ADD 1 TO REGISTROS
                            ELSE
                                 MOVE "10" TO FS-LISTA
                            END-IF
                    END-PERFORM
                    IF   REGISTROS = 0
                         MOVE 1 TO REGISTROS
                    END-IF
                    IF   REGISTROS < VERTICAL-LENGTH
                         MOVE REGISTROS TO VERTICAL-LENGTH
                    END-IF
               WHEN CLOSE-FILE
                    CONTINUE
               WHEN DELETE-FILE
                    CLOSE LISTA
                    MOVE 0 TO CARREGADO
                    DELETE FILE LISTA
               WHEN BEGIN-FILE
                    INITIALIZE LISTA-REG
                    MOVE WORK-SEQUENCE TO LISTA-SEQUENCE
                    EVALUATE ORDER-X
                        WHEN 1
                             START LISTA KEY NOT < LISTA1
                        WHEN 2
                             START LISTA KEY NOT < LISTA2
                        WHEN OTHER
                             START LISTA KEY NOT < LISTA1
                    END-EVALUATE
               WHEN END-FILE
                    MOVE HIGH-VALUE    TO LISTA-REG
                    MOVE WORK-SEQUENCE TO LISTA-SEQUENCE
                    EVALUATE ORDER-X
                        WHEN 1
                             START LISTA KEY NOT > LISTA1
                        WHEN 2
                             START LISTA KEY NOT > LISTA2
                        WHEN OTHER
                             START LISTA KEY NOT > LISTA1
                    END-EVALUATE
               WHEN READ-NEXT
      *             MOVE LISTA-REG TO ANTERIOR
                    READ LISTA NEXT RECORD
                    IF   FS-LISTA > "09"
                    OR  (WORK-SEQUENCE NOT = LISTA-SEQUENCE)
      *                  MOVE ANTERIOR TO LISTA-REG
                         SET AT-END TO TRUE
                    END-IF
               WHEN READ-PREVIOUS
      *             MOVE LISTA-REG TO ANTERIOR
                    READ LISTA PREVIOUS RECORD
                    IF   FS-LISTA > "09"
                    OR  (WORK-SEQUENCE NOT = LISTA-SEQUENCE)
      *                  MOVE ANTERIOR TO LISTA-REG
                         SET AT-END TO TRUE
                    END-IF
               WHEN NOT-LESS
                    EVALUATE ORDER-X
                        WHEN 1
                             MOVE STRING-1      TO LISTA-STRING-1
                             MOVE WORK-SEQUENCE TO LISTA-SEQUENCE
                             START LISTA KEY NOT < LISTA1
                                   INVALID KEY
                                           SET AT-END TO TRUE
                             END-START
                        WHEN 2
                             MOVE STRING-2      TO LISTA-STRING-2
                             MOVE WORK-SEQUENCE TO LISTA-SEQUENCE
                             START LISTA KEY NOT < LISTA2
                                   INVALID KEY
                                           SET AT-END TO TRUE
                             END-START
                    END-EVALUATE
               WHEN NOT-GREATER
                    EVALUATE ORDER-X
                        WHEN 1
                             MOVE STRING-1      TO LISTA-STRING-1
                             MOVE WORK-SEQUENCE TO LISTA-SEQUENCE
                             START LISTA KEY NOT > LISTA1
                                   INVALID KEY
                                           SET AT-END TO TRUE
                             END-START
                        WHEN 2
                             MOVE STRING-2      TO LISTA-STRING-2
                             MOVE WORK-SEQUENCE TO LISTA-SEQUENCE
                             START LISTA KEY NOT > LISTA2
                                   INVALID KEY
                                           SET AT-END TO TRUE
                             END-START
                    END-EVALUATE
           END-EVALUATE

      *    CALL "DBG" USING USER-IO LISTA-REG WORK-AREA
           MOVE LISTA-STRING-1 TO STRING-1
           MOVE LISTA-STRING-2 TO STRING-2
           IF WORK-AREA (1: 2) = X"0000"
              MOVE LISTA-STRING-3 TO STRING-1 (80: 1)
           END-IF

           GOBACK.

       END PROGRAM CWREAD.
