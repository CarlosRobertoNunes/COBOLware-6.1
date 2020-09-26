       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWBOXM.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  07/05/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Programa provedor para a CWBOXF em CWBOXC    *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT OPTIONAL BOXWRK ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS BOXWRK-CHAVE
                  ALTERNATE RECORD KEY BOXWRK-STRING-1 WITH DUPLICATES
                  ALTERNATE RECORD KEY BOXWRK-STRING-2 WITH DUPLICATES
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-BOXWRK.

       DATA DIVISION.
       FILE SECTION.

       FD  BOXWRK
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-BOXWRK.

       01  BOXWRK-REG.
           05 BOXWRK-CHAVE.
              10 BOXWRK-SEQUENCIA  COMP-3 PIC  9(018).
           05 BOXWRK-STRING-1             PIC  X(080).
           05 BOXWRK-STRING-2             PIC  X(080).
           05 BOXWRK-COLOR-ITEM           PIC  X(010).
           05 BOXWRK-H-FILE               PIC  X(050).
           05 BOXWRK-H-LINE               PIC  9(002).
           05 BOXWRK-H-COLUMN             PIC  9(002).
           05 BOXWRK-H-TYPE               PIC  9(001).
           05 BOXWRK-H-VERTICAL-LENGTH    PIC  9(002).
           05 BOXWRK-H-HORIZONTAL-LENGTH  PIC  9(002).
           05 BOXWRK-H-COLOR-FRAME        PIC  9(002) COMP-X.
           05 BOXWRK-H-COLOR-BORDER       PIC  9(002) COMP-X.
           05 BOXWRK-H-COLOR-SHADE        PIC  9(002) COMP-X.
              66 BOXWRK-HELP
                 RENAMES BOXWRK-H-FILE THRU BOXWRK-H-COLOR-SHADE.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 REGISTROS           PIC  9(002) VALUE 0.
           05 VEZ                 PIC  9(001) VALUE 1.
           05 ER-BOXWRK.
              10 FS-BOXWRK             PIC  X(002) VALUE "00".
              10 LB-BOXWRK             PIC  X(255) VALUE SPACES.

       LINKAGE SECTION.

       01  USER-IO                            PIC  X(001).
           88 OPEN-FILE                            VALUE "O" "o".
           88 CLOSE-FILE                           VALUE "C" "c".
           88 BEGIN-FILE                           VALUE "B" "b".
           88 END-FILE                             VALUE "E" "e".
           88 AT-END                               VALUE "*".
           88 READ-NEXT                            VALUE "N" "n".
           88 READ-PREVIOUS                        VALUE "P" "p".
           88 NOT-LESS                             VALUE ">".
           88 NOT-GREATER                          VALUE "<".
           88 EDIT-KEY                             VALUE "$".
           88 WRITE-RECORD                         VALUE "W".
           88 CREATE-FILE                          VALUE "X".
       01  ORDER-X                            PIC  9(001).
       01  STRING-1                           PIC  X(080).
       01  STRING-2                           PIC  X(080).
       01  VERTICAL-LENGTH                    PIC  9(002).
       01  WORK-AREA                          PIC  X(050).
       01  CWBOXF-EDIT                 COMP-X PIC  9(002).
       01  CWBOXF-OPTION                      PIC  X(076).

       PROCEDURE DIVISION USING USER-IO ORDER-X
                                        STRING-1
                                        STRING-2
                                        VERTICAL-LENGTH
                                        WORK-AREA
                                        CWBOXF-EDIT CWBOXF-OPTION.
       000-INICIO.

           MOVE WORK-AREA TO LB-BOXWRK
           EVALUATE TRUE
               WHEN EDIT-KEY
                    CONTINUE *> Tecla de funçao em CWBOXF-EDIT
               WHEN OPEN-FILE
                    OPEN INPUT BOXWRK
                    IF   FS-BOXWRK > "09"
                         CALL "CWISAM" USING ER-BOXWRK
                    END-IF
                    MOVE 0 TO REGISTROS
                    PERFORM TEST AFTER UNTIL FS-BOXWRK > "09"
                                          OR REGISTROS = VERTICAL-LENGTH
                            READ BOXWRK NEXT RECORD
                                          IGNORE LOCK
                            IF   FS-BOXWRK < "10"
                                 ADD 1 TO REGISTROS
                            END-IF
                    END-PERFORM
                    IF   REGISTROS = 0
                         MOVE 1 TO REGISTROS
                         SET AT-END TO TRUE
                    END-IF
                    IF   REGISTROS < VERTICAL-LENGTH
                         MOVE REGISTROS TO VERTICAL-LENGTH
                    END-IF
               WHEN CLOSE-FILE
                    CLOSE BOXWRK
               WHEN BEGIN-FILE
                    INITIALIZE BOXWRK-REG
                    EVALUATE ORDER-X
                        WHEN 1
                             START BOXWRK KEY NOT < BOXWRK-STRING-1
                        WHEN 2
                             START BOXWRK KEY NOT < BOXWRK-STRING-2
                        WHEN OTHER
                             START BOXWRK KEY NOT < BOXWRK-CHAVE
                    END-EVALUATE
               WHEN END-FILE
                    MOVE HIGH-VALUE TO BOXWRK-REG
                    EVALUATE ORDER-X
                        WHEN 1
                             START BOXWRK KEY NOT > BOXWRK-STRING-1
                        WHEN 2
                             START BOXWRK KEY NOT > BOXWRK-STRING-2
                        WHEN OTHER
                             START BOXWRK KEY NOT > BOXWRK-CHAVE
                    END-EVALUATE
               WHEN READ-NEXT
                    READ BOXWRK NEXT RECORD
                                  IGNORE LOCK
                    IF   FS-BOXWRK > "09"
                         SET AT-END TO TRUE
                    END-IF
               WHEN READ-PREVIOUS
                    READ BOXWRK PREVIOUS RECORD
                                  IGNORE LOCK
                    IF   FS-BOXWRK > "09"
                         SET AT-END TO TRUE
                    END-IF
               WHEN NOT-LESS
                    EVALUATE ORDER-X
                        WHEN 1
                             MOVE STRING-1 TO BOXWRK-STRING-1
                             START BOXWRK KEY NOT < BOXWRK-STRING-1
                                   INVALID KEY
                                           SET AT-END TO TRUE
                              END-START
                        WHEN 2
                             MOVE STRING-2 TO BOXWRK-STRING-2
                             START BOXWRK KEY NOT < BOXWRK-STRING-2
                                   INVALID KEY
                                           SET AT-END TO TRUE
                              END-START
                    END-EVALUATE
               WHEN NOT-GREATER
                    EVALUATE ORDER-X
                        WHEN 1
                             MOVE STRING-1 TO BOXWRK-STRING-1
                             START BOXWRK KEY NOT > BOXWRK-STRING-1
                                   INVALID KEY
                                           SET AT-END TO TRUE
                              END-START
                        WHEN 2
                             MOVE STRING-2 TO BOXWRK-STRING-2
                             START BOXWRK KEY NOT > BOXWRK-STRING-2
                                   INVALID KEY
                                           SET AT-END TO TRUE
                              END-START
                    END-EVALUATE
           END-EVALUATE

           MOVE BOXWRK-STRING-1 TO STRING-1
           MOVE BOXWRK-STRING-2 TO STRING-2
           IF (READ-NEXT OR READ-PREVIOUS)
           AND FS-BOXWRK < '10'
           AND (BOXWRK-COLOR-ITEM NOT = SPACES)
               DISPLAY "CWBOXF-COLOR-ITEM" UPON ENVIRONMENT-NAME
               DISPLAY BOXWRK-COLOR-ITEM UPON ENVIRONMENT-VALUE
           END-IF
           GOBACK.

       END PROGRAM CWBOXM.
