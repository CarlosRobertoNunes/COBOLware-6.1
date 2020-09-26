      $Set CallFH"FHREDIR"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWREL5.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  17/06/2000.
       SECURITY.      *************************************************
                      *                                               *
                      *  Relator 1.0                                  *
                      *                                               *
                      *  Gerador de relatorios COBOLware              *
                      *                                               *
                      *  Modulo 5 - Apoio a CWBOXF para tabela de     *
                      *             Datanames CAB                     *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY CABS.SEL.

       DATA DIVISION.
       FILE SECTION.

       COPY CABS.FD.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 REGISTROS           PIC  9(002) VALUE 0.
           05 VEZ                 PIC  9(001) VALUE 1.
           05 ER-CABS.
              10 FS-CABS          PIC  X(002) VALUE "00".
              10 LB-CABS          PIC  X(255) VALUE "CABS".

       LINKAGE SECTION.

       01  USER-IO                            PIC  X(001).
           88 OPEN-FILE                               VALUE "O" "o".
           88 CLOSE-FILE                              VALUE "C" "c".
           88 BEGIN-FILE                              VALUE "B" "b".
           88 END-FILE                                VALUE "E" "e".
           88 AT-END                                  VALUE "*".
           88 READ-NEXT                               VALUE "N" "n".
           88 READ-PREVIOUS                           VALUE "P" "p".
           88 NOT-LESS                                VALUE ">".
           88 NOT-GREATER                             VALUE "<".
       01  ORDER-X                            PIC  9(001).
       01  STRING-1                           PIC  X(040).
       01  STRING-2                           PIC  X(040).
       01  VERTICAL-LENGTH                    PIC  9(002).
       01  WORK-AREA                          PIC  X(050).

       PROCEDURE DIVISION USING USER-IO ORDER-X
                                        STRING-1
                                        STRING-2
                                        VERTICAL-LENGTH
                                        WORK-AREA.
       DECLARATIVES.

       DECLARATIVES-CABS SECTION.

           USE AFTER ERROR PROCEDURE ON CABS.

       CHECK-CABS-TRAVADO.

           IF   FS-CABS = "99" OR "9D"
                CALL "CWISAM" USING ER-CABS
           END-IF.

       END DECLARATIVES.

       000 SECTION.
       000-INICIO.

           IF   VEZ = 1
                MOVE 2           TO VEZ
                MOVE WORK-AREA   TO LB-CABS
           END-IF

           PERFORM TEST AFTER UNTIL (FS-CABS NOT = "99")
                                AND (FS-CABS NOT = "9D")
           EVALUATE TRUE
               WHEN OPEN-FILE
                    OPEN INPUT CABS
                    IF   FS-CABS > "09"
                         CALL "CWISAM" USING ER-CABS
                    END-IF
                    MOVE 0 TO REGISTROS
                    PERFORM TEST AFTER UNTIL FS-CABS > "09"
                                          OR REGISTROS = VERTICAL-LENGTH
                            READ CABS NEXT RECORD
                            IF   FS-CABS < "10"
                                 ADD 1 TO REGISTROS
                            END-IF
                    END-PERFORM
                    IF   REGISTROS = 0
                         MOVE 1 TO REGISTROS
                    END-IF
                    IF   REGISTROS < VERTICAL-LENGTH
                         MOVE REGISTROS TO VERTICAL-LENGTH
                    END-IF
               WHEN CLOSE-FILE
                    CLOSE CABS
               WHEN BEGIN-FILE
                    INITIALIZE CABS-REG
                    START CABS KEY NOT < CABS-CHAVE
               WHEN END-FILE
                    MOVE HIGH-VALUE TO CABS-REG
                    START CABS KEY NOT > CABS-CHAVE
               WHEN READ-NEXT
                    READ CABS NEXT RECORD
                      AT END
                         SET AT-END TO TRUE
                    END-READ
               WHEN READ-PREVIOUS
                    READ CABS PREVIOUS RECORD
                      AT END
                         SET AT-END TO TRUE
                    END-READ
               WHEN NOT-LESS
                    MOVE STRING-2 TO CABS-CHAVE
                    START CABS KEY NOT < CABS-CHAVE
                          INVALID KEY
                                  SET AT-END TO TRUE
                    END-START
               WHEN NOT-GREATER
                    MOVE STRING-2 TO CABS-CHAVE
                    START CABS KEY NOT > CABS-CHAVE
                          INVALID KEY
                                  SET AT-END TO TRUE
                    END-START
           END-EVALUATE
           END-PERFORM

           MOVE CABS-CHAVE TO STRING-1
                              STRING-2
           GOBACK.

       END PROGRAM CWREL5.
