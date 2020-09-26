       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWLABE INITIAL.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  21/06/2005.
       SECURITY.      *************************************************
                      *                                               *
                      *  Verifica label especial DOC, RTF, HTMl e PDF *
                      *  Especial2 DBF, XLS, RPX e XML.               *
                      *                                               *
                      *************************************************
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 DEF                      PIC  X(100) VALUE SPACES.
           05 E.
              10 E1                    PIC  X(005) VALUE SPACES.
              10 E2                    PIC  X(005) VALUE SPACES.
           05 EXTW                     PIC  X(005) VALUE SPACES.
           05 EXTU                     PIC  X(005) VALUE SPACES.
              88 ESPECIAL VALUE "DOC" "RTF" "HTM" "HTML"  "PDF".
              88 ESPECIAL2 VALUE "DBF" "XLS" "RPX" "XML".
           05 MODO                     PIC  9(001) VALUE 0.
           05 ASTERISCO                PIC  9(001) VALUE 0.
           05 I                        PIC  9(003) VALUE 0.
           05 I-1                      PIC  9(003) VALUE 0.
           05 I-2                      PIC  9(003) VALUE 0.
           05 FS                       PIC  X(002) VALUE "00".
           05 OPERADOR                 PIC  X(030) VALUE SPACES.
           05 TMP                      PIC  X(255) VALUE SPACES.
           05 TASK                     PIC  9(006) VALUE 0.
           05 PROGRAMA                 PIC  X(008) VALUE SPACES.
           05 BARRA                    PIC  X(001) VALUE SPACE.
           05 TITULO                   PIC  X(050) VALUE SPACE.

       COPY CWUNIX.

       LINKAGE SECTION.

       01    OLD        PIC X(255).
       01    NEW        PIC X(255).
       01    EXT        PIC X(03).

       PROCEDURE DIVISION USING OLD NEW EXT.

       000-INICIO.

           IF   EXT = "***"
                MOVE 2 TO MODO
                MOVE "Salvar_como:" TO TITULO
           ELSE
                MOVE "Salvar_relat¢rio_com:" TO TITULO
           END-IF
           MOVE SPACES TO EXT
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH OLD
                   IF  OLD (I: 1) = "*"
                       MOVE 1 TO ASTERISCO
                   END-IF
                   COMPUTE I-1 = I - 1
                   COMPUTE I-2 = I + 1
                   IF  I > 1
                   AND OLD (I: 1) = "."
                   AND I < LENGTH OLD
                   AND (OLD (I-1: 1) NOT = ".")
                   AND (OLD (I-2: 1) NOT = ".")
                   AND (OLD (I-2: 1) NOT = "/")
                   AND (OLD (I-2: 1) NOT = "\")
                       ADD 1 TO I
                       MOVE OLD (I: ) TO EXTW EXTU
                       INSPECT EXTU CONVERTING MINUSCULAS
                                            TO MAIUSCULAS
                       IF  ESPECIAL
                       OR (ESPECIAL2 AND MODO = 2)
                           MOVE EXTU TO EXT
                       END-IF
                       SUBTRACT 1 FROM I
                       MOVE SPACES TO OLD (I: )
                   END-IF
           END-PERFORM
           IF  OLD = SPACES
           OR  ASTERISCO = 1
               IF   OLD = "*"
                    MOVE 0 TO I
                    IF  EXTU = "HTML" OR "HTM"
                        MOVE "HTML" TO EXTW
                    END-IF
                    PERFORM TEST AFTER UNTIL FS = "30" OR "35"
                            IF   I = 0
                                 MOVE "Novo." TO DEF
                                 MOVE EXTW TO DEF (6: )
                            ELSE
                                 MOVE SPACES TO DEF
                                 STRING "Novo" DELIMITED BY SIZE
                                        I      DELIMITED BY SIZE
                                        "."    DELIMITED BY SIZE
                                        EXTW   DELIMITED BY SPACE
                                 INTO DEF
                            END-IF
                            ADD 1 TO I
                            CALL "CWBINF" USING "I" FS DEF
                            IF  FS = "00"
                                CALL "CWBINF" USING "C" FS DEF
                            END-IF
                    END-PERFORM
               ELSE
                    STRING OLD DELIMITED BY SPACE
                           "." DELIMITED BY SIZE
                           EXTW DELIMITED BY SPACE
                    INTO DEF
               END-IF
               MOVE SPACES TO OLD NEW
               EXEC COBOLware Path
                    FILE OLD;OLD
                    DEFAULT DEF
                    HEADER TITULO
                    (WITH-DIR)
                    (WITH-DRIVES)
                    (WITH-NEWDIR)
                    (WITH-NEWFILE)
               END-EXEC
               IF  OLD NOT = SPACES
                   PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH OLD
                           IF OLD (I:1) = "."
                              ADD 1 TO I
                              MOVE OLD(I:) TO E1
                              MOVE NEW     TO E2
                              INSPECT E CONVERTING MINUSCULAS
                                                TO MAIUSCULAS
                              IF E1 = E2
                                 SUBTRACT 1 FROM I
                                 MOVE SPACES TO OLD(I:)
                              END-IF
                              EXIT PERFORM
                           END-IF
                   END-PERFORM
                   IF E1 = E2
                      STRING OLD DELIMITED BY SPACE
                          "." DELIMITED BY SIZE
                          EXTW DELIMITED BY SPACE
                          INTO NEW
                   ELSE
                      MOVE OLD TO NEW
                   END-IF
               ELSE
                   MOVE "OFF" TO EXT
               END-IF
           ELSE
               MOVE SPACES TO NEW
               STRING OLD  DELIMITED BY SPACE
                      "."  DELIMITED BY SIZE
                      EXTW DELIMITED BY SIZE
                      INTO NEW
      *        IF  NOT ESPECIAL
      *            MOVE NEW    TO OLD
      *            MOVE SPACES TO NEW
      *       END-IF
           END-IF
           IF (ESPECIAL
           OR (ESPECIAL2 AND MODO = 2))
           AND (NEW NOT = SPACES)
               MOVE SPACES TO OLD
               DISPLAY "TEMP"         UPON ENVIRONMENT-NAME
               ACCEPT  TMP            FROM ENVIRONMENT-VALUE
               IF   TMP = SPACES
                    DISPLAY "TMP"     UPON ENVIRONMENT-NAME
                    ACCEPT  TMP       FROM ENVIRONMENT-VALUE
               END-IF
               CALL  "CWGETU"      USING OPERADOR
                                         TASK
                                         PROGRAMA
                                         '?'
             CALL "CWUNIX" USING PARAMETROS-CWUNIX
             IF   CWUNIX-OFF
                  MOVE "\" TO BARRA
             ELSE
                  MOVE "/" TO BARRA
             END-IF
             STRING TMP   DELIMITED BY SPACE
                    BARRA "wk" TASK ".txt" DELIMITED BY SIZE
                    INTO OLD
           ELSE
               MOVE NEW    TO OLD
               MOVE SPACES TO NEW
           END-IF.

       000-99-FIM. GOBACK.

       END PROGRAM CWLABE.
