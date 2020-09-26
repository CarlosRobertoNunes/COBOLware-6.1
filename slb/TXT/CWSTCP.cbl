       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWSTCP.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  15/05/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula CBL_SET_CSR_POS                       *
                      *  Set cursor position                          *
                      *                                               *
                      *************************************************

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 ATTR                  PIC  9(002) COMP-X VALUE 0.
           05 ATTR2                 PIC  9(002) COMP-X VALUE 0.
           05 ATTR3                 PIC  9(002) COMP-X VALUE 0.
           05 SCREEN-POSITION.
              10 ROW-NUMBER         PIC  9(002) COMP-X VALUE 255.
              10 COLUMN-NUMBER      PIC  9(002) COMP-X VALUE 255.

       COPY CWUNIX.

       LINKAGE SECTION.

       01  CURPOS PIC XX.

       PROCEDURE DIVISION USING CURPOS.

           ON 1 CALL "CWUNIX" USING PARAMETROS-CWUNIX.

           IF  CWUNIX-OFF
               IF   SCREEN-POSITION NOT = X"FFFF"
                    CALL "CBL_READ_SCR_ATTRS" USING SCREEN-POSITION
                                                    ATTR3
                                                    X"0001"
               END-IF
               IF  (CURPOS NOT = SCREEN-POSITION)
               OR  ((ATTR3  = 112 OR 116)
               AND (SCREEN-POSITION NOT = X"FFFF"))
                    IF  SCREEN-POSITION NOT = X"FFFF"
                    AND(ATTR3 = 240 OR 244)
                        CALL "CBL_WRITE_SCR_ATTRS" USING SCREEN-POSITION
                                                         ATTR
                                                         X"0001"
                    END-IF
                    MOVE CURPOS TO SCREEN-POSITION
                    IF  SCREEN-POSITION NOT = X"FFFF"
                        CALL "CBL_READ_SCR_ATTRS" USING SCREEN-POSITION
                                                        ATTR
                                                        X"0001"
                        IF  ATTR = 112 OR 116
                            IF  ATTR = 112
                                MOVE 240 TO ATTR2
                            ELSE
                                MOVE 244 TO ATTR2
                            END-IF
                            CALL "CBL_WRITE_SCR_ATTRS" USING
                                                       SCREEN-POSITION
                                                       ATTR2
                                                       X"0001"
                        ELSE
                            MOVE X"FFFF" TO SCREEN-POSITION
                        END-IF
                    END-IF
               END-IF
           END-IF

           CALL "CBL_SET_CSR_POS" USING CURPOS
           GOBACK.

       END PROGRAM CWSTCP.
