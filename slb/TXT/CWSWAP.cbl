       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWSWAP.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  15/05/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula CBL_SWAP_SCR_CHATTRS                  *
                      *  Swap character &attribute                    *
                      *                                               *
                      *************************************************
       DATA DIVISION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
       COPY CWUNIX.
       01  CHARACTER-BUFFER2   PIC X(2000).
       01  ATTRIBUTE-BUFFER2   PIC X(2000).

       LINKAGE SECTION.

       01  SCREEN-POSITION.
           05 ROW-NUMBER       PIC 9(002) COMP-X.
           05 COLUMN-NUMBER    PIC 9(002) COMP-X.

       01  CHARACTER-BUFFER    PIC X(2000).
       01  ATTRIBUTE-BUFFER    PIC X(2000).
       01  STRING-LENGTH       PIC 9(004) COMP-X.

       PROCEDURE DIVISION USING SCREEN-POSITION
                                CHARACTER-BUFFER
                                ATTRIBUTE-BUFFER
                                STRING-LENGTH.
       000-INICIO.

           ON   1
                CALL "CWUNIX" USING PARAMETROS-CWUNIX.

           IF   CWUNIX-WINDOWS
                CALL "CBL_READ_SCR_CHATTRS"
                     USING SCREEN-POSITION
                           CHARACTER-BUFFER2 (1: STRING-LENGTH)
                           ATTRIBUTE-BUFFER2 (1: STRING-LENGTH)
                           STRING-LENGTH
                INSPECT CHARACTER-BUFFER2 (1: STRING-LENGTH)
                        CONVERTING ACENTOS-437
                                TO ACENTOS-850
                INSPECT CHARACTER-BUFFER (1: STRING-LENGTH)
                        CONVERTING ACENTOS-850
                                TO ACENTOS-437
                CALL "CBL_WRITE_SCR_CHATTRS"
                     USING SCREEN-POSITION
                           CHARACTER-BUFFER (1: STRING-LENGTH)
                           ATTRIBUTE-BUFFER (1: STRING-LENGTH)
                           STRING-LENGTH
                MOVE CHARACTER-BUFFER2 TO CHARACTER-BUFFER
                MOVE ATTRIBUTE-BUFFER2 TO ATTRIBUTE-BUFFER
           ELSE
                CALL "CBL_SWAP_SCR_CHATTRS"
                     USING SCREEN-POSITION
                           CHARACTER-BUFFER (1: STRING-LENGTH)
                           ATTRIBUTE-BUFFER (1: STRING-LENGTH)
                           STRING-LENGTH
           END-IF.

       000-99-FIM. GOBACK.

       END PROGRAM CWSWAP.
