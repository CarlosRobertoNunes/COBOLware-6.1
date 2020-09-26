       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWCLEA.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  15/05/2004.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula CBL_CLEAR_SCR Clear screen            *
                      *                                               *
                      *************************************************
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 LIXO-CHAR              PIC X(2000) VALUE ALL X"FF".
           05 LIXO-ATTR              PIC X(2000) VALUE ALL X"00".

       01  SCREEN-POSITION.
           05 ROW-NUMBER       PIC 9(002) COMP-X VALUE 0.
           05 COLUMN-NUMBER    PIC 9(002) COMP-X VALUE 0.

       01  CHARACTER-BUFFER    PIC X(2000) VALUE SPACES.
       01  ATTRIBUTE-BUFFER    PIC X(2000) VALUE SPACES.
       01  STRING-LENGTH       PIC 9(0004) COMP-X VALUE 2000.

       COPY CWUNIX.

       LINKAGE SECTION.

       01  CARACTERE PIC X(001).
       01  ATRIBUTO  PIC X(001).

       PROCEDURE DIVISION USING CARACTERE ATRIBUTO.
       000-INICIO.

           ON   1
                CALL "CWUNIX" USING PARAMETROS-CWUNIX.

           MOVE SPACES TO CHARACTER-BUFFER
                          ATTRIBUTE-BUFFER
           INSPECT CHARACTER-BUFFER
                   CONVERTING SPACES TO CARACTERE
           INSPECT ATTRIBUTE-BUFFER
                   CONVERTING SPACES TO ATRIBUTO

           IF   CWUNIX-ON
                CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
                                                   LIXO-CHAR
                                                   LIXO-ATTR
                                                   STRING-LENGTH
           END-IF

           CALL "CBL_WRITE_SCR_CHATTRS" USING SCREEN-POSITION
                                              CHARACTER-BUFFER
                                              ATTRIBUTE-BUFFER
                                              STRING-LENGTH.

       000-99-FIM. GOBACK.
       END PROGRAM CWCLEA.
