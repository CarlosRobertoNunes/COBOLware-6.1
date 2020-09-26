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
       WORKING-STORAGE SECTION.

       01  CHARACTERS-SAVE PIC X(2000) VALUE SPACES.
       01  ATTRIBUTES-SAVE PIC X(2000) VALUE SPACES.

       01  PARAMETROS-CWUSER.
           COPY CWUSER.

       LINKAGE SECTION.

       01  CWSWAP-POSITION.
           05 ROW-NUMBER       PIC 9(002) COMP-X.
           05 COLUMN-NUMBER    PIC 9(002) COMP-X.

       01  CHARACTER-BUFFER    PIC X(2000).
       01  ATTRIBUTE-BUFFER    PIC X(2000).
       01  STRING-LENGTH       PIC 9(004) COMP-X.

       PROCEDURE DIVISION USING CWSWAP-POSITION
                                CHARACTER-BUFFER
                                ATTRIBUTE-BUFFER
                                STRING-LENGTH.
       000-INICIO.

           COMPUTE cwuser-LINE   = ROW-NUMBER    + 1
           COMPUTE cwuser-COLUMN = COLUMN-NUMBER + 1
           MOVE STRING-LENGTH TO cwuser-LENGTH-CHAR
                                 cwuser-LENGTH-ATTR
           CALL "CWUSER"   USING X"13" PARAMETROS-CWUSER
           MOVE cwuser-CHARACTERS (1: STRING-LENGTH)
             TO CHARACTERS-SAVE   (1: STRING-LENGTH)
           MOVE cwuser-ATTRIBUTES (1: STRING-LENGTH)
             TO ATTRIBUTES-SAVE   (1: STRING-LENGTH)
           MOVE CHARACTER-BUFFER  (1: STRING-LENGTH)
             TO cwuser-CHARACTERS (1: STRING-LENGTH)
           MOVE ATTRIBUTE-BUFFER  (1: STRING-LENGTH)
             TO cwuser-ATTRIBUTES (1: STRING-LENGTH)
           CALL "CWUSER"   USING X"14" PARAMETROS-CWUSER
           MOVE CHARACTERS-SAVE   (1: STRING-LENGTH)
             TO cwuser-CHARACTERS (1: STRING-LENGTH)
           MOVE ATTRIBUTES-SAVE   (1: STRING-LENGTH)
             TO cwuser-ATTRIBUTES (1: STRING-LENGTH).

       000-99-FIM. GOBACK.

       END PROGRAM CWSWAP.
