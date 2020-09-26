       IDENTIFICATION DIVISION.
       PROGRAM-ID.    XSDRENAM.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  09/09/2017.
       SECURITY.      *************************************************
                      *                                               *
                      *  Suporte a programas gerados pelo XSEED       *
                      *  Rename de arquivo                            *
                      *                                               *
                      *************************************************
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  STATUS-CODE        PIC 9(002) COMP-5.
       01  NAMES.
           05 OLD             PIC X(060).
           05 NEW             PIC X(060).

       LINKAGE SECTION.

       01  OLD-FILENAME                 PIC  X(060).
       01  NEW-FILENAME                 PIC  X(060).
       01  GLB-STATUS                   PIC  X(005).

       PROCEDURE DIVISION USING OLD-FILENAME NEW-FILENAME GLB-STATUS.

       INICIO.

           MOVE OLD-FILENAME TO OLD
           MOVE NEW-FILENAME TO NEW
           MOVE SPACES       TO GLB-STATUS
           INSPECT NAMES CONVERTING X"00" TO SPACE
           CALL "FS_RENAME_FILE" USING OLD NEW
                                 RETURNING STATUS-CODE

           IF  STATUS-CODE NOT = 0
               CALL "CBL_RENAME_FILE" USING OLD NEW
                                  RETURNING STATUS-CODE
               IF  STATUS-CODE NOT = 0
                   MOVE ALL "*" TO GLB-STATUS
               END-IF
           END-IF

           GOBACK.

       END PROGRAM XSDRENAM.
