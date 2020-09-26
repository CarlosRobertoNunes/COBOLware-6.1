       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RENAME.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  28/04/2006.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula _MSRENAME                             *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01   STATUS-CODE        PIC 9(002) COMP-5.
       01   NAMES.
            05 OLD             PIC X(050).
            05 NEW             PIC X(050).

       LINKAGE SECTION.

       01  OLD-NAME     PIC X(50).
       01  LEN-OLD-NAME PIC 9(008) COMP-X.
       01  NEW-NAME     PIC X(50).
       01  LEN-NEW-NAME PIC 9(008) COMP-X.
       01  F-STATUS     PIC X(002).

       PROCEDURE DIVISION USING OLD-NAME
                                LEN-OLD-NAME
                                NEW-NAME
                                LEN-NEW-NAME
                                F-STATUS.

       000-INICIO.

           MOVE OLD-NAME (1: LEN-OLD-NAME) TO OLD
           MOVE NEW-NAME (1: LEN-NEW-NAME) TO NEW
           INSPECT NAMES CONVERTING X"00" TO SPACE
           CALL "FS_RENAME_FILE" USING OLD NEW
                                 RETURNING STATUS-CODE
           IF  STATUS-CODE = 0
               MOVE "00" TO F-STATUS
           ELSE
               CALL "CBL_RENAME_FILE" USING OLD NEW
                                  RETURNING STATUS-CODE
               IF  STATUS-CODE = 0
                   MOVE "00" TO F-STATUS
               ELSE
                   MOVE "30" TO F-STATUS
               END-IF
           END-IF.

       000-99-FIM. GOBACK.
       END PROGRAM RENAME.
