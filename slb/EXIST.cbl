       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EXIST.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  28/04/2006.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula _MSREMOVE                             *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01   FILENAME            PIC X(50).
       01   FILE-DETAILS.
            05  FILE-SIZE           PIC X(8) COMP-X.
            05  FILE-DATE.
            10      DAYX                PIC X COMP-X.
            10      MONTH               PIC X COMP-X.
            10      YEAR                PIC X(2) COMP-X.
            05  FILE-TIME.
                10  HOURS               PIC X COMP-X.
                10  MINUTES             PIC X COMP-X.
                10  SECONDS             PIC X COMP-X.
                10  HUNDREDTHS          PIC X COMP-X.
       01   STATUS-CODE        PIC 9(002) COMP-5.

       LINKAGE SECTION.

       01  OLD-NAME     PIC X(50).
       01  LEN-OLD-NAME PIC 9(008) COMP-X.
       01  F-STATUS     PIC X(02).

       PROCEDURE DIVISION USING OLD-NAME
                                LEN-OLD-NAME
                                F-STATUS.

       000-INICIO.

           MOVE OLD-NAME (1: LEN-OLD-NAME) TO FILENAME
           INSPECT FILENAME CONVERTING LOW-VALUES TO SPACE
           CALL "FS_CHECK_FILE_EXIST" USING     FILENAME
                                                FILE-DETAILS
                                      RETURNING STATUS-CODE
           IF  STATUS-CODE = 0
               MOVE "00" TO F-STATUS
           ELSE
               CALL "CBL_CHECK_FILE_EXIST" USING FILENAME
                                                 FILE-DETAILS
                                       RETURNING STATUS-CODE
               IF  STATUS-CODE = 0
                   MOVE "00" TO F-STATUS
               ELSE
                   MOVE "30" TO F-STATUS
               END-IF
           END-IF.

       000-99-FIM. GOBACK.
       END PROGRAM EXIST.
