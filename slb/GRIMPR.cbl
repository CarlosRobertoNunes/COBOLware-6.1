       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GRIMPR.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  23/02/2011.
       SECURITY.      *************************************************
                      *                                               *
                      *  Compatibilza chmada GRIMPR da versÆo 3       *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

       COPY CWIMPR.

       LINKAGE SECTION.

       01  PARAMETROS-GRIMPR.
           05 GRIMPR-REPORT                  PIC X(007) VALUE "-------".
           05 GRIMPR-FORM-TYPE               PIC X(001) VALUE "1".
              88 SIZE-132                               VALUE "1".
              88 SIZE-080                               VALUE "2".
              88 SIZE-220                               VALUE "3".
              88 END-PRINT                              VALUE "9".
           05 GRIMPR-SIZE-PAGE               PIC 9(002) VALUE 59.
           05 GRIMPR-TITLE                   PIC X(174) VALUE SPACES.
           05 GRIMPR-SUB-TITLE               PIC X(174) VALUE SPACES.
           05 GRIMPR-HEADER-1                PIC X(220) VALUE SPACES.
           05 GRIMPR-HEADER-2                PIC X(220) VALUE SPACES.
           05 GRIMPR-HEADER-3                PIC X(220) VALUE SPACES.
           05 GRIMPR-HEADER-4                PIC X(220) VALUE SPACES.
           05 GRIMPR-HEADER-5                PIC X(220) VALUE SPACES.
           05 GRIMPR-DETAIL                  PIC X(220) VALUE SPACES.
           05 GRIMPR-TIME-REPORT             PIC X(010) VALUE SPACES.
              88 SET-CLOSE                              VALUE "CLOSE".
           05 REDEFINES GRIMPR-TIME-REPORT.
              10 GRIMPR-SET-GET              PIC X(002).
                 88 SET-PAGE                            VALUE "#P" "#p".
                 88 GET-PAGE                            VALUE "?P" "?p".
              10 GRIMPR-SPECIAL-PAGE         PIC 9(004).
              10 GRIMPR-SPECIAL-FOLD         PIC 9(004).
           05 REDEFINES GRIMPR-TIME-REPORT.
              10 FILLER                      PIC X(002).
                 88 SET-DATE                            VALUE "#D" "#d".
                 88 SET-TIME                            VALUE "#T" "#t".
                 88 SET-SIZE-PAGE                       VALUE "#S" "#s".
                 88 SET-QUIET                           VALUE "><".
              10 GRIMPR-SPECIAL-DATE-TIME    PIC 9(006).
              10 FILLER                      PIC X(002).
           05 REDEFINES GRIMPR-TIME-REPORT.
              10 FILLER                      PIC X(002).
                 88 SET-PAGE-OFF                        VALUE "#X" "#x".
                 88 SET-FOLD-OFF                        VALUE "#Y" "#y".
                 88 SET-FOLD-PAGE-OFF                   VALUE "#Z" "#z".
                 88 SET-WEEK-OFF                        VALUE "#J" "#j".
   876           88 SET-DATE-OFF                        VALUE "#C" "#c".
   877           88 SET-TIME-OFF                        VALUE "#R" "#r".
   878           88 SET-PRINTER-SET                     VALUE "#>".
   879           88 SET-PRINTER-RESET                   VALUE "#<".
              10 GRIMPR-PAGE-TXT             PIC X(007).
              10 FILLER                      PIC X(001).

       PROCEDURE DIVISION USING PARAMETROS-GRIMPR.

           MOVE GRIMPR-REPORT       TO CWIMPR-REPORT
           MOVE GRIMPR-FORM-TYPE    TO CWIMPR-FORM-TYPE
           MOVE GRIMPR-SIZE-PAGE    TO CWIMPR-SIZE-PAGE
           MOVE GRIMPR-TITLE        TO CWIMPR-TITLE
                                       CWIMPR-NOTE
           MOVE GRIMPR-SUB-TITLE    TO CWIMPR-SUB-TITLE
           MOVE GRIMPR-HEADER-1     TO CWIMPR-HEADER-1
           MOVE GRIMPR-HEADER-2     TO CWIMPR-HEADER-2
           MOVE GRIMPR-HEADER-3     TO CWIMPR-HEADER-3
           MOVE GRIMPR-HEADER-4     TO CWIMPR-HEADER-4
           MOVE GRIMPR-HEADER-5     TO CWIMPR-HEADER-5
           MOVE GRIMPR-DETAIL       TO CWIMPR-DETAIL
           MOVE GRIMPR-TIME-REPORT  TO CWIMPR-TIME-REPORT

           EVALUATE TRUE
                    WHEN SET-PAGE      SET CWIMPR-SET-PAGE      TO TRUE
                                       MOVE GRIMPR-SPECIAL-PAGE
                                         TO CWIMPR-SPECIAL-PAGE
                                       MOVE GRIMPR-SPECIAL-FOLD
                                         TO CWIMPR-SPECIAL-FOLD
                                       MOVE SPACES TO CWIMPR-TIME-REPORT
                    WHEN GET-PAGE      SET CWIMPR-GET-PAGE      TO TRUE
                                       MOVE SPACES TO CWIMPR-TIME-REPORT
                    WHEN SET-DATE      SET CWIMPR-SET-DATE      TO TRUE
                                       MOVE GRIMPR-SPECIAL-DATE-TIME
                                         TO CWIMPR-SPECIAL-DATE-TIME
                                       MOVE SPACES TO CWIMPR-TIME-REPORT
                    WHEN SET-TIME      SET CWIMPR-SET-TIME      TO TRUE
                                       MOVE GRIMPR-SPECIAL-DATE-TIME
                                         TO CWIMPR-SPECIAL-DATE-TIME
                                       MOVE SPACES TO CWIMPR-TIME-REPORT
                    WHEN SET-SIZE-PAGE SET CWIMPR-SET-SIZE-PAGE TO TRUE
                                       MOVE GRIMPR-SPECIAL-DATE-TIME
                                         TO CWIMPR-SIZE-PAGE
                                       SET CWIMPR-SET-NO-OPTION TO TRUE
                                       MOVE SPACES TO CWIMPR-TIME-REPORT
                                       GOBACK
                    WHEN SET-QUIET     SET CWIMPR-SET-QUIET     TO TRUE
                                       MOVE SPACES TO CWIMPR-TIME-REPORT
                    WHEN SET-PAGE-OFF  SET CWIMPR-SET-PAGE-OFF  TO TRUE
                                       MOVE GRIMPR-PAGE-TXT
                                         TO CWIMPR-PAGE-TXT
                                       MOVE SPACES TO CWIMPR-TIME-REPORT
                    WHEN SET-FOLD-OFF  SET CWIMPR-SET-FOLD-OFF  TO TRUE
                                       MOVE GRIMPR-PAGE-TXT
                                         TO CWIMPR-PAGE-TXT
                                       MOVE SPACES TO CWIMPR-TIME-REPORT
                    WHEN SET-WEEK-OFF  SET CWIMPR-SET-WEEK-OFF  TO TRUE
                                       MOVE SPACES TO CWIMPR-TIME-REPORT
                    WHEN SET-FOLD-PAGE-OFF
                         SET CWIMPR-SET-FOLD-PAGE-OFF  TO TRUE
                         MOVE SPACES TO CWIMPR-TIME-REPORT
                    WHEN SET-DATE-OFF
                         SET CWIMPR-SET-DATE-OFF TO TRUE
                         MOVE SPACES TO CWIMPR-TIME-REPORT
                    WHEN SET-TIME-OFF
                         SET CWIMPR-SET-TIME-OFF TO TRUE
                         MOVE SPACES TO CWIMPR-TIME-REPORT
                    WHEN SET-PRINTER-SET
                         SET CWIMPR-SET-PRINTER-SET TO TRUE
                         MOVE SPACES TO CWIMPR-TIME-REPORT
                    WHEN SET-PRINTER-RESET
                         SET CWIMPR-SET-PRINTER-RESET TO TRUE
                         MOVE SPACES TO CWIMPR-TIME-REPORT
                    WHEN OTHER
                         SET CWIMPR-SET-NO-OPTION TO TRUE
           END-EVALUATE

           CALL "CWIMPR" USING PARAMETROS-CWIMPR
           IF  SET-CLOSE
               CANCEL "CWIMPR"
               INITIALIZE PARAMETROS-GRIMPR
               MOVE ALL "-" TO GRIMPR-REPORT
               MOVE "1"     TO GRIMPR-FORM-TYPE
           ELSE
               MOVE CWIMPR-TIME-REPORT   TO GRIMPR-TIME-REPORT
               SET  CWIMPR-SET-NO-OPTION TO TRUE
               MOVE CWIMPR-FORM-TYPE     TO GRIMPR-FORM-TYPE
               MOVE CWIMPR-TITLE         TO GRIMPR-TITLE
               MOVE CWIMPR-SUB-TITLE     TO GRIMPR-SUB-TITLE
               MOVE CWIMPR-DETAIL        TO GRIMPR-DETAIL
           END-IF

           IF   GET-PAGE
                MOVE CWIMPR-SPECIAL-PAGE TO GRIMPR-SPECIAL-PAGE
                MOVE CWIMPR-SPECIAL-FOLD TO GRIMPR-SPECIAL-FOLD
           END-IF

