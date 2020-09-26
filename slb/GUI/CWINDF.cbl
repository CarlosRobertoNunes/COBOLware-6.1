       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWINDF.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  14/10/2009.
       SECURITY.      *************************************************
                      *                                               *
                      *  Atributos da janela                          *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.

       LINKAGE SECTION.

       01  SP2-WINDOW-DEF.
           05  SP2-WD-RET-CODE         PIC S9(4) COMP-5.
           05  SP2-WD-LENS.
               10  SP2-WD-LEN-LEN      PIC S9(4) COMP-5 VALUE +10.
               10  SP2-WD-NUM-LEN      PIC S9(4) COMP-5 VALUE +38.
               10  SP2-WD-CHAR-LEN     PIC S9(4) COMP-5 VALUE +38.
               10  SP2-WD-VAR-LEN      PIC S9(4) COMP-5 VALUE +80.
               10  SP2-WD-TITLE-LEN    PIC S9(4) COMP-5 VALUE +80.
           05  SP2-WD-DATA.
      ******** SP2-WD-NUM-DATA ********
               10  SP2-WD-WINDOW-ID    PIC S9(4) COMP-5.
               10  SP2-WD-OWNR-ID      PIC S9(4) COMP-5.
               10  SP2-WD-GUI-ID       PIC S9(4) COMP-5.
               10  SP2-WD-GUI-ID2      PIC S9(4) COMP-5.
               10  SP2-WD-WIDTH        PIC S9(4) COMP-5.
               10  SP2-WD-HEIGHT       PIC S9(4) COMP-5.
               10  SP2-WD-ROW          PIC S9(4) COMP-5.
               10  SP2-WD-COL          PIC S9(4) COMP-5.
               10  SP2-WD-TOT-WIDTH    PIC S9(4) COMP-5.
               10  SP2-WD-TOT-HEIGHT   PIC S9(4) COMP-5.
               10  SP2-WD-HOR-DISP     PIC S9(4) COMP-5.
               10  SP2-WD-VERT-DISP    PIC S9(4) COMP-5.
               10  SP2-WD-TITLE-ROWS   PIC S9(4) COMP-5.
               10  SP2-WD-MENU-ROWS    PIC S9(4) COMP-5.
               10  SP2-WD-MENU-ID      PIC S9(4) COMP-5.
               10  SP2-WD-MENU-ID2     PIC S9(4) COMP-5.
               10  SP2-WD-CELL-WIDTH   PIC S9(4) COMP-5.
               10  SP2-WD-CELL-HEIGHT  PIC S9(4) COMP-5.
               10  SP2-WD-TOOLBAR-ROWS PIC S9(4) COMP-5.
      ******** SP2-WD-CHAR-DATA *******
               10  SP2-WD-NAME         PIC X(8).
               10  SP2-WD-PANEL-NAME   PIC X(8).
               10  SP2-WD-MENU-NAME    PIC X(8).
               10  SP2-WD-COLR         PIC X.
               10  SP2-WD-BOR-TYPE     PIC X.
               10  SP2-WD-INIT-SW      PIC X.
               10  SP2-WD-PAINT-SW     PIC X.
               10  SP2-WD-OPTS-SW      PIC X.
               10  SP2-WD-HIDE-SW      PIC X.
               10  SP2-WD-SBAR-SW      PIC X.
               10  SP2-WD-NO-TABS-SW   PIC X.
               10  SP2-WD-MORE-OPTIONS PIC X.
               10  SP2-WD-CELL-SIZE    PIC X.
               10  SP2-WD-DIV-WIDTH    PIC X.
               10  SP2-WD-DIV-HEIGHT   PIC X.
               10  SP2-WD-OPTIONS-3    PIC X.
               10  SP2-WD-SYSTEM-MENU  PIC X.
      ******** SP2-WD-VAR-DATA *******

       PROCEDURE DIVISION USING SP2-WINDOW-DEF.

       000-INICIO.


       000-99-FIM. GOBACK.

       END PROGRAM CWINDF.
