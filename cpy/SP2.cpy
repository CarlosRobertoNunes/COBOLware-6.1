      *********************
      * SP2.CPY contains: *
      * function codes    *
      * parameter layouts *
      * key values        *
      * line-drawing codes*
      *********************

      ***************************************************
      * CHANGES LOG                                     *
      * 4/17/96 toolbar function codes                  *
      * 4/17/96 sp2-wd-toolbar-rows                     *
      * 4/17/96 sp2-key-toolbar                         *
      * 5/6/96 vbx function codes                       *
      * 5/6/96 sp2-vbx-parm                             *
      * 5/6/96 sp2-key-vbx                              *
      * 5/14/96 sp2-pd-syns-len                         *
      * 5/18/96 sp2-cd-menu-option                      *
      * 8/2/96 sp2-pd-div-width, sp2-pd-div-height      *
      * 8/2/96 sp2-pd-help-len, sp2-pd-help-keyword     *
      * 8/2/96 sp2-fd-help-len, sp2-fd-help-keyword     *
      * 8/2/96 sp2-cd-next-hor, sp2-cd-last-hor         *
      * 8/2/96 sp2-delete-static                        *
      * 10/1/96 set-icon-file-name                      *
      * 10/23/96 sp2-mark-field                         *
      * 10/30/96 sp2-set-keyboard-buffer                *
      * 11/19/96 sp2-fd-misc-options                    *
      * 12/31/96 sp2-key-escape redefines sp2-key-esc   *
      * 1/2/96 remove unused items from menu-def        *
      * 1/31/96 sp2-reserve-memory                      *
      * 2/28/97 correct sp2-pd-var-len                  *
      * 4/22/97 sp2-fo-char-set                         *
      * 5/14/97 sp2-pd-cell-size, sp2-wd-cell-size      *
      * 8/7/97 sp2-get-field-data                       *
      * 8/18/98 sp2-set-configuration                   *
      * 9/2/98 sp2-execute-program                      *
      * 11/10/98 sp2-rx-long-data                       *
      * 11/10/98 sp2-get-repeat-ext                     *
      * 1/6/99 sp2-execute-client-prog                  *
      * 3/19/99 sp2-copy-file funcs                     *
      * 3/24/99 sp2-get-command-line                    *
      * 5/24/99 thin client funcs into sp2tc.cpy        *
      * 5/24/99 sp2-execute-program-2                   *
      * 6/3/99 pd-options-3                             *
      * 11/8/99 wd-options-3                            *
      * 11/16/99 rd-misc-options                        *
      * 2/8/00 wd-div-width/height                      *
      * 7/28/00 fo-decipoints                           *
      * 8/1/00 key-scroll-click                         *
      * 8/1/00 key-switch-denied                        *
      * 9/21/00 correct fo-num-len                      *
      * 9/21/00 increase pd-var-lens                    *
      * 3/14/01 property stuff                          *
      * 11/27/01 key-sys-shutdown, key-app-close        *
      * 1/2/02 pr-var-data comment                      *
      * 3/25/02 set-record                              *
      * 3/5/03 pr-var-len-l                             *
      * 5/6/03 deactivate-window                        *
      * 10/17/03 pd-options-4                           *
      * 11/12/04 fo-rotation                            *
      * 8/29/05 fd-anchor                               *
      * 10/5/05 wd-system-menu                          *
      * 1/4/06 gd-anchor,rd-anchor                      *
      * 12/11/06 bring-to-foreground                    *
      * 3/20/08 co-fg-bg (3)                            *
      * 4/2/08 fd-bor-colr                              *
      * 6/11/08 fd-options-3                            *
      * 6/30/08 key-window-closed, etc.                 *
      * 6/30/08 pd-options-5                            *
      * 7/8/08 pd-tab-options                           *
      * 12/10/08 longs in pd and fd                     *
      * 12/10/08 longs in pr                            *
      ***************************************************

      **********************
      * SP2 FUNCTION CODES *
      **********************

       01  SP2-FUNCTION-CODES.
      ******** primary functions ******
           05  SP2-OPEN-FILE           PIC S9(4) COMP-5 VALUE +1.
           05  SP2-CLOSE-WINDOW        PIC S9(4) COMP-5 VALUE +12.
           05  SP2-END-SESSION         PIC S9(4) COMP-5 VALUE +16.
           05  SP2-CONVERSE-PANEL      PIC S9(4) COMP-5 VALUE +19.
           05  SP2-CLOSE-FILE          PIC S9(4) COMP-5 VALUE +23.
      ******** window functions *******
           05  SP2-OPEN-WINDOW         PIC S9(4) COMP-5 VALUE +0.
           05  SP2-DISPLAY-WINDOW      PIC S9(4) COMP-5 VALUE +3.
           05  SP2-CLEAR-WINDOW        PIC S9(4) COMP-5 VALUE +11.
           05  SP2-ACTIVATE-WINDOW     PIC S9(4) COMP-5 VALUE +15.
           05  SP2-SET-WINDOW-DEF      PIC S9(4) COMP-5 VALUE +33.
           05  SP2-GET-WINDOW-DEF      PIC S9(4) COMP-5 VALUE +34.
           05  SP2-ACTIVATE-INTERNAL   PIC S9(4) COMP-5 VALUE +73.
           05  SP2-DEACTIVATE-WINDOW   PIC S9(4) COMP-5 VALUE +131.
           05  SP2-BRING-TO-FOREGROUND PIC S9(4) COMP-5 VALUE +139.
      ******** panel functions ********
           05  SP2-READ-PANEL          PIC S9(4) COMP-5 VALUE +2.
           05  SP2-GET-INPUT           PIC S9(4) COMP-5 VALUE +4.
           05  SP2-SET-PANEL-DEF       PIC S9(4) COMP-5 VALUE +5.
           05  SP2-GET-PANEL-DEF       PIC S9(4) COMP-5 VALUE +6.
           05  SP2-SET-PANEL-FIELDS    PIC S9(4) COMP-5 VALUE +13.
           05  SP2-WRITE-PANEL         PIC S9(4) COMP-5 VALUE +22.
           05  SP2-COPY-PANEL          PIC S9(4) COMP-5 VALUE +35.
           05  SP2-SET-PANEL-COLORS    PIC S9(4) COMP-5 VALUE +42.
           05  SP2-SET-PANEL-TYPES     PIC S9(4) COMP-5 VALUE +43.
           05  SP2-READ-NEXT-PANEL     PIC S9(4) COMP-5 VALUE +56.
           05  SP2-DELETE-RECORD       PIC S9(4) COMP-5 VALUE +60.
           05  SP2-DISPLAY-PANEL       PIC S9(4) COMP-5 VALUE +70.
           05  SP2-CLEAR-PANEL         PIC S9(4) COMP-5 VALUE +71.
           05  SP2-SET-RECORD          PIC S9(4) COMP-5 VALUE +128.
      ******** field functions ********
           05  SP2-SET-FIELD-DEF       PIC S9(4) COMP-5 VALUE +7.
           05  SP2-GET-FIELD-DEF       PIC S9(4) COMP-5 VALUE +8.
           05  SP2-DELETE-FIELD        PIC S9(4) COMP-5 VALUE +36.
           05  SP2-DISPLAY-FIELD       PIC S9(4) COMP-5 VALUE +58.
           05  SP2-GET-NEXT-FIELD-DEF  PIC S9(4) COMP-5 VALUE +67.
           05  SP2-MARK-FIELD          PIC S9(4) COMP-5 VALUE +84.
           05  SP2-GET-FIELD-DATA      PIC S9(4) COMP-5 VALUE +89.
      ******** static field functions *
           05  SP2-SET-STATIC-DEF      PIC S9(4) COMP-5 VALUE +9.
           05  SP2-GET-STATIC-DEF      PIC S9(4) COMP-5 VALUE +10.
           05  SP2-GET-NEXT-STATIC-DEF PIC S9(4) COMP-5 VALUE +68.
           05  SP2-DELETE-STATIC       PIC S9(4) COMP-5 VALUE +82.
      ******** group functions ********
           05  SP2-SET-GROUP-DEF       PIC S9(4) COMP-5 VALUE +28.
           05  SP2-GET-GROUP-DEF       PIC S9(4) COMP-5 VALUE +29.
           05  SP2-DELETE-GROUP        PIC S9(4) COMP-5 VALUE +39.
           05  SP2-GET-NEXT-GROUP-DEF  PIC S9(4) COMP-5 VALUE +69.
      ******** repeat group functions *
           05  SP2-SET-REPEAT-DEF      PIC S9(4) COMP-5 VALUE +20.
           05  SP2-GET-REPEAT-DEF      PIC S9(4) COMP-5 VALUE +21.
           05  SP2-DELETE-REPEAT       PIC S9(4) COMP-5 VALUE +37.
           05  SP2-SET-REPEAT-EXT      PIC S9(4) COMP-5 VALUE +57.
           05  SP2-GET-REPEAT-EXT      PIC S9(4) COMP-5 VALUE +106.
      ******** font functions *********
           05  SP2-SET-FONT-DEF        PIC S9(4) COMP-5 VALUE +30.
           05  SP2-GET-FONT-DEF        PIC S9(4) COMP-5 VALUE +31.
           05  SP2-WRITE-FONTS         PIC S9(4) COMP-5 VALUE +32.
           05  SP2-QUERY-FONT          PIC S9(4) COMP-5 VALUE +61.
           05  SP2-DELETE-FONT         PIC S9(4) COMP-5 VALUE +122.
      ******** color functions ********
           05  SP2-SET-COLOR-DEF       PIC S9(4) COMP-5 VALUE +40.
           05  SP2-WRITE-COLORS        PIC S9(4) COMP-5 VALUE +41.
           05  SP2-GET-COLOR-DEF       PIC S9(4) COMP-5 VALUE +45.
           05  SP2-QUERY-COLOR         PIC S9(4) COMP-5 VALUE +62.
           05  SP2-DELETE-COLOR        PIC S9(4) COMP-5 VALUE +123.
      ******** menu functions *********
           05  SP2-SET-MENU-DEF        PIC S9(4) COMP-5 VALUE +17.
           05  SP2-READ-MENU           PIC S9(4) COMP-5 VALUE +25.
           05  SP2-WRITE-MENU          PIC S9(4) COMP-5 VALUE +26.
           05  SP2-GET-MENU-DEF        PIC S9(4) COMP-5 VALUE +38.
           05  SP2-SET-MENU-OPTION     PIC S9(4) COMP-5 VALUE +46.
           05  SP2-GET-MENU-OPTION     PIC S9(4) COMP-5 VALUE +47.
           05  SP2-INSERT-MENU-OPTION  PIC S9(4) COMP-5 VALUE +51.
           05  SP2-DELETE-MENU-OPTION  PIC S9(4) COMP-5 VALUE +52.
           05  SP2-CLEAR-MENU          PIC S9(4) COMP-5 VALUE +55.
      ******** message functions ******
           05  SP2-DISPLAY-MESSAGE     PIC S9(4) COMP-5 VALUE +48.
           05  SP2-SET-MOUSE-SHAPE     PIC S9(4) COMP-5 VALUE +59.
      ******** clipboard functions ****
           05  SP2-SET-CLIPBOARD       PIC S9(4) COMP-5 VALUE +53.
           05  SP2-GET-CLIPBOARD       PIC S9(4) COMP-5 VALUE +54.
      ******** toolbar functions ******
           05  SP2-OPEN-TOOLBAR        PIC S9(4) COMP-5 VALUE +75.
           05  SP2-ACTIVATE-TOOLBAR    PIC S9(4) COMP-5 VALUE +76.
           05  SP2-DEACTIVATE-TOOLBAR  PIC S9(4) COMP-5 VALUE +77.
           05  SP2-DISPLAY-TOOLBAR     PIC S9(4) COMP-5 VALUE +78.
           05  SP2-CLOSE-TOOLBAR       PIC S9(4) COMP-5 VALUE +79.
      ******** file functions *********
           05  SP2-CREATE-FILE         PIC S9(4) COMP-5 VALUE +24.
           05  SP2-QUERY-FILE          PIC S9(4) COMP-5 VALUE +63.
           05  SP2-SET-ICON-FILE-NAME  PIC S9(4) COMP-5 VALUE +83.
      ******** miscellaneous functions
           05  SP2-CHECK-OLD-VERSION   PIC S9(4) COMP-5 VALUE +44.
           05  SP2-GET-VERSION         PIC S9(4) COMP-5 VALUE +64.
           05  SP2-SET-TEXT-MODE       PIC S9(4) COMP-5 VALUE +66.
           05  SP2-SET-KEYBOARD-BUFFER PIC S9(4) COMP-5 VALUE +85.
           05  SP2-RESERVE-MEMORY      PIC S9(4) COMP-5 VALUE +88.
           05  SP2-SET-CONFIGURATION   PIC S9(4) COMP-5 VALUE +92.
           05  SP2-SET-PROPERTY        PIC S9(4) COMP-5 VALUE +93.
           05  SP2-EXECUTE-PROGRAM-2   PIC S9(4) COMP-5 VALUE +97.
           05  SP2-GET-PROPERTY        PIC S9(4) COMP-5 VALUE +121.
           05  SP2-SET-NOTIFY-ICON     PIC S9(4) COMP-5 VALUE +141.
           05  SP2-EXECUTE-FILE        PIC S9(4) COMP-5 VALUE +142.
      ******** add-on functions *******
           05  SP2-SET-VBX             PIC S9(4) COMP-5 VALUE +80.
           05  SP2-GET-VBX             PIC S9(4) COMP-5 VALUE +81.

      *************************
      * END OF FUNCTION CODES *
      *************************

      *************************
      * SP2 PARAMETER LAYOUTS *
      *************************

      ******************************
      * file definition            *
      * parameter for OPEN-FILE    *
      * also used with CREATE-FILE *
      *                QUERY-FILE  *
      ******************************

       01  SP2-FILE-DEF.
           05  SP2-FI-RET-CODE         PIC S9(4) COMP-5.
           05  SP2-FI-LENS.
               10  SP2-FI-LEN-LEN      PIC S9(4) COMP-5 VALUE +10.
               10  SP2-FI-NUM-LEN      PIC S9(4) COMP-5 VALUE +0.
               10  SP2-FI-CHAR-LEN     PIC S9(4) COMP-5 VALUE +2.
               10  SP2-FI-VAR-LEN      PIC S9(4) COMP-5 VALUE +80.
               10  SP2-FI-NAME-LEN     PIC S9(4) COMP-5 VALUE +80.
           05  SP2-FI-DATA.
      ******** SP2-FI-CHAR-DATA *******
               10  SP2-FI-MODE         PIC X.
               10  SP2-FI-SHARE        PIC X.
      ******** SP2-FI-VAR-DATA ********
               10  SP2-FI-NAME         PIC X(80).

      ********************************
      * parameter for CONVERSE-PANEL *
      * also used with GET-INPUT     *
      ********************************

      **************************************************************
      * this parameter will only be used in this form if you are   *
      * defining panels dynamically.  Otherwise you should use the *
      * parameter layout generated by the panel editor.  GET-INPUT *
      * actually only uses the data up to the end of IP-CHAR-DATA. *
      * FIELD-LEN, etc. are set to zero because it is usually      *
      * easier to define data areas for panels separately and use  *
      * SET-PANEL-FIELDS, etc. to identify them to SP2.  This      *   
      * allows you to use the same CONVERSE-DATA area for multiple *
      * panels yet have separate data areas for the panels (these  * 
      * data areas must be kept intact while the corresponding     *
      * windows are open)                                          *  
      **************************************************************

       01  SP2-CONVERSE-DATA.
           05  SP2-CD-RET-CODE         PIC S9(4) COMP-5.
           05  SP2-CD-LENS.
               10  SP2-CD-LEN-LEN      PIC S9(4) COMP-5 VALUE +20.
               10  SP2-CD-IP-NUM-LEN   PIC S9(4) COMP-5 VALUE +40.
               10  SP2-CD-IP-CHAR-LEN  PIC S9(4) COMP-5 VALUE +106.
               10  SP2-CD-OP-NUM-LEN   PIC S9(4) COMP-5 VALUE +6.
               10  SP2-CD-OP-CHAR-LEN  PIC S9(4) COMP-5 VALUE +2.
               10  SP2-CD-FIELD-LEN    PIC S9(4) COMP-5 VALUE +0.
               10  SP2-CD-COLR-LEN     PIC S9(4) COMP-5 VALUE +0.
               10  SP2-CD-TYPE-LEN     PIC S9(4) COMP-5 VALUE +0.
               10  FILLER              PIC S9(4) COMP-5 VALUE +0.
               10  FILLER              PIC S9(4) COMP-5 VALUE +0.
           05  SP2-CD-DATA.
      ******** SP2-CD-IP-NUM-DATA *****
               10  SP2-CD-KEY          PIC S9(4) COMP-5.
               10  SP2-CD-NEXT-FLD-ID  PIC S9(4) COMP-5.
               10  SP2-CD-NEXT-FLD-NUM PIC S9(4) COMP-5.
               10  SP2-CD-NEXT-TAB-NUM PIC S9(4) COMP-5.
               10  SP2-CD-NEXT-OCCURS  PIC S9(4) COMP-5.
               10  SP2-CD-LAST-FLD-ID  PIC S9(4) COMP-5.
               10  SP2-CD-LAST-FLD-NUM PIC S9(4) COMP-5.
               10  SP2-CD-LAST-TAB-NUM PIC S9(4) COMP-5.
               10  SP2-CD-LAST-OCCURS  PIC S9(4) COMP-5.
               10  SP2-CD-MENU-ID      PIC S9(4) COMP-5.
               10  SP2-CD-CTRL-FIELD-KEY REDEFINES SP2-CD-MENU-ID
                                       PIC S9(4) COMP-5. 
               10  SP2-CD-BUTTON-ID REDEFINES SP2-CD-MENU-ID
                                       PIC S9(4) COMP-5. 
               10  SP2-CD-ROW-COL-SW   PIC S9(4) COMP-5.
               10  SP2-CD-CURSOR-ROW   PIC S9(4) COMP-5.
               10  SP2-CD-CURSOR-COL   PIC S9(4) COMP-5.
               10  SP2-CD-LAST-ROW     PIC S9(4) COMP-5.
               10  SP2-CD-LAST-COL     PIC S9(4) COMP-5.
               10  SP2-CD-DISP-SW      PIC S9(4) COMP-5.
               10  SP2-CD-NEXT-VERT    PIC S9(4) COMP-5.
               10  SP2-CD-LAST-VERT    PIC S9(4) COMP-5.
               10  SP2-CD-NEXT-HOR     PIC S9(4) COMP-5.
               10  SP2-CD-LAST-HOR     PIC S9(4) COMP-5.
      ******** SP2-CD-IP-CHAR-DATA ****
               10  SP2-CD-NEXT-PANEL   PIC X(8).
               10  SP2-CD-NEXT-FIELD   PIC X(30).
               10  SP2-CD-LAST-FIELD   PIC X(30).
               10  SP2-CD-MENU-OPTION  PIC X(30).
               10  SP2-CD-SWITCH-SW    PIC X.
               10  SP2-CD-SIZE-SW      PIC X.
               10  SP2-CD-MOUSE-SW     PIC X.
               10  SP2-CD-CAPTURE-SW   PIC X.
               10  SP2-CD-WAIT-SW      PIC X.
               10  SP2-CD-CURS-SW      PIC X.
               10  SP2-CD-CHG-SW       PIC X.
               10  SP2-CD-TIMEOUT      PIC X.
      ******** SP2-CD-OP-NUM-DATA *****
               10  SP2-CD-PAN-POS-SW   PIC S9(4) COMP-5.
               10  SP2-CD-PAN-ROW      PIC S9(4) COMP-5.
               10  SP2-CD-PAN-COL      PIC S9(4) COMP-5.
      ******** SP2-CD-OP-CHAR-DATA ****
               10  SP2-CD-NEW-WINDOW   PIC X.
               10  SP2-CD-DISPLAY-SW   PIC X.
      ******** SP2-CD-FIELD-DATA ******
      ******** SP2-CD-COLR-DATA *******
      ******** SP2-CD-TYPE-DATA *******

      *********************************
      * null parameter                *
      * parameter for CLOSE-WINDOW    *
      *               CLOSE-FILE      *
      *               END-SESSION     *
      * also used with DISPLAY-WINDOW *
      *                CLEAR-WINDOW   *
      *                WRITE-PANEL    *
      *                WRITE-MENU     *
      *                WRITE-FONTS    *
      *                WRITE-COLORS   *
      *                CLEAR-MENU     *
      *                SET-MOUSE-SHAPE*
      *********************************

       01  SP2-NULL-PARM.
           05  SP2-NP-RET-CODE         PIC S9(4) COMP-5.

      *********************************
      * window definition             *
      * parameter for OPEN-WINDOW     *
      * also used with GET-WINDOW-DEF *
      *                SET-WINDOW-DEF *
      *********************************

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
               10  SP2-WD-TITLE        PIC X(80).

      **********************************
      * name definition                *
      * parameter for READ-PANEL       *
      *               ACTIVATE-WINDOW  *
      *               ACTIVATE-PANEL   *
      *               READ-MENU        *
      * also used with COPY-PANEL      *
      *                READ-NEXT-PANEL *
      **********************************

       01  SP2-NAME-DEF.
           05  SP2-ND-RET-CODE         PIC S9(4) COMP-5.
           05  SP2-ND-NAME             PIC X(8).

      ************************************
      * menu option definition           *
      * parameter for GET-MENU-OPTION    *
      *               SET-MENU-OPTION    *
      *               INSERT-MENU-OPTION *
      *               DELETE-MENU-OPTION *
      ************************************

       01  SP2-MENU-OPTION.
           05  SP2-MO-RET-CODE         PIC S9(4) COMP-5.
           05  SP2-MO-LENS.
               10  SP2-MO-LEN-LEN      PIC S9(4) COMP-5 VALUE +12.
               10  SP2-MO-NUM-LEN      PIC S9(4) COMP-5 VALUE +6.
               10  SP2-MO-CHAR-LEN     PIC S9(4) COMP-5 VALUE +2.
               10  SP2-MO-VAR-LEN      PIC S9(4) COMP-5 VALUE +60.
               10  SP2-MO-NAME-LEN     PIC S9(4) COMP-5 VALUE +30.
               10  SP2-MO-TEXT-LEN     PIC S9(4) COMP-5 VALUE +30.
           05  SP2-MO-DATA.
      ******** SP2-MO-NUM-DATA ********
               10  SP2-MO-ID           PIC S9(4) COMP-5.
               10  SP2-MO-OWNR-ID      PIC S9(4) COMP-5.
               10  SP2-MO-ACC-KEY      PIC S9(4) COMP-5.
      ******** SP2-MO-CHAR-DATA *******
               10  SP2-MO-TYPE         PIC X.
               10  SP2-MO-STATE        PIC X.
      ******** SP2-MO-VAR-DATA *******
               10  SP2-MO-NAME         PIC X(30).
               10  SP2-MO-TEXT         PIC X(30).

      *********************************
      * message definition            *
      * parameter for DISPLAY-MESSAGE *
      *********************************

       01  SP2-MESSAGE-DATA.
           05  SP2-MS-RET-CODE         PIC S9(4) COMP-5.
           05  SP2-MS-LENS.
               10  SP2-MS-LEN-LEN      PIC S9(4) COMP-5 VALUE +12.
               10  SP2-MS-NUM-LEN      PIC S9(4) COMP-5 VALUE +2.
               10  SP2-MS-CHAR-LEN     PIC S9(4) COMP-5 VALUE +4.
               10  SP2-MS-VAR-LEN      PIC S9(4) COMP-5 VALUE +160.
               10  SP2-MS-TITLE-LEN    PIC S9(4) COMP-5 VALUE +80.
               10  SP2-MS-LINE-LEN     PIC S9(4) COMP-5 VALUE +80.
           05  SP2-MS-DATA.
      ******** SP2-MS-NUM-DATA ********
               10  SP2-MS-LINE-CNT     PIC S9(4) COMP-5.
      ******** SP2-MS-CHAR-DATA *******
               10  SP2-MS-ICON         PIC X.
               10  SP2-MS-BUTTON       PIC X.
               10  SP2-MS-CANCEL       PIC X.
               10  SP2-MS-REPLY        PIC X.
      ******** SP2-MS-VAR-DATA *******
               10  SP2-MS-TITLE        PIC X(80).
               10  SP2-MS-TEXT         PIC X(80).

      *******************************
      * clipboard data definition   *
      * parameter for SET-CLIPBOARD *
      *               GET-CLIPBOARD *
      *******************************

       01  SP2-CLIPBOARD-DATA.
           05  SP2-CB-RET-CODE         PIC S9(4) COMP-5.
           05  SP2-CB-LENS.
               10  SP2-CB-LEN-LEN      PIC S9(4) COMP-5 VALUE +10.
               10  SP2-CB-NUM-LEN      PIC S9(4) COMP-5 VALUE +2.
               10  SP2-CB-CHAR-LEN     PIC S9(4) COMP-5 VALUE +0.
               10  SP2-CB-VAR-LEN      PIC S9(4) COMP-5 VALUE +80.
               10  SP2-CB-TEXT-LEN     PIC S9(4) COMP-5 VALUE +80.
           05  SP2-CB-DATA.
      ******** SP2-CB-NUM-DATA ********
               10  FILLER              PIC S9(4) COMP-5.
      ******** SP2-CB-VAR-DATA *******
               10  SP2-CB-TEXT         PIC X(80).

      ********************************
      * repeat extension data        *
      * parameter for SET-REPEAT-EXT *
      ********************************

       01  SP2-REPEAT-EXT.
           05  SP2-RX-RET-CODE         PIC S9(4) COMP-5.
           05  SP2-RX-LENS.
               10  SP2-RX-LEN-LEN      PIC S9(4) COMP-5 VALUE +18.
               10  SP2-RX-NUM-LEN      PIC S9(4) COMP-5 VALUE +20.
               10  SP2-RX-CHAR-LEN     PIC S9(4) COMP-5 VALUE +0.
               10  SP2-RX-PTR-LEN      PIC S9(4) COMP-5 VALUE +12.
               10  SP2-RX-FIELD-LEN    PIC S9(4) COMP-5 VALUE +0.
               10  SP2-RX-COLR-LEN     PIC S9(4) COMP-5 VALUE +0.
               10  SP2-RX-TYPE-LEN     PIC S9(4) COMP-5 VALUE +0.
               10  SP2-RX-LONG-LEN     PIC S9(4) COMP-5 VALUE +16.
               10  FILLER              PIC S9(4) COMP-5.
           05  SP2-RX-DATA.
      ******** SP2-RX-NUM-DATA ********
               10  SP2-RX-ID           PIC S9(4) COMP-5.
               10  SP2-RX-NEXT-OCC     PIC S9(4) COMP-5.
               10  SP2-RX-DISP-SW      PIC S9(4) COMP-5.
               10  SP2-RX-NEW-DISP     PIC S9(4) COMP-5.
               10  SP2-RX-BLOCK-SW     PIC S9(4) COMP-5.
               10  SP2-RX-BLOCK-DISP   PIC S9(4) COMP-5.
               10  SP2-RX-BLOCK-OCCS   PIC S9(4) COMP-5.
               10  SP2-RX-TOTAL-SW     PIC S9(4) COMP-5.
               10  SP2-RX-TOTAL-OCCS   PIC S9(4) COMP-5.
               10  FILLER              PIC S9(4) COMP-5.
      ******** SP2-RX-PTR-DATA ********
               10  SP2-RX-FIELD-PTR    POINTER.
               10  SP2-RX-COLR-PTR     POINTER.
               10  SP2-RX-TYPE-PTR     POINTER.
      ******** SP2-RX-FIELD-DATA ******
      ******** SP2-RX-COLR-DATA *******
      ******** SP2-RX-TYPE-DATA *******
      ******** SP2-RX-LONG-DATA *******
               10  SP2-RX-NEXT-OCC-L   PIC S9(8) COMP-5.
               10  SP2-RX-NEW-DISP-L   PIC S9(8) COMP-5.
               10  SP2-RX-BLOCK-DISP-L PIC S9(8) COMP-5.
               10  SP2-RX-TOTAL-OCCS-L PIC S9(8) COMP-5.

      ********************************
      * vbx property data            *
      * parameter for SET-VBX        *
      ********************************

       01  SP2-VBX-PARM.
           05  SP2-VB-RET-CODE         PIC S9(4) COMP-5.
           05  SP2-VB-LENS.
               10  SP2-VB-LEN-LEN      PIC S9(4) COMP-5 VALUE +8.
               10  SP2-VB-NUM-LEN      PIC S9(4) COMP-5 VALUE +6.
               10  SP2-VB-CHAR-LEN     PIC S9(4) COMP-5 VALUE +30.
               10  SP2-VB-VAR-LEN      PIC S9(4) COMP-5 VALUE +80.
           05  SP2-VB-DATA.
      ******** SP2-VB-NUM-DATA ********
               10  SP2-VB-ID           PIC S9(4) COMP-5.
               10  SP2-VB-PROP         PIC S9(4) COMP-5.
               10  SP2-VB-INDEX        PIC S9(4) COMP-5.
      ******** SP2-VB-CHAR-DATA *******
               10  SP2-VB-PROP-NAME    PIC X(30).
      ******** SP2-VB-VAR-DATA ********
               10  SP2-VB-VALUE        PIC X(80).

      ********************************
      * panel definition             *
      * parameter for GET-PANEL-DEF  *
      * also used with SET-PANEL-DEF *
      ********************************

       01  SP2-PANEL-DEF.
           05  SP2-PD-RET-CODE         PIC S9(4) COMP-5.
           05  SP2-PD-LENS.
               10  SP2-PD-LEN-LEN      PIC S9(4) COMP-5 VALUE +30.
               10  SP2-PD-NUM-LEN      PIC S9(4) COMP-5 VALUE +48.
               10  SP2-PD-CHAR-LEN     PIC S9(4) COMP-5 VALUE +76.
               10  SP2-PD-VAR-LEN      PIC S9(4) COMP-5 VALUE +291.
               10  SP2-PD-DESC-LEN     PIC S9(4) COMP-5 VALUE +40.
               10  SP2-PD-TITLE-LEN    PIC S9(4) COMP-5 VALUE +40.
               10  SP2-PD-CURS-KEY-LEN PIC S9(4) COMP-5 VALUE +50.
               10  SP2-PD-CTRL-KEY-LEN PIC S9(4) COMP-5 VALUE +40.
               10  SP2-PD-SYNS-LEN     PIC S9(4) COMP-5 VALUE +0.
               10  SP2-PD-MSG-TEXT-LEN PIC S9(4) COMP-5 VALUE +80.
               10  SP2-PD-USER-LEN     PIC S9(4) COMP-5 VALUE +1.
               10  SP2-PD-HELP-LEN     PIC S9(4) COMP-5 VALUE +40.
               10  FILLER              PIC S9(4) COMP-5 VALUE +0.
               10  SP2-PD-LENL-LEN     PIC S9(4) COMP-5 VALUE -1.
               10  SP2-PD-NUML-LEN     PIC S9(4) COMP-5 VALUE +40.
           05  SP2-PD-DATA.
      ******** SP2-PD-NUM-DATA ********
               10  SP2-PD-WIDTH        PIC S9(4) COMP-5.
               10  SP2-PD-HEIGHT       PIC S9(4) COMP-5.
               10  SP2-PD-ROW          PIC S9(4) COMP-5.
               10  SP2-PD-COL          PIC S9(4) COMP-5.
               10  SP2-PD-FLD-CNT      PIC S9(4) COMP-5.
               10  SP2-PD-PROG-CNT     PIC S9(4) COMP-5.
               10  SP2-PD-PROG-LEN     PIC S9(4) COMP-5.
               10  SP2-PD-HELP-KEY     PIC S9(4) COMP-5.
               10  SP2-PD-EDIT-OV-KEY  PIC S9(4) COMP-5.
               10  SP2-PD-MSG-REFRESH  PIC S9(4) COMP-5.
               10  SP2-PD-TITLE-ROWS   PIC S9(4) COMP-5.
               10  SP2-PD-DEF-PB       PIC S9(4) COMP-5.
               10  FILLER              PIC S9(4) COMP-5.
               10  SP2-PD-MENU-ROWS    PIC S9(4) COMP-5.
               10  SP2-PD-TOT-WIDTH    PIC S9(4) COMP-5.
               10  SP2-PD-TOT-HEIGHT   PIC S9(4) COMP-5.
               10  SP2-PD-MSG-LEN      PIC S9(4) COMP-5.
               10  SP2-PD-CELL-WIDTH   PIC S9(4) COMP-5.
               10  SP2-PD-CELL-HEIGHT  PIC S9(4) COMP-5.
               10  SP2-PD-FONT-ID      PIC S9(4) COMP-5.
               10  SP2-PD-PROG-LEN-L   PIC S9(8) COMP-5.
               10  SP2-PD-PROG-CNT-L   PIC S9(8) COMP-5.
      ******** SP2-PD-CHAR-DATA *******
               10  SP2-PD-NAME         PIC X(8).
               10  SP2-PD-MENU-NAME    PIC X(8).
               10  FILLER              PIC X.
               10  SP2-PD-CUR-FLD-COLR PIC X.
               10  SP2-PD-CURS-SKIP    PIC X.
               10  SP2-PD-CURS-SHOW    PIC X.
               10  SP2-PD-CURS-IN-FLD  PIC X.
               10  SP2-PD-SHIFT-NUMS   PIC X.
               10  SP2-PD-BLANK-NUMS   PIC X. 
               10  SP2-PD-ASSUME-DEC   PIC X. 
               10  SP2-PD-FORMAT-NUMS  PIC X. 
               10  SP2-PD-CURS-WRAP    PIC X. 
               10  SP2-PD-INIT-NUMS    PIC X. 
               10  SP2-PD-OVERRIDE-REQ PIC X. 
               10  SP2-PD-CELL-SIZE    PIC X. 
               10  FILLER              PIC X. 
               10  SP2-PD-MISC-OPTIONS PIC X. 
               10  SP2-PD-DIV-WIDTH    PIC X. 
               10  SP2-PD-DIV-HEIGHT   PIC X. 
               10  FILLER              PIC X. 
               10  FILLER              PIC X. 
               10  FILLER              PIC X. 
               10  FILLER              PIC X. 
               10  FILLER              PIC X. 
               10  SP2-PD-COLR         PIC X. 
               10  SP2-PD-PROG-DATE    PIC X. 
               10  SP2-PD-HELP         PIC X(8).
               10  SP2-PD-TEXT-OPTIONS PIC X.
               10  FILLER              PIC X(8).
               10  SP2-PD-TOOLBAR-NAME PIC X(8).
               10  SP2-PD-MSG-COLR     PIC X.
               10  SP2-PD-MORE-OPTIONS PIC X.
               10  SP2-PD-OPTIONS-3    PIC X.
               10  SP2-PD-OPTIONS-4    PIC X.
               10  SP2-PD-OPTIONS-5    PIC X.
               10  SP2-PD-TAB-OPTIONS  PIC X.
               10  FILLER              PIC X.
               10  FILLER              PIC X.
               10  FILLER              PIC X.
               10  SP2-PD-WIN-BOR-TYPE PIC X.
               10  SP2-PD-INITIAL-SW   PIC X.
      ******** SP2-PD-VAR-DATA ********
               10  SP2-PD-DESCRIPTION  PIC X(40).
               10  SP2-PD-TITLE        PIC X(40).
               10  SP2-PD-CURS-KEYS.
                   15  SP2-PD-LEFT     PIC S9(4) COMP-5.
                   15  SP2-PD-RIGHT    PIC S9(4) COMP-5.
                   15  SP2-PD-UP       PIC S9(4) COMP-5.
                   15  SP2-PD-DOWN     PIC S9(4) COMP-5.
                   15  SP2-PD-TAB      PIC S9(4) COMP-5.
                   15  SP2-PD-BACKTAB  PIC S9(4) COMP-5.
                   15  SP2-PD-TB-ERASE PIC S9(4) COMP-5.
                   15  SP2-PD-BT-ERASE PIC S9(4) COMP-5.
                   15  SP2-PD-DELETE   PIC S9(4) COMP-5.
                   15  SP2-PD-BACKSPAC PIC S9(4) COMP-5.
                   15  SP2-PD-ERASE    PIC S9(4) COMP-5.
                   15  SP2-PD-INSERT   PIC S9(4) COMP-5.
                   15  SP2-PD-HOME     PIC S9(4) COMP-5.
                   15  SP2-PD-END      PIC S9(4) COMP-5.
                   15  SP2-PD-SCRL-UP  PIC S9(4) COMP-5.
                   15  SP2-PD-SCRL-DN  PIC S9(4) COMP-5.
                   15  SP2-PD-SCRL-LT  PIC S9(4) COMP-5.
                   15  SP2-PD-SCRL-RT  PIC S9(4) COMP-5.
                   15  FILLER          PIC S9(4) COMP-5.
                   15  FILLER          PIC S9(4) COMP-5.
                   15  FILLER          PIC S9(4) COMP-5.
                   15  FILLER          PIC S9(4) COMP-5.
                   15  FILLER          PIC S9(4) COMP-5.
                   15  SP2-PD-HOME-PAN PIC S9(4) COMP-5.
                   15  SP2-PD-END-PAN  PIC S9(4) COMP-5.
               10  SP2-PD-CTRL-KEYS.
                   15  SP2-PD-CTRL-KEY OCCURS 20  
                                       PIC S9(4) COMP-5.
               10  SP2-PD-MSG-TEXT     PIC X(80).
               10  SP2-PD-USER-DATA    PIC X.
               10  SP2-PD-HELP-KEYWORD PIC X(40).
     
      *********************************
      * static definition             *
      * parameter for GET-STATIC-DEF  *
      * also used with SET-STATIC-DEF *
      *********************************

       01  SP2-STATIC-DEF.
           05  SP2-SD-RET-CODE         PIC S9(4) COMP-5.
           05  SP2-SD-LENS.
               10  SP2-SD-LEN-LEN      PIC S9(4) COMP-5 VALUE +10.
               10  SP2-SD-NUM-LEN      PIC S9(4) COMP-5 VALUE +12.
               10  SP2-SD-CHAR-LEN     PIC S9(4) COMP-5 VALUE +4.
               10  SP2-SD-VAR-LEN      PIC S9(4) COMP-5 VALUE +80.
               10  SP2-SD-TEXT-LEN     PIC S9(4) COMP-5 VALUE +80.
           05  SP2-SD-DATA.
      ******** SP2-SD-NUM-DATA ********
               10  SP2-SD-ID           PIC S9(4) COMP-5.
               10  SP2-SD-ROW          PIC S9(4) COMP-5.
               10  SP2-SD-COL          PIC S9(4) COMP-5.
               10  SP2-SD-WIDTH        PIC S9(4) COMP-5.
               10  SP2-SD-HEIGHT       PIC S9(4) COMP-5.
               10  SP2-SD-FONT-ID      PIC S9(4) COMP-5.
      ******** SP2-SD-CHAR-DATA *******
               10  SP2-SD-COLR         PIC X.
               10  SP2-SD-TYPE         PIC X.
               10  SP2-SD-JUSTIFY      PIC X.
               10  SP2-SD-MISC-OPTIONS PIC X.
      ******** SP2-SD-VAR-DATA ********
               10  SP2-SD-TEXT         PIC X(80).

      ********************************
      * field definition             *
      * parameter for GET-FIELD-DEF  *
      * also used with SET-FIELD-DEF *
      *                DELETE-FIELD  *
      ********************************

      **************************************************************
      * the variable portion of this parameter is set up slightly  * 
      * differently to the other information parameters.  This     *
      * variable data is defined as one contiguous block of length *
      * 2000.  This is because the data can vary so much depending *
      * on the field type.  Individual data items within the block *
      * should be extracted based on the corresponding length      *
      * values.  SP2 knows to reset these individual lengths to    * 
      * their proper values if they are all originally passed as   *
      * zero.  All the information parameters can be handled this  *
      * way if you prefer.  If you are using this parameter for    *
      * maintenance you should set the length values yourself so   *
      * so that they correspond to the actual variable data that   *
      * you are passing.                                           *
      **************************************************************

       01  SP2-FIELD-DEF.
           05  SP2-FD-RET-CODE         PIC S9(4) COMP-5.
           05  SP2-FD-LENS.
               10  SP2-FD-LEN-LEN      PIC S9(4) COMP-5 VALUE +30.
               10  SP2-FD-NUM-LEN      PIC S9(4) COMP-5 VALUE +52.
               10  SP2-FD-CHAR-LEN     PIC S9(4) COMP-5 VALUE +74.
               10  SP2-FD-VAR-LEN      PIC S9(4) COMP-5 VALUE +2000.
               10  SP2-FD-VAR-LENS.
                   15  SP2-FD-FORMAT-LEN   
                                       PIC S9(4) COMP-5 VALUE +0.
                   15  SP2-FD-CAPTION-LEN  
                                       PIC S9(4) COMP-5 VALUE +0.
                   15  SP2-FD-INITIAL-LEN  
                                       PIC S9(4) COMP-5 VALUE +0.
                   15  FILLER          PIC S9(4) COMP-5 VALUE +0.
                   15  SP2-FD-RANGE-LEN    
                                       PIC S9(4) COMP-5 VALUE +0.
                   15  SP2-FD-DISCRETE-LEN 
                                       PIC S9(4) COMP-5 VALUE +0.
                   15  SP2-FD-MSG-TEXT-LEN 
                                       PIC S9(4) COMP-5 VALUE +0.
                   15  SP2-FD-USER-LEN PIC S9(4) COMP-5 VALUE +0.
                   15  SP2-FD-HELP-LEN PIC S9(4) COMP-5 VALUE +0.
               10  SP2-FD-LENL-LEN     PIC S9(4) COMP-5 VALUE -1.
               10  SP2-FD-NUML-LEN     PIC S9(4) COMP-5 VALUE +44.
           05  SP2-FD-DATA.
      ******** SP2-FD-NUM-DATA ********
               10  SP2-FD-ID           PIC S9(4) COMP-5.
               10  SP2-FD-GUI-ID       PIC S9(4) COMP-5.
               10  SP2-FD-GUI-ID-2     PIC S9(4) COMP-5.
               10  SP2-FD-OCCURRENCE   PIC S9(4) COMP-5.
               10  SP2-FD-BASE-ID      PIC S9(4) COMP-5.
               10  SP2-FD-ROW          PIC S9(4) COMP-5.
               10  SP2-FD-COL          PIC S9(4) COMP-5.
               10  SP2-FD-PROG-OFF     PIC S9(4) COMP-5.
               10  SP2-FD-FLD-NUM      PIC S9(4) COMP-5.
               10  SP2-FD-TAB-NUM      PIC S9(4) COMP-5.
               10  SP2-FD-PROG-NUM     PIC S9(4) COMP-5.
               10  SP2-FD-WIDTH        PIC S9(4) COMP-5.
               10  SP2-FD-HEIGHT       PIC S9(4) COMP-5.
               10  SP2-FD-MAX-LEN      PIC S9(4) COMP-5.
               10  SP2-FD-PROG-LEN     PIC S9(4) COMP-5.
               10  SP2-FD-ITEM-LEN     PIC S9(4) COMP-5.
               10  FILLER              PIC S9(4) COMP-5.
               10  SP2-FD-HELP-KEY     PIC S9(4) COMP-5.
               10  FILLER              PIC S9(4) COMP-5.
               10  SP2-FD-GROUP-ID     PIC S9(4) COMP-5.
               10  SP2-FD-REPEAT-ID    PIC S9(4) COMP-5.
               10  SP2-FD-FONT-ID      PIC S9(4) COMP-5.
               10  SP2-FD-PROG-OFF-L   PIC S9(8) COMP-5.
               10  SP2-FD-PROG-NUM-L   PIC S9(8) COMP-5.
      ******** SP2-FD-CHAR-DATA *******
               10  SP2-FD-NAME         PIC X(30).
               10  SP2-FD-TYPE         PIC X.
               10  SP2-FD-OUTPUT       PIC X.
               10  SP2-FD-PROG-DEC     PIC X.
               10  FILLER              PIC X(4).
               10  SP2-FD-INIT-NUMS    PIC X. 
               10  SP2-FD-MISC-OPTIONS PIC X. 
               10  FILLER              PIC X.
               10  SP2-FD-HELP         PIC X(8).
               10  SP2-FD-MORE-OPTIONS PIC X.
               10  SP2-FD-BOR-COLR     PIC X.
               10  SP2-FD-ANCHOR       PIC X.
               10  SP2-FD-OPTIONS-3    PIC X.
               10  FILLER              PIC X(4).
               10  SP2-FD-REQUIRED     PIC X.
               10  SP2-FD-PROG-CTRL    PIC X.
               10  SP2-FD-JUSTIFY      PIC X.
               10  SP2-FD-FILL         PIC X.
               10  SP2-FD-ASSUME-DEC   PIC X.
               10  SP2-FD-SPEC-FMT     PIC X.
               10  SP2-FD-CASE         PIC X.
               10  SP2-FD-IMBED-BLANKS PIC X.
               10  SP2-FD-CUR-COLR     PIC X.
               10  SP2-FD-CURS-SKIP    PIC X.
               10  SP2-FD-CURS-SHOW    PIC X.
               10  SP2-FD-BLANK-FIRST  PIC X.
               10  SP2-FD-BLANK-ZERO   PIC X.
               10  SP2-FD-CTRL-TYPE    PIC X.
               10  SP2-FD-COLR         PIC X.
               10  SP2-FD-MNEMONIC     PIC X.
               10  SP2-FD-BOR-TYPE     PIC X.
               10  SP2-FD-PROG-SPEC    PIC X.
      ******** SP2-FD-VAR-DATA ********
               10  SP2-FD-VAR-DATA     PIC X(2000).
      ******** SP2-FD-FMT *************       
      ******** SP2-FD-CAPTION *********
      ******** SP2-FD-INITIAL-VAL *****       
      ******** SP2-FD-RANGE-VALS ******       
      ******** SP2-FD-DISC-VALS *******      
      ******** SP2-FD-MSG-TEXT ********      
      ******** SP2-FD-USER-DATA *******      
      ******** SP2-FD-HELP-KEYWORD ****      

      ********************************
      * group definition             *
      * parameter for GET-GROUP-DEF  *
      * also used with SET-GROUP-DEF *
      *                DELETE-GROUP  *
      ********************************

       01  SP2-GROUP-DEF.
           05  SP2-GD-RET-CODE         PIC S9(4) COMP-5.
           05  SP2-GD-LENS.
               10  SP2-GD-LEN-LEN      PIC S9(4) COMP-5 VALUE +12.
               10  SP2-GD-NUM-LEN      PIC S9(4) COMP-5 VALUE +14.
               10  SP2-GD-CHAR-LEN     PIC S9(4) COMP-5 VALUE +48.
               10  SP2-GD-VAR-LEN      PIC S9(4) COMP-5 VALUE +50.
               10  SP2-GD-ID-LEN       PIC S9(4) COMP-5 VALUE +20.
               10  SP2-GD-TITLE-LEN    PIC S9(4) COMP-5 VALUE +30. 
           05  SP2-GD-DATA.
      ******** SP2-GD-NUM-DATA ********
               10  SP2-GD-ID           PIC S9(4) COMP-5.
               10  SP2-GD-ROW          PIC S9(4) COMP-5.
               10  SP2-GD-COL          PIC S9(4) COMP-5.
               10  SP2-GD-WIDTH        PIC S9(4) COMP-5.
               10  SP2-GD-HEIGHT       PIC S9(4) COMP-5.
               10  SP2-GD-FLD-CNT      PIC S9(4) COMP-5.
               10  SP2-GD-FONT-ID      PIC S9(4) COMP-5.
      ******** SP2-GD-CHAR-DATA *******
               10  SP2-GD-NAME         PIC X(30).
               10  SP2-GD-TAB-WITHIN   PIC X.
               10  SP2-GD-CUR-COLR     PIC X.
               10  FILLER              PIC X.
               10  SP2-GD-SELECT-TYPE  PIC X.
               10  FILLER              PIC X.
               10  FILLER              PIC X.
               10  FILLER              PIC X(3).
               10  FILLER              PIC X(3).
               10  FILLER              PIC X.
               10  FILLER              PIC X.
               10  SP2-GD-COLR         PIC X.
               10  SP2-GD-ANCHOR       PIC X.
               10  SP2-GD-BOR-TYPE     PIC X.
               10  FILLER              PIC X.
      ******** SP2-GD-VAR-DATA ********
               10  SP2-GD-FLD-ID OCCURS 10       
                                       PIC S9(4) COMP-5.
               10  SP2-GD-TITLE        PIC X(30).

      *********************************
      * repeat definition             *
      * parameter for GET-REPEAT-DEF  *
      * also used with SET-REPEAT-DEF *
      *                DELETE-REREAT  *
      *********************************

       01  SP2-REPEAT-DEF.
           05  SP2-RD-RET-CODE         PIC S9(4) COMP-5.
           05  SP2-RD-LENS.
               10  SP2-RD-LEN-LEN      PIC S9(4) COMP-5 VALUE +10.
               10  SP2-RD-NUM-LEN      PIC S9(4) COMP-5 VALUE +44.
               10  SP2-RD-CHAR-LEN     PIC S9(4) COMP-5 VALUE +4.
               10  SP2-RD-VAR-LEN      PIC S9(4) COMP-5 VALUE +104.
               10  SP2-RD-BASE-LEN     PIC S9(4) COMP-5 VALUE +20.
           05  SP2-RD-DATA.
      ******** SP2-RD-NUM-DATA ********
               10  SP2-RD-ID           PIC S9(4) COMP-5.
               10  SP2-RD-ROW          PIC S9(4) COMP-5.
               10  SP2-RD-COL          PIC S9(4) COMP-5.
               10  SP2-RD-WIDTH        PIC S9(4) COMP-5.
               10  SP2-RD-HEIGHT       PIC S9(4) COMP-5.
               10  SP2-RD-VERT-OCC     PIC S9(4) COMP-5.
               10  SP2-RD-VERT-VIS     PIC S9(4) COMP-5.
               10  SP2-RD-VERT-GAP     PIC S9(4) COMP-5.
               10  SP2-RD-VERT-DISP    PIC S9(4) COMP-5.
               10  SP2-RD-VERT-SHIFT   PIC S9(4) COMP-5.
               10  SP2-RD-VERT-BAR-ID  PIC S9(4) COMP-5.
               10  SP2-RD-HOR-OCC      PIC S9(4) COMP-5.
               10  SP2-RD-HOR-VIS      PIC S9(4) COMP-5.
               10  SP2-RD-HOR-GAP      PIC S9(4) COMP-5.
               10  SP2-RD-HOR-DISP     PIC S9(4) COMP-5.
               10  SP2-RD-HOR-SHIFT    PIC S9(4) COMP-5.
               10  SP2-RD-HOR-BAR-ID   PIC S9(4) COMP-5.
               10  SP2-RD-LAST-OCCURS  PIC S9(4) COMP-5.
               10  SP2-RD-BEG-LINE-NUM PIC S9(4) COMP-5.
               10  SP2-RD-PROG-NUM     PIC S9(4) COMP-5.
               10  SP2-RD-PROG-LEN     PIC S9(4) COMP-5.
               10  SP2-RD-FLD-CNT      PIC S9(4) COMP-5.
      ******** SP2-RD-CHAR-DATA *******
               10  SP2-RD-BOR-TYPE     PIC X.
               10  SP2-RD-TAB-SW       PIC X.
               10  SP2-RD-MISC-OPTIONS PIC X.
               10  SP2-RD-ANCHOR       PIC X.
      ******** SP2-RD-VAR-DATA ********
               10  SP2-RD-BASE-ID OCCURS 10       
                                       PIC S9(4) COMP-5.
               10  SP2-RD-FILE-IND     PIC S9(4) COMP-5.
               10  SP2-RD-FILE-LEN     PIC S9(4) COMP-5.
               10  SP2-RD-FILE-NAME    PIC X(80).

      *******************************
      * font definition             *
      * parameter for GET-FONT-DEF  *
      * also used with SET-FONT-DEF *
      *                QUERY-FONT   *
      *******************************

       01  SP2-FONT-DEF.
           05  SP2-FO-RET-CODE         PIC S9(4) COMP-5.
           05  SP2-FO-LENS.
               10  SP2-FO-LEN-LEN      PIC S9(4) COMP-5 VALUE +8.
               10  SP2-FO-NUM-LEN      PIC S9(4) COMP-5 VALUE +16.
               10  SP2-FO-CHAR-LEN     PIC S9(4) COMP-5 VALUE +6.
               10  SP2-FO-VAR-LEN      PIC S9(4) COMP-5 VALUE +30.
           05  SP2-FO-DATA.
      ******** SP2-FO-NUM-DATA ********
               10  SP2-FO-ID           PIC S9(4) COMP-5.
               10  SP2-FO-WIDTH        PIC S9(4) COMP-5.
               10  SP2-FO-HEIGHT       PIC S9(4) COMP-5.
               10  SP2-FO-GUI-ID       PIC S9(4) COMP-5.
               10  SP2-FO-GUI-ID-2     PIC S9(4) COMP-5.
               10  SP2-FO-DECIPOINTS   PIC S9(4) COMP-5.
               10  FILLER              PIC S9(4) COMP-5.
               10  SP2-FO-ROTATION     PIC S9(4) COMP-5.
      ******** SP2-FO-CHAR-DATA *******
               10  SP2-FO-PITCH        PIC X.
               10  SP2-FO-WEIGHT       PIC X.
               10  SP2-FO-ITALIC       PIC X.
               10  SP2-FO-STRIKE-OUT   PIC X.
               10  SP2-FO-UNDERLINE    PIC X.
               10  SP2-FO-CHAR-SET     PIC X.
      ******** SP2-FO-VAR-DATA ********
               10  SP2-FO-NAME         PIC X(30).

      ********************************
      * color definition             *
      * parameter for GET-COLOR-DEF  *
      * also used with SET-COLOR-DEF *
      *                QUERY-COLOR   *
      ********************************

       01  SP2-COLOR-DEF.
           05  SP2-CO-RET-CODE         PIC S9(4) COMP-5.
           05  SP2-CO-LENS.
               10  SP2-CO-LEN-LEN      PIC S9(4) COMP-5 VALUE +8.
               10  SP2-CO-NUM-LEN      PIC S9(4) COMP-5 VALUE +2.
               10  SP2-CO-CHAR-LEN     PIC S9(4) COMP-5 VALUE +24.
               10  SP2-CO-VAR-LEN      PIC S9(4) COMP-5 VALUE +0.
           05  SP2-CO-DATA.
      ******** SP2-CO-NUM-DATA ********
               10  SP2-CO-ID           PIC S9(4) COMP-5.
      ******** SP2-CO-CHAR-DATA *******
               10  SP2-CO-FG-BG OCCURS 3.
                   15  SP2-CO-NUM      PIC X.
                   15  SP2-CO-TYPE     PIC X.
                   15  SP2-CO-SYSTEM   PIC X.
                   15  SP2-CO-TEXT     PIC X.
                   15  SP2-CO-RED      PIC X.
                   15  SP2-CO-GREEN    PIC X.
                   15  SP2-CO-BLUE     PIC X.
                   15  FILLER          PIC X.
      ******** SP2-CO-VAR-DATA ********
               10  SP2-CO-NAME         PIC X(30).

      *******************************
      * menu definition             *
      * parameter for GET-MENU-DEF  *
      * also used with SET-MENU-DEF *
      *******************************

       01  SP2-MENU-DEF.
           05  SP2-MD-RET-CODE         PIC S9(4) COMP-5.
           05  SP2-MD-LENS.
               10  SP2-MD-LEN-LEN      PIC S9(4) COMP-5 VALUE +18.
               10  SP2-MD-NUM-LEN      PIC S9(4) COMP-5 VALUE +2.
               10  SP2-MD-CHAR-LEN     PIC S9(4) COMP-5 VALUE +18.
               10  SP2-MD-VAR-LEN      PIC S9(4) COMP-5 VALUE +3800.
               10  SP2-MD-OPTN-LEN     PIC S9(4) COMP-5 VALUE +6.
               10  SP2-MD-OPTC-LEN     PIC S9(4) COMP-5 VALUE +2.
               10  SP2-MD-OPTV-LEN     PIC S9(4) COMP-5 VALUE +30.
               10  SP2-MD-NAME-LEN     PIC S9(4) COMP-5 VALUE +0.
               10  SP2-MD-TEXT-LEN     PIC S9(4) COMP-5 VALUE +30.
           05  SP2-MD-DATA.
      ******** SP2-MD-NUM-DATA ********
               10  SP2-MD-OPTION-CNT   PIC S9(4) COMP-5.
      ******** SP2-MD-CHAR-DATA *******
               10  SP2-MD-NAME         PIC X(8).
               10  SP2-MD-DRAW-SW      PIC X.
               10  FILLER              PIC X.
               10  FILLER              PIC X.
               10  FILLER              PIC X.
               10  FILLER              PIC X.
               10  FILLER              PIC X.
               10  FILLER              PIC X.
               10  FILLER              PIC X.
               10  FILLER              PIC X.
               10  FILLER              PIC X. 
      ******** SP2-MD-VAR-DATA ********
               10  SP2-MD-OPTION OCCURS 100.
      ******** SP2-MD-OPTN-DATA *******
                   15  SP2-MDO-ID      PIC S9(4) COMP-5.
                   15  SP2-MDO-OWNR-ID PIC S9(4) COMP-5.
                   15  SP2-MDO-ACC-KEY PIC S9(4) COMP-5.
      ******** SP2-MD-OPTC-DATA *******
                   15  SP2-MDO-TYPE    PIC X.
                   15  SP2-MDO-STATE   PIC X.
      ******** SP2-MD-OPTV-DATA *******
      ******** SP2-MDO-NAME ***********  
                   15  SP2-MDO-TEXT    PIC X(30).

      **********************************
      * common buffer area             *
      * parameter for GET-VERSION      *
      **********************************

       01  SP2-BUFFER.
           05  SP2-BF-RET-CODE         PIC S9(4) COMP-5.
           05  SP2-BF-LEN              PIC S9(4) COMP-5 VALUE +80.
           05  SP2-BF-DATA             PIC X(80).

      ********************
      * property get/set *
      ********************

       01  SP2-PROPERTY.
           05  SP2-PR-RET-CODE         PIC S9(4) COMP-5.
           05  SP2-PR-LENS.
               10  SP2-PR-LEN-LEN      PIC S9(4) COMP-5 VALUE +18.
               10  SP2-PR-NUM-LEN      PIC S9(4) COMP-5 VALUE +6.
               10  SP2-PR-CHAR-LEN     PIC S9(4) COMP-5 VALUE +20.
               10  SP2-PR-VAR-LEN      PIC S9(4) COMP-5 VALUE +2000.
               10  FILLER              PIC S9(4) COMP-5 VALUE +0.
               10  SP2-PR-LENL-LEN     PIC S9(4) COMP-5 VALUE -1.
               10  SP2-PR-NUML-LEN     PIC S9(4) COMP-5 VALUE +0.
               10  SP2-PR-VAR-LEN-L    PIC S9(8) COMP-5 VALUE +0. 
           05  SP2-PR-DATA.
      ******** SP2-PR-NUM-DATA ********
               10  SP2-PR-ID           PIC S9(4) COMP-5.
               10  SP2-PR-ROW          PIC S9(4) COMP-5.
               10  SP2-PR-COL          PIC S9(4) COMP-5.
      ******** SP2-PR-CHAR-DATA *******
               10  SP2-PR-KEY.
                   15  SP2-PR-OBJECT-TYPE
                                       PIC X.
                       88  SP2-PR-WINDOW   VALUE "W".     
                       88  SP2-PR-PANEL    VALUE "P".     
                       88  SP2-PR-STATIC   VALUE "S".     
                       88  SP2-PR-FIELD    VALUE "F".     
                       88  SP2-PR-GROUP    VALUE "G".     
                       88  SP2-PR-REPEAT   VALUE "R".     
                   15  SP2-PR-TYPE     PIC X.
                       88  SP2-PR-LEN-T    VALUE "L".     
                       88  SP2-PR-NUM-T    VALUE "N".     
                       88  SP2-PR-CHAR-T   VALUE "C".     
                       88  SP2-PR-VAR-T    VALUE "V".     
                   15  SP2-PR-VAR-TYPE PIC X.
                       88  SP2-PR-VAR-1    VALUE "A".     
                       88  SP2-PR-VAR-2    VALUE "B".     
                       88  SP2-PR-VAR-3    VALUE "C".     
                       88  SP2-PR-VAR-4    VALUE "D".     
                       88  SP2-PR-VAR-5    VALUE "E".     
                       88  SP2-PR-VAR-6    VALUE "F".     
                       88  SP2-PR-VAR-7    VALUE "G".     
                       88  SP2-PR-VAR-8    VALUE "H".     
                       88  SP2-PR-VAR-9    VALUE "I".     
                       88  SP2-PR-VAR-10   VALUE "J".     
                   15  SP2-PR-OFFSET   PIC 9(5).
                   15  SP2-PR-LEN      PIC 9(5).
                   15  SP2-PR-FORMAT   PIC X.
                       88  SP2-PR-NUMBER   VALUE "N".
                       88  SP2-PR-BINARY   VALUE "B".
                       88  SP2-PR-DECIMAL  VALUE "D".  
                       88  SP2-PR-LONG     VALUE "L".  
                   15  SP2-PR-ACTION   PIC X.
                       88  SP2-PR-REDRAW   VALUE "R".
                       88  SP2-PR-RECREATE VALUE "C".
                   15  SP2-PR-VAR-ACT  PIC X.
                       88  SP2-PR-RESET-LEN  
                                           VALUE "L".
               10  FILLER              PIC X(4).
      ******** SP2-PR-VAR-DATA ********
               10  SP2-PR-VALUE        PIC X(2000).
               10  SP2-PR-NUM-VALUE REDEFINES SP2-PR-VALUE
                                       PIC 9(5).
               10  SP2-PR-BIN-VALUE REDEFINES SP2-PR-VALUE.
                   15  SP2-PR-BIT-VALUE OCCURS 8 
                                       PIC X.
               10  SP2-PR-LONG-VALUE REDEFINES SP2-PR-VALUE
                                       PIC 9(8).
      ******** SP2-PR-VAR-DATA-L ******
      ******** 10  SP2-PR-VALUE-L *****

      *********************
      * END OF PARAMETERS *
      *********************

      *************************
      * SP2 KEY VALUES        *
      * returned in KEY field *
      * in CONVERSE-DATA      *
      *************************

       01  SP2-KEYS.
           05  SP2-KEY-CTRL-A          PIC S9(4) COMP-5 VALUE +1.
           05  SP2-KEY-CTRL-B          PIC S9(4) COMP-5 VALUE +2.
           05  SP2-KEY-CTRL-C          PIC S9(4) COMP-5 VALUE +3.
           05  SP2-KEY-CTRL-D          PIC S9(4) COMP-5 VALUE +4.
           05  SP2-KEY-CTRL-E          PIC S9(4) COMP-5 VALUE +5.
           05  SP2-KEY-CTRL-F          PIC S9(4) COMP-5 VALUE +6.
           05  SP2-KEY-CTRL-G          PIC S9(4) COMP-5 VALUE +7.
           05  SP2-KEY-BACKSPAC        PIC S9(4) COMP-5 VALUE +8.
           05  SP2-KEY-TAB             PIC S9(4) COMP-5 VALUE +9.
           05  SP2-KEY-CTRL-J          PIC S9(4) COMP-5 VALUE +10.
           05  SP2-KEY-CTRL-K          PIC S9(4) COMP-5 VALUE +11.
           05  SP2-KEY-CTRL-L          PIC S9(4) COMP-5 VALUE +12.
           05  SP2-KEY-ENTER           PIC S9(4) COMP-5 VALUE +13.
           05  SP2-KEY-CTRL-N          PIC S9(4) COMP-5 VALUE +14.
           05  SP2-KEY-CTRL-O          PIC S9(4) COMP-5 VALUE +15.
           05  SP2-KEY-CTRL-P          PIC S9(4) COMP-5 VALUE +16.
           05  SP2-KEY-CTRL-Q          PIC S9(4) COMP-5 VALUE +17.
           05  SP2-KEY-CTRL-R          PIC S9(4) COMP-5 VALUE +18.
           05  SP2-KEY-CTRL-S          PIC S9(4) COMP-5 VALUE +19.
           05  SP2-KEY-CTRL-T          PIC S9(4) COMP-5 VALUE +20.
           05  SP2-KEY-CTRL-U          PIC S9(4) COMP-5 VALUE +21.
           05  SP2-KEY-CTRL-V          PIC S9(4) COMP-5 VALUE +22.
           05  SP2-KEY-CTRL-W          PIC S9(4) COMP-5 VALUE +23.
           05  SP2-KEY-CTRL-X          PIC S9(4) COMP-5 VALUE +24.
           05  SP2-KEY-CTRL-Y          PIC S9(4) COMP-5 VALUE +25.
           05  SP2-KEY-CTRL-Z          PIC S9(4) COMP-5 VALUE +26.
           05  SP2-KEY-ESC             PIC S9(4) COMP-5 VALUE +27.
           05  SP2-KEY-ESCAPE REDEFINES SP2-KEY-ESC
                                       PIC S9(4) COMP-5.
           05  SP2-KEY-CTRL-BACKSLASH  PIC S9(4) COMP-5 VALUE +28.
           05  SP2-KEY-CTRL-BRACKET    PIC S9(4) COMP-5 VALUE +29.
           05  SP2-KEY-CTRL-MINUS      PIC S9(4) COMP-5 VALUE +31.
           05  SP2-KEY-SPACEBAR        PIC S9(4) COMP-5 VALUE +32.
           05  SP2-KEY-BACKTAB         PIC S9(4) COMP-5 VALUE +271.
           05  SP2-KEY-ALT-Q           PIC S9(4) COMP-5 VALUE +272.
           05  SP2-KEY-ALT-W           PIC S9(4) COMP-5 VALUE +273.
           05  SP2-KEY-ALT-E           PIC S9(4) COMP-5 VALUE +274.
           05  SP2-KEY-ALT-R           PIC S9(4) COMP-5 VALUE +275.
           05  SP2-KEY-ALT-T           PIC S9(4) COMP-5 VALUE +276.
           05  SP2-KEY-ALT-Y           PIC S9(4) COMP-5 VALUE +277.
           05  SP2-KEY-ALT-U           PIC S9(4) COMP-5 VALUE +278.
           05  SP2-KEY-ALT-I           PIC S9(4) COMP-5 VALUE +279.
           05  SP2-KEY-ALT-O           PIC S9(4) COMP-5 VALUE +280.
           05  SP2-KEY-ALT-P           PIC S9(4) COMP-5 VALUE +281.
           05  SP2-KEY-ALT-A           PIC S9(4) COMP-5 VALUE +286.
           05  SP2-KEY-ALT-S           PIC S9(4) COMP-5 VALUE +287.
           05  SP2-KEY-ALT-D           PIC S9(4) COMP-5 VALUE +288.
           05  SP2-KEY-ALT-F           PIC S9(4) COMP-5 VALUE +289.
           05  SP2-KEY-ALT-G           PIC S9(4) COMP-5 VALUE +290.
           05  SP2-KEY-ALT-H           PIC S9(4) COMP-5 VALUE +291.
           05  SP2-KEY-ALT-J           PIC S9(4) COMP-5 VALUE +292.
           05  SP2-KEY-ALT-K           PIC S9(4) COMP-5 VALUE +293.
           05  SP2-KEY-ALT-L           PIC S9(4) COMP-5 VALUE +294.
           05  SP2-KEY-ALT-Z           PIC S9(4) COMP-5 VALUE +300.
           05  SP2-KEY-ALT-X           PIC S9(4) COMP-5 VALUE +301.
           05  SP2-KEY-ALT-C           PIC S9(4) COMP-5 VALUE +302.
           05  SP2-KEY-ALT-V           PIC S9(4) COMP-5 VALUE +303.
           05  SP2-KEY-ALT-B           PIC S9(4) COMP-5 VALUE +304.
           05  SP2-KEY-ALT-N           PIC S9(4) COMP-5 VALUE +305.
           05  SP2-KEY-ALT-M           PIC S9(4) COMP-5 VALUE +306.
           05  SP2-KEY-F1              PIC S9(4) COMP-5 VALUE +315.
           05  SP2-KEY-F2              PIC S9(4) COMP-5 VALUE +316.
           05  SP2-KEY-F3              PIC S9(4) COMP-5 VALUE +317.
           05  SP2-KEY-F4              PIC S9(4) COMP-5 VALUE +318.
           05  SP2-KEY-F5              PIC S9(4) COMP-5 VALUE +319.
           05  SP2-KEY-F6              PIC S9(4) COMP-5 VALUE +320.
           05  SP2-KEY-F7              PIC S9(4) COMP-5 VALUE +321.
           05  SP2-KEY-F8              PIC S9(4) COMP-5 VALUE +322.
           05  SP2-KEY-F9              PIC S9(4) COMP-5 VALUE +323.
           05  SP2-KEY-F10             PIC S9(4) COMP-5 VALUE +324.
           05  SP2-KEY-HOME            PIC S9(4) COMP-5 VALUE +327.
           05  SP2-KEY-UP              PIC S9(4) COMP-5 VALUE +328.
           05  SP2-KEY-PGUP            PIC S9(4) COMP-5 VALUE +329.
           05  SP2-KEY-LEFT            PIC S9(4) COMP-5 VALUE +331.
           05  SP2-KEY-RIGHT           PIC S9(4) COMP-5 VALUE +333.
           05  SP2-KEY-END             PIC S9(4) COMP-5 VALUE +335.
           05  SP2-KEY-DOWN            PIC S9(4) COMP-5 VALUE +336.
           05  SP2-KEY-PGDN            PIC S9(4) COMP-5 VALUE +337.
           05  SP2-KEY-INSERT          PIC S9(4) COMP-5 VALUE +338.
           05  SP2-KEY-DELETE          PIC S9(4) COMP-5 VALUE +339.
           05  SP2-KEY-SHIFT-F1        PIC S9(4) COMP-5 VALUE +340.
           05  SP2-KEY-SHIFT-F2        PIC S9(4) COMP-5 VALUE +341.
           05  SP2-KEY-SHIFT-F3        PIC S9(4) COMP-5 VALUE +342.
           05  SP2-KEY-SHIFT-F4        PIC S9(4) COMP-5 VALUE +343.
           05  SP2-KEY-SHIFT-F5        PIC S9(4) COMP-5 VALUE +344.
           05  SP2-KEY-SHIFT-F6        PIC S9(4) COMP-5 VALUE +345.
           05  SP2-KEY-SHIFT-F7        PIC S9(4) COMP-5 VALUE +346.
           05  SP2-KEY-SHIFT-F8        PIC S9(4) COMP-5 VALUE +347.
           05  SP2-KEY-SHIFT-F9        PIC S9(4) COMP-5 VALUE +348.
           05  SP2-KEY-SHIFT-F10       PIC S9(4) COMP-5 VALUE +349.
           05  SP2-KEY-CTRL-F1         PIC S9(4) COMP-5 VALUE +350.
           05  SP2-KEY-CTRL-F2         PIC S9(4) COMP-5 VALUE +351.
           05  SP2-KEY-CTRL-F3         PIC S9(4) COMP-5 VALUE +352.
           05  SP2-KEY-CTRL-F4         PIC S9(4) COMP-5 VALUE +353.
           05  SP2-KEY-CTRL-F5         PIC S9(4) COMP-5 VALUE +354.
           05  SP2-KEY-CTRL-F6         PIC S9(4) COMP-5 VALUE +355.
           05  SP2-KEY-CTRL-F7         PIC S9(4) COMP-5 VALUE +356.
           05  SP2-KEY-CTRL-F8         PIC S9(4) COMP-5 VALUE +357.
           05  SP2-KEY-CTRL-F9         PIC S9(4) COMP-5 VALUE +358.
           05  SP2-KEY-CTRL-F10        PIC S9(4) COMP-5 VALUE +359.
           05  SP2-KEY-ALT-F1          PIC S9(4) COMP-5 VALUE +360.
           05  SP2-KEY-ALT-F2          PIC S9(4) COMP-5 VALUE +361.
           05  SP2-KEY-ALT-F3          PIC S9(4) COMP-5 VALUE +362.
           05  SP2-KEY-ALT-F4          PIC S9(4) COMP-5 VALUE +363.
           05  SP2-KEY-ALT-F5          PIC S9(4) COMP-5 VALUE +364.
           05  SP2-KEY-ALT-F6          PIC S9(4) COMP-5 VALUE +365.
           05  SP2-KEY-ALT-F7          PIC S9(4) COMP-5 VALUE +366.
           05  SP2-KEY-ALT-F8          PIC S9(4) COMP-5 VALUE +367.
           05  SP2-KEY-ALT-F9          PIC S9(4) COMP-5 VALUE +368.
           05  SP2-KEY-ALT-F10         PIC S9(4) COMP-5 VALUE +369.
           05  SP2-KEY-CTRL-PRTSC      PIC S9(4) COMP-5 VALUE +370.
           05  SP2-KEY-CTRL-LEFT       PIC S9(4) COMP-5 VALUE +371.
           05  SP2-KEY-CTRL-RIGHT      PIC S9(4) COMP-5 VALUE +372.
           05  SP2-KEY-CTRL-END        PIC S9(4) COMP-5 VALUE +373.
           05  SP2-KEY-CTRL-PGDN       PIC S9(4) COMP-5 VALUE +374.
           05  SP2-KEY-CTRL-HOME       PIC S9(4) COMP-5 VALUE +375.
           05  SP2-KEY-ALT-1           PIC S9(4) COMP-5 VALUE +376.
           05  SP2-KEY-ALT-2           PIC S9(4) COMP-5 VALUE +377.
           05  SP2-KEY-ALT-3           PIC S9(4) COMP-5 VALUE +378.
           05  SP2-KEY-ALT-4           PIC S9(4) COMP-5 VALUE +379.
           05  SP2-KEY-ALT-5           PIC S9(4) COMP-5 VALUE +380.
           05  SP2-KEY-ALT-6           PIC S9(4) COMP-5 VALUE +381.
           05  SP2-KEY-ALT-7           PIC S9(4) COMP-5 VALUE +382.
           05  SP2-KEY-ALT-8           PIC S9(4) COMP-5 VALUE +383.
           05  SP2-KEY-ALT-9           PIC S9(4) COMP-5 VALUE +384.
           05  SP2-KEY-ALT-0           PIC S9(4) COMP-5 VALUE +385.
           05  SP2-KEY-ALT-MINUS       PIC S9(4) COMP-5 VALUE +386.
           05  SP2-KEY-ALT-EQUAL       PIC S9(4) COMP-5 VALUE +387.
           05  SP2-KEY-CTRL-PGUP       PIC S9(4) COMP-5 VALUE +388.
           05  SP2-KEY-F11             PIC S9(4) COMP-5 VALUE +389.
           05  SP2-KEY-F12             PIC S9(4) COMP-5 VALUE +390.
           05  SP2-KEY-SHIFT-F11       PIC S9(4) COMP-5 VALUE +391.
           05  SP2-KEY-SHIFT-F12       PIC S9(4) COMP-5 VALUE +392.
           05  SP2-KEY-CTRL-F11        PIC S9(4) COMP-5 VALUE +393.
           05  SP2-KEY-CTRL-F12        PIC S9(4) COMP-5 VALUE +394.
           05  SP2-KEY-ALT-F11         PIC S9(4) COMP-5 VALUE +395.
           05  SP2-KEY-ALT-F12         PIC S9(4) COMP-5 VALUE +396.
           05  SP2-KEY-CTRL-UP         PIC S9(4) COMP-5 VALUE +397.
           05  SP2-KEY-CTRL-DOWN       PIC S9(4) COMP-5 VALUE +401.
           05  SP2-KEY-CTRL-INSERT     PIC S9(4) COMP-5 VALUE +402.
           05  SP2-KEY-CTRL-DELETE     PIC S9(4) COMP-5 VALUE +403.
           05  SP2-KEY-CTRL-TAB        PIC S9(4) COMP-5 VALUE +404.
           05  SP2-KEY-TIMEOUT         PIC S9(4) COMP-5 VALUE -1.
           05  SP2-KEY-SELECT          PIC S9(4) COMP-5 VALUE -2.
           05  SP2-KEY-SWITCH          PIC S9(4) COMP-5 VALUE -3.
           05  SP2-KEY-CTRL-FIELD      PIC S9(4) COMP-5 VALUE -4.
           05  SP2-KEY-CLOSE           PIC S9(4) COMP-5 VALUE -5.
           05  SP2-KEY-MENU            PIC S9(4) COMP-5 VALUE -6.
           05  SP2-KEY-MORE            PIC S9(4) COMP-5 VALUE -7.
           05  SP2-KEY-SIZE            PIC S9(4) COMP-5 VALUE -9.
           05  SP2-KEY-MOUSE           PIC S9(4) COMP-5 VALUE -10.
           05  SP2-KEY-CLICK-RIGHT     PIC S9(4) COMP-5 VALUE -11.
           05  SP2-KEY-DOUBLE-CLICK    PIC S9(4) COMP-5 VALUE -12.
           05  SP2-KEY-DOUBLE-RIGHT    PIC S9(4) COMP-5 VALUE -13.
           05  SP2-KEY-CLICK-OUTSIDE   PIC S9(4) COMP-5 VALUE -14.
           05  SP2-KEY-TOOLBAR         PIC S9(4) COMP-5 VALUE -15.
           05  SP2-KEY-VBX             PIC S9(4) COMP-5 VALUE -16.
           05  SP2-KEY-SCROLL-CLICK    PIC S9(4) COMP-5 VALUE -18.
           05  SP2-KEY-SWITCH-DENIED   PIC S9(4) COMP-5 VALUE -19.
           05  SP2-KEY-SYS-SHUTDOWN    PIC S9(4) COMP-5 VALUE -22.
           05  SP2-KEY-APP-CLOSE       PIC S9(4) COMP-5 VALUE -23.
           05  SP2-KEY-WINDOW-OPENED   PIC S9(4) COMP-5 VALUE -26.
           05  SP2-KEY-WINDOW-CLOSED   PIC S9(4) COMP-5 VALUE -27.
           05  SP2-KEY-WINDOW-ENTERED  PIC S9(4) COMP-5 VALUE -28.
           05  SP2-KEY-WINDOW-EXITED   PIC S9(4) COMP-5 VALUE -29.
           05  SP2-KEY-NOTIFY-ICON     PIC S9(4) COMP-5 VALUE -30.

      *********************
      * END OF KEY VALUES *
      *********************

      ******************************
      * SP2 FIELD TYPE CODES       *
      * used with SP2-CD-TYPE-DATA *
      ******************************

       01  SP2-FIELD-TYPES.
           05  SP2-DISPLAY-ONLY        PIC X VALUE "o".
           05  SP2-GREYED-OUT          PIC X VALUE "g".

      **********************
      * END OF FIELD TYPES *
      **********************

      **************************
      * SP2 LINE DRAWING CODES *
      * used with static data  *
      **************************

       01  SP2-LINES.
           05  SP2-LINE-TOP-LEFT       PIC X VALUE X"DA".
           05  SP2-LINE-HORIZONTAL     PIC X VALUE X"C4".
           05  SP2-LINE-TOP-RIGHT      PIC X VALUE X"BF".
           05  SP2-LINE-VERTICAL       PIC X VALUE X"B3".
           05  SP2-LINE-BOTTOM-LEFT    PIC X VALUE X"C0".
           05  SP2-LINE-BOTTOM-RIGHT   PIC X VALUE X"D9".

      ***********************
      * END OF LINE DRAWING *
      ***********************

      *************************
      * SP2 MOUSE SHAPE CODES *
      *************************

       01  SP2-MOUSE-SHAPES.
           05  SP2-MOUSE-ARROW         PIC S9(4) COMP-5 VALUE +0.
           05  SP2-MOUSE-LOCATE        PIC S9(4) COMP-5 VALUE +1.
           05  SP2-MOUSE-TOP-LEFT      PIC S9(4) COMP-5 VALUE +2.
           05  SP2-MOUSE-TOP           PIC S9(4) COMP-5 VALUE +3.
           05  SP2-MOUSE-TOP-RIGHT     PIC S9(4) COMP-5 VALUE +4.
           05  SP2-MOUSE-RIGHT         PIC S9(4) COMP-5 VALUE +5.
           05  SP2-MOUSE-WAIT          PIC S9(4) COMP-5 VALUE +6.
           05  SP2-MOUSE-BOTTOM-RIGHT  PIC S9(4) COMP-5 VALUE +7.
           05  SP2-MOUSE-BOTTOM        PIC S9(4) COMP-5 VALUE +8.
           05  SP2-MOUSE-BOTTOM-LEFT   PIC S9(4) COMP-5 VALUE +9.
           05  SP2-MOUSE-LEFT          PIC S9(4) COMP-5 VALUE +10.

      ***********************
      * END OF MOUSE SHAPES *
      ***********************

      **************************
      * SP2 SYSTEM COLOR CODES *
      **************************

       01  SP2-SYSTEM-COLRS.
           05  SP2-COLR-DEFAULT        PIC X VALUE X"00".
           05  SP2-COLR-WINDOW         PIC X VALUE X"01".
           05  SP2-COLR-HIGHLIGHT      PIC X VALUE X"02".
           05  SP2-COLR-MENU           PIC X VALUE X"03".

      *****************************
      * END OF SYSTEM COLOR CODES *
      *****************************
