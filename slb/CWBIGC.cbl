       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWBIGC.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  05/04/1994.
       SECURITY.      *************************************************
                      *                                               *
                      * Caractere em letras grandes PIC X(7) x 8      *
                      * Antiga XSDBIG                                 *
                      *                                               *
                      *************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 I         PIC  9(002) VALUE 0.
           05 NORMAL.
              10        PIC  X(027) VALUE "!2#$%&'()*+,-./0123456789:;".
              10        PIC  X(027) VALUE "<=>?@[\]^_`ABCDEFGHIJKLMNOP".
              10        PIC  X(014) VALUE "QRSTUVWXYZ{|}~".
           05 REDEFINES NORMAL.
              10 LETRA  PIC  X(001) OCCURS 68.
           05 BIG-ECHO.
              10        PIC  X(007) value "  ###  ".
              10        PIC  X(007) value "  ###  ".
              10        PIC  X(007) value "  ###  ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "  ###  ".
              10        PIC  X(007) value "  ###  ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "### ###".
              10        PIC  X(007) value "### ###".
              10        PIC  X(007) value " #   # ".
              10        PIC  X(007) value "#   #  ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "  # #  ".
              10        PIC  X(007) value "  # #  ".
              10        PIC  X(007) value "#######".
              10        PIC  X(007) value "  # #  ".
              10        PIC  X(007) value "#######".
              10        PIC  X(007) value "  # #  ".
              10        PIC  X(007) value "  # #  ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "#  #  #".
              10        PIC  X(007) value "#  #   ".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "   #  #".
              10        PIC  X(007) value "#  #  #".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "###   #".
              10        PIC  X(007) value "# #  # ".
              10        PIC  X(007) value "### #  ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "  # ###".
              10        PIC  X(007) value " #  # #".
              10        PIC  X(007) value "#   ###".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "  ##   ".
              10        PIC  X(007) value " #  #  ".
              10        PIC  X(007) value "  ##   ".
              10        PIC  X(007) value " ###   ".
              10        PIC  X(007) value "#   # #".
              10        PIC  X(007) value "#    # ".
              10        PIC  X(007) value " ###  #".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "  ###  ".
              10        PIC  X(007) value "  ###  ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "  #    ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "   ##  ".
              10        PIC  X(007) value "  #    ".
              10        PIC  X(007) value " #     ".
              10        PIC  X(007) value " #     ".
              10        PIC  X(007) value " #     ".
              10        PIC  X(007) value "  #    ".
              10        PIC  X(007) value "   ##  ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "  ##   ".
              10        PIC  X(007) value "    #  ".
              10        PIC  X(007) value "     # ".
              10        PIC  X(007) value "     # ".
              10        PIC  X(007) value "     # ".
              10        PIC  X(007) value "    #  ".
              10        PIC  X(007) value "  ##   ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value " #   # ".
              10        PIC  X(007) value "  # #  ".
              10        PIC  X(007) value "#######".
              10        PIC  X(007) value "  # #  ".
              10        PIC  X(007) value " #   # ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "  ###  ".
              10        PIC  X(007) value "  ###  ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "  #    ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "  ###  ".
              10        PIC  X(007) value "  ###  ".
              10        PIC  X(007) value "  ###  ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "      #".
              10        PIC  X(007) value "     # ".
              10        PIC  X(007) value "    #  ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "  #    ".
              10        PIC  X(007) value " #     ".
              10        PIC  X(007) value "#      ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "  ###  ".
              10        PIC  X(007) value " #   # ".
              10        PIC  X(007) value "#   # #".
              10        PIC  X(007) value "#  #  #".
              10        PIC  X(007) value "# #   #".
              10        PIC  X(007) value " #   # ".
              10        PIC  X(007) value "  ###  ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "  ##   ".
              10        PIC  X(007) value " # #   ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "      #".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "#      ".
              10        PIC  X(007) value "#      ".
              10        PIC  X(007) value "#######".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "      #".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "      #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "#      ".
              10        PIC  X(007) value "#    # ".
              10        PIC  X(007) value "#    # ".
              10        PIC  X(007) value "#    # ".
              10        PIC  X(007) value "#######".
              10        PIC  X(007) value "     # ".
              10        PIC  X(007) value "     # ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "#######".
              10        PIC  X(007) value "#      ".
              10        PIC  X(007) value "#      ".
              10        PIC  X(007) value "###### ".
              10        PIC  X(007) value "      #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#      ".
              10        PIC  X(007) value "###### ".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "#######".
              10        PIC  X(007) value "#    # ".
              10        PIC  X(007) value "    #  ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "  #    ".
              10        PIC  X(007) value "  #    ".
              10        PIC  X(007) value "  #    ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value " ######".
              10        PIC  X(007) value "      #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "  ###  ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "  ###  ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "  ###  ".
              10        PIC  X(007) value "  ###  ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "  ###  ".
              10        PIC  X(007) value "  ###  ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "  #    ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "    #  ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "  #    ".
              10        PIC  X(007) value " #     ".
              10        PIC  X(007) value "  #    ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "    #  ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "  #    ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "    #  ".
              10        PIC  X(007) value "     # ".
              10        PIC  X(007) value "    #  ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "  #    ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "      #".
              10        PIC  X(007) value "   ### ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "# ### #".
              10        PIC  X(007) value "# ### #".
              10        PIC  X(007) value "# #### ".
              10        PIC  X(007) value "#      ".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value " #     ".
              10        PIC  X(007) value " #     ".
              10        PIC  X(007) value " #     ".
              10        PIC  X(007) value " #     ".
              10        PIC  X(007) value " #     ".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "#      ".
              10        PIC  X(007) value " #     ".
              10        PIC  X(007) value "  #    ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "    #  ".
              10        PIC  X(007) value "     # ".
              10        PIC  X(007) value "      #".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "     # ".
              10        PIC  X(007) value "     # ".
              10        PIC  X(007) value "     # ".
              10        PIC  X(007) value "     # ".
              10        PIC  X(007) value "     # ".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "  # #  ".
              10        PIC  X(007) value " #   # ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "#######".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "  ###  ".
              10        PIC  X(007) value "  ###  ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "    #  ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "  # #  ".
              10        PIC  X(007) value " #   # ".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#######".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "###### ".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "###### ".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "###### ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#      ".
              10        PIC  X(007) value "#      ".
              10        PIC  X(007) value "#      ".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "###### ".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "###### ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "#######".
              10        PIC  X(007) value "#      ".
              10        PIC  X(007) value "#      ".
              10        PIC  X(007) value "#####  ".
              10        PIC  X(007) value "#      ".
              10        PIC  X(007) value "#      ".
              10        PIC  X(007) value "#######".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "#######".
              10        PIC  X(007) value "#      ".
              10        PIC  X(007) value "#      ".
              10        PIC  X(007) value "#####  ".
              10        PIC  X(007) value "#      ".
              10        PIC  X(007) value "#      ".
              10        PIC  X(007) value "#      ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#      ".
              10        PIC  X(007) value "#  ####".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#######".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "  ###  ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "  ###  ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "      #".
              10        PIC  X(007) value "      #".
              10        PIC  X(007) value "      #".
              10        PIC  X(007) value "      #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "#    # ".
              10        PIC  X(007) value "#   #  ".
              10        PIC  X(007) value "#  #   ".
              10        PIC  X(007) value "###    ".
              10        PIC  X(007) value "#  #   ".
              10        PIC  X(007) value "#   #  ".
              10        PIC  X(007) value "#    # ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "#      ".
              10        PIC  X(007) value "#      ".
              10        PIC  X(007) value "#      ".
              10        PIC  X(007) value "#      ".
              10        PIC  X(007) value "#      ".
              10        PIC  X(007) value "#      ".
              10        PIC  X(007) value "#######".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "##   ##".
              10        PIC  X(007) value "# # # #".
              10        PIC  X(007) value "#  #  #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "##    #".
              10        PIC  X(007) value "# #   #".
              10        PIC  X(007) value "#  #  #".
              10        PIC  X(007) value "#   # #".
              10        PIC  X(007) value "#    ##".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "#######".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#######".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "###### ".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "###### ".
              10        PIC  X(007) value "#      ".
              10        PIC  X(007) value "#      ".
              10        PIC  X(007) value "#      ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#   # #".
              10        PIC  X(007) value "#    # ".
              10        PIC  X(007) value " #### #".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "###### ".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "###### ".
              10        PIC  X(007) value "#   #  ".
              10        PIC  X(007) value "#    # ".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#      ".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "      #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "#######".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value " ##### ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value " #   # ".
              10        PIC  X(007) value "  # #  ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "#  #  #".
              10        PIC  X(007) value "#  #  #".
              10        PIC  X(007) value "#  #  #".
              10        PIC  X(007) value "#  #  #".
              10        PIC  X(007) value "#  #  #".
              10        PIC  X(007) value " ## ## ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value " #   # ".
              10        PIC  X(007) value "  # #  ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "  # #  ".
              10        PIC  X(007) value " #   # ".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "#     #".
              10        PIC  X(007) value " #   # ".
              10        PIC  X(007) value "  # #  ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "#######".
              10        PIC  X(007) value "     # ".
              10        PIC  X(007) value "    #  ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "  #    ".
              10        PIC  X(007) value " #     ".
              10        PIC  X(007) value "#######".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "  ###  ".
              10        PIC  X(007) value " #     ".
              10        PIC  X(007) value " #     ".
              10        PIC  X(007) value "##     ".
              10        PIC  X(007) value " #     ".
              10        PIC  X(007) value " #     ".
              10        PIC  X(007) value "  ###  ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "   #   ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "  ###  ".
              10        PIC  X(007) value "     # ".
              10        PIC  X(007) value "     # ".
              10        PIC  X(007) value "     ##".
              10        PIC  X(007) value "     # ".
              10        PIC  X(007) value "     # ".
              10        PIC  X(007) value "  ###  ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value " ##    ".
              10        PIC  X(007) value "#  #  #".
              10        PIC  X(007) value "    ## ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
              10        PIC  X(007) value "       ".
           05 REDEFINES BIG-ECHO.
              10 LETRAO PIC  X(056) OCCURS 68.

       LINKAGE SECTION.

       COPY CWBIGC.

       PROCEDURE DIVISION USING PARAMETROS-CWBIGC.

       000-INICIO.

            INSPECT CWBIGC-CHAR CONVERTING MINUSCULAS TO MAIUSCULAS
            MOVE '"'    TO LETRA (2)
            MOVE SPACES TO CWBIGC-BIG

            PERFORM VARYING I FROM 1 BY 1 UNTIL I > 68
                    IF   LETRA (I) = CWBIGC-CHAR
                         MOVE LETRAO (I) TO CWBIGC-BIG
                         EXIT PERFORM
                    END-IF
            END-PERFORM

            IF   CWBIGC-FILL NOT = SPACES
            AND  CWBIGC-FILL NOT = "#"
                 INSPECT CWBIGC-BIG CONVERTING "#"
                                            TO CWBIGC-FILL
            END-IF

            MOVE "#" TO CWBIGC-FILL.

       000-99-FIM. GOBACK.
       END PROGRAM CWBIGC.

