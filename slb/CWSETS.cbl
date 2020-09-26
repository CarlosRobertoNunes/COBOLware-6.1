       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWSETS.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  01/11/2002.
       SECURITY.      *************************************************
                      *                                               *
                      *  Armazena elementos do ambiente               *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 DIR                 PIC X(050) VALUE SPACES.
           05 CWMENU.
              10 CWMENU-END            PIC  X(008) VALUE SPACES.
              10 CWMENU-AREA01 POINTER.
              10 CWMENU-AREA02 POINTER.
              10 CWMENU-AREA03 POINTER.
              10 CWMENU-AREA04 POINTER.
              10 CWMENU-AREA05 POINTER.
              10 CWMENU-AREA06 POINTER.
              10 CWMENU-AREA07 POINTER.
              10 CWMENU-AREA08 POINTER.
              10 CWMENU-AREA09 POINTER.
              10 CWMENU-AREA10 POINTER.

       LINKAGE SECTION.

       01   OPCAO    PIC X.
       01   ELEMENTO PIC X(3).
       01   VALOR    PIC X(50).

       PROCEDURE DIVISION USING OPCAO ELEMENTO VALOR.

       000-INICIO.

            IF OPCAO = X'01'
               MOVE ELEMENTO(1: LENGTH CWMENU) TO CWMENU
               GOBACK
            ELSE
               IF OPCAO = X'02'
                  MOVE CWMENU TO ELEMENTO(1: LENGTH CWMENU)
                  GOBACK
               END-IF
           END-IF
            EVALUATE OPCAO ALSO TRUE
                     WHEN "S" ALSO ELEMENTO = "DIR"
                              MOVE VALOR TO DIR
                     WHEN "G" ALSO ELEMENTO = "DIR"
                              MOVE DIR TO VALOR
            END-EVALUATE.

       000-99-FIM. GOBACK.

       END PROGRAM CWSETS.
