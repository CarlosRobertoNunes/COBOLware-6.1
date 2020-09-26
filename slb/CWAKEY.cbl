       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWAKEY.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  21/04/2005.
       SECURITY.      *************************************************
                      *                                               *
                      *  Simula ACCEPT FROM ESCAPE KEY                *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 HOTKEY.
              06                  PIC X(007) VALUE "HOTKEY-".
              06 TECLA            PIC 9(003) VALUE 0.
           05 I                   PIC 9(001) VALUE 0.
           05 Y                   PIC 9(001) VALUE 0.
           05 programa            PIC X(255) VALUE SPACES.
           05 COMANDO             PIC X(255) VALUE SPACES.
           05 cobolware           PIC X(255) VALUE SPACES.

       COPY CWUNIX.
       COPY CWGETS.

       LINKAGE SECTION.

       01  LK-TECLA      PIC X(018).
       01  TAMANHO-TECLA PIC 9(008) COMP-X.

       PROCEDURE DIVISION USING LK-TECLA
                                TAMANHO-TECLA.
       000-INICIO.
           ON 1
              DISPLAY "COBOLWARE" UPON ENVIRONMENT-NAME
              ACCEPT  COBOLware   from ENVIRONMENT-value.

           IF   TAMANHO-TECLA = 1000
                MOVE LK-TECLA (1: 3) TO TECLA (1: 3)
                MOVE SPACES          TO programa
                DISPLAY HOTKEY UPON ENVIRONMENT-NAME
                ACCEPT programa FROM ENVIRONMENT-VALUE
                IF programa Not = SPACES
                   CALL "CWUNIX" USING PARAMETROS-CWUNIX
                   CALL "CWGETS" USING PARAMETROS-CWGETS
                   MOVE SPACES      TO comando
                   evaluate true
                       when CWUNIX-ON
                            STRING COBOLware   delimited by space
                                '/cobware.lbr/cwmenu /u:'
                                               delimited by size
                                CWGETS-USUARIO delimited by space
                                ' /s:'         delimited by size
                                CWGETS-SENHA   delimited by space
                                ' /r:'         delimited by size
                                programa       delimited by size
                                into comando
                       when CWUNIX-GUI
                            STRING COBOLware   delimited by space
                                '\cwmenuG /u:'
                                               delimited by size
                                CWGETS-USUARIO delimited by space
                                ' /s:'         delimited by size
                                CWGETS-SENHA   delimited by space
                                ' /r:'         delimited by size
                                programa       delimited by size
                                into comando
                       when CWUNIX-DOS
                            STRING COBOLware   delimited by space
                                '\cwmenuF /u:'
                                               delimited by size
                                CWGETS-USUARIO delimited by space
                                ' /s:'         delimited by size
                                CWGETS-SENHA   delimited by space
                                ' /r:'         delimited by size
                                programa       delimited by size
                                into comando
                       when Other
                            STRING COBOLware   delimited by space
                                '\cwmenuT /u:'
                                               delimited by size
                                CWGETS-USUARIO delimited by space
                                ' /s:'         delimited by size
                                CWGETS-SENHA   delimited by space
                                ' /r:'         delimited by size
                                programa       delimited by size
                                into comando
                   end-evaluate
                   EXEC COBOLware EXECsystem ERASE-OFF
                        COMMAND comando
                   END-EXEC
                END-IF
           ELSE
                MOVE TAMANHO-TECLA TO Y
                MOVE ALL "0"       TO LK-TECLA (1: Y)
                MOVE 3             TO I
                PERFORM UNTIL Y = 0
                           OR I = 0
                        MOVE TECLA (I: 1) TO LK-TECLA (Y: 1)
                        SUBTRACT 1 FROM I Y
                END-PERFORM
           END-IF.

       000-99-FIM. GOBACK.

       END PROGRAM CWAKEY.
