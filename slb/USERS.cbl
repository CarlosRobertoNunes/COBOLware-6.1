       IDENTIFICATION DIVISION.
       PROGRAM-ID.    USERS.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  29/07/2000.
       SECURITY.      *************************************************
                      *                                               *
                      *  Exemplo de programa provedor de informacoes  *
                      *  para o Relator 1.0                           *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 i pic 9(3) value 0.

       COPY CWCONF.
       LINKAGE SECTION.

       COPY RELATOR.
           05 NOME                           PIC  X(030).
           05 NIVEL-PS                       PIC  9(002).
           05 SIZE-PS                        PIC  9(002).
           05 FATOR-PS                       PIC  9(002).
           05 SENHA                          PIC  X(030).
           05 GRUPO                          PIC  X(022).
           05 MODO-MENU                      PIC  9(001).
           05 PATH-SPOOL                     PIC  X(030).
           05 PRINTER-DEFAULT                PIC  X(008).
           05 QUADRO-PS                      PIC  9(001).
           05 SPOOL-OPTIONS.
              10 ZEBRA                       PIC  9(001).
              10 FULLSCREEN                  PIC  9(001).
              10 COLUNA-SORT                 PIC  9(001).
              10 OPCAO-SORT                  PIC  9(001).
           05 RELATOR-IN                     PIC  9(009).
           05 RELATOR-OUT                    PIC  9(009).

       PROCEDURE DIVISION USING PARAMETROS-RELATOR.

       000-INICIO.

           ON   1
                INITIALIZE RELATOR-CAMPOS
                SET CWSQLC-OPEN TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                IF   FS-CWCONF > "09"
                     CALL "CWCONF" USING "ISAM"
                END-IF
                IF   FS-CWCONF > "09"
                     SET RELATOR-FIM TO TRUE
                END-IF
                MOVE HIGH-VALUES TO CWCONF-REG.

           SET CWSQLC-READ TO TRUE
           SET CWSQLC-EQUAL TO TRUE
           SET CWSQLC-IGNORE-LOCK TO TRUE
           CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                if  fs-cwconf not = "00"
                    add 1 to i
                       SET RELATOR-FIM TO TRUE
                    if i = 100
                       SET CWSQLC-CLOSE TO TRUE
                       CALL "CWCONF" USING CWSQLC
                                           CWCONF-REG
                                           FS-CWCONF
                                           KCO PCO
                    end-if
                else
                    IF   CWCONF-TIPO NOT = "PS"
                         GO TO 000-INICIO
                    END-IF
                    PERFORM 110-MOVER-CWCONF THRU 110-99-FIM
                END-if.

       000-99-FIM. GOBACK.

       110-MOVER-CWCONF.

           MOVE CWCONF-NOME               TO NOME
           MOVE CWCONF-NIVEL-PS           TO NIVEL-PS
           MOVE CWCONF-SIZE-PS            TO SIZE-PS
           MOVE CWCONF-FATOR-PS           TO FATOR-PS
           MOVE CWCONF-SENHA              TO SENHA
           MOVE CWCONF-GRUPO              TO GRUPO
           MOVE CWCONF-MODO-MENU          TO MODO-MENU
           MOVE CWCONF-PATH-SPOOL         TO PATH-SPOOL
           MOVE CWCONF-PRINTER-DEFAULT    TO PRINTER-DEFAULT
           MOVE CWCONF-QUADRO-PS          TO QUADRO-PS
           MOVE CWCONF-SPOOL-OPTIONS      TO SPOOL-OPTIONS
           MOVE CWCONF-RELATOR-IN         TO RELATOR-IN
           MOVE CWCONF-RELATOR-OUT        TO RELATOR-OUT.

       110-99-FIM. EXIT.

       END PROGRAM USERS.
