       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWGETU.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  23/04/1989.
       SECURITY.      *************************************************
                      *                                               *
                      *   Retorna Usuario, task e programa            *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1.
           05 PROV                     PIC  X(001) VALUE "0".
           05 MSG                      PIC  X(080) VALUE SPACES.
           05 MOLDURA                  PIC  9(001) VALUE 0.
           05 USUARIO                  PIC  X(030) VALUE SPACES.
           05 TASK                     PIC  9(006) VALUE 0.
           05 PROGRAMA                 PIC  X(008) VALUE SPACES.
           05 LOG                      PIC  X(001) VALUE SPACE.
              88 LOG-ON                            VALUE "1".
           05 X91-RESULT        COMP-X PIC  9(002) VALUE 0.
           05 X91-FUNCTION      COMP-X PIC  9(002) VALUE 16.
           05 X91-PARAMETER     COMP-X PIC  9(002) VALUE 0.

       COPY CWTIME.
       COPY CWCONF.

       LINKAGE SECTION.

       01  USUARIO-L            PIC   X(030).
       01  TASK-L               PIC   9(006).
       01  PROGRAMA-L           PIC   X(008).
       01  CWMENU               PIC   X(001).

       PROCEDURE DIVISION USING USUARIO-L TASK-L PROGRAMA-L CWMENU.

       000-INICIO.

           CALL X"91" USING X91-RESULT X91-FUNCTION X91-PARAMETER

           IF   X91-PARAMETER = 0
                STOP RUN
           END-IF

           IF   CWMENU = "#"
                MOVE PROGRAMA-L TO PROGRAMA
                GOBACK
           END-IF
           IF   CWMENU = "$"
                MOVE "1" TO PROV
                GOBACK
           END-IF

           IF   CWMENU = "%"
                MOVE "0" TO PROV
                GOBACK
           END-IF

           IF   CWMENU = "&"
                MOVE PROV TO CWMENU
                GOBACK
           END-IF

           IF   CWMENU = "9"
                MOVE TASK-L TO TASK
                MOVE SPACE  TO CWMENU
                EXIT PROGRAM
           END-IF

           IF   CWMENU = "M"
                MOVE TASK-L TO MOLDURA
                MOVE SPACE  TO CWMENU
                EXIT PROGRAM
           END-IF

           IF   CWMENU = "m"
                MOVE MOLDURA TO TASK-L
                MOVE SPACE   TO CWMENU
                EXIT PROGRAM
           END-IF

           IF   CWMENU = "3"
                MOVE USUARIO-L TO USUARIO
rofer           DISPLAY 'CWLOGN' UPON ENVIRONMENT-NAME
rofer           DISPLAY USUARIO  UPON ENVIRONMENT-VALUE
                SET CWSQLC-OPEN TO TRUE
                CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO PCO
                IF FS-CWCONF < '10'
                   MOVE "PS"   TO CWCONF-TIPO
                   MOVE SPACES TO CWCONF-ELEMENTO
                   SET CWSQLC-READ TO TRUE
                   SET CWSQLC-IGNORE-LOCK TO TRUE
                   CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO
                                                         PCO
                   IF FS-CWCONF < '10'
                      DISPLAY 'CWLOGG'      UPON ENVIRONMENT-NAME
                      DISPLAY  CWCONF-GRUPO UPON ENVIRONMENT-VALUE
                   END-IF
                   SET CWSQLC-CLOSE TO TRUE
                   CALL "CWCONF" USING CWSQLC CWCONF-REG FS-CWCONF KCO
                                                                   PCO
                END-IF
                GOBACK
           END-IF
           IF   CWMENU EQUAL "1" OR "0" OR "2"
                IF   CWMENU NOT = "2"
                     MOVE CWMENU    TO LOG
                     MOVE USUARIO-L TO USUARIO
                ELSE
                     MOVE USUARIO-L TO MSG
                END-IF
                MOVE "N"        TO CWMENU
                MOVE TASK-L     TO TASK
                MOVE PROGRAMA-L TO PROGRAMA
                EXIT PROGRAM
           ELSE
                IF   CWMENU EQUAL "?"
                     MOVE "N"        TO CWMENU
                     MOVE USUARIO    TO USUARIO-L
                     MOVE TASK       TO TASK-L
                     MOVE PROGRAMA   TO PROGRAMA-L
rofer                DISPLAY 'CWLOGN' UPON ENVIRONMENT-NAME
rofer                DISPLAY USUARIO  UPON ENVIRONMENT-VALUE
                     EXIT PROGRAM
                END-IF
           END-IF.

       000-99-FIM. STOP RUN.

       END PROGRAM CWGETU.
