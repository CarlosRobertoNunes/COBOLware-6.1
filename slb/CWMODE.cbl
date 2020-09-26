       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWMODE.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  05/05/2005.
       SECURITY.      *************************************************
                      *                                               *
                      *  Consulta e mantem modulo                     *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO.
           05 CUR-TIPO                 PIC  X(002) VALUE SPACES.
           05 CUR-PAGINA               PIC  9(004) VALUE 0.
           05 CUR-MODULO               PIC  X(025) VALUE SPACES.

       COPY CWCONF.

       LINKAGE SECTION.

       01  FUNCAO     PIC  X(001).
       01  PAGINA     PIC  9(004).
       01  MODULO     PIC  X(015).
       01  TIPO-REG   PIC  X(002).

       PROCEDURE DIVISION USING FUNCAO PAGINA MODULO TIPO-REG.

       000-INICIO.

           EVALUATE FUNCAO
               WHEN "C"
                    MOVE CUR-PAGINA TO PAGINA
                    MOVE CUR-MODULO TO MODULO
                    MOVE CUR-TIPO   TO TIPO-REG
               WHEN "W"
                    MOVE PAGINA   TO CUR-PAGINA
                    MOVE MODULO   TO CUR-MODULO
                    MOVE TIPO-REG TO CUR-TIPO
               WHEN "G"
                    SET CWSQLC-OPEN TO TRUE
                    CALL "CWCONF" USING CWSQLC
                                        CWCONF-REG
                                        FS-CWCONF
                                        KCO PCO
                    MOVE "MD"     TO CWCONF-REGMD
                    MOVE TIPO-REG TO CWCONF-TIPO-MODULO
                    MOVE PAGINA TO CWCONF-PAGINA
                    SET CWSQLC-READ TO TRUE
                    SET CWSQLC-EQUAL TO TRUE
                    SET CWSQLC-IGNORE-LOCK TO TRUE
                    CALL "CWCONF" USING CWSQLC
                                        CWCONF-REG
                                        FS-CWCONF
                                        KCO PCO
                    MOVE CWCONF-MODULO TO MODULO
                    SET CWSQLC-CLOSE TO TRUE
                    CALL "CWCONF" USING CWSQLC
                                        CWCONF-REG
                                        FS-CWCONF
                                        KCO PCO
               WHEN "S"
                    SET CWSQLC-UPDATE TO TRUE
                    CALL "CWCONF" USING CWSQLC
                                        CWCONF-REG
                                        FS-CWCONF
                                        KCO PCO
                    MOVE "MD"     TO CWCONF-REGMD
                    MOVE TIPO-REG TO CWCONF-TIPO-MODULO
                    MOVE PAGINA   TO CWCONF-PAGINA
                    SET CWSQLC-READ TO TRUE
                    SET CWSQLC-EQUAL TO TRUE
                    SET CWSQLC-IGNORE-LOCK TO TRUE
                    CALL "CWCONF" USING CWSQLC
                                        CWCONF-REG
                                        FS-CWCONF
                                        KCO PCO
                    MOVE MODULO TO CWCONF-MODULO
                    IF   FS-CWCONF < "10"
                         SET CWSQLC-REWRITE TO TRUE
                         CALL "CWCONF" USING CWSQLC
                                             CWCONF-REG
                                             FS-CWCONF
                                             KCO PCO
                    ELSE
                         SET CWSQLC-WRITE TO TRUE
                         CALL "CWCONF" USING CWSQLC
                                             CWCONF-REG
                                             FS-CWCONF
                                             KCO PCO
                    END-IF
                    SET CWSQLC-CLOSE TO TRUE
                    CALL "CWCONF" USING CWSQLC
                                        CWCONF-REG
                                        FS-CWCONF
                                        KCO PCO
           END-EVALUATE.

       000-99-FIM. GOBACK.

       END PROGRAM CWMODE.
