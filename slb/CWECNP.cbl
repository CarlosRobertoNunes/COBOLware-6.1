       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWECNP.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  25/06/1989.
       SECURITY.      *************************************************
                      *                                               *
                      *   Edita CNPJ,CEI ou CIC conforme validade     *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO-1. COPY CWCASE.
           05 MODO                   PIC  X(003) VALUE SPACES.

       COPY CWDCNP.
       COPY CWDCIC.
       COPY CWDCEI.

       LINKAGE SECTION.

       01  CWECNP-CNPJ-CEI-CIC   COMP-3 PIC  9(014).

       01  CWECNP-CNPJ-CEI-CIC-ED       PIC  X(018).
       01  REDEFINES CWECNP-CNPJ-CEI-CIC-ED.
           05 CNPJ                      PIC  99.999.999/9999.99.
           05 CIC REDEFINES CNPJ        PIC  BBBB999.999.999.99.
           05 CEI REDEFINES CNPJ        PIC  B99.999.99.999.9/9.

       PROCEDURE DIVISION USING CWECNP-CNPJ-CEI-CIC
                                CWECNP-CNPJ-CEI-CIC-ED.

       010-PROCESSAMENTO.

           MOVE CWECNP-CNPJ-CEI-CIC  TO CWDCNP-CNPJ
                                        CWDCIC-CIC
                                        CWDCEI-CEI
           CALL "CWDCNP"     USING PARAMETROS-CWDCNP
           CALL "CWDCEI"     USING PARAMETROS-CWDCEI
           CALL "CWDCIC"     USING PARAMETROS-CWDCIC

           EVALUATE TRUE
               WHEN CWECNP-CNPJ-CEI-CIC EQUAL ZERO
                    MOVE SPACES TO CWECNP-CNPJ-CEI-CIC-ED
               WHEN CWDCIC-RETORNO EQUAL "00"
                AND CWDCNP-RETORNO EQUAL "00"
                AND CWECNP-CNPJ-CEI-CIC LESS 99999999999
                    DISPLAY "CWECNP" UPON ENVIRONMENT-NAME
                    ACCEPT  MODO FROM ENVIRONMENT-VALUE
                    INSPECT MODO CONVERTING MINUSCULAS TO MAIUSCULAS
                    EVALUATE TRUE
                       WHEN MODO = 'CPF' OR 'CIC'
                            MOVE CWECNP-CNPJ-CEI-CIC TO CIC
                       WHEN MODO = 'CNPJ' OR 'CGC'
                            MOVE CWECNP-CNPJ-CEI-CIC TO CIC
                       WHEN OTHER
                            IF   CWDCNP-CNPJ (9: 2) = "00"
                            AND (CWECNP-CNPJ-CEI-CIC NOT = 00292009380)
                            AND (CWECNP-CNPJ-CEI-CIC NOT = 00241006767)
                                 MOVE CWECNP-CNPJ-CEI-CIC TO CNPJ
                            ELSE
                                 MOVE CWECNP-CNPJ-CEI-CIC TO CIC
                            END-IF
                    END-EVALUATE
                    MOVE "-" TO CWECNP-CNPJ-CEI-CIC-ED (16: 01)
               WHEN CWDCIC-RETORNO EQUAL "00"
                AND CWECNP-CNPJ-CEI-CIC LESS 99999999999
                    MOVE CWECNP-CNPJ-CEI-CIC TO CIC
                    MOVE "-" TO CWECNP-CNPJ-CEI-CIC-ED (16: 01)
               WHEN CWDCNP-RETORNO EQUAL "00"
                    MOVE CWECNP-CNPJ-CEI-CIC TO CNPJ
                    MOVE "-" TO CWECNP-CNPJ-CEI-CIC-ED (16: 01)
               WHEN CWDCEI-RETORNO EQUAL "0"
                    MOVE CWECNP-CNPJ-CEI-CIC TO CEI
                    MOVE "-"                 TO CEI  (4: 1)
                                                CEI  (8: 1)
                                                CEI (15: 1)
               WHEN OTHER
                    MOVE ALL X"B0" TO CWECNP-CNPJ-CEI-CIC-ED
          END-EVALUATE.

       010-99-FIM. EXIT PROGRAM.

       END PROGRAM CWECNP.
