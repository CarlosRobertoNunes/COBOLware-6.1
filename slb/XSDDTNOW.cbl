       IDENTIFICATION DIVISION.
       PROGRAM-ID.    XSDDTNOW.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  09/09/2017.
       SECURITY.      *************************************************
                      *                                               *
                      * Suporte a programas gerados pelo XSEED        *
                      * Obter data e hora corrente em varios formatos *
                      *                                               *
                      *************************************************
       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 TABELA-DE-MESES.
              10 PIC X(36) VALUE "JANFEVMARABRMAIJUNJULAGOSETOUTNOVDEZ".
           05 REDEFINES TABELA-DE-MESES.
              10 NOME-MES PIC X(3) OCCURS 12.

       LINKAGE SECTION.

       01  GLB-DCTYPE                 PIC  X(002).

       01  GLB-DTITENS.
           05 ACTMTH-A.
              10 ACTMTH               PIC  9(004).
           05 GLB-DATE                PIC  X(007).
           05 GLB-TIME                PIC  9(008).
           05 GLB-UNIQUE              PIC  9(012).
           05 INPUT-DATE              PIC  X(007).
           05 TODAY                   PIC  X(007).
           05 TODAYS-DAY              PIC  9(002).
           05 TODAYS-MONTH            PIC  X(003).
           05 TODAYS-YEAR             PIC  9(002).
           05 TODAYS-DATE-NUM         PIC  X(006).
           05 TODAYS-MONTH-NUM        PIC  9(002).
           05 TRANNO                  PIC  9(006).
           05 GLB-YYMMDD.
              10 GLB-YY               PIC  9(002).
              10 GLB-MM               PIC  9(002).
              10 GLB-DD               PIC  9(002).
          05 glb-dc-yy-mm-dd.
             10 glb-dc-yy-mm-dd-yy    pic  9(002).
             10 glb-dc-yy-mm-dd-b1    pic  x(001).
             10 glb-dc-yy-mm-dd-mm    pic  9(002).
             10 glb-dc-yy-mm-dd-b2    pic  x(001).
             10 glb-dc-yy-mm-dd-dd    pic  9(002).
          05 glb-dc-yy-mmm-dd.
             10 glb-dc-yy-mmm-dd-yy   pic  9(002).
             10 glb-dc-yy-mmm-dd-b1   pic  x(001).
             10 glb-dc-yy-mmm-dd-mm   pic  x(003).
             10 glb-dc-yy-mmm-dd-b2   pic  x(001).
             10 glb-dc-yy-mmm-dd-dd   pic  9(002).
          05 glb-wc-yy-mm-dd.
             10 glb-wc-yy-mm-dd-yy    pic  9(002).
             10 glb-wc-yy-mm-dd-mm    pic  9(002).
             10 glb-wc-yy-mm-dd-dd    pic  9(002).
          05 glb-wc-yy-mmm-dd.
             10 glb-wc-yy-mmm-dd-yy   pic  9(002).
             10 glb-wc-yy-mmm-dd-mm   pic  x(003).
             10 glb-wc-yy-mmm-dd-dd   pic  9(002).

       PROCEDURE DIVISION USING GLB-DCTYPE GLB-DTITENS.

       000-INICIO.

           ACCEPT  GLB-YYMMDD        FROM DATE
           ACCEPT  GLB-TIME          FROM TIME
           COMPUTE GLB-UNIQUE           = GLB-TIME * 10000
           MOVE    GLB-YYMMDD          TO ACTMTH-A
           MOVE    GLB-DD              TO TODAYS-DAY
                                          glb-dc-yy-mm-dd-dd
                                          glb-dc-yy-mmm-dd-dd
                                          glb-wc-yy-mm-dd-dd
                                          glb-wc-yy-mmm-dd-dd
           MOVE    NOME-MES (GLB-MM)   TO TODAYS-MONTH
           MOVE    GLB-YY              TO TODAYS-YEAR
                                          glb-dc-yy-mm-dd-yy
                                          glb-dc-yy-mmm-dd-yy
                                          glb-wc-yy-mm-dd-yy
                                          glb-wc-yy-mmm-dd-yy
           MOVE    GLB-MM              TO TODAYS-MONTH-NUM
                                          glb-dc-yy-mm-dd-mm
                                          glb-wc-yy-mm-dd-mm
           MOVE    NOME-MES (GLB-MM)   TO glb-dc-yy-mmm-dd-mm
                                          glb-wc-yy-mmm-dd-mm
           INSPECT GLB-DCTYPE CONVERTING MINUSCULAS TO MAIUSCULAS

           EVALUATE GLB-DCTYPE
              WHEN "UK"
                   MOVE GLB-DD            TO GLB-DATE (1: 2)
                   MOVE NOME-MES (GLB-MM) TO GLB-DATE (3: 3)
                   MOVE GLB-YY            TO GLB-DATE (6: 2)
                   MOVE GLB-DATE          TO INPUT-DATE
                                             TODAY
                   MOVE GLB-DD            TO TODAYS-DATE-NUM (1: 2)
                   MOVE GLB-MM            TO TODAYS-DATE-NUM (3: 2)
                   MOVE GLB-YY            TO TODAYS-DATE-NUM (5: 2)
              WHEN "IN"
                   MOVE GLB-YY            TO GLB-DATE (1: 2)
                   MOVE NOME-MES (GLB-MM) TO GLB-DATE (3: 3)
                   MOVE GLB-DD            TO GLB-DATE (6: 2)
                   MOVE GLB-DATE          TO INPUT-DATE
                                             TODAY
                   MOVE GLB-YY            TO TODAYS-DATE-NUM (1: 2)
                   MOVE GLB-MM            TO TODAYS-DATE-NUM (3: 2)
                   MOVE GLB-DD            TO TODAYS-DATE-NUM (5: 2)
              WHEN "US"
                   MOVE NOME-MES (GLB-MM) TO GLB-DATE (1: 3)
                   MOVE GLB-DD            TO GLB-DATE (4: 2)
                   MOVE GLB-YY            TO GLB-DATE (6: 2)
                   MOVE GLB-DATE          TO INPUT-DATE
                                             TODAY
                   MOVE GLB-MM            TO TODAYS-DATE-NUM (1: 2)
                   MOVE GLB-DD            TO TODAYS-DATE-NUM (3: 2)
                   MOVE GLB-YY            TO TODAYS-DATE-NUM (5: 2)
           END-EVALUATE

           MOVE '/' TO glb-dc-yy-mm-dd-b1
                       glb-dc-yy-mm-dd-b2
                       glb-dc-yy-mmm-dd-b1
                       glb-dc-yy-mmm-dd-b2.

       000-99-FIM. GOBACK.

       END PROGRAM XSDDTNOW.
