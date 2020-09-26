      $SET CALLFH"EXTFH"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWWBMP.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  01/05/2009.
       SECURITY.      *************************************************
                      *                                               *
                      *  Converte arquivos de imagem para BMP         *
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.                  DECIMAL-POINT IS COMMA
                                  CALL-CONVENTION 74 IS WIN32.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT BINARIO ASSIGN TO DISK
                  ORGANIZATION  IS BINARY SEQUENTIAL
                  LOCK MODE IS EXCLUSIVE
                  FILE STATUS   IS FS-BINARIO.

       DATA DIVISION.
       FILE SECTION.

       FD  BINARIO
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-BINARIO.

       01  BINARIO-REG PIC X(1).

       WORKING-STORAGE SECTION.

       78  SP2 value "SP2".
       01  AREAS-DE-TRABALHO. COPY CWCASE.
           05 WORK COMP-5 PIC S9(8) value 0.
           05 RET  COMP-5 PIC S9(8) value 0.
           05 IMAGE-TYPE COMP-5 PIC S9(8) value 0.
           05 COBWARE            PIC  X(255) VALUE SPACES.
           05 COBDIR             PIC  X(255) VALUE SPACES.
           05 FS-BINARIO         PIC  X(002) VALUE "00".
           05 LB-BINARIO         PIC  X(2048) VALUE SPACES.
           05 INFILE-E           PIC  X(050) VALUE SPACES.
           05 MSG                PIC  X(074) VALUE SPACES.
           05 vez                PIC  9(007) value 0.
           05 I                  PIC  9(005) COMP-5 value 0.
           05 Y                  PIC  9(005) COMP-5 value 0.
           05 TIPO               PIC  X(005) VALUE SPACES.
              88 BMP        value "bmp" "dib".
              88 ICO        value "ico".
              88 JPEG       value "jpg" "jif" "jpe" "jpeg" "jfif".
              88 JNG        value "jng".
              88 KOALA      value "koa".
              88 LBM        value "lbm".
              88 IFF        value "iff".
              88 MNG        value "mng".
              88 PBM        value "pbm".
              88 PBMRAW     value "pbm".
              88 PCD        value "pcd".
              88 PCX        value "pcx".
              88 PGM        value "pgm".
              88 PGMRAW     value "pgm".
              88 PNG        value "png".
              88 PPM        value "ppm".
              88 PPMRAW     value "ppm".
              88 RAS        value "ras".
              88 TARGA      value "tga" "targa".
              88 TIFF       value "tif" "tiff".
              88 WBMP       value "wbmp" "wbm".
              88 PSD        value "psd".
              88 CUT        value "cut".
              88 XBM        value "xbm".
              88 XPM        value "xmp".
              88 DDS        value "dds".
              88 GIF        value "gif".
              88 HDR        value "xmp".
              88 FAXG3      value "g3".
              88 SGI        value "sgi".
              88 EXR        value "exr".
              88 J2K        value "j2k" "j2c".
              88 JP2        value "jp2".
              88 PFM        value "pfm".
              88 PICT       value "pct" "pict" "pic".
              88 RAW        value "raw".
           05 BMPWORK-LABEL                  VALUE "CW000000.BMP".
              15                 PIC  X(002).
              15 TASK            PIC  9(006).
              15                 PIC  X(001).
              15 EXTENSAO        PIC  X(003).
           05 OUTFILE2 PIC X(13) VALUE SPACES.

       COPY CWEXEC.
       COPY CWSPWS.

       78  FIF-BMP         VALUE 0.
       78  FIF-ICO         VALUE 1.
       78  FIF-JPEG        VALUE 2.
       78  FIF-JNG         VALUE 3.
       78  FIF-KOALA       VALUE 4.
       78  FIF-LBM         VALUE 5.
       78  FIF-IFF         VALUE 5.
       78  FIF-MNG         VALUE 6.
       78  FIF-PBM         VALUE 7.
       78  FIF-PBMRAW      VALUE 8.
       78  FIF-PCD         VALUE 9.
       78  FIF-PCX         VALUE 10.
       78  FIF-PGM         VALUE 11.
       78  FIF-PGMRAW      VALUE 12.
       78  FIF-PNG         VALUE 13.
       78  FIF-PPM         VALUE 14.
       78  FIF-PPMRAW      VALUE 15.
       78  FIF-RAS         VALUE 16.
       78  FIF-TARGA       VALUE 17.
       78  FIF-TIFF        VALUE 18.
       78  FIF-WBMP        VALUE 19.
       78  FIF-PSD         VALUE 20.
       78  FIF-CUT         VALUE 21.
       78  FIF-XBM         VALUE 22.
       78  FIF-XPM         VALUE 23.
       78  FIF-DDS         VALUE 24.
       78  FIF-GIF         VALUE 25.
       78  FIF-HDR         VALUE 26.
       78  FIF-FAXG3       VALUE 27.
       78  FIF-SGI         VALUE 28.
       78  FIF-EXR         VALUE 29.
       78  FIF-J2K         VALUE 30.
       78  FIF-JP2         VALUE 31.
       78  FIF-PFM         VALUE 32.
       78  FIF-PICT        VALUE 33.
       78  FIF-RAW         VALUE 34.

       LINKAGE SECTION.

       01  INFILE PIC X(255).
       01 OUTFILE PIC X(12).

       PROCEDURE DIVISION USING INFILE OUTFILE.

       000-INICIO.

           IF INFILE = SPACES
              GOBACK
           END-IF

           ON 1
              COPY CWSPPD.
              DISPLAY "COBOLWARE" UPON ENVIRONMENT-NAME
              ACCEPT   COBWARE    FROM ENVIRONMENT-VALUE
              DISPLAY "COBDIR" UPON ENVIRONMENT-NAME
              ACCEPT   COBDIR  FROM ENVIRONMENT-VALUE.

           MOVE INFILE TO LB-BINARIO
           PERFORM TEST AFTER UNTIL FS-BINARIO NOT = '9A'
                   OPEN INPUT BINARIO
           END-PERFORM
           IF FS-BINARIO NOT = '00'
              MOVE SPACES TO LB-BINARIO
              MOVE 0      TO Y
              PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH OF COBDIR
                      ADD 1 TO Y
                      IF COBDIR(I:1) = ';' OR I = LENGTH OF COBDIR
VC    *               OR SPACE
                         MOVE "\" TO LB-BINARIO (Y:1)
                         ADD 1 TO Y
                         MOVE INFILE TO LB-BINARIO (Y:I)
                         OPEN INPUT BINARIO
                         IF FS-BINARIO = '00'
                         OR COBDIR(I:1) = SPACE
                            EXIT PERFORM
                         ELSE
                            MOVE SPACES TO LB-BINARIO
                            MOVE 0      TO Y
                         END-IF
                      ELSE
                         MOVE COBDIR (I: 1) TO LB-BINARIO (Y: 1)
                      END-IF
              END-PERFORM
           END-IF
           IF FS-BINARIO NOT = '00'
              MOVE SPACES TO OUTFILE
           END-IF
           CLOSE BINARIO
           PERFORM VARYING I FROM LENGTH OF INFILE
                      BY -1 UNTIL I = 1
                               OR INFILE (I: 1) = '.'
                   CONTINUE
           END-PERFORM
           ADD 1 TO I
           MOVE INFILE(I: ) TO TIPO
           MOVE SPACES TO CWEXEC-COMANDO
           CALL "CWTASK" USING "4" TASK
           MOVE BMPWORK-LABEL TO OUTFILE
           inspect tipo
                   converting maiusculas
                           to minusculas

           EVALUATE TRUE
               WHEN BMP       MOVE FIF-BMP        TO IMAGE-TYPE
               WHEN ICO       MOVE FIF-ICO        TO IMAGE-TYPE
               WHEN JPEG      MOVE FIF-JPEG       TO IMAGE-TYPE
               WHEN JNG       MOVE FIF-JNG        TO IMAGE-TYPE
               WHEN KOALA     MOVE FIF-KOALA      TO IMAGE-TYPE
               WHEN LBM       MOVE FIF-LBM        TO IMAGE-TYPE
               WHEN IFF       MOVE FIF-IFF        TO IMAGE-TYPE
               WHEN MNG       MOVE FIF-MNG        TO IMAGE-TYPE
               WHEN PBM       MOVE FIF-PBM        TO IMAGE-TYPE
               WHEN PBMRAW    MOVE FIF-PBMRAW     TO IMAGE-TYPE
               WHEN PCD       MOVE FIF-PCD        TO IMAGE-TYPE
               WHEN PCX       MOVE FIF-PCX        TO IMAGE-TYPE
               WHEN PGM       MOVE FIF-PGM        TO IMAGE-TYPE
               WHEN PGMRAW    MOVE FIF-PGMRAW     TO IMAGE-TYPE
               WHEN PNG       MOVE FIF-PNG        TO IMAGE-TYPE
               WHEN PPM       MOVE FIF-PPM        TO IMAGE-TYPE
               WHEN PPMRAW    MOVE FIF-PPMRAW     TO IMAGE-TYPE
               WHEN RAS       MOVE FIF-RAS        TO IMAGE-TYPE
               WHEN TARGA     MOVE FIF-TARGA      TO IMAGE-TYPE
               WHEN TIFF      MOVE FIF-TIFF       TO IMAGE-TYPE
               WHEN WBMP      MOVE FIF-WBMP       TO IMAGE-TYPE
               WHEN PSD       MOVE FIF-PSD        TO IMAGE-TYPE
               WHEN CUT       MOVE FIF-CUT        TO IMAGE-TYPE
               WHEN XBM       MOVE FIF-XBM        TO IMAGE-TYPE
               WHEN XPM       MOVE FIF-XPM        TO IMAGE-TYPE
               WHEN DDS       MOVE FIF-DDS        TO IMAGE-TYPE
               WHEN GIF       MOVE FIF-GIF        TO IMAGE-TYPE
               WHEN HDR       MOVE FIF-HDR        TO IMAGE-TYPE
               WHEN FAXG3     MOVE FIF-FAXG3      TO IMAGE-TYPE
               WHEN SGI       MOVE FIF-SGI        TO IMAGE-TYPE
               WHEN EXR       MOVE FIF-EXR        TO IMAGE-TYPE
               WHEN J2K       MOVE FIF-J2K        TO IMAGE-TYPE
               WHEN JP2       MOVE FIF-JP2        TO IMAGE-TYPE
               WHEN PFM       MOVE FIF-PFM        TO IMAGE-TYPE
               WHEN PICT      MOVE FIF-PICT       TO IMAGE-TYPE
               WHEN RAW       MOVE FIF-RAW        TO IMAGE-TYPE
               WHEN OTHER
                    IF INFILE NOT = INFILE-E
                       MOVE INFILE TO INFILE-E
                       MOVE SPACES TO MSG
                       STRING 'Formato de imagem de "' DELIMITED BY SIZE
                              INFILE  DELIMITED BY SPACE
                              '" não suportado' DELIMITED BY SIZE
                              INTO MSG
                       MOVE "b"      TO SP2-MS-ICON
                       MOVE "o"      TO SP2-MS-BUTTON
                       MOVE MSG      TO SP2-MS-TEXT
                       MOVE "Aviso:" TO SP2-MS-TITLE
                       MOVE 1        TO SP2-MS-LINE-CNT
                       MOVE SPACES TO OUTFILE
                       CALL SP2   USING SP2-DISPLAY-MESSAGE
                                        SP2-MESSAGE-DATA
                    END-IF
                    goback
           END-EVALUATE

           INSPECT LB-BINARIO CONVERTING SPACE TO LOW-VALUES
           CALL WIN32 "_FreeImage_Load@12" USING
                                            BY VALUE     IMAGE-TYPE
                                            BY REFERENCE LB-BINARIO
                                            BY VALUE     0
                                   RETURNING WORK
           MOVE FIF-BMP TO IMAGE-TYPE
           INSPECT LB-BINARIO CONVERTING LOW-VALUES TO SPACE
           STRING OUTFILE X'00' DELIMITED BY SIZE INTO OUTFILE2
           CALL WIN32 "_FreeImage_Save@16" USING
                                            BY VALUE     IMAGE-TYPE
                                            BY VALUE     WORK
                                            BY REFERENCE OUTFILE2
                                            BY VALUE     0
                                   RETURNING RET.
           CALL WIN32 "_FreeImage_Unload@4" USING
                                            BY VALUE     WORK
                                   RETURNING RET.

       000-99-FIM. GOBACK.
       END PROGRAM CWWBMP.
