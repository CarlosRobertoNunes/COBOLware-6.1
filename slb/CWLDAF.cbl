       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWLDAF.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  28/08/2008.
       SECURITY.      *************************************************
                      *                                               *
                      *  Intercomunicaá∆o entre linguagens via LDA    *
                      *  Local Data Area                              *
                      *                                               *
                      *************************************************

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  Byte-stream-File-Routines.
      *    05 LDAMSG        pic x(050)      value spaces.
           05 LDANAME       pic x(255)      value 'lda.txt'.
           05 access-mode   pic x comp-x    value 3.
           05 deny-mode     pic x comp-x    value 3.
           05 device        pic x comp-x.
           05 file-handle   pic x(4).
           05 file-offset   pic x(8) comp-x value 0.
           05 byte-count    pic x(4) comp-x value 0.
           05 flags         pic x comp-x    value 0.
           05 reserved      pic x comp-x.

       LINKAGE SECTION.

       01 OPTION    PIC X.
       01 LDABUFFER PIC X.
       01 LDALENGTH PIC 9(008) COMP-X.

       PROCEDURE DIVISION USING OPTION LDABUFFER LDALENGTH.

       000-INICIO.

           DISPLAY 'LDA'  UPON ENVIRONMENT-NAME
           ACCEPT LDANAME FROM ENVIRONMENT-VALUE
           MOVE   LDALENGTH TO byte-count

           IF   OPTION = 'A' OR 'a'
                call "CBL_OPEN_FILE" using LDANAME
                                           access-mode
                                           deny-mode
                                           device
                                           file-handle
                if return-code not = 0
                   MOVE LOW-VALUES TO LDABUFFER(1:LDALENGTH)
      *            String 'Arquivo LDA(Local Data Area) "'
      *                   DELIMITED BY SIZE
      *                   LDANAME DELIMITED BY SPACE
      *                   '") n∆o encontrado' DELIMITED BY SIZE
      *                   INTO LDAMSG
      *            EXEC COBOLware Send
      *                 Message LDAMSG
      *            END-EXEC
                ELSE
                    call "CBL_READ_FILE" using file-handle
                                               file-offset
                                               byte-count
                                               flags
                                               LDABUFFER
                end-if
           ELSE
                call "CBL_CREATE_FILE" using LDANAME
                                             access-mode
                                             deny-mode
                                             device
                                             file-handle
                call "CBL_WRITE_FILE" using file-handle
                                            file-offset
                                            byte-count
                                            flags
                                            LDABUFFER
           END-IF

           call "CBL_CLOSE_FILE" using file-handle.

       000-99-FIM. GOBACK.

       END PROGRAM CWLDAF.
