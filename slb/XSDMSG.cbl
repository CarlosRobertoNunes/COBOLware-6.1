       IDENTIFICATION DIVISION.
       PROGRAM-ID.    XSDMSG.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  20/09/2017.
       SECURITY.      *************************************************
                      *                                               *
                      *  Suporte a programas gerados pelo XSEED       *
                      *  Tratamento de mensagens                      *
                      *                                               *
                      *************************************************
       DATA DIVISION.
       LINKAGE SECTION.

       01  GLB-MSGINDEX            PIC  9(002).
       01  GLB-INDEX               PIC  9(002).
       01  GLB-MESSAGE.
           05 GLB-MSGCOPY          PIC  9(002) OCCURS 21 TIMES.
           05 GLB-MSGHEADER        PIC  X(016) OCCURS 21 TIMES.
           05 GLB-MSGTRAILER       PIC  X(050) OCCURS 21 TIMES.
       01  GLB-SENDMSG.
           05 GLB-SMSGHEADER       PIC  X(016).
           05 GLB-SMSGTRAILER      PIC  X(064).
       01  GLB-LINE                PIC  9(002).
       01  GLB-RECALL              PIC  X(008).
       01  GLB-TARGET              PIC  X(003).
       01     ISPEC                PIC  X(005).
       01  P2-ISPEC                PIC  X(005).
       01  P2-FLAG                 PIC  X(001).
       01  GLB-LSN                 PIC  X(005).
       01  GLB-REQUEST             PIC  X(001).
       01  GLB-CALL                PIC  X(005).
       01  GLB-TEACH               PIC  X(005).
       01  P2-TEACH                PIC  X(005).
       01  GLB-CURSOR              PIC  9(004).
       01  CURSOR-LC               PIC  9(004).
       01  GLB-ERROR               PIC  X(005).

       SCREEN SECTION.

       01 ERRMSG BACKGROUND-COLOR IS 01.
          10   LINE GLB-LINE COL 01 PIC X(16) FROM
             GLB-MSGHEADER(GLB-INDEX) REVERSE-VIDEO BEEP.
          10   LINE GLB-LINE COL 17 PIC X(50) FROM
             GLB-MSGTRAILER(GLB-INDEX) HIGHLIGHT.
          10   LINE GLB-LINE COL 67 VALUE "              ".
       01 ATTMSG BACKGROUND-COLOR IS 01.
          10   LINE GLB-LINE COL 01 PIC X(16) FROM
             GLB-MSGHEADER(GLB-INDEX) REVERSE-VIDEO.
          10   LINE GLB-LINE COL 17 PIC X(50) FROM
             GLB-MSGTRAILER(GLB-INDEX) HIGHLIGHT.
          10   LINE GLB-LINE COL 67 VALUE "              ".
       01 SENDMSG BACKGROUND-COLOR IS 01.
          10   LINE 25 COL 01 PIC X(16) FROM
             GLB-SMSGHEADER REVERSE-VIDEO.
          10   LINE 25 COL 17 PIC X(64) FROM
             GLB-SMSGTRAILER HIGHLIGHT.
       01 CLEMSG BACKGROUND-COLOR IS 01.
             10   LINE 25 COL 01 VALUE "
      -    "                                                ".

       PROCEDURE DIVISION USING GLB-MSGINDEX
                                GLB-INDEX
                                GLB-MESSAGE
                                GLB-SENDMSG
                                GLB-LINE
                                GLB-RECALL
                                GLB-TARGET
                                   ISPEC
                                P2-ISPEC
                                P2-FLAG
                                GLB-LSN
                                GLB-REQUEST
                                GLB-CALL
                                GLB-TEACH
                                P2-TEACH
                                GLB-CURSOR
                                CURSOR-LC
                                GLB-ERROR.

       P-MESSAGE.
           EVALUATE TRUE
             WHEN GLB-MSGINDEX = ZEROS
                  DISPLAY CLEMSG
             WHEN GLB-MSGINDEX = 1
                  MOVE GLB-MSGINDEX TO GLB-INDEX
                  MOVE 25 TO GLB-LINE
                  IF GLB-MSGHEADER(GLB-INDEX) = "ATTENTION"
                     DISPLAY ATTMSG
                  ELSE
                     IF GLB-TARGET = "UNX"
                        CALL X"05"
                        PERFORM P-PAGE2 THRU P-PAGE2-EXT
                     ELSE
                        DISPLAY ERRMSG
                     END-IF
                  END-IF
      *           IF GLB-RECALL NOT = ISPEC AND
      *              GLB-RECALL NOT = SPACES
      *              PERFORM VARYING GLB-TOTAL FROM 1 BY 1
      *                      UNTIL GLB-TOTAL > 1000
      *              END-PERFORM
      *              MOVE ZEROS TO GLB-TOTAL
      *           END-IF
             WHEN OTHER
                  CALL X"05"
                  PERFORM P-PAGE2 THRU P-PAGE2-EXT
           END-EVALUATE
           .
       P-MESSAGE-EXT. EXIT.

       P-PAGE2.
           IF GLB-RECALL NOT = ISPEC AND
              GLB-RECALL NOT = SPACES
              MOVE 1 TO P2-FLAG
           END-IF

           CALL "XSDPAG2" USING
                BY REFERENCE GLB-MESSAGE
                BY REFERENCE GLB-MSGINDEX
                BY REFERENCE P2-ISPEC
                BY REFERENCE P2-TEACH
                BY REFERENCE P2-FLAG
                BY REFERENCE GLB-LSN
           END-CALL
           CANCEL "XSDPAG2"

           IF P2-FLAG = 1
              EXIT PARAGRAPH
           END-IF

           IF P2-ISPEC NOT = SPACES
              MOVE P2-ISPEC TO GLB-RECALL
              MOVE P2-TEACH TO GLB-TEACH
              IF GLB-RECALL = ISPEC
*******          PERFORM CLOSE-FILES
                 MOVE "Y" TO GLB-REQUEST
                 MOVE GLB-RECALL TO GLB-CALL
                 EXIT PROGRAM
              END-IF
              MOVE "Y" TO GLB-REQUEST
*******       PERFORM P-RECALL
           ELSE
              MOVE GLB-CURSOR TO CURSOR-LC
              MOVE "*****" TO GLB-ERROR
*******       GO TO SHOW-SCREEN-D
           END-IF
           .
       P-PAGE2-EXT. EXIT.

       END PROGRAM XSDMSG.
