      $set CallFh"FHREDIR"
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CWHELPFS.
       AUTHOR.        COBOLware Services Ltda.
       DATE-WRITTEN.  06/10/2015.
       SECURITY.      *************************************************
                      *                                               *
                      *   Apoio a CWHELP para acesso ao FileShare
                      *                                               *
                      *************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT HELPIN ASSIGN TO DISK
                  ORGANIZATION  IS BINARY SEQUENTIAL
                  LOCK MODE     IS EXCLUSIVE
                  FILE STATUS   IS FS-HELPIN.

       DATA DIVISION.
       FILE SECTION.

       FD  HELPIN
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS LB-HELPIN.

       01  HELPIN-REG                       PIC  X.

       WORKING-STORAGE SECTION.

       LINKAGE SECTION.

       01   OP-HELPIN  PIC X(01).
       01   LB-HELPIN  PIC X(50).
       01   FS-HELPIN  PIC X(02).
       01   RG-HELPIN  PIC X(01).

       PROCEDURE DIVISION USING OP-HELPIN
                                LB-HELPIN
                                FS-HELPIN
                                RG-HELPIN.

           EVALUATE OP-HELPIN
                WHEN 'I'
                     PERFORM TEST AFTER UNTIL FS-HELPIN NOT = '9A'
                             OPEN INPUT HELPIN
                     END-PERFORM
                WHEN 'R'
                     READ HELPIN INTO RG-HELPIN
                WHEN 'C'
                     CLOSE HELPIN
           END-EVALUATE.

       END PROGRAM CWHELPFS.
