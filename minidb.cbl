       >>SOURCE FORMAT FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. MINIDB.
 
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT CUSTOMER-FILE ASSIGN TO "customers.dat"
        ORGANIZATION IS LINE SEQUENTIAL
        FILE STATUS IS WS-FILE-STATUS.
 
    SELECT TEMP-FILE ASSIGN TO "customers.tmp"
        ORGANIZATION IS LINE SEQUENTIAL
        FILE STATUS IS WS-TEMP-STATUS.
 
DATA DIVISION.
FILE SECTION.
FD  CUSTOMER-FILE.
01  CUSTOMER-RECORD.
    05  CUST-ID        PIC 9(5).
    05  CUST-NAME      PIC X(20).
    05  CUST-EMAIL     PIC X(30).
 
FD  TEMP-FILE.
01  TEMP-RECORD.
    05  TEMP-ID        PIC 9(5).
    05  TEMP-NAME      PIC X(20).
    05  TEMP-EMAIL     PIC X(30).
 
WORKING-STORAGE SECTION.
01  WS-FILE-STATUS     PIC XX.
01  WS-TEMP-STATUS     PIC XX.
01  WS-MENU-CHOICE     PIC 9.
01  WS-CONTINUE-FLAG   PIC X VALUE "Y".
01  WS-FOUND-FLAG      PIC X VALUE "N".
 
01  WS-INPUT-ID        PIC 9(5).
01  WS-INPUT-NAME      PIC X(20).
01  WS-INPUT-EMAIL     PIC X(30).
 
01  WS-OLD-NAME        PIC X(20) VALUE "customers.dat".
01  WS-TMP-NAME        PIC X(20) VALUE "customers.tmp".
01  WS-RENAME-RESULT   PIC S9(9) COMP-5.
 
PROCEDURE DIVISION.
MAIN-LOGIC.
    PERFORM UNTIL WS-CONTINUE-FLAG = "N"
        PERFORM DISPLAY-MENU
        PERFORM GET-CHOICE
        EVALUATE WS-MENU-CHOICE
            WHEN 1 PERFORM ADD-RECORD
            WHEN 2 PERFORM REMOVE-RECORD
            WHEN 3 PERFORM RETRIEVE-RECORD
            WHEN 4 MOVE "N" TO WS-CONTINUE-FLAG
            WHEN OTHER
                DISPLAY "Invalid choice, try again."
        END-EVALUATE
    END-PERFORM.
 
    STOP RUN.
 
DISPLAY-MENU.
    DISPLAY " ".
    DISPLAY "==== Mini Customer Database ====".
    DISPLAY "1. Add record".
    DISPLAY "2. Remove record".
    DISPLAY "3. Retrieve record".
    DISPLAY "4. Exit".
    DISPLAY "Enter choice: " WITH NO ADVANCING.
 
GET-CHOICE.
    ACCEPT WS-MENU-CHOICE.
 
*> ADD: append a new record, after checking the ID isn't already
*> in use (avoids silent duplicates).

ADD-RECORD.
    DISPLAY "Enter ID (5 digits): " WITH NO ADVANCING.
    ACCEPT WS-INPUT-ID.
 
    MOVE "N" TO WS-FOUND-FLAG.
    OPEN INPUT CUSTOMER-FILE.
    IF WS-FILE-STATUS = "00"
        PERFORM UNTIL WS-FILE-STATUS = "10"
            READ CUSTOMER-FILE
                AT END
                    MOVE "10" TO WS-FILE-STATUS
                NOT AT END
                    IF CUST-ID = WS-INPUT-ID
                        MOVE "Y" TO WS-FOUND-FLAG
                    END-IF
            END-READ
        END-PERFORM
        CLOSE CUSTOMER-FILE
    END-IF.
 
    IF WS-FOUND-FLAG = "Y"
        DISPLAY "Error: ID " WS-INPUT-ID " already exists."
    ELSE
        DISPLAY "Enter name: " WITH NO ADVANCING
        ACCEPT WS-INPUT-NAME
        DISPLAY "Enter email: " WITH NO ADVANCING
        ACCEPT WS-INPUT-EMAIL
 
        OPEN EXTEND CUSTOMER-FILE
        IF WS-FILE-STATUS NOT = "00"
            OPEN OUTPUT CUSTOMER-FILE
        END-IF
 
        MOVE WS-INPUT-ID    TO CUST-ID
        MOVE WS-INPUT-NAME  TO CUST-NAME
        MOVE WS-INPUT-EMAIL TO CUST-EMAIL
        WRITE CUSTOMER-RECORD
        CLOSE CUSTOMER-FILE
        DISPLAY "Record added."
    END-IF.
 
*> REMOVE: copy every record except the matching one into a temp
*> file, then swap the temp file in for the original.

REMOVE-RECORD.
    DISPLAY "Enter ID to remove: " WITH NO ADVANCING.
    ACCEPT WS-INPUT-ID.
 
    MOVE "N" TO WS-FOUND-FLAG.
    OPEN INPUT CUSTOMER-FILE.
    OPEN OUTPUT TEMP-FILE.
 
    PERFORM UNTIL WS-FILE-STATUS = "10"
        READ CUSTOMER-FILE
            AT END
                MOVE "10" TO WS-FILE-STATUS
            NOT AT END
                IF CUST-ID = WS-INPUT-ID
                    MOVE "Y" TO WS-FOUND-FLAG
                ELSE
                    MOVE CUST-ID    TO TEMP-ID
                    MOVE CUST-NAME  TO TEMP-NAME
                    MOVE CUST-EMAIL TO TEMP-EMAIL
                    WRITE TEMP-RECORD
                END-IF
        END-READ
    END-PERFORM.
 
    CLOSE CUSTOMER-FILE.
    CLOSE TEMP-FILE.
 
    IF WS-FOUND-FLAG = "Y"
        CALL "CBL_DELETE_FILE" USING WS-OLD-NAME
        CALL "CBL_RENAME_FILE" USING WS-TMP-NAME WS-OLD-NAME
            RETURNING WS-RENAME-RESULT
        DISPLAY "Record removed."
    ELSE
        CALL "CBL_DELETE_FILE" USING WS-TMP-NAME
        DISPLAY "Error: ID " WS-INPUT-ID " not found."
    END-IF.

*> RETRIEVE: scan for a matching ID and display it.
RETRIEVE-RECORD.
    DISPLAY "Enter ID to retrieve: " WITH NO ADVANCING.
    ACCEPT WS-INPUT-ID.
 
    MOVE "N" TO WS-FOUND-FLAG.
    OPEN INPUT CUSTOMER-FILE.
    PERFORM UNTIL WS-FILE-STATUS = "10"
        READ CUSTOMER-FILE
            AT END
                MOVE "10" TO WS-FILE-STATUS
            NOT AT END
                IF CUST-ID = WS-INPUT-ID
                    MOVE "Y" TO WS-FOUND-FLAG
                    DISPLAY "ID:    " CUST-ID
                    DISPLAY "Name:  " CUST-NAME
                    DISPLAY "Email: " CUST-EMAIL
                END-IF
        END-READ
    END-PERFORM.
    CLOSE CUSTOMER-FILE.
 
    IF WS-FOUND-FLAG = "N"
        DISPLAY "Error: ID " WS-INPUT-ID " not found."
    END-IF.
