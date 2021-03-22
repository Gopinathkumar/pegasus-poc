001100 01 AH00-REC.
001200*****************************************************************
001300***  master file common fields
001400*****************************************************************
001500
001600    05 MAST-ACCOUNT-ID          PIC X(12).
001700    05 MAST-REC-TYPE            PIC X(4).
001800 05  AH00-ACCOUNT-HISTORY-BASE.
001900      10 AH00-SEGMENT-LENGTH USAGE COMP
002000                              PIC S9(4).
002100      10 AH00-ACT-HISTORY-KEY.
002200       15 AH00-CLIENT-NUM     PIC 9(04).
002300       15 AH00-APPLICATION-NUM
002400                              PIC 9(12).
002500       15 AH00-APPLICATION-SUFFIX
002600                              PIC 9(2).
002700      10 AH00-ACT-HISTORY-DATA.
002800       15 AH00-FILLER-AMSAH00 PIC X(10).
002810    05 FILLER                           PIC X(304)    .
