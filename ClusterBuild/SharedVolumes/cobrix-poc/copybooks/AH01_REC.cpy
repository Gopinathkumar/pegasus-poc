003000 01 AH01-REC.
003100*****************************************************************
003200***  master file common fields
003300*****************************************************************
003400
003500    05 MAST-ACCOUNT-ID          PIC X(12).
003600    05 MAST-REC-TYPE            PIC X(4).
003700**************************************************************
003800* SEGMENT NAME              : AMSAH01                        *
003900**************************************************************
004000* ADD ROLL-DUE-DTE-FLAG                03/07/08 GP4DAC  B13861
004100* CHANGE TYPE FOR AH01-ACTUAL-APR      02/06/06 GP4T2S  B64255
004200* ADD WEIGHT-AVG-APR                   08/19/05 GP4T2S  A63756
004300* ADD NUM-OVERLIMIT                    12/15/04 GP4M1S  A52116    0004000
004400* ADD BKDT-PIF-FLAG                    04/12/04 GP4GE1  A43008    0004000
004500* ADD DATE-HIGH-OL-CTD                 01/14/04 GP4JYH  A48928
004600* ADD STMT-INSERT-GROUP-ID             01/09/04 GP4M1S  A47304
004700* ADD WAIVE-LATE-FEE-FLAG, WAIVE-OVERLIMIT-FEE-FLAG     A39177
004800* ADD RES-PAYMENT-TYPE       7/25/02 FOR J DUDZIK       A31701
004900* REMOVED TRIAD-SCORE-CARD-ID, TRIAD-SCORE-ALIGNED, TRIAD-
005000* SCORE-RAW, MDS-AUTH-BEHAVIOR-SCORE, MDS-COLLECT-BEHAVIOR-SCOR
005100* 07/17/02 G EWING FOR J CARDONA                       A33525
005200**************************************************************
005300*
005400*#IF NOT DOMESTIC
005500*#   ERROR - AMCAH01  ONLY VALID FOR DOMESTIC PROGRAMS
005600*#END-IF
005700*
005800     05 AH01-ACT-CYCLE-HIST-INFO.
005900        10 AH01-SEGMENT-LENGTH                  PIC S9(4) COMP.
006000        10 AH01-ACT-HIST-CYCLE-KEY.
006100           15 AH01-CDATE-CYCLE                  PIC S9(7) COMP-3.
006200        10 AH01-ACT-HIST-CYCLE-DATA             PIC X(324).
006300        10 AH01-CYCLE-DATA REDEFINES AH01-ACT-HIST-CYCLE-DATA.
006400           15 AH01-FIXED-PAYMENT-INDICATOR      PIC X(1).
006500           15 AH01-PAYMENT-STATUS-CTD           PIC X(1).
006600              88 AH01-PAYMENT-NOT-MADE            VALUE 'N'.
006700              88 AH01-PAID-MIN-PAYMENT-DUE        VALUE 'M'.
006800              88 AH01-PAID-IN-FULL                VALUE 'F'.
006900              88 AH01-PAID-LESS-MIN-PAY           VALUE 'S'.
007000              88 AH01-PAID-GT-MIN-LT-FULL         VALUE 'G'.
007100           15 AH01-PAYMENT-TIMING-CTD           PIC X(1).
007200              88 AH01-PAID-BEFORE-DUE-DATE        VALUE 'B'.
007300              88 AH01-PAID-ON-DUE-DATE            VALUE 'O'.
007400              88 AH01-PAID-AFTER-DUE-DATE         VALUE 'A'.
007500           15 AH01-SKIP-A-PAY-FLAG              PIC X(1).
007600              88 AH01-SKIP-A-PAY-SCHED            VALUE 'S'.
007700           15 AH01-SKIP-A-PAY-RESULT            PIC X(1).
007800              88 AH01-SKIP-A-PAY-NO-PAY           VALUE 'S'.
007900              88 AH01-SKIP-A-PAY-PAID             VALUE 'P'.
008000           15 AH01-HIT-HIGH-BALANCE             PIC X(1).
008100              88 AH01-HIT-HIGH-YES                VALUE 'Y'.
008200           15 AH01-PAST-DUE-REAGE               PIC X(1).
008300              88 AH01-PD-REAGE-NO                 VALUE SPACE.
008400              88 AH01-PD-REAGE-MAN                VALUE 'M'.
008500              88 AH01-PD-REAGE-AUTO               VALUE 'A'.
008600              88 AH01-PD-REAGE-NSF                VALUE 'N'.
008700           15 AH01-CHARGEOFF                    PIC X(1).
008800              88 AH01-CHARGEOFF-MAN               VALUE 'M'.
008900              88 AH01-CHARGEOFF-AUTO              VALUE 'A'.
009000           15 AH01-DISCLOSURE-GRP               PIC X(8).
009100           15 AH01-ALTERNATE-DISCLOSURE-GRP     PIC X(8).
009200           15 AH01-ALT-DISCLOSURE-STATUS        PIC X(1).
009300           15 AH01-RES-PAYMENT-TYPE             PIC 9(1).
009400              88 AH01-RES-NO-PAYMENT              VALUE 0.
009500              88 AH01-RES-MAIL-PAYMENT            VALUE 1.
009600              88 AH01-RES-NON-MAIL-PAYMENT        VALUE 2.
009700           15 AH01-STMT-INSERT-GROUP-ID         PIC 9(3).
009800           15 AH01-BKDT-PIF-FLAG                PIC X(1).         0058000
009900              88 AH01-BKDT-PIF-YES                VALUE 'Y'.      0059000
010000              88 AH01-BKDT-PIF-NO                 VALUE 'N'.      0060000
010100           15 AH01-NUM-OVERLIMIT                PIC S9(3) COMP-3.
010200           15 AH01-ACTIVE-INS-CLAIM-LTD         PIC X(1).         0061000
010300           15 FILLER                            PIC X(1).         0061000
010400           15 AH01-CYCLE-BACKDATED-INDICATOR    PIC X(1).
010500              88 AH01-CYCLE-BACKDATED             VALUE 'Y'.
010600              88 AH01-CYCLE-NOT-BACKDATED         VALUE ' '.
010700           15 AH01-PAID-IN-FULL-FLAG            PIC X(1).
010800              88 AH01-PAID-IN-FULL-YES            VALUE 'Y'.
010900              88 AH01-PAID-IN-FULL-NO             VALUE 'N'.
011000           15 AH01-AMT-BKDT-CTD                 PIC S9(11)V9(2)
011100               COMP-3.
011200           15 AH01-HIGH-BAL-SINCE-PIF           PIC S9(11)V99
011300               COMP-3.
011400           15 AH01-CNCY-CODE                    PIC X(03).
011500           15 AH01-SPLIT-RATE-DATE              PIC S9(7) COMP-3.
011600           15 AH01-OPT-SET-ACCT-FEE             PIC X(4).
011700           15 AH01-OPT-SET-ANN-FEE              PIC X(4).
011800           15 AH01-OPT-SET-TRANS-FEE            PIC X(4).
011900           15 AH01-WEIGHTED-AVG-APR             PIC S9(3)V99
012000                                                  COMP-3.
012100           15 AH01-ROLL-DUE-DTE-FLAG            PIC X(1).
012200              88 AH01-NOT-CHNG-DUE-DTE            VALUE 'N'.
012300              88 AH01-CHNG-DUE-DTE                VALUE 'Y'.
012400           15 FILLER                            PIC X(09).
012500           15 AH01-AMT-NON-NSF-PMT-REV-CTD      PIC S9(11)V9(2)
012600               COMP-3.
012700           15 AH01-CREDIT-LMT-LAST-PAY-CALC     PIC S9(11) COMP-3.
012800           15 AH01-BKDT-TRAN-AMT                PIC S9(11)V9(2)
012900               COMP-3.
013000           15 AH01-AMT-EST-INTERCHG-CTD         PIC S9(11)V9(2)
013100               COMP-3.
013200           15 AH01-AMT-HIGH-OVERLIMIT-CTD       PIC S9(11)V9(2)
013300               COMP-3.
013400           15 AH01-WAIVE-OVERLIMIT-FEE-FLAG     PIC X(01).
013500           15 AH01-WAIVE-LATE-FEE-FLAG          PIC X(01).
013600           15 AH01-DATE-HIGH-OL-CTD             PIC S9(7) COMP-3.
013700           15 AH01-MDS-REISSUE-SCORE            PIC S9(5) COMP-3.
013800           15 AH01-BALANCE-CURRENT              PIC S9(11)V9(2)
013900               COMP-3.
014000           15 AH01-LIMIT-CREDIT                 PIC S9(11) COMP-3.
014100           15 AH01-AMT-PASTDUE                  PIC S9(11)V9(2)
014200               OCCURS 12 TIMES COMP-3.
014300           15 AH01-AMT-OVERLIMIT                PIC S9(11)V9(2)
014400               COMP-3.
014500           15 AH01-AMT-ACTIVE-DISPUTES-OUT      PIC S9(11)V9(2)
014600               COMP-3.
014700           15 AH01-CALC-MIN-PAYMENT             PIC S9(11)V9(2)
014800               COMP-3.
014900           15 AH01-STMT-MIN-PAYMENT             PIC S9(11)V9(2)
015000               COMP-3.
015100           15 AH01-DATE-PAYMENT-DUE             PIC S9(7) COMP-3.
015200           15 AH01-NUM-PAYMENTS                 PIC S9(7) COMP-3.
015300           15 AH01-AMT-PAYMENTS                 PIC S9(11)V9(2)
015400               COMP-3.
015500           15 AH01-AMT-PREPAYMENT               PIC S9(11)V9(2)
015600               COMP-3.
015700           15 AH01-AMT-NSF-PAYMENTS             PIC S9(11)V9(2)
015800               COMP-3.
015900           15 AH01-NUM-NSF-RETURNS              PIC S9(3) COMP-3.
016000           15 AH01-AMT-SMALL-BAL-WRITEOFF       PIC S9(5)V9(2)
016100               COMP-3.
016200           15 AH01-ACTUAL-APR                   PIC S9(8)V9(3)
016300               COMP-3.
016400           15 AH01-NUM-DAYS-IN-BILLING-CYC      PIC S9(3) COMP-3.
016500           15 AH01-BALANCE-PREV-STATEMENT       PIC S9(11)V9(2)
016600               COMP-3.
016700           15 AH01-DATE-PREV-STATEMENT          PIC S9(7) COMP-3.
016800           15 AH01-NUM-MISC-DEBITS-CTD          PIC S9(5) COMP-3.
016900           15 AH01-AMT-MISC-DEBITS-CTD          PIC S9(11)V9(2)
017000               COMP-3.
017100           15 AH01-NUM-MISC-CREDITS-CTD         PIC S9(5) COMP-3.
017200           15 AH01-AMT-MISC-CREDITS-CTD         PIC S9(11)V9(2)
017300               COMP-3.
017310    05 FILLER                           PIC X(4)    .
017400
