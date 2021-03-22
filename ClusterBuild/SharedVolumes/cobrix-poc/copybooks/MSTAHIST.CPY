000100
000200*****************************************************************
000210*** ACCOUNT HISTORY                                                       
000400*** ALL RECORDS ARE FIXED LENGTH 350 BYTES FOR PCBANK                     
000500*****************************************************************
000600
000300 01 MAST-HEADER-REC.
000400    05 HEADER-ACCOUNT-ID                PIC X(12)     .
000500    05 HEADER-RECORD-TYPE               PIC X(4)      .
000600    05 CLIENT-NUMBER                    PIC X(4)      .
000700    05 HEADER-TRANS-ID                  PIC X(8)      .
000800    05 FILE-CREATION-DATE               PIC X(8)      .
000900    05 FILLER                           PIC X(314)    .
001000
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
002900
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
017500 01 AH31-REC.
017600*****************************************************************
017700***  master file common fields
017800*****************************************************************
017900
018000    05 MAST-ACCOUNT-ID          PIC X(12).
018100    05 MAST-REC-TYPE            PIC X(4).
018200******************* DDS COPYLIB GENERATION ******************
018300* Segment name              : AMSAH31
018400* GENERATION DATE AND TIME  : 08/07/98 14:13                      0003000
018500* GENERATED BY USER/PROJECT : DB2ADM12 / IAS                      0004000
018600**************************************************************
018700     05 AH31-BALANCE-HISTORY-INFO.
018800        10 AH31-SEGMENT-LENGTH                  PIC S9(4) COMP.
018900        10 AH31-BALANCE-HISTORY-KEY.
019000           15 AH31-TRANSACTION-CATEGORY         PIC 9(4).
019100        10 AH31-BALANCE-HISTORY-DATA.
019200           15 AH31-CLASS-CODE                   PIC X(2).
019300           15 AH31-DEFER-MIN-PAY-FLAG           PIC X(1).
019400              88 AH31-DEFER-MIN-PAY-YES           VALUE 'Y'.
019500           15 AH31-DEFER-FCHG-FLAG              PIC X(1).
019600              88 AH31-DEFER-FCHG-YES              VALUE 'Y'.
019700           15 AH31-ADB-CURR-SUBJ-FCHG-FLAG      PIC X(1).
019800              88 AH31-ADB-CURR-SUBJ-FCHG-YES      VALUE 'Y'.
019900              88 AH31-ADB-CURR-SUBJ-FCHG-NO       VALUE 'N'.
020000           15 AH31-ADB-CYC1P-SUBJ-FCHG-FLAG     PIC X(1).
020100              88 AH31-ADB-CYC1P-SUBJ-FCHG-YES     VALUE 'Y'.
020200              88 AH31-ABD-CYC1P-SUBJ-FCHG-NO      VALUE 'N'.
020300           15 AH31-ADB-FCHG-SUBJ-FCHG-FLAG      PIC X(1).
020400              88 AH31-ADB-FCHG-SUBJ-FCHG-YES      VALUE 'Y'.
020500              88 AH31-ADB-FCHG-SUBJ-FCHG-NO       VALUE 'N'.
020600           15 AH31-PRV-CYC-ADB-CURR-SBJ-FCHG    PIC X(1).
020700              88 AH31-PRV-CYC-ADB-SUBJ-FCHG-YES   VALUE 'Y'.
020800              88 AH31-PRV-CYC-ADB-SUBJ-FCHG-NO    VALUE 'N'.
020900           15 AH31-GRACE-METHOD                 PIC X(1).
021000              88 AH31-GRACE-NO-FC                 VALUE '1'.
021100              88 AH31-GRACE-FC-IF-CURR-ACTIVE     VALUE '2'.
021200              88 AH31-GRACE-FC-ON-CURR-ACTIVE     VALUE '3'.
021300              88 AH31-GRACE-NO-GRACE-PERIOD       VALUE '4'.
021400           15 FILLER                            PIC X(2).
021500           15 AH31-DBT-NUM-CREDITS-CYC          PIC S9(7) COMP-3.
021600           15 AH31-DBT-NUM-DEBITS-CYC           PIC S9(7) COMP-3.
021700           15 AH31-DBT-AMT-CREDITS-CYC          PIC S9(11)V9(2)
021800               COMP-3.
021900           15 AH31-DBT-AMT-DEBITS-CYC           PIC S9(11)V9(2)
022000               COMP-3.
022100           15 AH31-TERMS-BALANCE-CODE           PIC 9(4).
022200           15 FILLER                            PIC X(18).
022300           15 AH31-FEE-ADJ-FOR-BKDT-AMT         PIC S9(11)V9(2)
022400               COMP-3.
022500           15 AH31-AMT-BALANCE-SEG-DISPUTE      PIC S9(11)V9(2)
022600               COMP-3.
022700           15 AH31-AMT-CURRENT-OUT              PIC S9(11)V9(2)
022800               COMP-3.
022900           15 AH31-AMT-CYCLE1P-OUT              PIC S9(11)V9(2)
023000               COMP-3.
023100           15 AH31-NUM-DEBITS-CTD               PIC S9(7) COMP-3.
023200           15 AH31-AMT-DEBITS-CTD               PIC S9(11)V9(2)
023300               COMP-3.
023400           15 AH31-NUM-CREDITS-CTD              PIC S9(7) COMP-3.
023500           15 AH31-AMT-CREDITS-CTD              PIC S9(11)V9(2)
023600               COMP-3.
023700           15 AH31-NUM-CREDIT-ADJUST-CTD        PIC S9(7) COMP-3.
023800           15 AH31-AMT-CREDIT-ADJUST-CTD        PIC S9(11)V9(2)
023900               COMP-3.
024000           15 AH31-NUM-DEBIT-ADJUST-CTD         PIC S9(7) COMP-3.
024100           15 AH31-AMT-DEBIT-ADJUST-CTD         PIC S9(11)V9(2)
024200               COMP-3.
024300           15 AH31-AMT-PAYMENT-APPLIED          PIC S9(11)V9(2)
024400               COMP-3.
024500           15 AH31-AMT-DEBITS-APPLIED           PIC S9(11)V9(2)
024600               COMP-3.
024700           15 AH31-AMT-CREDITS-APPLIED          PIC S9(11)V9(2)
024800               COMP-3.
024900           15 AH31-PRIN-COMPUTED-PAY-DUE        PIC S9(11)V9(2)
025000               COMP-3.
025100           15 AH31-BALANCE-PREVIOUS-PRIN        PIC S9(11)V9(2)
025200               COMP-3.
025300           15 AH31-BALANCE-PREVIOUS-FCHG        PIC S9(11)V9(2)
025400               COMP-3.
025500           15 AH31-AMT-DEFERRED-FCHG            PIC S9(11)V9(2)
025600               COMP-3.
025700           15 AH31-AMT-FCHG-DEBITS-CTD          PIC S9(11)V9(2)
025800               COMP-3.
025900           15 AH31-AMT-FCHG-CREDITS-CTD         PIC S9(11)V9(2)
026000               COMP-3.
026100           15 AH31-AMT-FCHG-OUT                 PIC S9(11)V9(2)
026200               COMP-3.
026300           15 AH31-AMT-FCHG-BILLED              PIC S9(11)V9(2)
026400               COMP-3.
026500           15 AH31-AMT-ADB-NOT-SUB-FCHG         PIC S9(11)V9(2)
026600               COMP-3.
026700           15 AH31-HAPR-ADB-AMT                 PIC S9(11)V9(2)
026800               COMP-3.
026900           15 AH31-AMT-ADB-CURRENT              PIC S9(11)V9(2)
027000               COMP-3.
027100           15 AH31-AMT-ADB-CYCLE1P              PIC S9(11)V9(2)
027200               COMP-3.
027300           15 AH31-AMT-ADB-FCHG                 PIC S9(11)V9(2)
027400               COMP-3.
027500           15 AH31-PREV-CYC-FCHG                PIC S9(11)V9(2)   0093000
027600               COMP-3.
027610    05 FILLER                           PIC X(82)    .                    
027700
027800 01 AH32-REC.
027900*****************************************************************
028000***  master file common fields
028100*****************************************************************
028200
028300    05 MAST-ACCOUNT-ID          PIC X(12).
028400    05 MAST-REC-TYPE            PIC X(4).
028500******************* DDS COPYLIB GENERATION ******************
028600* SEGMENT NAME              : AMSAH32
028700* GENERATION DATE AND TIME  : 03/15/99 15:15
028800* GENERATED BY USER/PROJECT : DB2ADM12 / IAS
028900* ADDED AH32-RB-ADJ-FOR-BKDT-AMT    TIM SIMON              A43008
029000* ADD REACHBACK-APR                                        A43684
029100* ADD REACHBACK-MPR-DPR, REACHBACK-ASSESSED, REACHBACK-ADB A43684
029200**************************************************************
029300     05 AH32-TERMS-BALANCE-INFO.
029400        10 AH32-SEGMENT-LENGTH                  PIC S9(4) COMP.
029500        10 AH32-TERMS-BALANCE-KEY.
029600           15 AH32-TERMS-BALANCE-CODE           PIC 9(4).
029700        10 AH32-MIN-FCHG-FLAG                   PIC X(1).
029800           88 AH32-MIN-FCHG-YES                   VALUE 'Y'.
029900           88 AH32-MIN-FCHG-NO                    VALUE 'N'.
030000        10 AH32-OPTION-SET                      PIC X(4).
030100        10 AH32-CMP-METHOD                      PIC X(2).
030200           88 AH32-CMP-METHOD-FIXED               VALUE '01'.
030300           88 AH32-CMP-METHOD-VARIABLE            VALUE '02'.
030400           88 AH32-CMP-METHOD-FLOAT               VALUE '03'.
030500           88 AH32-CMP-METHOD-BREAK               VALUE '04'.
030600           88 AH32-CMP-METHOD-EQUITY              VALUE '05'.
030700        10 AH32-FINCHG-METHOD                   PIC X(1).
030800        10 AH32-OVERRIDE-IN-EFFECT              PIC X(1).
030900           88 AH32-YES-IN-EFFECT                  VALUE 'Y'.
031000           88 AH32-NOT-IN-EFFECT                  VALUE 'N'.
031100        10 FILLER                               PIC X(41).
031200        10 AH32-FCHG-ADJ-FOR-BKDT-AMT           PIC S9(11)V9(2)
031300            COMP-3.
031400        10 AH32-BSTFC-TOT-AGGR                  PIC S9(11)V9(2)
031500            COMP-3.
031600        10 AH32-BSTFC-TOT-ADB                   PIC S9(11)V9(2)
031700            COMP-3.
031800        10 AH32-AMT-FCHG                        PIC S9(11)V9(2)
031900            COMP-3.
032000        10 AH32-REACHBACK-ADB                   PIC S9(11)V9(2)
032100            COMP-3.
032200        10 AH32-REACHBACK-ASSESSED              PIC S9(11)V9(2)
032300            COMP-3.
032400        10 AH32-REACHBACK-MPR-DPR               PIC S9(2)V9(7)
032500            COMP-3.
032600        10 AH32-REACHBACK-APR                   PIC S9(4)V9(9)
032700            COMP-3.
032800        10 AH32-RB-ADJ-FOR-BKDT-AMT             PIC S9(11)V9(2)
032900            COMP-3.
033000        10 FILLER                               PIC X(17).
033100        10 AH32-TERM-BALANCE-BILLED-INFO OCCURS 3 TIMES.
033200           15 AH32-APR-TYPE-FLAG                PIC X(1).
033300              88 AH32-APR-FIXED                   VALUE 'F'.
033400              88 AH32-APR-VARIABLE                VALUE 'V'.
033500           15 AH32-APR-PERIOD-FLAG              PIC X(1).
033600              88 AH32-APR-DAILY                   VALUE 'D'.
033700              88 AH32-APR-MONTHLY                 VALUE 'M'.
033800           15 AH32-AMT-BALANCED-BILLED          PIC S9(11)V9(2)
033900               COMP-3.
034000           15 AH32-AMT-FCHG-BILLED              PIC S9(11)V9(2)
034100               COMP-3.
034200           15 AH32-APR-BILLED                   PIC S9(4)V9(9)
034300               COMP-3.
034400           15 AH32-MPR-DPR-BILLED               PIC S9(2)V9(7)
034500               COMP-3.
034600        10 FILLER                               PIC X(88).
034610    05 FILLER                           PIC X(28)    .
034700
034800 01 AH35-REC.
034900*****************************************************************
035000***  master file common fields
035100*****************************************************************
035200
035300    05 MAST-ACCOUNT-ID          PIC X(12).
035400    05 MAST-REC-TYPE            PIC X(4).
035500******************* DDS COPYLIB GENERATION ******************
035600* SEGMENT NAME              : AMSAH35
035700* REDEFINE OF AH35-PREV-CYC-ADB        05/22/08  GP4DJM B26036
035800*          AS AH35-SAC-ACCRUED-FC-NSUB-IVA
035900* ADD AH35-AMT-DIR-PAY-PRIN-ONLY-CTD             GP4LH5 B26618
036000* ADD SAC-CREDIT-ADJ        : GP4M1S / 08/24/04 / A51109
036100* ADD CURR-CYC-ADB-AT-GOTO- : GP4GE1 / 09/12/03 / A32329
036200* RATE, AMT-FINCHG-AT-GOTO- : GP4GE1 / 09/12/03 / A32329
036300* RATE                      : GP4GE1 / 09/12/03 / A32329
036400* ADD AMT-TRANSACTION       : GP4GE1 / 09/06/01 / A28932
036500* ADD AMT-CREDIT-CTD        : GP4KXF / 02/09/01 / A22520
036600* ADD 2 NEW FIELDS          : GP4KXF / 01/12/01 / A22520
036700* GENERATION DATE AND TIME  : 02/10/99 17:17
036800* GENERATED BY USER/PROJECT : DB2ADM12 / IAS
036900**************************************************************
037000     05 AH35-TLP-HIST.
037100        10 AH35-SEGMENT-LENGTH             PIC S9(04) COMP.
037200        10 AH35-EXPIRATION-DATE            PIC S9(07) COMP-3.
037300        10 AH35-TLP-TYPE-TRAN              PIC  X(01).
037400           88 AH35-SAME-AS-CASH                    VALUE 'S'.
037500           88 AH35-INSTALLMENT-BILLING             VALUE 'I'.
037600           88 AH35-DEFERRED-BILLING                VALUE 'D'.
037700           88 AH35-SAME-AS-CASH-FIX-DATE           VALUE 'C'.
037800           88 AH35-DEFERRED-BILL-FIX-DATE          VALUE 'F'.
037900           88 AH35-DEFERRED-EXPIRED                VALUE 'E'.
038000           88 AH35-SAME-AS-CASH-EXPIRED            VALUE 'R'.
038100           88 AH35-SAC-EXP-AMT-CURR-BUCKETS        VALUE 'T'.
038200           88 AH35-DFR-EXP-AMT-CURR-BUCKETS        VALUE 'G'.
038300        10 AH35-TLP-KEY.
038400           15 AH35-CDATE-STMT-BEG          PIC S9(07) COMP-3.
038500           15 AH35-DATE-POST               PIC S9(07) COMP-3.
038600           15 AH35-TIME-POST               PIC S9(07) COMP-3.
038700        10 AH35-TLP-HIST-DATA              PIC X(181).
038800        10 AH35-SAC-DFR-HIST-DATA REDEFINES AH35-TLP-HIST-DATA.
038900           15 AH35-AMT-OUTSTANDING         PIC S9(11)V9(2) COMP-3.
039000           15 AH35-AMT-FINCHG-OUTSTANDING  PIC S9(11)V9(2) COMP-3.
039100           15 AH35-AMT-FINCHG-ACCRUED      PIC S9(11)V9(2) COMP-3.
039200           15 AH35-AMT-FINCHG-BILLED       PIC S9(11)V9(2) COMP-3.
039300           15 AH35-DISP-IND                PIC  X(01).
039400           15 AH35-EXP-CPT-IND             PIC  X(01).
039500           15 AH35-TERMS-BALANCE-CODE      PIC  9(04).
039600           15 AH35-EXP-CPT-FLAG            PIC  X(01).
039700           15 AH35-PROC-FCHG-IND           PIC  X(01).
039800           15 AH35-TLP-LAST-NUM            PIC S9(05) COMP-3.
039900           15 AH35-PREV-CYC-PRIN-BAL-OUT   PIC S9(11)V9(2) COMP-3.
040000           15 AH35-PREV-CYC-AMT-FCHG-OUT   PIC S9(11)V9(2) COMP-3.
040100           15 AH35-CURR-CR-AMT-APPLIED     PIC S9(11)V9(2) COMP-3.
040200           15 AH35-CURR-PAY-APPLIED        PIC S9(11)V9(2) COMP-3.
040300           15 AH35-AMT-FCHG-BILLED-CURR    PIC S9(11)V9(2) COMP-3.
040400           15 AH35-AMT-FCHG-ACCRUED-CURR   PIC S9(11)V9(2) COMP-3.
040500           15 AH35-AMT-DIR-PAY-PRIN-ONLY-CTD REDEFINES
040600              AH35-AMT-FCHG-ACCRUED-CURR   PIC S9(11)V9(2) COMP-3.
040700           15 AH35-PREV-CYC-ADB            PIC S9(11)V9(2) COMP-3.
040800           15 AH35-SAC-ACCRUED-FC-NSUB-IVA
040900               REDEFINES AH35-PREV-CYC-ADB PIC S9(11)V9(2) COMP-3.
041000           15 AH35-CURR-CYC-ADB            PIC S9(11)V9(2) COMP-3.
041100           15 AH35-AMT-CURR-DEFERRED       PIC S9(11)V9(2) COMP-3.
041200           15 AH35-AMT-PAYMENT-DUE         PIC S9(11)V9(2) COMP-3.
041300           15 AH35-PLAN-NEXT-PAYMENT-DATE  PIC S9(07)      COMP-3.
041400           15 AH35-PAYMENT-RATE            PIC S9(11)V9(2) COMP-3.
041500           15 AH35-PAYMENT-IND             PIC  X(01).
041600              88 AH35-STANDARD-SAC                 VALUE 'S'.
041700              88 AH35-MONTHLY-PAY-DUE              VALUE 'M'.
041800              88 AH35-NON-MONTHLY-NO-PAY-DUE       VALUE 'N'.
041900              88 AH35-NON-MONTHLY-PAY-DUE          VALUE 'P'.
042000           15 AH35-INS-BILLED-CTD          PIC S9(05)V9(2) COMP-3.
042100           15 AH35-INS-ACCRUED-FOR-PAYMENT PIC S9(05)V9(2) COMP-3.
042200           15 AH35-RETL-ASGN-TCAT               PIC X(04).
042300           15 AH35-ORIGINAL-TCAT                PIC X(04).
042400           15 AH35-CLASS-CODE                   PIC X(02).
042500           15 AH35-AMT-CREDIT-CTD               PIC S9(11)V99
042600              COMP-3.
042700           15 AH35-AMT-TRANSACTION              PIC S9(11)V99
042800              COMP-3.
042900           15 AH35-EXPIRATION-DATE-ACTUAL       PIC S9(07) COMP-3.
043000           15 AH35-CURR-CYC-ADB-AT-GOTO-RATE    PIC S9(11)V99
043100                                                    COMP-3.
043200           15 AH35-AMT-FINCHG-AT-GOTO-RATE      PIC S9(11)V99
043300                                                    COMP-3.
043400           15 AH35-SAC-CREDIT-ADJ               PIC S9(11)V99
043500                                                    COMP-3.
043600           15 FILLER                            PIC X(03).
043610    05 FILLER                           PIC X(134)   .
043700
043800 01 AH02-CREDLIM-CHG              .
043900*****************************************************************
044000***  master file common fields
044100*****************************************************************
044200
044300    05 MAST-ACCOUNT-ID          PIC X(12).
044400    05 MAST-REC-TYPE            PIC X(4).
044500******************* DDS COPYLIB GENERATION ******************
044600* SEGMENT NAME              : AMSAH02
044700* CHANGED AH02 CLCHG-TYPE ADDED 88 GP4HM1     01/10/08     B16712
044800* ADDED AH02-PTP-TYPE              GP4DAC     07/18/07     B14210
044900*       AH02-PTP-GROUP             GP4DAC     07/18/07     B14210
045000*       AH02-FREQUENCY-CODE        GP4DAC     07/18/07     B14210
045100*       AH02-DATE-FINAL-PTP-DUE    GP4DAC     07/18/07     B14210
045200*       AH02-NUM-TOTAL-RECURRENCES GP4DAC     07/18/07     B14210
045300*       AH02-NUM-RECURRENCE        GP4DAC     07/18/07     B14210
045400*       AH02-AMT-PTP-AGGREGATE     GP4DAC     07/18/07     B14210
045500* ADDED HIST                       GP4LH5     08/31/06     B01779
045600* ADDED REAGE & UPAGE REDEFS:      GP4M1S  /  10/22/04  /  A52556
045700* GENERATION DATE AND TIME  : 02/25/97 17:04
045800* GENERATED BY USER/PROJECT : DB2ADM12 / IAS AMCAM0BR
045900**************************************************************
046000     05 AH02-ACT-HIST-DETAIL-INFO.
046100        10 AH02-SEGMENT-LENGTH                  PIC S9(4) COMP.
046200        10 AH02-ACT-HIST-DETAIL-KEY.
046300           15 AH02-HISTORY-TYPE                 PIC 9(2).
046400              88 AH02-CREDIT-LIMIT-CHANGES        VALUE 00.
046500              88 AH02-PROMISE-TO-PAYS             VALUE 01.
046600              88 AH02-DISPUTES                    VALUE 02.
046700              88 AH02-CHG-OFF-RIST-HIST           VALUE 03.
046800              88 AH02-AGENCY-HIST                 VALUE 04.
046900              88 AH02-REAGE-HISTORY               VALUE 05.
047000              88 AH02-ADJ-PAST-DUE-HIST           VALUE 06.
047100              88 AH02-HIST-BACK-UP                VALUE 08.
047200           15 AH02-CDATE-HISTORY                PIC S9(7) COMP-3.
047300           15 AH02-CTIME-HISTORY                PIC S9(7) COMP-3.
047400
047500        10 AH02-ACT-HIST-DETAIL-DATA            PIC X(268).
047600
047700        10 AH02-HIST-CREDIT-LIMIT-CHG REDEFINES
047800            AH02-ACT-HIST-DETAIL-DATA.
047900           15 AH02-OPERATOR-ID                  PIC X(6).
048000           15 AH02-CLCHG-TYPE                   PIC X(1).
048100              88 AH02-CLCHG-MANUAL                VALUE 'M'.
048200              88 AH02-CLCHG-AUTO                  VALUE 'A'.
048300              88 AH02-CLCHG-TRIAD                 VALUE 'T'.
048400              88 AH02-CLCHG-PRODCHNG              VALUE 'C'.
048500              88 AH02-CLCHG-PROBE                 VALUE 'P'.
048600           15 AH02-CLCHG-APPROVAL               PIC X(6).
048700           15 AH02-CLCHG-STATUS                 PIC X(1).
048800           15 AH02-CLCHG-FEE-OVR                PIC X(1).
048900           15 FILLER                            PIC X(100).
049000           15 AH02-LIMIT-CLCHG-BEFORE           PIC S9(11) COMP-3.
049100           15 AH02-AMT-CLCHG                    PIC S9(11) COMP-3.
049200           15 AH02-CLCHG-FEE-OVR-MTHS           PIC S9(3) COMP-3.
049300           15 FILLER                            PIC X(139).
049310    05 FILLER                           PIC X(54)   .                     
049400
049500 01 AH02-PTP                      .
049600*****************************************************************
049700***  master file common fields
049800*****************************************************************
049900
050000    05 MAST-ACCOUNT-ID          PIC X(12).
050100    05 MAST-REC-TYPE            PIC X(4).
050200******************* DDS COPYLIB GENERATION ******************
050300* SEGMENT NAME              : AMSAH02
050400* CHANGED AH02 CLCHG-TYPE ADDED 88 GP4HM1     01/10/08     B16712
050500* ADDED AH02-PTP-TYPE              GP4DAC     07/18/07     B14210
050600*       AH02-PTP-GROUP             GP4DAC     07/18/07     B14210
050700*       AH02-FREQUENCY-CODE        GP4DAC     07/18/07     B14210
050800*       AH02-DATE-FINAL-PTP-DUE    GP4DAC     07/18/07     B14210
050900*       AH02-NUM-TOTAL-RECURRENCES GP4DAC     07/18/07     B14210
051000*       AH02-NUM-RECURRENCE        GP4DAC     07/18/07     B14210
051100*       AH02-AMT-PTP-AGGREGATE     GP4DAC     07/18/07     B14210
051200* ADDED HIST                       GP4LH5     08/31/06     B01779
051300* ADDED REAGE & UPAGE REDEFS:      GP4M1S  /  10/22/04  /  A52556
051400* GENERATION DATE AND TIME  : 02/25/97 17:04
051500* GENERATED BY USER/PROJECT : DB2ADM12 / IAS AMCAM0BR
051600**************************************************************
051700     05 AH02-ACT-HIST-DETAIL-INFO.
051800        10 AH02-SEGMENT-LENGTH                  PIC S9(4) COMP.
051900        10 AH02-ACT-HIST-DETAIL-KEY.
052000           15 AH02-HISTORY-TYPE                 PIC 9(2).
052100              88 AH02-CREDIT-LIMIT-CHANGES        VALUE 00.
052200              88 AH02-PROMISE-TO-PAYS             VALUE 01.
052300              88 AH02-DISPUTES                    VALUE 02.
052400              88 AH02-CHG-OFF-RIST-HIST           VALUE 03.
052500              88 AH02-AGENCY-HIST                 VALUE 04.
052600              88 AH02-REAGE-HISTORY               VALUE 05.
052700              88 AH02-ADJ-PAST-DUE-HIST           VALUE 06.
052800              88 AH02-HIST-BACK-UP                VALUE 08.
052900           15 AH02-CDATE-HISTORY                PIC S9(7) COMP-3.
053000           15 AH02-CTIME-HISTORY                PIC S9(7) COMP-3.
053100
053200        10 AH02-ACT-HIST-DETAIL-DATA            PIC X(268).
053300
053400        10 AH02-PROMISE-TO-PAYS-DATA REDEFINES
053500            AH02-ACT-HIST-DETAIL-DATA.
053600           15 AH02-OPERATOR-ID                  PIC X(6).
053700           15 AH02-PTP-STATUS                   PIC X(1).
053800              88 AH02-PTP-BROKEN                  VALUE 'B'.
053900              88 AH02-PTP-KEPT                    VALUE 'K'.
054000              88 AH02-PTP-PARTIAL                 VALUE 'P'.
054100              88 AH02-PTP-DELETE                  VALUE 'D'.
054200           15 FILLER                            PIC X(100).
054300           15 AH02-PAYMENT-AMT                  PIC S9(11)V9(2)
054400               COMP-3.
054500           15 AH02-AMT-PTP                      PIC S9(11)V9(2)
054600               COMP-3.
054700           15 AH02-DATE-PTP-TAKEN               PIC S9(7) COMP-3.
054800           15 AH02-DATE-PTP-DUE                 PIC S9(7) COMP-3.
054900           15 AH02-PTP-TYPE                     PIC X(01).
055000           15 AH02-PTP-GROUP                    PIC 9(02).
055100           15 AH02-FREQUENCY-CODE               PIC X(01).
055200           15 AH02-DATE-FINAL-PTP-DUE           PIC S9(7) COMP-3.
055300           15 AH02-NUM-TOTAL-RECURRENCES        PIC 9(02).
055400           15 AH02-NUM-RECURRENCE               PIC 9(02).
055500           15 AH02-AMT-PTP-AGGREGATE            PIC S9(11)V99
055600                                                    COMP-3.
055700           15 FILLER                            PIC X(120).
055710    05 FILLER                           PIC X(54)   .                     
055800
055900 01 AH02-DISPUTES-REC             .
056000*****************************************************************
056100***  master file common fields
056200*****************************************************************
056300
056400    05 MAST-ACCOUNT-ID          PIC X(12).
056500    05 MAST-REC-TYPE            PIC X(4).
056600******************* DDS COPYLIB GENERATION ******************
056700* SEGMENT NAME              : AMSAH02
056800* CHANGED AH02 CLCHG-TYPE ADDED 88 GP4HM1     01/10/08     B16712
056900* ADDED AH02-PTP-TYPE              GP4DAC     07/18/07     B14210
057000*       AH02-PTP-GROUP             GP4DAC     07/18/07     B14210
057100*       AH02-FREQUENCY-CODE        GP4DAC     07/18/07     B14210
057200*       AH02-DATE-FINAL-PTP-DUE    GP4DAC     07/18/07     B14210
057300*       AH02-NUM-TOTAL-RECURRENCES GP4DAC     07/18/07     B14210
057400*       AH02-NUM-RECURRENCE        GP4DAC     07/18/07     B14210
057500*       AH02-AMT-PTP-AGGREGATE     GP4DAC     07/18/07     B14210
057600* ADDED HIST                       GP4LH5     08/31/06     B01779
057700* ADDED REAGE & UPAGE REDEFS:      GP4M1S  /  10/22/04  /  A52556
057800* GENERATION DATE AND TIME  : 02/25/97 17:04
057900* GENERATED BY USER/PROJECT : DB2ADM12 / IAS AMCAM0BR
058000**************************************************************
058100     05 AH02-ACT-HIST-DETAIL-INFO.
058200        10 AH02-SEGMENT-LENGTH                  PIC S9(4) COMP.
058300        10 AH02-ACT-HIST-DETAIL-KEY.
058400           15 AH02-HISTORY-TYPE                 PIC 9(2).
058500              88 AH02-CREDIT-LIMIT-CHANGES        VALUE 00.
058600              88 AH02-PROMISE-TO-PAYS             VALUE 01.
058700              88 AH02-DISPUTES                    VALUE 02.
058800              88 AH02-CHG-OFF-RIST-HIST           VALUE 03.
058900              88 AH02-AGENCY-HIST                 VALUE 04.
059000              88 AH02-REAGE-HISTORY               VALUE 05.
059100              88 AH02-ADJ-PAST-DUE-HIST           VALUE 06.
059200              88 AH02-HIST-BACK-UP                VALUE 08.
059300           15 AH02-CDATE-HISTORY                PIC S9(7) COMP-3.
059400           15 AH02-CTIME-HISTORY                PIC S9(7) COMP-3.
059500
059600        10 AH02-ACT-HIST-DETAIL-DATA            PIC X(268).
059700
059800        10 AH02-DISPUTES-DATA REDEFINES AH02-ACT-HIST-DETAIL-DATA.
059900           15 AH02-DISPUTE-CASE-TRACKING-NUM    PIC 9(6).
060000           15 AH02-DISPUTE-TYPE                 PIC X(3).
060100           15 AH02-FCHG-BYP-IND                 PIC X(1).
060200              88 AH02-FCHG-BYP-YES                VALUE 'Y'.
060300              88 AH02-FCHG-BYP-NO                 VALUE 'N'.
060400           15 AH02-DISPUTE-OPERATOR             PIC X(6).
060500           15 AH02-OPER-LAST-WORKED             PIC X(6).
060600           15 AH02-ACCOUNT-NUMBER               PIC X(19).
060700           15 AH02-REFERENCE-NUMBER             PIC X(23).
060800           15 AH02-TRANSACTION-DESCRIPTION      PIC X(41).
060900           15 AH02-MERCHANT-DESCRIPTION REDEFINES
061000               AH02-TRANSACTION-DESCRIPTION.
061100              20 AH02-MERCHANT-DBA-NAME         PIC X(25).
061200              20 AH02-MERCHANT-DBA-CITY         PIC X(13).
061300              20 AH02-MERCHANT-DBA-STATE        PIC X(3).
061400           15 AH02-MERCHANT-DBA-COUNTRY         PIC X(3).
061500           15 AH02-CREDIT-ISSUED                PIC X(1).
061600              88 AH02-CREDIT-ISSUED-YES           VALUE 'Y'.
061700              88 AH02-CREDIT-ISSUED-NO            VALUE 'N'.
061800           15 AH02-DEBIT-CREDIT-FLAG            PIC X(1).
061900              88 AH02-DEBIT-ITEM                  VALUE 'D'.
062000              88 AH02-CREDIT-ITEM                 VALUE 'C'.
062100           15 AH02-RESOLVE-STATUS               PIC X(1).
062200              88 AH02-DISPUTE-NOT-RESOLVED        VALUE SPACE.
062300              88 AH02-RESOLVED-CUST-FAVOR         VALUE 'C'.
062400              88 AH02-RESOLVED-ISSUER-FAVOR       VALUE 'I'.
062500           15 AH02-RESOLVE-ACTIONS              PIC X(4).
062600           15 AH02-DECISION-BY-OPERATOR         PIC X(6).
062700           15 AH02-CORRESP-ID                   PIC X(6).
062800           15 AH02-ACTION-CODE                  PIC X(2).
062900           15 AH02-RESULT-CODE                  PIC X(2).
063000           15 AH02-TRNS-ACCT-FUNC               PIC X(3).
063100           15 AH02-DISP-ENTIRE-BALANCE-FLAG     PIC X(1).
063200              88 AH02-DISP-ENTIRE-BALANCE-YES     VALUE 'Y'.
063300           15 AH02-PS2000-TRAN-ID               PIC X(15).
063400           15 AH02-BANKNET-REF-NUMBER REDEFINES
063500               AH02-PS2000-TRAN-ID              PIC X(9).
063600           15 AH02-MERCHANT-CATEGORY-CODE       PIC 9(4).
063700           15 AH02-TRANSACTION-CODE             PIC 9(4).
063800           15 AH02-TRANSACTION-CATEGORY         PIC 9(4).
063900           15 AH02-DISPUTE-TEXT                 PIC X(30).
064000           15 AH02-BKDATE-EFFECT-DATE           PIC S9(7) COMP-3.
064100           15 AH02-DATE-DISPUTE                 PIC S9(7) COMP-3.
064200           15 AH02-AMT-DISPUTE                  PIC S9(11)V9(2)
064300               COMP-3.
064400           15 AH02-DATE-MAIL-STAMP              PIC S9(7) COMP-3.
064500           15 AH02-DATE-DISPUTE-SETTLED         PIC S9(7) COMP-3.
064600           15 AH02-DISPUTE-TRANSACTION-KEY.
064700              20 AH02-CDATE-STMT-BEGIN          PIC S9(7) COMP-3.
064800              20 AH02-DATE-POST                 PIC S9(7) COMP-3.
064900              20 AH02-TIME-POST                 PIC S9(7) COMP-3.
065000           15 AH02-DATE-LAST-WORKED             PIC S9(7) COMP-3.
065100           15 AH02-DATE-TRANSACTION             PIC S9(7) COMP-3.
065200           15 AH02-AMT-TRANSACTION              PIC S9(11)V9(2)
065300               COMP-3.
065400           15 AH02-CENTRAL-PROC-DATE            PIC S9(7) COMP-3.
065500           15 FILLER                            PIC X(22).
065510    05 FILLER                           PIC X(54)   .                     
065600
065700 01 AH02-CHG-OFF                  .
065800*****************************************************************
065900***  master file common fields
066000*****************************************************************
066100
066200    05 MAST-ACCOUNT-ID          PIC X(12).
066300    05 MAST-REC-TYPE            PIC X(4).
066400******************* DDS COPYLIB GENERATION ******************
066500* SEGMENT NAME              : AMSAH02
066600* CHANGED AH02 CLCHG-TYPE ADDED 88 GP4HM1     01/10/08     B16712
066700* ADDED AH02-PTP-TYPE              GP4DAC     07/18/07     B14210
066800*       AH02-PTP-GROUP             GP4DAC     07/18/07     B14210
066900*       AH02-FREQUENCY-CODE        GP4DAC     07/18/07     B14210
067000*       AH02-DATE-FINAL-PTP-DUE    GP4DAC     07/18/07     B14210
067100*       AH02-NUM-TOTAL-RECURRENCES GP4DAC     07/18/07     B14210
067200*       AH02-NUM-RECURRENCE        GP4DAC     07/18/07     B14210
067300*       AH02-AMT-PTP-AGGREGATE     GP4DAC     07/18/07     B14210
067400* ADDED HIST                       GP4LH5     08/31/06     B01779
067500* ADDED REAGE & UPAGE REDEFS:      GP4M1S  /  10/22/04  /  A52556
067600* GENERATION DATE AND TIME  : 02/25/97 17:04
067700* GENERATED BY USER/PROJECT : DB2ADM12 / IAS AMCAM0BR
067800**************************************************************
067900     05 AH02-ACT-HIST-DETAIL-INFO.
068000        10 AH02-SEGMENT-LENGTH                  PIC S9(4) COMP.
068100        10 AH02-ACT-HIST-DETAIL-KEY.
068200           15 AH02-HISTORY-TYPE                 PIC 9(2).
068300              88 AH02-CREDIT-LIMIT-CHANGES        VALUE 00.
068400              88 AH02-PROMISE-TO-PAYS             VALUE 01.
068500              88 AH02-DISPUTES                    VALUE 02.
068600              88 AH02-CHG-OFF-RIST-HIST           VALUE 03.
068700              88 AH02-AGENCY-HIST                 VALUE 04.
068800              88 AH02-REAGE-HISTORY               VALUE 05.
068900              88 AH02-ADJ-PAST-DUE-HIST           VALUE 06.
069000              88 AH02-HIST-BACK-UP                VALUE 08.
069100           15 AH02-CDATE-HISTORY                PIC S9(7) COMP-3.
069200           15 AH02-CTIME-HISTORY                PIC S9(7) COMP-3.
069300
069400        10 AH02-ACT-HIST-DETAIL-DATA            PIC X(268).
069500
069600        10 AH02-CHG-OFF-RIST-HIST-DATA REDEFINES
069700            AH02-ACT-HIST-DETAIL-DATA.
069800           15 AH02-BALANCE-PRINCIPAL            PIC S9(11)V9(2)
069900               COMP-3.
070000           15 AH02-BALANCE-CHARGEOFF            PIC S9(11)V9(2)
070100               COMP-3.
070200           15 AH02-BAL-RIST                     PIC S9(11)V9(2)
070300               COMP-3.
070310    05 FILLER                           PIC X(54)   .                     
070400
070500 01 AH02-COLL-AGENCY              .
070600*****************************************************************
070700***  master file common fields
070800*****************************************************************
070900
071000    05 MAST-ACCOUNT-ID          PIC X(12).
071100    05 MAST-REC-TYPE            PIC X(4).
071200******************* DDS COPYLIB GENERATION ******************
071300* SEGMENT NAME              : AMSAH02
071400* CHANGED AH02 CLCHG-TYPE ADDED 88 GP4HM1     01/10/08     B16712
071500* ADDED AH02-PTP-TYPE              GP4DAC     07/18/07     B14210
071600*       AH02-PTP-GROUP             GP4DAC     07/18/07     B14210
071700*       AH02-FREQUENCY-CODE        GP4DAC     07/18/07     B14210
071800*       AH02-DATE-FINAL-PTP-DUE    GP4DAC     07/18/07     B14210
071900*       AH02-NUM-TOTAL-RECURRENCES GP4DAC     07/18/07     B14210
072000*       AH02-NUM-RECURRENCE        GP4DAC     07/18/07     B14210
072100*       AH02-AMT-PTP-AGGREGATE     GP4DAC     07/18/07     B14210
072200* ADDED HIST                       GP4LH5     08/31/06     B01779
072300* ADDED REAGE & UPAGE REDEFS:      GP4M1S  /  10/22/04  /  A52556
072400* GENERATION DATE AND TIME  : 02/25/97 17:04
072500* GENERATED BY USER/PROJECT : DB2ADM12 / IAS AMCAM0BR
072600**************************************************************
072700     05 AH02-ACT-HIST-DETAIL-INFO.
072800        10 AH02-SEGMENT-LENGTH                  PIC S9(4) COMP.
072900        10 AH02-ACT-HIST-DETAIL-KEY.
073000           15 AH02-HISTORY-TYPE                 PIC 9(2).
073100              88 AH02-CREDIT-LIMIT-CHANGES        VALUE 00.
073200              88 AH02-PROMISE-TO-PAYS             VALUE 01.
073300              88 AH02-DISPUTES                    VALUE 02.
073400              88 AH02-CHG-OFF-RIST-HIST           VALUE 03.
073500              88 AH02-AGENCY-HIST                 VALUE 04.
073600              88 AH02-REAGE-HISTORY               VALUE 05.
073700              88 AH02-ADJ-PAST-DUE-HIST           VALUE 06.
073800              88 AH02-HIST-BACK-UP                VALUE 08.
073900           15 AH02-CDATE-HISTORY                PIC S9(7) COMP-3.
074000           15 AH02-CTIME-HISTORY                PIC S9(7) COMP-3.
074100
074200        10 AH02-ACT-HIST-DETAIL-DATA            PIC X(268).
074300
074400        10 AH02-COLL-AGENCY-HIST-DATA REDEFINES
074500            AH02-ACT-HIST-DETAIL-DATA.
074600           15 AH02-AGENCY-CODE                  PIC X(06).
074700           15 AH02-AGENCY-ASSIGN-DATA.
074800              20  AH02-AGCY-ASSIGN-PAST-DUE     PIC X(02).
074900              20  AH02-AGCY-ASSIGN-METHOD       PIC X(01).
075000                  88  ASSIGN-MANUAL             VALUE 'M'.
075100                  88  ASSIGN-AUTO               VALUE 'A'.
075200           15 AH02-AGENCY-RECALL-DATA.
075300              20  AH02-RECALL-DATE              PIC S9(07)
075400                 COMP-3.
075500              20  AH02-RECALL-PAST-DUE          PIC X(02).
075600              20  AH02-AGCY-RECALL-METHOD       PIC X(01).
075700                  88 RECALL-MANUAL              VALUE 'M'.
075800                  88 RECALL-AUTO                VALUE 'A'.
075900           15  FILLER                           PIC X(252).
075910    05 FILLER                           PIC X(54)   .                     
076000
076100 01 AH02-REAGE                    .
076200*****************************************************************
076300***  master file common fields
076400*****************************************************************
076500
076600    05 MAST-ACCOUNT-ID          PIC X(12).
076700    05 MAST-REC-TYPE            PIC X(4).
076800******************* DDS COPYLIB GENERATION ******************
076900* SEGMENT NAME              : AMSAH02
077000* CHANGED AH02 CLCHG-TYPE ADDED 88 GP4HM1     01/10/08     B16712
077100* ADDED AH02-PTP-TYPE              GP4DAC     07/18/07     B14210
077200*       AH02-PTP-GROUP             GP4DAC     07/18/07     B14210
077300*       AH02-FREQUENCY-CODE        GP4DAC     07/18/07     B14210
077400*       AH02-DATE-FINAL-PTP-DUE    GP4DAC     07/18/07     B14210
077500*       AH02-NUM-TOTAL-RECURRENCES GP4DAC     07/18/07     B14210
077600*       AH02-NUM-RECURRENCE        GP4DAC     07/18/07     B14210
077700*       AH02-AMT-PTP-AGGREGATE     GP4DAC     07/18/07     B14210
077800* ADDED HIST                       GP4LH5     08/31/06     B01779
077900* ADDED REAGE & UPAGE REDEFS:      GP4M1S  /  10/22/04  /  A52556
078000* GENERATION DATE AND TIME  : 02/25/97 17:04
078100* GENERATED BY USER/PROJECT : DB2ADM12 / IAS AMCAM0BR
078200**************************************************************
078300     05 AH02-ACT-HIST-DETAIL-INFO.
078400        10 AH02-SEGMENT-LENGTH                  PIC S9(4) COMP.
078500        10 AH02-ACT-HIST-DETAIL-KEY.
078600           15 AH02-HISTORY-TYPE                 PIC 9(2).
078700              88 AH02-CREDIT-LIMIT-CHANGES        VALUE 00.
078800              88 AH02-PROMISE-TO-PAYS             VALUE 01.
078900              88 AH02-DISPUTES                    VALUE 02.
079000              88 AH02-CHG-OFF-RIST-HIST           VALUE 03.
079100              88 AH02-AGENCY-HIST                 VALUE 04.
079200              88 AH02-REAGE-HISTORY               VALUE 05.
079300              88 AH02-ADJ-PAST-DUE-HIST           VALUE 06.
079400              88 AH02-HIST-BACK-UP                VALUE 08.
079500           15 AH02-CDATE-HISTORY                PIC S9(7) COMP-3.
079600           15 AH02-CTIME-HISTORY                PIC S9(7) COMP-3.
079700
079800        10 AH02-ACT-HIST-DETAIL-DATA            PIC X(268).
079900
080000        10 AH02-REAGE-HISTORY-DATA    REDEFINES
080100            AH02-ACT-HIST-DETAIL-DATA.
080200           15  AH02-REAGE-PAYMENT-COUNTER-IND   PIC X(01).
080300           15  AH02-REAGE-CONS-PAY-COUNTER      PIC S9(3) COMP-3.
080400           15  AH02-PREV-REAGE-DATE             PIC S9(7) COMP-3.
080500           15  AH02-WORKOUT-PREV-REAGE-DATE     PIC S9(7) COMP-3.
080600           15  AH02-DATE-MAN-PASTDUE-REAGE      PIC S9(7) COMP-3.
080700           15  AH02-DATE-AUTO-PASTDUE-REAGE     PIC S9(7) COMP-3.
080800           15  AH02-CUST-SERV-PREV-REAGE-DATE   PIC S9(7) COMP-3.
080900           15  AH02-REAGE-CONSECUTIVE-PYMT  OCCURS 12 TIMES
081000                                                PIC X(1).
081100           15  AH02-WORKOUT-PREV2-REAGE-DATE    PIC S9(7) COMP-3.
081200           15  AH02-NUM-AUTO-PD-REAGE-LTD       PIC S9(3) COMP-3.
081300           15  FILLER                           PIC X(227).
081310    05 FILLER                           PIC X(54)   .                     
081400
081500 01 AH02-ADJ-PASTDUE              .
081600*****************************************************************
081700***  master file common fields
081800*****************************************************************
081900
082000    05 MAST-ACCOUNT-ID          PIC X(12).
082100    05 MAST-REC-TYPE            PIC X(4).
082200******************* DDS COPYLIB GENERATION ******************
082300* SEGMENT NAME              : AMSAH02
082400* CHANGED AH02 CLCHG-TYPE ADDED 88 GP4HM1     01/10/08     B16712
082500* ADDED AH02-PTP-TYPE              GP4DAC     07/18/07     B14210
082600*       AH02-PTP-GROUP             GP4DAC     07/18/07     B14210
082700*       AH02-FREQUENCY-CODE        GP4DAC     07/18/07     B14210
082800*       AH02-DATE-FINAL-PTP-DUE    GP4DAC     07/18/07     B14210
082900*       AH02-NUM-TOTAL-RECURRENCES GP4DAC     07/18/07     B14210
083000*       AH02-NUM-RECURRENCE        GP4DAC     07/18/07     B14210
083100*       AH02-AMT-PTP-AGGREGATE     GP4DAC     07/18/07     B14210
083200* ADDED HIST                       GP4LH5     08/31/06     B01779
083300* ADDED REAGE & UPAGE REDEFS:      GP4M1S  /  10/22/04  /  A52556
083400* GENERATION DATE AND TIME  : 02/25/97 17:04
083500* GENERATED BY USER/PROJECT : DB2ADM12 / IAS AMCAM0BR
083600**************************************************************
083700     05 AH02-ACT-HIST-DETAIL-INFO.
083800        10 AH02-SEGMENT-LENGTH                  PIC S9(4) COMP.
083900        10 AH02-ACT-HIST-DETAIL-KEY.
084000           15 AH02-HISTORY-TYPE                 PIC 9(2).
084100              88 AH02-CREDIT-LIMIT-CHANGES        VALUE 00.
084200              88 AH02-PROMISE-TO-PAYS             VALUE 01.
084300              88 AH02-DISPUTES                    VALUE 02.
084400              88 AH02-CHG-OFF-RIST-HIST           VALUE 03.
084500              88 AH02-AGENCY-HIST                 VALUE 04.
084600              88 AH02-REAGE-HISTORY               VALUE 05.
084700              88 AH02-ADJ-PAST-DUE-HIST           VALUE 06.
084800              88 AH02-HIST-BACK-UP                VALUE 08.
084900           15 AH02-CDATE-HISTORY                PIC S9(7) COMP-3.
085000           15 AH02-CTIME-HISTORY                PIC S9(7) COMP-3.
085100
085200        10 AH02-ACT-HIST-DETAIL-DATA            PIC X(268).
085300
085400        10 AH02-ADJ-PAST-DUE-HISTORY  REDEFINES
085500            AH02-ACT-HIST-DETAIL-DATA.
085600           15  AH02-ADJ-CALC-MIN-PAY-DUE        PIC S9(11)V9(2)
085700                                                          COMP-3.
085800           15  AH02-ADJ-AMT-PAST-DUE  OCCURS 12 TIMES
085900                                                PIC S9(11)V9(2)
086000                                                          COMP-3.
086100           15  AH02-ADJ-NET-PAYMENT-AMT         PIC S9(11)V9(2)
086200                                                          COMP-3.
086300           15  AH02-NSF-UPAGE-COUNTER           PIC 9(1).
086400           15  AH02-ADJ-OVERLIMIT-AMOUNT        PIC S9(11)V9(2)
086500                                                          COMP-3.
086600           15  AH02-ADJ-BALANCE-AT-CYCLE        PIC S9(11)V9(2)
086700                                                          COMP-3.
086800           15  AH02-ADJ-DATE-LAST-HIST-UPD      PIC S9(7) COMP-3.
086900           15  FILLER                           PIC X(151).
086910    05 FILLER                           PIC X(54)   .                     
087000
087100 01 AH02-HIST-BKP                 .
087200*****************************************************************
087300***  master file common fields
087400*****************************************************************
087500
087600    05 MAST-ACCOUNT-ID          PIC X(12).
087700    05 MAST-REC-TYPE            PIC X(4).
087800******************* DDS COPYLIB GENERATION ******************
087900* SEGMENT NAME              : AMSAH02
088000* CHANGED AH02 CLCHG-TYPE ADDED 88 GP4HM1     01/10/08     B16712
088100* ADDED AH02-PTP-TYPE              GP4DAC     07/18/07     B14210
088200*       AH02-PTP-GROUP             GP4DAC     07/18/07     B14210
088300*       AH02-FREQUENCY-CODE        GP4DAC     07/18/07     B14210
088400*       AH02-DATE-FINAL-PTP-DUE    GP4DAC     07/18/07     B14210
088500*       AH02-NUM-TOTAL-RECURRENCES GP4DAC     07/18/07     B14210
088600*       AH02-NUM-RECURRENCE        GP4DAC     07/18/07     B14210
088700*       AH02-AMT-PTP-AGGREGATE     GP4DAC     07/18/07     B14210
088800* ADDED HIST                       GP4LH5     08/31/06     B01779
088900* ADDED REAGE & UPAGE REDEFS:      GP4M1S  /  10/22/04  /  A52556
089000* GENERATION DATE AND TIME  : 02/25/97 17:04
089100* GENERATED BY USER/PROJECT : DB2ADM12 / IAS AMCAM0BR
089200**************************************************************
089300     05 AH02-ACT-HIST-DETAIL-INFO.
089400        10 AH02-SEGMENT-LENGTH                  PIC S9(4) COMP.
089500        10 AH02-ACT-HIST-DETAIL-KEY.
089600           15 AH02-HISTORY-TYPE                 PIC 9(2).
089700              88 AH02-CREDIT-LIMIT-CHANGES        VALUE 00.
089800              88 AH02-PROMISE-TO-PAYS             VALUE 01.
089900              88 AH02-DISPUTES                    VALUE 02.
090000              88 AH02-CHG-OFF-RIST-HIST           VALUE 03.
090100              88 AH02-AGENCY-HIST                 VALUE 04.
090200              88 AH02-REAGE-HISTORY               VALUE 05.
090300              88 AH02-ADJ-PAST-DUE-HIST           VALUE 06.
090400              88 AH02-HIST-BACK-UP                VALUE 08.
090500           15 AH02-CDATE-HISTORY                PIC S9(7) COMP-3.
090600           15 AH02-CTIME-HISTORY                PIC S9(7) COMP-3.
090700
090800        10 AH02-ACT-HIST-DETAIL-DATA            PIC X(268).
090900
091000        10 AH02-HIST-BACK-UP-DATA REDEFINES
091100            AH02-ACT-HIST-DETAIL-DATA.
091200
091300           15 AH02-HIST-BACK-UP-DATA-01 OCCURS 7 TIMES.
091400
091500             20 AH02-HIST-INS-STATUS             PIC X(1).
091600             20 AH02-HIST-INS-RATE-ACTIVE-FLAG   PIC X(1).
091700             20 AH02-HIST-INS-SEG-TYPE           PIC X(1).
091800             20 AH02-HIST-INS-LIAB-AMT           PIC S9(11)V9(2)
091900                                                   COMP-3.
092000             20 AH02-HIST-INS-DATE-ENROLLED      PIC S9(7)
092100                                                   COMP-3.
092200             20 AH02-HIST-INS-OPTION-SET         PIC 9(3).
092300             20 AH02-HIST-INS-DATE-LAST-PRE      PIC S9(7)
092400                                                   COMP-3.
092500             20 AH02-HIST-INS-CANCEL-DATE        PIC S9(7)
092600                                                   COMP-3.
092700             20 AH02-HIST-INS-AMT-LAST-PREMIUM   PIC S9(7)V9(2)
092800                                                   COMP-3.
092900             20 AH02-HIST-INS-MTD-RATE-DATA.
093000
093100               25 AH02-HIST-INS-RATE-NUM         PIC 9(2).
093200               25 AH02-HIST-INS-JOINT-SINGLE-ID  PIC X(2).
093300               25 AH02-HIST-INS-PERCENT          PIC S9(2)V9(3)
093400                                                      COMP-3.
093500             20 FILLER                           PIC X(1).
093600
093700           15 FILLER                        PIC X(2).
093710    05 FILLER                           PIC X(54)   .                     
093800
093900 01 AH02-REGZ                     .
094000*****************************************************************
094100***  master file common fields
094200*****************************************************************
094300
094400    05 MAST-ACCOUNT-ID          PIC X(12).
094500    05 MAST-REC-TYPE            PIC X(4).
094600    05 AH02-ACT-HIST-DETAIL-INFO.
094700       10 AH02-SEGMENT-LENGTH                  PIC S9(4) COMP.
094800       10 AH02-ACT-HIST-DETAIL-KEY.
094900          15 AH02-HISTORY-TYPE                 PIC 9(2).
095000             88 AH02-CREDIT-LIMIT-CHANGES        VALUE 00.
095100             88 AH02-PROMISE-TO-PAYS             VALUE 01.
095200             88 AH02-DISPUTES                    VALUE 02.
095300             88 AH02-CHG-OFF-RIST-HIST           VALUE 03.
095400             88 AH02-AGENCY-HIST                 VALUE 04.
095500             88 AH02-REAGE-HISTORY               VALUE 05.
095600             88 AH02-ADJ-PAST-DUE-HIST           VALUE 06.
095700             88 AH02-HIST-BACK-UP                VALUE 08.
095800             88 AH02-REGZ-HIST                   VALUE 09.
095900             88 AH02-PAY-IN-FULL-HIST            VALUE 10.
096000          15 AH02-CDATE-HISTORY                PIC S9(7) COMP-3.
096100          15 AH02-CTIME-HISTORY                PIC S9(7) COMP-3.
096200       10 AH02-ACT-HIST-DETAIL-DATA            PIC X(268).
096300       10 AH02-REGZ-HIST-DATA REDEFINES
096400              AH02-ACT-HIST-DETAIL-DATA.
096500          15 AH02-CYCB-YTD-INTEREST-CHARGED   PIC S9(9)V99
096600                                                  COMP-3.
096700          15 AH02-CYCB-PYTD-INTEREST-CHARGD   PIC S9(9)V99
096800                                                  COMP-3.
096900          15 AH02-CYCB-YTD-FEES-CHARGED       PIC S9(9)V99
097000                                                  COMP-3.
097100          15 AH02-CYCB-PYTD-FEES-CHARGED      PIC S9(9)V99
097200                                                  COMP-3.
097300          15 AH02-CYCB-YTD-MIN-INTEREST       PIC S9(9)V99
097400                                                  COMP-3.
097500          15 AH02-CYCB-PYTD-MIN-INTEREST      PIC S9(9)V99
097600                                                  COMP-3.
097700          15 AH02-CYCB-CTD-INTEREST-CHARGED   PIC S9(9)V99
097800                                                  COMP-3.
097900          15 AH02-CYCB-CTD-FEES               PIC S9(9)V99
098000                                                  COMP-3.
098100          15 AH02-CYCB-CTD-MIN-INTEREST       PIC S9(9)V99
098200                                                  COMP-3.
098300          15 AH02-PP-OPTION-START-DATE        PIC S9(7)
098400                                                  COMP-3.
098500          15 AH02-PP-OPTION-STOP-DATE         PIC S9(7)
098600                                                  COMP-3.
098700          15 AH02-PENALTY-PRICING-OPTION      PIC X(04).
098800          15 AH02-PENALTY-PO-REMOVAL-TYPE     PIC X(01).
098900          15 AH02-PENALTY-PO-ADD-TYPE         PIC X(01).
099000          15 AH02-PPO-NOTIFICATION-DATE       PIC S9(7)
099100                                                  COMP-3.
099200          15 AH02-PPO-NOTIFICATION-TYPE       PIC X(01).
099300          15 AH02-PPO-NOTIFICATION-MSG        PIC X(05).
099400          15 AH02-PENALTY-PO-PENDING-DATE     PIC S9(7).
099500          15 AH02-PPO-PENDING-PP-OPTION       PIC X(04).
099600          15 AH02-PPO-PENDING-PP-ADD-TYPE     PIC X(01).
099700          15 AH02-PENALTY-PO-CYCLE-COUNTER    PIC S9(5)
099800                                                  COMP-3.
099900          15 AH02-PRORATE-STMT-MIN-PAY        PIC S9(9)V99
100000                                                  COMP-3.
100100          15 AH02-PPO-HIGH-PENALTY-RATE       PIC S9(04)V9(9)
100200                                                  COMP-3.
100300          15 AH02-REGZ-HIGH-LATE-FEE          PIC S9(9)V99
100400                                                  COMP-3.
100500          15 AH02-FORCASTED-LATE-FEE-DATE     PIC S9(7)
100600                                                  COMP-3.
100700          15 AH02-STATF-PENALTY-PRICE         PIC X(01).
100800          15 AH02-PROTECT-BALANCE-DATE        PIC S9(07)
100900                                                  COMP-3.
101000          15 AH02-PROTECT-BALANCE-OPTION      PIC X(04).
101100          15 AH02-PB-NOTIFICATION-DATE        PIC S9(7)
101200                                                  COMP-3.
101300          15 AH02-PB-NOTIFICATION-TYPE        PIC X(01).
101400          15 AH02-PB-STMT-MSG                 PIC X(005).
101500          15 AH02-PB-CORR-ID                  PIC X(006).
101600          15 AH02-PP-HIGH-PAST-DUE            PIC X(002).
101700          15 AH02-PP-COMBINED-NOTICE          PIC X(001).
101800          15 AH02-PPO-NOTIFICATION-CORR       PIC X(006).
101900          15 AH02-PROTECTED-BALANCE-AGENT     PIC X(01).
102000          15 AH02-PB-IPPO-DISCLOSURE-GRP      PIC X(08).
102100          15 AH02-DATE-PROTECT-BAL-CIT        PIC S9(7)
102200                                                  COMP-3.
102300          15 AH02-STATF-OVR-LIMIT-OPT-IN      PIC X(1).
102400          15 AH02-CYCB-CTD-INTEREST-ADJ     PIC S9(9)V99
102500                                                  COMP-3.
102600          15 AH02-CYCB-YTD-INTEREST-ADJ     PIC S9(9)V99
102700                                                  COMP-3.
102800          15 AH02-CYCB-PYTD-INTEREST-ADJ    PIC S9(9)V99
102900                                                  COMP-3.
103000          15 AH02-CYCB-CTD-FEES-ADJ         PIC S9(9)V99
103100                                                  COMP-3.
103200          15 AH02-CYCB-YTD-FEES-ADJ         PIC S9(9)V99
103300                                                  COMP-3.
103400          15 AH02-CYCB-PYTD-FEES-ADJ        PIC S9(9)V99
103500                                                  COMP-3.
103600          15 FILLER                           PIC X(068).
103610    05 FILLER                           PIC X(54)   .                     
103700
103800 01 AH02-HIST-TYPE10              .
103900*****************************************************************
104000***  master file common fields
104100*****************************************************************
104200
104300    05 MAST-ACCOUNT-ID          PIC X(12).
104400    05 MAST-REC-TYPE            PIC X(4).
104500    05 AH02-ACT-HIST-DETAIL-INFO.
104600       10 AH02-SEGMENT-LENGTH                  PIC S9(4) COMP.
104700       10 AH02-ACT-HIST-DETAIL-KEY.
104800          15 AH02-HISTORY-TYPE                 PIC 9(2).
104900             88 AH02-CREDIT-LIMIT-CHANGES        VALUE 00.
105000             88 AH02-PROMISE-TO-PAYS             VALUE 01.
105100             88 AH02-DISPUTES                    VALUE 02.
105200             88 AH02-CHG-OFF-RIST-HIST           VALUE 03.
105300             88 AH02-AGENCY-HIST                 VALUE 04.
105400             88 AH02-REAGE-HISTORY               VALUE 05.
105500             88 AH02-ADJ-PAST-DUE-HIST           VALUE 06.
105600             88 AH02-HIST-BACK-UP                VALUE 08.
105700             88 AH02-REGZ-HIST                   VALUE 09.
105800             88 AH02-PAY-IN-FULL-HIST            VALUE 10.
105900          15 AH02-CDATE-HISTORY                PIC S9(7) COMP-3.
106000          15 AH02-CTIME-HISTORY                PIC S9(7) COMP-3.
106100       10 AH02-ACT-HIST-DETAIL-DATA            PIC X(268).
106200       10 AH02-TYPE10-HIST-DETAIL-DATA REDEFINES
106300              AH02-ACT-HIST-DETAIL-DATA.
106400          15 AH02-PROJ-PIF-AMT            PIC S9(11)V9(2)
106500                                                  COMP-3.
106600          15 FILLER                           PIC X(261).
106610    05 FILLER                           PIC X(54)   .                     
106700
106800 01 MAST-TRAILER-REC.
106900    05 TRAILER-ACCOUNT-ID               PIC X(12)     .
107000    05 TRAILER-RECORD-TYPE              PIC X(4)      .
107100    05 TRAILER-CLIENT-NUMBER            PIC X(4)      .
107200    05 TRAILER-TRANS-ID                 PIC X(8)      .
107300    05 TRAILER-CNTS OCCURS 15 TIMES. 
107400      10 TRAILER-REC-TYPE               PIC X(4).
107500      10 TRAILER-REC-CNT                PIC X(9).
107600      10 TRAILER-REC-CNT-X  REDEFINES                
107700         TRAILER-REC-CNT                PIC 9(9).                         
107800    05 FILLER                           PIC X(127)   .                    
107300
