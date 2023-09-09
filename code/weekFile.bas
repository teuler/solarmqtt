Option Escape

Const DAY_RECLEN  = 24 ' incl. "\n"
Const REC_FILL_CH = "_"
Const FNAME_WEEK$ = "solarweek.dat"
Const POW_CORR    = 1.12

Dim string dat$, d$ = "A:"

On Error Skip 1
Kill FNAME_WEEK$
Drive d$

Open FNAME_WEEK$ For Random As #2
dat$ = "06-09-2023," +Str$(86.352, 4,3)
Print #2, dat$ +String$(DAY_RECLEN -1 -Len(dat$), REC_FILL_CH) +"\n";
dat$ = "07-09-2023," +Str$(89.824, 4,3)
Print #2, dat$ +String$(DAY_RECLEN -1 -Len(dat$), REC_FILL_CH) +"\n";
dat$ = "08-09-2023," +Str$((11.24 +72.15) *POW_CORR, 4,3)
Print #2, dat$ +String$(DAY_RECLEN -1 -Len(dat$), REC_FILL_CH) +"\n";
dat$ = "09-09-2023," +Str$((12.79 +73.78) *POW_CORR, 4,3)
Print #2, dat$ +String$(DAY_RECLEN -1 -Len(dat$), REC_FILL_CH) +"\n";
Close #2
