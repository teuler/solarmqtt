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
dat$ = "16-09-2023," +Str$(117.6, 4,3)
Print #2, dat$ +String$(DAY_RECLEN -1 -Len(dat$), REC_FILL_CH) +"\n";
dat$ = "17-09-2023," +Str$(119.7, 4,3)
Print #2, dat$ +String$(DAY_RECLEN -1 -Len(dat$), REC_FILL_CH) +"\n";
dat$ = "18-09-2023," +Str$(122.42, 4,3)
Print #2, dat$ +String$(DAY_RECLEN -1 -Len(dat$), REC_FILL_CH) +"\n";
dat$ = "19-09-2023," +Str$(122.86, 4,3)
Print #2, dat$ +String$(DAY_RECLEN -1 -Len(dat$), REC_FILL_CH) +"\n";
dat$ = "20-09-2023," +Str$(125.44, 4,3)
Print #2, dat$ +String$(DAY_RECLEN -1 -Len(dat$), REC_FILL_CH) +"\n";
dat$ = "21-09-2023," +Str$((26.73 +88.68) *POW_CORR, 4,3)
Print #2, dat$ +String$(DAY_RECLEN -1 -Len(dat$), REC_FILL_CH) +"\n";
Close #2
                                                                      