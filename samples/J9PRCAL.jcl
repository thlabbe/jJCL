//J9PRCAL  JOB UTI00TX0,'PR GRECCO G03-CAL',CLASS=Z,MSGCLASS=X,         JOB15724
//*        RESTART=DEL1602,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=PLURM.MOVE.PRREF.JCL.PROD
//MEMBER   INCLUDE MEMBER=$$PRT
//*----------------------------------------------------------------
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCAL01
//*----------------------------------------------------------------
//DEL0101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STRECCRE DD DSN=&MIG.CAL01.TRECCRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA03ADH  DD DSN=&MIG.CAL01.A03ADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STADHADH DD DSN=&MIG.CAL01.TADHADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STUECADH DD DSN=&MIG.CAL01.TUECADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STUEDADH DD DSN=&MIG.CAL01.TUEDADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STDNDAS  DD DSN=&MIG.CAL01.TDNDAS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TRECCRE
//*----------------------------------------------------------------
//SORT0102 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STRECCRE SORT T-GR-RECCRE-C-GRPT-GEST ASC [25:1—
//* STRECCRE SORT T-GR-RECCRE-C-ADH ASC [26:9—
//* STRECCRE SORT T-GR-RECCRE-C-ORDRE ASC [35:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRECCRE
//SORTIN   DD DISP=SHR,DSN=&CIB.TRECCRE
//SORTOUT  DD DSN=&MIG.CAL01.TRECCRE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(25,1,CH,A,
               26,9,CH,A,
               35,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A03ADH
//*----------------------------------------------------------------
//SORT0103 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA03ADH SORT A03-ADHERENT-C-GRPT-GEST ASC [1:1—
//* SA03ADH SORT A03-ADHERENT-C-ADH ASC [3:9—
//* SA03ADH SORT A03-ADHERENT-C-ORDRE ASC [13:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03ADHER
//SORTIN   DD DISP=SHR,DSN=&MIG.A03ADHER
//SORTOUT  DD DSN=&MIG.CAL01.A03ADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,1,CH,A,
               3,9,CH,A,
               13,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TADHADH
//*----------------------------------------------------------------
//SORT0104 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STADHADH SORT ADHSADH-C-GRPT-GEST ASC [94:1—
//* STADHADH SORT ADHSADH-C-ADH ASC [95:9—
//* STADHADH SORT ADHSADH-C-ORDRE ASC [104:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TADHADH
//SORTIN   DD DISP=SHR,DSN=&CIB.TADHADH
//SORTOUT  DD DSN=&MIG.CAL01.TADHADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(94,1,CH,A,
               95,9,CH,A,
               104,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TUECADH
//*----------------------------------------------------------------
//SORT0105 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STUECADH SORT TABUECAD-C-GRPT-GEST ASC [101:1—
//* STUECADH SORT TABUECAD-C-ADH ASC [102:9—
//* STUECADH SORT TABUECAD-C-ORDRE ASC [111:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TUECADH
//SORTIN   DD DISP=SHR,DSN=&CIB.TUECADH
//SORTOUT  DD DSN=&MIG.CAL01.TUECADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(101,1,CH,A,
               102,9,CH,A,
               111,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TUEDADH
//*----------------------------------------------------------------
//SORT0106 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STUEDADH SORT TABUEDAD-C-GRPT-GEST ASC [101:1—
//* STUEDADH SORT TABUEDAD-C-ADH ASC [102:9—
//* STUEDADH SORT TABUEDAD-C-ORDRE ASC [111:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TUEDADH
//SORTIN   DD DISP=SHR,DSN=&CIB.TUEDADH
//SORTOUT  DD DSN=&MIG.CAL01.TUEDADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(101,1,CH,A,
               102,9,CH,A,
               111,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TDNDAS
//*----------------------------------------------------------------
//SORT0107 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STUEDADH SORT TABUEDAD-C-GRPT-GEST ASC [101:1—
//* STUEDADH SORT TABUEDAD-C-ADH ASC [102:9—
//* STUEDADH SORT TABUEDAD-C-ORDRE ASC [111:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TDNDAS
//SORTIN   DD DISP=SHR,DSN=&CIB.TDNDAS
//SORTOUT  DD DSN=&MIG.CAL01.TDNDAS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(11,1,CH,A,
               12,9,CH,A,
               21,4,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCAL01A DD DSN=&MIG.CAL01.FCAL01A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCAL02A DD DSN=&MIG.CAL01.FCAL02A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCAL01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCAL01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCAL01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRECCRE DD DSN=&MIG.CAL01.TRECCRE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA03ADH  DD DSN=&MIG.CAL01.A03ADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTADHADH DD DSN=&MIG.CAL01.TADHADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTUECADH DD DSN=&MIG.CAL01.TUECADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTUEDADH DD DSN=&MIG.CAL01.TUEDADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL01A DD DSN=&MIG.CAL01.FCAL01A.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL02A DD DSN=&MIG.CAL01.FCAL02A.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCAL01
//*----------------------------------------------------------------
//PPRCAL01 EXEC PGM=PPRCAL01,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCAL01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCAL01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCAL01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STDNDAS  DD DISP=SHR,DSN=&MIG.CAL01.TDNDAS
//STRECCRE DD DISP=SHR,DSN=&MIG.CAL01.TRECCRE
//SA03ADH  DD DISP=SHR,DSN=&MIG.CAL01.A03ADH
//STADHADH DD DISP=SHR,DSN=&MIG.CAL01.TADHADH
//STUECADH DD DISP=SHR,DSN=&MIG.CAL01.TUECADH
//STUEDADH DD DISP=SHR,DSN=&MIG.CAL01.TUEDADH
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCAL01A DD DSN=&MIG.CAL01.FCAL01A,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFCAL02A DD DSN=&MIG.CAL01.FCAL02A,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRECCRE DD DSN=&MIG.CAL01.TRECCRE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA03ADH  DD DSN=&MIG.CAL01.A03ADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTADHADH DD DSN=&MIG.CAL01.TADHADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTUECADH DD DSN=&MIG.CAL01.TUECADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTUEDADH DD DSN=&MIG.CAL01.TUEDADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL01A DD DSN=&MIG.CAL01.FCAL01A.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL02A DD DSN=&MIG.CAL01.FCAL02A.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCAL02
//*----------------------------------------------------------------
//DEL0201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCAL01A DD DSN=&MIG.CAL02.FCAL01A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCAL02A DD DSN=&MIG.CAL02.FCAL02A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01ENTR DD DSN=&MIG.CAL02.A01ENTR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCAL01A
//*----------------------------------------------------------------
//SORT0201 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCAL01A SORT FCAL01A-C-ID-PERS ASC [80:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCAL01A
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL01.FCAL01A
//SORTOUT  DD DSN=&MIG.CAL02.FCAL01A,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(80,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FCAL02A
//*----------------------------------------------------------------
//SORT0202 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCAL02A SORT FCAL02A-C-ID-PERS ASC [164:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCAL02A
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL01.FCAL02A
//SORTOUT  DD DSN=&MIG.CAL02.FCAL02A,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(164,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01ENTR
//*----------------------------------------------------------------
//SORT0203 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01ENTR SORT A01-ENTR-ETAB-C-ID-PERS ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01ENTR
//SORTIN   DD DISP=SHR,DSN=&SRC.A01ENTR
//SORTOUT  DD DSN=&MIG.CAL02.A01ENTR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0202       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCAL01B DD DSN=&MIG.CAL02.FCAL01B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCAL02B DD DSN=&MIG.CAL02.FCAL02B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCAL02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCAL02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCAL02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL01A DD DSN=&MIG.CAL02.FCAL01A.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL02A DD DSN=&MIG.CAL02.FCAL02A.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01ENTR DD DSN=&MIG.CAL02.A01ENTR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL01B DD DSN=&MIG.CAL02.FCAL01B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL02B DD DSN=&MIG.CAL02.FCAL02B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCAL02
//*----------------------------------------------------------------
//PPRCAL02 EXEC PGM=PPRCAL02,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCAL02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCAL02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCAL02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCAL01A DD DISP=SHR,DSN=&MIG.CAL02.FCAL01A
//SFCAL02A DD DISP=SHR,DSN=&MIG.CAL02.FCAL02A
//SA01ENTR DD DISP=SHR,DSN=&MIG.CAL02.A01ENTR
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCAL01B DD DSN=&MIG.CAL02.FCAL01B,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFCAL02B DD DSN=&MIG.CAL02.FCAL02B,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL01A DD DSN=&MIG.CAL02.FCAL01A.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL02A DD DSN=&MIG.CAL02.FCAL02A.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01ENTR DD DSN=&MIG.CAL02.A01ENTR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL01B DD DSN=&MIG.CAL02.FCAL01B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL02B DD DSN=&MIG.CAL02.FCAL02B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCAL03
//*----------------------------------------------------------------
//DEL0301       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCAL01B DD DSN=&MIG.CAL03.FCAL01B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB07RECV DD DSN=&MIG.CAL03.B07RECV,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB07ACTR DD DSN=&MIG.CAL03.B07ACTR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCAL01B
//*----------------------------------------------------------------
//SORT0301 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCAL01B SORT FCAL01B-C-ID-RECOUVR ASC [1:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCAL01B
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL02.FCAL01B
//SORTOUT  DD DSN=&MIG.CAL03.FCAL01B,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B07RECV
//*----------------------------------------------------------------
//SORT0302 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB07RECV SORT B07-RECOUVR-C-ID-RECOUVR ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.B07RECOU
//SORTIN   DD DISP=SHR,DSN=&SRC.B07RECOU
//SORTOUT  DD DSN=&MIG.CAL03.B07RECV,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B07ACTR
//*----------------------------------------------------------------
//SORT0303 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB07ACTR SORT B07-ACTION-REC-C-ID-RECOUVR ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.B07ACTIO
//SORTIN   DD DISP=SHR,DSN=&SRC.B07ACTIO
//SORTOUT  DD DSN=&MIG.CAL03.B07ACTR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0302       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCAL03  DD DSN=&MIG.CAL03.FCAL03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFB07NF  DD DSN=&MIG.CAL03.FB07NF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCAL04  DD DSN=&MIG.CAL03.FCAL04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0303       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCAL03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCAL03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCAL03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL01B DD DSN=&MIG.CAL03.FCAL01B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZB07RECV DD DSN=&MIG.CAL03.B07RECV.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZB07ACTR DD DSN=&MIG.CAL03.B07ACTR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL03  DD DSN=&MIG.CAL03.FCAL03.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL04  DD DSN=&MIG.CAL03.FCAL04.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCAL03
//*----------------------------------------------------------------
//PPRCAL03 EXEC PGM=PPRCAL03,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCAL03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCAL03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCAL03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCAL01B DD DISP=SHR,DSN=&MIG.CAL03.FCAL01B
//SB07RECV DD DISP=SHR,DSN=&MIG.CAL03.B07RECV
//SB07ACTR DD DISP=SHR,DSN=&MIG.CAL03.B07ACTR
//SB07ANF  DD DISP=SHR,DSN=&MIG.CAL03.B07ACTR
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCAL03  DD DSN=&MIG.CAL03.FCAL03,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFB07NF  DD DSN=&MIG.CAL03.FB07NF,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFCAL04  DD DSN=&MIG.CAL03.FCAL04,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL01B DD DSN=&MIG.CAL03.FCAL01B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZB07RECV DD DSN=&MIG.CAL03.B07RECV.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZB07ACTR DD DSN=&MIG.CAL03.B07ACTR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL03  DD DSN=&MIG.CAL03.FCAL03.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL04  DD DSN=&MIG.CAL03.FCAL04.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCAL04
//*----------------------------------------------------------------
//DEL0401       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCAL03  DD DSN=&MIG.CAL04.FCAL03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCAL04  DD DSN=&MIG.CAL04.FCAL04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCAL03
//*----------------------------------------------------------------
//SORT0401 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCAL03 SORT FCAL03-C-GRPT-GEST ASC [13:1—
//* SFCAL03 SORT FCAL03-C-ADH ASC [14:9—
//* SFCAL03 SORT FCAL03-C-ORDRE ASC [23:4—
//* SFCAL03 SORT FCAL03-AN-REF ASC [56:4—
//* SFCAL03 SORT FCAL03-DT-DEB-PRDE-REF ASC [60:10—
//* SFCAL03 SORT FCAL03-DT-FIN-PRDE-REF ASC [70:10—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCAL03
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL03.FCAL03
//SORTOUT  DD DSN=&MIG.CAL04.FCAL03,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(13,1,CH,A,
               14,9,CH,A,
               23,4,CH,A,
               56,4,CH,A,
               60,10,CH,A,
               70,10,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FCAL04
//*----------------------------------------------------------------
//SORT0402 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCAL04 SORT FCAL04-C-GRPT-GEST ASC [1:1—
//* SFCAL04 SORT FCAL04-C-ADH ASC [2:9—
//* SFCAL04 SORT FCAL04-C-ORDRE ASC [11:4—
//* SFCAL04 SORT FCAL04-AN-REF ASC [15:4—
//* SFCAL04 SORT FCAL04-DT-DEB-PRDE-REF ASC [19:10—
//* SFCAL04 SORT FCAL04-DT-FIN-PRDE-REF ASC [29:10—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCAL04
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL03.FCAL04
//SORTOUT  DD DSN=&MIG.CAL04.FCAL04,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,1,CH,A,
               2,9,CH,A,
               11,4,CH,A,
               15,4,CH,A,
               19,10,CH,A,
               29,10,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0402       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCAL05  DD DSN=&MIG.CAL04.FCAL05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0403       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCAL04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCAL04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCAL04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL03  DD DSN=&MIG.CAL04.FCAL03.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL04  DD DSN=&MIG.CAL04.FCAL04.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL05  DD DSN=&MIG.CAL04.FCAL05.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCAL04
//*----------------------------------------------------------------
//PPRCAL04 EXEC PGM=PPRCAL04,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCAL04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCAL04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCAL04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCAL03  DD DISP=SHR,DSN=&MIG.CAL04.FCAL03
//SFCAL04  DD DISP=SHR,DSN=&MIG.CAL04.FCAL04
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCAL05  DD DSN=&MIG.CAL04.FCAL05,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL03  DD DSN=&MIG.CAL04.FCAL03.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL04  DD DSN=&MIG.CAL04.FCAL04.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL05  DD DSN=&MIG.CAL04.FCAL05.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCAL05
//*----------------------------------------------------------------
//DEL0501       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCAL02B DD DSN=&MIG.CAL05.FCAL02B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCAL02B
//*----------------------------------------------------------------
//SORT0501 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCAL02B SORT FCAL02B-C-GRPT-GEST ASC [25:1—
//* SFCAL02B SORT FCAL02B-C-ADH ASC [26:9—
//* SFCAL02B SORT FCAL02B-C-ORDRE ASC [35:4—
//* SFCAL02B SORT FCAL02B-ANPER ASC [160:4—
//* SFCAL02B SORT FCAL02B-COPERREF ASC [157:3—
//* SFCAL02B SORT FCAL02B-DTDEBPER ASC [98:10—
//* SFCAL02B SORT FCAL02B-DTFINPER DESC [108:10—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCAL02B
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL02.FCAL02B
//SORTOUT  DD DSN=&MIG.CAL05.FCAL02B,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(25,1,CH,A,
               26,9,CH,A,
               35,4,CH,A,
               160,4,CH,A,
               157,3,CH,A,
               98,10,CH,A,
               108,10,CH,D)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0502       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCAL02C DD DSN=&MIG.CAL05.FCAL02C,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCAL07A DD DSN=&MIG.CAL05.FCAL07A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0503       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCAL05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCAL05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCAL05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL02B DD DSN=&MIG.CAL05.FCAL02B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL02C DD DSN=&MIG.CAL05.FCAL02C.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL07A DD DSN=&MIG.CAL05.FCAL07A.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCAL05
//*----------------------------------------------------------------
//PPRCAL05 EXEC PGM=PPRCAL05,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCAL05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCAL05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCAL05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCAL02B DD DISP=SHR,DSN=&MIG.CAL05.FCAL02B
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCAL02C DD DSN=&MIG.CAL05.FCAL02C,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFCAL07A DD DSN=&MIG.CAL05.FCAL07A,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL02B DD DSN=&MIG.CAL05.FCAL02B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL02C DD DSN=&MIG.CAL05.FCAL02C.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL07A DD DSN=&MIG.CAL05.FCAL07A.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCAL06
//*----------------------------------------------------------------
//DEL0601       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCAL02C DD DSN=&MIG.CAL06.FCAL02C,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCAL07A DD DSN=&MIG.CAL06.FCAL07A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*GRC-191--SB05OPER DD DSN=&MIG.CAL06.B05OPER,
//*GRC-191--            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCAL02C
//*----------------------------------------------------------------
//SORT0601 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCAL02C SORT FCAL02C-C-GRPT-GEST ASC [25:1—
//* SFCAL02C SORT FCAL02C-C-ADH ASC [26:9—
//* SFCAL02C SORT FCAL02C-C-ORDRE ASC [35:4—
//* SFCAL02C SORT FCAL02C-ANPER ASC [160:4—
//* SFCAL02C SORT FCAL02C-COPERREF ASC [157:3—
//* SFCAL02C SORT FCAL02C-C-TYPE-PER ASC [45:1—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCAL02C
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL05.FCAL02C
//SORTOUT  DD DSN=&MIG.CAL06.FCAL02C,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(25,1,CH,A,
               26,9,CH,A,
               35,4,CH,A,
               160,4,CH,A,
               157,3,CH,A,
               45,1,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FCAL07A
//*----------------------------------------------------------------
//SORT0602 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCAL07A SORT FCAL07A-C-GRPT-GEST ASC [13:1—
//* SFCAL07A SORT FCAL07A-C-ADH ASC [14:9—
//* SFCAL07A SORT FCAL07A-C-ORDRE ASC [23:4—
//* SFCAL07A SORT FCAL07A-ANPER ASC [27:4—
//* SFCAL07A SORT FCAL07A-COPERREF ASC [31:3—
//* SFCAL07A SORT FCAL07A-C-TYPE-PER ASC [34:1—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCAL07A
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL05.FCAL07A
//SORTOUT  DD DSN=&MIG.CAL06.FCAL07A,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(13,1,CH,A,
               14,9,CH,A,
               23,4,CH,A,
               27,4,CH,A,
               31,3,CH,A,
               34,1,CH,A)
/*
//*FNC GRC-191 - AJO le 18/07/2013
//*GRC-191--*----------------------------------------------------------------
//*GRC-191--*TRI DU FICHIER B05OPER
//*GRC-191--*----------------------------------------------------------------
//*GRC-191--SORT0603 EXEC PGM=SORT,COND=(0,NE)
//*GRC-191--* DEBUT CRITERE XGEN
//*GRC-191--* SB05OPER SORT B05-OPER-COMPTE-C-GRPT-GEST ASC [41:1—
//*GRC-191--* SB05OPER SORT B05-OPER-COMPTE-C-ADH ASC [43:9—
//*GRC-191--* SB05OPER SORT B05-OPER-COMPTE-C-ORDRE ASC [53:4—
//*GRC-191--* SB05OPER SORT B05-OPER-COMPTE-C-EXE-AFFECT ASC [87:4—
//*GRC-191--* FIN   CRITERE XGEN
//*GRC-191--SYSOUT   DD SYSOUT=*
//*GRC-191--COPY     DD DISP=SHR,DSN=&CPYSRC.B05OPER
//*GRC-191--SORTIN   DD DISP=SHR,DSN=&SRC.B05OPER
//*GRC-191--SORTOUT  DD DSN=&MIG.CAL06.B05OPER,
//*GRC-191--            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//*GRC-191--            SPACE=(TRK,(5000,500),RLSE)
//*GRC-191--SYSIN    DD *
//*GRC-191--  SORT FIELDS=(41,1,CH,A,
//*GRC-191--               43,9,CH,A,
//*GRC-191--               53,4,CH,A,
//*GRC-191--               87,4,CH,A)
//*GRC-191--/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0602       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCAL02D DD DSN=&MIG.CAL06.FCAL02D,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCAL07B DD DSN=&MIG.CAL06.FCAL07B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0603       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCAL06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCAL06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCAL06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL02C DD DSN=&MIG.CAL06.FCAL02C.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL07A DD DSN=&MIG.CAL06.FCAL07A.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*GRC-191--ZB05OPER DD DSN=&MIG.CAL06.B05OPER.R,
//*GRC-191--            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL02D DD DSN=&MIG.CAL06.FCAL02D.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL07B DD DSN=&MIG.CAL06.FCAL07B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCAL06
//*----------------------------------------------------------------
//PPRCAL06 EXEC PGM=PPRCAL06,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCAL06,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCAL06,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCAL06,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCAL02C DD DISP=SHR,DSN=&MIG.CAL06.FCAL02C
//SFCAL07A DD DISP=SHR,DSN=&MIG.CAL06.FCAL07A
//*GRC-191--SB05OPER DD DISP=SHR,DSN=&MIG.CAL06.B05OPER
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCAL02D DD DSN=&MIG.CAL06.FCAL02D,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFCAL07B DD DSN=&MIG.CAL06.FCAL07B,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL02C DD DSN=&MIG.CAL06.FCAL02C.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL07A DD DSN=&MIG.CAL06.FCAL07A.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*GRC-191--ZB05OPER DD DSN=&MIG.CAL06.B05OPER.R,
//*GRC-191--            DISP=(NEW,CATLG,CATLG),
//*GRC-191--            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//*GRC-191--            SPACE=(TRK,(200,100),RLSE)
//ZFCAL02D DD DSN=&MIG.CAL06.FCAL02D.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL07B DD DSN=&MIG.CAL06.FCAL07B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCAL07
//*----------------------------------------------------------------
//DEL0701       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCAL07B DD DSN=&MIG.CAL07.FCAL07B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCAL07B
//*----------------------------------------------------------------
//SORT0701 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCAL07B SORT FCAL07B-NUM-REGROUP ASC [1:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCAL07B
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL06.FCAL07B
//SORTOUT  DD DSN=&MIG.CAL07.FCAL07B,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0702       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCAL07C DD DSN=&MIG.CAL07.FCAL07C,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0703       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCAL07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCAL07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCAL07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL07B DD DSN=&MIG.CAL07.FCAL07B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL07C DD DSN=&MIG.CAL07.FCAL07C.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCAL07
//*----------------------------------------------------------------
//PPRCAL07 EXEC PGM=PPRCAL07,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCAL07,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCAL07,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCAL07,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCAL07B DD DISP=SHR,DSN=&MIG.CAL07.FCAL07B
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCAL07C DD DSN=&MIG.CAL07.FCAL07C,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL07B DD DSN=&MIG.CAL07.FCAL07B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL07C DD DSN=&MIG.CAL07.FCAL07C.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCAL08
//*----------------------------------------------------------------
//DEL0801       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCAL07C DD DSN=&MIG.CAL08.FCAL07C,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCAL02C DD DSN=&MIG.CAL08.FCAL02C,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCAL07C
//*----------------------------------------------------------------
//SORT0801 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCAL07C SORT FCAL07C-C-GRPT-GEST ASC [13:1—
//* SFCAL07C SORT FCAL07C-C-ADH ASC [14:9—
//* SFCAL07C SORT FCAL07C-C-ORDRE ASC [23:4—
//* SFCAL07C SORT FCAL07C-C-TYPE-PER ASC [34:1—
//* SFCAL07C SORT FCAL07C-COPERREF ASC [31:3—
//* SFCAL07C SORT FCAL07C-ANPER ASC [27:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCAL07C
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL07.FCAL07C
//SORTOUT  DD DSN=&MIG.CAL08.FCAL07C,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(13,1,CH,A,
               14,9,CH,A,
               23,4,CH,A,
               34,1,CH,A,
               31,3,CH,A,
               27,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FCAL02C
//*----------------------------------------------------------------
//SORT0802 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCAL02C SORT FCAL02C-C-GRPT-GEST ASC [25:1—
//* SFCAL02C SORT FCAL02C-C-ADH ASC [26:9—
//* SFCAL02C SORT FCAL02C-C-ORDRE ASC [35:4—
//* SFCAL02C SORT FCAL02C-C-TYPE-PER ASC [45:1—
//* SFCAL02C SORT FCAL02C-COPERREF ASC [157:3—
//* SFCAL02C SORT FCAL02C-ANPER ASC [160:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCAL02C
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL05.FCAL02C
//SORTOUT  DD DSN=&MIG.CAL08.FCAL02C,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(25,1,CH,A,
               26,9,CH,A,
               35,4,CH,A,
               45,1,CH,A,
               157,3,CH,A,
               160,4,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0802       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCAL07D DD DSN=&MIG.CAL08.FCAL07D,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0803       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCAL08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCAL08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCAL08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL07C DD DSN=&MIG.CAL08.FCAL07C.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL02C DD DSN=&MIG.CAL08.FCAL02C.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL07D DD DSN=&MIG.CAL08.FCAL07D.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCAL08
//*----------------------------------------------------------------
//PPRCAL08 EXEC PGM=PPRCAL08,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCAL08,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCAL08,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCAL08,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCAL07C DD DISP=SHR,DSN=&MIG.CAL08.FCAL07C
//SFCAL02C DD DISP=SHR,DSN=&MIG.CAL08.FCAL02C
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCAL07D DD DSN=&MIG.CAL08.FCAL07D,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL07C DD DSN=&MIG.CAL08.FCAL07C.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL02C DD DSN=&MIG.CAL08.FCAL02C.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL07D DD DSN=&MIG.CAL08.FCAL07D.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCAL09
//*----------------------------------------------------------------
//DEL0901       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCAL02D DD DSN=&MIG.CAL09.FCAL02D,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCAL02D
//*----------------------------------------------------------------
//SORT0901 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCAL02D SORT FCAL02D-C-ID-RECOUVR ASC [1:12—
//* SFCAL02D SORT FCAL02D-NUM-REGROUP ASC [217:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCAL02D
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL06.FCAL02D
//SORTOUT  DD DSN=&MIG.CAL09.FCAL02D,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A,
               217,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0902       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCAL08A DD DSN=&MIG.CAL09.FCAL08A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0903       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCAL09,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCAL09,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCAL09,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL02D DD DSN=&MIG.CAL09.FCAL02D.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL08A DD DSN=&MIG.CAL09.FCAL08A.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCAL09
//*----------------------------------------------------------------
//PPRCAL09 EXEC PGM=PPRCAL09,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCAL09,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCAL09,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCAL09,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCAL02D DD DISP=SHR,DSN=&MIG.CAL09.FCAL02D
//SB07REC  DD DISP=SHR,DSN=&MIG.CAL03.B07RECV
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCAL08A DD DSN=&MIG.CAL09.FCAL08A,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL02D DD DSN=&MIG.CAL09.FCAL02D.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL08A DD DSN=&MIG.CAL09.FCAL08A.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCAL10
//*----------------------------------------------------------------
//DEL1001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCAL02D DD DSN=&MIG.CAL10.FCAL02D,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCAL08A DD DSN=&MIG.CAL10.FCAL08A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB07REC  DD DSN=&MIG.CAL10.B07REC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCAL02D
//*----------------------------------------------------------------
//SORT1001 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCAL02D SORT FCAL02D-C-GRPT-GEST ASC [25:1—
//* SFCAL02D SORT FCAL02D-C-ADH ASC [26:9—
//* SFCAL02D SORT FCAL02D-C-ORDRE ASC [35:4—
//* SFCAL02D SORT FCAL02D-C-EXE-AFFECT ASC [39:4—
//* SFCAL02D SORT FCAL02D-C-TYPE-PER ASC [45:1—
//* SFCAL02D SORT FCAL02D-C-PER-AFFECT ASC [43:2—
//* SFCAL02D SORT FCAL02D-C-ORDRE-CTX ASC [46:5—
//* SFCAL02D SORT FCAL02D-NUM-REGROUP ASC [217:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCAL02D
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL06.FCAL02D
//SORTOUT  DD DSN=&MIG.CAL10.FCAL02D,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(25,1,CH,A,
               26,9,CH,A,
               35,4,CH,A,
               39,4,CH,A,
               45,1,CH,A,
               43,2,CH,A,
               46,5,CH,A,
               217,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FCAL08A
//*----------------------------------------------------------------
//SORT1002 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCAL08A SORT FCAL08A-C-GRPT-GEST ASC [1:1—
//* SFCAL08A SORT FCAL08A-C-ADH ASC [2:9—
//* SFCAL08A SORT FCAL08A-C-ORDRE ASC [11:4—
//* SFCAL08A SORT FCAL08A-C-EXE-AFFECT ASC [15:4—
//* SFCAL08A SORT FCAL08A-C-TYPE-PER ASC [19:1—
//* SFCAL08A SORT FCAL08A-C-PER-AFFECT ASC [20:2—
//* SFCAL08A SORT FCAL08A-C-ORDRE-CTX ASC [22:5—
//* SFCAL08A SORT FCAL08A-NUM-REGROUP ASC [39:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCAL08A
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL09.FCAL08A
//SORTOUT  DD DSN=&MIG.CAL10.FCAL08A,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,1,CH,A,
               2,9,CH,A,
               11,4,CH,A,
               15,4,CH,A,
               19,1,CH,A,
               20,2,CH,A,
               22,5,CH,A,
               39,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B07REC
//*----------------------------------------------------------------
//SORT1003 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB07REC SORT B07-RECOUVR-C-GRPT-GEST ASC [15:1—
//* SB07REC SORT B07-RECOUVR-C-ADH ASC [17:9—
//* SB07REC SORT B07-RECOUVR-C-ORDRE ASC [27:4—
//* SB07REC SORT B07-RECOUVR-C-EXE-AFFECT ASC [35:4—
//* SB07REC SORT B07-RECOUVR-C-TYPE-PER ASC [42:1—
//* SB07REC SORT B07-RECOUVR-C-PER-AFFECT ASC [40:2—
//* SB07REC SORT B07-RECOUVR-C-ORDRE-CTX ASC [44:5—
//* SB07REC SORT B07-RECOUVR-C-ID-RECOUVR DESC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.B07RECOU
//SORTIN   DD DISP=SHR,DSN=&SRC.B07RECOU
//SORTOUT  DD DSN=&MIG.CAL10.B07REC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(15,1,CH,A,
               17,9,CH,A,
               27,4,CH,A,
               35,4,CH,A,
               42,1,CH,A,
               40,2,CH,A,
               44,5,CH,A,
               2,12,CH,D)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCAL08B DD DSN=&MIG.CAL10.FCAL08B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCAL10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCAL10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCAL10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL02D DD DSN=&MIG.CAL10.FCAL02D.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL08A DD DSN=&MIG.CAL10.FCAL08A.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZB07REC  DD DSN=&MIG.CAL10.B07REC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL08B DD DSN=&MIG.CAL10.FCAL08B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCAL10
//*----------------------------------------------------------------
//PPRCAL10 EXEC PGM=PPRCAL10,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCAL10,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCAL10,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCAL10,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCAL02D DD DISP=SHR,DSN=&MIG.CAL10.FCAL02D
//SFCAL08A DD DISP=SHR,DSN=&MIG.CAL10.FCAL08A
//SB07REC  DD DISP=SHR,DSN=&MIG.CAL10.B07REC
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCAL08B DD DSN=&MIG.CAL10.FCAL08B,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL02D DD DSN=&MIG.CAL10.FCAL02D.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL08A DD DSN=&MIG.CAL10.FCAL08A.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZB07REC  DD DSN=&MIG.CAL10.B07REC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL08B DD DSN=&MIG.CAL10.FCAL08B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCAL11
//*----------------------------------------------------------------
//DEL1101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCAL08A DD DSN=&MIG.CAL11.FCAL08A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCAL08B DD DSN=&MIG.CAL11.FCAL08B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCAL08A
//*----------------------------------------------------------------
//SORT1101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCAL08A SORT FCAL08A-C-ID-RECOUVR ASC [27:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCAL08A
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL09.FCAL08A
//SORTOUT  DD DSN=&MIG.CAL11.FCAL08A,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(27,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FCAL08B
//*----------------------------------------------------------------
//SORT1102 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCAL08B SORT FCAL08B-C-ID-RECOUVR ASC [27:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCAL08B
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL10.FCAL08B
//SORTOUT  DD DSN=&MIG.CAL11.FCAL08B,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(27,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCAL09A DD DSN=&MIG.CAL11.FCAL09A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCAL11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCAL11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCAL11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL08A DD DSN=&MIG.CAL11.FCAL08A.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL08B DD DSN=&MIG.CAL11.FCAL08B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL09A DD DSN=&MIG.CAL11.FCAL09A.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCAL11
//*----------------------------------------------------------------
//PPRCAL11 EXEC PGM=PPRCAL11,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCAL11,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCAL11,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCAL11,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCAL08A DD DISP=SHR,DSN=&MIG.CAL11.FCAL08A
//SFCAL08B DD DISP=SHR,DSN=&MIG.CAL11.FCAL08B
//SB07ACT  DD DISP=SHR,DSN=&MIG.CAL03.B07ACTR
//SB07REC  DD DISP=SHR,DSN=&MIG.CAL03.B07RECV
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCAL09A DD DSN=&MIG.CAL11.FCAL09A,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL08A DD DSN=&MIG.CAL11.FCAL08A.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL08B DD DSN=&MIG.CAL11.FCAL08B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL09A DD DSN=&MIG.CAL11.FCAL09A.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCAL12
//*----------------------------------------------------------------
//DEL1201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCAL09A DD DSN=&MIG.CAL12.FCAL09A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCAL09A
//*----------------------------------------------------------------
//SORT1201 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCAL09A SORT FCAL09A-NUM-REGROUP ASC [13:12—
//* SFCAL09A SORT FCAL09A-NUM-PRIORITE ASC [25:1—
//* SFCAL09A SORT FCAL09A-D-EFFECTIVE ASC [26:8—
//* SFCAL09A SORT FCAL09A-D-EXIG-PAI ASC [34:8—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCAL09A
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL11.FCAL09A
//SORTOUT  DD DSN=&MIG.CAL12.FCAL09A,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(13,12,CH,A,
               25,1,CH,A,
               26,8,CH,A,
               34,8,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1202       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCAL09B DD DSN=&MIG.CAL12.FCAL09B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCAL12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCAL12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCAL12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL09A DD DSN=&MIG.CAL12.FCAL09A.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL09B DD DSN=&MIG.CAL12.FCAL09B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCAL12
//*----------------------------------------------------------------
//PPRCAL12 EXEC PGM=PPRCAL12,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCAL12,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCAL12,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCAL12,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCAL09A DD DISP=SHR,DSN=&MIG.CAL12.FCAL09A
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCAL09B DD DSN=&MIG.CAL12.FCAL09B,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL09A DD DSN=&MIG.CAL12.FCAL09A.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL09B DD DSN=&MIG.CAL12.FCAL09B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCAL13
//*----------------------------------------------------------------
//DEL1301       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCAL05  DD DSN=&MIG.CAL13.FCAL05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCAL09B DD DSN=&MIG.CAL13.FCAL09B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB07IP   DD DSN=&MIG.CAL13.B07IP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCAL05
//*----------------------------------------------------------------
//SORT1301 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCAL05 SORT FCAL05-C-ID-RECOUVR ASC [1:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCAL05
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL04.FCAL05
//SORTOUT  DD DSN=&MIG.CAL13.FCAL05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FCAL09B
//*----------------------------------------------------------------
//SORT1302 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCAL09B SORT FCAL09B-C-ID-RECOUVR ASC [1:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCAL09B
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL12.FCAL09B
//SORTOUT  DD DSN=&MIG.CAL13.FCAL09B,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B07IP
//*----------------------------------------------------------------
//SORT1304 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB07IP SORT B07-IP-C-ID-RECOUVR ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.B07IP
//SORTIN   DD DISP=SHR,DSN=&SRC.B07IP
//SORTOUT  DD DSN=&MIG.CAL13.B07IP,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1302       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCAL06  DD DSN=&MIG.CAL13.FCAL06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCAL10  DD DSN=&MIG.CAL13.FCAL10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1303       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCAL13,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCAL13,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCAL13,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL05  DD DSN=&MIG.CAL13.FCAL05.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL09B DD DSN=&MIG.CAL13.FCAL09B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZB07IP   DD DSN=&MIG.CAL13.B07IP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL06  DD DSN=&MIG.CAL13.FCAL06.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL10  DD DSN=&MIG.CAL13.FCAL10.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCAL13
//*----------------------------------------------------------------
//PPRCAL13 EXEC PGM=PPRCAL13,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCAL13,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCAL13,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCAL13,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCAL05  DD DISP=SHR,DSN=&MIG.CAL13.FCAL05
//SFCAL09B DD DISP=SHR,DSN=&MIG.CAL13.FCAL09B
//SB07ACT  DD DISP=SHR,DSN=&MIG.CAL03.B07ACTR
//SB07ANF  DD DISP=SHR,DSN=&MIG.CAL03.B07ACTR
//SB07IP   DD DISP=SHR,DSN=&MIG.CAL13.B07IP
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCAL06  DD DSN=&MIG.CAL13.FCAL06,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFCAL10  DD DSN=&MIG.CAL13.FCAL10,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SNFCAL10  DD DSN=&MIG.CAL13.NFCAL10,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL05  DD DSN=&MIG.CAL13.FCAL05.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL09B DD DSN=&MIG.CAL13.FCAL09B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZB07IP   DD DSN=&MIG.CAL13.B07IP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL06  DD DSN=&MIG.CAL13.FCAL06.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL10  DD DSN=&MIG.CAL13.FCAL10.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCAL14
//*----------------------------------------------------------------
//DEL1401       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FTYPACT  DD DSN=&MIG.CAL14.FTYPACT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTYCAL   DD DSN=&MIG.CAL14.FTYCAL,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCAL05  DD DSN=&MIG.CAL14.FCAL05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCAL06  DD DSN=&MIG.CAL14.FCAL06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCAL06  DD DSN=&MIG.CAL14.FB07NF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FTYPACT
//*----------------------------------------------------------------
//SORT1401 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* FTYPACT SORT TYPACT-TYCALREC ASC [1:5—
//* FTYPACT SORT TYPACT-C-TYP-ACT-REC ASC [6:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.TTYPACT
//SORTIN   DD DISP=SHR,DSN=&TAB.TTYPACT
//SORTOUT  DD DSN=&MIG.CAL14.FTYPACT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,5,CH,A,
               6,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FTYCAL
//*----------------------------------------------------------------
//SORT1402 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* FTYCAL SORT TYCAL-TYPOBJ ASC [1:3—
//* FTYCAL SORT TYCAL-TTS ASC [4:1—
//* FTYCAL SORT TYCAL-URCREP ASC [5:1—
//* FTYCAL SORT TYCAL-RJ ASC [6:1—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.TTYCAL
//SORTIN   DD DISP=SHR,DSN=&TAB.TTYCAL
//SORTOUT  DD DSN=&MIG.CAL14.FTYCAL,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,3,CH,A,
               4,1,CH,A,
               5,1,CH,A,
               6,1,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FCAL05
//*----------------------------------------------------------------
//SORT1403 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCAL05 SORT FCAL05-C-GRPT-GEST ASC [13:1—
//* SFCAL05 SORT FCAL05-C-ADH ASC [14:9—
//* SFCAL05 SORT FCAL05-C-ORDRE ASC [23:4—
//* SFCAL05 SORT FCAL05-AN-REF ASC [56:4—
//* SFCAL05 SORT FCAL05-DT-DEB-PRDE-REF ASC [60:10—
//* SFCAL05 SORT FCAL05-DT-FIN-PRDE-REF ASC [70:10—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCAL05
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL04.FCAL05
//SORTOUT  DD DSN=&MIG.CAL14.FCAL05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(13,1,CH,A,
               14,9,CH,A,
               23,4,CH,A,
               56,4,CH,A,
               60,10,CH,A,
               70,10,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FCAL06
//*----------------------------------------------------------------
//SORT1404 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCAL06 SORT FCAL06-C-GRPT-GEST ASC [13:1—
//* SFCAL06 SORT FCAL06-C-ADH ASC [14:9—
//* SFCAL06 SORT FCAL06-C-ORDRE ASC [23:4—
//* SFCAL06 SORT FCAL06-AN-REF ASC [27:4—
//* SFCAL06 SORT FCAL06-DT-DEB-PRDE-REF ASC [31:10—
//* SFCAL06 SORT FCAL06-DT-FIN-PRDE-REF ASC [41:10—
//* SFCAL06 SORT FCAL06-D-PREVUE DESC [54:8—
//* SFCAL06 SORT FCAL06-C-TYPE-ACT-REC DESC [51:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCAL06
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL13.FCAL06
//SORTOUT  DD DSN=&MIG.CAL14.FCAL06,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(13,1,CH,A,
               14,9,CH,A,
               23,4,CH,A,
               27,4,CH,A,
               31,10,CH,A,
               41,10,CH,A,
               54,8,CH,D,
               51,3,CH,D)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FCAL06
//*----------------------------------------------------------------
//SORT1405 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCAL06 SORT FCAL06-C-GRPT-GEST ASC [13:1—
//* SFCAL06 SORT FCAL06-C-ADH ASC [14:9—
//* SFCAL06 SORT FCAL06-C-ORDRE ASC [23:4—
//* SFCAL06 SORT FCAL06-AN-REF ASC [27:4—
//* SFCAL06 SORT FCAL06-DT-DEB-PRDE-REF ASC [31:10—
//* SFCAL06 SORT FCAL06-DT-FIN-PRDE-REF ASC [41:10—
//* SFCAL06 SORT FCAL06-D-PREVUE DESC [54:8—
//* SFCAL06 SORT FCAL06-C-TYPE-ACT-REC DESC [51:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FB07NF
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL03.FB07NF
//SORTOUT  DD DSN=&MIG.CAL14.FB07NF,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(13,1,CH,A,
               14,9,CH,A,
               23,4,CH,A,
               56,4,CH,A,
               60,10,CH,A,
               70,10,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1402       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SCAL     DD DSN=&MIG.CAL14.CAL,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SHCR     DD DSN=&MIG.CAL14.HCR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SARA     DD DSN=&MIG.CAL14.ARA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SHAR     DD DSN=&MIG.CAL14.HAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCOA     DD DSN=&MIG.CAL14.COA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCALCPT  DD DSN=&MIG.CAL14.CALCPT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1403       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCAL14,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCAL14,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCAL14,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFETAACT DD DSN=&MIG.CAL14.FETAACT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFTYPACT DD DSN=&MIG.CAL14.FTYPACT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFTYCAL  DD DSN=&MIG.CAL14.FTYCAL.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFGRINST DD DSN=&MIG.CAL14.FGRINST.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL05  DD DSN=&MIG.CAL14.FCAL05.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL06  DD DSN=&MIG.CAL14.FCAL06.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCAL     DD DSN=&MIG.CAL14.CAL.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZHCR     DD DSN=&MIG.CAL14.HCR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZARA     DD DSN=&MIG.CAL14.ARA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZHAR     DD DSN=&MIG.CAL14.HAR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCOA     DD DSN=&MIG.CAL14.COA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCALCPT  DD DSN=&MIG.CAL14.CALCPT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCAL14
//*----------------------------------------------------------------
//PPRCAL14 EXEC PGM=PPRCAL14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCAL14,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCAL14,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCAL14,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//FETAACT  DD DISP=SHR,DSN=&TAB.TCETACT
//FTYPACT  DD DISP=SHR,DSN=&MIG.CAL14.FTYPACT
//FTYCAL   DD DISP=SHR,DSN=&MIG.CAL14.FTYCAL
//FGRINST  DD DISP=SHR,DSN=&TAB.TGRINST
//SFCAL05  DD DISP=SHR,DSN=&MIG.CAL14.FCAL05
//SFCAL06  DD DISP=SHR,DSN=&MIG.CAL14.FCAL06
//SFB07NF  DD DISP=SHR,DSN=&MIG.CAL14.FB07NF
//*--------------<FICHIERS CIBLES>---------------------------------
//SCAL     DD DSN=&MIG.CAL14.CAL,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SHCR     DD DSN=&MIG.CAL14.HCR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SARA     DD DSN=&MIG.CAL14.ARA,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SHAR     DD DSN=&MIG.CAL14.HAR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SCOA     DD DSN=&MIG.CAL14.COA,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SCALCPT  DD DSN=&MIG.CAL14.CALCPT,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFETAACT DD DSN=&MIG.CAL14.FETAACT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFTYPACT DD DSN=&MIG.CAL14.FTYPACT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFTYCAL  DD DSN=&MIG.CAL14.FTYCAL.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFGRINST DD DSN=&MIG.CAL14.FGRINST.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL05  DD DSN=&MIG.CAL14.FCAL05.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL06  DD DSN=&MIG.CAL14.FCAL06.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCAL     DD DSN=&MIG.CAL14.CAL.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZHCR     DD DSN=&MIG.CAL14.HCR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZARA     DD DSN=&MIG.CAL14.ARA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZHAR     DD DSN=&MIG.CAL14.HAR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCOA     DD DSN=&MIG.CAL14.COA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCALCPT  DD DSN=&MIG.CAL14.CALCPT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCAL19
//*----------------------------------------------------------------
//DEL1901       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SORC     DD DSN=&MIG.CAL19.ORC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STUEDADH DD DSN=&MIG.CAL19.TUEDADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER ORC
//*----------------------------------------------------------------
//SORT1901 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SORC SORT ORC-IDTECADH ASC [16:20—
//* SORC SORT ORC-NOORDADS ASC [36:3—
//* SORC SORT ORC-NOINS    ASC [45:4—
//* SORC SORT ORC-NOORDUEC ASC [39:5—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.ORC
//SORTIN   DD DISP=SHR,DSN=&CIB.ORC
//SORTOUT  DD DSN=&MIG.CAL19.ORC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,20,CH,A,
               36,3,CH,A,
               45,4,CH,A,
               39,5,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TUEDADH
//*----------------------------------------------------------------
//SORT1902 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STUEDADH SORT TABUEDAD-ID-TECH-UR-ADHR ASC [21:20—
//* STUEDADH SORT TABUEDAD-NO-ORD-ADHS ASC [41:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TUEDADH
//SORTIN   DD DISP=SHR,DSN=&CIB.TUEDADH
//SORTOUT  DD DSN=&MIG.CAL19.TUEDADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,20,CH,A,
               41,3,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1902       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SORCANT  DD DSN=&MIG.CAL19.ORCANT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1903       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCAL19,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCAL19,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCAL19,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZORC     DD DSN=&MIG.CAL19.ORC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTUEDADH DD DSN=&MIG.CAL19.TUEDADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZORCANT  DD DSN=&MIG.CAL19.ORCANT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCAL19
//*----------------------------------------------------------------
//PPRCAL19 EXEC PGM=PPRCAL19,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCAL19,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCAL19,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCAL19,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SORC     DD DISP=SHR,DSN=&MIG.CAL19.ORC
//STUEDADH DD DISP=SHR,DSN=&MIG.CAL19.TUEDADH
//*--------------<FICHIERS CIBLES>---------------------------------
//SORCANT  DD DSN=&MIG.CAL19.ORCANT,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZORC     DD DSN=&MIG.CAL19.ORC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTUEDADH DD DSN=&MIG.CAL19.TUEDADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZORCANT  DD DSN=&MIG.CAL19.ORCANT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCAL20
//*----------------------------------------------------------------
//DEL2001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SORCANT  DD DSN=&MIG.CAL20.ORCANT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER ORCANT
//*----------------------------------------------------------------
//SORT2001 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SORCANT SORT ORC-C-GRPT-GEST ASC [179:1—
//* SORCANT SORT ORC-C-ADH ASC [180:9—
//* SORCANT SORT ORC-C-ORDRE ASC [189:4—
//* FIN   CRITERE XGEN
//*GRC-223 > Ajout critère de tri pour prendre top-cal=1 en premier
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.CPY0198
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL19.ORCANT
//SORTOUT  DD DSN=&MIG.CAL20.ORCANT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(179,1,CH,A,
               180,9,CH,A,
               189,4,CH,A,
               198,1,CH,D)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SCAL     DD DSN=&MIG.CAL20.CAL,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SHCR     DD DSN=&MIG.CAL20.HCR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SARA     DD DSN=&MIG.CAL20.ARA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SHAR     DD DSN=&MIG.CAL20.HAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SLCO     DD DSN=&MIG.CAL20.LCO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCALCPTS DD DSN=&MIG.CAL20.CALCPTS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCAL20,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCAL20,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCAL20,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCALCPT  DD DSN=&MIG.CAL20.CALCPT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZORCANT  DD DSN=&MIG.CAL20.ORCANT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA03ADH  DD DSN=&MIG.CAL20.A03ADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCAL     DD DSN=&MIG.CAL20.CAL.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZHCR     DD DSN=&MIG.CAL20.HCR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZARA     DD DSN=&MIG.CAL20.ARA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZHAR     DD DSN=&MIG.CAL20.HAR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZLCO     DD DSN=&MIG.CAL20.LCO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCALCPTS DD DSN=&MIG.CAL20.CALCPTS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCAL20
//*----------------------------------------------------------------
//PPRCAL20 EXEC PGM=PPRCAL20,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCAL20,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCAL20,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCAL20,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//CALCPT   DD DISP=SHR,DSN=&MIG.CAL14.CALCPT
//SORCANT  DD DISP=SHR,DSN=&MIG.CAL20.ORCANT
//SA03ADH  DD DISP=SHR,DSN=&MIG.CAL01.A03ADH
//*--------------<FICHIERS CIBLES>---------------------------------
//SCAL     DD DSN=&MIG.CAL20.CAL,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SHCR     DD DSN=&MIG.CAL20.HCR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SARA     DD DSN=&MIG.CAL20.ARA,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SHAR     DD DSN=&MIG.CAL20.HAR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SLCO     DD DSN=&MIG.CAL20.LCO,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SCALCPTS DD DSN=&MIG.CAL20.CALCPTS,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZCALCPT  DD DSN=&MIG.CAL20.CALCPT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZORCANT  DD DSN=&MIG.CAL20.ORCANT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA03ADH  DD DSN=&MIG.CAL20.A03ADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCAL     DD DSN=&MIG.CAL20.CAL.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZHCR     DD DSN=&MIG.CAL20.HCR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZARA     DD DSN=&MIG.CAL20.ARA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZHAR     DD DSN=&MIG.CAL20.HAR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZLCO     DD DSN=&MIG.CAL20.LCO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCALCPTS DD DSN=&MIG.CAL20.CALCPTS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCAL16
//*----------------------------------------------------------------
//DEL1601       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FTYPACT  DD DSN=&MIG.CAL16.FTYPACT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTYCAL   DD DSN=&MIG.CAL16.FTYCAL,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCAL02D DD DSN=&MIG.CAL16.FCAL02D,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCAL07D DD DSN=&MIG.CAL16.FCAL07D,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCAL09B DD DSN=&MIG.CAL16.FCAL09B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCAL10  DD DSN=&MIG.CAL16.FCAL10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SNFCAL10  DD DSN=&MIG.CAL16.NFCAL10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FTYPACT
//*----------------------------------------------------------------
//SORT1601 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* FTYPACT SORT TYPACT-TYCALREC ASC [1:5—
//* FTYPACT SORT TYPACT-C-TYP-ACT-REC ASC [6:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.TTYPACT
//SORTIN   DD DISP=SHR,DSN=&TAB.TTYPACT
//SORTOUT  DD DSN=&MIG.CAL16.FTYPACT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,5,CH,A,
               6,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FTYCAL
//*----------------------------------------------------------------
//SORT1602 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* FTYCAL SORT TYCAL-TYPOBJ ASC [1:3—
//* FTYCAL SORT TYCAL-TTS ASC [4:1—
//* FTYCAL SORT TYCAL-URCREP ASC [5:1—
//* FTYCAL SORT TYCAL-RJ ASC [6:1—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.TTYCAL
//SORTIN   DD DISP=SHR,DSN=&TAB.TTYCAL
//SORTOUT  DD DSN=&MIG.CAL16.FTYCAL,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,3,CH,A,
               4,1,CH,A,
               5,1,CH,A,
               6,1,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FCAL02D
//*----------------------------------------------------------------
//SORT1603 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCAL02D SORT FCAL02D-NUM-REGROUP ASC [217:12—
//* SFCAL02D SORT FCAL02D-TYINS ASC [152:1—
//* SFCAL02D SORT FCAL02D-NOINS ASC [153:4—
//* SFCAL02D SORT FCAL02D-IDOBJREC ASC [132:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCAL02D
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL06.FCAL02D
//SORTOUT  DD DSN=&MIG.CAL16.FCAL02D,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(217,12,CH,A,
               152,1,CH,A,
               153,4,CH,A,
               132,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FCAL07D
//*----------------------------------------------------------------
//SORT1604 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCAL07D SORT FCAL07D-NUM-REGROUP ASC [1:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCAL07D
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL08.FCAL07D
//SORTOUT  DD DSN=&MIG.CAL16.FCAL07D,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FCAL09B
//*----------------------------------------------------------------
//SORT1605 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCAL09B SORT FCAL09B-NUM-REGROUP ASC [13:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCAL09B
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL12.FCAL09B
//SORTOUT  DD DSN=&MIG.CAL16.FCAL09B,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(13,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FCAL10
//*----------------------------------------------------------------
//SORT1606 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCAL10 SORT FCAL10-NUM-REGROUP ASC [13:12—
//* SFCAL10 SORT FCAL10-D-PREVUE DESC [28:8—
//* SFCAL10 SORT FCAL10-C-TYPE-ACT-REC DESC [25:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCAL10
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL13.FCAL10
//SORTOUT  DD DSN=&MIG.CAL16.FCAL10,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(13,12,CH,A,
               28,8,CH,D,
               25,3,CH,D)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER NFCAL10
//*----------------------------------------------------------------
//SORT1607 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCAL10 SORT FCAL10-NUM-REGROUP ASC [13:12—
//* SFCAL10 SORT FCAL10-D-PREVUE DESC [28:8—
//* SFCAL10 SORT FCAL10-C-TYPE-ACT-REC DESC [25:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCAL10
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL13.NFCAL10
//SORTOUT  DD DSN=&MIG.CAL16.NFCAL10,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(13,12,CH,A,
               28,8,CH,D,
               25,3,CH,D)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1602       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SCAL     DD DSN=&MIG.CAL16.CAL,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SHCR     DD DSN=&MIG.CAL16.HCR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SARA     DD DSN=&MIG.CAL16.ARA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SHAR     DD DSN=&MIG.CAL16.HAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SLCO     DD DSN=&MIG.CAL16.LCO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCOA     DD DSN=&MIG.CAL16.COA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1603       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCAL16,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCAL16,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCAL16,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCALCPT  DD DSN=&MIG.CAL16.CALCPT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFTYPACT DD DSN=&MIG.CAL16.FTYPACT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFTYCAL  DD DSN=&MIG.CAL16.FTYCAL.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFETAACT DD DSN=&MIG.CAL16.FETAACT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL02D DD DSN=&MIG.CAL16.FCAL02D.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL07D DD DSN=&MIG.CAL16.FCAL07D.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL09B DD DSN=&MIG.CAL16.FCAL09B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL10  DD DSN=&MIG.CAL16.FCAL10.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCAL     DD DSN=&MIG.CAL16.CAL.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZHCR     DD DSN=&MIG.CAL16.HCR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZARA     DD DSN=&MIG.CAL16.ARA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZHAR     DD DSN=&MIG.CAL16.HAR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZLCO     DD DSN=&MIG.CAL16.LCO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCOA     DD DSN=&MIG.CAL16.COA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCAL16
//*----------------------------------------------------------------
//PPRCAL16 EXEC PGM=PPRCAL16,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCAL16,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCAL16,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCAL16,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//CALCPT   DD DISP=SHR,DSN=&MIG.CAL20.CALCPTS
//TC1RSEU  DD DISP=SHR,DSN=&TAB.TC1RSEU
//FTYPACT  DD DISP=SHR,DSN=&MIG.CAL16.FTYPACT
//FTYCAL   DD DISP=SHR,DSN=&MIG.CAL16.FTYCAL
//FETAACT  DD DISP=SHR,DSN=&TAB.TCETACT
//SFCAL02D DD DISP=SHR,DSN=&MIG.CAL16.FCAL02D
//SFCAL07D DD DISP=SHR,DSN=&MIG.CAL16.FCAL07D
//SFCAL09B DD DISP=SHR,DSN=&MIG.CAL16.FCAL09B
//SFCAL10  DD DISP=SHR,DSN=&MIG.CAL16.FCAL10
//SNFCAL10  DD DISP=SHR,DSN=&MIG.CAL16.NFCAL10
//*--------------<FICHIERS CIBLES>---------------------------------
//SCAL     DD DSN=&MIG.CAL16.CAL,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SHCR     DD DSN=&MIG.CAL16.HCR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SARA     DD DSN=&MIG.CAL16.ARA,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SHAR     DD DSN=&MIG.CAL16.HAR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SLCO     DD DSN=&MIG.CAL16.LCO,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SCOA     DD DSN=&MIG.CAL16.COA,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZCALCPT  DD DSN=&MIG.CAL16.CALCPT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFTYPACT DD DSN=&MIG.CAL16.FTYPACT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFTYCAL  DD DSN=&MIG.CAL16.FTYCAL.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFETAACT DD DSN=&MIG.CAL16.FETAACT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL02D DD DSN=&MIG.CAL16.FCAL02D.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL07D DD DSN=&MIG.CAL16.FCAL07D.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL09B DD DSN=&MIG.CAL16.FCAL09B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL10  DD DSN=&MIG.CAL16.FCAL10.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCAL     DD DSN=&MIG.CAL16.CAL.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZHCR     DD DSN=&MIG.CAL16.HCR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZARA     DD DSN=&MIG.CAL16.ARA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZHAR     DD DSN=&MIG.CAL16.HAR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZLCO     DD DSN=&MIG.CAL16.LCO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCOA     DD DSN=&MIG.CAL16.COA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCAL1A
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//DEL1A01  EXEC PGM=IEFBR14
//SYSPRINT DD SYSOUT=*
//SARA     DD DSN=&MIG.CAL1A.ARA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SHAR     DD DSN=&MIG.CAL1A.HAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER ARA
//*----------------------------------------------------------------
//SORT1A01 EXEC PGM=SORT
//* DEBUT CRITERE XGEN
//* ARA SORT ARA-IDCALREC ASC [49:20]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL16.ARA
//COPY     DD DISP=SHR,DSN=&CPYCIB.ARA
//SORTOUT  DD DSN=&MIG.CAL1A.ARA,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER HAR
//*----------------------------------------------------------------
//SORT1A01 EXEC PGM=SORT
//* DEBUT CRITERE XGEN
//* ARA SORT ARA-IDCALREC ASC [49:20]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL16.HAR
//COPY     DD DISP=SHR,DSN=&CPYCIB.HAR
//SORTOUT  DD DSN=&MIG.CAL1A.HAR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1A02  EXEC PGM=IEFBR14
//SYSPRINT DD SYSOUT=*
//*SARA1B   DD DSN=&CIB.ARA1B,
//SARA1B   DD DSN=&MIG.CAL1A.ARA1B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZARA     DD DSN=&MIG.CAL1A.ARA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZARA1B   DD DSN=&MIG.CAL1A.ARA1B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1A03       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCAL1A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCAL1A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCAL1A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION
//*----------------------------------------------------------------
//PPRCAL1A EXEC PGM=PPRCAL1A
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCAL1A,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCAL1A,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5000,500),RLSE)
//FICTRACE DD DSN=&TRC.PPRCAL1A,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5000,500),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DUMMY
//*--------------<FICHIERS SOURCES>--------------------------------
//*SARA     DD DISP=SHR,DSN=&SRC.ARA
//SARA     DD DISP=SHR,DSN=&MIG.CAL1A.ARA
//*SHAR     DD DISP=SHR,DSN=&SRC.HAR
//SHAR     DD DISP=SHR,DSN=&MIG.CAL1A.HAR
//*--------------<FICHIERS CIBLES>---------------------------------
//*SARA1B   DD DSN=&CIB.ARA1B,
//SARA1B   DD DSN=&MIG.CAL1A.ARA1B,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,500),RLSE)
//*SHAR1B   DD DSN=&CIB.ARA1B,
//SHAR1B   DD DSN=&MIG.CAL1A.HAR1B,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,500),RLSE)
//*--------------<FICHIERS REDUITS>--------------------------------
//ZARA     DD DSN=&MIG.CAL1A.ARA.R,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(10,50),RLSE)
//ZARA1B   DD DSN=&MIG.CAL1A.ARA1B,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(10,50),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCAL1B
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//DEL1B01  EXEC PGM=IEFBR14
//SYSPRINT DD SYSOUT=*
//SB05PERI DD DSN=&MIG.CAL1B.B05PERI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SARA1B   DD DSN=&MIG.CAL1B.ARA1B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SHAR1B   DD DSN=&MIG.CAL1B.HAR1B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER B05PERI
//*----------------------------------------------------------------
//SORT1B01 EXEC PGM=SORT
//* DEBUT CRITERE XGEN
//* B05PERI SORT B05-PER-DEB-C-ID-RECOUVR ASC [17:12]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.B05PERIO
//COPY     DD DISP=SHR,DSN=&CPYSRC.B05PERIO
//SORTOUT  DD DSN=&MIG.CAL1B.B05PERI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(17,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER ARA1B
//*----------------------------------------------------------------
//SORT1B02 EXEC PGM=SORT
//* DEBUT CRITERE XGEN
//* ARA1B SORT ARA1B-IDCALREC-NUM ASC [125:12]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL1A.ARA1B
//COPY     DD DISP=SHR,DSN=&CPYMIG.ARA1B
//SORTOUT  DD DSN=&MIG.CAL1B.ARA1B,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(125,12,CH,A,
                69,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER HAR1B
//*----------------------------------------------------------------
//SORT1B03 EXEC PGM=SORT
//* DEBUT CRITERE XGEN
//* HAR1B SORT HAR1B-IDCALREC-NUM ASC [542:12]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL1A.HAR1B
//COPY     DD DISP=SHR,DSN=&CPYMIG.HAR1B
//SORTOUT  DD DSN=&MIG.CAL1B.HAR1B,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(542,12,CH,A,
                69,20,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1B02  EXEC PGM=IEFBR14
//SYSPRINT DD SYSOUT=*
//*SHAR     DD DSN=&CIB.HAR,
//SHAR     DD DSN=&MIG.CAL1B.HAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1B03       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCAL1B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCAL1B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCAL1B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION
//*----------------------------------------------------------------
//PPRCAL1B EXEC PGM=PPRCAL1B
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCAL1B,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCAL1B,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5000,500),RLSE)
//FICTRACE DD DSN=&TRC.PPRCAL1B,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5000,500),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DUMMY
//*--------------<FICHIERS SOURCES>--------------------------------
//*SB05PERI DD DISP=SHR,DSN=&SRC.B05PERI
//SB05PERI DD DISP=SHR,DSN=&MIG.CAL1B.B05PERI
//*SARA1B   DD DISP=SHR,DSN=&SRC.ARA1B
//SARA1B   DD DISP=SHR,DSN=&MIG.CAL1B.ARA1B
//*SHAR1B   DD DISP=SHR,DSN=&SRC.HAR1B
//SHAR1B   DD DISP=SHR,DSN=&MIG.CAL1B.HAR1B
//*--------------<FICHIERS CIBLES>---------------------------------
//*SHAR     DD DSN=&CIB.HAR,
//SHAR     DD DSN=&MIG.CAL1B.HAR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,500),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCAL17
//*----------------------------------------------------------------
//DEL1701       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SCAL01   DD DSN=&MIG.CAL17.CAL01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCAL02   DD DSN=&MIG.CAL17.CAL02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCAL03   DD DSN=&MIG.CAL17.CAL03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SHCR01   DD DSN=&MIG.CAL17.HCR01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SHCR02   DD DSN=&MIG.CAL17.HCR02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SHCR03   DD DSN=&MIG.CAL17.HCR03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SARA01   DD DSN=&MIG.CAL17.ARA01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SARA02   DD DSN=&MIG.CAL17.ARA02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SARA03   DD DSN=&MIG.CAL17.ARA03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SHAR01   DD DSN=&MIG.CAL17.HAR01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SHAR02   DD DSN=&MIG.CAL17.HAR02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SHAR03   DD DSN=&MIG.CAL17.HAR03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SLCO02   DD DSN=&MIG.CAL17.LCO02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SLCO03   DD DSN=&MIG.CAL17.LCO03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCOA01   DD DSN=&MIG.CAL17.COA01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCOA03   DD DSN=&MIG.CAL17.COA03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER CAL01
//*----------------------------------------------------------------
//SORT1701 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCAL01 SORT F1CAL-IDCALREC ASC [49:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.CAL
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL14.CAL
//SORTOUT  DD DSN=&MIG.CAL17.CAL01,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER CAL02
//*----------------------------------------------------------------
//SORT1702 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCAL02 SORT F2CAL-IDCALREC ASC [49:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.CAL
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL20.CAL
//SORTOUT  DD DSN=&MIG.CAL17.CAL02,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER CAL03
//*----------------------------------------------------------------
//SORT1703 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCAL03 SORT F3CAL-IDCALREC ASC [49:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.CAL
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL16.CAL
//SORTOUT  DD DSN=&MIG.CAL17.CAL03,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER HCR01
//*----------------------------------------------------------------
//SORT1704 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SHCR01 SORT F1HCR-IDCALREC ASC [49:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.HCR
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL14.HCR
//SORTOUT  DD DSN=&MIG.CAL17.HCR01,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER HCR02
//*----------------------------------------------------------------
//SORT1705 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SHCR02 SORT F2HCR-IDCALREC ASC [49:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.HCR
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL20.HCR
//SORTOUT  DD DSN=&MIG.CAL17.HCR02,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER HCR03
//*----------------------------------------------------------------
//SORT1706 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SHCR03 SORT F3HCR-IDCALREC ASC [49:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.HCR
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL16.HCR
//SORTOUT  DD DSN=&MIG.CAL17.HCR03,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER ARA01
//*----------------------------------------------------------------
//SORT1707 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SARA01 SORT F1ARA-IDCALREC ASC [49:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.ARA
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL14.ARA
//SORTOUT  DD DSN=&MIG.CAL17.ARA01,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER ARA02
//*----------------------------------------------------------------
//SORT1708 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SARA02 SORT F2ARA-IDCALREC ASC [49:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.ARA
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL20.ARA
//SORTOUT  DD DSN=&MIG.CAL17.ARA02,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER ARA03
//*----------------------------------------------------------------
//SORT1709 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SARA03 SORT F3ARA-IDCALREC ASC [49:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.ARA
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL16.ARA
//SORTOUT  DD DSN=&MIG.CAL17.ARA03,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER HAR01
//*----------------------------------------------------------------
//SORT1710 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SHAR01 SORT F1HAR-IDCALREC ASC [49:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.HAR
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL14.HAR
//SORTOUT  DD DSN=&MIG.CAL17.HAR01,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER HAR02
//*----------------------------------------------------------------
//SORT1711 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SHAR02 SORT F2HAR-IDCALREC ASC [49:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.HAR
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL20.HAR
//SORTOUT  DD DSN=&MIG.CAL17.HAR02,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER HAR03
//*----------------------------------------------------------------
//SORT1712 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SHAR03 SORT F3HAR-IDCALREC ASC [49:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.HAR
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL1B.HAR
//SORTOUT  DD DSN=&MIG.CAL17.HAR03,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER LCO02
//*----------------------------------------------------------------
//SORT1713 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SLCO02 SORT F2LCO-IDCALREC ASC [49:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.LCO
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL20.LCO
//SORTOUT  DD DSN=&MIG.CAL17.LCO02,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER LCO03
//*----------------------------------------------------------------
//SORT1714 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SLCO03 SORT F3LCO-IDCALREC ASC [49:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.LCO
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL16.LCO
//SORTOUT  DD DSN=&MIG.CAL17.LCO03,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER COA01
//*----------------------------------------------------------------
//SORT1715 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCOA01 SORT F1COA-IDCALREC ASC [49:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.COA
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL14.COA
//SORTOUT  DD DSN=&MIG.CAL17.COA01,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER COA03
//*----------------------------------------------------------------
//SORT1716 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCOA03 SORT F3COA-IDCALREC ASC [49:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.COA
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL16.COA
//SORTOUT  DD DSN=&MIG.CAL17.COA03,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1702       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCAL11  DD DSN=&MIG.CAL17.FCAL11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SHCR     DD DSN=&CIB.HCR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SARA     DD DSN=&CIB.ARA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SHAR     DD DSN=&CIB.HAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SLCO     DD DSN=&CIB.LCO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCOA     DD DSN=&CIB.COA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1703       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCAL17,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCAL17,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCAL17,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCAL01   DD DSN=&MIG.CAL17.CAL01.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCAL02   DD DSN=&MIG.CAL17.CAL02.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCAL03   DD DSN=&MIG.CAL17.CAL03.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZHCR01   DD DSN=&MIG.CAL17.HCR01.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZHCR02   DD DSN=&MIG.CAL17.HCR02.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZHCR03   DD DSN=&MIG.CAL17.HCR03.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZARA01   DD DSN=&MIG.CAL17.ARA01.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZARA02   DD DSN=&MIG.CAL17.ARA02.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZARA03   DD DSN=&MIG.CAL17.ARA03.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZHAR01   DD DSN=&MIG.CAL17.HAR01.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZHAR02   DD DSN=&MIG.CAL17.HAR02.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZHAR03   DD DSN=&MIG.CAL17.HAR03.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZLCO02   DD DSN=&MIG.CAL17.LCO02.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZLCO03   DD DSN=&MIG.CAL17.LCO03.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCOA01   DD DSN=&MIG.CAL17.COA01.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCOA03   DD DSN=&MIG.CAL17.COA03.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL11  DD DSN=&MIG.CAL17.FCAL11.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZHCR     DD DSN=&MIG.CAL17.HCR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZARA     DD DSN=&MIG.CAL17.ARA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZHAR     DD DSN=&MIG.CAL17.HAR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZLCO     DD DSN=&MIG.CAL17.LCO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCOA     DD DSN=&MIG.CAL17.COA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCAL17
//*----------------------------------------------------------------
//PPRCAL17 EXEC PGM=PPRCAL17,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCAL17,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCAL17,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCAL17,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SCAL01   DD DISP=SHR,DSN=&MIG.CAL17.CAL01
//SCAL02   DD DISP=SHR,DSN=&MIG.CAL17.CAL02
//SCAL03   DD DISP=SHR,DSN=&MIG.CAL17.CAL03
//SHCR01   DD DISP=SHR,DSN=&MIG.CAL17.HCR01
//SHCR02   DD DISP=SHR,DSN=&MIG.CAL17.HCR02
//SHCR03   DD DISP=SHR,DSN=&MIG.CAL17.HCR03
//SARA01   DD DISP=SHR,DSN=&MIG.CAL17.ARA01
//SARA02   DD DISP=SHR,DSN=&MIG.CAL17.ARA02
//SARA03   DD DISP=SHR,DSN=&MIG.CAL17.ARA03
//SHAR01   DD DISP=SHR,DSN=&MIG.CAL17.HAR01
//SHAR02   DD DISP=SHR,DSN=&MIG.CAL17.HAR02
//SHAR03   DD DISP=SHR,DSN=&MIG.CAL17.HAR03
//SLCO02   DD DISP=SHR,DSN=&MIG.CAL17.LCO02
//SLCO03   DD DISP=SHR,DSN=&MIG.CAL17.LCO03
//SCOA01   DD DISP=SHR,DSN=&MIG.CAL17.COA01
//SCOA03   DD DISP=SHR,DSN=&MIG.CAL17.COA03
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCAL11  DD DSN=&MIG.CAL17.FCAL11,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SHCR     DD DSN=&CIB.HCR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SARA     DD DSN=&CIB.ARA,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SHAR     DD DSN=&CIB.HAR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SLCO     DD DSN=&CIB.LCO,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SCOA     DD DSN=&CIB.COA,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZCAL01   DD DSN=&MIG.CAL17.CAL01.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCAL02   DD DSN=&MIG.CAL17.CAL02.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCAL03   DD DSN=&MIG.CAL17.CAL03.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZHCR01   DD DSN=&MIG.CAL17.HCR01.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZHCR02   DD DSN=&MIG.CAL17.HCR02.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZHCR03   DD DSN=&MIG.CAL17.HCR03.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZARA01   DD DSN=&MIG.CAL17.ARA01.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZARA02   DD DSN=&MIG.CAL17.ARA02.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZARA03   DD DSN=&MIG.CAL17.ARA03.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZHAR01   DD DSN=&MIG.CAL17.HAR01.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZHAR02   DD DSN=&MIG.CAL17.HAR02.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZHAR03   DD DSN=&MIG.CAL17.HAR03.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZLCO02   DD DSN=&MIG.CAL17.LCO02.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZLCO03   DD DSN=&MIG.CAL17.LCO03.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCOA01   DD DSN=&MIG.CAL17.COA01.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCOA03   DD DSN=&MIG.CAL17.COA03.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL11  DD DSN=&MIG.CAL17.FCAL11.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZHCR     DD DSN=&MIG.CAL17.HCR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZARA     DD DSN=&MIG.CAL17.ARA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZHAR     DD DSN=&MIG.CAL17.HAR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZLCO     DD DSN=&MIG.CAL17.LCO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCOA     DD DSN=&MIG.CAL17.COA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCAL18
//*----------------------------------------------------------------
//DEL1801       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCAL11  DD DSN=&MIG.CAL18.FCAL11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCAL11
//*----------------------------------------------------------------
//SORT1801 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCAL11 SORT F1CAL-IDTECADH ASC [16:20—
//* SFCAL11 SORT F1CAL-IDREFCALUTI ASC [69:10—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.CAL
//SORTIN   DD DISP=SHR,DSN=&MIG.CAL17.FCAL11
//SORTOUT  DD DSN=&MIG.CAL18.FCAL11,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,20,CH,A,
               69,10,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1802       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SCAL     DD DSN=&CIB.CAL,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1803       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCAL18,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCAL18,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCAL18,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCAL11  DD DSN=&MIG.CAL18.FCAL11.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCAL     DD DSN=&MIG.CAL18.CAL.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCAL18
//*----------------------------------------------------------------
//PPRCAL18 EXEC PGM=PPRCAL18,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCAL18,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCAL18,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCAL18,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCAL11  DD DISP=SHR,DSN=&MIG.CAL18.FCAL11
//*--------------<FICHIERS CIBLES>---------------------------------
//SCAL     DD DSN=&CIB.CAL,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCAL11  DD DSN=&MIG.CAL18.FCAL11.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCAL     DD DSN=&MIG.CAL18.CAL.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCAL50
//*----------------------------------------------------------------
//DEL5001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SCAL     DD DSN=&MIG.CAL50.CAL,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SLCO     DD DSN=&MIG.CAL50.LCO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SHCR     DD DSN=&MIG.CAL50.HCR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SARA     DD DSN=&MIG.CAL50.ARA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SHAR     DD DSN=&MIG.CAL50.HAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCOA     DD DSN=&MIG.CAL50.COA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER CAL
//*----------------------------------------------------------------
//SORT5001 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCAL SORT CAL-IDCALREC ASC [49:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.CAL
//SORTIN   DD DISP=SHR,DSN=&CIB.CAL
//SORTOUT  DD DSN=&MIG.CAL50.CAL,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER LCO
//*----------------------------------------------------------------
//SORT5002 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SLCO SORT LCO-IDCALREC ASC [49:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.LCO
//SORTIN   DD DISP=SHR,DSN=&CIB.LCO
//SORTOUT  DD DSN=&MIG.CAL50.LCO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER HCR
//*----------------------------------------------------------------
//SORT5003 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SHCR SORT HCR-IDCALREC ASC [49:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.HCR
//SORTIN   DD DISP=SHR,DSN=&CIB.HCR
//SORTOUT  DD DSN=&MIG.CAL50.HCR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER ARA
//*----------------------------------------------------------------
//SORT5004 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SARA SORT ARA-IDCALREC ASC [49:20—
//* SARA SORT ARA-IDACTREC ASC [69:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.ARA
//SORTIN   DD DISP=SHR,DSN=&CIB.ARA
//SORTOUT  DD DSN=&MIG.CAL50.ARA,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A,
               69,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER HAR
//*----------------------------------------------------------------
//SORT5005 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SHAR SORT HAR-IDCALREC ASC [49:20—
//* SHAR SORT HAR-IDACTREC ASC [69:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.HAR
//SORTIN   DD DISP=SHR,DSN=&CIB.HAR
//SORTOUT  DD DSN=&MIG.CAL50.HAR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A,
               69,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER COA
//*----------------------------------------------------------------
//SORT5006 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCOA SORT COA-IDCALREC ASC [49:20—
//* SCOA SORT COA-IDACTREC ASC [69:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.COA
//SORTIN   DD DISP=SHR,DSN=&CIB.COA
//SORTOUT  DD DSN=&MIG.CAL50.COA,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(49,20,CH,A,
               69,20,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL5002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SCALEND  DD DSN=&CIB.CALEND,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL5003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCAL50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCAL50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCAL50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCAL     DD DSN=&MIG.CAL50.CAL.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZLCO     DD DSN=&MIG.CAL50.LCO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZHCR     DD DSN=&MIG.CAL50.HCR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZARA     DD DSN=&MIG.CAL50.ARA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZHAR     DD DSN=&MIG.CAL50.HAR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCOA     DD DSN=&MIG.CAL50.COA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCALEND  DD DSN=&MIG.CAL50.CALEND.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCAL50
//*----------------------------------------------------------------
//PPRCAL50 EXEC PGM=PPRCAL50,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCAL50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCAL50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCAL50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SCAL     DD DISP=SHR,DSN=&MIG.CAL50.CAL
//SLCO     DD DISP=SHR,DSN=&MIG.CAL50.LCO
//SHCR     DD DISP=SHR,DSN=&MIG.CAL50.HCR
//SARA     DD DISP=SHR,DSN=&MIG.CAL50.ARA
//SHAR     DD DISP=SHR,DSN=&MIG.CAL50.HAR
//SCOA     DD DISP=SHR,DSN=&MIG.CAL50.COA
//*--------------<FICHIERS CIBLES>---------------------------------
//SCALEND  DD DSN=&CIB.CALEND,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZCAL     DD DSN=&MIG.CAL50.CAL.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZLCO     DD DSN=&MIG.CAL50.LCO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZHCR     DD DSN=&MIG.CAL50.HCR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZARA     DD DSN=&MIG.CAL50.ARA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZHAR     DD DSN=&MIG.CAL50.HAR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCOA     DD DSN=&MIG.CAL50.COA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCALEND  DD DSN=&MIG.CAL50.CALEND.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*
//* FIN DU JCL DE MIGRATION
