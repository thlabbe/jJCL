//J9PRINS  JOB UTI00TX0,'PR GRECCO G14-INS',CLASS=Z,MSGCLASS=X,
//*        RESTART=DEL0101,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=PLURM.MOVE.PRREF.JCL.PROD
//MEMBER   INCLUDE MEMBER=$$PRRE
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRINS01
//*----------------------------------------------------------------
//DEL0101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFPADH2  DD DSN=&MIG.INS01.FPADH2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA3ADHER DD DSN=&MIG.INS01.A3ADHER,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB04COND DD DSN=&MIG.INS01.B04COND,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FPADH2
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFPADH2 SORT FPADH2-C-GRPT-GEST ASC [24:1—
//* SFPADH2 SORT FPADH2-ADH ASC [25:9—
//* SFPADH2 SORT FPADH2-C-ORDRE ASC [34:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FPADH2
//SORTIN   DD DISP=SHR,DSN=&CIB.FPADH2
//SORTOUT  DD DSN=&MIG.INS01.FPADH2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(24,1,CH,A,
               25,9,CH,A,
               34,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI2 DU FICHIER A03ADH
//*----------------------------------------------------------------
//SORT0102 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA03ADH SORT A03-ADHERENT-C-GRPT-GEST ASC [1:1—
//* SA03ADH SORT A03-ADHERENT-C-ADH ASC [3:9—
//* SA03ADH SORT A03-ADHERENT-C-ORDRE ASC [13:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03ADHER
//SORTIN   DD DISP=SHR,DSN=&MIG.A03ADHER
//SORTOUT  DD DSN=&MIG.INS01.A3ADHER,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,1,CH,A,
               3,9,CH,A,
               13,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI3 DU FICHIER B04COND
//*----------------------------------------------------------------
//SORT0103 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB04COND SORT B04-CONTRAT-DUCS-C-GRPT-GEST ASC [1:1—
//* SB04COND SORT B04-CONTRAT-DUCS-C-ADH ASC [3:9—
//* SB04COND SORT B04-CONTRAT-DUCS-C-ORDRE ASC [13:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.B04CDUCS
//SORTIN   DD DISP=SHR,DSN=&SRC.B04CDUCS
//SORTOUT  DD DSN=&MIG.INS01.B04COND,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,1,CH,A,
               3,9,CH,A,
               13,4,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SIND     DD DSN=&MIG.INS01.IND,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SUDE     DD DSN=&CIB.UDE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRINS01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRINS01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRINS01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPADH2  DD DSN=&MIG.INS01.FPADH2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA03ADH  DD DSN=&MIG.INS01.A03ADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZB04CDUC DD DSN=&MIG.INS01.B04CDUC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZIND     DD DSN=&MIG.INS01.IND.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZUDE     DD DSN=&MIG.INS01.UDE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRINS01
//*----------------------------------------------------------------
//PPRINS01 EXEC PGM=PPRINS01,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRINS01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRINS01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRINS01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFPADH2  DD DISP=SHR,DSN=&MIG.INS01.FPADH2
//SA03ADH  DD DISP=SHR,DSN=&MIG.INS01.A3ADHER
//SB04CDUC DD DISP=SHR,DSN=&MIG.INS01.B04COND
//*--------------<FICHIERS CIBLES>---------------------------------
//SIND     DD DSN=&MIG.INS01.IND,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SUDE     DD DSN=&CIB.UDE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPADH2  DD DSN=&MIG.INS01.FPADH2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA03ADH  DD DSN=&MIG.INS01.A03ADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZB04CDUC DD DSN=&MIG.INS01.B04CDUC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZIND     DD DSN=&MIG.INS01.IND.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZUDE     DD DSN=&MIG.INS01.UDE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRINS02
//*----------------------------------------------------------------
//DEL0201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SEIND    DD DSN=&MIG.INS02.EIND,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENADH DD DSN=&MIG.INS02.TRENADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER EIND
//*----------------------------------------------------------------
//SORT0201 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SEIND SORT EIND-IDTECADH ASC [16:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.IND
//SORTIN   DD DISP=SHR,DSN=&MIG.INS01.IND
//SORTOUT  DD DSN=&MIG.INS02.EIND,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TRENADH
//*----------------------------------------------------------------
//SORT0202 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STRENADH SORT TRENADH-ID-TECH-UR-ADHR ASC [1:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRENADH
//SORTIN   DD DISP=SHR,DSN=&CIB.TRENADH
//SORTOUT  DD DSN=&MIG.INS02.TRENADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0202       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SSIND    DD DSN=&CIB.IND,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRINS02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRINS02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRINS02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZEIND    DD DSN=&MIG.INS02.EIND.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRENADH DD DSN=&MIG.INS02.TRENADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZSIND    DD DSN=&MIG.INS02.SIND.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRINS02
//*----------------------------------------------------------------
//PPRINS02 EXEC PGM=PPRINS02,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRINS02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRINS02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRINS02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SEIND    DD DISP=SHR,DSN=&MIG.INS02.EIND
//STRENADH DD DISP=SHR,DSN=&MIG.INS02.TRENADH
//*--------------<FICHIERS CIBLES>---------------------------------
//SSIND    DD DSN=&CIB.IND,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZEIND    DD DSN=&MIG.INS02.EIND.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRENADH DD DSN=&MIG.INS02.TRENADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZSIND    DD DSN=&MIG.INS02.SIND.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL5002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SINSDEM  DD DSN=&CIB.INSDEM,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL5003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRINS50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRINS50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRINS50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZIND     DD DSN=&MIG.INS50.IND.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZUDE     DD DSN=&MIG.INS50.UDE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZINSDEM  DD DSN=&MIG.INS50.INSDEM.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRINS50
//*----------------------------------------------------------------
//PPRINS50 EXEC PGM=PPRINS50,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRINS50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRINS50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRINS50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//IND      DD DISP=SHR,DSN=&CIB.IND
//UDE      DD DISP=SHR,DSN=&CIB.UDE
//*--------------<FICHIERS CIBLES>---------------------------------
//SINSDEM  DD DSN=&CIB.INSDEM,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZIND     DD DSN=&MIG.INS50.IND.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZUDE     DD DSN=&MIG.INS50.UDE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZINSDEM  DD DSN=&MIG.INS50.INSDEM.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*
//* FIN DU JCL DE MIGRATION
