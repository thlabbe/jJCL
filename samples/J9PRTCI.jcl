//J9PRTCI  JOB UTI00TX0,'PR DROITS C04-TCI',CLASS=Z,MSGCLASS=X,
//*         RESTART=DEL0201,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=PLURM.MOVE.PR.JCL.PROD
//MEMBER   INCLUDE MEMBER=$$PREX
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRTCI01
//*----------------------------------------------------------------
//DEL0101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFPADR01 DD DSN=&MIG.TCI01.FPADR01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ21DDDN DD DSN=&MIG.TCI01.J21DDDN,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FPADR01
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFPADR01 SORT FPADR01-C-ID-PERS ASC ç1:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FPADR01
//SORTIN   DD DISP=SHR,DSN=&CIB.FPADR01
//SORTOUT  DD DSN=&MIG.TCI01.FPADR01,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J21DDDN
//*----------------------------------------------------------------
//SORT0102 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ21DDDN SORT J21-DDR-DNF-C-ID-PERS ASC [16:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J21DDR
//SORTIN   DD DISP=SHR,DSN=&SRC.J21DDR
//SORTOUT  DD DSN=&MIG.TCI01.J21DDDN,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFTCI1   DD DSN=&MIG.TCI01.FTCI1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRTCI01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRTCI01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRTCI01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPADR01 DD DSN=&MIG.TCI01.FPADR01.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ21DDDN DD DSN=&MIG.TCI01.J21DDDN.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFTCI1   DD DSN=&MIG.TCI01.FTCI1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRTCI01
//*----------------------------------------------------------------
//PPRTCI01 EXEC PGM=PPRTCI01,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRTCI01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRTCI01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRTCI01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFPADR01 DD DISP=SHR,DSN=&MIG.TCI01.FPADR01
//SJ21DDDN DD DISP=SHR,DSN=&MIG.TCI01.J21DDDN
//*--------------<FICHIERS CIBLES>---------------------------------
//SFTCI1   DD DSN=&MIG.TCI01.FTCI1,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPADR01 DD DSN=&MIG.TCI01.FPADR01.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ21DDDN DD DSN=&MIG.TCI01.J21DDDN.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFTCI1   DD DSN=&MIG.TCI01.FTCI1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRTCI02
//*----------------------------------------------------------------
//DEL0201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFTCI1   DD DSN=&MIG.TCI02.FTCI1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ21DDDN DD DSN=&MIG.TCI02.J21DDDN,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ21DPRC DD DSN=&MIG.TCI02.J21DPRC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ21DDC  DD DSN=&MIG.TCI02.J21DDC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ21SAL  DD DSN=&MIG.TCI02.J21SAL,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FTCI1
//*----------------------------------------------------------------
//SORT0201 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFTCI1 SORT FTCI1-C-ID-DDR-DNF ASC ç1:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.FTCI1
//SORTIN   DD DISP=SHR,DSN=&MIG.TCI01.FTCI1
//SORTOUT  DD DSN=&MIG.TCI02.FTCI1,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J21DDDN
//*----------------------------------------------------------------
//SORT0202 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ21DDDN SORT J21-DDR-DNF-C-ID-DDR-DNF ASC ç2:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J21DDR
//SORTIN   DD DISP=SHR,DSN=&SRC.J21DDR
//SORTOUT  DD DSN=&MIG.TCI02.J21DDDN,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,UNIT=(SYSDA,20),
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J21DPRC
//*----------------------------------------------------------------
//SORT0203 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ21DPRC SORT J21-DOSSIER-PRC-C-ID-DDR-DNF ASC [16:12ó
//* SJ21DPRC SORT J21-DOSSIER-PRC-C-ID-DOS-PRC DESC [2:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J21DOSSI
//SORTIN   DD DISP=SHR,DSN=&SRC.J21DOSSI
//SORTOUT  DD DSN=&MIG.TCI02.J21DPRC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,12,CH,A,
               2,12,CH,D)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J21DDC
//*----------------------------------------------------------------
//SORT0204 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ21DDC SORT J21-DDC-C-ID-DDR-DNF ASC [2:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J21DDC
//SORTIN   DD DISP=SHR,DSN=&SRC.J21DDC
//SORTOUT  DD DSN=&MIG.TCI02.J21DDC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J21SAL
//*----------------------------------------------------------------
//SORT0205 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ21SAL SORT J21-SAL-C-ID-DDR-DNF ASC [2:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J21SAL
//SORTIN   DD DISP=SHR,DSN=&SRC.J21SAL
//SORTOUT  DD DSN=&MIG.TCI02.J21SAL,
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
//STKPCDDR DD DSN=&CIB.TKPCDDR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCDNF DD DSN=&CIB.TKPCDNF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCSVR DD DSN=&CIB.TKPCSVR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCSNF DD DSN=&CIB.TKPCSNF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCDDC DD DSN=&CIB.TKPCDDC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCSAL DD DSN=&CIB.TKPCSAL,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFTCI2   DD DSN=&MIG.TCI02.FTCI2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRTCI02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRTCI02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRTCI02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFTABCO  DD DSN=&MIG.TCI02.FTABCO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFTCI1   DD DSN=&MIG.TCI02.FTCI1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ21DDDN DD DSN=&MIG.TCI02.J21DDDN.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ21DPRC DD DSN=&MIG.TCI02.J21DPRC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ21DDC  DD DSN=&MIG.TCI02.J21DDC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ21SAL  DD DSN=&MIG.TCI02.J21SAL.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTKPCDDR DD DSN=&MIG.TCI02.TKPCDDR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTKPCDNF DD DSN=&MIG.TCI02.TKPCDNF.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTKPCSVR DD DSN=&MIG.TCI02.TKPCSVR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTKPCSNF DD DSN=&MIG.TCI02.TKPCSNF.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTKPCDDC DD DSN=&MIG.TCI02.TKPCDDC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTKPCSAL DD DSN=&MIG.TCI02.TKPCSAL.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFTCI2   DD DSN=&MIG.TCI02.FTCI2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRTCI02
//*----------------------------------------------------------------
//PPRTCI02 EXEC PGM=PPRTCI02,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRTCI02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRTCI02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRTCI02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//FTABCO   DD DISP=SHR,DSN=&TAB.TCODTDI
//SFTCI1   DD DISP=SHR,DSN=&MIG.TCI02.FTCI1
//SJ21DDDN DD DISP=SHR,DSN=&MIG.TCI02.J21DDDN
//SJ21DPRC DD DISP=SHR,DSN=&MIG.TCI02.J21DPRC
//SJ21DDC  DD DISP=SHR,DSN=&MIG.TCI02.J21DDC
//SJ21SAL  DD DISP=SHR,DSN=&MIG.TCI02.J21SAL
//*--------------<FICHIERS CIBLES>---------------------------------
//STKPCDDR DD DSN=&CIB.TKPCDDR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STKPCDNF DD DSN=&CIB.TKPCDNF,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STKPCSVR DD DSN=&CIB.TKPCSVR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STKPCSNF DD DSN=&CIB.TKPCSNF,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STKPCDDC DD DSN=&CIB.TKPCDDC,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STKPCSAL DD DSN=&CIB.TKPCSAL,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFTCI2   DD DSN=&MIG.TCI02.FTCI2,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFTABCO  DD DSN=&MIG.TCI02.FTABCO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFTCI1   DD DSN=&MIG.TCI02.FTCI1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ21DDDN DD DSN=&MIG.TCI02.J21DDDN.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ21DPRC DD DSN=&MIG.TCI02.J21DPRC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ21DDC  DD DSN=&MIG.TCI02.J21DDC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ21SAL  DD DSN=&MIG.TCI02.J21SAL.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTKPCDDR DD DSN=&MIG.TCI02.TKPCDDR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTKPCDNF DD DSN=&MIG.TCI02.TKPCDNF.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTKPCSVR DD DSN=&MIG.TCI02.TKPCSVR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTKPCSNF DD DSN=&MIG.TCI02.TKPCSNF.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTKPCDDC DD DSN=&MIG.TCI02.TKPCDDC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTKPCSAL DD DSN=&MIG.TCI02.TKPCSAL.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFTCI2   DD DSN=&MIG.TCI02.FTCI2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRTCI03
//*----------------------------------------------------------------
//DEL0301       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFTCI2   DD DSN=&MIG.TCI03.FTCI2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ21DPRC DD DSN=&MIG.TCI03.J21DPRC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ21ANO  DD DSN=&MIG.TCI03.J21ANO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ21LOC  DD DSN=&MIG.TCI03.J21LOC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ21ENT  DD DSN=&MIG.TCI03.J21ENT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ21PCO  DD DSN=&MIG.TCI03.J21PCO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ21PDR  DD DSN=&MIG.TCI03.J21PDR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ21TRA  DD DSN=&MIG.TCI03.J21TRA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ21PTS  DD DSN=&MIG.TCI03.J21PTS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FTCI2
//*----------------------------------------------------------------
//SORT0301 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFTCI2 SORT FTCI2-C-ID-DOS-PRC ASC ç1:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.FTCI2
//SORTIN   DD DISP=SHR,DSN=&MIG.TCI02.FTCI2
//SORTOUT  DD DSN=&MIG.TCI03.FTCI2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J21DPRC
//*----------------------------------------------------------------
//SORT0302 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ21DPRC SORT J21-DOSSIER-PRC-C-ID-DOS-PRC ASC ç2:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J21DOSSI
//SORTIN   DD DISP=SHR,DSN=&SRC.J21DOSSI
//SORTOUT  DD DSN=&MIG.TCI03.J21DPRC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J21ANO
//*----------------------------------------------------------------
//SORT0303 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ21ANO SORT J21-ANO-PRC-C-ID-DOS-PRC ASC [37:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J21ANO
//SORTIN   DD DISP=SHR,DSN=&SRC.J21ANO
//SORTOUT  DD DSN=&MIG.TCI03.J21ANO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(37,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J21LOC
//*----------------------------------------------------------------
//SORT0304 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ21DPRC SORT J21-DOSSIER-PRC-C-ID-DOS-PRC ASC ç2:12Ù
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J21LOC
//SORTIN   DD DISP=SHR,DSN=&SRC.J21LOC
//SORTOUT  DD DSN=&MIG.TCI03.J21LOC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
  *MEMSIZE=800000000
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J21ENT
//*----------------------------------------------------------------
//SORT0305 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ21ENT SORT J21-ENT-C-ID-DOS-PRC ASC [2:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J21ENT
//SORTIN   DD DISP=SHR,DSN=&SRC.J21ENT
//SORTOUT  DD DSN=&MIG.TCI03.J21ENT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
  *MEMSIZE=800000000
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J21PCO
//*----------------------------------------------------------------
//SORT0306 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ21PCO SORT J21-PCO-C-ID-DOS-PRC ASC [2:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J21PCO
//SORTIN   DD DISP=SHR,DSN=&SRC.J21PCO
//SORTOUT  DD DSN=&MIG.TCI03.J21PCO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J21PDR
//*----------------------------------------------------------------
//SORT0307 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ21PDR SORT J21-PDR-C-ID-DOS-PRC ASC [2:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J21PDR
//SORTIN   DD DISP=SHR,DSN=&SRC.J21PDR
//SORTOUT  DD DSN=&MIG.TCI03.J21PDR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J21TRA
//*----------------------------------------------------------------
//SORT0308 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ21TRA SORT J21-TRA-C-ID-DOS-PRC ASC [2:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J21TRA
//SORTIN   DD DISP=SHR,DSN=&SRC.J21TRA
//SORTOUT  DD DSN=&MIG.TCI03.J21TRA,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J21PTS
//*----------------------------------------------------------------
//SORT0309 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ21PTS SORT J21-PTS-C-ID-DOS-PRC ASC [2:12ó
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J21PTS
//SORTIN   DD DISP=SHR,DSN=&SRC.J21PTS
//SORTOUT  DD DSN=&MIG.TCI03.J21PTS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,UNIT=(SYSDA,20),
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0302       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STKPCSIG DD DSN=&CIB.TKPCSIG,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCDEB DD DSN=&CIB.TKPCDEB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCLOC DD DSN=&CIB.TKPCLOC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCENT DD DSN=&CIB.TKPCENT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCPCO DD DSN=&CIB.TKPCPCO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCPDR DD DSN=&CIB.TKPCPDR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCTRA DD DSN=&CIB.TKPCTRA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STKPCPTS DD DSN=&CIB.TKPCPTS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0303       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRTCI03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRTCI03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRTCI03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFINST   DD DSN=&MIG.TCI03.FINST.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFTCI2   DD DSN=&MIG.TCI03.FTCI2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ21DPRC DD DSN=&MIG.TCI03.J21DPRC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ21ANO  DD DSN=&MIG.TCI03.J21ANO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ21LOC  DD DSN=&MIG.TCI03.J21LOC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ21ENT  DD DSN=&MIG.TCI03.J21ENT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ21PCO  DD DSN=&MIG.TCI03.J21PCO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ21PDR  DD DSN=&MIG.TCI03.J21PDR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ21TRA  DD DSN=&MIG.TCI03.J21TRA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ21PTS  DD DSN=&MIG.TCI03.J21PTS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTKPCSIG DD DSN=&MIG.TCI03.TKPCSIG.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTKPCDEB DD DSN=&MIG.TCI03.TKPCDEB.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTKPCLOC DD DSN=&MIG.TCI03.TKPCLOC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTKPCENT DD DSN=&MIG.TCI03.TKPCENT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTKPCPCO DD DSN=&MIG.TCI03.TKPCPCO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTKPCPDR DD DSN=&MIG.TCI03.TKPCPDR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTKPCTRA DD DSN=&MIG.TCI03.TKPCTRA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTKPCPTS DD DSN=&MIG.TCI03.TKPCPTS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRTCI03
//*----------------------------------------------------------------
//PPRTCI03 EXEC PGM=PPRTCI03,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRTCI03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRTCI03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRTCI03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//FINST    DD DISP=SHR,DSN=&SRC.L01INSTI
//SFTCI2   DD DISP=SHR,DSN=&MIG.TCI03.FTCI2
//SJ21DPRC DD DISP=SHR,DSN=&MIG.TCI03.J21DPRC
//SJ21ANO  DD DISP=SHR,DSN=&MIG.TCI03.J21ANO
//SJ21LOC  DD DISP=SHR,DSN=&MIG.TCI03.J21LOC
//SJ21ENT  DD DISP=SHR,DSN=&MIG.TCI03.J21ENT
//SJ21PCO  DD DISP=SHR,DSN=&MIG.TCI03.J21PCO
//SJ21PDR  DD DISP=SHR,DSN=&MIG.TCI03.J21PDR
//SJ21TRA  DD DISP=SHR,DSN=&MIG.TCI03.J21TRA
//SJ21PTS  DD DISP=SHR,DSN=&MIG.TCI03.J21PTS
//*--------------<FICHIERS CIBLES>---------------------------------
//STKPCSIG DD DSN=&CIB.TKPCSIG,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STKPCDEB DD DSN=&CIB.TKPCDEB,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STKPCLOC DD DSN=&CIB.TKPCLOC,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STKPCENT DD DSN=&CIB.TKPCENT,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STKPCPCO DD DSN=&CIB.TKPCPCO,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STKPCPDR DD DSN=&CIB.TKPCPDR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STKPCTRA DD DSN=&CIB.TKPCTRA,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STKPCPTS DD DSN=&CIB.TKPCPTS,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFINST   DD DSN=&MIG.TCI03.FINST.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFTCI2   DD DSN=&MIG.TCI03.FTCI2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ21DPRC DD DSN=&MIG.TCI03.J21DPRC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ21ANO  DD DSN=&MIG.TCI03.J21ANO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ21LOC  DD DSN=&MIG.TCI03.J21LOC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ21ENT  DD DSN=&MIG.TCI03.J21ENT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ21PCO  DD DSN=&MIG.TCI03.J21PCO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ21PDR  DD DSN=&MIG.TCI03.J21PDR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ21TRA  DD DSN=&MIG.TCI03.J21TRA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ21PTS  DD DSN=&MIG.TCI03.J21PTS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTKPCSIG DD DSN=&MIG.TCI03.TKPCSIG.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTKPCDEB DD DSN=&MIG.TCI03.TKPCDEB.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTKPCLOC DD DSN=&MIG.TCI03.TKPCLOC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTKPCENT DD DSN=&MIG.TCI03.TKPCENT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTKPCPCO DD DSN=&MIG.TCI03.TKPCPCO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTKPCPDR DD DSN=&MIG.TCI03.TKPCPDR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTKPCTRA DD DSN=&MIG.TCI03.TKPCTRA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTKPCPTS DD DSN=&MIG.TCI03.TKPCPTS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*PPRTCI50 NON EXECUTE CAR LE KM N UTILISE PAS LE PIVOT ENCAPSULE
