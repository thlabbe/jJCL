//J9PRENT  JOB UTI00TX0,'PR BREF A05-ENT 2',CLASS=Z,MSGCLASS=X,
//* Modifié par GCA : mise en dummy des TRENVIB et TRENVIE dans la concaténation
//* produisant les pivots PVENTH50 (mail de Patrice Mora 18/6 à 16h15
//*  MODOFICATION temporaire (doublons, analyse MOE à faire)
//*         RESTART=DEL1602,
//*         RESTART=DEL5001,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=PLURM.MOVE.PR.JCL.PROD
//MEMBER   INCLUDE MEMBER=$$PRE
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRENT05
//*----------------------------------------------------------------
//DEL0501       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFPILO2  DD DSN=&MIG.ENT05.FPILO2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFENT4   DD DSN=&MIG.ENT05.FENT4,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFENT31  DD DSN=&MIG.ENT05.FENT31,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFENT32  DD DSN=&MIG.ENT05.FENT32,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FPILO2
//*----------------------------------------------------------------
//SORT0501 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFPILO2 SORT FPILO2-C-ID-PERS ASC [41:12—
//* FIN   CRITERE XGEN
//*FNC2643 C'EST FPILO2 QU'IL FAUT PRENDRE
//*SORTIN   DD DISP=SHR,DSN=&CIB.FPILO
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.FPILO2
//SORTIN   DD DISP=SHR,DSN=&CIB.FPILO2
//SORTOUT  DD DSN=&MIG.ENT05.FPILO2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(41,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FENT4
//*----------------------------------------------------------------
//SORT0502 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFENT4 SORT FENT4-C-ID-PERS ASC ¬21:12¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.FENT4
//SORTIN   DD DISP=SHR,DSN=&MIG.ENT04.FENT4
//SORTOUT  DD DSN=&MIG.ENT05.FENT4,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FENT3
//*----------------------------------------------------------------
//SORT0503 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFENT3 SORT FENT3-C-ID-PERS-ENTR ASC [50:12—
//* SFENT3 SORT FENT3-D-FONDATION ASC [62:8—
//* SFENT3 SORT FENT3-C-NIC ASC [45:5—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.FENT3
//SORTIN   DD DISP=SHR,DSN=&MIG.ENT03.FENT3
//SORTOUT  DD DSN=&MIG.ENT05.FENT31,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(50,12,CH,A,
               62,3,CH,A,
               45,5,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FENT3
//*----------------------------------------------------------------
//SORT0504 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFENT3 SORT FENT3-C-ID-PERS-ENTR ASC [50:12—
//* SFENT3 SORT FENT3-D-SITUATION DESC [37:8—
//* SFENT3 SORT FENT3-C-NIC ASC [45:5—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.FENT3
//SORTIN   DD DISP=SHR,DSN=&MIG.ENT03.FENT3
//SORTOUT  DD DSN=&MIG.ENT05.FENT32,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(50,12,CH,A,
               37,8,CH,D,
               45,5,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0502       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFPENT5  DD DSN=&CIB.FPENT5,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFENT5   DD DSN=&MIG.ENT05.FENT5,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0503       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRENT05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRENT05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRENT05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPILO2  DD DSN=&MIG.ENT05.FPILO2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT4   DD DSN=&MIG.ENT05.FENT4.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZSIEGES  DD DSN=&MIG.ENT05.SIEGES.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZETABOUV DD DSN=&MIG.ENT05.ETABOUV.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZETABFER DD DSN=&MIG.ENT05.ETABFER.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPENT5  DD DSN=&MIG.ENT05.FPENT5.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT5   DD DSN=&MIG.ENT05.FENT5.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRENT05
//*----------------------------------------------------------------
//PPRENT05 EXEC PGM=PPRENT05,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRENT05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRENT05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRENT05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFPILO2  DD DISP=SHR,DSN=&MIG.ENT05.FPILO2
//SFENT4   DD DISP=SHR,DSN=&MIG.ENT05.FENT4
//SSIEGES  DD DISP=SHR,DSN=&MIG.ENT05.FENT31
//SETABOUV DD DISP=SHR,DSN=&MIG.ENT05.FENT31
//SETABFER DD DISP=SHR,DSN=&MIG.ENT05.FENT32
//*--------------<FICHIERS \IBLES>---------------------------------
//SFPENT5  DD DSN=&CIB.FPENT5,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFENT5   DD DSN=&MIG.ENT05.FENT5,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPILO2  DD DSN=&MIG.ENT05.FPILO2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT4   DD DSN=&MIG.ENT05.FENT4.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZSIEGES  DD DSN=&MIG.ENT05.SIEGES.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZETABOUV DD DSN=&MIG.ENT05.ETABOUV.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZETABFER DD DSN=&MIG.ENT05.ETABFER.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPENT5  DD DSN=&MIG.ENT05.FPENT5.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT5   DD DSN=&MIG.ENT05.FENT5.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRENT06
//*----------------------------------------------------------------
//DEL0601       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFPILO1  DD DSN=&MIG.ENT06.FPILO1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFENT5   DD DSN=&MIG.ENT06.FENT5,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPILO2  DD DSN=&MIG.ENT06.FPILO2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFENT4   DD DSN=&MIG.ENT06.FENT4,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFENT3   DD DSN=&MIG.ENT06.FENT3,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FPILO1
//*----------------------------------------------------------------
//SORT0601 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFPILO1 SORT FPILO-C-ID-PERS ASC ¬41:12¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.FPILO
//SORTIN   DD DISP=SHR,DSN=&CIB.FPILO
//SORTOUT  DD DSN=&MIG.ENT06.FPILO1,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(41,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FENT5
//*----------------------------------------------------------------
//SORT0602 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFENT5 SORT AFPILO-C-ID-PERS ASC ¬41:12¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.FPILO
//SORTIN   DD DISP=SHR,DSN=&MIG.ENT05.FENT5
//SORTOUT  DD DSN=&MIG.ENT06.FENT5,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(41,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FPILO2
//*----------------------------------------------------------------
//SORT0603 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFPILO2 SORT FPILO2-C-ID-PERS ASC ¬41:12¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.FPILO2
//SORTIN   DD DISP=SHR,DSN=&CIB.FPILO2
//SORTOUT  DD DSN=&MIG.ENT06.FPILO2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(41,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FENT4
//*----------------------------------------------------------------
//SORT0604 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFENT4 SORT FENT4-C-ID-PERS ASC ¬21:12¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.FENT4
//SORTIN   DD DISP=SHR,DSN=&MIG.ENT04.FENT4
//SORTOUT  DD DSN=&MIG.ENT06.FENT4,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FENT3
//*----------------------------------------------------------------
//SORT0605 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFENT3 SORT FENT3-C-ID-PERS ASC ¬21:12¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.FENT3
//SORTIN   DD DISP=SHR,DSN=&MIG.ENT03.FENT3
//SORTOUT  DD DSN=&MIG.ENT06.FENT3,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0602       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STRENDFT DD DSN=&CIB.TRENDFT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENLLO DD DSN=&CIB.TRENLLO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0603       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRENT06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRENT06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRENT06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPILO1  DD DSN=&MIG.ENT06.FPILO1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT5   DD DSN=&MIG.ENT06.FENT5.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPILO2  DD DSN=&MIG.ENT06.FPILO2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT4   DD DSN=&MIG.ENT06.FENT4.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT3   DD DSN=&MIG.ENT06.FENT3.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRENDFT DD DSN=&MIG.ENT06.TRENDFT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRENLLO DD DSN=&MIG.ENT06.TRENLLO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRENT06
//*----------------------------------------------------------------
//PPRENT06 EXEC PGM=PPRENT06,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRENT06,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRENT06,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRENT06,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFPILO1  DD DISP=SHR,DSN=&MIG.ENT06.FPILO1
//SFENT5   DD DISP=SHR,DSN=&MIG.ENT06.FENT5
//SFPILO2  DD DISP=SHR,DSN=&MIG.ENT06.FPILO2
//SFENT4   DD DISP=SHR,DSN=&MIG.ENT06.FENT4
//SFENT3   DD DISP=SHR,DSN=&MIG.ENT06.FENT3
//*--------------<FICHIERS CIBLES>---------------------------------
//STRENDFT DD DSN=&CIB.TRENDFT,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRENLLO DD DSN=&CIB.TRENLLO,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPILO1  DD DSN=&MIG.ENT06.FPILO1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT5   DD DSN=&MIG.ENT06.FENT5.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPILO2  DD DSN=&MIG.ENT06.FPILO2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT4   DD DSN=&MIG.ENT06.FENT4.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT3   DD DSN=&MIG.ENT06.FENT3.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRENDFT DD DSN=&MIG.ENT06.TRENDFT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRENLLO DD DSN=&MIG.ENT06.TRENLLO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRENT15
//*----------------------------------------------------------------
//DEL1501       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SA01ENTB DD DSN=&MIG.ENT15.A01ENTB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SC02DOSC DD DSN=&MIG.ENT15.C02DOSC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER A01ENTB
//*----------------------------------------------------------------
//SORT1501 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01ENTB SORT A01-ENTR-ETAB-C-ID-PERS ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01ENTR
//*SORTIN   DD DISP=SHR,DSN=&MIG.ENT00.AFENT0
//SORTIN   DD DISP=SHR,DSN=&SRC.A01ENTR
//SORTOUT  DD DSN=&MIG.ENT15.A01ENTB,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER C02DOSC
//*----------------------------------------------------------------
//SORT1502 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SC02DOSC SORT C02-DOSS-CTX-C-ID-PERS ASC ¬32:12¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.C02DOSS
//SORTIN   DD DISP=SHR,DSN=&SRC.C02DOSS
//SORTOUT  DD DSN=&MIG.ENT15.C02DOSC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(32,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1502       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFENT15  DD DSN=&MIG.ENT15.FENT15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1503       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRENT15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRENT15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRENT15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01ENTB DD DSN=&MIG.ENT15.A01ENTB.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZC02DOSC DD DSN=&MIG.ENT15.C02DOSC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT15  DD DSN=&MIG.ENT15.FENT15.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRENT15
//*----------------------------------------------------------------
//PPRENT15 EXEC PGM=PPRENT15,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRENT15,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRENT15,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRENT15,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SA01ENTB DD DISP=SHR,DSN=&MIG.ENT15.A01ENTB
//SC02DOSC DD DISP=SHR,DSN=&MIG.ENT15.C02DOSC
//*--------------<FICHIERS CIBLES>---------------------------------
//SFENT15  DD DSN=&MIG.ENT15.FENT15,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01ENTB DD DSN=&MIG.ENT15.A01ENTB.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZC02DOSC DD DSN=&MIG.ENT15.C02DOSC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT15  DD DSN=&MIG.ENT15.FENT15.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRENT07
//*----------------------------------------------------------------
//DEL0701       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//C02EVE   DD DSN=&MIG.ENT07.C02EVE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SC02DOSC DD DSN=&MIG.ENT07.C02DOSC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFENT15  DD DSN=&MIG.ENT07.FENT15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SC02EVED DD DSN=&MIG.ENT07.C02EVED,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SC02SJUG DD DSN=&MIG.ENT07.C02SJUG,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER C02DOSC
//*----------------------------------------------------------------
//SORT0702 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SC02DOSC SORT C02-DOSS-CTX-C-ID-DOSS ASC ¬19:12¦
//* SC02DOSC SORT C02-DOSS-CTX-C-ID-PERS ASC ¬32:12¦
//* SC02DOSC SORT C02-DOSS-CTX-D-JUGEMENT ASC ¬1:8¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.C02DOSS
//SORTIN   DD DISP=SHR,DSN=&SRC.C02DOSS
//SORTOUT  DD DSN=&MIG.ENT07.C02DOSC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(19,12,CH,A,
               32,12,CH,A,
               1,8,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FENT15
//*----------------------------------------------------------------
//SORT0703 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFENT15 SORT FENT15-C-ID-DOSS ASC ¬1:12¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.FENT15
//SORTIN   DD DISP=SHR,DSN=&MIG.ENT15.FENT15
//SORTOUT  DD DSN=&MIG.ENT07.FENT15,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER C02EVED
//*----------------------------------------------------------------
//SORT0704 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SC02EVED SORT C02-EVE-PROC-C-ID-DOSS ASC ¬347:12¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.C02EVEPR
//SORTIN   DD DISP=SHR,DSN=&SRC.C02EVEPR
//SORTOUT  DD DSN=&MIG.ENT07.C02EVED,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(347,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER C02SJUG
//*----------------------------------------------------------------
//SORT0705 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SC02SJUG SORT C02-SUIVI-JUG-C-ID-DOSS ASC ¬2:12¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.C02SUIVI
//SORTIN   DD DISP=SHR,DSN=&SRC.C02SUIVI
//SORTOUT  DD DSN=&MIG.ENT07.C02SJUG,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0702       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFENT7   DD DSN=&MIG.ENT07.FENT7,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCHRVIE DD DSN=&MIG.ENT07.FCHRVIE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0703       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRENT07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRENT07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRENT07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZC02EVE  DD DSN=&MIG.ENT07.C02EVE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZC02DOSC DD DSN=&MIG.ENT07.C02DOSC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT15  DD DSN=&MIG.ENT07.FENT15.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZC02EVED DD DSN=&MIG.ENT07.C02EVED.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZC02SJUG DD DSN=&MIG.ENT07.C02SJUG.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT7   DD DSN=&MIG.ENT07.FENT7.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCHRVIE DD DSN=&MIG.ENT07.FCHRVIE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRENT07
//*----------------------------------------------------------------
//PPRENT07 EXEC PGM=PPRENT07,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRENT07,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRENT07,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRENT07,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//*C02EVE   DD DISP=SHR,DSN=&SRC.C02EVENE
//TBRCYVI  DD DISP=SHR,DSN=&TAB.TBRCYVI
//SC02DOSC DD DISP=SHR,DSN=&MIG.ENT07.C02DOSC
//SFENT15  DD DISP=SHR,DSN=&MIG.ENT07.FENT15
//SC02EVED DD DISP=SHR,DSN=&MIG.ENT07.C02EVED
//SC02SJUG DD DISP=SHR,DSN=&MIG.ENT07.C02SJUG
//*--------------<FICHIERS CIBLES>---------------------------------
//SFENT7   DD DSN=&MIG.ENT07.FENT7,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFCHRVIE DD DSN=&MIG.ENT07.FCHRVIE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZC02EVE  DD DSN=&MIG.ENT07.C02EVE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZC02DOSC DD DSN=&MIG.ENT07.C02DOSC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT15  DD DSN=&MIG.ENT07.FENT15.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZC02EVED DD DSN=&MIG.ENT07.C02EVED.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZC02SJUG DD DSN=&MIG.ENT07.C02SJUG.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT7   DD DSN=&MIG.ENT07.FENT7.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCHRVIE DD DSN=&MIG.ENT07.FCHRVIE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRENT16
//*----------------------------------------------------------------
//DEL1601       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SA01ENTB DD DSN=&MIG.ENT16.A01ENTB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA03CONT DD DSN=&MIG.ENT16.A03CONT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER A01ENTB
//*----------------------------------------------------------------
//SORT1601 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01ENTB SORT A01-ENTR-ETAB-C-ID-PERS ASC ¬2:12¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01ENTR
//SORTIN   DD DISP=SHR,DSN=&SRC.A01ENTR
//SORTOUT  DD DSN=&MIG.ENT16.A01ENTB,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A03CONT
//*----------------------------------------------------------------
//SORT1602 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA03CONT SORT A03-CONTRAT-C-ID-PERS ASC ¬61:12¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03CONTR
//SORTIN   DD DISP=SHR,DSN=&MIG.A03CONTR
//SORTOUT  DD DSN=&MIG.ENT16.A03CONT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(61,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1602       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFENT16  DD DSN=&MIG.ENT16.FENT16,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1603       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRENT16,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRENT16,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRENT16,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01ENTB DD DSN=&MIG.ENT16.A01ENTB.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA03CONT DD DSN=&MIG.ENT16.A03CONT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT16  DD DSN=&MIG.ENT16.FENT16.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRENT16
//*----------------------------------------------------------------
//PPRENT16 EXEC PGM=PPRENT16,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRENT16,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRENT16,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRENT16,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SA01ENTB DD DISP=SHR,DSN=&MIG.ENT16.A01ENTB
//SA03CONT DD DISP=SHR,DSN=&MIG.ENT16.A03CONT
//*--------------<FICHIERS CIBLES>---------------------------------
//SFENT16  DD DSN=&MIG.ENT16.FENT16,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01ENTB DD DSN=&MIG.ENT16.A01ENTB.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA03CONT DD DSN=&MIG.ENT16.A03CONT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT16  DD DSN=&MIG.ENT16.FENT16.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRENT08
//*----------------------------------------------------------------
//DEL0801       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SA03MCON DD DSN=&MIG.ENT08.A03MCON,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA03CONT DD DSN=&MIG.ENT08.A03CONT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFENT16  DD DSN=&MIG.ENT08.FENT16,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER A03MCON
//*----------------------------------------------------------------
//SORT0801 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA03MCON SORT A03-MVT-CONTRAT-C-ID-CONTRAT ASC ¬2:12¦
//* SA03MCON SORT A03-MVT-CONTRAT-D-DEB-EFFET ASC ¬14:8¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03MVTCO
//SORTIN   DD DISP=SHR,DSN=&SRC.A03MVTCO
//SORTOUT  DD DSN=&MIG.ENT08.A03MCON,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A,
               14,8,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A03CONT
//*----------------------------------------------------------------
//SORT0802 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA03CONT SORT A03-CONTRAT-C-ID-CONTRAT ASC ¬2:12¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03CONTR
//SORTIN   DD DISP=SHR,DSN=&MIG.A03CONTR
//SORTOUT  DD DSN=&MIG.ENT08.A03CONT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FENT16
//*----------------------------------------------------------------
//SORT0803 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFENT16 SORT FENT16-C-ID-CONTRAT ASC ¬1:12¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.FENT16
//SORTIN   DD DISP=SHR,DSN=&MIG.ENT16.FENT16
//SORTOUT  DD DSN=&MIG.ENT08.FENT16,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0802       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFENT8   DD DSN=&MIG.ENT08.FENT8,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCHRVIS DD DSN=&MIG.ENT08.FCHRVIS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0803       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRENT08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRENT08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRENT08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCHRVIE DD DSN=&MIG.ENT08.FCHRVIE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFINST   DD DSN=&MIG.ENT08.FINST.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFREG    DD DSN=&MIG.ENT08.FREG.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA03MCON DD DSN=&MIG.ENT08.A03MCON.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA03CONT DD DSN=&MIG.ENT08.A03CONT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT16  DD DSN=&MIG.ENT08.FENT16.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT8   DD DSN=&MIG.ENT08.FENT8.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCHRVIS DD DSN=&MIG.ENT08.FCHRVIS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRENT08
//*----------------------------------------------------------------
//PPRENT08 EXEC PGM=PPRENT08,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRENT08,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRENT08,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRENT08,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//FCHRVIE  DD DISP=SHR,DSN=&MIG.ENT07.FCHRVIE
//FINST    DD DISP=SHR,DSN=&SRC.L01INSTI
//FREG     DD DISP=SHR,DSN=&SRC.L01REGIM
//SA03MCON DD DISP=SHR,DSN=&MIG.ENT08.A03MCON
//SA03CONT DD DISP=SHR,DSN=&MIG.ENT08.A03CONT
//SFENT16  DD DISP=SHR,DSN=&MIG.ENT08.FENT16
//*--------------<FICHIERS CIBLES>---------------------------------
//SFENT8   DD DSN=&MIG.ENT08.FENT8,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFCHRVIS DD DSN=&MIG.ENT08.FCHRVIS,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCHRVIE DD DSN=&MIG.ENT08.FCHRVIE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFINST   DD DSN=&MIG.ENT08.FINST.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFREG    DD DSN=&MIG.ENT08.FREG.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA03MCON DD DSN=&MIG.ENT08.A03MCON.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA03CONT DD DSN=&MIG.ENT08.A03CONT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT16  DD DSN=&MIG.ENT08.FENT16.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT8   DD DSN=&MIG.ENT08.FENT8.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCHRVIS DD DSN=&MIG.ENT08.FCHRVIS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRENT09
//*----------------------------------------------------------------
//DEL0901       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SA01SUIE DD DSN=&MIG.ENT09.A01SUIE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01ENTB DD DSN=&MIG.ENT09.A01ENTB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFENT3   DD DSN=&MIG.ENT09.FENT3,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFENT4   DD DSN=&MIG.ENT09.FENT4,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER A01SUIE
//*----------------------------------------------------------------
//SORT0901 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01SUIE SORT A01-S-ECO-ENTR-C-ID-PERS ASC ¬2:12¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01STENT
//SORTIN   DD DISP=SHR,DSN=&SRC.A01STENT
//SORTOUT  DD DSN=&MIG.ENT09.A01SUIE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01ENTB
//*----------------------------------------------------------------
//SORT0902 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01ENTB SORT A01-ENTR-ETAB-C-ID-PERS ASC ¬2:12¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01ENTR
//SORTIN   DD DISP=SHR,DSN=&SRC.A01ENTR
//*modif pour reel CVL
//*SORTIN   DD DISP=SHR,DSN=&MIG.ENT00.AFENT0
//SORTOUT  DD DSN=&MIG.ENT09.A01ENTB,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FENT3
//*----------------------------------------------------------------
//SORT0903 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFENT3 SORT FENT3-C-ID-PERS ASC ¬21:12¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.FENT3
//SORTIN   DD DISP=SHR,DSN=&MIG.ENT03.FENT3
//SORTOUT  DD DSN=&MIG.ENT09.FENT3,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FENT4
//*----------------------------------------------------------------
//SORT0904 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFENT4 SORT FENT4-C-ID-PERS ASC ¬21:12¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.FENT4
//SORTIN   DD DISP=SHR,DSN=&MIG.ENT04.FENT4
//SORTOUT  DD DSN=&MIG.ENT09.FENT4,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0902       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFENT9   DD DSN=&MIG.ENT09.FENT9,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0903       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRENT09,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRENT09,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRENT09,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01SUIE DD DSN=&MIG.ENT09.A01SUIE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01ENTB DD DSN=&MIG.ENT09.A01ENTB.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT3   DD DSN=&MIG.ENT09.FENT3.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT4   DD DSN=&MIG.ENT09.FENT4.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT9   DD DSN=&MIG.ENT09.FENT9.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRENT09
//*----------------------------------------------------------------
//PPRENT09 EXEC PGM=PPRENT09,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRENT09,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRENT09,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRENT09,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SA01SUIE DD DISP=SHR,DSN=&MIG.ENT09.A01SUIE
//SA01ENTB DD DISP=SHR,DSN=&MIG.ENT09.A01ENTB
//SFENT3   DD DISP=SHR,DSN=&MIG.ENT09.FENT3
//SFENT4   DD DISP=SHR,DSN=&MIG.ENT09.FENT4
//*--------------<FICHIERS CIBLES>---------------------------------
//SFENT9   DD DSN=&MIG.ENT09.FENT9,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01SUIE DD DSN=&MIG.ENT09.A01SUIE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01ENTB DD DSN=&MIG.ENT09.A01ENTB.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT3   DD DSN=&MIG.ENT09.FENT3.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT4   DD DSN=&MIG.ENT09.FENT4.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT9   DD DSN=&MIG.ENT09.FENT9.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRENT10
//*----------------------------------------------------------------
//DEL1001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SA01SUIE DD DSN=&MIG.ENT10.A01SUIE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFENT9   DD DSN=&MIG.ENT10.FENT9,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER A01SUIE
//*----------------------------------------------------------------
//SORT1001 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01SUIE SORT A01-SUITE-ECO-C-ID-SUITE-ECO ASC ¬2:12¦
//* SA01SUIE SORT A01-SUITE-ECO-C-CHAIN-ENTR ASC ¬15:5¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01STECO
//SORTIN   DD DISP=SHR,DSN=&SRC.A01STECO
//SORTOUT  DD DSN=&MIG.ENT18.A01SUIE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FENT9
//*----------------------------------------------------------------
//SORT1802 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFENT9 SORT FENT9-C-ID-SUITE-ECO ASC [13:12—
//* SFENT9 SORT FENT9-C-ID-PERS ASC [1:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.FENT9
//SORTIN   DD DISP=SHR,DSN=&MIG.ENT09.FENT9
//SORTOUT  DD DSN=&MIG.ENT18.FENT9,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(13,12,CH,A,
               1,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1802       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFENT18E DD DSN=&MIG.ENT18.FENT18E,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFENT18B DD DSN=&MIG.ENT18.FENT18B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1803       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRENT18,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRENT18,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRENT18,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01SUIE DD DSN=&MIG.ENT18.A01SUIE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT9   DD DSN=&MIG.ENT18.FENT9.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT18E DD DSN=&MIG.ENT18.FENT18E.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT18B DD DSN=&MIG.ENT18.FENT18B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRENT18
//*----------------------------------------------------------------
//PPRENT18 EXEC PGM=PPRENT18,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRENT18,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRENT18,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRENT18,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SA01SUIE DD DISP=SHR,DSN=&MIG.ENT18.A01SUIE
//SFENT9   DD DISP=SHR,DSN=&MIG.ENT18.FENT9
//*--------------<FICHIERS CIBLES>---------------------------------
//SFENT18E DD DSN=&MIG.ENT18.FENT18E,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFENT18B DD DSN=&MIG.ENT18.FENT18B,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01SUIE DD DSN=&MIG.ENT18.A01SUIE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT9   DD DSN=&MIG.ENT18.FENT9.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT18E DD DSN=&MIG.ENT18.FENT18E.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT18B DD DSN=&MIG.ENT18.FENT18B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRENT19
//*----------------------------------------------------------------
//DEL1901       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFENT18E DD DSN=&MIG.ENT19.FENT18E,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFENT18B DD DSN=&MIG.ENT19.FENT18B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FENT18E
//*----------------------------------------------------------------
//SORT1901 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFENT18E SORT ENT-FENT18-C-ID-SUITE-ECO ASC [13:12—
//* SFENT18E SORT ENT-FENT18-C-ID-PERS ASC [1:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.FENT18
//SORTIN   DD DISP=SHR,DSN=&MIG.ENT18.FENT18E
//SORTOUT  DD DSN=&MIG.ENT19.FENT18E,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(13,12,CH,A,
               1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FENT18B
//*----------------------------------------------------------------
//SORT1902 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFENT18B SORT ETAB-FENT18-C-ID-SUITE-ECO ASC [13:12—
//* SFENT18B SORT ETAB-FENT18-C-ID-PERS ASC [1:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.FENT18
//SORTIN   DD DISP=SHR,DSN=&MIG.ENT18.FENT18B
//SORTOUT  DD DSN=&MIG.ENT19.FENT18B,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(13,12,CH,A,
               1,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1902       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFENT20E DD DSN=&MIG.ENT19.FENT20E,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFENT20B DD DSN=&MIG.ENT19.FENT20B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1903       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRENT19,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRENT19,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRENT19,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT18E DD DSN=&MIG.ENT19.FENT18E.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT18B DD DSN=&MIG.ENT19.FENT18B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT20E DD DSN=&MIG.ENT19.FENT20E.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT20B DD DSN=&MIG.ENT19.FENT20B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRENT19
//*----------------------------------------------------------------
//PPRENT19 EXEC PGM=PPRENT19,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRENT19,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRENT19,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRENT19,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFENT18E DD DISP=SHR,DSN=&MIG.ENT19.FENT18E
//SFENT18B DD DISP=SHR,DSN=&MIG.ENT19.FENT18B
//*--------------<FICHIERS CIBLES>---------------------------------
//SFENT20E DD DSN=&MIG.ENT19.FENT20E,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFENT20B DD DSN=&MIG.ENT19.FENT20B,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT18E DD DSN=&MIG.ENT19.FENT18E.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT18B DD DSN=&MIG.ENT19.FENT18B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT20E DD DSN=&MIG.ENT19.FENT20E.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT20B DD DSN=&MIG.ENT19.FENT20B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRENT10
//*----------------------------------------------------------------
//DEL1001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFENT20E DD DSN=&MIG.ENT10.FENT20E,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFENT20B DD DSN=&MIG.ENT10.FENT20B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FENT20E
//*----------------------------------------------------------------
//SORT1001 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFENT20E SORT ENT-FENT20-P-C-ID-PERS-ENTR ASC [163:12—
//* SFENT20E SORT ENT-FENT20-D-DEB-EFFET ASC [175:8—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.FENT20
//SORTIN   DD DISP=SHR,DSN=&MIG.ENT19.FENT20E
//SORTOUT  DD DSN=&MIG.ENT10.FENT20E,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(163,12,CH,A,175,8,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FENT20B
//*----------------------------------------------------------------
//SORT1002 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFENT20B SORT ETAB-FENT20-P-C-ID-PERS-ENTR ASC [163:12—
//* SFENT20B SORT ETAB-FENT20-D-DEB-EFFET ASC [175:8—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.FENT20
//SORTIN   DD DISP=SHR,DSN=&MIG.ENT19.FENT20B
//SORTOUT  DD DSN=&MIG.ENT10.FENT20B,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(163,12,CH,A,175,8,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STRENVIB DD DSN=&MIG.ENT10.TRENVIB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFENT10  DD DSN=&MIG.ENT10.FENT10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRENT10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRENT10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRENT10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCHRVIE DD DSN=&MIG.ENT10.FCHRVIE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT20E DD DSN=&MIG.ENT10.FENT20E.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT20B DD DSN=&MIG.ENT10.FENT20B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRENVIB DD DSN=&MIG.ENT10.TRENVIB.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT10  DD DSN=&MIG.ENT10.FENT10.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRENT10
//*----------------------------------------------------------------
//PPRENT10 EXEC PGM=PPRENT10,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRENT10,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRENT10,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRENT10,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//FCHRVIE  DD DISP=SHR,DSN=&MIG.ENT08.FCHRVIS
//SFENT20E DD DISP=SHR,DSN=&MIG.ENT10.FENT20E
//SFENT20B DD DISP=SHR,DSN=&MIG.ENT10.FENT20B
//*--------------<FICHIERS CIBLES>---------------------------------
//STRENVIB DD DSN=&MIG.ENT10.TRENVIB,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFENT10  DD DSN=&MIG.ENT10.FENT10,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCHRVIE DD DSN=&MIG.ENT10.FCHRVIE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT20E DD DSN=&MIG.ENT10.FENT20E.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT20B DD DSN=&MIG.ENT10.FENT20B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRENVIB DD DSN=&MIG.ENT10.TRENVIB.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT10  DD DSN=&MIG.ENT10.FENT10.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRENT20
//*----------------------------------------------------------------
//DEL2001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFENT10  DD DSN=&MIG.ENT20.FENT10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFENT18X DD DSN=&MIG.ENT20.FENT18X,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFENT18Y DD DSN=&MIG.ENT20.FENT18Y,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FENT10
//*----------------------------------------------------------------
//SORT2001 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFENT10 SORT TRENVIE-DT-AVIS-DCLA ASC [319:10—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRENVIE
//SORTIN   DD DISP=SHR,DSN=&MIG.ENT10.FENT10
//SORTOUT  DD DSN=&MIG.ENT20.FENT10,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(319,10,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FENT18X
//*----------------------------------------------------------------
//SORT2002 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFENT18X SORT FENT18-D-DEB-EFFET ASC [175:8—
//* SFENT18X SORT FENT18-C-ID-PERS ASC [1:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.FENT18
//SORTIN   DD DISP=SHR,DSN=&MIG.ENT18.FENT18B
//SORTOUT  DD DSN=&MIG.ENT20.FENT18X,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(175,8,CH,A,
               1,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STRENVIB DD DSN=&CIB.TRENVIB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFENT10B DD DSN=&MIG.ENT20.FENT10B,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFENTDB  DD DSN=&CIB.TRENVI3.DOUBLONS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRENT20,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRENT20,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRENT20,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT10  DD DSN=&MIG.ENT20.FENT10.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT18X DD DSN=&MIG.ENT20.FENT18X.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT18Y DD DSN=&MIG.ENT20.FENT18Y.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRENVIB DD DSN=&MIG.ENT20.TRENVIB.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENT10B DD DSN=&MIG.ENT20.FENT10B.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRENT20
//*----------------------------------------------------------------
//PPRENT20 EXEC PGM=PPRENT20,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRENT20,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRENT20,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRENT20,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFENT10  DD DISP=SHR,DSN=&MIG.ENT20.FENT10
//SFENT18X DD DISP=SHR,DSN=&MIG.ENT20.FENT18X
//SFENT18Y DD DISP=SHR,DSN=&MIG.ENT20.FENT18X
//*--------------<FICHIERS CIBLES>---------------------------------
//STRENVIB DD DSN=&CIB.TRENVIB,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFENTDB  DD DSN=&CIB.TRENVI3.DOUBLONS,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFENT10B DD DSN=&MIG.ENT20.FENT10B,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT10  DD DSN=&MIG.ENT20.FENT10.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT18X DD DSN=&MIG.ENT20.FENT18X.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT18Y DD DSN=&MIG.ENT20.FENT18Y.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRENVIB DD DSN=&MIG.ENT20.TRENVIB.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENT10B DD DSN=&MIG.ENT20.FENT10B.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES
//*----------------------------------------------------------------
//DEL4001  EXEC PGM=IEFBR14
//SYSPRINT DD SYSOUT=*
//STRENETB DD DSN=&MIG.ENT40.TRENETB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRENIEB DD DSN=&MIG.ENT40.TRENIEB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TRENETB
//*----------------------------------------------------------------
//SORT4001 EXEC PGM=SORT
//* DEBUT CRITERE XGEN
//* TRENETB SORT TRENETB-ID-TECH-UR-ETAB ASC [1:20]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.ENT03.TRENETB
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRENETB
//SORTOUT  DD DSN=&MIG.ENT40.TRENETB,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TRENIEB
//*----------------------------------------------------------------
//SORT4002 EXEC PGM=SORT
//* DEBUT CRITERE XGEN
//* TRENIEB SORT TRENIEB-ID-TECH-UR-ETAB ASC [1:20]
//* TRENIEB SORT TRENIEB-NO-SEQ DESC [21:4]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TRENIEB
//COPY     DD DISP=SHR,DSN=&CPYCIB.TRENIEB
//SORTOUT  DD DSN=&MIG.ENT40.TRENIEB,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,4,CH,D)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL4002  EXEC PGM=IEFBR14
//SYSPRINT DD SYSOUT=*
//SCRENETB DD DSN=&CIB.TRENETB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRENETB DD DSN=&RED.ENT40.TRENETB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRENIEB DD DSN=&RED.ENT40.TRENIEB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCRENETB DD DSN=&RED.ENT40.CRENETB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL4003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRENT40,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRENT40,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRENT40,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION
//*----------------------------------------------------------------
//PPRENT40 EXEC PGM=PPRENT40
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRENT40,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRENT40,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5000,500),RLSE)
//FICTRACE DD DSN=&TRC.PPRENT40,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5000,500),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DUMMY
//*--------------<FICHIERS SOURCES>--------------------------------
//STRENETB DD DISP=SHR,DSN=&MIG.ENT40.TRENETB
//STRENIEB DD DISP=SHR,DSN=&MIG.ENT40.TRENIEB
//*--------------<FICHIERS CIBLES>---------------------------------
//SCRENETB DD DSN=&CIB.TRENETB,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,500),RLSE)
//*--------------<FICHIERS REDUITS>--------------------------------
//ZTRENETB DD DSN=&RED.ENT40.TRENETB,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(10,50),RLSE)
//ZTRENIEB DD DSN=&RED.ENT40.TRENIEB,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(10,50),RLSE)
//ZCRENETB DD DSN=&RED.ENT40.CRENETB,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(10,50),RLSE)
//*
//*BREF-059 - AJO le 20/08/2013 - Voir J9PPREN3
//*//*----------------------------------------------------------------
//*//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRENT50
//*//*----------------------------------------------------------------
//*//DEL5001       EXEC PGM=IEFBR14,COND=(0,NE)
//*//SYSPRINT DD SYSOUT=*
//*//STRENVIE    DD DSN=&CIB.TRENVIE,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//STRCOMPB    DD DSN=&CIB.TRCOMPB,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//STRCOPRE    DD DSN=&CIB.ENT50.TRCOPRE,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//STRCOPIE    DD DSN=&CIB.TRCOPIE,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*/*
//*//*----------------------------------------------------------------
//*//*CONCATENATION DES 3 FICHIERS TRENVIX
//*//*----------------------------------------------------------------
//*//SORT5001 EXEC PGM=SORT
//*//COPY     DD DISP=SHR,DSN=&CPYCIB.TRENVIE
//*//SORTIN01 DD DISP=SHR,DSN=&MIG.ENT07.FENT7
//*//SORTIN02 DD DISP=SHR,DSN=&MIG.ENT08.FENT8
//*//SORTIN03 DD DISP=SHR,DSN=&MIG.ENT20.FENT10B
//*//SORTOUT  DD DSN=&CIB.TRENVIE,
//*//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//*//            SPACE=(TRK,(5000,500),RLSE)
//*//SYSIN    DD *
//*  SORT FIELDS=COPY
//*/*
//*//*----------------------------------------------------------------
//*//*CONCATENATION DES 2 FICHIERS TRCOMPB
//*//*----------------------------------------------------------------
//*//SORT5002 EXEC PGM=SORT
//*//SYSOUT   DD SYSOUT=*
//*//COPY     DD DISP=SHR,DSN=&CPYCIB.TRCOMPB
//*//SORTIN01 DD DISP=SHR,DSN=&MIG.ENT03.TRCOMPB
//*//SORTIN02 DD DISP=SHR,DSN=&MIG.ENT04.FMPB2
//*//SORTOUT  DD DSN=&CIB.TRCOMPB,
//*//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//*//            SPACE=(TRK,(5000,500),RLSE)
//*//SYSIN    DD *
//*  SORT FIELDS=COPY
//*/*
//*//*----------------------------------------------------------------
//*//*CONCATENATION DES 2 FICHIERS TRCOPRE
//*//*----------------------------------------------------------------
//*//SORT5003 EXEC PGM=SORT
//*//SYSOUT   DD SYSOUT=*
//*//COPY     DD DISP=SHR,DSN=&CPYCIB.TRCOPRE
//*//SORTIN01 DD DISP=SHR,DSN=&MIG.ENT03.TRCOPRE
//*//SORTIN02 DD DISP=SHR,DSN=&MIG.ENT04.FPRE2
//*//SORTOUT  DD DSN=&CIB.TRCOPRE,
//*//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//*//            SPACE=(TRK,(5000,500),RLSE)
//*//SYSIN    DD *
//*  SORT FIELDS=COPY
//*/*
//*//*----------------------------------------------------------------
//*//*CONCATENATION DES 2 FICHIERS TRCOPIE
//*//*----------------------------------------------------------------
//*//SORT5004 EXEC PGM=SORT
//*//SYSOUT   DD SYSOUT=*
//*//COPY     DD DISP=SHR,DSN=&CPYCIB.TRCOPIE
//*//SORTIN01 DD DISP=SHR,DSN=&MIG.ENT03.TRCOPIE
//*//SORTIN02 DD DISP=SHR,DSN=&MIG.ENT04.FPIE2
//*//SORTOUT  DD DSN=&CIB.TRCOPIE,
//*//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//*//            SPACE=(TRK,(5000,500),RLSE)
//*//SYSIN    DD *
//*  SORT FIELDS=COPY
//*/*
//*//*----------------------------------------------------------------
//*//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*//*----------------------------------------------------------------
//*//DEL5002       EXEC PGM=IEFBR14,COND=(0,NE)
//*//SYSPRINT DD SYSOUT=*
//*//SBREFENT DD DSN=&CIB.BREFENT,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//*----------------------------------------------------------------
//*//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*//*----------------------------------------------------------------
//*//DEL5003       EXEC PGM=IEFBR14,COND=(0,NE)
//*//SYSPRINT DD SYSOUT=*
//*//FANO     DD DSN=&ANO.PPRENT50,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//FCPT     DD DSN=&CPT.PPRENT50,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//FTRC     DD DSN=&TRC.PPRENT50,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//ZTRENADH DD DSN=&MIG.ENT50.TRENADH.R,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//ZTRENAPB DD DSN=&MIG.ENT50.TRENAPB.R,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//ZTRENAPE DD DSN=&MIG.ENT50.TRENAPE.R,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//ZTRENCJU DD DSN=&MIG.ENT50.TRENCJU.R,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//ZTRENCVE DD DSN=&MIG.ENT50.TRENCVE.R,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//ZTRENDFT DD DSN=&MIG.ENT50.TRENDFT.R,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//ZTRENENS DD DSN=&MIG.ENT50.TRENENS.R,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//ZTRENENT DD DSN=&MIG.ENT50.TRENENT.R,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//ZTRENETB DD DSN=&MIG.ENT50.TRENETB.R,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//ZTRENFJU DD DSN=&MIG.ENT50.TRENFJU.R,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//ZTRENGRP DD DSN=&MIG.ENT50.TRENGRP.R,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//ZTRENIAB DD DSN=&MIG.ENT50.TRENIAB.R,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//ZTRENIAE DD DSN=&MIG.ENT50.TRENIAE.R,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//ZTRENIEB DD DSN=&MIG.ENT50.TRENIEB.R,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//ZTRENLLO DD DSN=&MIG.ENT50.TRENLLO.R,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//ZTRENRAT DD DSN=&MIG.ENT50.TRENRAT.R,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//ZTRENSGE DD DSN=&MIG.ENT50.TRENSGE.R,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//ZTRENSIG DD DSN=&MIG.ENT50.TRENSIG.R,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//ZTRENSOC DD DSN=&MIG.ENT50.TRENSOC.R,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//ZTRENSOM DD DSN=&MIG.ENT50.TRENSOM.R,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//ZTRENVIB DD DSN=&MIG.ENT50.TRENVIB.R,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//ZTRENVIE DD DSN=&MIG.ENT50.TRENVIE.R,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//ZTRCOADB DD DSN=&MIG.ENT50.TRCOADB.R,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//ZTRCOHAB DD DSN=&MIG.ENT50.TRCOHAB.R,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//ZTRCOMPB DD DSN=&MIG.ENT50.TRCOMPB.R,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//ZTRCOPRE DD DSN=&MIG.ENT50.TRCOPRE.R,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//ZTRCOPIE DD DSN=&CIB.TRCOPIE.R,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//ZBREFENT DD DSN=&MIG.ENT50.BREFENT.R,
//*//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*//*----------------------------------------------------------------
//*//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRENT50
//*//*----------------------------------------------------------------
//*//PPRENT50 EXEC PGM=PPRENT50,COND=(0,NE)
//*//SYSPRINT DD SYSOUT=*
//*//SYSOUT   DD SYSOUT=*
//*//ANOMAL   DD DSN=&ANO.PPRENT50,
//*//            DISP=(NEW,CATLG,DELETE),
//*//            SPACE=(TRK,(5000,500),RLSE)
//*//COMPTEUR DD DSN=&CPT.PPRENT50,
//*//            DISP=(NEW,CATLG,DELETE),
//*//            SPACE=(TRK,(500,50),RLSE)
//*//FICTRACE DD DSN=&TRC.PPRENT50,
//*//            DISP=(NEW,CATLG,DELETE),
//*//            SPACE=(TRK,(500,50),RLSE)
//*//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//*//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*//*--------------<FICHIERS SOURCES>--------------------------------
//*//TRENADH  DD DISP=SHR,DSN=&CIB.TRENADH
//*//TRENAPB  DD DISP=SHR,DSN=&CIB.TRENAPB
//*//TRENAPE  DD DISP=SHR,DSN=&CIB.TRENAPE
//*//TRENCJU  DD DISP=SHR,DSN=&CIB.TRENCJU
//*//TRENCVE  DD DISP=SHR,DSN=&CIB.TRENCVE
//*//TRENDFT  DD DISP=SHR,DSN=&CIB.TRENDFT
//*//TRENENS  DD DISP=SHR,DSN=&CIB.TRENENS
//*//TRENENT  DD DISP=SHR,DSN=&CIB.TRENENT
//*//TRENETB  DD DISP=SHR,DSN=&CIB.TRENETB
//*//TRENFJU  DD DISP=SHR,DSN=&CIB.TRENFJU
//*//TRENGRP  DD DISP=SHR,DSN=&CIB.TRENGRP
//*//TRENIAB  DD DISP=SHR,DSN=&CIB.TRENIAB
//*//TRENIAE  DD DISP=SHR,DSN=&CIB.TRENIAE
//*//TRENIEB  DD DISP=SHR,DSN=&CIB.TRENIEB
//*//TRENLLO  DD DISP=SHR,DSN=&CIB.TRENLLO
//*//TRENRAT  DD DISP=SHR,DSN=&CIB.TRENRAT
//*//TRENSGE  DD DISP=SHR,DSN=&CIB.TRENSGE
//*//TRENSIG  DD DISP=SHR,DSN=&CIB.TRENSIG
//*//TRENSOC  DD DISP=SHR,DSN=&CIB.TRENSOC
//*//TRENSOM  DD DISP=SHR,DSN=&CIB.TRENSOM
//*//TRENVIB  DD DISP=SHR,DSN=&CIB.TRENVIB
//*//TRENVIE  DD DISP=SHR,DSN=&CIB.TRENVIE
//*//* BREF 024 025 remise en ligne des TRENVI GCA 20130605
//*//*TRENVIB  DD DUMMY
//*//*TRENVIE  DD DUMMY
//*//* <== GCA
//*//TRCOADB  DD DISP=SHR,DSN=&CIB.TRCOADB
//*//* BREF-050 20130807 GCA mise en dummy du TRCOHAB ==>
//*//*TRCOHAB  DD DISP=SHR,DSN=&CIB.TRCOHAB
//*//TRCOHAB  DD DUMMY
//*//* BREF-050 20130807 GCA mise en dummy du TRCOHAB <==
//*//TRCOMPB  DD DISP=SHR,DSN=&CIB.TRCOMPB
//*//TRCOPRE  DD DISP=SHR,DSN=&CIB.TRCOPRE
//*//TRCOPIE  DD DISP=SHR,DSN=&CIB.TRCOPIE
//*//*--------------<FICHIERS CIBLES>---------------------------------
//*//SBREFENT DD DSN=&CIB.BREFENT,
//*//            DISP=(NEW,CATLG,CATLG),
//*//            SPACE=(TRK,(200,100),RLSE)
//*//ZTRENADH DD DSN=&MIG.ENT50.TRENADH.R,
//*//            DISP=(NEW,CATLG,CATLG),
//*//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//*//            SPACE=(TRK,(200,100),RLSE)
//*//ZTRENAPB DD DSN=&MIG.ENT50.TRENAPB.R,
//*//            DISP=(NEW,CATLG,CATLG),
//*//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//*//            SPACE=(TRK,(200,100),RLSE)
//*//ZTRENAPE DD DSN=&MIG.ENT50.TRENAPE.R,
//*//            DISP=(NEW,CATLG,CATLG),
//*//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//*//            SPACE=(TRK,(200,100),RLSE)
//*//ZTRENCJU DD DSN=&MIG.ENT50.TRENCJU.R,
//*//            DISP=(NEW,CATLG,CATLG),
//*//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//*//            SPACE=(TRK,(200,100),RLSE)
//*//ZTRENCVE DD DSN=&MIG.ENT50.TRENCVE.R,
//*//            DISP=(NEW,CATLG,CATLG),
//*//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//*//            SPACE=(TRK,(200,100),RLSE)
//*//ZTRENDFT DD DSN=&MIG.ENT50.TRENDFT.R,
//*//            DISP=(NEW,CATLG,CATLG),
//*//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//*//            SPACE=(TRK,(200,100),RLSE)
//*//ZTRENENS DD DSN=&MIG.ENT50.TRENENS.R,
//*//            DISP=(NEW,CATLG,CATLG),
//*//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//*//            SPACE=(TRK,(200,100),RLSE)
//*//ZTRENENT DD DSN=&MIG.ENT50.TRENENT.R,
//*//            DISP=(NEW,CATLG,CATLG),
//*//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//*//            SPACE=(TRK,(200,100),RLSE)
//*//ZTRENETB DD DSN=&MIG.ENT50.TRENETB.R,
//*//            DISP=(NEW,CATLG,CATLG),
//*//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//*//            SPACE=(TRK,(200,100),RLSE)
//*//ZTRENFJU DD DSN=&MIG.ENT50.TRENFJU.R,
//*//            DISP=(NEW,CATLG,CATLG),
//*//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//*//            SPACE=(TRK,(200,100),RLSE)
//*//ZTRENGRP DD DSN=&MIG.ENT50.TRENGRP.R,
//*//            DISP=(NEW,CATLG,CATLG),
//*//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//*//            SPACE=(TRK,(200,100),RLSE)
//*//ZTRENIAB DD DSN=&MIG.ENT50.TRENIAB.R,
//*//            DISP=(NEW,CATLG,CATLG),
//*//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//*//            SPACE=(TRK,(200,100),RLSE)
//*//ZTRENIAE DD DSN=&MIG.ENT50.TRENIAE.R,
//*//            DISP=(NEW,CATLG,CATLG),
//*//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//*//            SPACE=(TRK,(200,100),RLSE)
//*//ZTRENIEB DD DSN=&MIG.ENT50.TRENIEB.R,
//*//            DISP=(NEW,CATLG,CATLG),
//*//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//*//            SPACE=(TRK,(200,100),RLSE)
//*//ZTRENLLO DD DSN=&MIG.ENT50.TRENLLO.R,
//*//            DISP=(NEW,CATLG,CATLG),
//*//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//*//            SPACE=(TRK,(200,100),RLSE)
//*//ZTRENRAT DD DSN=&MIG.ENT50.TRENRAT.R,
//*//            DISP=(NEW,CATLG,CATLG),
//*//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//*//            SPACE=(TRK,(200,100),RLSE)
//*//ZTRENSGE DD DSN=&MIG.ENT50.TRENSGE.R,
//*//            DISP=(NEW,CATLG,CATLG),
//*//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//*//            SPACE=(TRK,(200,100),RLSE)
//*//ZTRENSIG DD DSN=&MIG.ENT50.TRENSIG.R,
//*//            DISP=(NEW,CATLG,CATLG),
//*//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//*//            SPACE=(TRK,(200,100),RLSE)
//*//ZTRENSOC DD DSN=&MIG.ENT50.TRENSOC.R,
//*//            DISP=(NEW,CATLG,CATLG),
//*//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//*//            SPACE=(TRK,(200,100),RLSE)
//*//ZTRENSOM DD DSN=&MIG.ENT50.TRENSOM.R,
//*//            DISP=(NEW,CATLG,CATLG),
//*//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//*//            SPACE=(TRK,(200,100),RLSE)
//*//ZTRENVIB DD DSN=&MIG.ENT50.TRENVIB.R,
//*//            DISP=(NEW,CATLG,CATLG),
//*//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//*//            SPACE=(TRK,(200,100),RLSE)
//*//ZTRENVIE DD DSN=&MIG.ENT50.TRENVIE.R,
//*//            DISP=(NEW,CATLG,CATLG),
//*//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//*//            SPACE=(TRK,(200,100),RLSE)
//*//ZTRCOADB DD DSN=&MIG.ENT50.TRCOADB.R,
//*//            DISP=(NEW,CATLG,CATLG),
//*//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//*//            SPACE=(TRK,(200,100),RLSE)
//*//ZTRCOHAB DD DSN=&MIG.ENT50.TRCOHAB.R,
//*//            DISP=(NEW,CATLG,CATLG),
//*//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//*//            SPACE=(TRK,(200,100),RLSE)
//*//ZTRCOMPB DD DSN=&MIG.ENT50.TRCOMPB.R,
//*//            DISP=(NEW,CATLG,CATLG),
//*//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//*//            SPACE=(TRK,(200,100),RLSE)
//*//ZTRCOPRE DD DSN=&MIG.ENT50.TRCOPRE.R,
//*//            DISP=(NEW,CATLG,CATLG),
//*//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//*//            SPACE=(TRK,(200,100),RLSE)
//*//ZTRCOPIE DD DSN=&CIB.TRCOPIE.R,
//*//            DISP=(NEW,CATLG,CATLG),
//*//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//*//            SPACE=(TRK,(200,100),RLSE)
//*//ZBREFENT DD DSN=&MIG.ENT50.BREFENT.R,
//*//            DISP=(NEW,CATLG,CATLG),
//*//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//*//            SPACE=(TRK,(200,100),RLSE)
//*//*
//*//*
//*//*=========================================
//*//* Conversion en ebcdic des fichiers cibles
//*//*=========================================
//*
//*JOB99  EXEC JCL=JCVENT01
//*
//* FIN DU JCL DE MIGRATION