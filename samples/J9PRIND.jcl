//J9PRIND  JOB UTI00TX0,'PR BREF A01-IND',CLASS=Z,MSGCLASS=X,
//*         RESTART=DEL1002,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=PLURM.MOVE.PR.JCL.PROD
//MEMBER   INCLUDE MEMBER=$$PRT
//*----------------------------------------------------------------
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRIND05
//*----------------------------------------------------------------
//DEL0501       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SINDV    DD DSN=&MIG.IND05.INDV,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SLIAISON DD DSN=&MIG.IND05.LIAISON,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPARTCPT DD DSN=&MIG.IND05.PARTCPT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SJ21DDDN DD DSN=&MIG.IND05.J21DDDN,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SD01CERT DD DSN=&MIG.IND05.D01CERTI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER INDV
//*----------------------------------------------------------------
//SORT0502 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SINDV SORT D01-INDIVIDU-C-ID-PERS ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.D01INDIV
//SORTIN   DD DISP=SHR,DSN=&SRC.D01INDIV
//SORTOUT  DD DSN=&MIG.IND05.INDV,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,UNIT=(SYSDA,20),
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*
//*----------------------------------------------------------------
//*TRI DU FICHIER LIAISON
//*----------------------------------------------------------------
//SORT0503 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SLIAISON SORT A01-LIAISON-C-ID-PERS ASC [19:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01LIAIS
//SORTIN   DD DISP=SHR,DSN=&SRC.A01LIAIS
//SORTOUT  DD DSN=&MIG.IND05.LIAISON,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(19,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER PARTCPT
//*----------------------------------------------------------------
//SORT0504 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SPARTCPT SORT J03-PART-C-ID-PERS ASC [2:12—
//* SPARTCPT SORT J03-PART-D-ETAT-PART DESC [50:8— / FNC 2644 /
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J03PARTI
//SORTIN   DD DISP=SHR,DSN=&SRC.J03PARTI
//SORTOUT  DD DSN=&MIG.IND05.PARTCPT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A,
               50,8,CH,D)
  *MEMSIZE=800000000
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER J21DDDN
//*----------------------------------------------------------------
//SORT0505 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ21DDDN SORT J21-DDR-DNF-C-ID-PERS ASC [16:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J21DDR
//SORTIN   DD DISP=SHR,DSN=&SRC.J21DDR
//SORTOUT  DD DSN=&MIG.IND05.J21DDDN,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER D01CERTI
//*----------------------------------------------------------------
//SORT0506 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SJ21DDDN SORT J21-DDR-DNF-C-ID-PERS ASC [16:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.D01CERTI
//SORTIN   DD DISP=SHR,DSN=&SRC.D01CERTI
//SORTOUT  DD DSN=&MIG.IND05.D01CERTI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0502       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFI1     DD DSN=&MIG.IND05.FI1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0503       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRIND05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRIND05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRIND05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFRIND2  DD DSN=&MIG.IND05.FRIND2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZINDV    DD DSN=&MIG.IND05.INDV.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZLIAISON DD DSN=&MIG.IND05.LIAISON.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZPARTCPT DD DSN=&MIG.IND05.PARTCPT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZJ21DDDN DD DSN=&MIG.IND05.J21DDDN.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFI1     DD DSN=&MIG.IND05.FI1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRIND05
//*----------------------------------------------------------------
//PPRIND05 EXEC PGM=PPRIND05,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRIND05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRIND05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRIND05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SINDV    DD DISP=SHR,DSN=&MIG.IND05.INDV
//SLIAISON DD DISP=SHR,DSN=&MIG.IND05.LIAISON
//SPARTCPT DD DISP=SHR,DSN=&MIG.IND05.PARTCPT
//SJ21DDDN DD DISP=SHR,DSN=&MIG.IND05.J21DDDN
//SD01CERT DD DISP=SHR,DSN=&MIG.IND05.D01CERTI
//*--------------<FICHIERS CIBLES>---------------------------------
//SFI1     DD DSN=&MIG.IND05.FI1,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZINDV    DD DSN=&MIG.IND05.INDV.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZLIAISON DD DSN=&MIG.IND05.LIAISON.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZPARTCPT DD DSN=&MIG.IND05.PARTCPT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZJ21DDDN DD DSN=&MIG.IND05.J21DDDN.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFI1     DD DSN=&MIG.IND05.FI1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRIND10
//* ADH Sans objet pour Prémalliance
//*----------------------------------------------------------------
//DEL1001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SPERAFFI DD DSN=&MIG.IND10.PERAFFI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*SADH     DD DSN=&MIG.IND10.ADH,
//*            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SLIAISON DD DSN=&MIG.IND10.LIAISON,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFI1     DD DSN=&MIG.IND10.FI1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SADRIND  DD DSN=&MIG.IND10.ADRIND,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCERTIF  DD DSN=&MIG.IND10.CERTIF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER PERAFFI
//*----------------------------------------------------------------
//SORT1001 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SPERAFFI SORT J03-PER-AFFIL-C-ID-PERS ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.J03PERAF
//SORTIN   DD DISP=SHR,DSN=&SRC.J03PERAF
//SORTOUT  DD DSN=&MIG.IND10.PERAFFI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FI1
//*----------------------------------------------------------------
//SORT1004 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFI1 SORT D01-INDIVIDU-C-ID-PERS ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.FI1
//SORTIN   DD DISP=SHR,DSN=&MIG.IND05.FI1
//SORTOUT  DD DSN=&MIG.IND10.FI1,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER ADRIND
//*----------------------------------------------------------------
//SORT1005 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SADRIND SORT D01-ADRESSE-IND-C-ID-PERS ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.D01ADRES
//SORTIN   DD DISP=SHR,DSN=&SRC.D01ADRES
//SORTOUT  DD DSN=&MIG.IND10.ADRIND,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER CERTIF
//*----------------------------------------------------------------
//SORT1006 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCERTIF SORT D01-CERTIF-BL-C-ID-PERS ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.D01CERTI
//SORTIN   DD DISP=SHR,DSN=&SRC.D01CERTI
//SORTOUT  DD DSN=&MIG.IND10.CERTIF,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STRININD DD DSN=&CIB.TRININD,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRINROL DD DSN=&CIB.TRINROL,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRINIDA DD DSN=&CIB.TRINIDA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRINIDR DD DSN=&CIB.TRINIDR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRCOADD DD DSN=&CIB.TRCOADD,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPIND1  DD DSN=&CIB.FPIND1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCHADR   DD DSN=&CIB.CHADR1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRIND10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRIND10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRIND10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCPAYS   DD DSN=&MIG.IND10.CPAYS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCCMUN   DD DSN=&MIG.IND10.CCMUN.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZPERAFFI DD DSN=&MIG.IND10.PERAFFI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZLIAISON DD DSN=&MIG.IND10.LIAISON.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*ZADH     DD DSN=&MIG.IND10.ADH.R,
//*            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFI1     DD DSN=&MIG.IND10.FI1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZADRIND  DD DSN=&MIG.IND10.ADRIND.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCERTIF  DD DSN=&MIG.IND10.CERTIF.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRININD DD DSN=&MIG.IND10.TRININD.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRINROL DD DSN=&MIG.IND10.TRINROL.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRINIDA DD DSN=&MIG.IND10.TRINIDA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRINIDR DD DSN=&MIG.IND10.TRINIDR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRCOADD DD DSN=&MIG.IND10.TRCOADD.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPIND1  DD DSN=&MIG.IND10.FPIND1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCHADR   DD DSN=&MIG.IND10.CHADR1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRIND10
//*----------------------------------------------------------------
//PPRIND10 EXEC PGM=PPRIND10,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRIND10,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRIND10,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRIND10,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//CPAYS    DD DISP=SHR,DSN=&TAB.CPAY
//*CCMUN    DD DISP=SHR,DSN=&TAB.CCMUN
//CCMUN    DD DISP=SHR,DSN=&SRC.A01RFCOM
//SPERAFFI DD DISP=SHR,DSN=&MIG.IND10.PERAFFI
//SLIAISON DD DISP=SHR,DSN=&MIG.IND05.LIAISON
//*SADH     DD DISP=SHR,DSN=&MIG.IND10.ADH
//SADH     DD DUMMY
//SFI1     DD DISP=SHR,DSN=&MIG.IND10.FI1
//SADRIND  DD DISP=SHR,DSN=&MIG.IND10.ADRIND
//SCERTIF  DD DISP=SHR,DSN=&MIG.IND10.CERTIF
//*--------------<FICHIERS CIBLES>---------------------------------
//STRININD DD DSN=&CIB.TRININD,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRINROL DD DSN=&CIB.TRINROL,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRINIDA DD DSN=&CIB.TRINIDA,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRINIDR DD DSN=&CIB.TRINIDR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRCOADD DD DSN=&CIB.TRCOADD,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFPIND1  DD DSN=&CIB.FPIND1,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SCHADR   DD DSN=&CIB.CHADR1,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZCPAYS   DD DSN=&MIG.IND10.CPAYS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCCMUN   DD DSN=&MIG.IND10.CCMUN.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZPERAFFI DD DSN=&MIG.IND10.PERAFFI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZLIAISON DD DSN=&MIG.IND10.LIAISON.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*ZADH     DD DSN=&MIG.IND10.ADH.R,
//*            DISP=(NEW,CATLG,CATLG),
//*            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//*            SPACE=(TRK,(200,100),RLSE)
//ZFI1     DD DSN=&MIG.IND10.FI1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZADRIND  DD DSN=&MIG.IND10.ADRIND.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCERTIF  DD DSN=&MIG.IND10.CERTIF.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRININD DD DSN=&MIG.IND10.TRININD.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRINROL DD DSN=&MIG.IND10.TRINROL.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRINIDA DD DSN=&MIG.IND10.TRINIDA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRINIDR DD DSN=&MIG.IND10.TRINIDR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRCOADD DD DSN=&MIG.IND10.TRCOADD.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPIND1  DD DSN=&MIG.IND10.FPIND1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCHADR   DD DSN=&MIG.IND10.CHADR1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
/*
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRIND50
//*----------------------------------------------------------------
//DEL5001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL5002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SBREFRIN DD DSN=&CIB.BREFRIN,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL5003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRIND50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRIND50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRIND50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRININD DD DSN=&MIG.IND50.TRININD.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRINIDR DD DSN=&MIG.IND50.TRINIDR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRINROL DD DSN=&MIG.IND50.TRINROL.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRCOADD DD DSN=&MIG.IND50.TRCOADD.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRINIDA DD DSN=&MIG.IND50.TRINIDA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZBREFRIN DD DSN=&MIG.IND50.BREFRIN.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRIND50
//*----------------------------------------------------------------
//PPRIND50 EXEC PGM=PPRIND50,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRIND50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRIND50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRIND50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//TRININD  DD DISP=SHR,DSN=&CIB.TRININD
//TRINIDR  DD DISP=SHR,DSN=&CIB.TRINIDR
//TRINROL  DD DISP=SHR,DSN=&CIB.TRINROL
//TRCOADD  DD DISP=SHR,DSN=&CIB.TRCOADD
//TRINIDA  DD DISP=SHR,DSN=&CIB.TRINIDA
//*--------------<FICHIERS CIBLES>---------------------------------
//SBREFRIN DD DSN=&CIB.BREFRIN,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRININD DD DSN=&MIG.IND50.TRININD.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRINIDR DD DSN=&MIG.IND50.TRINIDR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRINROL DD DSN=&MIG.IND50.TRINROL.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRCOADD DD DSN=&MIG.IND50.TRCOADD.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRINIDA DD DSN=&MIG.IND50.TRINIDA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZBREFRIN DD DSN=&MIG.IND50.BREFRIN.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*=========================================
//* Conversion en ebcdic des fichiers cibles
//*=========================================
//*
//*JOB99  EXEC JCL=JCVIND01
//*
//* FIN DU JCL DE MIGRATION