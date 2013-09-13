//J9PRAUE  JOB UTI00TX0,'PR UE B03-AUE',CLASS=Z,MSGCLASS=X,
//*          RESTART=DEL2203,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=PLURM.MOVE.PR.JCL.PROD
//MEMBER   INCLUDE MEMBER=$$PRR
//*----------------------------------------------------------------
//* Ajout d'un sum fields=none sur CIB.TAUEPAC
//* Extraire et envoyer les doublons
//*
//* BAC-058 20130905 ==> le fichier source TABMUM renomé en TAUEMUM
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE29
//*----------------------------------------------------------------
//*****
//* ALA DEBUT : CHANGEMENT DES PROG
//*****
//DEL2901  EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STAUEMUM DD DSN=&MIG.AUE29.TAUEMUM,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STABUEM  DD DSN=&MIG.AUE29.TABUEM,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TABMUM
//*----------------------------------------------------------------
//SORT2901 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STABMUM SORT TAUEMUM-ID-TECH-TIER ASC [1:20—
//* STABMUM SORT TAUEMUM-NO-ORD-SEMB ASC [21:5—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TABMUM
//SORTIN   DD DISP=SHR,DSN=&TAB.TABMUM
//SORTOUT  DD DSN=&MIG.AUE29.TABMUM,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,5,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TABUEM
//*----------------------------------------------------------------
//SORT2902 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STABUEM SORT TABUEM-ID-TECH-TIER ASC [1:20—
//* STABUEM SORT TABUEM-NO-ORD-SEMB ASC [21:5—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TABUEM
//SORTIN   DD DISP=SHR,DSN=&TAB.TABUEM
//SORTOUT  DD DSN=&MIG.AUE29.TABUEM,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,5,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2902  EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STAUEMUM DD DSN=&CIB.TAUEMUM,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAUEUEM DD DSN=&CIB.TAUEUEM,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2903       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE29,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE29,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE29,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUEMUM DD DSN=&MIG.AUE29.TAUEMUM.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTABUEM  DD DSN=&MIG.AUE29.TABUEM.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUEMUM DD DSN=&MIG.AUE29.TAUEMUM.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUEUEM DD DSN=&MIG.AUE29.TAUEUEM.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE29
//*----------------------------------------------------------------
//PPRAUE29 EXEC PGM=PPRAUE29,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE29,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE29,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE29,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STABMUM  DD DISP=SHR,DSN=&MIG.AUE29.TABMUM
//STABUEM  DD DISP=SHR,DSN=&MIG.AUE29.TABUEM
//*--------------<FICHIERS CIBLES>---------------------------------
//STAUEMUM DD DSN=&CIB.TAUEMUM,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STAUEUEM DD DSN=&CIB.TAUEUEM,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUEMUM DD DSN=&MIG.AUE29.TAUEMUM.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUEUEM DD DSN=&MIG.AUE29.TAUEUEM.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTABUEM  DD DSN=&MIG.AUE29.TABUEM.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTABMUM DD DSN=&MIG.AUE29.TABMUM.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*****
//* ALA FIN : PROG FAIT PAR TRI
//*****
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE01
//*----------------------------------------------------------------
//DEL0101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STABCC   DD DSN=&MIG.AUE01.TABCC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA03CONT DD DSN=&MIG.AUE01.A03CONT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TABCC
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT,COND=(0,NE)
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TCNTCPI
//SORTIN   DD DISP=SHR,DSN=&CIB.TCNTCPI
//SORTOUT  DD DSN=&MIG.AUE01.TABCC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(55,12,CH,A)
/*
//*
//*----------------------------------------------------------------
//*TRI DU FICHIER A03CONT
//*----------------------------------------------------------------
//SORT0102 EXEC PGM=SORT,COND=(0,NE)
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03CONTR
//SORTIN   DD DISP=SHR,DSN=&MIG.A03CONTR
//SORTOUT  DD DSN=&MIG.AUE01.A03CONT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE01  DD DSN=&MIG.AUE01.FAUE01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA02RCPI DD DSN=&MIG.AUE01.A02RCPI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTABCC   DD DSN=&MIG.AUE01.TABCC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA03CONT DD DSN=&MIG.AUE01.A03CONT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE01  DD DSN=&MIG.AUE01.FAUE01.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE01
//*----------------------------------------------------------------
//PPRAUE01 EXEC PGM=PPRAUE01,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//A02RCPI  DD DISP=SHR,DSN=&SRC.A02RFCPI
//STABCC   DD DISP=SHR,DSN=&MIG.AUE01.TABCC
//SA03CONT DD DISP=SHR,DSN=&MIG.AUE01.A03CONT
//*--------------<FICHIERS CIBLES>---------------------------------
//SFAUE01  DD DSN=&MIG.AUE01.FAUE01,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZA02RCPI DD DSN=&MIG.AUE01.A02RCPI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTABCC   DD DSN=&MIG.AUE01.TABCC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA03CONT DD DSN=&MIG.AUE01.A03CONT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE01  DD DSN=&MIG.AUE01.FAUE01.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE02
//*----------------------------------------------------------------
//DEL0201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE01  DD DSN=&MIG.AUE02.FAUE01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STABADH  DD DSN=&MIG.AUE02.TABADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FAUE01
//*----------------------------------------------------------------
//SORT0201 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUE01 SORT FAUE01-ID-TECH-UR-ADHR ASC [1:20—
//* SFAUE01 SORT FAUE01-NO-ORD-ADHS ASC [21:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FAUE01
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE01.FAUE01
//SORTOUT  DD DSN=&MIG.AUE02.FAUE01,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TABADH
//*----------------------------------------------------------------
//SORT0202 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STABADH SORT ADHSADH-ID-TECH-UR-ADHR ASC [21:20—
//* STABADH SORT ADHSADH-NO-ORD-ADHS ASC [41:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.TADHADH
//SORTIN   DD DISP=SHR,DSN=&CIB.TADHADH
//SORTOUT  DD DSN=&MIG.AUE02.TABADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,20,CH,A,
               41,3,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0202       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE02  DD DSN=&MIG.AUE02.FAUE02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE01  DD DSN=&MIG.AUE02.FAUE01.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTABADH  DD DSN=&MIG.AUE02.TABADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE02  DD DSN=&MIG.AUE02.FAUE02.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE02
//*----------------------------------------------------------------
//PPRAUE02 EXEC PGM=PPRAUE02,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFAUE01  DD DISP=SHR,DSN=&MIG.AUE02.FAUE01
//STABADH  DD DISP=SHR,DSN=&MIG.AUE02.TABADH
//*--------------<FICHIERS CIBLES>---------------------------------
//SFAUE02  DD DSN=&MIG.AUE02.FAUE02,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE01  DD DSN=&MIG.AUE02.FAUE01.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTABADH  DD DSN=&MIG.AUE02.TABADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE02  DD DSN=&MIG.AUE02.FAUE02.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE03
//*----------------------------------------------------------------
//DEL0301       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE02  DD DSN=&MIG.AUE03.FAUE02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA03POPC DD DSN=&MIG.AUE03.A03POPC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA03ETBC DD DSN=&MIG.AUE03.A03ETBC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STCNCTR  DD DSN=&MIG.AUE03.TCNCTR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FAUE02
//*----------------------------------------------------------------
//SORT0301 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUE02 SORT FAUE02-FAUE01-C-ID-CONTRAT ASC [44:12—
//* SFAUE02 SORT FAUE02-FAUE01-C-CPI ASC [59:17—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FAUE02
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE02.FAUE02
//SORTOUT  DD DSN=&MIG.AUE03.FAUE02,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(44,12,CH,A,
               59,17,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A03POPC
//*----------------------------------------------------------------
//SORT0302 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA03POPC SORT A03-POPUL-CONTR-C-ID-CONTRAT ASC [2:12—
//* SA03POPC SORT A03-POPUL-CONTR-C-CPI ASC [14:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03POPCO
//SORTIN   DD DISP=SHR,DSN=&SRC.A03POPCO
//SORTOUT  DD DSN=&MIG.AUE03.A03POPC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A,
               14,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A03ETBC
//*----------------------------------------------------------------
//SORT0303 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA03ETBC SORT A03-ETAB-CONTR-C-ID-CONTRAT ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03ETABC
//SORTIN   DD DISP=SHR,DSN=&SRC.A03ETABC
//SORTOUT  DD DSN=&MIG.AUE03.A03ETBC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TCNCTR
//*----------------------------------------------------------------
//SORT0304 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STCNCTR SORT CNTCTR-C-ID-CONTRAT ASC [155:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.TCNTCTR
//SORTIN   DD DISP=SHR,DSN=&CIB.TCNTCTR
//SORTOUT  DD DSN=&MIG.AUE03.TCNCTR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(155,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0302       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE03  DD DSN=&MIG.AUE03.FAUE03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFAUE29  DD DSN=&MIG.AUE03.FAUE29,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0303       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE02  DD DSN=&MIG.AUE03.FAUE02.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA03CON  DD DSN=&MIG.AUE03.A03CON.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA03POPC DD DSN=&MIG.AUE03.A03POPC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA03ETBC DD DSN=&MIG.AUE03.A03ETBC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTCNCTR  DD DSN=&MIG.AUE03.TCNCTR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE03  DD DSN=&MIG.AUE03.FAUE03.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE29  DD DSN=&MIG.AUE03.FAUE29.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE03
//*----------------------------------------------------------------
//PPRAUE03 EXEC PGM=PPRAUE03,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFAUE02  DD DISP=SHR,DSN=&MIG.AUE03.FAUE02
//SA03CON  DD DISP=SHR,DSN=&MIG.AUE01.A03CONT
//SA03POPC DD DISP=SHR,DSN=&MIG.AUE03.A03POPC
//SA03ETBC DD DISP=SHR,DSN=&MIG.AUE03.A03ETBC
//STCNCTR  DD DISP=SHR,DSN=&MIG.AUE03.TCNCTR
//*--------------<FICHIERS CIBLES>---------------------------------
//SFAUE03  DD DSN=&MIG.AUE03.FAUE03,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFAUE29  DD DSN=&MIG.AUE03.FAUE29,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE02  DD DSN=&MIG.AUE03.FAUE02.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA03CON  DD DSN=&MIG.AUE03.A03CON.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA03POPC DD DSN=&MIG.AUE03.A03POPC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA03ETBC DD DSN=&MIG.AUE03.A03ETBC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTCNCTR  DD DSN=&MIG.AUE03.TCNCTR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE03  DD DSN=&MIG.AUE03.FAUE03.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE29  DD DSN=&MIG.AUE03.FAUE29.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE04
//*----------------------------------------------------------------
//DEL0401       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE29  DD DSN=&MIG.AUE04.FAUE29,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPENT2  DD DSN=&MIG.AUE04.FPENT2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FAUE29
//*----------------------------------------------------------------
//SORT0401 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUE29 SORT FAUE29-C-ID-PERS ASC [355:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FAUE29
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE03.FAUE29
//SORTOUT  DD DSN=&MIG.AUE04.FAUE29,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(355,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FPENT2
//*----------------------------------------------------------------
//SORT0402 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFPENT2 SORT FPENT2-C-ID-PERS ASC [21:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.FPENT2
//SORTIN   DD DISP=SHR,DSN=&CIB.FPENT2
//SORTOUT  DD DSN=&MIG.AUE04.FPENT2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0402       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE04  DD DSN=&MIG.AUE04.FAUE04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0403       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE29  DD DSN=&MIG.AUE04.FAUE29.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPENT2  DD DSN=&MIG.AUE04.FPENT2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE04  DD DSN=&MIG.AUE04.FAUE04.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE04
//*----------------------------------------------------------------
//PPRAUE04 EXEC PGM=PPRAUE04,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFAUE29  DD DISP=SHR,DSN=&MIG.AUE04.FAUE29
//SFPENT2  DD DISP=SHR,DSN=&MIG.AUE04.FPENT2
//*--------------<FICHIERS CIBLES>---------------------------------
//SFAUE04  DD DSN=&MIG.AUE04.FAUE04,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE29  DD DSN=&MIG.AUE04.FAUE29.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPENT2  DD DSN=&MIG.AUE04.FPENT2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE04  DD DSN=&MIG.AUE04.FAUE04.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE30
//*----------------------------------------------------------------
//DEL3001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SCATCPI  DD DSN=&MIG.AUE30.CATCPI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER CATCPI
//*----------------------------------------------------------------
//SORT3001 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCATCPI SORT CATCPI-C-ID-CONTRAT ASC [134:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TCATCPI
//SORTIN   DD DISP=SHR,DSN=&CIB.TCATCPI
//SORTOUT  DD DSN=&MIG.AUE30.CATCPI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(134,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE30  DD DSN=&MIG.AUE30.FAUE30,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE30,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE30,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE30,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA02RCPI DD DSN=&MIG.AUE30.A02RCPI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCATCPI  DD DSN=&MIG.AUE30.CATCPI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA03CONT DD DSN=&MIG.AUE30.A03CONT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE30  DD DSN=&MIG.AUE30.FAUE30.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE30
//*----------------------------------------------------------------
//PPRAUE30 EXEC PGM=PPRAUE30,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE30,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE30,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE30,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//A02RCPI  DD DISP=SHR,DSN=&SRC.A02RFCPI
//SCATCPI  DD DISP=SHR,DSN=&MIG.AUE30.CATCPI
//SA03CONT DD DISP=SHR,DSN=&MIG.AUE01.A03CONT
//*--------------<FICHIERS CIBLES>---------------------------------
//SFAUE30  DD DSN=&MIG.AUE30.FAUE30,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZA02RCPI DD DSN=&MIG.AUE30.A02RCPI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCATCPI  DD DSN=&MIG.AUE30.CATCPI.R,
//            DISP=(NEW,CATLG,CATLG)C,
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA03CONT DD DSN=&MIG.AUE30.A03CONT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE30  DD DSN=&MIG.AUE30.FAUE30.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE17
//*----------------------------------------------------------------
//DEL1701       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STABCAC  DD DSN=&MIG.AUE17.TABCAC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFAUE01  DD DSN=&MIG.AUE17.FAUE01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFAUE30  DD DSN=&MIG.AUE17.FAUE30,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TABCAC
//*----------------------------------------------------------------
//SORT1701 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STABCAC SORT CATCPI-ID-TECH-UR-ADHR ASC [21:20—
//* STABCAC SORT CATCPI-NO-ORD-ADHS ASC [41:3—
//* STABCAC SORT CATCPI-NO-ORD-CPRO ASC [64:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TCATCPI
//SORTIN   DD DISP=SHR,DSN=&CIB.TCATCPI
//SORTOUT  DD DSN=&MIG.AUE17.TABCAC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,20,CH,A,
               41,3,CH,A,
               64,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FAUE01
//*----------------------------------------------------------------
//SORT1702 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUE01 SORT FAUE01-ID-TECH-UR-ADHR ASC [1:20—
//* SFAUE01 SORT FAUE01-NO-ORD-ADHS ASC [21:3—
//* SFAUE01 SORT FAUE01-NO-ORD-CPRO ASC [27:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FAUE01
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE01.FAUE01
//SORTOUT  DD DSN=&MIG.AUE17.FAUE01,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               27,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FAUE30
//*----------------------------------------------------------------
//SORT1703 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUE30 SORT FAUE30-ID-TECH-UR-ADHR ASC [1:20—
//* SFAUE30 SORT FAUE30-NO-ORD-ADHS ASC [21:3—
//* SFAUE30 SORT FAUE30-NO-ORD-CPRO ASC [24:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FAUE30
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE30.FAUE30
//SORTOUT  DD DSN=&MIG.AUE17.FAUE30,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               24,3,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1702       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STAUEPOP DD DSN=&CIB.TAUEPOP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STPOPCPI DD DSN=&CIB.TPOPCPI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAUERPD DD DSN=&CIB.TAUERPD,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFAUE17  DD DSN=&MIG.AUE17.FAUE17,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1703       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE17,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE17,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE17,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTCPICP  DD DSN=&MIG.AUE17.TCPICP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTABCAC  DD DSN=&MIG.AUE17.TABCAC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE01  DD DSN=&MIG.AUE17.FAUE01.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE30  DD DSN=&MIG.AUE17.FAUE30.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUEPOP DD DSN=&MIG.AUE17.TAUEPOP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTPOPCPI DD DSN=&MIG.AUE17.TPOPCPI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUERPD DD DSN=&MIG.AUE17.TAUERPD.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE17  DD DSN=&MIG.AUE17.FAUE17.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE17
//*----------------------------------------------------------------
//PPRAUE17 EXEC PGM=PPRAUE17,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE17,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE17,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE17,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//TCPICP   DD DISP=SHR,DSN=&TAB.TCPICPN
//STABCAC  DD DISP=SHR,DSN=&MIG.AUE17.TABCAC
//SFAUE01  DD DISP=SHR,DSN=&MIG.AUE17.FAUE01
//SFAUE30  DD DISP=SHR,DSN=&MIG.AUE17.FAUE30
//*--------------<FICHIERS CIBLES>---------------------------------
//STAUEPOP DD DSN=&CIB.TAUEPOP,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STPOPCPI DD DSN=&CIB.TPOPCPI,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STAUERPD DD DSN=&CIB.TAUERPD,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFAUE17  DD DSN=&MIG.AUE17.FAUE17,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTCPICP  DD DSN=&MIG.AUE17.TCPICP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTABCAC  DD DSN=&MIG.AUE17.TABCAC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE01  DD DSN=&MIG.AUE17.FAUE01.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE30  DD DSN=&MIG.AUE17.FAUE30.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUEPOP DD DSN=&MIG.AUE17.TAUEPOP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTPOPCPI DD DSN=&MIG.AUE17.TPOPCPI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUERPD DD DSN=&MIG.AUE17.TAUERPD.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE17  DD DSN=&MIG.AUE17.FAUE17.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE18
//*----------------------------------------------------------------
//DEL1801       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE17  DD DSN=&MIG.AUE18.FAUE17,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFAUE03  DD DSN=&MIG.AUE18.FAUE03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFAUE04  DD DSN=&MIG.AUE18.FAUE04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FAUE17
//*----------------------------------------------------------------
//SORT1801 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUE17 SORT FAUE17-ID-TECH-UR-ADHR ASC [13:20—
//* SFAUE17 SORT FAUE17-NO-ORD-ADHS ASC [10:3—
//* SFAUE17 SORT FAUE17-NO-ORD-CPRO ASC [33:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FAUE17
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE17.FAUE17
//SORTOUT  DD DSN=&MIG.AUE18.FAUE17,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(13,20,CH,A,
               10,3,CH,A,
               33,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FAUE03
//*----------------------------------------------------------------
//SORT1802 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUE03 SORT FAUE03-FAUE01-ID-TECH-UR-ADHR ASC [1:20—
//* SFAUE03 SORT FAUE03-FAUE01-NO-ORD-ADHS ASC [21:3—
//* SFAUE03 SORT FAUE03-FAUE01-NO-ORD-CPRO ASC [27:3—
//* SFAUE03 SORT FAUE03-FAUE01-C-ID-CONTRAT ASC [44:12—
//* SFAUE03 SORT FAUE03-FAUE01-C-CPI ASC [59:17—
//* SFAUE03 SORT FAUE03-D-DEB-EFFET-PC ASC [394:8—
//* SFAUE03 SORT FAUE03-D-FIN-EFFET-PC ASC [402:8—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FAUE03
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE03.FAUE03
//SORTOUT  DD DSN=&MIG.AUE18.FAUE03,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               27,3,CH,A,
               44,12,CH,A,
               59,17,CH,A,
               394,8,CH,A,
               402,8,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FAUE04
//*----------------------------------------------------------------
//SORT1803 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUE04 SORT FAUE04-FAUE01-ID-TECH-UR-ADHR ASC [1:20—
//* SFAUE04 SORT FAUE04-FAUE01-NO-ORD-ADHS ASC [21:3—
//* SFAUE04 SORT FAUE04-FAUE01-NO-ORD-CPRO ASC [27:3—
//* SFAUE04 SORT FAUE04-FAUE01-C-ID-CONTRAT ASC [44:12—
//* SFAUE04 SORT FAUE04-C-ID-PERS ASC [355:12—
//* SFAUE04 SORT FAUE04-D-DEB-EFFET-EC ASC [367:8—
//* SFAUE04 SORT FAUE04-D-FIN-EFFET-EC ASC [375:8—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FAUE04
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE04.FAUE04
//SORTOUT  DD DSN=&MIG.AUE18.FAUE04,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               27,3,CH,A,
               44,12,CH,A,
               355,12,CH,A,
               367,8,CH,A,
               375,8,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1802       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE18  DD DSN=&MIG.AUE18.FAUE18,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1803       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE18,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE18,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE18,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE17  DD DSN=&MIG.AUE18.FAUE17.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE03  DD DSN=&MIG.AUE18.FAUE03.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE04  DD DSN=&MIG.AUE18.FAUE04.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE18  DD DSN=&MIG.AUE18.FAUE18.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE18
//*----------------------------------------------------------------
//PPRAUE18 EXEC PGM=PPRAUE18,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE18,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE18,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE18,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFAUE17  DD DISP=SHR,DSN=&MIG.AUE18.FAUE17
//SFAUE03  DD DISP=SHR,DSN=&MIG.AUE18.FAUE03
//SFAUE04  DD DISP=SHR,DSN=&MIG.AUE18.FAUE04
//*--------------<FICHIERS CIBLES>---------------------------------
//SFAUE18  DD DSN=&MIG.AUE18.FAUE18,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE17  DD DSN=&MIG.AUE18.FAUE17.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE03  DD DSN=&MIG.AUE18.FAUE03.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE04  DD DSN=&MIG.AUE18.FAUE04.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE18  DD DSN=&MIG.AUE18.FAUE18.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE19
//*----------------------------------------------------------------
//DEL1901       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE18  DD DSN=&MIG.AUE19.FAUE18,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FAUE18
//*----------------------------------------------------------------
//SORT1901 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUE18 SORT FAUE18-ID-TECH-UR-ADHR ASC [1:20—
//* SFAUE18 SORT FAUE18-NO-ORD-ADHS ASC [21:3—
//* SFAUE18 SORT FAUE18-NO-ORD-CPRO ASC [24:3—
//* SFAUE18 SORT FAUE18-CPI ASC [36:3—
//* SFAUE18 SORT FAUE18-ID-PERS ASC [51:12—
//* SFAUE18 SORT FAUE18-DEB-CHEVMT ASC [63:8—
//* SFAUE18 SORT FAUE18-FIN-CHEVMT ASC [71:8—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FAUE18
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE18.FAUE18
//SORTOUT  DD DSN=&MIG.AUE19.FAUE18,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               24,3,CH,A,
               36,3,CH,A,
               51,12,CH,A,
               63,8,CH,A,
               71,8,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1902       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//*STAUEREP DD DSN=&CIB.TAUEREP,
//STAUEREP DD DSN=&MIG.AUE19.TAUEREP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1903       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE19,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE19,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE19,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE18  DD DSN=&MIG.AUE19.FAUE18.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUEREP DD DSN=&MIG.AUE19.TAUEREP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE19
//*----------------------------------------------------------------
//PPRAUE19 EXEC PGM=PPRAUE19,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE19,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE19,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE19,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFAUE18  DD DISP=SHR,DSN=&MIG.AUE19.FAUE18
//*--------------<FICHIERS CIBLES>---------------------------------
//*STAUEREP DD DSN=&CIB.TAUEREP,
//STAUEREP DD DSN=&MIG.AUE19.TAUEREP,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE18  DD DSN=&MIG.AUE19.FAUE18.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUEREP DD DSN=&MIG.AUE19.TAUEREP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE39
//*----------------------------------------------------------------
//DEL3901       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUEREP DD DSN=&MIG.AUE39.FAUEREP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPENT7  DD DSN=&MIG.AUE39.FPENT7,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FAUEREP
//*----------------------------------------------------------------
//SORT3901 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUEREP SORT TAUEREP-ID-TECH-UR-ETAB ASC [10:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAUEREP
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE19.TAUEREP
//SORTOUT  DD DSN=&MIG.AUE39.FAUEREP,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(10,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FPENT7
//*----------------------------------------------------------------
//SORT3902 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFPENT7 SORT FPENT7-ID-TECH-UR-ETAB ASC [1:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FPENT7
//SORTIN   DD DISP=SHR,DSN=&CIB.FPENT7
//SORTOUT  DD DSN=&MIG.AUE39.FPENT7,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3902       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STAUEREP DD DSN=&CIB.TAUEREP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3903       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE39,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE39,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE39,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUEREP DD DSN=&MIG.AUE39.FAUEREP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPENT7  DD DSN=&MIG.AUE39.FPENT7.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUEREP DD DSN=&MIG.AUE39.TAUEREP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE39
//*----------------------------------------------------------------
//PPRAUE39 EXEC PGM=PPRAUE39,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE39,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE39,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE39,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFAUEREP DD DISP=SHR,DSN=&MIG.AUE39.FAUEREP
//SFPENT7  DD DISP=SHR,DSN=&MIG.AUE39.FPENT7
//*--------------<FICHIERS CIBLES>---------------------------------
//STAUEREP DD DSN=&CIB.TAUEREP,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUEREP DD DSN=&MIG.AUE39.FAUEREP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPENT7  DD DSN=&MIG.AUE39.FPENT7.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUEREP DD DSN=&MIG.AUE39.TAUEREP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE20
//*----------------------------------------------------------------
//DEL2001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE17  DD DSN=&MIG.AUE20.FAUE17,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFAUE03  DD DSN=&MIG.AUE20.FAUE03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FAUE17
//*----------------------------------------------------------------
//SORT2001 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUE17 SORT FAUE17-ID-TECH-UR-ADHR ASC [13:20—
//* SFAUE17 SORT FAUE17-NO-ORD-ADHS ASC [10:3—
//* SFAUE17 SORT FAUE17-NO-ORD-CPRO ASC [33:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FAUE17
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE17.FAUE17
//SORTOUT  DD DSN=&MIG.AUE20.FAUE17,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(13,20,CH,A,
               10,3,CH,A,
               33,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FAUE03
//*----------------------------------------------------------------
//SORT2002 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUE03 SORT FAUE03-FAUE01-ID-TECH-UR-ADHR ASC [1:20—
//* SFAUE03 SORT FAUE03-FAUE01-NO-ORD-ADHS ASC [21:3—
//* SFAUE03 SORT FAUE03-FAUE01-NO-ORD-CPRO ASC [27:3—
//* SFAUE03 SORT FAUE03-FAUE01-C-CPI ASC [71:17—
//* SFAUE03 SORT FAUE03-FAUE01-C-ID-CONTRAT ASC [56:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FAUE03
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE03.FAUE03
//SORTOUT  DD DSN=&MIG.AUE20.FAUE03,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               27,3,CH,A,
               71,17,CH,A,
               56,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STAUEECA DD DSN=&CIB.TAUEECA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE20,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE20,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE20,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTART36  DD DSN=&MIG.AUE20.TART36.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRENZC1 DD DSN=&MIG.AUE20.TRENZC1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE17  DD DSN=&MIG.AUE20.FAUE17.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE03  DD DSN=&MIG.AUE20.FAUE03.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUEECA DD DSN=&MIG.AUE20.TAUEECA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE20
//*----------------------------------------------------------------
//PPRAUE20 EXEC PGM=PPRAUE20,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE20,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE20,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE20,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//TART36   DD DISP=SHR,DSN=&TAB.TART36
//TRENZC1  DD DISP=SHR,DSN=&TAB.TRENZC1
//SFAUE17  DD DISP=SHR,DSN=&MIG.AUE20.FAUE17
//SFAUE03  DD DISP=SHR,DSN=&MIG.AUE20.FAUE03
//*--------------<FICHIERS CIBLES>---------------------------------
//STAUEECA DD DSN=&CIB.TAUEECA,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTART36  DD DSN=&MIG.AUE20.TART36.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRENZC1 DD DSN=&MIG.AUE20.TRENZC1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE17  DD DSN=&MIG.AUE20.FAUE17.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE03  DD DSN=&MIG.AUE20.FAUE03.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUEECA DD DSN=&MIG.AUE20.TAUEECA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE05
//*----------------------------------------------------------------
//DEL0501       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SA03ADH  DD DSN=&MIG.AUE05.A03ADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA03ETAD DD DSN=&MIG.AUE05.A03ETAD,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFAUE17  DD DSN=&MIG.AUE05.FAUE17,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER A03ADH
//*----------------------------------------------------------------
//SORT0501 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA03ADH SORT A03-ADHERENT-C-ID-ADH ASC [34:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03ADHER
//SORTIN   DD DISP=SHR,DSN=&MIG.A03ADHER
//*SORTIN   DD DISP=SHR,DSN=&MIG.A3ADHER
//SORTOUT  DD DSN=&MIG.AUE05.A03ADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(34,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A03ETAD
//*----------------------------------------------------------------
//SORT0502 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA03ETAD SORT A03-ETAT-ADHERENT-C-ID-ADH ASC [16:12—
//* SA03ETAD SORT A03-ADH-D-DEB-EFFET ASC [116:8—
//* SA03ETAD SORT A03-ADH-D-CREATION DESC +135:8]
//* SA03ETAD SORT A03-ADH-C-ID-ETAT DESC +2:12]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03ETADH
//SORTIN   DD DISP=SHR,DSN=&SRC.A03ETADH
//SORTOUT  DD DSN=&MIG.AUE05.A03ETAD,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,12,CH,A,
               116,8,CH,A,
               135,8,CH,D,
               2,12,CH,D)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A03ADH
//*----------------------------------------------------------------
//SORT0503 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUE17 SORT FAUE17-C-ID-ADH ASC [64:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FAUE17
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE17.FAUE17
//SORTOUT  DD DSN=&MIG.AUE05.FAUE17,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(64,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0502       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE05  DD DSN=&MIG.AUE05.FAUE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0503       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA03ADH  DD DSN=&MIG.AUE05.A03ADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA03ETAD DD DSN=&MIG.AUE05.A03ETAD.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE17  DD DSN=&MIG.AUE05.FAUE17.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE05  DD DSN=&MIG.AUE05.FAUE05.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE05
//*----------------------------------------------------------------
//PPRAUE05 EXEC PGM=PPRAUE05,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SA03ADH  DD DISP=SHR,DSN=&MIG.AUE05.A03ADH
//SA03ETAD DD DISP=SHR,DSN=&MIG.AUE05.A03ETAD
//SFAUE17  DD DISP=SHR,DSN=&MIG.AUE05.FAUE17
//*--------------<FICHIERS CIBLES>---------------------------------
//SFAUE05  DD DSN=&MIG.AUE05.FAUE05,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZA03ADH  DD DSN=&MIG.AUE05.A03ADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA03ETAD DD DSN=&MIG.AUE05.A03ETAD.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE17  DD DSN=&MIG.AUE05.FAUE17.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE05  DD DSN=&MIG.AUE05.FAUE05.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE06
//*----------------------------------------------------------------
//DEL0601       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SA03CTR  DD DSN=&MIG.AUE06.A03CTR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFAUE17  DD DSN=&MIG.AUE06.FAUE17,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER A03CTR
//*----------------------------------------------------------------
//SORT0601 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA03CTR SORT A03-CONTRAT-C-ID-PERS ASC [61:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03CONTR
//SORTIN   DD DISP=SHR,DSN=&MIG.A03CONTR
//SORTOUT  DD DSN=&MIG.AUE06.A03CTR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(61,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FAUE17
//*----------------------------------------------------------------
//SORT0601 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUE17 SORT FAUE17-C-ID-PERS      ASC [36:12—
//* SFAUE17 SORT FAUE17-ID-POP         ASC [1:9—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FAUE17
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE17.FAUE17
//SORTOUT  DD DSN=&MIG.AUE06.FAUE17,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(36,12,CH,A,
               1,9,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0602       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE06  DD DSN=&MIG.AUE06.FAUE06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0603       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA03CTR  DD DSN=&MIG.AUE06.A03CTR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE17  DD DSN=&MIG.AUE06.FAUE17.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE06  DD DSN=&MIG.AUE06.FAUE06.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE06
//*----------------------------------------------------------------
//PPRAUE06 EXEC PGM=PPRAUE06,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE06,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE06,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE06,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SA03CTR  DD DISP=SHR,DSN=&MIG.AUE06.A03CTR
//SFAUE17  DD DISP=SHR,DSN=&MIG.AUE06.FAUE17
//*--------------<FICHIERS CIBLES>---------------------------------
//SFAUE06  DD DSN=&MIG.AUE06.FAUE06,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZA03CTR  DD DSN=&MIG.AUE06.A03CTR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE17  DD DSN=&MIG.AUE06.FAUE17.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE06  DD DSN=&MIG.AUE06.FAUE06.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE07
//*----------------------------------------------------------------
//DEL0701       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SA03ETCT DD DSN=&MIG.AUE07.A03ETCT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFAUE17  DD DSN=&MIG.AUE07.FAUE17,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A03ETCT
//*----------------------------------------------------------------
//SORT0701 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA03ETCT SORT A03-ETAT-CONTRAT-C-ID-CONTRAT ASC [16:12—
//* SA03ETCT SORT A03-ETAT-CONTRAT-D-DEB-EFFET ASC [36:8—
//* SA03ETCT SORT A03-ETAT-CONTRAT-C-ID-ETAT DESC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03ETCON
//SORTIN   DD DISP=SHR,DSN=&SRC.A03ETCON
//SORTOUT  DD DSN=&MIG.AUE07.A03ETCT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,12,CH,A,
               36,8,CH,A,
               2,12,CH,D)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FAUE17
//*----------------------------------------------------------------
//SORT0702 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUE17 SORT FAUE17-C-ID-CONTRAT      ASC [76:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FAUE17
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE17.FAUE17
//SORTOUT  DD DSN=&MIG.AUE07.FAUE17,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(76,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0702       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE07  DD DSN=&MIG.AUE07.FAUE07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0703       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA03CTR  DD DSN=&MIG.AUE07.A03CTR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA03ETCT DD DSN=&MIG.AUE07.A03ETCT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE17  DD DSN=&MIG.AUE07.FAUE17.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE07  DD DSN=&MIG.AUE07.FAUE07.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE07
//*----------------------------------------------------------------
//PPRAUE07 EXEC PGM=PPRAUE07,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE07,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE07,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE07,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SA03CTR  DD DISP=SHR,DSN=&MIG.AUE01.A03CONT
//SA03ETCT DD DISP=SHR,DSN=&MIG.AUE07.A03ETCT
//SFAUE17  DD DISP=SHR,DSN=&MIG.AUE07.FAUE17
//*--------------<FICHIERS CIBLES>---------------------------------
//SFAUE07  DD DSN=&MIG.AUE07.FAUE07,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZA03CTR  DD DSN=&MIG.AUE07.A03CTR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA03ETCT DD DSN=&MIG.AUE07.A03ETCT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE17  DD DSN=&MIG.AUE07.FAUE17.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE07  DD DSN=&MIG.AUE07.FAUE07.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE08
//*----------------------------------------------------------------
//DEL0801       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFPENT4  DD DSN=&MIG.AUE08.FPENT4,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFAUE05  DD DSN=&MIG.AUE08.FAUE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFAUE07  DD DSN=&MIG.AUE08.FAUE07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FPENT4
//*----------------------------------------------------------------
//SORT0801 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFPENT4 SORT FPENT4-C-ID-PERS ASC [1:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FPENT4
//SORTIN   DD DISP=SHR,DSN=&CIB.FPENT4
//SORTOUT  DD DSN=&MIG.AUE08.FPENT4,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
  *FIXEIN
  *LGENRIN=00032
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FAUE05
//*----------------------------------------------------------------
//SORT0802 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUE05 SORT FAUE05-C-ID-PERS ASC ¬1:12¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FAUE05
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE05.FAUE05
//SORTOUT  DD DSN=&MIG.AUE08.FAUE05,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A,
               29,9,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FAUE07
//*----------------------------------------------------------------
//SORT0803 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUE07 SORT FAUE07-C-ID-PERS ASC ¬1:12¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FAUE07
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE07.FAUE07
//SORTOUT  DD DSN=&MIG.AUE08.FAUE07,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A,
               44,9,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0802       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE08  DD DSN=&MIG.AUE08.FAUE08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0803       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPENT4  DD DSN=&MIG.AUE08.FPENT4.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE05  DD DSN=&MIG.AUE08.FAUE05.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE07  DD DSN=&MIG.AUE08.FAUE07.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE08  DD DSN=&MIG.AUE08.FAUE08.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE08
//*----------------------------------------------------------------
//PPRAUE08 EXEC PGM=PPRAUE08,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE08,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE08,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE08,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFPENT4  DD DISP=SHR,DSN=&MIG.AUE08.FPENT4
//SFAUE05  DD DISP=SHR,DSN=&MIG.AUE08.FAUE05
//SFAUE07  DD DISP=SHR,DSN=&MIG.AUE08.FAUE07
//*--------------<FICHIERS CIBLES>---------------------------------
//SFAUE08  DD DSN=&MIG.AUE08.FAUE08,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPENT4  DD DSN=&MIG.AUE08.FPENT4.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE05  DD DSN=&MIG.AUE08.FAUE05.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE07  DD DSN=&MIG.AUE08.FAUE07.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE08  DD DSN=&MIG.AUE08.FAUE08.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE26
//*----------------------------------------------------------------
//DEL2601       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE06  DD DSN=&MIG.AUE26.FAUE06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFAUE17  DD DSN=&MIG.AUE26.FAUE17,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFAUE08  DD DSN=&MIG.AUE26.FAUE08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FAUE06
//*----------------------------------------------------------------
//SORT2601 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUE06 SORT FAUE06-C-ID-PERS ASC ¬1:12¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FAUE06
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE06.FAUE06
//SORTOUT  DD DSN=&MIG.AUE26.FAUE06,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//* *FIXEIN
//*  *LGENRIN=00020
//*----------------------------------------------------------------
//*TRI DU FICHIER FAUE17
//*----------------------------------------------------------------
//SORT2602 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUE17 SORT FAUE17-C-ID-PERS ASC ¬36:12¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FAUE17
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE17.FAUE17
//SORTOUT  DD DSN=&MIG.AUE26.FAUE17,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(36,12,CH,A,
               1,9,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FAUE08
//*----------------------------------------------------------------
//SORT2603 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUE08 SORT FAUE08-C-ID-PERS ASC ¬1:12¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FAUE08
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE08.FAUE08
//SORTOUT  DD DSN=&MIG.AUE26.FAUE08,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A,
               30,9,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2602       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE26  DD DSN=&MIG.AUE26.FAUE26,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2603       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE26,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE26,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE26,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE06  DD DSN=&MIG.AUE26.FAUE06.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE17  DD DSN=&MIG.AUE26.FAUE17.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE08  DD DSN=&MIG.AUE26.FAUE08.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE26  DD DSN=&MIG.AUE26.FAUE26.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE26
//*----------------------------------------------------------------
//PPRAUE26 EXEC PGM=PPRAUE26,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE26,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE26,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE26,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFAUE06  DD DISP=SHR,DSN=&MIG.AUE26.FAUE06
//SFAUE17  DD DISP=SHR,DSN=&MIG.AUE26.FAUE17
//SFAUE08  DD DISP=SHR,DSN=&MIG.AUE26.FAUE08
//*--------------<FICHIERS CIBLES>---------------------------------
//SFAUE26  DD DSN=&MIG.AUE26.FAUE26,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE06  DD DSN=&MIG.AUE26.FAUE06.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE17  DD DSN=&MIG.AUE26.FAUE17.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE08  DD DSN=&MIG.AUE26.FAUE08.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE26  DD DSN=&MIG.AUE26.FAUE26.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE27
//*----------------------------------------------------------------
//DEL2701       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE26  DD DSN=&MIG.AUE27.FAUE26,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FAUE26
//*----------------------------------------------------------------
//SORT2701 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUE26 SORT FAUE26-C-ID-PERS ASC [1:12—
//* SFAUE26 SORT FAUE26-ID-POP ASC [13:9—
//* SFAUE26 SORT FAUE26-DATE ASC [22:8—
//* SFAUE26 SORT FAUE26-TYPE-EVT ASC [31:1—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FAUE26
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE26.FAUE26
//SORTOUT  DD DSN=&MIG.AUE27.FAUE26,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A,
               13,9,CH,A,
               22,8,CH,A,
               31,1,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2702       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE27  DD DSN=&MIG.AUE27.FAUE27,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2703       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE27,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE27,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE27,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE26  DD DSN=&MIG.AUE27.FAUE26.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE27  DD DSN=&MIG.AUE27.FAUE27.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE27
//*----------------------------------------------------------------
//PPRAUE27 EXEC PGM=PPRAUE27,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE27,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE27,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE27,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFAUE26  DD DISP=SHR,DSN=&MIG.AUE27.FAUE26
//*--------------<FICHIERS CIBLES>---------------------------------
//SFAUE27  DD DSN=&MIG.AUE27.FAUE27,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE26  DD DSN=&MIG.AUE27.FAUE26.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE27  DD DSN=&MIG.AUE27.FAUE27.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE21
//*----------------------------------------------------------------
//DEL2101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE27  DD DSN=&MIG.AUE21.FAUE27,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FAUE27
//*----------------------------------------------------------------
//SORT2101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUE27 SORT FAUE27-ID-POP ASC [1:9—
//* SFAUE27 SORT FAUE27-DT-DVAL-MOD-POP ASC [15:10—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FAUE27
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE27.FAUE27
//SORTOUT  DD DSN=&MIG.AUE21.FAUE27,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,9,CH,A,
               15,10,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STAUEMPO DD DSN=&CIB.TAUEMPO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE21,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE21,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE21,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE27  DD DSN=&MIG.AUE21.FAUE27.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUEMPO DD DSN=&MIG.AUE21.TAUEMPO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE21
//*----------------------------------------------------------------
//PPRAUE21 EXEC PGM=PPRAUE21,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE21,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE21,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE21,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFAUE27  DD DISP=SHR,DSN=&MIG.AUE21.FAUE27
//*--------------<FICHIERS CIBLES>---------------------------------
//STAUEMPO DD DSN=&CIB.TAUEMPO,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE27  DD DSN=&MIG.AUE21.FAUE27.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUEMPO DD DSN=&MIG.AUE21.TAUEMPO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE09
//*----------------------------------------------------------------
//DEL0901       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STABADH  DD DSN=&MIG.AUE09.TABADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01COMP DD DSN=&MIG.AUE09.A01COMP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TABADH
//*----------------------------------------------------------------
//SORT0901 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STABADH SORT ADHSADH-C-ID-PERS ASC [120:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.TADHADH
//SORTIN   DD DISP=SHR,DSN=&CIB.TADHADH
//SORTOUT  DD DSN=&MIG.AUE09.TABADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(120,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01COMP
//*----------------------------------------------------------------
//SORT0902 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01COMP SORT A01-COMPO-GRO-C-ID-PERS ASC [15:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01COMPO
//SORTIN   DD DISP=SHR,DSN=&SRC.A01COMPO
//SORTOUT  DD DSN=&MIG.AUE09.A01COMP,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(15,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0902       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE09  DD DSN=&MIG.AUE09.FAUE09,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0903       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE09,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE09,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE09,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTABADH  DD DSN=&MIG.AUE09.TABADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01COMP DD DSN=&MIG.AUE09.A01COMP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE09  DD DSN=&MIG.AUE09.FAUE09.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE09
//*----------------------------------------------------------------
//PPRAUE09 EXEC PGM=PPRAUE09,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE09,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE09,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE09,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STABADH  DD DISP=SHR,DSN=&MIG.AUE09.TABADH
//SA01COMP DD DISP=SHR,DSN=&MIG.AUE09.A01COMP
//*--------------<FICHIERS CIBLES>---------------------------------
//SFAUE09  DD DSN=&MIG.AUE09.FAUE09,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTABADH  DD DSN=&MIG.AUE09.TABADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01COMP DD DSN=&MIG.AUE09.A01COMP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE09  DD DSN=&MIG.AUE09.FAUE09.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE10
//*----------------------------------------------------------------
//DEL1001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STABADH  DD DSN=&MIG.AUE10.TABADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPENT1  DD DSN=&MIG.AUE10.FPENT1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPENT5  DD DSN=&MIG.AUE10.FPENT5,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TABADH
//*----------------------------------------------------------------
//SORT1001 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STABADH SORT ADHSADH-ID-TECH-UR-ADHR ASC [21:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.TADHADH
//SORTIN   DD DISP=SHR,DSN=&CIB.TADHADH
//SORTOUT  DD DSN=&MIG.AUE10.TABADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FPENT1
//*----------------------------------------------------------------
//SORT1002 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFPENT1 SORT FPENT1-ID-TECH-UR-ENTP ASC [121:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.FPENT1
//SORTIN   DD DISP=SHR,DSN=&CIB.FPENT1
//SORTOUT  DD DSN=&MIG.AUE10.FPENT1,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(121,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FPENT5
//*----------------------------------------------------------------
//SORT1003 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFPENT5 SORT FPENT5-ID-TECH-UR-ENTP ASC [1:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FPENT5
//SORTIN   DD DISP=SHR,DSN=&CIB.FPENT5
//SORTOUT  DD DSN=&MIG.AUE10.FPENT5,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE10  DD DSN=&MIG.AUE10.FAUE10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTABADH  DD DSN=&MIG.AUE10.TABADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPENT1  DD DSN=&MIG.AUE10.FPENT1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPENT5  DD DSN=&MIG.AUE10.FPENT5.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE10  DD DSN=&MIG.AUE10.FAUE10.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE10
//*----------------------------------------------------------------
//PPRAUE10 EXEC PGM=PPRAUE10,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE10,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE10,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE10,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STABADH  DD DISP=SHR,DSN=&MIG.AUE10.TABADH
//SFPENT1  DD DISP=SHR,DSN=&MIG.AUE10.FPENT1
//SFPENT5  DD DISP=SHR,DSN=&MIG.AUE10.FPENT5
//*--------------<FICHIERS CIBLES>---------------------------------
//SFAUE10  DD DSN=&MIG.AUE10.FAUE10,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTABADH  DD DSN=&MIG.AUE10.TABADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPENT1  DD DSN=&MIG.AUE10.FPENT1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPENT5  DD DSN=&MIG.AUE10.FPENT5.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE10  DD DSN=&MIG.AUE10.FAUE10.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE11
//*----------------------------------------------------------------
//DEL1101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE10  DD DSN=&MIG.AUE11.FAUE10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPENT6  DD DSN=&MIG.AUE11.FPENT6,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FAUE10
//*----------------------------------------------------------------
//SORT1101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUE10 SORT FAUE10-ID-TECH-UR-ETAB ASC [38:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FAUE10
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE10.FAUE10
//SORTOUT  DD DSN=&MIG.AUE11.FAUE10,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(38,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FPENT6
//*----------------------------------------------------------------
//SORT1102 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFPENT6 SORT FPENT6-ID-TECH-UR-ETAB ASC [1:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FPENT6
//SORTIN01 DD DISP=SHR,DSN=&CIB.FPENT61
//SORTIN02 DD DISP=SHR,DSN=&CIB.FPENT62
//SORTOUT  DD DSN=&MIG.AUE11.FPENT6,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE11  DD DSN=&MIG.AUE11.FAUE11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE10  DD DSN=&MIG.AUE11.FAUE10.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPENT6  DD DSN=&MIG.AUE11.FPENT6.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE11  DD DSN=&MIG.AUE11.FAUE11.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE11
//*----------------------------------------------------------------
//PPRAUE11 EXEC PGM=PPRAUE11,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE11,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE11,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE11,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFAUE10  DD DISP=SHR,DSN=&MIG.AUE11.FAUE10
//SFPENT6  DD DISP=SHR,DSN=&MIG.AUE11.FPENT6
//*--------------<FICHIERS CIBLES>---------------------------------
//SFAUE11  DD DSN=&MIG.AUE11.FAUE11,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE10  DD DSN=&MIG.AUE11.FAUE10.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPENT6  DD DSN=&MIG.AUE11.FPENT6.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE11  DD DSN=&MIG.AUE11.FAUE11.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE24
//*----------------------------------------------------------------
//DEL2401       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFPADH1  DD DSN=&MIG.AUE24.FPADH1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STCNTCPI DD DSN=&MIG.AUE24.TCNTCPI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FPADH1
//*----------------------------------------------------------------
//SORT2401 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFPADH1 SORT FPADH1-ID-TECH-UR-ADHR ASC [1:20—
//* SFPADH1 SORT FPADH1-NO-ORD-ADHS ASC [21:3—
//* SFPADH1 SORT FPADH1-NO-ORD-CTR ASC [24:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FPADH1
//SORTIN   DD DISP=SHR,DSN=&CIB.FPADH1
//SORTOUT  DD DSN=&MIG.AUE24.FPADH1,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               24,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TCNTCPI
//*----------------------------------------------------------------
//SORT2402 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STCNTCPI SORT CNTCPI-ID-TECH-UR-ADHR ASC [21:20—
//* STCNTCPI SORT CNTCPI-NO-ORD-ADHS ASC [41:3—
//* STCNTCPI SORT CNTCPI-NO-ORD-CTR ASC [44:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.TCNTCPI
//SORTIN   DD DISP=SHR,DSN=&CIB.TCNTCPI
//SORTOUT  DD DSN=&MIG.AUE24.TCNTCPI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,20,CH,A,
               41,3,CH,A,
               44,3,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2402       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE24  DD DSN=&MIG.AUE24.FAUE24,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2403       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE24,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE24,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE24,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPADH1  DD DSN=&MIG.AUE24.FPADH1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTCNTCPI DD DSN=&MIG.AUE24.TCNTCPI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE24  DD DSN=&MIG.AUE24.FAUE24.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE24
//*----------------------------------------------------------------
//PPRAUE24 EXEC PGM=PPRAUE24,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE24,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE24,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE24,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFPADH1  DD DISP=SHR,DSN=&MIG.AUE24.FPADH1
//*STCNTCPI DD DISP=SHR,DSN=&SRC.TCNTCPI
//STCNTCPI DD DISP=SHR,DSN=&MIG.AUE24.TCNTCPI
//*--------------<FICHIERS CIBLES>---------------------------------
//SFAUE24  DD DSN=&MIG.AUE24.FAUE24,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPADH1  DD DSN=&MIG.AUE24.FPADH1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTCNTCPI DD DSN=&MIG.AUE24.TCNTCPI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE24  DD DSN=&MIG.AUE24.FAUE24.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE25
//*----------------------------------------------------------------
//DEL2501       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STABADH  DD DSN=&MIG.AUE25.TABADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA03ADH  DD DSN=&MIG.AUE25.A03ADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB04COND DD DSN=&MIG.AUE25.B04COND,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TABADH
//*----------------------------------------------------------------
//SORT2501 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STABADH SORT ADHSADH-NO-REF-GPS-ADHS ASC [1:1—
//* STABADH SORT ADHSADH-NO-REF-GPS-ADHS ASC [2:9—
//* STABADH SORT ADHSADH-NO-REF-GPS-ADHS ASC [11:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.TADHADH
//SORTIN   DD DISP=SHR,DSN=&CIB.TADHADH
//SORTOUT  DD DSN=&MIG.AUE25.TABADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(44,1,CH,A,
               45,9,CH,A,
               54,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A03ADH
//*----------------------------------------------------------------
//SORT2502 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA03ADH SORT A03-ADHERENT-C-GRPT-GEST ASC [1:1—
//* SA03ADH SORT A03-ADHERENT-C-ADH ASC [3:9—
//* SA03ADH SORT A03-ADHERENT-C-ORDRE ASC [13:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03ADHER
//SORTIN   DD DISP=SHR,DSN=&MIG.A03ADHER
//SORTOUT  DD DSN=&MIG.AUE25.A03ADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,1,CH,A,
               3,9,CH,A,
               13,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B04COND
//*----------------------------------------------------------------
//SORT2503 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB04COND SORT B04-CONTRAT-DUCS-C-GRPT-GEST ASC [1:1—
//* SB04COND SORT B04-CONTRAT-DUCS-C-ADH ASC [3:9—
//* SB04COND SORT B04-CONTRAT-DUCS-C-ORDRE ASC [13:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.B04CDUCS
//SORTIN   DD DISP=SHR,DSN=&SRC.B04CDUCS
//SORTOUT  DD DSN=&MIG.AUE25.B04COND,
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
//DEL2502       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE25  DD DSN=&MIG.AUE25.FAUE25,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*BAC-001STAUEPAC DD DSN=&CIB.TAUEPAC,
//*            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2503       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE25,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE25,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE25,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTABADH  DD DSN=&MIG.AUE25.TABADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA03ADH  DD DSN=&MIG.AUE25.A03ADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZB04COND DD DSN=&MIG.AUE25.B04COND.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE25  DD DSN=&MIG.AUE25.FAUE25.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE25
//*----------------------------------------------------------------
//PPRAUE25 EXEC PGM=PPRAUE25,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE25,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE25,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE25,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SA03ADH  DD DISP=SHR,DSN=&MIG.AUE25.A03ADH
//STABADH  DD DISP=SHR,DSN=&MIG.AUE25.TABADH
//SB04COND DD DISP=SHR,DSN=&MIG.AUE25.B04COND
//*--------------<FICHIERS CIBLES>---------------------------------
//SFAUE25  DD DSN=&MIG.AUE25.FAUE25,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*STAUEPAC DD DSN=&CIB.TAUEPAC,
//*            DISP=(NEW,CATLG,CATLG),
//*            SPACE=(TRK,(200,100),RLSE)
//ZTABADH  DD DSN=&MIG.AUE25.TABADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA03ADH  DD DSN=&MIG.AUE25.A03ADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZB04COND DD DSN=&MIG.AUE25.B04COND.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE25  DD DSN=&MIG.AUE25.FAUE25.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES  du programme PPRAUE31
//*----------------------------------------------------------------
//DEL3101  EXEC PGM=IEFBR14
//SYSPRINT DD SYOUT=*
//STABADHS DD DSN=&MIG.AUE31.TABADHS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TABADHS
//*----------------------------------------------------------------
//SORT3101 EXEC PGM=SORT
//* DEBUT CRITERE XGEN
//* TABADHS SORT ADHSADH-C-GRPT-GEST ASC [94:1]
//* TABADHS SORT ADHSADH-C-ADH ASC [95:9]
//* TABADHS SORT ADHSADH-C-ORDRE ASC [104:4]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TADHADH
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH15.TADHADH
//SORTOUT  DD DSN=&MIG.AUE31.TABADHS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(94,1,CH,A,
               95,9,CH,A,
               104,4,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3102  EXEC PGM=IEFBR14
//SYSPRINT DD SYSOUT=*
//STAUEPAC DD DSN=&MIG.AUE31.TAUEPAC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE31,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE31,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTABADHS DD DSN=&MIG.AUE31.TABADHS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA03ADH  DD DSN=&MIG.AUE31.A03ADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUEPAC DD DSN=&MIG.AUE31.TAUEPAC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION
//*----------------------------------------------------------------
//PPRAUE31 EXEC PGM=PPRAUE31
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DISP=SHR,DSN=&ANO.PPRAUE31
//COMPTEUR DD DISP=SHR,DSN=&CPT.PPRAUE31
//FICTRACE DD DISP=SHR,DSN=&TRC.PPRAUE31
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STABADHS DD DISP=SHR,DSN=&MIG.AUE31.TABADHS
//SA03ADH  DD DISP=SHR,DSN=&MIG.AUE25.A03ADH
//*--------------<FICHIERS CIBLES>---------------------------------
//STAUEPAC DD DSN=&MIG.AUE31.TAUEPAC,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,500),RLSE)
//ZTABADHS DD DSN=&MIG.AUE31.TABADHS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA03ADH  DD DSN=&MIG.AUE31.A03ADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUEPAC DD DSN=&MIG.AUE31.TAUEPAC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
/*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE22
//*----------------------------------------------------------------
//DEL2201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STABADH  DD DSN=&MIG.AUE22.TABADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFAUE25  DD DSN=&MIG.AUE22.FAUE25,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFAUE09  DD DSN=&MIG.AUE22.FAUE09,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFAUE11  DD DSN=&MIG.AUE22.FAUE11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFAUE24  DD DSN=&MIG.AUE22.FAUE24,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAUEPA  DD DSN=&MIG.AUE22.TAUEPA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TABADH
//*----------------------------------------------------------------
//SORT2201 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STABADH SORT ADHSADH-ID-TECH-UR-ADHR ASC [21:20—
//* STABADH SORT ADHSADH-NO-ORD-ADHS ASC [41:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.TADHADH
//SORTIN   DD DISP=SHR,DSN=&CIB.TADHADH
//SORTOUT  DD DSN=&MIG.AUE22.TABADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,20,CH,A,
               41,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FAUE25
//*----------------------------------------------------------------
//SORT2202 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUE25 SORT FAUE25-ID-TECH-UR-ADHR ASC [1:20—
//* SFAUE25 SORT FAUE25-NO-ORD-ADHS ASC [21:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FAUE25
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE25.FAUE25
//SORTOUT  DD DSN=&MIG.AUE22.FAUE25,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FAUE09
//*----------------------------------------------------------------
//SORT2203 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUE09 SORT FAUE09-ID-TECH-UR-ADHR ASC [1:20—
//* SFAUE09 SORT FAUE09-NO-ORD-ADHS ASC [21:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FAUE09
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE09.FAUE09
//SORTOUT  DD DSN=&MIG.AUE22.FAUE09,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FAUE11
//*----------------------------------------------------------------
//SORT2204 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUE11 SORT FAUE11-FAUE10-ID-TECH-UR-ADHR ASC [1:20—
//* SFAUE11 SORT FAUE11-FAUE10-NO-ORD-ADHS ASC [21:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FAUE11
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE11.FAUE11
//SORTOUT  DD DSN=&MIG.AUE22.FAUE11,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FAUE24
//*----------------------------------------------------------------
//SORT2205 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUE24 SORT FAUE24-FPADH-ID-TECH-UR-ADHR ASC [1:20—
//* SFAUE24 SORT FAUE24-FPADH-NO-ORD-ADHS ASC [21:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FAUE24
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE24.FAUE24
//SORTOUT  DD DSN=&MIG.AUE22.FAUE24,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TAUEPAC
//*----------------------------------------------------------------
//SORT2206 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STAUEPA SORT TAUEPAC-ID-TECH-UR-ADHR ASC [1:20—
//* STAUEPA SORT TAUEPAC-NO-ORD-ADHS ASC [21:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAUEPAC
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE31.TAUEPAC
//SORTOUT  DD DSN=&MIG.AUE22.TAUEPA,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2202       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STAUEUED DD DSN=&MIG.AUE22.TAUEUED,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAUEUEC DD DSN=&MIG.AUE22.TAUEUEC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STUEDADH DD DSN=&CIB.TUEDADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STUECADH DD DSN=&CIB.TUECADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAUEMUD DD DSN=&CIB.TAUEMUD,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPAUE01 DD DSN=&CIB.FPAUE01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*STAUEMUC DD DSN=&CIB.TAUEMUC,
//STAUEMUC DD DSN=&MIG.AUE22.TAUEMUC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAUERDM DD DSN=&CIB.TAUERDM,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAUERCM DD DSN=&CIB.TAUERCM,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAUERDC DD DSN=&CIB.TAUERDC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE22,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE22,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE22,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUEUEM DD DSN=&MIG.AUE22.TAUEUEM.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTABADH  DD DSN=&MIG.AUE22.TABADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE25  DD DSN=&MIG.AUE22.FAUE25.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE09  DD DSN=&MIG.AUE22.FAUE09.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE11  DD DSN=&MIG.AUE22.FAUE11.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE24  DD DSN=&MIG.AUE22.FAUE24.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUEPA  DD DSN=&MIG.AUE22.TAUEPA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUEUED DD DSN=&MIG.AUE22.TAUEUED.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUEUEC DD DSN=&MIG.AUE22.TAUEUEC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTUEDADH DD DSN=&MIG.AUE22.TUEDADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTUECADH DD DSN=&MIG.AUE22.TUECADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUEMUD DD DSN=&MIG.AUE22.TAUEMUD.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPAUE01 DD DSN=&MIG.AUE22.FPAUE01.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUEMUC DD DSN=&MIG.AUE22.TAUEMUC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUERDM DD DSN=&MIG.AUE22.TAUERDM.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUERCM DD DSN=&MIG.AUE22.TAUERCM.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUERDC DD DSN=&MIG.AUE22.TAUERDC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE22
//*----------------------------------------------------------------
//PPRAUE22 EXEC PGM=PPRAUE22,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE22,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE22,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE22,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//TAUEUEM  DD DISP=SHR,DSN=&CIB.TAUEUEM
//STABADH  DD DISP=SHR,DSN=&MIG.AUE22.TABADH
//SFAUE25  DD DISP=SHR,DSN=&MIG.AUE22.FAUE25
//SFAUE09  DD DISP=SHR,DSN=&MIG.AUE22.FAUE09
//SFAUE11  DD DISP=SHR,DSN=&MIG.AUE22.FAUE11
//SFAUE24  DD DISP=SHR,DSN=&MIG.AUE22.FAUE24
//STAUEPA  DD DISP=SHR,DSN=&MIG.AUE22.TAUEPA
//*--------------<FICHIERS CIBLES>---------------------------------
//STAUEUED DD DSN=&MIG.AUE22.TAUEUED,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STAUEUEC DD DSN=&MIG.AUE22.TAUEUEC,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STUEDADH DD DSN=&CIB.TUEDADH,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STUECADH DD DSN=&CIB.TUECADH,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STAUEMUD DD DSN=&CIB.TAUEMUD,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFPAUE01 DD DSN=&CIB.FPAUE01,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STAUEMUC DD DSN=&MIG.AUE22.TAUEMUC,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STAUERDM DD DSN=&CIB.TAUERDM,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STAUERCM DD DSN=&CIB.TAUERCM,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STAUERDC DD DSN=&CIB.TAUERDC,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUEUEM DD DSN=&MIG.AUE22.TAUEUEM.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTABADH  DD DSN=&MIG.AUE22.TABADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE25  DD DSN=&MIG.AUE22.FAUE25.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE09  DD DSN=&MIG.AUE22.FAUE09.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE11  DD DSN=&MIG.AUE22.FAUE11.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE24  DD DSN=&MIG.AUE22.FAUE24.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUEPA  DD DSN=&MIG.AUE22.TAUEPA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUEUED DD DSN=&MIG.AUE22.TAUEUED.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUEUEC DD DSN=&MIG.AUE22.TAUEUEC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTUEDADH DD DSN=&MIG.AUE22.TUEDADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTUECADH DD DSN=&MIG.AUE22.TUECADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUEMUD DD DSN=&MIG.AUE22.TAUEMUD.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPAUE01 DD DSN=&MIG.AUE22.FPAUE01.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUEMUC DD DSN=&MIG.AUE22.TAUEMUC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUERDM DD DSN=&MIG.AUE22.TAUERDM.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUERCM DD DSN=&MIG.AUE22.TAUERCM.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUERDC DD DSN=&MIG.AUE22.TAUERDC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE2A
//*----------------------------------------------------------------
//DEL2A01  EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STAGAADN DD DSN=&MIG.AUE2A.TAGAADN,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAGACNT DD DSN=&MIG.AUE2A.TAGACNT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAUEUEC DD DSN=&MIG.AUE2A.TAUEUEC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAUEUED DD DSN=&MIG.AUE2A.TAUEUED,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TAGAADN
//*----------------------------------------------------------------
//SORT2A01 EXEC PGM=SORT
//* DEBUT CRITERE XGEN
//* TAGAADN SORT TAGAADN-ID-TECH-UR-ADHR ASC [1:20]
//* TAGAADN SORT TAGAADN-NO-ORD-ADHS ASC [21:3]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TAGAADN
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAGAADN
//SORTOUT  DD DSN=&MIG.AUE2A.TAGAADN,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TAGACNT
//*----------------------------------------------------------------
//SORT2A02 EXEC PGM=SORT
//* DEBUT CRITERE XGEN
//* TAGACNT SORT TAGACNT-ID-TECH-UR-ADHR ASC [1:20]
//* TAGACNT SORT TAGACNT-NO-ORD-ADHS ASC [21:3]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TAGACNT
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAGACNT
//SORTOUT  DD DSN=&MIG.AUE2A.TAGACNT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TAUEUEC
//*----------------------------------------------------------------
//SORT2A03 EXEC PGM=SORT
//* DEBUT CRITERE XGEN
//* TAUEUEC SORT TAUEUEC-ID-TECH-UR-ADHR ASC [1:20]
//* TAUEUEC SORT TAUEUEC-NO-ORD-ADHS ASC [21:3]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE22.TAUEUEC
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAUEUEC
//SORTOUT  DD DSN=&MIG.AUE2A.TAUEUEC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TAUEUED
//*----------------------------------------------------------------
//SORT2A04 EXEC PGM=SORT
//* DEBUT CRITERE XGEN
//* TAUEUED SORT TAUEUED-ID-TECH-UR-ADHR ASC [1:20]
//* TAUEUED SORT TAUEUED-NO-ORD-ADHS ASC [21:3]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE22.TAUEUED
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAUEUED
//SORTOUT  DD DSN=&MIG.AUE2A.TAUEUED,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2A02  EXEC PGM=IEFBR14
//SYSPRINT DD SYSOUT=*
//SSAUEUEC DD DSN=&CIB.TAUEUEC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SSAUEUED DD DSN=&CIB.TAUEUED,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGAADN DD DSN=&RED.AUE2A.TAGAADN,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGACNT DD DSN=&RED.AUE2A.TAGACNT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUEUEC DD DSN=&RED.AUE2A.TAUEUEC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUEUED DD DSN=&RED.AUE2A.TAUEUED,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZSAUEUEC DD DSN=&RED.AUE2A.SAUEUEC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZSAUEUED DD DSN=&RED.AUE2A.SAUEUED,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2A03       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE2A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE2A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE2A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION
//*----------------------------------------------------------------
//PPRAUE2A EXEC PGM=PPRAUE2A
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE2A,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE2A,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5000,500),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE2A,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5000,500),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DUMMY
//*--------------<FICHIERS SOURCES>--------------------------------
//STAGAADN DD DISP=SHR,DSN=&MIG.AUE2A.TAGAADN
//STAGACNT DD DISP=SHR,DSN=&MIG.AUE2A.TAGACNT
//STAUEUEC DD DISP=SHR,DSN=&MIG.AUE2A.TAUEUEC
//STAUEUED DD DISP=SHR,DSN=&MIG.AUE2A.TAUEUED
//*--------------<FICHIERS CIBLES>---------------------------------
//SSAUEUEC DD DSN=&CIB.TAUEUEC,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,500),RLSE)
//SSAUEUED DD DSN=&CIB.TAUEUED,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,500),RLSE)
//*--------------<FICHIERS REDUITS>--------------------------------
//ZTAGAADN DD DSN=&RED.AUE2A.TAGAADN,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(10,50),RLSE)
//ZTAGACNT DD DSN=&RED.AUE2A.TAGACNT,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(10,50),RLSE)
//ZTAUEUEC DD DSN=&RED.AUE2A.TAUEUEC,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(10,50),RLSE)
//ZTAUEUED DD DSN=&RED.AUE2A.TAUEUED,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(10,50),RLSE)
//ZSAUEUEC DD DSN=&RED.AUE2A.SAUEUEC,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(10,50),RLSE)
//ZSAUEUED DD DSN=&RED.AUE2A.SAUEUED,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(10,50),RLSE)
//*
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE40
//*----------------------------------------------------------------
//DEL4001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STMUCCPT DD DSN=&MIG.AUE40.TMUCCPT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STABADH  DD DSN=&MIG.AUE40.TABADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TMUCCPT
//*----------------------------------------------------------------
//*SORT4001 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STMUCCPT SORT TMUCCPT-C-GRPT-GEST ASC [1:1—
//* STMUCCPT SORT TMUCCPT-C-ADH ASC [2:9—
//* STMUCCPT SORT TMUCCPT-C-ORDRE ASC [11:4—
//* FIN   CRITERE XGEN
//*SYSOUT   DD SYSOUT=*
//*COPY     DD DSN=&CPYSRC.TMUCCPT
//*SORTIN   DD DISP=SHR,DSN=&TAB.TMUCCPT
//*SORTOUT  DD DSN=&MIG.AUE40.TMUCCPT,
//*            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//*            SPACE=(TRK,(5000,500),RLSE)
//*SYSIN    DD *
//*  SORT FIELDS=(1,1,CH,A,
//*               2,9,CH,A,
//*              11,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TABADH
//*----------------------------------------------------------------
//SORT4002 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STABADH SORT ADHSADH-C-GRPT-GEST ASC [94:1—
//* STABADH SORT ADHSADH-C-ADH ASC [95:9—
//* STABADH SORT ADHSADH-C-ORDRE ASC [104:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DSN=&CPYCIB.TADHADH
//SORTIN   DD DISP=SHR,DSN=&CIB.TADHADH
//SORTOUT  DD DSN=&MIG.AUE40.TABADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(94,1,CH,A,
               95,9,CH,A,
               104,4,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL4002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFMUCCPT DD DSN=&MIG.AUE40.FMUCCPT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL4003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE40,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE40,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE40,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTMUCCPT DD DSN=&MIG.AUE40.TMUCCPT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTABADH  DD DSN=&MIG.AUE40.TABADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFMUCCPT DD DSN=&MIG.AUE40.FMUCCPT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE40
//*----------------------------------------------------------------
//PPRAUE40 EXEC PGM=PPRAUE40,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE40,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE40,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE40,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STMUCCPT DD DUMMY
//*STMUCCPT DD DISP=SHR,DSN=&MIG.AUE40.TMUCCPT
//STABADH  DD DISP=SHR,DSN=&MIG.AUE40.TABADH
//*--------------<FICHIERS CIBLES>---------------------------------
//SFMUCCPT DD DSN=&MIG.AUE40.FMUCCPT,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTMUCCPT DD DSN=&MIG.AUE40.TMUCCPT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTABADH  DD DSN=&MIG.AUE40.TABADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFMUCCPT DD DSN=&MIG.AUE40.FMUCCPT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE41
//*----------------------------------------------------------------
//DEL4101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFMUCCPT DD DSN=&MIG.AUE41.FMUCCPT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAUEMU0 DD DSN=&MIG.AUE41.TAUEMU0,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FMUCCPT
//*----------------------------------------------------------------
//*SORT4101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFMUCCPT SORT FMUCCPT-ID-TECH-UR-ADHR ASC [36:20—
//* SFMUCCPT SORT FMUCCPT-NO-ORD-ADHS ASC [56:3—
//* SFMUCCPT SORT FMUCCPT-DT-DVAL-MOD-UEC ASC [15:10—
//* FIN   CRITERE XGEN
//*SYSOUT   DD SYSOUT=*
//*COPY     DD DSN=&CPYSRC.TMUCCPT
//*SORTIN   DD DISP=SHR,DSN=&MIG.AUE40.FMUCCPT
//*SORTOUT  DD DSN=&MIG.AUE41.FMUCCPT,
//*            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//*            SPACE=(TRK,(5000,500),RLSE)
//*SYSIN    DD *
//*  SORT FIELDS=(36,20,CH,A,
//*               56,3,CH,A,
//*               15,10,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TAUEMU0
//*----------------------------------------------------------------
//SORT4102 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STAUEMU0 SORT TAUEMUC-ID-TECH-UR-ADHR ASC [1:20—
//* STAUEMU0 SORT TAUEMUC-NO-ORD-ADHS ASC [21:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DSN=&CPYCIB.TAUEMUC
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE22.TAUEMUC
//SORTOUT  DD DSN=&MIG.AUE41.TAUEMU0,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL4102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFMUCCPS DD DSN=&MIG.AUE41.FMUCCPS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL4103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE41,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE41,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE41,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFMUCCPT DD DSN=&MIG.AUE41.FMUCCPT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUEMU0 DD DSN=&MIG.AUE41.TAUEMU0.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFMUCCPS DD DSN=&MIG.AUE41.FMUCCPS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE41
//*----------------------------------------------------------------
//PPRAUE41 EXEC PGM=PPRAUE41,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE41,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE41,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE41,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//*SFMUCCPT DD DISP=SHR,DSN=&MIG.AUE41.FMUCCPT
//SFMUCCPT DD DUMMY
//STAUEMU0 DD DISP=SHR,DSN=&MIG.AUE41.TAUEMU0
//*--------------<FICHIERS CIBLES>---------------------------------
//SFMUCCPS DD DSN=&MIG.AUE41.FMUCCPS,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFMUCCPT DD DSN=&MIG.AUE41.FMUCCPT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUEMU0 DD DSN=&MIG.AUE41.TAUEMU0.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFMUCCPS DD DSN=&MIG.AUE41.FMUCCPS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE42
//*----------------------------------------------------------------
//DEL4201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFMUCCPT DD DSN=&MIG.AUE42.FMUCCPT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAUEMU0 DD DSN=&MIG.AUE42.TAUEMU0,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FMUCCPT
//*----------------------------------------------------------------
//SORT4201 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFMUCCPT SORT FMUCCPT-ID-TECH-UR-ADHR ASC [36:20—
//* SFMUCCPT SORT FMUCCPT-NO-ORD-ADHS ASC [56:3—
//* SFMUCCPT SORT FMUCCPT-DT-DVAL-MOD-UEC ASC [15:10—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DSN=&CPYSRC.TMUCCPT
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE41.FMUCCPS
//SORTOUT  DD DSN=&MIG.AUE42.FMUCCPT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(36,20,CH,A,
               56,3,CH,A,
               15,10,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TAUEMU0
//*----------------------------------------------------------------
//SORT4202 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STAUEMU0 SORT TAUEMUC-ID-TECH-UR-ADHR ASC [1:20—
//* STAUEMU0 SORT TAUEMUC-NO-ORD-ADHS ASC [21:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DSN=&CPYCIB.TAUEMUC
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE22.TAUEMUC
//SORTOUT  DD DSN=&MIG.AUE42.TAUEMU0,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL4202       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STAUEMUC DD DSN=&CIB.TAUEMUC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL4203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE42,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE42,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE42,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFMUCCPT DD DSN=&MIG.AUE42.FMUCCPT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUEMU0 DD DSN=&MIG.AUE42.TAUEMU0.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUEMUC DD DSN=&MIG.AUE42.TAUEMUC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE42
//*----------------------------------------------------------------
//PPRAUE42 EXEC PGM=PPRAUE42,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE42,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE42,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE42,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFMUCCPT DD DISP=SHR,DSN=&MIG.AUE42.FMUCCPT
//STAUEMU0 DD DISP=SHR,DSN=&MIG.AUE42.TAUEMU0
//*--------------<FICHIERS CIBLES>---------------------------------
//STAUEMUC DD DSN=&CIB.TAUEMUC,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFMUCCPT DD DSN=&MIG.AUE42.FMUCCPT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUEMU0 DD DSN=&MIG.AUE42.TAUEMU0.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUEMUC DD DSN=&MIG.AUE42.TAUEMUC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE12
//*----------------------------------------------------------------
//DEL1201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SA01TIG  DD DSN=&MIG.AUE12.A01TIG,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPENT2  DD DSN=&MIG.AUE12.FPENT2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER A01TIG
//*----------------------------------------------------------------
//SORT1201 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01TIG SORT A01TGEST-C-ID-PERS ASC [16:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01TIERG
//SORTIN   DD DISP=SHR,DSN=&SRC.A01TIERG
//SORTOUT  DD DSN=&MIG.AUE12.A01TIG,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FPENT2
//*----------------------------------------------------------------
//SORT1202 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFPENT2 SORT FPENT2-C-ID-PERS ASC [21:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.FPENT2
//SORTIN   DD DISP=SHR,DSN=&CIB.FPENT2
//SORTOUT  DD DSN=&MIG.AUE12.FPENT2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1202       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE12  DD DSN=&MIG.AUE12.FAUE12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01TIG  DD DSN=&MIG.AUE12.A01TIG.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPENT2  DD DSN=&MIG.AUE12.FPENT2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE12  DD DSN=&MIG.AUE12.FAUE12.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE12
//*----------------------------------------------------------------
//PPRAUE12 EXEC PGM=PPRAUE12,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE12,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE12,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE12,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SA01TIG  DD DISP=SHR,DSN=&MIG.AUE12.A01TIG
//SFPENT2  DD DISP=SHR,DSN=&MIG.AUE12.FPENT2
//*--------------<FICHIERS CIBLES>---------------------------------
//SFAUE12  DD DSN=&MIG.AUE12.FAUE12,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01TIG  DD DSN=&MIG.AUE12.A01TIG.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPENT2  DD DSN=&MIG.AUE12.FPENT2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE12  DD DSN=&MIG.AUE12.FAUE12.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE13
//*----------------------------------------------------------------
//DEL1301       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE12  DD DSN=&MIG.AUE13.FAUE12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCIDINT  DD DSN=&MIG.AUE13.CIDINT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FAUE12
//*----------------------------------------------------------------
//SORT1301 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUE12 SORT FAUE12-C-ID-CONTACT ASC [15:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FAUE12
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE12.FAUE12
//SORTOUT  DD DSN=&MIG.AUE13.FAUE12,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(15,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER CIDINT
//*----------------------------------------------------------------
//SORT1302 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCIDINT SORT ID-INT-C-ID-CONTACT ASC [33:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.CIDINT
//SORTIN   DD DISP=SHR,DSN=&CIB.IDINT
//SORTOUT  DD DSN=&MIG.AUE13.CIDINT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(33,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1302       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE13  DD DSN=&MIG.AUE13.FAUE13,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1303       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE13,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE13,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE13,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE12  DD DSN=&MIG.AUE13.FAUE12.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCIDINT  DD DSN=&MIG.AUE13.CIDINT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE13  DD DSN=&MIG.AUE13.FAUE13.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE13
//*----------------------------------------------------------------
//PPRAUE13 EXEC PGM=PPRAUE13,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE13,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE13,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE13,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFAUE12  DD DISP=SHR,DSN=&MIG.AUE13.FAUE12
//SCIDINT  DD DISP=SHR,DSN=&MIG.AUE13.CIDINT
//*--------------<FICHIERS CIBLES>---------------------------------
//SFAUE13  DD DSN=&MIG.AUE13.FAUE13,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE12  DD DSN=&MIG.AUE13.FAUE12.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCIDINT  DD DSN=&MIG.AUE13.CIDINT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE13  DD DSN=&MIG.AUE13.FAUE13.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE14
//*----------------------------------------------------------------
//DEL1401       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE12  DD DSN=&MIG.AUE14.FAUE12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCIDINT  DD DSN=&MIG.AUE14.CIDINT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FAUE12
//*----------------------------------------------------------------
//SORT1401 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUE12 SORT FAUE12-C-ID-PERS ASC [27:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FAUE12
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE12.FAUE12
//SORTOUT  DD DSN=&MIG.AUE14.FAUE12,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(27,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER CIDINT
//*----------------------------------------------------------------
//SORT1402 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCIDINT SORT ID-INT-C-ID-PERS ASC [21:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.CIDINT
//SORTIN   DD DISP=SHR,DSN=&CIB.IDINT
//SORTOUT  DD DSN=&MIG.AUE14.CIDINT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1402       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE14  DD DSN=&MIG.AUE14.FAUE14,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1403       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE14,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE14,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE14,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE12  DD DSN=&MIG.AUE14.FAUE12.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCIDINT  DD DSN=&MIG.AUE14.CIDINT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE14  DD DSN=&MIG.AUE14.FAUE14.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE14
//*----------------------------------------------------------------
//PPRAUE14 EXEC PGM=PPRAUE14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE14,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE14,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE14,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFAUE12  DD DISP=SHR,DSN=&MIG.AUE14.FAUE12
//SCIDINT  DD DISP=SHR,DSN=&MIG.AUE14.CIDINT
//*--------------<FICHIERS CIBLES>---------------------------------
//SFAUE14  DD DSN=&MIG.AUE14.FAUE14,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE12  DD DSN=&MIG.AUE14.FAUE12.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCIDINT  DD DSN=&MIG.AUE14.CIDINT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE14  DD DSN=&MIG.AUE14.FAUE14.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE15
//*----------------------------------------------------------------
//DEL1501       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE13  DD DSN=&MIG.AUE15.FAUE13,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFAUE14  DD DSN=&MIG.AUE15.FAUE14,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPILO1  DD DSN=&MIG.AUE15.FPILO1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPILO2  DD DSN=&MIG.AUE15.FPILO2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPILO3  DD DSN=&MIG.AUE15.FPILO3,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FAUE13
//*----------------------------------------------------------------
//SORT1501 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUE13 SORT FAUE13-ID-TECH-INTR ASC [39:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FAUE13
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE13.FAUE13
//SORTOUT  DD DSN=&MIG.AUE15.FAUE13,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(39,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FAUE14
//*----------------------------------------------------------------
//SORT1502 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUE14 SORT FAUE14-ID-TECH-INTR ASC [39:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FAUE14
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE14.FAUE14
//SORTOUT  DD DSN=&MIG.AUE15.FAUE14,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(39,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FPILO1
//*----------------------------------------------------------------
//SORT1503 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFPILO1 SORT FPILO-ID-TECH-INTR ASC [1:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.FPILO
//SORTIN   DD DISP=SHR,DSN=&CIB.FPILO
//SORTOUT  DD DSN=&MIG.AUE15.FPILO1,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FPILO2
//*----------------------------------------------------------------
//SORT1504 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFPILO2 SORT FPILO2-ID-TECH-INTR ASC [1:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.FPILO2
//SORTIN   DD DISP=SHR,DSN=&CIB.FPILO2
//SORTOUT  DD DSN=&MIG.AUE15.FPILO2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FPILO3
//*----------------------------------------------------------------
//SORT1505 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFPILO3 SORT FPILO3-ID-TECH-INTR ASC [1:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.FPILO3
//SORTIN   DD DISP=SHR,DSN=&CIB.FPILO3
//SORTOUT  DD DSN=&MIG.AUE15.FPILO3,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1502       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE15  DD DSN=&MIG.AUE15.FAUE15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1503       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE13  DD DSN=&MIG.AUE15.FAUE13.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE14  DD DSN=&MIG.AUE15.FAUE14.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPILO1  DD DSN=&MIG.AUE15.FPILO1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPILO2  DD DSN=&MIG.AUE15.FPILO2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPILO3  DD DSN=&MIG.AUE15.FPILO3.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE15  DD DSN=&MIG.AUE15.FAUE15.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE15
//*----------------------------------------------------------------
//PPRAUE15 EXEC PGM=PPRAUE15,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE15,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE15,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE15,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFAUE13  DD DISP=SHR,DSN=&MIG.AUE15.FAUE13
//SFAUE14  DD DISP=SHR,DSN=&MIG.AUE15.FAUE14
//SFPILO1  DD DISP=SHR,DSN=&MIG.AUE15.FPILO1
//SFPILO2  DD DISP=SHR,DSN=&MIG.AUE15.FPILO2
//SFPILO3  DD DISP=SHR,DSN=&MIG.AUE15.FPILO3
//*--------------<FICHIERS CIBLES>---------------------------------
//SFAUE15  DD DSN=&MIG.AUE15.FAUE15,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE13  DD DSN=&MIG.AUE15.FAUE13.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE14  DD DSN=&MIG.AUE15.FAUE14.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPILO1  DD DSN=&MIG.AUE15.FPILO1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPILO2  DD DSN=&MIG.AUE15.FPILO2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPILO3  DD DSN=&MIG.AUE15.FPILO3.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE15  DD DSN=&MIG.AUE15.FAUE15.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE16
//*----------------------------------------------------------------
//DEL1601       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SA01TIG  DD DSN=&MIG.AUE16.A01TIG,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER A01TIG
//*----------------------------------------------------------------
//SORT1601 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01TIG SORT A01TGEST-C-GRPT-GEST ASC [29:1—
//* SA01TIG SORT A01TGEST-C-ADH ASC [31:9—
//* SA01TIG SORT A01TGEST-C-ORDRE ASC [41:4—
//* SA01TIG SORT A01TGEST-C-ENV-RECAP ASC [76:1—
//* SA01TIG SORT A01TGEST-C-ENV-APPEL ASC [74:1—
//* SA01TIG SORT A01TGEST-C-ENV-COMPTE ASC [85:1—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01TIERG
//SORTIN   DD DISP=SHR,DSN=&SRC.A01TIERG
//SORTOUT  DD DSN=&MIG.AUE16.A01TIG,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(29,1,CH,A,
               31,9,CH,A,
               41,4,CH,A,
               76,1,CH,A,
               74,1,CH,A,
               85,1,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1602       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE16  DD DSN=&MIG.AUE16.FAUE16,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1603       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE16,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE16,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE16,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01TIG  DD DSN=&MIG.AUE16.A01TIG.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE16  DD DSN=&MIG.AUE16.FAUE16.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE16
//*----------------------------------------------------------------
//PPRAUE16 EXEC PGM=PPRAUE16,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE16,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE16,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE16,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SA01TIG  DD DISP=SHR,DSN=&MIG.AUE16.A01TIG
//*--------------<FICHIERS CIBLES>---------------------------------
//SFAUE16  DD DSN=&MIG.AUE16.FAUE16,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01TIG  DD DSN=&MIG.AUE16.A01TIG.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE16  DD DSN=&MIG.AUE16.FAUE16.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE28
//*----------------------------------------------------------------
//DEL2801       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE16  DD DSN=&MIG.AUE28.FAUE16,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FAUE16
//*----------------------------------------------------------------
//SORT2801 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUE16 SORT FAUE16-C-GRPT-GEST ASC [1:1—
//* SFAUE16 SORT FAUE16-C-ADH ASC [2:9—
//* SFAUE16 SORT FAUE16-C-ORDRE ASC [11:4—
//* SFAUE16 SORT FAUE16-CAS-LCI ASC [15:1—
//* SFAUE16 SORT FAUE16-D-DEB-EFFET DESC [28:8—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FAUE16
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE16.FAUE16
//SORTOUT  DD DSN=&MIG.AUE28.FAUE16,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,1,CH,A,
               2,9,CH,A,
               11,4,CH,A,
               15,1,CH,A,
               28,8,CH,D)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2802       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFAUE28  DD DSN=&MIG.AUE28.FAUE28,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2803       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE28,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE28,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE28,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE16  DD DSN=&MIG.AUE28.FAUE16.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE28  DD DSN=&MIG.AUE28.FAUE28.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE28
//*----------------------------------------------------------------
//PPRAUE28 EXEC PGM=PPRAUE28,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE28,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE28,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE28,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFAUE16  DD DISP=SHR,DSN=&MIG.AUE28.FAUE16
//*--------------<FICHIERS CIBLES>---------------------------------
//SFAUE28  DD DSN=&MIG.AUE28.FAUE28,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE16  DD DSN=&MIG.AUE28.FAUE16.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE28  DD DSN=&MIG.AUE28.FAUE28.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRAUE23
//*----------------------------------------------------------------
//DEL2301       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STABADH  DD DSN=&MIG.AUE23.TABADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01TIG  DD DSN=&MIG.AUE23.A01TIG,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFAUE15  DD DSN=&MIG.AUE23.FAUE15,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFAUE28  DD DSN=&MIG.AUE23.FAUE28,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TABADH
//*----------------------------------------------------------------
//SORT2301 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STABADH SORT ADHSADH-NO-REF-GPS-ADHS(1:1) ASC [44:1—
//* STABADH SORT ADHSADH-NO-REF-GPS-ADHS(2:9) ASC [45:9—
//* STABADH SORT ADHSADH-NO-REF-GPS-ADHS(11:4) ASC [54:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.TADHADH
//SORTIN   DD DISP=SHR,DSN=&CIB.TADHADH
//SORTOUT  DD DSN=&MIG.AUE23.TABADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(44,1,CH,A,
               45,9,CH,A,
               54,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01TIG
//*----------------------------------------------------------------
//SORT2302 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01TIG SORT A01TGEST-C-GRPT-GEST ASC [29:1—
//* SA01TIG SORT A01TGEST-C-ADH ASC [31:9—
//* SA01TIG SORT A01TGEST-C-ORDRE ASC [41:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01TIERG
//SORTIN   DD DISP=SHR,DSN=&SRC.A01TIERG
//SORTOUT  DD DSN=&MIG.AUE23.A01TIG,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(29,1,CH,A,
               31,9,CH,A,
               41,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FAUE15
//*----------------------------------------------------------------
//SORT2303 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUE15 SORT FAUE15-FAUE12-C-GRPT-GEST ASC [1:1—
//* SFAUE15 SORT FAUE15-FAUE12-C-ADH ASC [2:9—
//* SFAUE15 SORT FAUE15-FAUE12-C-ORDRE ASC [11:4—
//* SFAUE15 SORT FAUE15-C-ORIGINE ASC [39:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FAUE15
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE15.FAUE15
//SORTOUT  DD DSN=&MIG.AUE23.FAUE15,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,1,CH,A,
               2,9,CH,A,
               11,4,CH,A,
               39,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FAUE28
//*----------------------------------------------------------------
//SORT2304 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFAUE28 SORT FAUE28-C-GRPT-GEST ASC [1:1—
//* SFAUE28 SORT FAUE28-C-ADH ASC [2:9—
//* SFAUE28 SORT FAUE28-C-ORDRE ASC [11:4—
//* SFAUE28 SORT FAUE28-CAS-LCI ASC [15:1—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYMIG.FAUE28
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE28.FAUE28
//SORTOUT  DD DSN=&MIG.AUE23.FAUE28,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,1,CH,A,
               2,9,CH,A,
               11,4,CH,A,
               15,1,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL2302       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STAUELDI DD DSN=&CIB.TAUELDI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAUELCI DD DSN=&CIB.TAUELCI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL2303       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE23,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE23,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE23,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTABADH  DD DSN=&MIG.AUE23.TABADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01TIG  DD DSN=&MIG.AUE23.A01TIG.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE15  DD DSN=&MIG.AUE23.FAUE15.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFAUE28  DD DSN=&MIG.AUE23.FAUE28.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUELDI DD DSN=&MIG.AUE23.TAUELDI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUELCI DD DSN=&MIG.AUE23.TAUELCI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE23
//*----------------------------------------------------------------
//PPRAUE23 EXEC PGM=PPRAUE23,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE23,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE23,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE23,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STABADH  DD DISP=SHR,DSN=&MIG.AUE23.TABADH
//SA01TIG  DD DISP=SHR,DSN=&MIG.AUE23.A01TIG
//SFAUE15  DD DISP=SHR,DSN=&MIG.AUE23.FAUE15
//SFAUE28  DD DISP=SHR,DSN=&MIG.AUE23.FAUE28
//*--------------<FICHIERS CIBLES>---------------------------------
//STAUELDI DD DSN=&CIB.TAUELDI,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STAUELCI DD DSN=&CIB.TAUELCI,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTABADH  DD DSN=&MIG.AUE23.TABADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01TIG  DD DSN=&MIG.AUE23.A01TIG.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE15  DD DSN=&MIG.AUE23.FAUE15.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFAUE28  DD DSN=&MIG.AUE23.FAUE28.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUELDI DD DSN=&MIG.AUE23.TAUELDI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUELCI DD DSN=&MIG.AUE23.TAUELCI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHAUE50
//*----------------------------------------------------------------
//DEL5001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STAUEPAC DD DSN=&CIB.TAUEPAC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TAUEPA
//*----------------------------------------------------------------
//SORT5001 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STAUEPA SORT TAUEPAC-ID-TECH-UR-ADHR ASC ¬1:20¦
//* STAUEPA SORT TAUEPAC-NO-ORD-ADHS ASC ¬21:3¦
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//* ALA : ajout copy
//COPY     DD DISP=SHR,DSN=&CPYCIB.TAUEPAC
//SORTIN   DD DISP=SHR,DSN=&MIG.AUE31.TAUEPAC
//SORTOUT  DD DSN=&CIB.TAUEPAC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               24,12,CH,A)
  SUM FIELDS=NONE
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL5002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SBACAUE  DD DSN=&CIB.BACAUE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL5003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRAUE50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRAUE50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRAUE50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUEUEM DD DSN=&MIG.AUE50.TAUEUEM.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUEUED DD DSN=&MIG.AUE50.TAUEUED.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUEUEC DD DSN=&MIG.AUE50.TAUEUEC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUERPD DD DSN=&MIG.AUE50.TAUERPD.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUEREP DD DSN=&MIG.AUE50.TAUEREP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUERDM DD DSN=&MIG.AUE50.TAUERDM.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUERCM DD DSN=&MIG.AUE50.TAUERCM.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUERDC DD DSN=&MIG.AUE50.TAUERDC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUEPOP DD DSN=&MIG.AUE50.TAUEPOP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUEMUM DD DSN=&MIG.AUE50.TAUEMUM.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUEMUD DD DSN=&MIG.AUE50.TAUEMUD.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUEMUC DD DSN=&MIG.AUE50.TAUEMUC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUEMPO DD DSN=&MIG.AUE50.TAUEMPO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUELDI DD DSN=&MIG.AUE50.TAUELDI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUELCI DD DSN=&MIG.AUE50.TAUELCI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAUEECA DD DSN=&MIG.AUE50.TAUEECA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZBACAUE  DD DSN=&MIG.AUE50.BACAUE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRAUE50
//*----------------------------------------------------------------
//PPRAUE50 EXEC PGM=PPRAUE50,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRAUE50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRAUE50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRAUE50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//TAUEUEM  DD DISP=SHR,DSN=&CIB.TAUEUEM
//TAUEUED  DD DISP=SHR,DSN=&CIB.TAUEUED
//TAUEUEC  DD DISP=SHR,DSN=&CIB.TAUEUEC
//TAUERPD  DD DISP=SHR,DSN=&CIB.TAUERPD
//TAUEREP  DD DISP=SHR,DSN=&CIB.TAUEREP
//TAUERDM  DD DISP=SHR,DSN=&CIB.TAUERDM
//TAUERCM  DD DISP=SHR,DSN=&CIB.TAUERCM
//TAUERDC  DD DISP=SHR,DSN=&CIB.TAUERDC
//TAUEPOP  DD DISP=SHR,DSN=&CIB.TAUEPOP
//TAUEMUM  DD DISP=SHR,DSN=&CIB.TAUEMUM
//TAUEMUD  DD DISP=SHR,DSN=&CIB.TAUEMUD
//TAUEMUC  DD DISP=SHR,DSN=&CIB.TAUEMUC
//TAUEMPO  DD DISP=SHR,DSN=&CIB.TAUEMPO
//TAUELDI  DD DISP=SHR,DSN=&CIB.TAUELDI
//TAUELCI  DD DISP=SHR,DSN=&CIB.TAUELCI
//TAUEECA  DD DISP=SHR,DSN=&CIB.TAUEECA
//TAUEPAC  DD DISP=SHR,DSN=&CIB.TAUEPAC
//*--------------<FICHIERS CIBLES>---------------------------------
//SBACAUE  DD DSN=&CIB.BACAUE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUEUEM DD DSN=&MIG.AUE50.TAUEUEM.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUEUED DD DSN=&MIG.AUE50.TAUEUED.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUEUEC DD DSN=&MIG.AUE50.TAUEUEC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUERPD DD DSN=&MIG.AUE50.TAUERPD.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUEREP DD DSN=&MIG.AUE50.TAUEREP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUERDM DD DSN=&MIG.AUE50.TAUERDM.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUERCM DD DSN=&MIG.AUE50.TAUERCM.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUERDC DD DSN=&MIG.AUE50.TAUERDC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUEPOP DD DSN=&MIG.AUE50.TAUEPOP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUEMUM DD DSN=&MIG.AUE50.TAUEMUM.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUEMUD DD DSN=&MIG.AUE50.TAUEMUD.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUEMUC DD DSN=&MIG.AUE50.TAUEMUC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUEMPO DD DSN=&MIG.AUE50.TAUEMPO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUELDI DD DSN=&MIG.AUE50.TAUELDI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUELCI DD DSN=&MIG.AUE50.TAUELCI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAUEECA DD DSN=&MIG.AUE50.TAUEECA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZBACAUE  DD DSN=&MIG.AUE50.BACAUE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//* FIN DU JCL DE MIGRATION