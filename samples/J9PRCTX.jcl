//J9PRCTX  JOB UTI00TX0,'PR GRECCO G06-CTX',CLASS=Z,MSGCLASS=X,
//*         RESTART=DEL0101,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=PLURM.MOVE.PR.JCL.PROD
//MEMBER   INCLUDE MEMBER=$$PRT
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCTX01
//*----------------------------------------------------------------
//DEL0101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SC02PER  DD DSN=&MIG.CTX01.C02PER,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB05PER  DD DSN=&MIG.CTX01.B05PER,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER C02PER
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SC02PER SORT C02-PER-DEB-CTX-C-ID-PER-DEBIT ASC [15:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.C02PERDE
//SORTIN   DD DISP=SHR,DSN=&SRC.C02PERDE
//SORTOUT  DD DSN=&MIG.CTX01.C02PER,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(15,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B05PER
//*----------------------------------------------------------------
//SORT0102 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05PER SORT B05-PER-DEB-C-ID-PER-DEBIT ASC [30:12—
//* SB05PER SORT B05-PER-DEB-C-ID-INST ASC [43:7—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.B05PERIO
//SORTIN   DD DISP=SHR,DSN=&SRC.B05PERIO
//SORTOUT  DD DSN=&MIG.CTX01.B05PER,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(30,12,CH,A,
               43,7,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCTX01  DD DSN=&MIG.CTX01.FCTX01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCTX01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCTX01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCTX01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZC02PER  DD DSN=&MIG.CTX01.C02PER.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZB05PER  DD DSN=&MIG.CTX01.B05PER.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCTX01  DD DSN=&MIG.CTX01.FCTX01.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCTX01
//*----------------------------------------------------------------
//PPRCTX01 EXEC PGM=PPRCTX01,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCTX01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCTX01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCTX01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SC02PER  DD DISP=SHR,DSN=&MIG.CTX01.C02PER
//SB05PER  DD DISP=SHR,DSN=&MIG.CTX01.B05PER
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCTX01  DD DSN=&MIG.CTX01.FCTX01,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZC02PER  DD DSN=&MIG.CTX01.C02PER.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZB05PER  DD DSN=&MIG.CTX01.B05PER.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCTX01  DD DSN=&MIG.CTX01.FCTX01.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCTX02
//*----------------------------------------------------------------
//DEL0201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SC02DOSS DD DSN=&MIG.CTX02.C02DOSS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCTX01  DD DSN=&MIG.CTX02.FCTX01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SC02EVEP DD DSN=&MIG.CTX02.C02EVEP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER C02DOSS
//*----------------------------------------------------------------
//SORT0201 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SC02DOSS SORT C02-DOSS-CTX-C-ID-DOSS ASC [19:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.C02DOSS
//*SORTIN   DD DISP=SHR,DSN=&SRC.C02DOSS
//SORTIN   DD DISP=SHR,DSN=&SRC.C02DOSS
//SORTOUT  DD DSN=&MIG.CTX02.C02DOSS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(19,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FCTX01
//*----------------------------------------------------------------
//SORT0202 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCTX01 SORT FCTX01-C-ID-DOSS ASC [13:12—
//* SFCTX01 SORT FCTX01-C-ID-INST ASC [37:7—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCTX01
//SORTIN   DD DISP=SHR,DSN=&MIG.CTX01.FCTX01
//SORTOUT  DD DSN=&MIG.CTX02.FCTX01,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(13,12,CH,A,
               37,7,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER C02EVEP
//*----------------------------------------------------------------
//SORT0203 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SC02EVEP SORT C02-EVE-PROC-C-ID-DOSS ASC [347:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.C02EVEPR
//SORTIN   DD DISP=SHR,DSN=&SRC.C02EVEPR
//SORTOUT  DD DSN=&MIG.CTX02.C02EVEP,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(347,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0202       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCTX02  DD DSN=&MIG.CTX02.FCTX02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCEVEPR2 DD DSN=&MIG.CTX02.CEVEPR2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCTX02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCTX02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCTX02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZL01INST DD DSN=&MIG.CTX02.L01INST.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZC02DOSS DD DSN=&MIG.CTX02.C02DOSS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCTX01  DD DSN=&MIG.CTX02.FCTX01.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZC02EVEP DD DSN=&MIG.CTX02.C02EVEP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCTX02  DD DSN=&MIG.CTX02.FCTX02.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCEVEPR2 DD DSN=&MIG.CTX02.CEVEPR2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCTX02
//*----------------------------------------------------------------
//PPRCTX02 EXEC PGM=PPRCTX02,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCTX02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCTX02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCTX02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//L01INST  DD DISP=SHR,DSN=&SRC.L01INSTI
//SC02DOSS DD DISP=SHR,DSN=&MIG.CTX02.C02DOSS
//SFCTX01  DD DISP=SHR,DSN=&MIG.CTX02.FCTX01
//SC02EVEP DD DISP=SHR,DSN=&MIG.CTX02.C02EVEP
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCTX02  DD DSN=&MIG.CTX02.FCTX02,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SCEVEPR2 DD DSN=&MIG.CTX02.CEVEPR2,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZL01INST DD DSN=&MIG.CTX02.L01INST.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZC02DOSS DD DSN=&MIG.CTX02.C02DOSS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCTX01  DD DSN=&MIG.CTX02.FCTX01.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZC02EVEP DD DSN=&MIG.CTX02.C02EVEP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCTX02  DD DSN=&MIG.CTX02.FCTX02.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCEVEPR2 DD DSN=&MIG.CTX02.CEVEPR2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCTX0A
//*----------------------------------------------------------------
//DEL0A01  EXEC PGM=IEFBR14
//SYSPRINT DD SYSOUT=*
//C02HIST  DD DSN=&MIG.CTX0A.C02HIST,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER C02HIST
//*----------------------------------------------------------------
//SORT0A01 EXEC PGM=SORT
//* DEBUT CRITERE XGEN
//* C02HIST SORT C02-H-ETAT-DOSS-C-ID-DOSS ASC [16:12]
//* C02HIST SORT C02-H-ETAT-DOSS-DH-DEB-EFFET ASC [117:8]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.C02HISTE
//COPY     DD DISP=SHR,DSN=&CPYSRC.C02HISTE
//SORTOUT  DD DSN=&MIG.CTX0A.C02HIST,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,12,CH,A,
               117,8,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0A02  EXEC PGM=IEFBR14
//SYSPRINT DD SYSOUT=*
//*SCHISTE1 DD DSN=&CIB.CHISTE1,
//SCHISTE1 DD DSN=&MIG.CTX0A.CHISTE1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZC02HIST DD DSN=&MIG.CTX0A.C02HIST.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCHISTE1 DD DSN=&MIG.CTX0A.CHISTE1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0A03       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCTX0A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCTX0A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCTX0A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION
//*----------------------------------------------------------------
//PPRCTX0A EXEC PGM=PPRCTX0A
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCTX0A,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCTX0A,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5000,500),RLSE)
//FICTRACE DD DSN=&TRC.PPRCTX0A,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5000,500),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DUMMY
//*--------------<FICHIERS SOURCES>--------------------------------
//*C02HIST  DD DISP=SHR,DSN=&SRC.C02HIST
//C02HIST  DD DISP=SHR,DSN=&MIG.CTX0A.C02HIST
//*--------------<FICHIERS CIBLES>---------------------------------
//*SCHISTE1 DD DSN=&CIB.CHISTE1,
//SCHISTE1 DD DSN=&MIG.CTX0A.CHISTE1,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,500),RLSE)
//*--------------<FICHIERS REDUITS>--------------------------------
//ZC02HIST DD DSN=&MIG.CTX0A.C02HIST.R,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(10,50),RLSE)
//ZCHISTE1 DD DSN=&MIG.CTX0A.CHISTE1.R,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(10,50),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCTX03
//*----------------------------------------------------------------
//DEL0301       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SC02HIST DD DSN=&MIG.CTX03.C02HIST,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SC02SUIV DD DSN=&MIG.CTX03.C02SUIV,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SC02CREA DD DSN=&MIG.CTX03.C02CREA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SC02SUIP DD DSN=&MIG.CTX03.C02SUIP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SC02TIER DD DSN=&MIG.CTX03.C02TIER,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCTX02  DD DSN=&MIG.CTX03.FCTX02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SC02DCTX DD DSN=&MIG.CTX03.C02DCTX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER C02HIST
//*----------------------------------------------------------------
//SORT0301 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SC02HIST SORT C02-H-ETAT-DOSS-C-ID-DOSS ASC [16:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.CHISTE2
//SORTIN   DD DISP=SHR,DSN=&MIG.CTX0A.CHISTE1
//SORTOUT  DD DSN=&MIG.CTX03.C02HIST,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER C02SUIV
//*----------------------------------------------------------------
//SORT0302 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SC02SUIV SORT C02-SUIVI-JUG-C-ID-DOSS ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.C02SUIVI
//SORTIN   DD DISP=SHR,DSN=&SRC.C02SUIVI
//SORTOUT  DD DSN=&MIG.CTX03.C02SUIV,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER C02CREA
//*----------------------------------------------------------------
//SORT0303 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SC02CREA SORT C02-CREANCES-C-ID-DOSS ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.C02CREAN
//SORTIN   DD DISP=SHR,DSN=&SRC.C02CREAN
//SORTOUT  DD DSN=&MIG.CTX03.C02CREA,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER C02SUIP
//*----------------------------------------------------------------
//SORT0304 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SC02SUIP SORT C02-SUI-PROC-IND-C-ID-DOSS ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.C02SUIVP
//SORTIN   DD DISP=SHR,DSN=&SRC.C02SUIVP
//SORTOUT  DD DSN=&MIG.CTX03.C02SUIP,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER C02TIER
//*----------------------------------------------------------------
//SORT0305 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SC02TIER SORT C02-TIERS-JUR-C-ID-DOSS ASC [50:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.C02TIERS
//SORTIN   DD DISP=SHR,DSN=&SRC.C02TIERS
//SORTOUT  DD DSN=&MIG.CTX03.C02TIER,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(50,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FCTX02
//*----------------------------------------------------------------
//SORT0306 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCTX02 SORT FCTX02-C-ID-DOSS ASC [13:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCTX02
//SORTIN   DD DISP=SHR,DSN=&MIG.CTX02.FCTX02
//SORTOUT  DD DSN=&MIG.CTX03.FCTX02,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(13,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER C02DCTX
//*----------------------------------------------------------------
//SORT0307 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SC02DCTX SORT C02-DAS-CTX-C-ID-DOSS ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.C02DCTX
//SORTIN   DD DISP=SHR,DSN=&SRC.C02DCTX
//SORTOUT  DD DSN=&MIG.CTX03.C02DCTX,
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
//SCHISTE2 DD DSN=&MIG.CTX03.CHISTE2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCSUIVI2 DD DSN=&MIG.CTX03.CSUIVI2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCCREAN2 DD DSN=&MIG.CTX03.CCREAN2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCSUIVP2 DD DSN=&MIG.CTX03.CSUIVP2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCTIERJ2 DD DSN=&MIG.CTX03.CTIERJ2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCDCTX2  DD DSN=&MIG.CTX03.CDCTX2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0303       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCTX03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCTX03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCTX03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZC02HIST DD DSN=&MIG.CTX03.C02HIST.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZC02SUIV DD DSN=&MIG.CTX03.C02SUIV.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZC02CREA DD DSN=&MIG.CTX03.C02CREA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZC02SUIP DD DSN=&MIG.CTX03.C02SUIP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZC02TIER DD DSN=&MIG.CTX03.C02TIER.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCTX02  DD DSN=&MIG.CTX03.FCTX02.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZC02DCTX DD DSN=&MIG.CTX03.C02DCTX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCHISTE2 DD DSN=&MIG.CTX03.CHISTE2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCSUIVI2 DD DSN=&MIG.CTX03.CSUIVI2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCCREAN2 DD DSN=&MIG.CTX03.CCREAN2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCSUIVP2 DD DSN=&MIG.CTX03.CSUIVP2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCTIERJ2 DD DSN=&MIG.CTX03.CTIERJ2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCDCTX2  DD DSN=&MIG.CTX03.CDCTX2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCTX03
//*----------------------------------------------------------------
//PPRCTX03 EXEC PGM=PPRCTX03,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCTX03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCTX03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCTX03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SCHISTE1 DD DISP=SHR,DSN=&MIG.CTX03.C02HIST
//SC02SUIV DD DISP=SHR,DSN=&MIG.CTX03.C02SUIV
//SC02CREA DD DISP=SHR,DSN=&MIG.CTX03.C02CREA
//SC02SUIP DD DISP=SHR,DSN=&MIG.CTX03.C02SUIP
//SC02TIER DD DISP=SHR,DSN=&MIG.CTX03.C02TIER
//SFCTX02  DD DISP=SHR,DSN=&MIG.CTX03.FCTX02
//SC02DCTX DD DISP=SHR,DSN=&MIG.CTX03.C02DCTX
//*--------------<FICHIERS CIBLES>---------------------------------
//SCHISTE2 DD DSN=&MIG.CTX03.CHISTE2,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SCSUIVI2 DD DSN=&MIG.CTX03.CSUIVI2,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SCCREAN2 DD DSN=&MIG.CTX03.CCREAN2,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SCSUIVP2 DD DSN=&MIG.CTX03.CSUIVP2,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SCTIERJ2 DD DSN=&MIG.CTX03.CTIERJ2,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SCDCTX2  DD DSN=&MIG.CTX03.CDCTX2,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZC02HIST DD DSN=&MIG.CTX03.C02HIST.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZC02SUIV DD DSN=&MIG.CTX03.C02SUIV.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZC02CREA DD DSN=&MIG.CTX03.C02CREA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZC02SUIP DD DSN=&MIG.CTX03.C02SUIP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZC02TIER DD DSN=&MIG.CTX03.C02TIER.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCTX02  DD DSN=&MIG.CTX03.FCTX02.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZC02DCTX DD DSN=&MIG.CTX03.C02DCTX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCHISTE2 DD DSN=&MIG.CTX03.CHISTE2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCSUIVI2 DD DSN=&MIG.CTX03.CSUIVI2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCCREAN2 DD DSN=&MIG.CTX03.CCREAN2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCSUIVP2 DD DSN=&MIG.CTX03.CSUIVP2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCTIERJ2 DD DSN=&MIG.CTX03.CTIERJ2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCDCTX2  DD DSN=&MIG.CTX03.CDCTX2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCTX04
//*----------------------------------------------------------------
//DEL0401       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STADHADH DD DSN=&MIG.CTX04.TADHADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCTX02  DD DSN=&MIG.CTX04.FCTX02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TADHADH
//*----------------------------------------------------------------
//SORT0401 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STADHADH SORT ADHSADH-C-ID-PERS ASC [120:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TADHADH
//SORTIN   DD DISP=SHR,DSN=&CIB.TADHADH
//SORTOUT  DD DSN=&MIG.CTX04.TADHADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(120,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FCTX02
//*----------------------------------------------------------------
//SORT0402 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCTX02 SORT FCTX02-C-ID-PERS ASC [1:12—
//* SFCTX02 SORT FCTX02-D-CREATION ASC [25:8—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCTX02
//SORTIN   DD DISP=SHR,DSN=&MIG.CTX02.FCTX02
//SORTOUT  DD DSN=&MIG.CTX04.FCTX02,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A,
               25,8,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0402       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SPERSCTX DD DSN=&CIB.PERSCTX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCTX03  DD DSN=&MIG.CTX04.FCTX03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0403       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCTX04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCTX04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCTX04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTADHADH DD DSN=&MIG.CTX04.TADHADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCTX02  DD DSN=&MIG.CTX04.FCTX02.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZPERSCTX DD DSN=&MIG.CTX04.PERSCTX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCTX03  DD DSN=&MIG.CTX04.FCTX03.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCTX04
//*----------------------------------------------------------------
//PPRCTX04 EXEC PGM=PPRCTX04,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCTX04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCTX04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCTX04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STADHADH DD DISP=SHR,DSN=&MIG.CTX04.TADHADH
//SFCTX02  DD DISP=SHR,DSN=&MIG.CTX04.FCTX02
//*--------------<FICHIERS CIBLES>---------------------------------
//SPERSCTX DD DSN=&CIB.PERSCTX,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFCTX03  DD DSN=&MIG.CTX04.FCTX03,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTADHADH DD DSN=&MIG.CTX04.TADHADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCTX02  DD DSN=&MIG.CTX04.FCTX02.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZPERSCTX DD DSN=&MIG.CTX04.PERSCTX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCTX03  DD DSN=&MIG.CTX04.FCTX03.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCTX05
//*----------------------------------------------------------------
//DEL0501       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SPERSCTX DD DSN=&MIG.CTX05.PERSCTX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCTX02  DD DSN=&MIG.CTX05.FCTX02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCTX03  DD DSN=&MIG.CTX05.FCTX03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCHISTE2 DD DSN=&MIG.CTX05.CHISTE2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCSUIVI2 DD DSN=&MIG.CTX05.CSUIVI2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCCREAN2 DD DSN=&MIG.CTX05.CCREAN2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCSUIVP2 DD DSN=&MIG.CTX05.CSUIVP2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCTIERJ2 DD DSN=&MIG.CTX05.CTIERJ2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCEVEPR2 DD DSN=&MIG.CTX05.CEVEPR2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCDCTX2  DD DSN=&MIG.CTX05.CDCTX2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER PERSCTX
//*----------------------------------------------------------------
//SORT0501 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SPERSCTX SORT TAB-PERS-CTX-C-ID-PERS ASC [1:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.PERSCTX
//SORTIN   DD DISP=SHR,DSN=&CIB.PERSCTX
//SORTOUT  DD DSN=&MIG.CTX05.PERSCTX,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FCTX02
//*----------------------------------------------------------------
//SORT0502 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCTX02 SORT FCTX02-C-ID-PERS ASC [1:12—
//* SFCTX02 SORT FCTX02-D-CREATION ASC [25:8—
//* SFCTX02 SORT FCTX02-C-ID-DOSS ASC [13:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCTX02
//SORTIN   DD DISP=SHR,DSN=&MIG.CTX02.FCTX02
//SORTOUT  DD DSN=&MIG.CTX05.FCTX02,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A,
               25,8,CH,A,
               13,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FCTX03
//*----------------------------------------------------------------
//SORT0503 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCTX03 SORT FCTX03-C-ID-PERS ASC [1:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCTX03
//SORTIN   DD DISP=SHR,DSN=&MIG.CTX04.FCTX03
//SORTOUT  DD DSN=&MIG.CTX05.FCTX03,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER CHISTE2
//*----------------------------------------------------------------
//SORT0504 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCHISTE2 SORT CHISTE2-C-ID-PERS ASC [166:12—
//* SCHISTE2 SORT CHISTE2-DH-CREATION ASC [28:18—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.CHISTE2
//SORTIN   DD DISP=SHR,DSN=&MIG.CTX03.CHISTE2
//SORTOUT  DD DSN=&MIG.CTX05.CHISTE2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(136,12,CH,A,
               28,8,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER CSUIVI2
//*----------------------------------------------------------------
//SORT0505 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCSUIVI2 SORT CSUIVI2-C-ID-PERS ASC [594:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.CSUIVI2
//SORTIN   DD DISP=SHR,DSN=&MIG.CTX03.CSUIVI2
//SORTOUT  DD DSN=&MIG.CTX05.CSUIVI2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(594,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER CCREAN2
//*----------------------------------------------------------------
//SORT0506 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCCREAN2 SORT CCREAN2-C-ID-PERS ASC [595:12—
//* SCCREAN2 SORT CCREAN2-DH-ENREG-EVE ASC [14:18—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.CCREAN2
//SORTIN   DD DISP=SHR,DSN=&MIG.CTX03.CCREAN2
//SORTOUT  DD DSN=&MIG.CTX05.CCREAN2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(595,12,CH,A,
               14,18,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER CSUIVP2
//*----------------------------------------------------------------
//SORT0507 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCSUIVP2 SORT CSUIVP2-C-ID-PERS ASC [578:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.CSUIVP2
//SORTIN   DD DISP=SHR,DSN=&MIG.CTX03.CSUIVP2
//SORTOUT  DD DSN=&MIG.CTX05.CSUIVP2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(578,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER CTIERJ2
//*----------------------------------------------------------------
//SORT0508 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCTIERJ2 SORT CTIERJ2-C-ID-PERS2 ASC [95:12—
//* SCTIERJ2 SORT CTIERJ2-C-ID-ROLE ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.CTIERJ2
//SORTIN   DD DISP=SHR,DSN=&MIG.CTX03.CTIERJ2
//SORTOUT  DD DSN=&MIG.CTX05.CTIERJ2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(95,12,CH,A,
               2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER CEVEPR2
//*----------------------------------------------------------------
//SORT0509 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCEVEPR2 SORT CEVEPR2-C-ID-PERS2 ASC [412:12—
//* SCEVEPR2 SORT CEVEPR2-DH-ENREG-EVE ASC [359:18—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.CEVEPR2
//SORTIN   DD DISP=SHR,DSN=&MIG.CTX02.CEVEPR2
//SORTOUT  DD DSN=&MIG.CTX05.CEVEPR2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(412,12,CH,A,
               359,18,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER CDCTX2
//*----------------------------------------------------------------
//SORT0510 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCDCTX2 SORT CDCTX2-C-ID-PERS ASC [36:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.CDCTX2
//SORTIN   DD DISP=SHR,DSN=&MIG.CTX03.CDCTX2
//SORTOUT  DD DSN=&MIG.CTX05.CDCTX2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(36,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0502       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SDOSSCTX DD DSN=&CIB.DOSSCTX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPCG     DD DSN=&MIG.CTX05.PCG,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SHPS     DD DSN=&MIG.CTX05.HPS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SEVX     DD DSN=&MIG.CTX05.EVX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPAX     DD DSN=&MIG.CTX05.PAX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPTX     DD DSN=&MIG.CTX05.PTX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPPC     DD DSN=&CIB.PPC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SHEP     DD DSN=&CIB.HEP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPGE     DD DSN=&MIG.CTX05.PGE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SECX     DD DSN=&CIB.ECX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0503       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCTX05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCTX05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCTX05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZL01GEST DD DSN=&MIG.CTX05.L01GEST.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01AUDI DD DSN=&MIG.CTX05.A01AUDI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZC02EVEN DD DSN=&MIG.CTX05.C02EVEN.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTGESTEG DD DSN=&MIG.CTX05.TGESTEG.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTGREVX  DD DSN=&MIG.CTX05.TGREVX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPTIE01 DD DSN=&MIG.CTX05.FPTIE01.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZHISTE2  DD DSN=&MIG.CTX05.HISTE2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTGRTIEJ DD DSN=&MIG.CTX05.TGRTIEJ.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZPERSCTX DD DSN=&MIG.CTX05.PERSCTX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCTX02  DD DSN=&MIG.CTX05.FCTX02.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCTX03  DD DSN=&MIG.CTX05.FCTX03.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCHISTE2 DD DSN=&MIG.CTX05.CHISTE2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCSUIVI2 DD DSN=&MIG.CTX05.CSUIVI2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCCREAN2 DD DSN=&MIG.CTX05.CCREAN2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCSUIVP2 DD DSN=&MIG.CTX05.CSUIVP2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCTIERJ2 DD DSN=&MIG.CTX05.CTIERJ2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCEVEPR2 DD DSN=&MIG.CTX05.CEVEPR2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCDCTX2  DD DSN=&MIG.CTX05.CDCTX2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZDOSSCTX DD DSN=&MIG.CTX05.DOSSCTX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZPCG     DD DSN=&MIG.CTX05.PCG.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZHPS     DD DSN=&MIG.CTX05.HPS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZEVX     DD DSN=&MIG.CTX05.EVX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZPAX     DD DSN=&MIG.CTX05.PAX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZPTX     DD DSN=&MIG.CTX05.PTX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZPPC     DD DSN=&MIG.CTX05.PPC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZHEP     DD DSN=&MIG.CTX05.HEP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZPGE     DD DSN=&MIG.CTX05.PGE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZECX     DD DSN=&MIG.CTX05.ECX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCTX05
//*----------------------------------------------------------------
//PPRCTX05 EXEC PGM=PPRCTX05,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCTX05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCTX05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCTX05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//L01GEST  DD DISP=SHR,DSN=&SRC.L01GESTI
//A01AUDI  DD DISP=SHR,DSN=&SRC.A01AUDIE
//C02EVEN  DD DISP=SHR,DSN=&SRC.C02EVENE
//TGESTEG  DD DISP=SHR,DSN=&TAB.TGESTEG
//TGREVX   DD DISP=SHR,DSN=&TAB.TGREVX
//A01TRIB  DD DISP=SHR,DSN=&SRC.A01TRIBU
//FPTIE01  DD DISP=SHR,DSN=&CIB.FPTIE01
//HISTE2   DD DISP=SHR,DSN=&MIG.CTX05.CHISTE2
//TGRTIEJ  DD DISP=SHR,DSN=&TAB.TGRTIEJ
//SPERSCTX DD DISP=SHR,DSN=&MIG.CTX05.PERSCTX
//SFCTX02  DD DISP=SHR,DSN=&MIG.CTX05.FCTX02
//SFCTX03  DD DISP=SHR,DSN=&MIG.CTX05.FCTX03
//SCHISTE2 DD DISP=SHR,DSN=&MIG.CTX05.CHISTE2
//SCSUIVI2 DD DISP=SHR,DSN=&MIG.CTX05.CSUIVI2
//SCCREAN2 DD DISP=SHR,DSN=&MIG.CTX05.CCREAN2
//SCSUIVP2 DD DISP=SHR,DSN=&MIG.CTX05.CSUIVP2
//SCTIERJ2 DD DISP=SHR,DSN=&MIG.CTX05.CTIERJ2
//SCEVEPR2 DD DISP=SHR,DSN=&MIG.CTX05.CEVEPR2
//SCDCTX2  DD DISP=SHR,DSN=&MIG.CTX05.CDCTX2
//STDNDS2  DD DUMMY
//*--------------<FICHIERS CIBLES>---------------------------------
//SDOSSCTX DD DSN=&CIB.DOSSCTX,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SPCG     DD DSN=&MIG.CTX05.PCG,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SHPS     DD DSN=&MIG.CTX05.HPS,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SEVX     DD DSN=&MIG.CTX05.EVX,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SPAX     DD DSN=&MIG.CTX05.PAX,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SPTX     DD DSN=&MIG.CTX05.PTX,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SPPC     DD DSN=&CIB.PPC,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SHEP     DD DSN=&CIB.HEP,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SPGE     DD DSN=&MIG.CTX05.PGE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SECX     DD DSN=&CIB.ECX,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZL01GEST DD DSN=&MIG.CTX05.L01GEST.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01AUDI DD DSN=&MIG.CTX05.A01AUDI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZC02EVEN DD DSN=&MIG.CTX05.C02EVEN.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTGESTEG DD DSN=&MIG.CTX05.TGESTEG.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTGREVX  DD DSN=&MIG.CTX05.TGREVX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPTIE01 DD DSN=&MIG.CTX05.FPTIE01.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZHISTE2  DD DSN=&MIG.CTX05.HISTE2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTGRTIEJ DD DSN=&MIG.CTX05.TGRTIEJ.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZPERSCTX DD DSN=&MIG.CTX05.PERSCTX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCTX02  DD DSN=&MIG.CTX05.FCTX02.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCTX03  DD DSN=&MIG.CTX05.FCTX03.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCHISTE2 DD DSN=&MIG.CTX05.CHISTE2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCSUIVI2 DD DSN=&MIG.CTX05.CSUIVI2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCCREAN2 DD DSN=&MIG.CTX05.CCREAN2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCSUIVP2 DD DSN=&MIG.CTX05.CSUIVP2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCTIERJ2 DD DSN=&MIG.CTX05.CTIERJ2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCEVEPR2 DD DSN=&MIG.CTX05.CEVEPR2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCDCTX2  DD DSN=&MIG.CTX05.CDCTX2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZDOSSCTX DD DSN=&MIG.CTX05.DOSSCTX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZPCG     DD DSN=&MIG.CTX05.PCG.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZHPS     DD DSN=&MIG.CTX05.HPS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZEVX     DD DSN=&MIG.CTX05.EVX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZPAX     DD DSN=&MIG.CTX05.PAX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZPTX     DD DSN=&MIG.CTX05.PTX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZPPC     DD DSN=&MIG.CTX05.PPC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZHEP     DD DSN=&MIG.CTX05.HEP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZPGE     DD DSN=&MIG.CTX05.PGE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZECX     DD DSN=&MIG.CTX05.ECX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCTX06
//*----------------------------------------------------------------
//DEL0601       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SDOSSCTX DD DSN=&MIG.CTX06.DOSSCTX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCTX02  DD DSN=&MIG.CTX06.FCTX02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCEVEPR2 DD DSN=&MIG.CTX06.CEVEPR2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCTIERJ2 DD DSN=&MIG.CTX06.CTIERJ2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCSUIVI2 DD DSN=&MIG.CTX06.CSUIVI2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCSUIVP2 DD DSN=&MIG.CTX06.CSUIVP2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCHISTE2 DD DSN=&MIG.CTX06.CHISTE2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER DOSSCTX
//*----------------------------------------------------------------
//SORT0601 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SDOSSCTX SORT TAB-GR-DOSSCTX-C-ID-DOSS ASC [1:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.DOSSCTX
//SORTIN   DD DISP=SHR,DSN=&CIB.DOSSCTX
//SORTOUT  DD DSN=&MIG.CTX06.DOSSCTX,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FCTX02
//*----------------------------------------------------------------
//SORT0602 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCTX02 SORT FCTX02-C-ID-DOSS ASC [13:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCTX02
//SORTIN   DD DISP=SHR,DSN=&MIG.CTX02.FCTX02
//SORTOUT  DD DSN=&MIG.CTX06.FCTX02,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(13,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER CEVEPR2
//*----------------------------------------------------------------
//SORT0603 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCEVEPR2 SORT CEVEPR2-C-ID-DOSS ASC [347:12—
//* SCEVEPR2 SORT CEVEPR2-DH-ENREG-EVE ASC [359:18—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.CEVEPR2
//SORTIN   DD DISP=SHR,DSN=&MIG.CTX02.CEVEPR2
//SORTOUT  DD DSN=&MIG.CTX06.CEVEPR2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(347,12,CH,A,
               359,18,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER CTIERJ2
//*----------------------------------------------------------------
//SORT0604 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCTIERJ2 SORT CTIERJ2-C-ID-DOSS ASC [50:12—
//* SCTIERJ2 SORT CTIERJ2-C-ID-ROLE ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.CTIERJ2
//SORTIN   DD DISP=SHR,DSN=&MIG.CTX03.CTIERJ2
//SORTOUT  DD DSN=&MIG.CTX06.CTIERJ2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(50,12,CH,A,
               2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER CSUIVI2
//*----------------------------------------------------------------
//SORT0605 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCSUIVI2 SORT CSUIVI2-C-ID-DOSS ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.CSUIVI2
//SORTIN   DD DISP=SHR,DSN=&MIG.CTX03.CSUIVI2
//SORTOUT  DD DSN=&MIG.CTX06.CSUIVI2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER CSUIVP2
//*----------------------------------------------------------------
//SORT0606 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCSUIVP2 SORT CSUIVP2-C-ID-DOSS ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.CSUIVP2
//SORTIN   DD DISP=SHR,DSN=&MIG.CTX03.CSUIVP2
//SORTOUT  DD DSN=&MIG.CTX06.CSUIVP2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER CHISTE2
//*----------------------------------------------------------------
//SORT0607 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCHISTE2 SORT CHISTE2-C-ID-DOSS ASC [16:12—
//* SCHISTE2 SORT CHISTE2-DH-CREATION ASC [28:18—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.CHISTE2
//SORTIN   DD DISP=SHR,DSN=&MIG.CTX03.CHISTE2
//SORTOUT  DD DSN=&MIG.CTX06.CHISTE2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,12,CH,A,
               28,18,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0602       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SPCG     DD DSN=&MIG.CTX06.PCG,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SHPS     DD DSN=&MIG.CTX06.HPS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SEVX     DD DSN=&MIG.CTX06.EVX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPAX     DD DSN=&MIG.CTX06.PAX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPTX     DD DSN=&MIG.CTX06.PTX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPGE     DD DSN=&MIG.CTX06.PGE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SACX     DD DSN=&CIB.ACX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SLAA     DD DSN=&CIB.LAA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPAD     DD DSN=&CIB.PAD,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0603       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCTX06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCTX06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCTX06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01AUDI DD DSN=&MIG.CTX06.A01AUDI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPTIE01 DD DSN=&MIG.CTX06.FPTIE01.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZC02EVEN DD DSN=&MIG.CTX06.C02EVEN.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZL01GEST DD DSN=&MIG.CTX06.L01GEST.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTGREVX  DD DSN=&MIG.CTX06.TGREVX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTGESTEG DD DSN=&MIG.CTX06.TGESTEG.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTGRTIEJ DD DSN=&MIG.CTX06.TGRTIEJ.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZDOSSCTX DD DSN=&MIG.CTX06.DOSSCTX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCTX02  DD DSN=&MIG.CTX06.FCTX02.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCEVEPR2 DD DSN=&MIG.CTX06.CEVEPR2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCTIERJ2 DD DSN=&MIG.CTX06.CTIERJ2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCSUIVI2 DD DSN=&MIG.CTX06.CSUIVI2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCSUIVP2 DD DSN=&MIG.CTX06.CSUIVP2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCHISTE2 DD DSN=&MIG.CTX06.CHISTE2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZPCG     DD DSN=&MIG.CTX06.PCG.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZHPS     DD DSN=&MIG.CTX06.HPS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZEVX     DD DSN=&MIG.CTX06.EVX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZPAX     DD DSN=&MIG.CTX06.PAX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZPTX     DD DSN=&MIG.CTX06.PTX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZPGE     DD DSN=&MIG.CTX06.PGE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZACX     DD DSN=&MIG.CTX06.ACX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZLAA     DD DSN=&MIG.CTX06.LAA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZPAD     DD DSN=&MIG.CTX06.PAD.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCTX06
//*----------------------------------------------------------------
//PPRCTX06 EXEC PGM=PPRCTX06,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCTX06,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCTX06,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCTX06,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//A01AUDI  DD DISP=SHR,DSN=&SRC.A01AUDIE
//FPTIE01  DD DISP=SHR,DSN=&CIB.FPTIE01
//C02EVEN  DD DISP=SHR,DSN=&SRC.C02EVENE
//L01GEST  DD DISP=SHR,DSN=&SRC.L01GESTI
//TGREVX   DD DISP=SHR,DSN=&TAB.TGREVX
//TGESTEG  DD DISP=SHR,DSN=&TAB.TGESTEG
//TGRTIEJ  DD DISP=SHR,DSN=&TAB.TGRTIEJ
//SDOSSCTX DD DISP=SHR,DSN=&MIG.CTX06.DOSSCTX
//SFCTX02  DD DISP=SHR,DSN=&MIG.CTX06.FCTX02
//SCEVEPR2 DD DISP=SHR,DSN=&MIG.CTX06.CEVEPR2
//SCTIERJ2 DD DISP=SHR,DSN=&MIG.CTX06.CTIERJ2
//SCSUIVI2 DD DISP=SHR,DSN=&MIG.CTX06.CSUIVI2
//SCSUIVP2 DD DISP=SHR,DSN=&MIG.CTX06.CSUIVP2
//SCHISTE2 DD DISP=SHR,DSN=&MIG.CTX06.CHISTE2
//*--------------<FICHIERS CIBLES>---------------------------------
//SPCG     DD DSN=&MIG.CTX06.PCG,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SHPS     DD DSN=&MIG.CTX06.HPS,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SEVX     DD DSN=&MIG.CTX06.EVX,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SPAX     DD DSN=&MIG.CTX06.PAX,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SPTX     DD DSN=&MIG.CTX06.PTX,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SPGE     DD DSN=&MIG.CTX06.PGE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SACX     DD DSN=&CIB.ACX,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SLAA     DD DSN=&CIB.LAA,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SPAD     DD DSN=&CIB.PAD,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01AUDI DD DSN=&MIG.CTX06.A01AUDI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPTIE01 DD DSN=&MIG.CTX06.FPTIE01.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZC02EVEN DD DSN=&MIG.CTX06.C02EVEN.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZL01GEST DD DSN=&MIG.CTX06.L01GEST.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTGREVX  DD DSN=&MIG.CTX06.TGREVX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTGESTEG DD DSN=&MIG.CTX06.TGESTEG.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTGRTIEJ DD DSN=&MIG.CTX06.TGRTIEJ.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZDOSSCTX DD DSN=&MIG.CTX06.DOSSCTX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCTX02  DD DSN=&MIG.CTX06.FCTX02.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCEVEPR2 DD DSN=&MIG.CTX06.CEVEPR2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCTIERJ2 DD DSN=&MIG.CTX06.CTIERJ2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCSUIVI2 DD DSN=&MIG.CTX06.CSUIVI2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCSUIVP2 DD DSN=&MIG.CTX06.CSUIVP2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCHISTE2 DD DSN=&MIG.CTX06.CHISTE2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZPCG     DD DSN=&MIG.CTX06.PCG.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZHPS     DD DSN=&MIG.CTX06.HPS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZEVX     DD DSN=&MIG.CTX06.EVX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZPAX     DD DSN=&MIG.CTX06.PAX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZPTX     DD DSN=&MIG.CTX06.PTX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZPGE     DD DSN=&MIG.CTX06.PGE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZACX     DD DSN=&MIG.CTX06.ACX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZLAA     DD DSN=&MIG.CTX06.LAA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZPAD     DD DSN=&MIG.CTX06.PAD.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCTX09
//*----------------------------------------------------------------
//DEL0901       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SEPCG    DD DSN=&MIG.CTX09.EPCG,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFCTX02  DD DSN=&MIG.CTX09.FCTX02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER EPCG
//*----------------------------------------------------------------
//SORT0901 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SEPCG SORT EPCG-IDTECADH ASC [16:20—
//* SEPCG SORT EPCG-IDGRPPROCTX ASC [36:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.PCG
//SORTIN   DD DISP=SHR,DSN=&MIG.CTX06.PCG
//SORTOUT  DD DSN=&MIG.CTX09.EPCG,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,9,CH,A,
               36,9,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FCTX02
//*----------------------------------------------------------------
//SORT0902 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCTX02 SORT FCTX02-C-ID-PERS ASC [1:12—
//* SFCTX02 SORT FCTX02-C-ID-DOSS ASC [13:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCTX02
//SORTIN   DD DISP=SHR,DSN=&MIG.CTX02.FCTX02
//SORTOUT  DD DSN=&MIG.CTX09.FCTX02,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(4,9,CH,A,
               16,9,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0902       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//*SSPCG    DD DSN=&CIB.SPCG,
//SSPCG    DD DSN=&MIG.CTX09.SPCG,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0903       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCTX09,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCTX09,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCTX09,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZEPCG    DD DSN=&MIG.CTX09.EPCG.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCTX02  DD DSN=&MIG.CTX09.FCTX02.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZSPCG    DD DSN=&MIG.CTX09.SPCG.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCTX09
//*----------------------------------------------------------------
//PPRCTX09 EXEC PGM=PPRCTX09,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCTX09,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCTX09,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCTX09,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SEPCG    DD DISP=SHR,DSN=&MIG.CTX09.EPCG
//SFCTX02  DD DISP=SHR,DSN=&MIG.CTX09.FCTX02
//*--------------<FICHIERS CIBLES>---------------------------------
//SSPCG    DD DSN=&MIG.CTX09.SPCG,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZEPCG    DD DSN=&MIG.CTX09.EPCG.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCTX02  DD DSN=&MIG.CTX09.FCTX02.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZSPCG    DD DSN=&MIG.CTX09.SPCG.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCTX07
//*----------------------------------------------------------------
//DEL0701       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SPCG     DD DSN=&MIG.CTX07.PCG.T,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SHPS     DD DSN=&MIG.CTX07.HPS.T,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SEVX     DD DSN=&MIG.CTX07.EVX.T,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPAX     DD DSN=&MIG.CTX07.PAX.T,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPTX     DD DSN=&MIG.CTX07.PTX.T,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPGE     DD DSN=&MIG.CTX07.PGE.T,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*TRI MERGE DES FICHIERS PCG
//*----------------------------------------------------------------
//SORT0701 EXEC PGM=SORT,COND=(0,NE)
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.PCG
//SORTIN01 DD DISP=SHR,DSN=&MIG.CTX05.PCG
//SORTIN02 DD DISP=SHR,DSN=&MIG.CTX09.SPCG
//SORTOUT  DD DSN=&MIG.CTX07.PCG.T,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
/*
//*----------------------------------------------------------------
//*TRI MERGE DES FICHIERS HPS
//*----------------------------------------------------------------
//SORT0702 EXEC PGM=SORT,COND=(0,NE)
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.HPS
//SORTIN01 DD DISP=SHR,DSN=&MIG.CTX05.HPS
//SORTIN02 DD DISP=SHR,DSN=&MIG.CTX06.HPS
//SORTOUT  DD DSN=&MIG.CTX07.HPS.T,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
/*
//*----------------------------------------------------------------
//*TRI MERGE DES FICHIERS EVX
//*----------------------------------------------------------------
//SORT0703 EXEC PGM=SORT,COND=(0,NE)
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.EVX
//SORTIN01 DD DISP=SHR,DSN=&MIG.CTX05.EVX
//SORTIN02 DD DISP=SHR,DSN=&MIG.CTX06.EVX
//SORTOUT  DD DSN=&MIG.CTX07.EVX.T,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
/*
//*----------------------------------------------------------------
//*TRI MERGE DES FICHIERS PAX
//*----------------------------------------------------------------
//SORT0704 EXEC PGM=SORT,COND=(0,NE)
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.PAX
//SORTIN01 DD DISP=SHR,DSN=&MIG.CTX05.PAX
//SORTIN02 DD DISP=SHR,DSN=&MIG.CTX06.PAX
//SORTOUT  DD DSN=&MIG.CTX07.PAX.T,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
/*
//*----------------------------------------------------------------
//*TRI MERGE DES FICHIERS PTX
//*----------------------------------------------------------------
//SORT0705 EXEC PGM=SORT,COND=(0,NE)
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.PTX
//SORTIN01 DD DISP=SHR,DSN=&MIG.CTX05.PTX
//SORTIN02 DD DISP=SHR,DSN=&MIG.CTX06.PTX
//SORTOUT  DD DSN=&MIG.CTX07.PTX.T,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
/*
//*----------------------------------------------------------------
//*TRI MERGE DES FICHIERS PGE
//*----------------------------------------------------------------
//SORT0706 EXEC PGM=SORT,COND=(0,NE)
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.PGE
//SORTIN01 DD DISP=SHR,DSN=&MIG.CTX05.PGE
//SORTIN02 DD DISP=SHR,DSN=&MIG.CTX06.PGE
//SORTOUT  DD DSN=&MIG.CTX07.PGE.T,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=NONE
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0710       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SPCG     DD DSN=&CIB.PCG,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SHPS     DD DSN=&MIG.CTX07.HPS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SEVX     DD DSN=&MIG.CTX07.EVX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPAX     DD DSN=&CIB.PAX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPTX     DD DSN=&CIB.PTX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPGE     DD DSN=&CIB.PGE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0711       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCTX07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCTX07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCTX07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFIPCG   DD DSN=&MIG.CTX07.FIPCG.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFIHPS   DD DSN=&MIG.CTX07.FIHPS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFIEVX   DD DSN=&MIG.CTX07.FIEVX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFIPAX   DD DSN=&MIG.CTX07.FIPAX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFIPTX   DD DSN=&MIG.CTX07.FIPTX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFIPGE   DD DSN=&MIG.CTX07.FIPGE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZPCG     DD DSN=&MIG.CTX07.PCG.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZHPS     DD DSN=&MIG.CTX07.HPS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZEVX     DD DSN=&MIG.CTX07.EVX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZPAX     DD DSN=&MIG.CTX07.PAX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZPTX     DD DSN=&MIG.CTX07.PTX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZPGE     DD DSN=&MIG.CTX07.PGE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCTX07
//*----------------------------------------------------------------
//PPRCTX07 EXEC PGM=PPRCTX07,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCTX07,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCTX07,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCTX07,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//FIPCG    DD DISP=SHR,DSN=&MIG.CTX07.PCG.T
//FIHPS    DD DISP=SHR,DSN=&MIG.CTX07.HPS.T
//FIEVX    DD DISP=SHR,DSN=&MIG.CTX07.EVX.T
//FIPAX    DD DISP=SHR,DSN=&MIG.CTX07.PAX.T
//FIPTX    DD DISP=SHR,DSN=&MIG.CTX07.PTX.T
//FIPGE    DD DISP=SHR,DSN=&MIG.CTX07.PGE.T
//*--------------<FICHIERS CIBLES>---------------------------------
//SPCG     DD DSN=&CIB.PCG,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SHPS     DD DSN=&MIG.CTX07.HPS,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SEVX     DD DSN=&MIG.CTX07.EVX,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SPAX     DD DSN=&CIB.PAX,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SPTX     DD DSN=&CIB.PTX,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SPGE     DD DSN=&CIB.PGE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFIPCG   DD DSN=&MIG.CTX07.FIPCG.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFIHPS   DD DSN=&MIG.CTX07.FIHPS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFIEVX   DD DSN=&MIG.CTX07.FIEVX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFIPAX   DD DSN=&MIG.CTX07.FIPAX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFIPTX   DD DSN=&MIG.CTX07.FIPTX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFIPGE   DD DSN=&MIG.CTX07.FIPGE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZPCG     DD DSN=&MIG.CTX07.PCG.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZHPS     DD DSN=&MIG.CTX07.HPS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZEVX     DD DSN=&MIG.CTX07.EVX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZPAX     DD DSN=&MIG.CTX07.PAX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZPTX     DD DSN=&MIG.CTX07.PTX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZPGE     DD DSN=&MIG.CTX07.PGE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCTX11
//*----------------------------------------------------------------
//DEL1101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SHPS     DD DSN=&MIG.CTX11.HPS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPCG     DD DSN=&MIG.CTX11.PCG,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER HPS
//*----------------------------------------------------------------
//SORT1101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SHPS SORT HPS-IDTECADH ASC [16:20—
//* SHPS SORT HPS-IDGRPPROCTX ASC [36:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.HPS
//SORTIN   DD DISP=SHR,DSN=&MIG.CTX07.HPS
//SORTOUT  DD DSN=&MIG.CTX11.HPS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,20,CH,A,
               36,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER PCG
//*----------------------------------------------------------------
//SORT1102 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SPCG SORT PCG-IDTECADH ASC [16:20—
//* SPCG SORT PCG-IDGRPPROCTX ASC [36:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.PCG
//SORTIN   DD DISP=SHR,DSN=&CIB.PCG
//SORTOUT  DD DSN=&MIG.CTX11.PCG,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,20,CH,A,
               36,20,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SCHPS    DD DSN=&CIB.HPS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCTX11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCTX11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCTX11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZHPS     DD DSN=&MIG.CTX11.HPS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZPCG     DD DSN=&MIG.CTX11.PCG.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCHPS    DD DSN=&MIG.CTX11.CHPS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCTX11
//*----------------------------------------------------------------
//PPRCTX11 EXEC PGM=PPRCTX11,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCTX11,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCTX11,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCTX11,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SHPS     DD DISP=SHR,DSN=&MIG.CTX11.HPS
//SPCG     DD DISP=SHR,DSN=&MIG.CTX11.PCG
//*--------------<FICHIERS CIBLES>---------------------------------
//SCHPS    DD DSN=&CIB.HPS,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZHPS     DD DSN=&MIG.CTX11.HPS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZPCG     DD DSN=&MIG.CTX11.PCG.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCHPS    DD DSN=&MIG.CTX11.CHPS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCTX08
//*----------------------------------------------------------------
//DEL0801       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SEVX     DD DSN=&MIG.CTX08.EVX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPCG     DD DSN=&MIG.CTX08.PCG,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER EVX
//*----------------------------------------------------------------
//SORT0801 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SEVX SORT EVX-IDTECADH ASC [16:20—
//* SEVX SORT EVX-IDGRPPROCTX ASC [36:20—
//* SEVX SORT EVX-DTEFFEVT ASC [78:10—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.EVX
//SORTIN   DD DISP=SHR,DSN=&MIG.CTX07.EVX
//SORTOUT  DD DSN=&MIG.CTX08.EVX,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,20,CH,A,
               36,20,CH,A,
               78,10,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER PCG
//*----------------------------------------------------------------
//SORT0802 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SEVX SORT PCG-IDTECADH ASC [16:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.PCG
//SORTIN   DD DISP=SHR,DSN=&CIB.PCG
//SORTOUT  DD DSN=&MIG.CTX08.PCG,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,20,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0802       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SCIBEVX  DD DSN=&CIB.EVX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPCTX01 DD DSN=&CIB.FPCTX01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0803       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCTX08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCTX08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCTX08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZEVX     DD DSN=&MIG.CTX08.EVX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCIBEVX  DD DSN=&MIG.CTX08.CIBEVX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCTX08
//*----------------------------------------------------------------
//PPRCTX08 EXEC PGM=PPRCTX08,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCTX08,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCTX08,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCTX08,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SEVX     DD DISP=SHR,DSN=&MIG.CTX08.EVX
//SPCG     DD DISP=SHR,DSN=&MIG.CTX08.PCG
//*--------------<FICHIERS CIBLES>---------------------------------
//SCIBEVX  DD DSN=&CIB.EVX,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFPCTX01 DD DSN=&CIB.FPCTX01,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZEVX     DD DSN=&MIG.CTX08.EVX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCIBEVX  DD DSN=&MIG.CTX08.CIBEVX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCTX50
//*----------------------------------------------------------------
//DEL5001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL5002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SPROCTX  DD DSN=&CIB.PROCTX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCPCG    DD DSN=&CIB.CPCG,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCHPS    DD DSN=&CIB.CHPS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCHEP    DD DSN=&CIB.CHEP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCEVX    DD DSN=&CIB.CEVX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCPAX    DD DSN=&CIB.CPAX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCPTX    DD DSN=&CIB.CPTX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCPGE    DD DSN=&CIB.CPGE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCACX    DD DSN=&CIB.CACX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCLAA    DD DSN=&CIB.CLAA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCPPC    DD DSN=&CIB.CPPC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCECX    DD DSN=&CIB.CECX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCPAD    DD DSN=&CIB.CPAD,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCPTPIV  DD DSN=&CIB.CPTPIV,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL5003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCTX50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCTX50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCTX50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZSPCG    DD DSN=&MIG.CTX50.SPCG.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZSHPS    DD DSN=&MIG.CTX50.SHPS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZSHEP    DD DSN=&MIG.CTX50.SHEP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZSEVX    DD DSN=&MIG.CTX50.SEVX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZSPAX    DD DSN=&MIG.CTX50.SPAX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZSPTX    DD DSN=&MIG.CTX50.SPTX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZSPGE    DD DSN=&MIG.CTX50.SPGE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZSACX    DD DSN=&MIG.CTX50.SACX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZSLAA    DD DSN=&MIG.CTX50.SLAA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZSPPC    DD DSN=&MIG.CTX50.SPPC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZSECX    DD DSN=&MIG.CTX50.SECX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZSPAD    DD DSN=&MIG.CTX50.SPAD.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZPROCTX  DD DSN=&MIG.CTX50.PROCTX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCPCG    DD DSN=&MIG.CTX50.CPCG.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCHPS    DD DSN=&MIG.CTX50.CHPS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCHEP    DD DSN=&MIG.CTX50.CHEP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCEVX    DD DSN=&MIG.CTX50.CEVX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCPAX    DD DSN=&MIG.CTX50.CPAX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCPTX    DD DSN=&MIG.CTX50.CPTX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCPGE    DD DSN=&MIG.CTX50.CPGE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCACX    DD DSN=&MIG.CTX50.CACX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCLAA    DD DSN=&MIG.CTX50.CLAA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCPPC    DD DSN=&MIG.CTX50.CPPC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCECX    DD DSN=&MIG.CTX50.CECX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCPAD    DD DSN=&MIG.CTX50.CPAD.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCPTPIV  DD DSN=&MIG.CTX50.CPTPIV.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCTX50
//*----------------------------------------------------------------
//PPRCTX50 EXEC PGM=PPRCTX50,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCTX50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCTX50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCTX50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SPCG     DD DISP=SHR,DSN=&CIB.PCG
//SHPS     DD DISP=SHR,DSN=&CIB.HPS
//SHEP     DD DISP=SHR,DSN=&CIB.HEP
//SEVX     DD DISP=SHR,DSN=&CIB.EVX
//SPAX     DD DISP=SHR,DSN=&CIB.PAX
//SPTX     DD DISP=SHR,DSN=&CIB.PTX
//SPGE     DD DISP=SHR,DSN=&CIB.PGE
//SACX     DD DISP=SHR,DSN=&CIB.ACX
//SLAA     DD DISP=SHR,DSN=&CIB.LAA
//SPPC     DD DISP=SHR,DSN=&CIB.PPC
//SECX     DD DISP=SHR,DSN=&CIB.ECX
//SPAD     DD DISP=SHR,DSN=&CIB.PAD
//*--------------<FICHIERS CIBLES>---------------------------------
//SPROCTX  DD DSN=&CIB.PROCTX,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SCPCG    DD DSN=&CIB.CPCG,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SCHPS    DD DSN=&CIB.CHPS,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SCHEP    DD DSN=&CIB.CHEP,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SCEVX    DD DSN=&CIB.CEVX,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SCPAX    DD DSN=&CIB.CPAX,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SCPTX    DD DSN=&CIB.CPTX,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SCPGE    DD DSN=&CIB.CPGE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SCACX    DD DSN=&CIB.CACX,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SCLAA    DD DSN=&CIB.CLAA,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SCPPC    DD DSN=&CIB.CPPC,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SCECX    DD DSN=&CIB.CECX,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SCPAD    DD DSN=&CIB.CPAD,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SCPTPIV  DD DSN=&CIB.CPTPIV,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZSPCG    DD DSN=&MIG.CTX50.SPCG.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZSHPS    DD DSN=&MIG.CTX50.SHPS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZSHEP    DD DSN=&MIG.CTX50.SHEP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZSEVX    DD DSN=&MIG.CTX50.SEVX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZSPAX    DD DSN=&MIG.CTX50.SPAX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZSPTX    DD DSN=&MIG.CTX50.SPTX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZSPGE    DD DSN=&MIG.CTX50.SPGE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZSACX    DD DSN=&MIG.CTX50.SACX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZSLAA    DD DSN=&MIG.CTX50.SLAA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZSPPC    DD DSN=&MIG.CTX50.SPPC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZSECX    DD DSN=&MIG.CTX50.SECX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZSPAD    DD DSN=&MIG.CTX50.SPAD.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZPROCTX  DD DSN=&MIG.CTX50.PROCTX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCPCG    DD DSN=&MIG.CTX50.CPCG.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCHPS    DD DSN=&MIG.CTX50.CHPS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCHEP    DD DSN=&MIG.CTX50.CHEP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCEVX    DD DSN=&MIG.CTX50.CEVX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCPAX    DD DSN=&MIG.CTX50.CPAX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCPTX    DD DSN=&MIG.CTX50.CPTX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCPGE    DD DSN=&MIG.CTX50.CPGE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCACX    DD DSN=&MIG.CTX50.CACX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCLAA    DD DSN=&MIG.CTX50.CLAA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCPPC    DD DSN=&MIG.CTX50.CPPC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCECX    DD DSN=&MIG.CTX50.CECX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCPAD    DD DSN=&MIG.CTX50.CPAD.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCPTPIV  DD DSN=&MIG.CTX50.CPTPIV.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*
//* FIN DU JCL DE MIGRATION
