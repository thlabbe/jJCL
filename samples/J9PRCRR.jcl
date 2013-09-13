//J9PRCRR  JOB UTI00TX0,'PR GRECCO G07-CRR',CLASS=Z,MSGCLASS=X,
//*         RESTART=DEL0402,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=PLURM.MOVE.PR.JCL.PROD
//MEMBER   INCLUDE MEMBER=$$PRT
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRR01
//*----------------------------------------------------------------
//DEL0101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STDOSCT  DD DSN=&MIG.CRR01.TDOSCT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SC02DOS  DD DSN=&MIG.CRR01.C02DOS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SC02PER  DD DSN=&MIG.CRR01.C02PER,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TDOSCT
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STDOSCT SORT TAB-GR-DOSSCTX-C-ID-DOSS ASC [1:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.DOSSCTX
//SORTOUT  DD DSN=&MIG.CRR01.TDOSCT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER C02DOS
//*----------------------------------------------------------------
//SORT0102 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SC02DOS SORT C02-DOSS-CTX-C-ID-DOSS ASC [19:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.C02DOSS
//SORTOUT  DD DSN=&MIG.CRR01.C02DOS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(19,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER C02PER
//*----------------------------------------------------------------
//SORT0103 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SC02PER SORT C02-PER-DEB-CTX-C-ID-DOSS ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.C02PERDE
//SORTOUT  DD DSN=&MIG.CRR01.C02PER,
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
//SFCRR01  DD DSN=&MIG.CRR01.FCRR01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRR01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRR01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRR01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTDOSCT  DD DSN=&MIG.CRR01.TDOSCT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZC02DOS  DD DSN=&MIG.CRR01.C02DOS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZC02PER  DD DSN=&MIG.CRR01.C02PER.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCRR01  DD DSN=&MIG.CRR01.FCRR01.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRR01
//*----------------------------------------------------------------
//PPRCRR01 EXEC PGM=PPRCRR01,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRR01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRR01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRR01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STDOSCT  DD DISP=SHR,DSN=&MIG.CRR01.TDOSCT
//SC02DOS  DD DISP=SHR,DSN=&MIG.CRR01.C02DOS
//SC02PER  DD DISP=SHR,DSN=&MIG.CRR01.C02PER
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRR01  DD DSN=&MIG.CRR01.FCRR01,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTDOSCT  DD DSN=&MIG.CRR01.TDOSCT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZC02DOS  DD DSN=&MIG.CRR01.C02DOS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZC02PER  DD DSN=&MIG.CRR01.C02PER.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCRR01  DD DSN=&MIG.CRR01.FCRR01.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRR02
//*----------------------------------------------------------------
//DEL0201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRR01  DD DSN=&MIG.CRR02.FCRR01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPERCTX  DD DSN=&MIG.CRR02.PERCTX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB05PER  DD DSN=&MIG.CRR02.B05PER,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SECRIT4  DD DSN=&MIG.B05ECRI4,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRR01
//*----------------------------------------------------------------
//SORT0201 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRR01 SORT FCRR01-C-ID-PER-DEBIT ASC [45:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRR01.FCRR01
//SORTOUT  DD DSN=&MIG.CRR02.FCRR01,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(45,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER PERCTX
//*----------------------------------------------------------------
//SORT0202 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SPERCTX SORT PERCTX-C-ID-PER-DEBIT ASC [1:10—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TPERCTX
//*ORTIN   DD DISP=SHR,DSN=PIHURM.MOVE.PR.CIBLE.ECH.G18.TPERCTX
//SORTOUT  DD DSN=&MIG.CRR02.PERCTX,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,10,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B05PER
//*----------------------------------------------------------------
//SORT0203 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05PER SORT B05-PER-DEB-C-ID-PER-DEBIT ASC [30:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.B05PERIO
//SORTOUT  DD DSN=&MIG.CRR02.B05PER,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(30,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI4 DU FICHIER B05ECRI
//*----------------------------------------------------------------
//SORT0204 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05ECR SORT B05-ECRITURE-COMPTE-W1-R-DEBIT(3:10) ASC [39:10—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.B05ECRIT
//SORTOUT  DD DSN=&MIG.B05ECRI4,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(39,10,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0202       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRR02  DD DSN=&MIG.CRR02.FCRR02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRR02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRR02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRR02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCRR01  DD DSN=&MIG.CRR02.FCRR01.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZPERCTX  DD DSN=&MIG.CRR02.PERCTX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZB05PER  DD DSN=&MIG.CRR02.B05PER.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCRR02  DD DSN=&MIG.CRR02.FCRR02.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRR02
//*----------------------------------------------------------------
//PPRCRR02 EXEC PGM=PPRCRR02,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRR02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRR02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRR02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRR01  DD DISP=SHR,DSN=&MIG.CRR02.FCRR01
//SPERCTX  DD DISP=SHR,DSN=&MIG.CRR02.PERCTX
//SB05PER  DD DISP=SHR,DSN=&MIG.CRR02.B05PER
//SB05ECR  DD DISP=SHR,DSN=&MIG.B05ECRI4
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRR02  DD DSN=&MIG.CRR02.FCRR02,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//*WB05ECR  DD DSN=&&WB05ECR,
//*            DISP=(NEW,PASS),
//*            SPACE=(TRK,(5000,2500),RLSE)
//ZFCRR01  DD DSN=&MIG.CRR02.FCRR01.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZPERCTX  DD DSN=&MIG.CRR02.PERCTX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZB05PER  DD DSN=&MIG.CRR02.B05PER.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCRR02  DD DSN=&MIG.CRR02.FCRR02.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRR03
//*----------------------------------------------------------------
//DEL0301       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRR02  DD DSN=&MIG.CRR03.FCRR02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB07REC  DD DSN=&MIG.CRR03.B07REC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB07ACT  DD DSN=&MIG.CRR03.B07ACT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRR02
//*----------------------------------------------------------------
//SORT0301 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRR02 SORT FCRR02-C-ID-RECOUVR ASC [57:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRR02.FCRR02
//SORTOUT  DD DSN=&MIG.CRR03.FCRR02,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(57,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B07REC
//*----------------------------------------------------------------
//SORT0302 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB07REC SORT B07-RECOUVR-C-ID-RECOUVR ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.B07RECOU
//SORTOUT  DD DSN=&MIG.CRR03.B07REC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B07ACT
//*----------------------------------------------------------------
//SORT0303 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB07ACT SORT B07-ACTION-REC-C-ID-RECOUVR ASC [2:12—
//* SB07ACT SORT B07-ACTION-REC-C-TYPE-ACT-REC DESC [14:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.B07ACTIO
//SORTIN   DD DISP=SHR,DSN=&SRC.B07ACTIO
//SORTOUT  DD DSN=&MIG.CRR03.B07ACT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A,
               14,3,CH,D)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0302       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRR03  DD DSN=&MIG.CRR03.FCRR03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0303       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRR03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRR03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRR03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCRR02  DD DSN=&MIG.CRR03.FCRR02.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZB07REC  DD DSN=&MIG.CRR03.B07REC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZB07ACT  DD DSN=&MIG.CRR03.B07ACT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCRR03  DD DSN=&MIG.CRR03.FCRR03.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRR03
//*----------------------------------------------------------------
//PPRCRR03 EXEC PGM=PPRCRR03,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRR03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRR03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRR03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRR02  DD DISP=SHR,DSN=&MIG.CRR03.FCRR02
//SB07REC  DD DISP=SHR,DSN=&MIG.CRR03.B07REC
//SB07ACT  DD DISP=SHR,DSN=&MIG.CRR03.B07ACT
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRR03  DD DSN=&MIG.CRR03.FCRR03,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCRR02  DD DSN=&MIG.CRR03.FCRR02.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZB07REC  DD DSN=&MIG.CRR03.B07REC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZB07ACT  DD DSN=&MIG.CRR03.B07ACT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCRR03  DD DSN=&MIG.CRR03.FCRR03.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRR05
//*----------------------------------------------------------------
//DEL0501       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRR03  DD DSN=&MIG.CRR05.FCRR03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPCRE    DD DSN=&MIG.CRR05.PCRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPERCTX  DD DSN=&MIG.CRR05.PERCTX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRR03
//*----------------------------------------------------------------
//SORT0501 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRR03 SORT FCRR03-F02-IDLIGCRE ASC [120:20—
//* FIN   CRITERE XGEN
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRR03
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRR03.FCRR03
//SORTOUT  DD DSN=&MIG.CRR05.FCRR03,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(120,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER PCRE
//*----------------------------------------------------------------
//SORT0502 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SPCRE SORT PCRE-IDLIGCRE ASC [69:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.PCRE
//SORTOUT  DD DSN=&MIG.CRR05.PCRE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(69,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER PERCTX
//*----------------------------------------------------------------
//SORT0503 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SPERCTX SORT PERCTX-IDLIGCRE ASC [16:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.TPERCTX
//SORTOUT  DD DSN=&MIG.CRR05.PERCTX,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,20,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0502       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRR04  DD DSN=&MIG.CRR05.FCRR04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0503       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRR05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRR05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRR05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCRR03  DD DSN=&MIG.CRR05.FCRR03.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZPCRE    DD DSN=&MIG.CRR05.PCRE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCRR04  DD DSN=&MIG.CRR05.FCRR04.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRR05
//*----------------------------------------------------------------
//PPRCRR05 EXEC PGM=PPRCRR05,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRR05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRR05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRR05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFCRR03  DD DISP=SHR,DSN=&MIG.CRR05.FCRR03
//SPCRE    DD DISP=SHR,DSN=&MIG.CRR05.PCRE
//SPERCTX  DD DISP=SHR,DSN=&MIG.CRR05.PERCTX
//*--------------<FICHIERS CIBLES>---------------------------------
//SFCRR04  DD DSN=&MIG.CRR05.FCRR04,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCRR03  DD DSN=&MIG.CRR05.FCRR03.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZPCRE    DD DSN=&MIG.CRR05.PCRE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCRR04  DD DSN=&MIG.CRR05.FCRR04.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRR10
//*----------------------------------------------------------------
//DEL1001  EXEC PGM=IEFBR14
//SYSPRINT DD SYSOUT=*
//SFCRR04  DD DSN=&MIG.CRR10.FCRR04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SPCRE    DD DSN=&MIG.CRR10.PCRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRR04
//*----------------------------------------------------------------
//SORT1001 EXEC PGM=SORT
//* DEBUT CRITERE XGEN
//* FCRR04 SORT FCRR04-F03-IDLIGCRE ASC [120:20]
//* FCRR04 SORT FCRR04-F03-ID-TECH-UR-ADHR ASC [25:20]
//* FCRR04 SORT FCRR04-F03-C-ID-DOSS DESC [13:12]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRR05.FCRR04
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRR04
//SORTOUT  DD DSN=&MIG.CRR10.FCRR04,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(120,20,CH,A,
               25,20,CH,A,
               13,12,CH,D)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER PCRE
//*----------------------------------------------------------------
//SORT1002 EXEC PGM=SORT
//* DEBUT CRITERE XGEN
//* PCRE SORT PCRE-IDLIGCRE ASC [69:20]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&CIB.PCRE
//COPY     DD DISP=SHR,DSN=&CPYCIB.PCRE
//SORTOUT  DD DSN=&MIG.CRR10.PCRE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(69,20,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1002  EXEC PGM=IEFBR14
//SYSPRINT DD SYSOUT=*
//*SFCRR10  DD DSN=&CIB.FCRR10,
//SFCRR10  DD DSN=&MIG.CRR10.FCRR10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*SSUPCRR  DD DSN=&CIB.SUPCRR,
//SSUPCRR  DD DSN=&MIG.CRR10.SUPCRR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCRR04  DD DSN=&MIG.CRR10.FCRR04.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZPCRE    DD DSN=&MIG.CRR10.PCRE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCRR10  DD DSN=&MIG.CRR10.FCRR10.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZSUPCRR  DD DSN=&MIG.CRR10.SUPCRR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRR10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRR10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRR10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION
//*----------------------------------------------------------------
//PPRCRR10 EXEC PGM=PPRCRR10
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRR10,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRR10,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5000,500),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRR10,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(5000,500),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DUMMY
//*--------------<FICHIERS SOURCES>--------------------------------
//*SFCRR04  DD DISP=SHR,DSN=&SRC.FCRR04
//SFCRR04  DD DISP=SHR,DSN=&MIG.CRR10.FCRR04
//*SPCRE    DD DISP=SHR,DSN=&SRC.PCRE
//SPCRE    DD DISP=SHR,DSN=&MIG.CRR10.PCRE
//*--------------<FICHIERS CIBLES>---------------------------------
//*SFCRR10  DD DSN=&CIB.FCRR10,
//SFCRR10  DD DSN=&MIG.CRR10.FCRR10,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,500),RLSE)
//*SSUPCRR  DD DSN=&CIB.SUPCRR,
//SSUPCRR  DD DSN=&MIG.CRR10.SUPCRR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,500),RLSE)
//*
//* FIN DU JCL DE MIGRATION
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRR04
//*----------------------------------------------------------------
//DEL0401       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFCRR10  DD DSN=&MIG.CRR04.FCRR10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRR10
//*----------------------------------------------------------------
//SORT0401 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRR04 SORT FCRR04-F03-C-ID-PERS ASC [1:12—
//**SFCRR04 SORT FCRR04-F03-IDLIGCRE ASC [120:20—
//* SFCRR04 SORT FCRR04-F03-C-ID-DOSS ASC [13:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRR10.FCRR10
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRR04
//SORTOUT  DD DSN=&MIG.CRR04.FCRR10,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A,
               13,12,CH,A,
               45,12,CH,A,
               120,20,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0402       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SCRR     DD DSN=&CIB.CRR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0403       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRR04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRR04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRR04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTC1TPMI DD DSN=&MIG.CRR04.TC1TPMI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCRR04  DD DSN=&MIG.CRR04.FCRR04.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCRR     DD DSN=&MIG.CRR04.CRR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRR04
//*----------------------------------------------------------------
//PPRCRR04 EXEC PGM=PPRCRR04,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRR04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRR04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRR04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//*GRC-139--TC1TPMI  DD DISP=SHR,DSN=&TAB.TC1TPMI
//SFCRR10  DD DISP=SHR,DSN=&MIG.CRR04.FCRR10
//*--------------<FICHIERS CIBLES>---------------------------------
//SCRR     DD DSN=&CIB.CRR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTC1TPMI DD DSN=&MIG.CRR04.TC1TPMI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCRR04  DD DSN=&MIG.CRR04.FCRR04.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCRR     DD DSN=&MIG.CRR04.CRR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRR14
//*----------------------------------------------------------------
//DEL0401       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SSUPCRR  DD DSN=&MIG.CRR14.SUPCRR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FCRR10
//*----------------------------------------------------------------
//SORT0401 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFCRR04 SORT FCRR04-F03-C-ID-PERS ASC [1:12—
//**SFCRR04 SORT FCRR04-F03-IDLIGCRE ASC [120:20—
//* SFCRR04 SORT FCRR04-F03-C-ID-DOSS ASC [13:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CRR10.SUPCRR
//COPY     DD DISP=SHR,DSN=&CPYMIG.FCRR04
//SORTOUT  DD DSN=&MIG.CRR14.SUPCRR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A,
               13,12,CH,A,
               45,12,CH,A,
               120,20,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0402       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SSUPCFR     DD DSN=&CIB.SUPCFR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0403       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRR14,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRR14,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRR14,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTC1TPMI DD DSN=&MIG.CRR14.TC1TPMI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFCRR04  DD DSN=&MIG.CRR14.FCRR14.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCRR     DD DSN=&MIG.CRR14.CRR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRR04
//*----------------------------------------------------------------
//PPRCRR04 EXEC PGM=PPRCRR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRR14,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRR14,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRR14,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//TC1TPMI  DD DISP=SHR,DSN=&TAB.TC1TPMI
//SSUPCRR  DD DISP=SHR,DSN=&MIG.CRR14.SUPCRR
//*--------------<FICHIERS CIBLES>---------------------------------
//SSUPCFR  DD DSN=&CIB.SUPCFR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTC1TPMI DD DSN=&MIG.CRR04.TC1TPMI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFCRR04  DD DSN=&MIG.CRR14.FCRR04.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCRR     DD DSN=&MIG.CRR04.CRR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRCRR50
//*----------------------------------------------------------------
//DEL5001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL5002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SCRRCTX  DD DSN=&CIB.CRRCTX,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL5003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRCRR50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRCRR50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRCRR50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCRR     DD DSN=&MIG.CRR50.CRR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCRRCTX  DD DSN=&MIG.CRR50.CRRCTX.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRCRR50
//*----------------------------------------------------------------
//PPRCRR50 EXEC PGM=PPRCRR50,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRCRR50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRCRR50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRCRR50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//CRR      DD DISP=SHR,DSN=&CIB.CRR
//*--------------<FICHIERS CIBLES>---------------------------------
//SCRRCTX  DD DSN=&CIB.CRRCTX,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZCRR     DD DSN=&MIG.CRR50.CRR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCRRCTX  DD DSN=&MIG.CRR50.CRRCTX.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*
//* FIN DU JCL DE MIGRATION
