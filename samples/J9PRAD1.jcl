//J9PRAD1  JOB UTI00TX0,'PR BAC    B02-ADH',CLASS=Z,MSGCLASS=X,
//*         RESTART=DEL5303,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=PLURM.MOVE.PR.JCL.PROD
//MEMBER   INCLUDE MEMBER=$$PRT
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH01
//*----------------------------------------------------------------
//DEL0101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFPENT1  DD DSN=&MIG.ADH01.FPENT1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01ENTR DD DSN=&MIG.ADH01.A01ENTR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SADH     DD DSN=&MIG.ADH01.ADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FPENT1
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFPENT1 SORT FPENT1-C-ID-PERS ASC [141:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.FPENT1
//SORTIN   DD DISP=SHR,DSN=&CIB.FPENT1
//SORTOUT  DD DSN=&MIG.ADH01.FPENT1,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(141,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01ENTR
//*----------------------------------------------------------------
//SORT0102 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01ENTR SORT A01-ENTR-ETAB-C-ID-PERS ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01ENTR
//SORTIN   DD DISP=SHR,DSN=&SRC.A01ENTR
//SORTOUT  DD DSN=&MIG.ADH01.A01ENTR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER ADH
//*----------------------------------------------------------------
//SORT0103 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SADH SORT A03-ADHERENT-C-ID-PERS ASC [19:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03ADHER
//SORTIN   DD DISP=SHR,DSN=&MIG.A03ADHER
//SORTOUT  DD DSN=&MIG.ADH01.ADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(19,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH1   DD DSN=&MIG.ADH01.FADH1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPENT1  DD DSN=&MIG.ADH01.FPENT1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01ENTR DD DSN=&MIG.ADH01.A01ENTR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZADH     DD DSN=&MIG.ADH01.ADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH1   DD DSN=&MIG.ADH01.FADH1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH01
//*----------------------------------------------------------------
//PPRADH01 EXEC PGM=PPRADH01,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFPENT1  DD DISP=SHR,DSN=&MIG.ADH01.FPENT1
//SA01ENTR DD DISP=SHR,DSN=&MIG.ADH01.A01ENTR
//SADH     DD DISP=SHR,DSN=&MIG.ADH01.ADH
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADH1   DD DSN=&MIG.ADH01.FADH1,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPENT1  DD DSN=&MIG.ADH01.FPENT1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01ENTR DD DSN=&MIG.ADH01.A01ENTR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZADH     DD DSN=&MIG.ADH01.ADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH1   DD DSN=&MIG.ADH01.FADH1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH02
//*----------------------------------------------------------------
//DEL0201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH1   DD DSN=&MIG.ADH02.FADH1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCTRAT   DD DSN=&MIG.ADH02.CTRAT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADH1
//*----------------------------------------------------------------
//SORT0201 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADH1 SORT FADH1-C-GRPT-GEST-S ASC [1:1—
//* SFADH1 SORT FADH1-C-ADH-S ASC [2:9—
//* SFADH1 SORT FADH1-C-ORDRE-S ASC [11:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADH1
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH01.FADH1
//SORTOUT  DD DSN=&MIG.ADH02.FADH1,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,1,CH,A,
               2,9,CH,A,
               11,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER CTRAT
//*----------------------------------------------------------------
//SORT0202 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCTRAT SORT A03-CONTRAT-C-GRPT-GEST ASC [15:1—
//* SCTRAT SORT A03-CONTRAT-C-ADH ASC [17:9—
//* SCTRAT SORT A03-CONTRAT-C-ORDRE ASC [27:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03CONTR
//SORTIN   DD DISP=SHR,DSN=&MIG.A03CONTR
//SORTOUT  DD DSN=&MIG.ADH02.CTRAT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(15,1,CH,A,
               17,9,CH,A,
               27,4,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0202       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH2   DD DSN=&MIG.ADH02.FADH2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH1   DD DSN=&MIG.ADH02.FADH1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCTRAT   DD DSN=&MIG.ADH02.CTRAT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH2   DD DSN=&MIG.ADH02.FADH2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH02
//*----------------------------------------------------------------
//PPRADH02 EXEC PGM=PPRADH02,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADH1   DD DISP=SHR,DSN=&MIG.ADH02.FADH1
//SCTRAT   DD DISP=SHR,DSN=&MIG.ADH02.CTRAT
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADH2   DD DSN=&MIG.ADH02.FADH2,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH1   DD DSN=&MIG.ADH02.FADH1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCTRAT   DD DSN=&MIG.ADH02.CTRAT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH2   DD DSN=&MIG.ADH02.FADH2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH03
//*----------------------------------------------------------------
//DEL0301       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH2   DD DSN=&MIG.ADH03.FADH2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCHNCTR  DD DSN=&MIG.ADH03.CHNCTR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADH2
//*----------------------------------------------------------------
//SORT0301 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADH2 SORT FADH2-C-ID-CONTRAT-S ASC [97:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADH2
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH02.FADH2
//SORTOUT  DD DSN=&MIG.ADH03.FADH2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(97,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER CHNCTR
//*----------------------------------------------------------------
//SORT0302 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCHNCTR SORT A03-CHAIN-CONTR-C-ID-CONTR-SOU ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03CHAIN
//SORTIN   DD DISP=SHR,DSN=&SRC.A03CHAIN
//SORTOUT  DD DSN=&MIG.ADH03.CHNCTR,
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
//SFADH3   DD DSN=&MIG.ADH03.FADH3,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0303       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH2   DD DSN=&MIG.ADH03.FADH2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCHNCTR  DD DSN=&MIG.ADH03.CHNCTR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH3   DD DSN=&MIG.ADH03.FADH3.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH03
//*----------------------------------------------------------------
//PPRADH03 EXEC PGM=PPRADH03,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADH2   DD DISP=SHR,DSN=&MIG.ADH03.FADH2
//SCHNCTR  DD DISP=SHR,DSN=&MIG.ADH03.CHNCTR
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADH3   DD DSN=&MIG.ADH03.FADH3,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH2   DD DSN=&MIG.ADH03.FADH2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCHNCTR  DD DSN=&MIG.ADH03.CHNCTR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH3   DD DSN=&MIG.ADH03.FADH3.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH04
//*----------------------------------------------------------------
//DEL0401       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH3   DD DSN=&MIG.ADH04.FADH3,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCTRAT   DD DSN=&MIG.ADH04.CTRAT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADH3
//*----------------------------------------------------------------
//SORT0401 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADH3 SORT FADH3-C-ID-CONTR-DES ASC [195:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADH3
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH03.FADH3
//SORTOUT  DD DSN=&MIG.ADH04.FADH3,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(195,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER CTRAT
//*----------------------------------------------------------------
//SORT0402 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCTRAT SORT A03-CONTRAT-C-ID-CONTRAT ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03CONTR
//SORTIN   DD DISP=SHR,DSN=&MIG.A03CONTR
//SORTOUT  DD DSN=&MIG.ADH04.CTRAT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0402       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH4   DD DSN=&MIG.ADH04.FADH4,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0403       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH3   DD DSN=&MIG.ADH04.FADH3.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCTRAT   DD DSN=&MIG.ADH04.CTRAT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH4   DD DSN=&MIG.ADH04.FADH4.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH04
//*----------------------------------------------------------------
//PPRADH04 EXEC PGM=PPRADH04,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADH3   DD DISP=SHR,DSN=&MIG.ADH04.FADH3
//SCTRAT   DD DISP=SHR,DSN=&MIG.ADH04.CTRAT
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADH4   DD DSN=&MIG.ADH04.FADH4,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH3   DD DSN=&MIG.ADH04.FADH3.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCTRAT   DD DSN=&MIG.ADH04.CTRAT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH4   DD DSN=&MIG.ADH04.FADH4.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH05
//*----------------------------------------------------------------
//DEL0501       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH4   DD DSN=&MIG.ADH05.FADH4,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SADH     DD DSN=&MIG.ADH05.ADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADH4
//*----------------------------------------------------------------
//SORT0501 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADH4 SORT FADH4-C-GRPT-GEST-C ASC [252:1—
//* SFADH4 SORT FADH4-C-ADH-C ASC [253:9—
//* SFADH4 SORT FADH4-C-ORDRE-C ASC [262:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADH4
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH04.FADH4
//SORTOUT  DD DSN=&MIG.ADH05.FADH4,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(252,1,CH,A,
               253,9,CH,A,
               262,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER ADH
//*----------------------------------------------------------------
//SORT0502 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SADH SORT A03-ADHERENT-C-GRPT-GEST ASC [1:1—
//* SADH SORT A03-ADHERENT-C-ADH ASC [3:9—
//* SADH SORT A03-ADHERENT-C-ORDRE ASC [13:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03ADHER
//SORTIN   DD DISP=SHR,DSN=&MIG.A03ADHER
//*SORTIN   DD DISP=SHR,DSN=&MIG.A3ADHER
//SORTOUT  DD DSN=&MIG.ADH05.ADH,
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
//DEL0502       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH5   DD DSN=&MIG.ADH05.FADH5,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0503       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH4   DD DSN=&MIG.ADH05.FADH4.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZADH     DD DSN=&MIG.ADH05.ADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH5   DD DSN=&MIG.ADH05.FADH5.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH05
//*----------------------------------------------------------------
//PPRADH05 EXEC PGM=PPRADH05,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADH4   DD DISP=SHR,DSN=&MIG.ADH05.FADH4
//SADH     DD DISP=SHR,DSN=&MIG.ADH05.ADH
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADH5   DD DSN=&MIG.ADH05.FADH5,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH4   DD DSN=&MIG.ADH05.FADH4.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZADH     DD DSN=&MIG.ADH05.ADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH5   DD DSN=&MIG.ADH05.FADH5.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH06
//*----------------------------------------------------------------
//DEL0601       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH5   DD DSN=&MIG.ADH06.FADH5,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA3ETADH DD DSN=&MIG.ADH06.A3ETADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADH5
//*----------------------------------------------------------------
//SORT0601 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADH5 SORT FADH5-C-ID-ADH-S ASC [28:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADH5
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH05.FADH5
//SORTOUT  DD DSN=&MIG.ADH06.FADH5,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(28,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A3ETADH
//*----------------------------------------------------------------
//SORT0602 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA3ETADH SORT A03-ETAT-ADHERENT-C-ID-ADH ASC [16:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03ETADH
//SORTIN   DD DISP=SHR,DSN=&SRC.A03ETADH
//SORTOUT  DD DSN=&MIG.ADH06.A3ETADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0602       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH6   DD DSN=&MIG.ADH06.FADH6,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0603       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*ZFADH5   DD DSN=&CIB.FADH5.R,
//ZFADH5   DD DSN=&MIG.ADH06.FADH5.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA3ETADH DD DSN=&MIG.ADH06.A3ETADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH6   DD DSN=&MIG.ADH06.FADH6.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH06
//*----------------------------------------------------------------
//PPRADH06 EXEC PGM=PPRADH06,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH06,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH06,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH06,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADH5   DD DISP=SHR,DSN=&MIG.ADH06.FADH5
//SA3ETADH DD DISP=SHR,DSN=&MIG.ADH06.A3ETADH
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADH6   DD DSN=&MIG.ADH06.FADH6,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH5   DD DSN=&MIG.ADH06.FADH5.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA3ETADH DD DSN=&MIG.ADH06.A3ETADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH6   DD DSN=&MIG.ADH06.FADH6.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH07
//*----------------------------------------------------------------
//DEL0701       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH3   DD DSN=&MIG.ADH07.FADH3,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA3ETADH DD DSN=&MIG.ADH07.A3ETADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADH3
//*----------------------------------------------------------------
//SORT0701 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADH3 SORT FADH3-C-ID-ADH-S ASC [28:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADH3
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH03.FADH3
//SORTOUT  DD DSN=&MIG.ADH07.FADH3,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(28,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A3ETADH
//*----------------------------------------------------------------
//SORT0702 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA3ETADH SORT A03-ETAT-ADHERENT-C-ID-ADH ASC [16:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03ETADH
//SORTIN   DD DISP=SHR,DSN=&SRC.A03ETADH
//SORTOUT  DD DSN=&MIG.ADH07.A3ETADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0702       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH7   DD DSN=&MIG.ADH07.FADH7,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0703       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH3   DD DSN=&MIG.ADH07.FADH3.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA3ETADH DD DSN=&MIG.ADH07.A3ETADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH7   DD DSN=&MIG.ADH07.FADH7.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH07
//*----------------------------------------------------------------
//PPRADH07 EXEC PGM=PPRADH07,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH07,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH07,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH07,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADH3   DD DISP=SHR,DSN=&MIG.ADH07.FADH3
//SA3ETADH DD DISP=SHR,DSN=&MIG.ADH07.A3ETADH
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADH7   DD DSN=&MIG.ADH07.FADH7,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH3   DD DSN=&MIG.ADH07.FADH3.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA3ETADH DD DSN=&MIG.ADH07.A3ETADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH7   DD DSN=&MIG.ADH07.FADH7.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL5002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH8   DD DSN=&MIG.ADH50.FADH8,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL5003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH6   DD DSN=&MIG.ADH50.FADH6.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH7   DD DSN=&MIG.ADH50.FADH7.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH8   DD DSN=&MIG.ADH50.FADH8.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH50
//*----------------------------------------------------------------
//PPRADH50 EXEC PGM=PPRADH50,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//FADH6    DD DISP=SHR,DSN=&MIG.ADH06.FADH6
//FADH7    DD DISP=SHR,DSN=&MIG.ADH07.FADH7
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADH8   DD DSN=&MIG.ADH50.FADH8,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH6   DD DSN=&MIG.ADH50.FADH6.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH7   DD DSN=&MIG.ADH50.FADH7.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH8   DD DSN=&MIG.ADH50.FADH8.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH51
//*----------------------------------------------------------------
//DEL5101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH8   DD DSN=&MIG.ADH51.FADH8,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADH8
//*----------------------------------------------------------------
//SORT5101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADH8 SORT FADH8-C-ID-PERS-S ASC [15:12—
//* SFADH8 SORT FADH8-C-GRPT-GEST-S ASC [1:1—
//* SFADH8 SORT FADH8-C-ADH-S ASC [2:9—
//* SFADH8 SORT FADH8-C-ORDRE-S ASC [11:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADH8
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH50.FADH8
//SORTOUT  DD DSN=&MIG.ADH51.FADH8,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(15,12,CH,A,
               1,1,CH,A,
               2,9,CH,A,
               11,4,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL5102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFRGPT   DD DSN=&MIG.ADH51.FRGPT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL5103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH51,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH51,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH51,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH8   DD DSN=&MIG.ADH51.FADH8.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFRGPT   DD DSN=&MIG.ADH51.FRGPT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH51
//*----------------------------------------------------------------
//PPRADH51 EXEC PGM=PPRADH51,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH51,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH51,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH51,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADH8   DD DISP=SHR,DSN=&MIG.ADH51.FADH8
//*--------------<FICHIERS CIBLES>---------------------------------
//SFRGPT   DD DSN=&MIG.ADH51.FRGPT,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH8   DD DSN=&MIG.ADH51.FADH8.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFRGPT   DD DSN=&MIG.ADH51.FRGPT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH52
//*----------------------------------------------------------------
//DEL5201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH8   DD DSN=&MIG.ADH52.FADH8,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFRGPT   DD DSN=&MIG.ADH52.FRGPT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADH8
//*----------------------------------------------------------------
//SORT5201 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADH8 SORT FADH8-C-ID-PERS-S ASC [15:12—
//* SFADH8 SORT FADH8-C-GRPT-GEST-S ASC [1:1—
//* SFADH8 SORT FADH8-C-ADH-S ASC [2:9—
//* SFADH8 SORT FADH8-C-ORDRE-S ASC [11:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADH8
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH50.FADH8
//SORTOUT  DD DSN=&MIG.ADH52.FADH8,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(15,12,CH,A,
               1,1,CH,A,
               2,9,CH,A,
               11,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FRGPT
//*----------------------------------------------------------------
//SORT5202 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFRGPT SORT RGPT-C-ID-PERS ASC [24:12—
//* SFRGPT SORT RGPT-REF-ADHERENT ASC [1:14—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FRGPT
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH51.FRGPT
//SORTOUT  DD DSN=&MIG.ADH52.FRGPT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(24,12,CH,A,
               1,14,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL5202       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH9   DD DSN=&MIG.ADH52.FADH9,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL5203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH52,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH52,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH52,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH8   DD DSN=&MIG.ADH52.FADH8.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFRGPT   DD DSN=&MIG.ADH52.FRGPT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH9   DD DSN=&MIG.ADH52.FADH9.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH52
//*----------------------------------------------------------------
//PPRADH52 EXEC PGM=PPRADH52,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH52,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH52,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH52,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADH8   DD DISP=SHR,DSN=&MIG.ADH52.FADH8
//SFRGPT   DD DISP=SHR,DSN=&MIG.ADH52.FRGPT
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADH9   DD DSN=&MIG.ADH52.FADH9,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH8   DD DSN=&MIG.ADH52.FADH8.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFRGPT   DD DSN=&MIG.ADH52.FRGPT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH9   DD DSN=&MIG.ADH52.FADH9.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH54
//*----------------------------------------------------------------
//DEL5401       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH9   DD DSN=&MIG.ADH54.FADH9,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SK02COAU DD DSN=&MIG.ADH54.K02COAU,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADH9
//*----------------------------------------------------------------
//SORT5401 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADH9 SORT FADH9-C-CONTRAT-AURA ASC [156:17—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADH9
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH52.FADH9
//SORTOUT  DD DSN=&MIG.ADH54.FADH9,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(156,17,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER K02COAU
//*----------------------------------------------------------------
//SORT5402 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SK02COAU SORT K02-CONT-AURA-C-CONTRAT-AURA ASC [1:17—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.K02COAUR
//SORTIN   DD DISP=SHR,DSN=&SRC.K02COAUR
//SORTOUT  DD DSN=&MIG.ADH54.K02COAU,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,17,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL5402       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH9A  DD DSN=&MIG.ADH54.FADH9A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL5403       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH54,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH54,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH54,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH9   DD DSN=&MIG.ADH54.FADH9.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZK02COAU DD DSN=&MIG.ADH54.K02COAU.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH9A  DD DSN=&MIG.ADH54.FADH9A.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH54
//*----------------------------------------------------------------
//PPRADH54 EXEC PGM=PPRADH54,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH54,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH54,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH54,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADH9   DD DISP=SHR,DSN=&MIG.ADH54.FADH9
//SK02COAU DD DISP=SHR,DSN=&MIG.ADH54.K02COAU
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADH9A  DD DSN=&MIG.ADH54.FADH9A,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH9   DD DSN=&MIG.ADH54.FADH9.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZK02COAU DD DSN=&MIG.ADH54.K02COAU.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH9A  DD DSN=&MIG.ADH54.FADH9A.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH53
//*----------------------------------------------------------------
//DEL5301       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH9A  DD DSN=&MIG.ADH53.FADH9A,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01COMP DD DSN=&MIG.ADH53.A01COMP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADH9A
//*----------------------------------------------------------------
//SORT5301 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADH9A SORT FADH9A-C-ID-PERS-S ASC [15:12—
//* SFADH9A SORT FADH9A-CHRONO ASC [314:8—
//* SFADH9A SORT FADH9A-C-GRPT-GEST-S ASC [1:1—
//* SFADH9A SORT FADH9A-C-ADH-S ASC [2:9—
//* SFADH9A SORT FADH9A-C-ORDRE-S ASC [11:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADH9A
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH54.FADH9A
//SORTOUT  DD DSN=&MIG.ADH53.FADH9A,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(15,12,CH,A,
               314,8,CH,A,
               1,1,CH,A,
               2,9,CH,A,
               11,4,CH,A)
/*
//*---------------------------------------------------------------
//*TRI DU FICHIER A01COMP
//*----------------------------------------------------------------
//SORT5302 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01COMP SORT A01-COMPO-GRO-C-ID-PERS ASC +15:12]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01COMPO
//SORTIN   DD DISP=SHR,DSN=&SRC.A01COMPO
//SORTOUT  DD DSN=&MIG.ADH53.A01COMP,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(15,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL5302       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//*BAC-063 - AJO le 20/08/2013
//*STAGAADN DD DSN=&CIB.TAGAADN,
//STAGAADN DD DSN=&MIG.ADH53.TAGAADN,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPADH2  DD DSN=&CIB.FPADH2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STABADHS DD DSN=&MIG.ADH53.TABADHS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL5303       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH53,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH53,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH53,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZL01INST DD DSN=&MIG.ADH53.L01INST.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH9A  DD DSN=&MIG.ADH53.FADH9A.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01COMP DD DSN=&MIG.ADH53.A01COMP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGAADN DD DSN=&MIG.ADH53.TAGAADN.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPADH2  DD DSN=&MIG.ADH53.FPADH2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTABADHS DD DSN=&MIG.ADH53.TABADHS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH53
//*----------------------------------------------------------------
//PPRADH53 EXEC PGM=PPRADH53,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH53,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH53,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH53,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//L01INST  DD DISP=SHR,DSN=&SRC.L01INSTI
//SFADH9A  DD DISP=SHR,DSN=&MIG.ADH53.FADH9A
//SA01COMP DD DISP=SHR,DSN=&MIG.ADH53.A01COMP
//*--------------<FICHIERS CIBLES>---------------------------------
//*BAC-063 - AJO le 20/08/2013
//*STAGAADN DD DSN=&CIB.TAGAADN,
//STAGAADN DD DSN=&MIG.ADH53.TAGAADN,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFPADH2  DD DSN=&CIB.FPADH2,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STABADHS DD DSN=&MIG.ADH53.TABADHS,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZL01INST DD DSN=&MIG.ADH53.L01INST.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH9A  DD DSN=&MIG.ADH53.FADH9A.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01COMP DD DSN=&MIG.ADH53.A01COMP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAGAADN DD DSN=&MIG.ADH53.TAGAADN.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPADH2  DD DSN=&MIG.ADH53.FPADH2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTABADHS DD DSN=&MIG.ADH53.TABADHS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH10
//*----------------------------------------------------------------
//DEL1001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADHS   DD DSN=&MIG.ADH10.FADHS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFENTR   DD DSN=&MIG.ADH10.FENTR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADHS
//*----------------------------------------------------------------
//SORT1001 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADHS SORT ADHSADH-ID-TECH-UR-ADHR(1:9) ASC [21:9—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.TADHADH
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH53.TABADHS
//SORTOUT  DD DSN=&MIG.ADH10.FADHS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,9,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FENTR
//*----------------------------------------------------------------
//SORT1002 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFENTR SORT A01-ENTR-ETAB-C-ID-PERS-ENTR(4:9) ASC [19:9—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01ENTR
//SORTIN   DD DISP=SHR,DSN=&SRC.A01ENTR
//SORTOUT  DD DSN=&MIG.ADH10.FENTR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(19,9,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH10  DD DSN=&MIG.ADH10.FADH10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADHS   DD DSN=&MIG.ADH10.FADHS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFENTR   DD DSN=&MIG.ADH10.FENTR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH10  DD DSN=&MIG.ADH10.FADH10.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH10
//*----------------------------------------------------------------
//PPRADH10 EXEC PGM=PPRADH10,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH10,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH10,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH10,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADHS   DD DISP=SHR,DSN=&MIG.ADH10.FADHS
//SFENTR   DD DISP=SHR,DSN=&MIG.ADH10.FENTR
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADH10  DD DSN=&MIG.ADH10.FADH10,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADHS   DD DSN=&MIG.ADH10.FADHS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFENTR   DD DSN=&MIG.ADH10.FENTR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH10  DD DSN=&MIG.ADH10.FADH10.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH11
//*----------------------------------------------------------------
//DEL1101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH10  DD DSN=&MIG.ADH11.FADH10,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFETABC  DD DSN=&MIG.ADH11.FETABC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADH10
//*----------------------------------------------------------------
//SORT1101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADH10 SORT FADH10-C-ID-PERS ASC [21:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADH10
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH10.FADH10
//SORTOUT  DD DSN=&MIG.ADH11.FADH10,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FETABC
//*----------------------------------------------------------------
//SORT1102 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFETABC SORT A03-ETAB-CONTR-C-ID-PERS ASC [15:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03ETABC
//SORTIN   DD DISP=SHR,DSN=&SRC.A03ETABC
//SORTOUT  DD DSN=&MIG.ADH11.FETABC,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(15,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH11  DD DSN=&MIG.ADH11.FADH11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH10  DD DSN=&MIG.ADH11.FADH10.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFETABC  DD DSN=&MIG.ADH11.FETABC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH11  DD DSN=&MIG.ADH11.FADH11.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH11
//*----------------------------------------------------------------
//PPRADH11 EXEC PGM=PPRADH11,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH11,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH11,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH11,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADH10  DD DISP=SHR,DSN=&MIG.ADH11.FADH10
//SFETABC  DD DISP=SHR,DSN=&MIG.ADH11.FETABC
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADH11  DD DSN=&MIG.ADH11.FADH11,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH10  DD DSN=&MIG.ADH11.FADH10.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFETABC  DD DSN=&MIG.ADH11.FETABC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH11  DD DSN=&MIG.ADH11.FADH11.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH12
//*----------------------------------------------------------------
//DEL1201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH11  DD DSN=&MIG.ADH12.FADH11,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPOPCTR DD DSN=&MIG.ADH12.FPOPCTR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADH2   DD DSN=&MIG.ADH12.FADH2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADH11
//*----------------------------------------------------------------
//SORT1201 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADH11 SORT FADH11-C-ID-CONTRAT ASC [52:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADH11
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH11.FADH11
//SORTOUT  DD DSN=&MIG.ADH12.FADH11,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(52,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FPOPCTR
//*----------------------------------------------------------------
//SORT1202 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFPOPCTR SORT A03-POPUL-CONTR-C-ID-CONTRAT ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A03POPCO
//SORTIN   DD DISP=SHR,DSN=&SRC.A03POPCO
//SORTOUT  DD DSN=&MIG.ADH12.FPOPCTR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADH2
//*----------------------------------------------------------------
//SORT1203 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADH2 SORT FADH2-C-ID-CONTRAT-S ASC [97:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADH2
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH02.FADH2
//SORTOUT  DD DSN=&MIG.ADH12.FADH2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(97,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1202       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH12  DD DSN=&MIG.ADH12.FADH12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH11  DD DSN=&MIG.ADH12.FADH11.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPOPCTR DD DSN=&MIG.ADH12.FPOPCTR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH2   DD DSN=&MIG.ADH12.FADH2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH12  DD DSN=&MIG.ADH12.FADH12.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH12
//*----------------------------------------------------------------
//PPRADH12 EXEC PGM=PPRADH12,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH12,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH12,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH12,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADH11  DD DISP=SHR,DSN=&MIG.ADH12.FADH11
//SFPOPCTR DD DISP=SHR,DSN=&MIG.ADH12.FPOPCTR
//SFADH2   DD DISP=SHR,DSN=&MIG.ADH12.FADH2
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADH12  DD DSN=&MIG.ADH12.FADH12,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH11  DD DSN=&MIG.ADH12.FADH11.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPOPCTR DD DSN=&MIG.ADH12.FPOPCTR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH2   DD DSN=&MIG.ADH12.FADH2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH12  DD DSN=&MIG.ADH12.FADH12.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH14
//*----------------------------------------------------------------
//DEL1401       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STABADHS DD DSN=&MIG.ADH14.TABADHS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADH12  DD DSN=&MIG.ADH14.FADH12,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TABADHS
//*----------------------------------------------------------------
//SORT1401 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STABADHS SORT ADHSADH-C-GRPT-GEST ASC [94:1—
//* STABADHS SORT ADHSADH-C-ADH ASC [95:9—
//* STABADHS SORT ADHSADH-C-ORDRE ASC [104:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.TADHADH
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH53.TABADHS
//SORTOUT  DD DSN=&MIG.ADH14.TABADHS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(94,1,CH,A,
               95,9,CH,A,
               104,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADH12
//*----------------------------------------------------------------
//SORT1402 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADH12 SORT FADH12-C-GRPT-GEST ASC [100:1—
//* SFADH12 SORT FADH12-C-ADH ASC [101:9—
//* SFADH12 SORT FADH12-C-ORDRE ASC [110:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADH12
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH12.FADH12
//SORTOUT  DD DSN=&MIG.ADH14.FADH12,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(100,1,CH,A,
               101,9,CH,A,
               110,4,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1402       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH13  DD DSN=&MIG.ADH14.FADH13,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1403       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH14,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH14,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH14,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTABADHS DD DSN=&MIG.ADH14.TABADHS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH12  DD DSN=&MIG.ADH14.FADH12.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH13  DD DSN=&MIG.ADH14.FADH13.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH14
//*----------------------------------------------------------------
//PPRADH14 EXEC PGM=PPRADH14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH14,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH14,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH14,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STABADHS DD DISP=SHR,DSN=&MIG.ADH14.TABADHS
//SFADH12  DD DISP=SHR,DSN=&MIG.ADH14.FADH12
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADH13  DD DSN=&MIG.ADH14.FADH13,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTABADHS DD DSN=&MIG.ADH14.TABADHS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH12  DD DSN=&MIG.ADH14.FADH12.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH13  DD DSN=&MIG.ADH14.FADH13.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH13
//*----------------------------------------------------------------
//DEL1301       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADHS   DD DSN=&MIG.ADH13.FADHS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFADH13  DD DSN=&MIG.ADH13.FADH13,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FADHS
//*----------------------------------------------------------------
//SORT1301 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADHS SORT FADHS1-ID-TECH-UR-ADHR ASC [21:20—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADHS1
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH53.TABADHS
//SORTOUT  DD DSN=&MIG.ADH13.FADHS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,20,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FADH13
//*----------------------------------------------------------------
//SORT1302 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFADH13 SORT FADH13-ID-TECH-UR-ADHR ASC [1:20—
//* SFADH13 SORT FADH13-NO-ORD-ADHS ASC [114:3—
//* SFADH13 SORT FADH13-C-CPI ASC [81:3—
//* SFADH13 SORT FADH13-C-ID-PERS ASC [21:12—
//* SFADH13 SORT FADH13-D-DEB-EFFET ASC [64:8—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.FADH13
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH14.FADH13
//SORTOUT  DD DSN=&MIG.ADH13.FADH13,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               114,3,CH,A,
               81,3,CH,A,
               21,12,CH,A,
               64,8,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL1302       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//*BAC-063 - AJO le 20/08/2013
//*STAGAHMA DD DSN=&CIB.TAGAHMA,
//STAGAHMA DD DSN=&MIG.ADH13.TAGAHMA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STABADHS DD DSN=&CIB.TADHADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL1303       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH13,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH13,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH13,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADHS   DD DSN=&MIG.ADH13.FADHS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH13  DD DSN=&MIG.ADH13.FADH13.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGAHMA DD DSN=&MIG.ADH13.TAGAHMA.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTABADHS DD DSN=&MIG.ADH13.TABADHS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH13
//*----------------------------------------------------------------
//PPRADH13 EXEC PGM=PPRADH13,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH13,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH13,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH13,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFADHS   DD DISP=SHR,DSN=&MIG.ADH13.FADHS
//SFADH13  DD DISP=SHR,DSN=&MIG.ADH13.FADH13
//*--------------<FICHIERS CIBLES>---------------------------------
//*WFADH13  DD DSN=&&WFADH13,
//*            DISP=(NEW,PASS),
//*            SPACE=(TRK,(5000,2500),RLSE)
//*BAC-063 - AJO le 20/08/2013
//*STAGAHMA DD DSN=&CIB.TAGAHMA,
//STAGAHMA DD DSN=&MIG.ADH13.TAGAHMA,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STABADHS DD DSN=&CIB.TADHADH,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADHS   DD DSN=&MIG.ADH13.FADHS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH13  DD DSN=&MIG.ADH13.FADH13.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAGAHMA DD DSN=&MIG.ADH13.TAGAHMA.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTABADHS DD DSN=&MIG.ADH13.TABADHS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRADH30
//*----------------------------------------------------------------
//DEL3001       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STADHADH DD DSN=&MIG.ADH30.TADHADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TADHADH
//*----------------------------------------------------------------
//SORT3001 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STADHADH SORT ADHSADH-C-GRPT-GEST ASC [94:1—
//* STADHADH SORT ADHSADH-C-ADH ASC [95:9—
//* STADHADH SORT ADHSADH-C-ORDRE ASC [104:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.TADHADH
//SORTIN   DD DISP=SHR,DSN=&CIB.TADHADH
//SORTOUT  DD DSN=&MIG.ADH30.TADHADH,
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
//DEL3002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFADH30  DD DSN=&MIG.ADH30.FADH30,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL3003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRADH30,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRADH30,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRADH30,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTADHADH DD DSN=&MIG.ADH30.TADHADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA03ADH  DD DSN=&MIG.ADH30.A03ADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFADH30  DD DSN=&MIG.ADH30.FADH30.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRADH30
//*----------------------------------------------------------------
//PPRADH30 EXEC PGM=PPRADH30,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRADH30,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRADH30,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRADH30,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STADHADH DD DISP=SHR,DSN=&MIG.ADH30.TADHADH
//SA03ADH  DD DISP=SHR,DSN=&MIG.ADH05.ADH
//*--------------<FICHIERS CIBLES>---------------------------------
//SFADH30  DD DSN=&MIG.ADH30.FADH30,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTADHADH DD DSN=&MIG.ADH30.TADHADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA03ADH  DD DSN=&MIG.ADH30.A03ADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFADH30  DD DSN=&MIG.ADH30.FADH30.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*---------------*
//*PPRADH32
//*---------------*
//DEL3201  EXEC PGM=IEFBR14
//SYSPRINT DD SYOUT=*
//STABADHS DD DSN=&MIG.ADH32.TABADHS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TABADHS
//*----------------------------------------------------------------
//SORT3201 EXEC PGM=SORT
//* DEBUT CRITERE XGEN
//* TABADHS SORT ADHSADH-ID-TECH-UR-ADHR ASC [21:20]
//* TABADHS SORT ADHSADH-NO-ORD-ADHS ASC [41:3]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TADHADH
//SORTIN   DD DISP=SHR,DSN=&CIB.TADHADH
//SORTOUT  DD DSN=&MIG.ADH32.TABADHS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,20,CH,A,
               41,3,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3202  EXEC PGM=IEFBR14
//SYSPRINT DD SYSOUT=*
//SOABADHS DD DSN=&MIG.ADH32.OABADHS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
/*
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION
//*----------------------------------------------------------------
//PPRADH32 EXEC PGM=PPRADH32
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DISP=SHR,DSN=&ANO.PPRADH32
//COMPTEUR DD DISP=SHR,DSN=&CPT.PPRADH32
//FICTRACE DD DISP=SHR,DSN=&TRC.PPRADH32
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STABADHS DD DISP=SHR,DSN=&MIG.ADH32.TABADHS
//*--------------<FICHIERS CIBLES>---------------------------------
//SOABADHS DD DSN=&MIG.ADH32.OABADHS,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,500),RLSE)
//*
/*
//*--------------------*
//*PPRADH32
//*--------------------*
//DEL3301  EXEC PGM=IEFBR14
//SYSPRINT DD SYOUT=*
//STABADHS DD DSN=&MIG.ADH33.TABADHS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TABADHS
//*----------------------------------------------------------------
//SORT3301 EXEC PGM=SORT
//* DEBUT CRITERE XGEN
//* TABADHS SORT ADHSADH-C-GRPT-GEST ASC [94:1]
//* TABADHS SORT ADHSADH-C-ADH ASC [95:9]
//* TABADHS SORT ADHSADH-C-ORDRE ASC [104:4]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TADHADH
//SORTIN   DD DISP=SHR,DSN=&MIG.ADH32.OABADHS
//SORTOUT  DD DSN=&MIG.ADH33.TABADHS,
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
//DEL3302  EXEC PGM=IEFBR14
//SYSPRINT DD SYSOUT=*
//STAGAHSA DD DSN=&CIB.TAGAHSA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION
//*----------------------------------------------------------------
//PPRADH33 EXEC PGM=PPRADH33
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DISP=SHR,DSN=&ANO.PPRADH33
//COMPTEUR DD DISP=SHR,DSN=&CPT.PPRADH33
//FICTRACE DD DISP=SHR,DSN=&TRC.PPRADH33
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//PRELADH  DD DISP=SHR,DSN=&TAB.PRELADH
//STABADHS DD DISP=SHR,DSN=&MIG.ADH33.TABADHS
//SA3ADHER DD DISP=SHR,DSN=&MIG.ADH05.ADH
//*--------------<FICHIERS CIBLES>---------------------------------
//STAGAHSA DD DSN=&CIB.TAGAHSA,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,500),RLSE)
//*
/*
//*---------------------------*
//*JCL PROGRAMME PPRADH37      *
//*---------------------------*
//DEL3701  EXEC PGM=IEFBR14
//SYSPRINT DD SYOUT=*
//STADHADH DD DSN=&MIG.ADH37.TADHADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TADHADH
//*----------------------------------------------------------------
//SORT3703 EXEC PGM=SORT
//* DEBUT CRITERE XGEN
//* TADHADH SORT ADHSADH-NO-REF-GPS-ADHS (1:14) ASC [44:14]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TADHADH
//SORTIN   DD DISP=SHR,DSN=&CIB.TADHADH
//SORTOUT  DD DSN=&MIG.ADH37.TADHADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(44,14,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL3702  EXEC PGM=IEFBR14
//SYSPRINT DD SYSOUT=*
//STAGABNA DD DSN=&CIB.TAGABNA,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION
//*----------------------------------------------------------------
//PPRADH37 EXEC PGM=PPRADH37
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DISP=SHR,DSN=&ANO.PPRADH37
//COMPTEUR DD DISP=SHR,DSN=&CPT.PPRADH37
//FICTRACE DD DISP=SHR,DSN=&TRC.PPRADH37
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//PRELADH  DD DISP=SHR,DSN=&TAB.PRELADH
//STABADHS DD DISP=SHR,DSN=&MIG.ADH37.TADHADH
//SA3ADHER DD DISP=SHR,DSN=&MIG.ADH05.ADH
//*--------------<FICHIERS CIBLES>---------------------------------
//STAGABNA DD DSN=&CIB.TAGABNA,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(5000,500),RLSE)
//*
/*
//*----------------------------------------------------------------
//*
//* FIN DU JCL DE MIGRATION
