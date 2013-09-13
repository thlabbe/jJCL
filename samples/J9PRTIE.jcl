//J9VHTIE  JOB UTI00TX0,'PR GRECCO G10-TIE',CLASS=Z,MSGCLASS=X,
//*        RESTART=DEL0302,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=PLURM.MOVE.PR.JCL.PROD
//MEMBER   INCLUDE MEMBER=$$PRT
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRTIE01
//*----------------------------------------------------------------
//DEL0101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SA01ENTR DD DSN=&MIG.TIE01.A01ENTR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01ADRE DD DSN=&MIG.TIE01.A01ADRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER A01ENTR
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01ENTR SORT A01-ENTR-ETAB-C-ID-PERS ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01ENTR
//SORTIN   DD DISP=SHR,DSN=&SRC.A01ENTR
//SORTOUT  DD DSN=&MIG.TIE01.A01ENTR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01ADRE
//*----------------------------------------------------------------
//SORT0102 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01ADRE SORT A01-ADRESSE-C-ID-PERS ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01ADRES
//SORTIN   DD DISP=SHR,DSN=&SRC.A01ADRES
//SORTOUT  DD DSN=&MIG.TIE01.A01ADRE,
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
//SFICTIJ0 DD DSN=&MIG.TIE01.FICTIJ0,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFICCPT1 DD DSN=&MIG.TIE01.FICCPT1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRTIE01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRTIE01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRTIE01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01ENTR DD DSN=&MIG.TIE01.A01ENTR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01ADRE DD DSN=&MIG.TIE01.A01ADRE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFICTIJ0 DD DSN=&MIG.TIE01.FICTIJ0.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFICCPT1 DD DSN=&MIG.TIE01.FICCPT1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRTIE01
//*----------------------------------------------------------------
//PPRTIE01 EXEC PGM=PPRTIE01,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRTIE01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRTIE01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRTIE01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SA01ENTR DD DISP=SHR,DSN=&MIG.TIE01.A01ENTR
//SA01ADRE DD DISP=SHR,DSN=&MIG.TIE01.A01ADRE
//*--------------<FICHIERS CIBLES>---------------------------------
//SFICTIJ0 DD DSN=&MIG.TIE01.FICTIJ0,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFICCPT1 DD DSN=&MIG.TIE01.FICCPT1,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01ENTR DD DSN=&MIG.TIE01.A01ENTR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01ADRE DD DSN=&MIG.TIE01.A01ADRE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFICTIJ0 DD DSN=&MIG.TIE01.FICTIJ0.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFICCPT1 DD DSN=&MIG.TIE01.FICCPT1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRTIE02
//*----------------------------------------------------------------
//DEL0201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFICTIJ0 DD DSN=&MIG.TIE02.FICTIJ0,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01TIER DD DSN=&MIG.TIE02.A01TIER,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FICTIJ0
//*----------------------------------------------------------------
//SORT0201 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFICTIJ0 SORT FICTIJ-C-ID-PERS-ENTR ASC [1:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYMIG.CPY0818
//SORTIN   DD DISP=SHR,DSN=&MIG.TIE01.FICTIJ0
//SORTOUT  DD DSN=&MIG.TIE02.FICTIJ0,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01TIER
//*----------------------------------------------------------------
//SORT0202 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01TIER SORT A01-TIERS-TRIB-C-ID-PERS-ENTR ASC [15:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01TIERT
//SORTIN   DD DISP=SHR,DSN=&SRC.A01TIERT
//SORTOUT  DD DSN=&MIG.TIE02.A01TIER,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(15,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0202       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFICTIJ1 DD DSN=&MIG.TIE02.FICTIJ1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRTIE02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRTIE02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRTIE02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFICTIJ0 DD DSN=&MIG.TIE02.FICTIJ0.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01TIER DD DSN=&MIG.TIE02.A01TIER.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFICTIJ1 DD DSN=&MIG.TIE02.FICTIJ1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRTIE02
//*----------------------------------------------------------------
//PPRTIE02 EXEC PGM=PPRTIE02,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRTIE02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRTIE02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRTIE02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SFICTIJ0 DD DISP=SHR,DSN=&MIG.TIE02.FICTIJ0
//SA01TIER DD DISP=SHR,DSN=&MIG.TIE02.A01TIER
//*--------------<FICHIERS CIBLES>---------------------------------
//SFICTIJ1 DD DSN=&MIG.TIE02.FICTIJ1,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFICTIJ0 DD DSN=&MIG.TIE02.FICTIJ0.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01TIER DD DSN=&MIG.TIE02.A01TIER.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFICTIJ1 DD DSN=&MIG.TIE02.FICTIJ1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRTIE03
//*----------------------------------------------------------------
//DEL0301       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SA01TRIB DD DSN=&MIG.TIE03.A01TRIB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01ADRE DD DSN=&MIG.TIE03.A01ADRE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01COMM DD DSN=&MIG.TIE03.A01COMM,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SA01FRAI DD DSN=&MIG.TIE03.A01FRAI,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER A01TRIB
//*----------------------------------------------------------------
//SORT0302 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01TRIB SORT A01-TRIBUNAL-C-ID-PERS ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01TRIBU
//SORTIN   DD DISP=SHR,DSN=&SRC.A01TRIBU
//SORTOUT  DD DSN=&MIG.TIE03.A01TRIB,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01ADRE
//*----------------------------------------------------------------
//SORT0303 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01ADRE SORT A01-ENTR-ETAB-C-ID-PERS ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01ADRES
//SORTIN   DD DISP=SHR,DSN=&SRC.A01ADRES
//SORTOUT  DD DSN=&MIG.TIE03.A01ADRE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01COMM
//*----------------------------------------------------------------
//SORT0304 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01COMM SORT A01-COMM-TRIB-C-ID-PERS ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01COMM
//SORTIN   DD DISP=SHR,DSN=&SRC.A01COMM
//SORTOUT  DD DSN=&MIG.TIE03.A01COMM,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER A01FRAI
//*----------------------------------------------------------------
//SORT0305 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SA01FRAI SORT A01-FRAIS-PREVUS-C-ID-PERS ASC [8:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYSRC.A01FRAIS
//SORTIN   DD DISP=SHR,DSN=&SRC.A01FRAIS
//SORTOUT  DD DSN=&MIG.TIE03.A01FRAI,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(8,12,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0302       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFICTIJ2 DD DSN=&MIG.TIE03.FICTIJ2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFICCPT2 DD DSN=&MIG.TIE03.FICCPT2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SMTC     DD DSN=&CIB.MTC,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0303       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRTIE03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRTIE03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRTIE03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01TRIB DD DSN=&MIG.TIE03.A01TRIB.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01ADRE DD DSN=&MIG.TIE03.A01ADRE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01COMM DD DSN=&MIG.TIE03.A01COMM.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZA01FRAI DD DSN=&MIG.TIE03.A01FRAI.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFICTIJ2 DD DSN=&MIG.TIE03.FICTIJ2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFICCPT2 DD DSN=&MIG.TIE03.FICCPT2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZMTC     DD DSN=&MIG.TIE03.MTC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRTIE03
//*----------------------------------------------------------------
//PPRTIE03 EXEC PGM=PPRTIE03,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRTIE03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRTIE03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRTIE03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SA01TRIB DD DISP=SHR,DSN=&MIG.TIE03.A01TRIB
//SA01ADRE DD DISP=SHR,DSN=&MIG.TIE03.A01ADRE
//SA01COMM DD DISP=SHR,DSN=&MIG.TIE03.A01COMM
//SA01FRAI DD DISP=SHR,DSN=&MIG.TIE03.A01FRAI
//TC1TCRC  DD DISP=SHR,DSN=&TAB.TC1TCRC
//*--------------<FICHIERS CIBLES>---------------------------------
//SFICTIJ2 DD DSN=&MIG.TIE03.FICTIJ2,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFICCPT2 DD DSN=&MIG.TIE03.FICCPT2,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SMTC     DD DSN=&CIB.MTC,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01TRIB DD DSN=&MIG.TIE03.A01TRIB.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01ADRE DD DSN=&MIG.TIE03.A01ADRE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01COMM DD DSN=&MIG.TIE03.A01COMM.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZA01FRAI DD DSN=&MIG.TIE03.A01FRAI.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFICTIJ2 DD DSN=&MIG.TIE03.FICTIJ2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFICCPT2 DD DSN=&MIG.TIE03.FICCPT2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZMTC     DD DSN=&MIG.TIE03.MTC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRTIE04
//*----------------------------------------------------------------
//DEL0401       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SFICTIJ  DD DSN=&MIG.TIE04.FICTIJ,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFICCPT  DD DSN=&MIG.TIE04.FICCPT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER FICTIJ
//*----------------------------------------------------------------
//SORT0401 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFICTIJ SORT FICTIJ-ENR(39:2) ASC [39:2—
//* SFICTIJ SORT FICTIJ-ENR(44:1) ASC [44:1—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.TIJ
//SORTIN01 DD DISP=SHR,DSN=&MIG.TIE02.FICTIJ1
//SORTIN02 DD DISP=SHR,DSN=&MIG.TIE03.FICTIJ2
//SORTOUT  DD DSN=&MIG.TIE04.FICTIJ,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(39,2,CH,A,
               44,1,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER FICCPT
//*----------------------------------------------------------------
//SORT0402 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SFICTIJ SORT FICTIJ-ENR(39:2) ASC [39:2—
//* SFICTIJ SORT FICTIJ-ENR(44:1) ASC [44:1—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//COPY     DD DISP=SHR,DSN=&CPYCIB.CPT
//SORTIN01 DD DISP=SHR,DSN=&MIG.TIE01.FICCPT1
//SORTIN02 DD DISP=SHR,DSN=&MIG.TIE03.FICCPT2
//SORTOUT  DD DSN=&MIG.TIE04.FICCPT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(39,2,CH,A,
               44,1,CH,A)
/*
//*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0402       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SCPT     DD DSN=&CIB.CPT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STIJ     DD DSN=&CIB.TIJ,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SFPTIE01 DD DSN=&CIB.FPTIE01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0403       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRTIE04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRTIE04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRTIE04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFICCPT  DD DSN=&MIG.TIE04.FICCPT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCPT     DD DSN=&MIG.TIE04.CPT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFICTIJ  DD DSN=&MIG.TIE04.FICTIJ.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTIJ     DD DSN=&MIG.TIE04.TIJ.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZFPTIE01 DD DSN=&MIG.TIE04.FPTIE01.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRTIE04
//*----------------------------------------------------------------
//PPRTIE04 EXEC PGM=PPRTIE04,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRTIE04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRTIE04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRTIE04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//FICCPT   DD DISP=SHR,DSN=&MIG.TIE04.FICCPT
//SFICTIJ  DD DISP=SHR,DSN=&MIG.TIE04.FICTIJ
//*--------------<FICHIERS CIBLES>---------------------------------
//SCPT     DD DSN=&CIB.CPT,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STIJ     DD DSN=&CIB.TIJ,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SFPTIE01 DD DSN=&CIB.FPTIE01,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZFICCPT  DD DSN=&MIG.TIE04.FICCPT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCPT     DD DSN=&MIG.TIE04.CPT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFICTIJ  DD DSN=&MIG.TIE04.FICTIJ.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTIJ     DD DSN=&MIG.TIE04.TIJ.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZFPTIE01 DD DSN=&MIG.TIE04.FPTIE01.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PPRTIE05
//*----------------------------------------------------------------
//DEL0501       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0502       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STIERSJ  DD DSN=&CIB.TIERSJ,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0503       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PPRTIE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PPRTIE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PPRTIE05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTIJ     DD DSN=&MIG.TIE05.TIJ.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCPT     DD DSN=&MIG.TIE05.CPT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZMTC     DD DSN=&MIG.TIE05.MTC.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTIERSJ  DD DSN=&MIG.TIE05.TIERSJ.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PPRTIE05
//*----------------------------------------------------------------
//PPRTIE05 EXEC PGM=PPRTIE05,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PPRTIE05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PPRTIE05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PPRTIE05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//TIJ      DD DISP=SHR,DSN=&CIB.TIJ
//CPT      DD DISP=SHR,DSN=&CIB.CPT
//MTC      DD DISP=SHR,DSN=&CIB.MTC
//*--------------<FICHIERS CIBLES>---------------------------------
//STIERSJ  DD DSN=&CIB.TIERSJ,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTIERSJ  DD DSN=&MIG.TIE05.TIERSJ.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTIJ     DD DSN=&MIG.TIE05.TIJ.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCPT     DD DSN=&MIG.TIE05.CPT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZMTC     DD DSN=&MIG.TIE05.MTC.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//* FIN DU JCL DE MIGRATION
