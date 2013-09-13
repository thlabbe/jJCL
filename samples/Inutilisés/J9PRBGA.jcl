//J9VHBGA  JOB UTI00TX0,'VH BACGAR B05-BGA',CLASS=Z,MSGCLASS=X,         JOB00235
//*        RESTART=DEL0402,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=PLURM.MOVE.VH.JCL.PROD
//MEMBER   INCLUDE MEMBER=$$VHEX
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHBGA01
//*----------------------------------------------------------------
//DEL0101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STAGACNT DD DSN=&MIG.BGA01.TAGACNT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAGAGAR DD DSN=&MIG.BGA01.TAGAGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TAGACNT
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STAGACNT SORT TAGACNT-ID-TECH-UR-ADHR ASC [1:20—
//* STAGACNT SORT TAGACNT-NO-ORD-ADHS ASC [21:3—
//* STAGACNT SORT TAGACNT-NO-ORD-CTR ASC [24:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&REF.TAGACNT
//SORTOUT  DD DSN=&MIG.BGA01.TAGACNT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               24,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TAGAGAR
//*----------------------------------------------------------------
//SORT0102 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STAGAGAR SORT TAGAGAR-ID-TECH-UR-ADHR ASC [1:20—
//* STAGAGAR SORT TAGAGAR-NO-ORD-ADHS ASC [21:3—
//* STAGAGAR SORT TAGAGAR-NO-ORD-CTR ASC [24:3—
//* STAGAGAR SORT TAGAGAR-NO-ORD-CPRO ASC [27:3—
//* STAGAGAR SORT TAGAGAR-NO-ORD-GARA ASC [30:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&REF.TAGAGAR
//SORTOUT  DD DSN=&MIG.BGA01.TAGAGAR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               24,3,CH,A,
               27,3,CH,A,
               30,3,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STRV1PRO DD DSN=&MIG.BGA01.TRV1PRO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRVGMP  DD DSN=&MIG.BGA01.TRVGMP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRVGAR  DD DSN=&MIG.BGA01.TRVGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STRV1FOR DD DSN=&MIG.BGA01.TRV1FOR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHBGA01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHBGA01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHBGA01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZREFGAR  DD DSN=&MIG.BGA01.REFGAR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGACNT DD DSN=&MIG.BGA01.TAGACNT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGAGAR DD DSN=&MIG.BGA01.TAGAGAR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRV1PRO DD DSN=&MIG.BGA01.TRV1PRO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRVGMP  DD DSN=&MIG.BGA01.TRVGMP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRVGAR  DD DSN=&MIG.BGA01.TRVGAR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRV1FOR DD DSN=&MIG.BGA01.TRV1FOR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHBGA01
//*----------------------------------------------------------------
//PVHBGA01 EXEC PGM=PVHBGA01,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHBGA01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHBGA01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHBGA01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//REFGAR   DD DISP=SHR,DSN=&TABVH.REFGAR
//STAGACNT DD DISP=SHR,DSN=&MIG.BGA01.TAGACNT
//STAGAGAR DD DISP=SHR,DSN=&MIG.BGA01.TAGAGAR
//*--------------<FICHIERS CIBLES>---------------------------------
//STRV1PRO DD DSN=&MIG.BGA01.TRV1PRO,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRVGMP  DD DSN=&MIG.BGA01.TRVGMP,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRVGAR  DD DSN=&MIG.BGA01.TRVGAR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//STRV1FOR DD DSN=&MIG.BGA01.TRV1FOR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZREFGAR  DD DSN=&MIG.BGA01.REFGAR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAGACNT DD DSN=&MIG.BGA01.TAGACNT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAGAGAR DD DSN=&MIG.BGA01.TAGAGAR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRV1PRO DD DSN=&MIG.BGA01.TRV1PRO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRVGMP  DD DSN=&MIG.BGA01.TRVGMP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRVGAR  DD DSN=&MIG.BGA01.TRVGAR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRV1FOR DD DSN=&MIG.BGA01.TRV1FOR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHBGA02
//*----------------------------------------------------------------
//DEL0201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STRV1FOR DD DSN=&MIG.BGA02.TRV1FOR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAGACGF DD DSN=&MIG.BGA02.TAGACGF,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TRV1FOR
//*----------------------------------------------------------------
//SORT0201 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STRV1FOR SORT TRV1FOR-ID-TECH-UR-ADHR ASC [1:20—
//* STRV1FOR SORT TRV1FOR-NO-ORD-ADHS ASC [21:3—
//* STRV1FOR SORT TRV1FOR-NO-ORD-CTR ASC [24:3—
//* STRV1FOR SORT TRV1FOR-NO-ORD-CPRO ASC [27:3—
//* STRV1FOR SORT TRV1FOR-NO-ORD-GARA ASC [30:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.BGA01.TRV1FOR
//SORTOUT  DD DSN=&MIG.BGA02.TRV1FOR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               24,3,CH,A,
               27,3,CH,A,
               30,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TAGACGF
//*----------------------------------------------------------------
//SORT0202 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STAGACGF SORT TAGACGF-ID-TECH-UR-ADHR ASC [1:20—
//* STAGACGF SORT TAGACGF-NO-ORD-ADHS ASC [21:3—
//* STAGACGF SORT TAGACGF-NO-ORD-CTR ASC [24:3—
//* STAGACGF SORT TAGACGF-NO-ORD-CPRO ASC [27:3—
//* STAGACGF SORT TAGACGF-NO-ORD-GARA ASC [30:3—
//* STAGACGF SORT TAGACGF-NO-ORD-COTF-AMND ASC [33:3—
//*               33,3,CH,A,
//* STAGACGF SORT TAGACGF-DT-DEFF-COTF-AMND ASC [36:10—
//* STAGACGF SORT TAGACGF-DT-FEFF-COTF-AMND ASC [46:10—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&REF.TAGACGF
//SORTOUT  DD DSN=&MIG.BGA02.TAGACGF,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               24,3,CH,A,
               27,3,CH,A,
               30,3,CH,A,
               36,10,CH,A,
               46,10,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0202       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STRV2FOR DD DSN=&MIG.BGA02.TRV2FOR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHBGA02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHBGA02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHBGA02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRV1FOR DD DSN=&MIG.BGA02.TRV1FOR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGACGF DD DSN=&MIG.BGA02.TAGACGF.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRV2FOR DD DSN=&MIG.BGA02.TRV2FOR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHBGA02
//*----------------------------------------------------------------
//PVHBGA02 EXEC PGM=PVHBGA02,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHBGA02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHBGA02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHBGA02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STRV1FOR DD DISP=SHR,DSN=&MIG.BGA02.TRV1FOR
//STAGACGF DD DISP=SHR,DSN=&MIG.BGA02.TAGACGF
//*--------------<FICHIERS CIBLES>---------------------------------
//STRV2FOR DD DSN=&MIG.BGA02.TRV2FOR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRV1FOR DD DSN=&MIG.BGA02.TRV1FOR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAGACGF DD DSN=&MIG.BGA02.TAGACGF.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRV2FOR DD DSN=&MIG.BGA02.TRV2FOR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHBGA03
//*----------------------------------------------------------------
//DEL0301       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STRV1PRO DD DSN=&MIG.BGA03.TRV1PRO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAGACGP DD DSN=&MIG.BGA03.TAGACGP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STAGATRS DD DSN=&MIG.BGA03.TAGATRS,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TRV1PRO
//*----------------------------------------------------------------
//SORT0301 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STRV1PRO SORT TRV1PRO-ID-TECH-UR-ADHR ASC [1:20—
//* STRV1PRO SORT TRV1PRO-NO-ORD-ADHS ASC [21:3—
//* STRV1PRO SORT TRV1PRO-NO-ORD-CTR ASC [24:3—
//* STRV1PRO SORT TRV1PRO-NO-ORD-CPRO ASC [27:3—
//* STRV1PRO SORT TRV1PRO-NO-ORD-GARA ASC [30:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.BGA01.TRV1PRO
//SORTOUT  DD DSN=&MIG.BGA03.TRV1PRO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               24,3,CH,A,
               27,3,CH,A,
               30,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TAGACGP
//*----------------------------------------------------------------
//SORT0302 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STAGACGP SORT TAGACGP-ID-TECH-UR-ADHR ASC [1:20—
//* STAGACGP SORT TAGACGP-NO-ORD-ADHS ASC [21:3—
//* STAGACGP SORT TAGACGP-NO-ORD-CTR ASC [24:3—
//* STAGACGP SORT TAGACGP-NO-ORD-CPRO ASC [27:3—
//* STAGACGP SORT TAGACGP-NO-ORD-GARA ASC [30:3—
//* STAGACGP SORT TAGACGP-NO-ORD-COTP-AMND ASC [33:3—
//*               33,3,CH,A,
//* STAGACGP SORT TAGACGP-DT-DEFF-COTP-AMND ASC [36:10—
//* STAGACGP SORT TAGACGP-DT-FEFF-COTP-AMND ASC [46:10—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&REF.TAGACGP
//SORTOUT  DD DSN=&MIG.BGA03.TAGACGP,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               24,3,CH,A,
               27,3,CH,A,
               30,3,CH,A,
               36,10,CH,A,
               46,10,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TAGATRS
//*----------------------------------------------------------------
//SORT0303 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STAGATRS SORT TAGATRS-ID-TECH-UR-ADHR ASC [1:20—
//* STAGATRS SORT TAGATRS-NO-ORD-ADHS ASC [21:3—
//* STAGATRS SORT TAGATRS-NO-ORD-CTR ASC [24:3—
//* STAGATRS SORT TAGATRS-NO-ORD-CPRO ASC [27:3—
//* STAGATRS SORT TAGATRS-NO-ORD-GARA ASC [30:3—
//* STAGATRS SORT TAGATRS-NO-ORD-TX-RIST ASC [33:3—
//*               33,3,CH,A,
//* STAGATRS SORT TAGATRS-DT-DEFF-TX-RIST ASC [36:10—
//* STAGATRS SORT TAGATRS-DT-FEFF-TX-RIST ASC [46:10—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&REF.TAGATRS
//SORTOUT  DD DSN=&MIG.BGA03.TAGATRS,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               24,3,CH,A,
               27,3,CH,A,
               30,3,CH,A,
               36,10,CH,A,
               46,10,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0302       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STRV2PRO DD DSN=&MIG.BGA03.TRV2PRO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0303       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHBGA03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHBGA03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHBGA03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRV1PRO DD DSN=&MIG.BGA03.TRV1PRO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGACGP DD DSN=&MIG.BGA03.TAGACGP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTAGATRS DD DSN=&MIG.BGA03.TAGATRS.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRV2PRO DD DSN=&MIG.BGA03.TRV2PRO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHBGA03
//*----------------------------------------------------------------
//PVHBGA03 EXEC PGM=PVHBGA03,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHBGA03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHBGA03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHBGA03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STRV1PRO DD DISP=SHR,DSN=&MIG.BGA03.TRV1PRO
//STAGACGP DD DISP=SHR,DSN=&MIG.BGA03.TAGACGP
//STAGATRS DD DISP=SHR,DSN=&MIG.BGA03.TAGATRS
//*--------------<FICHIERS CIBLES>---------------------------------
//STRV2PRO DD DSN=&MIG.BGA03.TRV2PRO,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRV1PRO DD DSN=&MIG.BGA03.TRV1PRO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAGACGP DD DSN=&MIG.BGA03.TAGACGP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTAGATRS DD DSN=&MIG.BGA03.TAGATRS.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRV2PRO DD DSN=&MIG.BGA03.TRV2PRO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHBGA04
//*----------------------------------------------------------------
//DEL0401       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STRV2FOR DD DSN=&MIG.BGA04.TRV2FOR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCATFOR  DD DSN=&MIG.BGA04.CATFOR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TRV2FOR
//*----------------------------------------------------------------
//SORT0401 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STRV2FOR SORT TRV2FOR-ID-GARA-REF(6:4) ASC +66:4]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.BGA02.TRV2FOR
//SORTOUT  DD DSN=&MIG.BGA04.TRV2FOR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(66,4,CH,A,
               1,20,CH,A,
               21,3,CH,A,
               24,3,CH,A,
               27,3,CH,A,
               30,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER CATFOR
//*----------------------------------------------------------------
//SORT0402 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCATFOR SORT CATFOR-ID-GARA-REF ASC [1:4—
//* SCATFOR SORT CATFOR-DT-DEB-PRDE-FORF ASC [97:10—
//* SCATFOR SORT CATFOR-DT-FIN-PRDE-FORF ASC [109:10—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//*SORTIN   DD DISP=SHR,DSN=&TABVH.CATFOR
//SORTIN   DD DISP=SHR,DSN=&TABVH.CATFORB
//SORTOUT  DD DSN=&MIG.BGA04.CATFOR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,4,CH,A,
               97,10,CH,A,
               109,10,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0402       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SRESFOR  DD DSN=&CIB.RESFOR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0403       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHBGA04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHBGA04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHBGA04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRV2FOR DD DSN=&MIG.BGA04.TRV2FOR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCATFOR  DD DSN=&MIG.BGA04.CATFOR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZRESFOR  DD DSN=&MIG.BGA04.RESFOR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHBGA04
//*----------------------------------------------------------------
//PVHBGA04 EXEC PGM=PVHBGA04,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHBGA04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHBGA04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHBGA04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STRV2FOR DD DISP=SHR,DSN=&MIG.BGA04.TRV2FOR
//SCATFOR  DD DISP=SHR,DSN=&MIG.BGA04.CATFOR
//*--------------<FICHIERS CIBLES>---------------------------------
//SRESFOR  DD DSN=&CIB.RESFOR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRV2FOR DD DSN=&MIG.BGA04.TRV2FOR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCATFOR  DD DSN=&MIG.BGA04.CATFOR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZRESFOR  DD DSN=&MIG.BGA04.RESFOR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHBGA05
//*----------------------------------------------------------------
//DEL0501       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STRV2PRO DD DSN=&MIG.BGA05.TRV2PRO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCATPRO  DD DSN=&MIG.BGA05.CATPRO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TRV2PRO
//*----------------------------------------------------------------
//SORT0501 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STRV2PRO SORT TRV2PRO-ID-GARA-REF(6:4) ASC +66:4]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.BGA03.TRV2PRO
//SORTOUT  DD DSN=&MIG.BGA05.TRV2PRO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(66,4,CH,A,
               1,20,CH,A,
               21,3,CH,A,
               24,3,CH,A,
               27,3,CH,A,
               30,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER CATPRO
//*----------------------------------------------------------------
//SORT0502 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCATPRO SORT CATPRO-ID-GARA-REF ASC [1:4—
//* SCATPRO SORT CATPRO-DT-DEB-PRDE-PRPR ASC [97:10—
//* SCATPRO SORT CATPRO-DT-FIN-PRDE-PRPR ASC [109:10—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//*SORTIN   DD DISP=SHR,DSN=&TABVH.CATPRO
//SORTIN   DD DISP=SHR,DSN=&TABVH.CATPROB
//SORTOUT  DD DSN=&MIG.BGA05.CATPRO,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,4,CH,A,
               97,10,CH,A,
               109,10,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0502       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SRESPRO  DD DSN=&CIB.RESPRO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0503       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHBGA05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHBGA05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHBGA05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRV2PRO DD DSN=&MIG.BGA05.TRV2PRO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCATPRO  DD DSN=&MIG.BGA05.CATPRO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZRESPRO  DD DSN=&MIG.BGA05.RESPRO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHBGA05
//*----------------------------------------------------------------
//PVHBGA05 EXEC PGM=PVHBGA05,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHBGA05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHBGA05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHBGA05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STRV2PRO DD DISP=SHR,DSN=&MIG.BGA05.TRV2PRO
//SCATPRO  DD DISP=SHR,DSN=&MIG.BGA05.CATPRO
//*--------------<FICHIERS CIBLES>---------------------------------
//SRESPRO  DD DSN=&CIB.RESPRO,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRV2PRO DD DSN=&MIG.BGA05.TRV2PRO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCATPRO  DD DSN=&MIG.BGA05.CATPRO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZRESPRO  DD DSN=&MIG.BGA05.RESPRO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHBGA06
//*----------------------------------------------------------------
//DEL0601       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STRVGMP  DD DSN=&MIG.BGA06.TRVGMP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCATGMP  DD DSN=&MIG.BGA06.CATGMP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TRVGMP
//*----------------------------------------------------------------
//SORT0601 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STRVGMP SORT TRVGMP-ID-GARA-REF(6:4) ASC +66:4]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.BGA01.TRVGMP
//SORTOUT  DD DSN=&MIG.BGA06.TRVGMP,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(66,4,CH,A,
               1,20,CH,A,
               21,3,CH,A,
               24,3,CH,A,
               27,3,CH,A,
               30,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER CATGMP
//*----------------------------------------------------------------
//SORT0602 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCATGMP SORT CATGMP-ID-GARA-REF ASC [1:4—
//* SCATGMP SORT CATGMP-DT-DEB-PRDE-GMP ASC [97:10—
//* SCATGMP SORT CATGMP-DT-FIN-PRDE-GMP ASC [109:10—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//*SORTIN   DD DISP=SHR,DSN=&TABVH.CATGMP
//SORTIN   DD DISP=SHR,DSN=&TABVH.CATGMPB
//SORTOUT  DD DSN=&MIG.BGA06.CATGMP,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,4,CH,A,
               97,10,CH,A,
               109,10,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0602       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SRESGMP  DD DSN=&CIB.RESGMP,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0603       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHBGA06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHBGA06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHBGA06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRVGMP  DD DSN=&MIG.BGA06.TRVGMP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCATGMP  DD DSN=&MIG.BGA06.CATGMP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZRESGMP  DD DSN=&MIG.BGA06.RESGMP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHBGA06
//*----------------------------------------------------------------
//PVHBGA06 EXEC PGM=PVHBGA06,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHBGA06,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHBGA06,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHBGA06,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STRVGMP  DD DISP=SHR,DSN=&MIG.BGA06.TRVGMP
//SCATGMP  DD DISP=SHR,DSN=&MIG.BGA06.CATGMP
//*--------------<FICHIERS CIBLES>---------------------------------
//SRESGMP  DD DSN=&CIB.RESGMP,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRVGMP  DD DSN=&MIG.BGA06.TRVGMP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCATGMP  DD DSN=&MIG.BGA06.CATGMP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZRESGMP  DD DSN=&MIG.BGA06.RESGMP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHBGA07
//*----------------------------------------------------------------
//DEL0701       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STRVGAR  DD DSN=&MIG.BGA07.TRVGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCATGAR  DD DSN=&MIG.BGA07.CATGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TRVGAR
//*----------------------------------------------------------------
//SORT0701 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STRVGAR SORT TRVGAR-ID-GARA-REF(6:4) ASC +66:4]
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.BGA01.TRVGAR
//SORTOUT  DD DSN=&MIG.BGA07.TRVGAR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(66,4,CH,A,
               1,20,CH,A,
               21,3,CH,A,
               24,3,CH,A,
               27,3,CH,A,
               30,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER CATGAR
//*----------------------------------------------------------------
//SORT0702 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCATGAR SORT CATGAR-ID-GARA-REF ASC [1:4—
//* SCATGAR SORT CATGAR-DT-DEB-PRDE-GARA ASC [97:10—
//* SCATGAR SORT CATGAR-DT-FIN-PRDE-GARA ASC [109:10—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//*SORTIN   DD DISP=SHR,DSN=&TABVH.CATGAR
//SORTIN   DD DISP=SHR,DSN=&TABVH.CATGARB
//SORTOUT  DD DSN=&MIG.BGA07.CATGAR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,4,CH,A,
               97,10,CH,A,
               109,10,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0702       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SRESGAR  DD DSN=&CIB.RESGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0703       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHBGA07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHBGA07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHBGA07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTRVGAR  DD DSN=&MIG.BGA07.TRVGAR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCATGAR  DD DSN=&MIG.BGA07.CATGAR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZRESGAR  DD DSN=&MIG.BGA07.RESGAR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHBGA07
//*----------------------------------------------------------------
//PVHBGA07 EXEC PGM=PVHBGA07,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHBGA07,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHBGA07,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHBGA07,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STRVGAR  DD DISP=SHR,DSN=&MIG.BGA07.TRVGAR
//SCATGAR  DD DISP=SHR,DSN=&MIG.BGA07.CATGAR
//*--------------<FICHIERS CIBLES>---------------------------------
//SRESGAR  DD DSN=&CIB.RESGAR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTRVGAR  DD DSN=&MIG.BGA07.TRVGAR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCATGAR  DD DSN=&MIG.BGA07.CATGAR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZRESGAR  DD DSN=&MIG.BGA07.RESGAR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHBGA08
//*----------------------------------------------------------------
//DEL0801       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0802       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SBACGAR  DD DSN=&CIB.BACGAR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0803       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHBGA08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHBGA08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHBGA08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZRESPRO  DD DSN=&MIG.BGA08.RESPRO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZRESGMP  DD DSN=&MIG.BGA08.RESGMP.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZRESFOR  DD DSN=&MIG.BGA08.RESFOR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZRESGAR  DD DSN=&MIG.BGA08.RESGAR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZBACGAR  DD DSN=&MIG.BGA08.BACGAR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHBGA08
//*----------------------------------------------------------------
//PVHBGA08 EXEC PGM=PVHBGA08,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHBGA08,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHBGA08,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHBGA08,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//RESPRO   DD DISP=SHR,DSN=&CIB.RESPRO
//RESGMP   DD DISP=SHR,DSN=&CIB.RESGMP
//RESFOR   DD DISP=SHR,DSN=&CIB.RESFOR
//RESGAR   DD DISP=SHR,DSN=&CIB.RESGAR
//*--------------<FICHIERS CIBLES>---------------------------------
//SBACGAR  DD DSN=&CIB.BACGAR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZRESPRO  DD DSN=&MIG.BGA08.RESPRO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZRESGMP  DD DSN=&MIG.BGA08.RESGMP.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZRESFOR  DD DSN=&MIG.BGA08.RESFOR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZRESGAR  DD DSN=&MIG.BGA08.RESGAR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZBACGAR  DD DSN=&MIG.BGA08.BACGAR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//* FIN DU JCL DE MIGRATION
