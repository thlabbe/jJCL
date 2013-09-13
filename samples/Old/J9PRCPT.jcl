//J9VHCPT  JOB UTI00TX0,'VH GRECCO G01-CPT',CLASS=Z,MSGCLASS=X,
//*        RESTART=DEL0702,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=PLURM.MOVE.VH.JCL.PROD
//MEMBER   INCLUDE MEMBER=$$VHT
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCPT01
//*----------------------------------------------------------------
//DEL0101       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STUECADH DD DSN=&MIG.CPT01.TUECADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB05OPER DD DSN=&MIG.CPT01.B05OPER,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TUECADH
//*----------------------------------------------------------------
//SORT0101 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STUECADH SORT TABUECAD-C-GRPT-GEST ASC [101:1—
//* STUECADH SORT TABUECAD-C-ADH ASC [102:9—
//* STUECADH SORT TABUECAD-C-ORDRE ASC [111:4—
//* FIN   CRITERE XGEN
//SORTWK01 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK02 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK03 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK04 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK05 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK06 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK07 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK08 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK09 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK10 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&REF.TUECADH
//SORTOUT  DD DSN=&MIG.CPT01.TUECADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(101,1,CH,A,
               102,9,CH,A,
               111,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B05OPER
//*----------------------------------------------------------------
//SORT0102 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05OPER SORT B05-OPER-COMPTE-C-GRPT-GEST ASC [41:1—
//* SB05OPER SORT B05-OPER-COMPTE-C-ADH ASC [43:9—
//* SB05OPER SORT B05-OPER-COMPTE-C-ORDRE ASC [52:5—
//* FIN   CRITERE XGEN
//SORTWK01 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK02 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK03 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK04 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK05 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK06 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK07 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK08 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK09 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SORTWK10 DD    UNIT=SYSDA,SPACE=(CYL,(100,100),RLSE)
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.B05OPER
//SORTOUT  DD DSN=&MIG.CPT01.B05OPER,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(CYL,(50,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(41,1,CH,A,
               43,9,CH,A,
               52,5,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0102       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SWKOPER1 DD DSN=&MIG.CPT01.WKOPER1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0103       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCPT01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCPT01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCPT01,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTUECADH DD DSN=&MIG.CPT01.TUECADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZB05OPER DD DSN=&MIG.CPT01.B05OPER.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZWKOPER1 DD DSN=&MIG.CPT01.WKOPER1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCPT01
//*----------------------------------------------------------------
//PVHCPT01 EXEC PGM=PVHCPT01,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCPT01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCPT01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCPT01,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STUECADH DD DISP=SHR,DSN=&MIG.CPT01.TUECADH
//SB05OPER DD DISP=SHR,DSN=&MIG.CPT01.B05OPER
//*--------------<FICHIERS CIBLES>---------------------------------
//SWKOPER1 DD DSN=&MIG.CPT01.WKOPER1,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTUECADH DD DSN=&MIG.CPT01.TUECADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZB05OPER DD DSN=&MIG.CPT01.B05OPER.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZWKOPER1 DD DSN=&MIG.CPT01.WKOPER1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCPT02
//*----------------------------------------------------------------
//DEL0201       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STUECADH DD DSN=&MIG.CPT02.TUECADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SWKOPER1 DD DSN=&MIG.CPT02.WKOPER1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TUECADH
//*----------------------------------------------------------------
//SORT0201 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STUECADH SORT TABUECAD-ID-TECH-UR-ADHR ASC [21:20—
//* STUECADH SORT TABUECAD-NO-ORD-ADHS ASC [41:3—
//* STUECADH SORT TABUECAD-NO-ORD-UEC ASC [68:5—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&REF.TUECADH
//SORTOUT  DD DSN=&MIG.CPT02.TUECADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,20,CH,A,
               41,3,CH,A,
               65,5,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER WKOPER1
//*----------------------------------------------------------------
//SORT0202 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SWKOPER1 SORT WKOPER1-ID-TECH-UR-ADHR ASC [1:20—
//* SWKOPER1 SORT WKOPER1-NO-ORD-ADHS ASC [21:3—
//* SWKOPER1 SORT WKOPER1-NO-ORD-UEC ASC [24:5—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CPT01.WKOPER1
//SORTOUT  DD DSN=&MIG.CPT02.WKOPER1,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               24,5,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0202       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SCUE     DD DSN=&MIG.CPT02.CUE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0203       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCPT02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCPT02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCPT02,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTUECADH DD DSN=&MIG.CPT02.TUECADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZWKOPER1 DD DSN=&MIG.CPT02.WKOPER1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCUE     DD DSN=&MIG.CPT02.CUE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCPT02
//*----------------------------------------------------------------
//PVHCPT02 EXEC PGM=PVHCPT02,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCPT02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCPT02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCPT02,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STUECADH DD DISP=SHR,DSN=&MIG.CPT02.TUECADH
//SWKOPER1 DD DISP=SHR,DSN=&MIG.CPT02.WKOPER1
//*--------------<FICHIERS CIBLES>---------------------------------
//SCUE     DD DSN=&MIG.CPT02.CUE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTUECADH DD DSN=&MIG.CPT02.TUECADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZWKOPER1 DD DSN=&MIG.CPT02.WKOPER1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCUE     DD DSN=&MIG.CPT02.CUE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCPT03
//*----------------------------------------------------------------
//DEL0301       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SB05OPER DD DSN=&MIG.CPT03.B05OPER,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB05INFO DD DSN=&MIG.CPT03.B05INFO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER B05OPER
//*----------------------------------------------------------------
//SORT0301 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05OPER SORT B05-OPER-COMPTE-C-OPER ASC [2:12—
//* FIN   CRITERE XGEN
//SORTWK01 DD    UNIT=SYSDA,SPACE=(CYL,(1000,1000),RLSE)
//SORTWK02 DD    UNIT=SYSDA,SPACE=(CYL,(1000,1000),RLSE)
//SORTWK03 DD    UNIT=SYSDA,SPACE=(CYL,(1000,1000),RLSE)
//SORTWK04 DD    UNIT=SYSDA,SPACE=(CYL,(1000,1000),RLSE)
//SORTWK05 DD    UNIT=SYSDA,SPACE=(CYL,(1000,1000),RLSE)
//SORTWK06 DD    UNIT=SYSDA,SPACE=(CYL,(1000,1000),RLSE)
//SORTWK07 DD    UNIT=SYSDA,SPACE=(CYL,(1000,1000),RLSE)
//SORTWK08 DD    UNIT=SYSDA,SPACE=(CYL,(1000,1000),RLSE)
//SORTWK09 DD    UNIT=SYSDA,SPACE=(CYL,(1000,1000),RLSE)
//SORTWK10 DD    UNIT=SYSDA,SPACE=(CYL,(1000,1000),RLSE)
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.B05OPER
//SORTOUT  DD DSN=&MIG.CPT03.B05OPER,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(2,12,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B05INFO
//*----------------------------------------------------------------
//SORT0302 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05INFO SORT B05-INFOS-DEBIT-C-OPER ASC [2:12—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.B05INFOD
//SORTOUT  DD DSN=&MIG.CPT03.B05INFO,
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
//SWKOPER2 DD DSN=&MIG.CPT03.WKOPER2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0303       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCPT03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCPT03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCPT03,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZB05OPER DD DSN=&MIG.CPT03.B05OPER.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZB05INFO DD DSN=&MIG.CPT03.B05INFO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZWKOPER2 DD DSN=&MIG.CPT03.WKOPER2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCPT03
//*----------------------------------------------------------------
//PVHCPT03 EXEC PGM=PVHCPT03,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCPT03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCPT03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCPT03,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SB05OPER DD DISP=SHR,DSN=&MIG.CPT03.B05OPER
//SB05INFO DD DISP=SHR,DSN=&MIG.CPT03.B05INFO
//*--------------<FICHIERS CIBLES>---------------------------------
//SWKOPER2 DD DSN=&MIG.CPT03.WKOPER2,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZB05OPER DD DSN=&MIG.CPT03.B05OPER.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZB05INFO DD DSN=&MIG.CPT03.B05INFO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZWKOPER2 DD DSN=&MIG.CPT03.WKOPER2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCPT04
//*----------------------------------------------------------------
//DEL0401       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STCNTCTR DD DSN=&MIG.CPT04.TCNTCTR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SWKOPER2 DD DSN=&MIG.CPT04.WKOPER2,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SWKOPERB DD DSN=&MIG.CPT04.WKOPERB,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TCNTCTR
//*----------------------------------------------------------------
//SORT0401 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STCNTCTR SORT CNTCTR-C-GRPT-GEST ASC [117:1—
//* STCNTCTR SORT CNTCTR-C-ADH ASC [118:9—
//* STCNTCTR SORT CNTCTR-C-ORDRE ASC [127:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&REF.TCNTCTR
//SORTOUT  DD DSN=&MIG.CPT04.TCNTCTR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(117,1,CH,A,
               118,9,CH,A,
               127,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER WKOPER2
//*----------------------------------------------------------------
//SORT0403 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SWKOPER2 SORT WKOPER2-C-GRPT-GEST ASC [1:1—
//* SWKOPER2 SORT WKOPER2-C-ADH ASC [2:9—
//* SWKOPER2 SORT WKOPER2-C-ORDRE ASC [11:4—
//* SWKOPER2 SORT WKOPER2-D-CREATION DESC [37:8—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CPT03.WKOPER2
//SORTOUT  DD DSN=&MIG.CPT04.WKOPER2,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,1,CH,A,
               2,9,CH,A,
               11,4,CH,A,
               37,8,CH,D)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER WKOPERB
//*----------------------------------------------------------------
//SORT0404 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SWKOPERB SORT WKOPERB-C-GRPT-GEST ASC [1:1—
//* SWKOPERB SORT WKOPERB-C-ADH ASC [2:9—
//* SWKOPERB SORT WKOPERB-C-ORDRE ASC [11:4—
//* SWKOPERB SORT WKOPERB-C-EXE-AFFECT DESC [29:4—
//* SWKOPERB SORT WKOPERB-C-PER-AFFECT DESC [33:2—
//* SWKOPERB SORT WKOPERB-C-TYPE-PER DESC [35:1—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CPT03.WKOPER2
//SORTOUT  DD DSN=&MIG.CPT04.WKOPERB,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,1,CH,A,
               2,9,CH,A,
               11,4,CH,A,
               29,4,CH,D,
               33,2,CH,D,
               35,1,CH,D)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0402       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SWKECRI1 DD DSN=&MIG.CPT04.WKECRI1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0403       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCPT04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCPT04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCPT04,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTCNTCTR DD DSN=&MIG.CPT04.TCNTCTR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZWKOPER2 DD DSN=&MIG.CPT04.WKOPER2.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZWKECRI1 DD DSN=&MIG.CPT04.WKECRI1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZWKOPERB DD DSN=&MIG.CPT04.WKOPERB.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCPT04
//*----------------------------------------------------------------
//PVHCPT04 EXEC PGM=PVHCPT04,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCPT04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCPT04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCPT04,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STCNTCTR DD DISP=SHR,DSN=&MIG.CPT04.TCNTCTR
//SB05ECRI DD DISP=SHR,DSN=&TRI.B05ECRI1
//SWKOPER2 DD DISP=SHR,DSN=&MIG.CPT04.WKOPER2
//SWKOPERB DD DISP=SHR,DSN=&MIG.CPT04.WKOPERB
//*--------------<FICHIERS CIBLES>---------------------------------
//SWKECRI1 DD DSN=&MIG.CPT04.WKECRI1,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTCNTCTR DD DSN=&MIG.CPT04.TCNTCTR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZWKOPER2 DD DSN=&MIG.CPT04.WKOPER2.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZWKECRI1 DD DSN=&MIG.CPT04.WKECRI1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZWKOPERB DD DSN=&MIG.CPT04.WKOPERB.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCPT05
//*----------------------------------------------------------------
//DEL0501       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STCNTCTR DD DSN=&MIG.CPT05.TCNTCTR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCUE     DD DSN=&MIG.CPT05.CUE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SWKECRI1 DD DSN=&MIG.CPT05.WKECRI1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TCNTCTR
//*----------------------------------------------------------------
//SORT0501 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STCNTCTR SORT CNTCTR-ID-TECH-UR-ADHR ASC [21:20—
//* STCNTCTR SORT CNTCTR-NO-ORD-ADHS ASC [41:3—
//* STCNTCTR SORT CNTCTR-C-TYPE-PROD ASC [185:1—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&REF.TCNTCTR
//SORTOUT  DD DSN=&MIG.CPT05.TCNTCTR,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,20,CH,A,
               41,3,CH,A,
               185,1,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER CUE
//*----------------------------------------------------------------
//SORT0502 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCUE SORT CUE-IDTECADH ASC [16:20—
//* SCUE SORT CUE-NOORDADS ASC [36:3—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CPT02.CUE
//SORTOUT  DD DSN=&MIG.CPT05.CUE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,20,CH,A,
               36,3,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER WKECRI1
//*----------------------------------------------------------------
//SORT0503 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SWKECRI1 SORT WKECRI1-ID-TECH-UR-ADHR ASC [1:20—
//* SWKECRI1 SORT WKECRI1-NO-ORD-ADHS ASC [21:3—
//* SWKECRI1 SORT WKECRI1-C-TYPE-PROD ASC [24:1—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CPT04.WKECRI1
//SORTOUT  DD DSN=&MIG.CPT05.WKECRI1,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               24,1,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0502       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SSCT     DD DSN=&MIG.CPT05.SCT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0503       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCPT05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCPT05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCPT05,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTCNTCTR DD DSN=&MIG.CPT05.TCNTCTR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCUE     DD DSN=&MIG.CPT05.CUE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZWKECRI1 DD DSN=&MIG.CPT05.WKECRI1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZSCT     DD DSN=&MIG.CPT05.SCT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCPT05
//*----------------------------------------------------------------
//PVHCPT05 EXEC PGM=PVHCPT05,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCPT05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCPT05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCPT05,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STCNTCTR DD DISP=SHR,DSN=&MIG.CPT05.TCNTCTR
//SCUE     DD DISP=SHR,DSN=&MIG.CPT05.CUE
//SWKECRI1 DD DISP=SHR,DSN=&MIG.CPT05.WKECRI1
//*--------------<FICHIERS CIBLES>---------------------------------
//SSCT     DD DSN=&MIG.CPT05.SCT,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTCNTCTR DD DSN=&MIG.CPT05.TCNTCTR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCUE     DD DSN=&MIG.CPT05.CUE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZWKECRI1 DD DSN=&MIG.CPT05.WKECRI1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZSCT     DD DSN=&MIG.CPT05.SCT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCPT06
//*----------------------------------------------------------------
//DEL0601       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//STUECADH DD DSN=&MIG.CPT06.TUECADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB05SYNT DD DSN=&MIG.CPT06.B05SYNT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SB05OPER DD DSN=&MIG.CPT06.B05OPER,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER TUECADH
//*----------------------------------------------------------------
//SORT0601 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STUECADH SORT TABUECAD-C-GRPT-GEST ASC [101:1—
//* STUECADH SORT TABUECAD-C-ADH ASC [102:9—
//* STUECADH SORT TABUECAD-C-ORDRE ASC [111:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&REF.TUECADH
//SORTOUT  DD DSN=&MIG.CPT06.TUECADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(101,1,CH,A,
               102,9,CH,A,
               111,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B05SYNT
//*----------------------------------------------------------------
//SORT0602 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05SYNT SORT B05-SYNTHESE-CPTE-C-GRPT-GEST ASC [1:1—
//* SB05SYNT SORT B05-SYNTHESE-CPTE-C-ADH ASC [3:9—
//* SB05SYNT SORT B05-SYNTHESE-CPTE-C-ORDRE ASC [13:4—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.B05SYNTH
//SORTOUT  DD DSN=&MIG.CPT06.B05SYNT,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,1,CH,A,
               3,9,CH,A,
               13,4,CH,A,
               18,4,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER B05OPER
//*----------------------------------------------------------------
//SORT0603 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SB05OPER SORT B05-OPER-COMPTE-C-GRPT-GEST ASC [41:1—
//* SB05OPER SORT B05-OPER-COMPTE-C-ADH ASC [42:10—
//* SB05OPER SORT B05-OPER-COMPTE-C-ORDRE ASC [52:5—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&SRC.B05OPER
//SORTOUT  DD DSN=&MIG.CPT06.B05OPER,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(41,1,CH,A,
               42,10,CH,A,
               52,5,CH,A,
               87,4,CH,A)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0602       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SWKSYNT1 DD DSN=&MIG.CPT06.WKSYNT1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0603       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCPT06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCPT06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCPT06,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTUECADH DD DSN=&MIG.CPT06.TUECADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZB05SYNT DD DSN=&MIG.CPT06.B05SYNT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZB05OPER DD DSN=&MIG.CPT06.B05OPER.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZWKSYNT1 DD DSN=&MIG.CPT06.WKSYNT1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCPT06
//*----------------------------------------------------------------
//PVHCPT06 EXEC PGM=PVHCPT06,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCPT06,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCPT06,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCPT06,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//STUECADH DD DISP=SHR,DSN=&MIG.CPT06.TUECADH
//SB05SYNT DD DISP=SHR,DSN=&MIG.CPT06.B05SYNT
//SB05OPER DD DISP=SHR,DSN=&MIG.CPT06.B05OPER
//*--------------<FICHIERS CIBLES>---------------------------------
//SWKSYNT1 DD DSN=&MIG.CPT06.WKSYNT1,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZTUECADH DD DSN=&MIG.CPT06.TUECADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZB05SYNT DD DSN=&MIG.CPT06.B05SYNT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZB05OPER DD DSN=&MIG.CPT06.B05OPER.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZWKSYNT1 DD DSN=&MIG.CPT06.WKSYNT1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCPT07
//*----------------------------------------------------------------
//DEL0701       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SCUE     DD DSN=&MIG.CPT07.CUE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//STUECADH DD DSN=&MIG.CPT07.TUECADH,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SWKSYNT1 DD DSN=&MIG.CPT07.WKSYNT1,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*TRI DU FICHIER CUE
//*----------------------------------------------------------------
//SORT0701 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SCUE SORT CUE-IDTECADH ASC [16:20—
//* SCUE SORT CUE-NOORDADS ASC [36:3—
//* SCUE SORT CUE-NOORDUEC ASC [39:5—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CPT02.CUE
//SORTOUT  DD DSN=&MIG.CPT07.CUE,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,20,CH,A,
               36,3,CH,A,
               39,5,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER TUECADH
//*----------------------------------------------------------------
//SORT0702 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* STUECADH SORT TABUECAD-ID-TECH-UR-ADHR ASC [21:20—
//* STUECADH SORT TABUECAD-NO-ORD-ADHS ASC [41:3—
//* STUECADH SORT TABUECAD-NO-ORD-UEC ASC [68:5—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&REF.TUECADH
//SORTOUT  DD DSN=&MIG.CPT07.TUECADH,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(21,20,CH,A,
               41,3,CH,A,
               68,5,CH,A)
/*
//*----------------------------------------------------------------
//*TRI DU FICHIER WKSYNT1
//*----------------------------------------------------------------
//SORT0703 EXEC PGM=SORT,COND=(0,NE)
//* DEBUT CRITERE XGEN
//* SWKSYNT1 SORT CUE-IDTECADH ASC [1:20—
//* SWKSYNT1 SORT CUE-NOORDADS ASC [21:3—
//* SWKSYNT1 SORT CUE-IDTECADH ASC [24:5—
//* SWKSYNT1 SORT WKSYNT1-C-EXE-AFFECT  57:4—
//* SWKSYNT1 SORT CUE-D-CREATION DESC [61:8—
//* FIN   CRITERE XGEN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DISP=SHR,DSN=&MIG.CPT06.WKSYNT1
//SORTOUT  DD DSN=&MIG.CPT07.WKSYNT1,
//            DISP=(NEW,CATLG,DELETE),DCB=*.SORTIN,
//            SPACE=(TRK,(5000,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(1,20,CH,A,
               21,3,CH,A,
               24,5,CH,A,
               57,4,CH,A,
               61,8,CH,D)
/*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0702       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SBNO     DD DSN=&MIG.CPT07.BNO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0703       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCPT07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCPT07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCPT07,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCUE     DD DSN=&MIG.CPT07.CUE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZTUECADH DD DSN=&MIG.CPT07.TUECADH.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZWKSYNT1 DD DSN=&MIG.CPT07.WKSYNT1.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZWKSYNTB DD DSN=&MIG.CPT07.WKSYNTB.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZBNO     DD DSN=&MIG.CPT07.BNO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCPT07
//*----------------------------------------------------------------
//PVHCPT07 EXEC PGM=PVHCPT07,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCPT07,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCPT07,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCPT07,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SCUE     DD DISP=SHR,DSN=&MIG.CPT07.CUE
//STUECADH DD DISP=SHR,DSN=&MIG.CPT07.TUECADH
//SWKSYNT1 DD DISP=SHR,DSN=&MIG.CPT07.WKSYNT1
//SWKSYNTB DD DISP=SHR,DSN=&MIG.CPT07.WKSYNT1
//*--------------<FICHIERS CIBLES>---------------------------------
//SBNO     DD DSN=&MIG.CPT07.BNO,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZCUE     DD DSN=&MIG.CPT07.CUE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZTUECADH DD DSN=&MIG.CPT07.TUECADH.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZWKSYNT1 DD DSN=&MIG.CPT07.WKSYNT1.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZWKSYNTB DD DSN=&MIG.CPT07.WKSYNTB.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZBNO     DD DSN=&MIG.CPT07.BNO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TRIES DU PGM PVHCPT08
//*----------------------------------------------------------------
//DEL0801       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL0802       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SCOMSCO  DD DSN=&CIB.COMSCO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCIBCUE  DD DSN=&CIB.CUE,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCIBSCT  DD DSN=&CIB.SCT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SCIBBNO  DD DSN=&CIB.BNO,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL0803       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHCPT08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHCPT08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHCPT08,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCUE     DD DSN=&MIG.CPT08.CUE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZSCT     DD DSN=&MIG.CPT08.SCT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZBNO     DD DSN=&MIG.CPT08.BNO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCOMSCO  DD DSN=&MIG.CPT08.COMSCO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCIBCUE  DD DSN=&MIG.CPT08.CIBCUE.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCIBSCT  DD DSN=&MIG.CPT08.CIBSCT.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZCIBBNO  DD DSN=&MIG.CPT08.CIBBNO.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHCPT08
//*----------------------------------------------------------------
//PVHCPT08 EXEC PGM=PVHCPT08,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHCPT08,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHCPT08,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHCPT08,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//CUE      DD DISP=SHR,DSN=&MIG.CPT02.CUE
//SCT      DD DISP=SHR,DSN=&MIG.CPT05.SCT
//BNO      DD DISP=SHR,DSN=&MIG.CPT07.BNO
//*--------------<FICHIERS CIBLES>---------------------------------
//SCOMSCO  DD DSN=&CIB.COMSCO,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SCIBCUE  DD DSN=&CIB.CUE,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SCIBSCT  DD DSN=&CIB.SCT,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SCIBBNO  DD DSN=&CIB.BNO,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZCUE     DD DSN=&MIG.CPT08.CUE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZSCT     DD DSN=&MIG.CPT08.SCT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZBNO     DD DSN=&MIG.CPT08.BNO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCOMSCO  DD DSN=&MIG.CPT08.COMSCO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCIBCUE  DD DSN=&MIG.CPT08.CIBCUE.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCIBSCT  DD DSN=&MIG.CPT08.CIBSCT.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZCIBBNO  DD DSN=&MIG.CPT08.CIBBNO.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//* FIN DU JCL DE MIGRATION
