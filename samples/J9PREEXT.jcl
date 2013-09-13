//J9VHEXT  JOB UTI00TX0,'VH GRECCO G05-EXT',CLASS=Z,MSGCLASS=X,
//*        RESTART=DEL5002,
//         MSGLEVEL=(1,1),REGION=0M,NOTIFY=&SYSUID
//*----------------------------------------------------------------
//JCLLIB   JCLLIB ORDER=PLURM.MOVE.VHREF.JCL.PROD
//MEMBER   INCLUDE MEMBER=$$VHT
//*----------------------------------------------------------------
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS EN SORTIE DU PGM
//*----------------------------------------------------------------
//DEL5002       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SEXTSOL  DD DSN=&CIB.EXTSOL,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SRCR     DD DSN=&CIB.RCR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//SAFR     DD DSN=&CIB.AFR,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP DE SUPPRESSION DES FICHIERS TECHNIQUES DU PGM
//*----------------------------------------------------------------
//DEL5003       EXEC PGM=IEFBR14,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//FANO     DD DSN=&ANO.PVHEXT50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FCPT     DD DSN=&CPT.PVHEXT50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//FTRC     DD DSN=&TRC.PVHEXT50,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZERCR    DD DSN=&MIG.EXT50.ERCR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZEAFR    DD DSN=&MIG.EXT50.EAFR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZEXTSOL  DD DSN=&MIG.EXT50.EXTSOL.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZRCR     DD DSN=&MIG.EXT50.RCR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//ZAFR     DD DSN=&MIG.EXT50.AFR.R,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*----------------------------------------------------------------
//*STEP D EXECUTION DU PROGRAMME DE MIGRATION PVHEXT50
//*----------------------------------------------------------------
//PVHEXT50 EXEC PGM=PVHEXT50,COND=(0,NE)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//ANOMAL   DD DSN=&ANO.PVHEXT50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(5000,500),RLSE)
//COMPTEUR DD DSN=&CPT.PVHEXT50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FICTRACE DD DSN=&TRC.PVHEXT50,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(500,50),RLSE)
//FWORMAJ  DD DISP=SHR,DSN=&PARAM
//NSPITAB  DD DISP=SHR,DSN=&SPITAB
//*--------------<FICHIERS SOURCES>--------------------------------
//SERCR    DD DUMMY
//SEAFR    DD DUMMY
//*--------------<FICHIERS CIBLES>---------------------------------
//SEXTSOL  DD DSN=&CIB.EXTSOL,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SRCR     DD DSN=&CIB.RCR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//SAFR     DD DSN=&CIB.AFR,
//            DISP=(NEW,CATLG,CATLG),
//            SPACE=(TRK,(200,100),RLSE)
//ZERCR    DD DSN=&MIG.EXT50.ERCR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZEAFR    DD DSN=&MIG.EXT50.EAFR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZEXTSOL  DD DSN=&MIG.EXT50.EXTSOL.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZRCR     DD DSN=&MIG.EXT50.RCR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//ZAFR     DD DSN=&MIG.EXT50.AFR.R,
//            DISP=(NEW,CATLG,CATLG),
//            DCB=(RECFM=VB,LRECL=12004,BLKSIZE=24000),
//            SPACE=(TRK,(200,100),RLSE)
//*
//* FIN DU JCL DE MIGRATION
