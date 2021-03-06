/*                      SIMA - CONTABILIDAD PROFESORES
		     DECLARACION DE ARCHIVOS DEL SISTEMA

**************************************************************************
* NOMBRE DEL PROGRAMA......: ARC-PROF.PRG                                *
* TITULO DEL PROGRAMA......: DECLARACION DE ARCHIVOS DEL SISTEMAS        *
* PROGRAMAS QUE LO INVOCAN.: La mayoria de funciones del sistema         *
* PROGRAMAS QUE INVOCA.....: Ninguno                                     *
**************************************************************************

AUTOR: Nelson Fern�ndez G�mez       FECHA DE CREACION: MAR 08/96 VIE A
       Colombia, Bucaramanga        INICIO: 10:00 AM   MAR 08/96 VIE

REQUERIMIENTOS:

1- Para que el programa funcione se debe invocar utilizando la clausula
   #INCLUDE dentro de la funcion que lo va a utilizar.

OBJETIVOS:

1- Declara todas las variables que contiene la declaraci�n de todos los
   archivos que conforma el sistema.

2- El objetivo principal de esta declaraci�n es estandarizar el modo
   de acceso a los archivos.

*------------------------------------------------------------------------*
*                              PROGRAMA                                  *
*------------------------------------------------------------------------*/

*>>>>DECLARACION DE ARCHIVOS GENERALES
       LOCAL PathWeb := '..\simaweb'        // Path del Sitio

       LOCAL PathSis := 'BASES'             // Path General del sistema
       LOCAL PathTem := '1FILETEM'          // Paht Temporal
       LOCAL ExtFile := '.DAT'              // Extension para las bases
       LOCAL cExtUsb := '.USB'              // Extension para las bases
       LOCAL cExtNtx := '.NEL'              // Extension para indices
       LOCAL lSiRed  := .T.                 // .T. Apertura de Archivos MultiUsuario
       LOCAL lNoRed  := .F.                 // .F. Apertura de Archivo MonoUsuario
       LOCAL aUseDbf := {}                  // Vector para Apertura de Varias Dbfs

       LOCAL PathDoc := 'c:\misdoc'         // Path de Documentos del sima
       LOCAL PathPrn := 'PRINTER'           // Path para impresiones
       LOCAL cNtxTem := '.TEM'              // Extension indice temporal
       LOCAL FileTem := 'TEMPO'+cNtxTem     // Archivo datos temporal
       LOCAL fNtxTem := 'TEMPO'+cExtNtx     // Archivo Indice temporal
     *�Archivo Temporal

       LOCAL PathAaa := ''
       LOCAL FileAaa := PathAaa+'TEMP'+ExtFile
       LOCAL fNtxAaa := ''
     *�Archivo de Usuarios

       LOCAL PathErr := ''
       LOCAL fErrSys := PathErr+'ERRORSYS'+ExtFile
       LOCAL fNtxErr := ''

       LOCAL FileClr := 'CFGCOLOR'+ExtFile
     *�Archivo de Configuraci�n del Color

       LOCAL FilePrn := 'DRIVERS'+ExtFile
     *�Archivo de Configuraci�n de Impresoras

       LOCAL FileDat := 'DATO0103'+ExtFile
     *�Archivo de Datos del Sistema

       LOCAL FileLog := 'LOGPROFE'+ExtFile
     *�Archivo del log del profesor

       LOCAL nHanXml := 0                   // Manejador del Archivo
       LOCAL FileXml := ''                  // Archivo Xml
     *�Archivo Xml

       LOCAL FileInf := 'INFORSYS'+ExtFile
     *�Archivo de informes del sistema

       LOCAL FileIso := 'FILESISO'+ExtFile
     *�Archivo de documentos de Iso
*>>>>FIN DECLARACION DE ARCHIVOS GENERALES

*>>>>DECLARACION DE ARCHIVOS DEL SISTEMA
       LOCAL fSimSpr := 'SIMAPROF'+ExtFile
     *�Archivo de Configuraci�n del sistema

       LOCAL fSprAno := 'PROF'             // Ej: PROF1996.DAT
     *�Archivo Tabla del a�o

       LOCAL PathTbl := ''
       LOCAL fProTbl := PathTbl+'PROF'     // Ej: PROFBTO.DAT
       LOCAL fNtxTbl := ''
     *�Archivo Tabla del nivel

       LOCAL PathInd := ''
       LOCAL FileInd := PathInd+'IND'      // Ej: INDBTO.DAT
       LOCAL fNtxInd := PathInd+'IND'      // Ej: INDBTO.NEL
     *�Archivo de Indicadores de los profesores

       LOCAL PCarPro := ''
       LOCAL FCarPro := PCarPro+'CARPRO'    // Ej: CARPRO95.DAT
       LOCAL fNtxCap := ''
     *�Archivo de carga academica del profesor

       LOCAL PMatPro := ''
       LOCAL FMatPro := PMatPro+'MATPRO'    // Ej: MATPRO95.DAT
       LOCAL fNtxMaP := ''
     *�Archivo de Materias que dicta el profesor

       LOCAL PathPro := ''
       LOCAL FilePro := PathPro+'PROFES'    // Ej: PROFES.DAT o PROFES96.DAT
       LOCAL fNtxPro := ''
     *�Archivo del Profesor para notas y contabilidad

       LOCAL PNotPro := ''
       LOCAL FNotPro := PNotPro+''          //  Ej: 06011601.DAT
       LOCAL fNtxNop := ''
     *�Archivo de notas de un curso
*>>>>FIN DECLARACION DE ARCHIVOS DEL SISTEMA

*>>>>DECLARACION DE ARCHIVOS COMPARTIDOS
       LOCAL fSimaCo := 'SIMACONT'+ExtFile
     *�Archivo de Configuraci�n del sistema

       LOCAL fConTbl := PathTbl+'CONT'             // Ej: CONTBTO.DAT
     *�Archivo Configuraci�n del nivel

       LOCAL fPazEst := 'PAZEST'	           // Ej: PAZEST01.DAT
     *�Archivo de Paz y Salvos

       LOCAL fDpzEst := 'DEUPAZ'	           // Ej: DEUPAZ01.DAT
     *�Archivo de Deudas del Paz y Salvo

       LOCAL FileAlu := 'MAE'
       LOCAL fNtxAlu := 'MAE'
     *�Archivo de Alumnos

       LOCAL FileSin := 'SUB'                      // Ej: SUBBTO08.DAT
     *�Archivo Clasificaci�n de Indicadores

       LOCAL PathBto := ''
       LOCAL FMaeBto := 'MAEBTO'            // Ej: MAEBTO95.DAT
       LOCAL fNtxBto := 'MAEBTO'
       LOCAL Ntx1Bto := 'APEBTO'            // Ej: APEBTO95.NEL
       LOCAL Ntx2Bto := 'NOMBTO'            // Ej: NOMBTO95.NEL
     *�Archivo de Alumnos de Bachillerato

       LOCAL PathPri := ''
       LOCAL FMaePri := 'MAEPRI'            // Ej: MAEPRI94.DAT
       LOCAL fNtxPri := 'MAEPRI'
       LOCAL Ntx1Pri := 'APEPRI'            // Ej: APEPRI95.NEL
       LOCAL Ntx2Pri := 'NOMPRI'            // Ej: NOMPRI95.NEL
     *�Archivo de Alumnos de Primaria

       LOCAL PathJar := ''
       LOCAL FMaeJar := 'MAEJAR'            // Ej: MAEJAR94.DAT
       LOCAL fNtxJar := 'MAEJAR'
       LOCAL Ntx1Jar := 'APEJAR'            // Ej: APEJAR95.NEL
       LOCAL Ntx2Jar := 'NOMJAR'            // Ej: NOMJAR95.NEL
     *�Archivo de Alumnos de Jard�n

       LOCAL PathGua := ''
       LOCAL FMaeGua := 'MAEGUA'            // Ej: MAEGUA94.DAT
       LOCAL fNtxGua := 'MAEGUA'
       LOCAL Ntx1Gua := 'APEGUA'            // Ej: APEGUA95.NEL
       LOCAL Ntx2Gua := 'NOMGUA'            // Ej: NOMGUA95.NEL
     *�Archivo de Alumnos de Guarderia

       LOCAL FileGru := 'GRU'                  // Ej: GRUBTO99.DAT

       LOCAL PathNiv := ''
       LOCAL FileNiv := PathNiv+'NIVELS'    // Ej: NIVELS95.DAT
       LOCAL fNtxNiv := PathNiv+'NIVELS'       // Ej: NIVELS95.NEL
     *�Archivo de Niveles

       LOCAL PathPla := ''
       LOCAL FilePla := PathPla+'PL'       // Ej: PLAN0695.DAT  Por Nivel
       LOCAL fNtxPla := ''                 //     PL060195.DAT  Por Grupo
     *�Archivo de Plan de materias a dictar por nivel

       LOCAL PathMat := ''
       LOCAL FileMat := PathMat+'MATERIAS'+ExtFile
       LOCAL fNtxMat := ''
     *�Archivo de Materias

       LOCAL PathSel := ''
       LOCAL FileSel := PathSel+'SELE'     // Ej: SELEBTO.DAT
       LOCAL fNtxSel := PathSel+'SELE'     // Ej: SELEBTO.NEL
     *�Archivo de Selecci�n de Materias

       LOCAL PathCar := ''
       LOCAL FileCar := PathCar+'CARGA'     // Ej: CARGABTO.DAT
       LOCAL fNtxCar := PathCar+'CARGA'     //     CARGABTO.NEL
     *�Archivo de Cargas

       LOCAL PathCla := ''
       LOCAL FileCla := PathCla+'CLASEIND'+ExtFile
       LOCAL fNtxCla := PathCla+'CLASEIND'+ExtFile
     *�Archivo de Clase de Indicadores

       LOCAL PathNro := ''
       LOCAL FNroInd := PathNro+'NRO'      // Ej: NROBTO96.DAT
       LOCAL fNtxNro := PathNro+'NRO'      // Ej: NROBTO96.NEL
     *�Archivo de Control de los n�mero de Indicadores

       LOCAL PathEva := ''
       LOCAL FileEva := PathEva+'TABLAEVA'+ExtFile
       LOCAL fNtxEva := PathEva+'TABLAEVA'+ExtFile
     *�Archivo Tabla de Evaluaci�n

       LOCAL FileJvf := 'Juiciosv'+ExtFile
     *�Archivo de Juicios Valorativos
*>>>>FIN DECLARACION DE ARCHIVOS COMPARTIDOS