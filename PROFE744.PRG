/* SIMA - CONTABILIDAD PROFESORES

MODULO      : UTILIDADES
SUBMODULO...: ACTUALIZAR ASIGNATURAS

**************************************************************************
* TITULO..: ACTUALIZACION DE LAS ASIGNATURAS                             *
**************************************************************************

AUTOR: Nelson Fern�ndez G�mez       FECHA DE CREACION: OCT 30/2001 MAR A
       Colombia, Bucaramanga        INICIO: 03:15 PM   OCT 30/2001 MAR

OBJETIVOS:

1- Actualiza la Estructura todos los archivos de las materias que
   dicta el profesor en cada curso.

2- Retorna Nil

*------------------------------------------------------------------------*
*                              PROGRAMA                                  *
*------------------------------------------------------------------------*/

FUNCTION Profe_744(aP1,aP2,aP3)

*>>>>DESCRIPCION DE PARAMETROS
/*     aP1                                  // Parametros Generales
       aP2                                  // Parametros Generales
       aP3                                  // Parametros Generales */
*>>>>FIN DESCRIPCION DE PARAMETROS

*>>>>DECLARACION DE VARIABLES
       #INCLUDE "inkey.ch"                  // Declaraci�n de teclas
       #INCLUDE "CAMPOS\ARC-SPR.PRG"       // Archivos del Sistema

       LOCAL cSavPan := ''                  // Salvar Pantalla
       LOCAL lHayErr := .F.                 // .T. Hay Error

       LOCAL lTodos  := .F.                 // .T. Procesar todos

       LOCAL cCodigoTpr := ''               // C�digo del Profesor

       LOCAL GetList := {}                  // Variable del sistema
*>>>>FIN DECLARACION DE VARIABLES

*>>>>DECLARACION PARAMETROS GENERALES
       LOCAL lShared := xPrm(aP1,'lShared') // .T. Sistema Compartido
       LOCAL nModCry := xPrm(aP1,'nModCry') // Modo de Protecci�n
       LOCAL cCodSui := xPrm(aP1,'cCodSui') // C�digo del Sistema
       LOCAL cNomSis := xPrm(aP1,'cNomSis') // Nombre del Sistema
     *�Detalles del Sistema

       LOCAL cEmpPal := xPrm(aP1,'cEmpPal') // Nombre de la Empresa principal
       LOCAL cNitEmp := xPrm(aP1,'cNitEmp') // Nit de la Empresa
       LOCAL cNomEmp := xPrm(aP1,'cNomEmp') // Nombre de la Empresa
       LOCAL cNomSec := xPrm(aP1,'cNomSec') // Nombre de la Empresa Secundario
       LOCAL cCodEmp := xPrm(aP1,'cCodEmp') // C�digo de la Empresa
     *�Detalles de la Empresa

       LOCAL cNomUsr := xPrm(aP1,'cNomUsr') // Nombre del Usuario
       LOCAL cAnoUsr := xPrm(aP1,'cAnoUsr') // A�o del usuario
       LOCAL cAnoSis := xPrm(aP1,'cAnoSis') // A�o del sistema
       LOCAL cPatSis := xPrm(aP1,'cPatSis') // Path del sistema
     *�Detalles del Usuario

       LOCAL PathW01 := xPrm(aP1,'PathW01') // Sitio del Sistema No.01
       LOCAL PathW02 := xPrm(aP1,'PathW02') // Sitio del Sistema No.02
       LOCAL PathW03 := xPrm(aP1,'PathW03') // Sitio del Sistema No.03
       LOCAL PathW04 := xPrm(aP1,'PathW04') // Sitio del Sistema No.04
       LOCAL PathW05 := xPrm(aP1,'PathW05') // Sitio del Sistema No.05
       LOCAL PathW06 := xPrm(aP1,'PathW06') // Sitio del Sistema No.06
       LOCAL PathW07 := xPrm(aP1,'PathW07') // Sitio del Sistema No.07
       LOCAL PathW08 := xPrm(aP1,'PathW08') // Sitio del Sistema No.08
       LOCAL PathW09 := xPrm(aP1,'PathW09') // Sitio del Sistema No.09
       LOCAL PathW10 := xPrm(aP1,'PathW10') // Sitio del Sistema No.10
     *�Sitios del Sistema

       LOCAL PathUno := xPrm(aP1,'PathUno') // Path de Integraci�n Uno
       LOCAL PathDos := xPrm(aP1,'PathDos') // Path de Integraci�n Dos
       LOCAL PathTre := xPrm(aP1,'PathTre') // Path de Integraci�n Tres
       LOCAL PathCua := xPrm(aP1,'PathCua') // Path de Integraci�n Cuatro
     *�Path de Integraci�n

       LOCAL cMaeAlu := xPrm(aP1,'cMaeAlu') // Maestros habilitados
       LOCAL cMaeAct := xPrm(aP1,'cMaeAct') // Maestro Activo
       LOCAL cJorTxt := xPrm(aP1,'cJorTxt') // Jornada escogida
     *�Detalles Acad�micos

       LOCAL nFilPal := xPrm(aP1,'nFilPal') // Fila Inferior Men� principal
       LOCAL nFilInf := xPrm(aP1,'nFilInf') // Fila Inferior del SubMen�
       LOCAL nColInf := xPrm(aP1,'nColInf') // Columna Inferior del SubMen�
     *�Detalles Tecnicos
*>>>>FIN DECLARACION PARAMETROS GENERALES

*>>>>DECLARACION DE PARAMETROS ESPECIFICOS
       LOCAL lMemUsb := xPrm(aP1,'lMemUsb') // .T. SimaProf en Usb. .F. SimaProf en la Red
       LOCAL cCodPro := xPrm(aP1,'cCodPro') // C�digo del Profesor
       LOCAL cPatPro := xPrm(aP1,'cPatPro') // Path del Profesor
       LOCAL cNomPro := xPrm(aP1,'cNomPro') // Nombre del Profesor
       CloseAll()
*>>>>DECLARACION DE PARAMETROS ESPECIFICOS

*>>>>SELECCION DEL NIVEL
       cMaeAct := cNivelEst(nFilInf+1,nColInf,cMaeAlu)
       nParameSet(aP1,'cMaeAct',cMaeAct)
*>>>>FIN SELECCION DEL NIVEL

*>>>>LECTURA DEL USUARIO PROFESOR
       cSavPan := SAVESCREEN(0,0,24,79)
       cPatPro := cUsrPro(lShared,cNomSis,cPatSis,cNomUsr,cAnoUsr,cMaeAct,;
			  lMemUsb,@cCodigoTpr,@cNomPro,.F.)
       RESTSCREEN(0,0,24,79,cSavPan)

       lTodos := .F.
       IF EMPTY(cPatPro)
	  lTodos := lPregunta('DESEA ACTUALIZAR TODOS LOS PROFESORES? No Si')
	  IF !lTodos
	     CLOSE ALL
	     RETURN NIL
	  ENDIF
       ENDIF
       IF !lPregunta('DESEA CONTINUAR? Si No')
	  CLOSE ALL
	  RETURN NIL
       ENDIF
       CloseDbf('TCO')
*>>>>FIN LECTURA DEL USUARIO PROFESOR

*>>>>ACTUALIZACION DEL REGISTRO
       SELECT PRO
       IF lTodos
	  PRO->(DBGOTOP())
       ENDIF
*>>>>FIN ACTUALIZACION DEL REGISTRO

*>>>>ACTUALIZACION DE LAS ESTRUCTURAS
       DO WHILE .T.

**********IMPRESION DE LA LINEA DE ESTADO
	    LineaEstado('CODIGO:'+PRO->cCodigoPro+;
			'�No. '+STR(PRO->(RECNO()),4)+'/'+;
				STR(PRO->(RECCOUNT()),4),cNomSis)
**********FIN IMPRESION DE LA LINEA DE ESTADO

**********LECTURA DEL PATH DEL PROFESOR
	    IF lTodos
	       cPatPro := cPatSis+'\'+cMaeAct+'\'+PRO->cCodigoPro
	    ENDIF
**********FIN LECTURA DEL PATH DEL PROFESOR

**********VALIDACION DE EXISTENCIA
	    IF !FILE(cPatPro+'\'+FCarPro+cAnoSis+ExtFile)
	       PRO->(DBSKIP())
	       IF PRO->(EOF())
		  EXIT
	       ENDIF
	       LOOP
	    ENDIF
**********FIN VALIDACION DE EXISTENCIA

**********SELECION DE LAS AREAS DE TRABAJO
	    IF !lUseDbf(.T.,cPatPro+'\'+;
			    FCarPro+cAnoSis+ExtFile,'CAP',NIL,lShared)
	       cError('ABRIENDO ARCHIVO CARGAS DEL PROFESOR')
	       CLOSE ALL
	       RETURN .F.
	    ENDIF
**********FIN SELECION DE LAS AREAS DE TRABAJO

**********VALIDACION DE CONTENIDOS DE ARCHIVOS
	    lHayErr := .T.
	    DO CASE
	    CASE CAP->(RECCOUNT()) == 0
		 cError('NO EXISTE CARGA ACADEMICA DEL PROFESOR '+;
			'CODIGO: '+PRO->cCodigoPro)
	    OTHERWISE
		 lHayErr :=.F.
	    ENDCASE
	    IF lHayErr
	       CloseDbf('CAP')
	       PRO->(DBSKIP())
	       IF PRO->(EOF())
		  EXIT
	       ENDIF
	       LOOP
	    ENDIF
**********FIN VALIDACION DE CONTENIDOS DE ARCHIVOS

**********ACTUALIZACION DE LOS ARCHIVOS DE LA CARGA ACADEMICA
	    SELECT CAP
	    GO TOP
	    DO WHILE .NOT. CAP->(EOF())

*--------------IMPRESION DE LA LINEA DE ESTADO
		 LineaEstado('CODIGO:'+PRO->cCodigoPro+;
			     '�No. '+STR(PRO->(RECNO()),4)+'/'+;
				     STR(PRO->(RECCOUNT()),4)+;
			     '�ARCHIVO:'+CAP->cCodigoCar+ExtFile+;
			     '�No. '+STR(CAP->(RECNO()),4)+'/'+;
				     STR(CAP->(RECCOUNT()),4),cNomSis)
*--------------FIN IMPRESION DE LA LINEA DE ESTADO

*--------------ACTUALIZACION DE LA ESTRUCTURA
		 IF FILE(cPatPro+'\'+CAP->cCodigoCar+ExtFile)
		    nActStrNoP(lShared,0,cPatPro,CAP->cCodigoCar+ExtFile)
		 ENDIF
*--------------FIN ACTUALIZACION DE LA ESTRUCTURA

	       CAP->(DBSKIP())

	    ENDDO
	    CloseDbf('CAP')
**********FIN ACTUALIZACION DE LOS ARCHIVOS DE LA CARGA ACADEMICA

**********ANALISIS DE DECISION
	    IF !lTodos
	       EXIT
	    ENDIF
**********FIN ANALISIS DE DECISION

**********AVANCE DEL REGISTRO
	    PRO->(DBSKIP())
	    IF PRO->(EOF())
	       EXIT
	    ENDIF
**********AVANCE DEL REGISTRO

       ENDDO
       CLOSE ALL
       RETURN NIL
*>>>>FIN ACTUALIZACION DE LAS ESTRUCTURAS