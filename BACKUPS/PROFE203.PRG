/* SIMA - CONTABILIDAD PROFESORES

MODULO      : LOGROS
SUBMODULO...: CONSULTAR

**************************************************************************
* TITULO..: CONSULTA DE LOS INDICADORES                                  *
**************************************************************************

AUTOR: Nelson Fern�ndez G�mez       FECHA DE CREACION: MAR 16/2006 MIE A
       Colombia, Bucaramanga        INICIO: 04:00 PM   MAR 16/2006 MIE


OBJETIVOS:

1- Permite consultar los indicadores con las recuperaciones de acuerdo
   a la configuraci�n.

2- Retorna NIL

*------------------------------------------------------------------------*
*                              PROGRAMA                                  *
*------------------------------------------------------------------------*/

FUNCTION Profe_203(aP1,aP2,aP3)

*>>>>DESCRIPCION DE PARAMETROS
/*     aP1                                  // Parametros Generales
       aP2                                  // Parametros Generales
       aP3                                  // Parametros Generales */
*>>>>FIN DESCRIPCION DE PARAMETROS

*>>>>DECLARACION DE VARIABLES
       #INCLUDE "CAMPOS\ARC-SPR.PRG"       // Archivos del Sistema

       LOCAL cSavPan := ''                  // Salvar Pantalla
       LOCAL cSavLin := ''                  // Salvar Linea
       LOCAL lHayErr := .F.                 // .T. Hay Error
     *�Variables generales

       LOCAL nFilSup := 0                   // Columna Superior
       LOCAL nColSup := 0                   // Columna Superior
       LOCAL nInfFil := 0                   // Fila Inferior
       LOCAL nInfCol := 0                   // Columna Inferior

       LOCAL nTotPer := 5                   // Total de Periodos
       LOCAL nNroPer := 0                   // N�mero del periodo
       LOCAL cMsgTxt := ''                  // Mensaje Temporal


       LOCAL GetList := {}                  // Variable del Sistema
     *�Variables espec�ficas 

       LOCAL cCodigoTma := ''               // C�digo de la Materia
       LOCAL lMatSelTma := .F.              // .T. Materia Selectiva
     *�Variables de Campo
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
       LOCAL cCodPro := xPrm(aP1,'cCodPro') // C�digo del Profesor
       LOCAL cPatPro := xPrm(aP1,'cPatPro') // Path del Profesor
       LOCAL cNomPro := xPrm(aP1,'cNomPro') // Nombre del Profesor
       CloseAll()
*>>>>DECLARACION DE PARAMETROS ESPECIFICOS

*>>>>SELECION DE LAS AREAS DE TRABAJO
       IF !lUseMae(lShared,PathUno+'\'+cPatSis,cMaeAct,cAnoSis)               .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FileMat,'MAT',NIL,lShared)                 .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FileNiv+cAnoSis+ExtFile,'NIV',NIL,lShared) .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FConTbl+cMaeACt+ExtFile,'TCO',NIL,lShared) .OR.;
	  !lUseDbf(.T.,cPatSis+'\'+;
		       fSprAno+cAnoUsr+ExtFile,'PRA',NIL,lShared) .OR.;
	  !lUseDbf(.T.,cPatPro+'\'+;
		       FCarPro+cAnoSis+ExtFile,'CAP',NIL,lShared)
	  cError('ABRIENDO ARCHIVOS')
	  CLOSE ALL
	  RETURN NIL
       ENDIF
*>>>>FIN SELECION DE LAS AREAS DE TRABAJO

*>>>>VALIDACION DE CONTENIDOS DE ARCHIVOS
       lHayErr := .T.
       SELECT &cMaeAct
       DO CASE
       CASE RECCOUNT() == 0
	    cError('NO EXISTEN ESTUDIANTS GRABADOS')

       CASE MAT->(RECCOUNT()) == 0
	    cError('NO EXISTEN MATERIAS GRABADAS')

       CASE NIV->(RECCOUNT()) == 0
	    cError('NO EXISTEN NIVELES GRABADOS')

       CASE TCO->(RECCOUNT()) == 0
	    cError('NO EXISTEN CONFIGURACION PARA EL NIVEL DE LA CONTABILIDAD')

       CASE PRA->(RECCOUNT()) == 0
	    cError('NO EXISTE CONFIGURACION DEL A�O')

       CASE CAP->(RECCOUNT()) == 0
	    cError('NO EXISTE CARGA ACADEMICA DEL PROFESOR')
       OTHERWISE
	    lHayErr :=.F.
       ENDCASE
       IF lHayErr
	  CLOSE ALL
	  RETURN NIL
       ENDIF
*>>>>FIN VALIDACION DE CONTENIDOS DE ARCHIVOS

*>>>>SELECCION DE LA MATERIA DEL GRUPO
       SELECT CAP
       IF CAP->(RECCOUNT()) # 0

**********INICIALIZACION DE LAS COORDENADAS
	    nFilSup := nFilInf+2
	    nColSup := nColInf
	    IF nFilSup+RECCOUNT() > 22
	       nInfFil := 21
	    ELSE
	       nInfFil := nFilSup + RECCOUNT()-1
	    ENDIF
	    nInfCol := nColSup+LEN(CAP->cNombreMat)+2
**********FIN INICIALIZACION DE LAS COORDENADAS

**********BROWSE DE SELECCION
	    DO WHILE .T.

*--------------SELECCION DEL REGISTRO
		 SELECT CAP
		 IF nBrowseDbf(nFilSup,02,nInfFil,78,;
			      {||SUBS(CAP->cCodigoCar,5,4)+' '+;
			       Materia()}) == 0
		    CLOSE ALL
		    RETURN NIL
		 ENDIF
		 lLocCodigo('cCodigoMat','MAT',SUBS(CAP->cCodigoCar,1,4))
		 lMatSelTma := MAT->lMatSelMat
*--------------FIN SELECCION DEL REGISTRO

*--------------PREGUNTA DE DECISION
		 cMsgTxt := 'ESCOGIO '+ALLTRIM(CAP->cNombreMat)+' '+;
			    'DEL GRUPO: '+SUBS(CAP->cCodigoCar,5,4)
		 cMsgTxt := SUBS(cMsgTxt,1,50)

		 IF lPregunta(cMsgTxt+' DESEA CONTINUAR? Si No')
		    EXIT
		 ENDIF
*--------------FIN PREGUNTA DE DECISION

	    ENDDO
**********FIN BROWSE DE SELECCION

       ENDIF
       cCodigoTma := SUBS(CAP->cCodigoCar,1,4)

       nNroPer := nLeeNroPer(nFilInf+1,nColInf,nFilPal,nTotPer)
       IF EMPTY(nNroPer)
	  CloseAll()
	  RETURN NIL
       ENDIF
*>>>>FIN SELECCION DE LA MATERIA DEL GRUPO

*>>>>MANTENIMIENTO DEL ARCHIVO
       ManDbfNoP(aP1,aP2,aP3,;
		 cPatPro+'\'+CAP->cCodigoCar+ExtFile,nNroPer,cCodigoTma,;
		 SUBS(TCO->cEvaIndTbl,nNroPer,1),;
		 lMatSelTma,.F.,.F.)
       CloseAll()
       RETURN NIL
*>>>>FIN MANTENIMIENTO DEL ARCHIVO
