/* SIMA - CONTABILIDAD PROFESORES

MODULO      : INDICADORES
SUBMODULO...: CONSULTAR

**************************************************************************
* TITULO..: CONSULTA DE INDICADORES                                      *
**************************************************************************

AUTOR: Nelson Fern�ndez G�mez       FECHA DE CREACION: MAR 20/1996 JUE A
       Colombia, Bucaramanga        INICIO: 12:35 PM   MAR 20/1996 JUE

OBJETIVOS:

1- Permite permite modificar los indicadores grabados

2- Retorna NIL

*------------------------------------------------------------------------*
*                              PROGRAMA                                  *
*------------------------------------------------------------------------*/

FUNCTION Profe_502(aP1,aP2,aP3)

*>>>>DESCRIPCION DE PARAMETROS
/*     aP1                                  // Parametros Generales
       aP2                                  // Parametros Generales
       aP3                                  // Parametros Generales */
*>>>>FIN DESCRIPCION DE PARAMETROS

*>>>>DECLARACION DE VARIABLES
       #INCLUDE "inkey.ch"                  // Declaraci�n de teclas
       #INCLUDE "CAMPOS\ARC-SPR.PRG"       // Archivos del Sistema

       LOCAL cSavPan := ''                  // Salvar Pantalla
       LOCAL lHayErr    := .F.              // .T. Hay Error
       LOCAL oBrwInd := NIL                 // Browse de Indicadores

       LOCAL nNroOpc := 1                   // Numero de la opcion
       LOCAL nLenOpc := 0                   // Longitud mayor de los opciones
       LOCAL aMenus  := {}                  // Vector de declaracion de men�
       LOCAL aAyuda  := {}                  // Vector de ayudas para el men�
       LOCAL cClaInd := ''                  // Clase del Indicador

       LOCAL cSavLin := ''                  // Salvar Linea
       LOCAL aTitulo := {}                  // Titulos de las Columnas
       LOCAL aTamCol := {}                  // Tama�o de las Columnas
       LOCAL aNroCol := {}                  // N�meros de Columnas

       LOCAL nNroFil := 0                   // Fila de lectura
       LOCAL nNroCol := 1                   // Columna de lectura
       LOCAL nFilSup := 0                   // Columna Superior
       LOCAL nColSup := 0                   // Columna Superior
       LOCAL nInfFil := 0                   // Fila Inferior
       LOCAL nInfCol := 0                   // Columna Inferior
       LOCAL cMsgTxt := ''                  // Mensaje Temporal
       LOCAL cSecInd := 0                   // Secuencial del Indicador

       LOCAL GetList := {}                  // Variable del Sistema

       LOCAL cCodigoTni := ''               // C�digo del Nivel
       LOCAL cCodigoTcl := ''               // C�digo de la clase
       LOCAL cCodigoTma := ''               // C�digo de la materia
       LOCAL cNombreTma := ''               // Nombre de la materia
       LOCAL cCodigoTju := ''               // C�digo del Indicador
       LOCAL cNombreTju := ''               // Nombre del Indicador

       FIELD cCodigoMat                     // C�digo de la materia
       FIELD cCodigoCla                     // C�digo de la clase
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

*>>>>SELECCION DE LAS AREAS DE TRABAJO
       IF !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FConTbl+cMaeACt+ExtFile,'TCO',NIL,lShared) .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
		       FileCla,'CLA',NIL,lShared)             .OR.;
	  !lUseDbf(.T.,PathUno+'\'+cPatSis+'\'+cMaeAct+'\'+;
			FileMat,'MAT',NIL,lShared) .OR.;
	  !lUseDbf(.T.,cPatSis+'\'+cMaeAct+'\'+;
		       FileInd+cMaeAct+ExtFile,'IND',;
		       cPatSis+'\'+cMaeAct+'\'+;
		       FNtxInd+cMaeAct+cExtNtx,lShared) .OR.;
	  !lUseDbf(.T.,cPatPro+'\'+;
		       FMatPro+cAnoSis+ExtFile,'MAP',NIL,lShared)
	  cError('ABRIENDO ARCHIVOS')
	  CLOSE ALL
	  RETURN NIL
       ENDIF
*>>>>FIN SELECCION DE LAS AREAS DE TRABAJO

*>>>>VALIDACION DE CONTENIDOS DE ARCHIVOS
       lHayErr := .T.
       DO CASE
       CASE TCO->(RECCOUNT()) == 0
	    cError('NO EXISTEN CONFIGURACION PARA EL NIVEL DE LA CONTABILIDAD')

       CASE CLA->(RECCOUNT()) == 0
	    cError('NO SE HAN DEFINIDO LAS CLASES DE INDICADORES')

       CASE MAP->(RECCOUNT()) == 0
	    cError('NO EXISTEN MATERIAS DEL PROFESOR GRABADAS')

       CASE MAT->(RECCOUNT()) == 0
	    cError('NO EXISTEN MATERIAS GRABADAS')
       OTHERWISE
	    lHayErr :=.F.
       ENDCASE
       IF lHayErr
	  CLOSE ALL
	  RETURN NIL
       ENDIF
       lHayErr := .F.
*>>>>FIN VALIDACION DE CONTENIDOS DE ARCHIVOS

*>>>>SELECCION DE LA MATERIA
       SELECT MAP
       IF MAP->(RECCOUNT()) # 1

**********INICIALIZACION DE LAS COORDENADAS
	    nFilSup := nFilInf+2
	    nColSup := nColInf
	    IF nFilSup+RECCOUNT() > 22
	       nInfFil := 21
	    ELSE
	       nInfFil := nFilSup + RECCOUNT()-1
	    ENDIF
	    nInfCol := nColSup+LEN(MAP->cNombreMat)+2
**********FIN INICIALIZACION DE LAS COORDENADAS

**********BROWSE DE SELECCION
	    DO WHILE .T.
	       IF nBrowseDbf(nFilSup,02,nInfFil,78,;
			     {||MAP->cCodigoNiv+' '+MAP->cNombreMat}) == 0
		  CLOSE ALL
		  RETURN NIL
	       ENDIF
	       cMsgTxt := 'ESCOGIO '+ALLTRIM(MAP->cNombreMat)+;
			  IF(TCO->lIndNivTbl,' DEL NIVEL '+MAP->cCodigoNiv,'')
	       cMsgTxt := SUBS(cMsgTxt,1,50)

	       IF lPregunta(cMsgTxt+' DESEA CONTINUAR? Si No')
		  EXIT
	       ENDIF
	    ENDDO
**********FIN BROWSE DE SELECCION

       ENDIF
       cCodigoTni := MAP->cCodigoNiv
       cCodigoTma := MAP->cCodigoMat
*>>>>FIN SELECCION DE LA MATERIA

*>>>>VALIDACION DEL LIMITE DE LA ULTIMA COLUMNA
       nLenOpc := LEN(CLA->cNombreCla)
      *Calculo de la Longitud mayor

       IF nColInf + nLenOpc + 2 > 78
	  nColInf := 78-nLenOpc-2
       ENDIF
*>>>>FIN VALIDACION DEL LIMITE DE LA ULTIMA COLUMNA

*>>>>SELECCION DE LA CLASE DE INDICADOR
      SELECT CLA
      DO WHILE .T.
	 IF nSelRegDbf(nFilInf+1,nColInf-1,'CLA','cNombreCla') == 0
	    CLOSE ALL
	    RETURN NIL
	 ENDIF
	 IF lPregunta('ESCOGIO '+ALLTRIM(CLA->cNombreCla)+;
	    ' DESEA CONTINUAR? Si No')
	    EXIT
	 ENDIF
      ENDDO
      cCodigoTcl := CLA->cCodigoCla
*>>>>FIN SELECCION DE LA CLASE DE INDICADOR

*>>>>SELECION DE LA MATERIA O AREA A GRABAR
       IF EMPTY(CLA->cCodigoMat)
	  IF UPPER(CLA->cCodigoCla) == 'A'
	     cCodigoTma := SUBS(MAP->cCodigoMat,1,2)+'00'
	  ELSE
	     cCodigoTma := MAP->cCodigoMat
	  ENDIF
       ELSE
	  cCodigoTma := CLA->cCodigoMat
	  cCodigoTni := SPACE(02)
       ENDIF
*>>>>FIN SELECION DE LA MATERIA O AREA A GRABAR

*>>>>LOCALIZACION DE LA MATERIA GENERAL
       IF !lLocCodigo('cCodigoMat','MAT',cCodigoTma)
	  cError('NO SE ENCUENTRA EL CODIGO '+cCodigoTma+' '+;
		 'PARA GRABACION GENERAL')
	  CLOSE ALL
	  RETURN NIL
       ENDIF
*>>>>FIN LOCALIZACION DE LA MATERIA GENERAL

*>>>>FILTRACION DE LOS INDICADORES
       SELECT IND
       cCodigoTju := cCodigoTma+cCodigoTcl
       IF TCO->lIndNivTbl
	  SET FILTER TO SUBS(IND->cCodigoJui,1,5) == cCodigoTju .AND.;
			IND->cCodigoNiv == cCodigoTni
       ELSE
	  SET FILTER TO SUBS(IND->cCodigoJui,1,5) == cCodigoTju
       ENDIF
       GO TOP
       IF EOF()
	  cError('NO HAY INDICADORES '+;
		 IF(TCO->lIndNivTbl,'DE NIVEL','DE MATERIA')+' '+;
		 'PARCIALES PARA '+ALLTRIM(MAT->cNombreMat)+' '+;
		 'DE '+ALLTRIM(CLA->cNombreCla))
	  CLOSE ALL
	  RETURN NIL
       ENDIF
*>>>>FIN FILTRACION DE LOS INDICADORES

*>>>>VALIDACION DEL NOMBRE DE LA MATERIA
       IF EMPTY(CLA->cCodigoMat)
	  cNombreTma := IF(TCO->lIndNivTbl,;
			  'NIVEL '+cCodigoTni,'')+' '+;
			  ALLTRIM(MAT->cNombreMat)
	  cNombreTma := SUBS(cNombreTma,1,70)
       ELSE
	  cNombreTma := ''
       ENDIF
       LineaEstado(cNombreTma,cNomSis)
*>>>>FIN VALIDACION DEL NOMBRE DE LA MATERIA

*>>>>BROWSE DE LOS INDICADORES
       oBrwInd := oDefBrwInd(lShared,nFilPal+1,00,22,79,;
			     '<< CLASE DE INDICADOR: '+;
			     ALLTRIM(CLA->cNombreCla)+" >>",;
			     " <F5>BUSCAR <ESC>REGRESAR",'')

       SETKEY(K_F5,{||BuscarInd(oBrwInd,cCodigoTju,;
		      IF(TCO->lIndNivTbl,cCodigoTni,''))})
       IND->(CtrlBrw(lShared,oBrwInd))
       SETKEY(K_F5,NIL)
       SET FILTER TO
       CLOSE ALL
       RETURN NIL
*>>>>FIN BROWSE DE LOS INDICADORES

/*************************************************************************
* TITULO DE LA FUNCION..: DEFINICION DEL OBJETO BROWSE DE INDICADORES    *
**************************************************************************

AUTOR: Nelson Fern�ndez G�mez       FECHA DE CREACION: MAR 20/96 JUE A
       Colombia, Bucaramanga        INICIO: 08:40 PM   MAR 20/96 JUE

OBJETIVOS:

1- Define el objeto Browse del archivo

SINTAXIS:

 oBrwBan := oDefBrwBan(00,00,24,79)

*------------------------------------------------------------------------*
*                              PROGRAMA                                  *
*------------------------------------------------------------------------*/

FUNCTION oDefBrwInd(lShared,nFilSup,nColSup,nFilInf,nColInf,;
		    cTitUno,cTitDos,cTitTre)

*>>>>PARAMETROS DE LA FUNCION
/*     lShared                              // .T. Archivos Compartidos
       nFilSup	                            // Fila Superior
       nColSup	                            // Columna Superior
       nFilInf	                            // Fila Inferior
       nColInf	                            // Columna Inferior
       cTitUno                              // T�tulo Uno
       cTitDos                              // T�tulo Dos
       cTitTre                              // T�tulo Tres */
*>>>>FIN PARAMETROS DE LA FUNCION

*>>>>DECLARACION DE VARIABLES
       LOCAL oColumn := NIL                 // Columna del objeto
       LOCAL oBrwInd := NIL		    // Browse de Indcos
*>>>>FIN DECLARACION DE VARIABLES

*>>>>DEFINICION DEL OBJETO BROWSE
       oBrwInd := TBROWSEDB(nFilSup+1,nColSup+1,nFilInf-1,nColInf-1)
      *Definici�n de Objeto y asignaci�n de las coordenadas

       oBrwInd:ColSep    := '�'
       oBrwInd:HeadSep   := '�'

       oBrwInd:Cargo     := {cTitUno,cTitDos,cTitTre}
     *�Definici�n de cabeceras y l�neas de cabeceras

       SELECT IND
       oColumn	     := TBCOLUMNNEW('INDICADOR', {||IND->cNombreJui})
       oColumn:Cargo := {{'MODI',.F.},{'ALIAS','IND'},{'FIELD','cNombreJui'},;
			 {'PICTURE','@!'}}
       oBrwInd:ADDCOLUMN(oColumn)
     *�Definici�n Columna

       oColumn	     := TBCOLUMNNEW('CODIGO', {||IND->cCodigoJui})
       oColumn:Cargo := {{'MODI',.F.}}
       oBrwInd:ADDCOLUMN(oColumn)
     *�Definici�n Columna

       oColumn	     := TBCOLUMNNEW('PADRE', {||IND->cCodLogJui})
       oColumn:Cargo := {{'MODI',.F.},{'ALIAS','IND'},{'FIELD','cCodLogJui'},;
			 {'PICTURE','@!'}}
       oBrwInd:ADDCOLUMN(oColumn)
     *�Definici�n Columna

       oColumn	     := TBCOLUMNNEW('ID DEL CODIGO', {||IND->cIdeCodJui})
       oColumn:Cargo := {{'MODI',.F.},{'ALIAS','IND'},{'FIELD','cIdeCodJui'},;
			 {'PICTURE','@!'}}
       oBrwInd:ADDCOLUMN(oColumn)
     *�Definici�n Columna

       oColumn	     := TBCOLUMNNEW('PROFESOR', {||IND->cCodigoPro})
       oColumn:Cargo := {{'MODI',.F.},{'ALIAS','IND'},{'FIELD','cCodigoPro'},;
			 {'PICTURE','@!'}}
       oBrwInd:ADDCOLUMN(oColumn)
     *�Definici�n Columna

       RETURN oBrwInd
*>>>>FIN DEFINICION DEL OBJETO BROWSE