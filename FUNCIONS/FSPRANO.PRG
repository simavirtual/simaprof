/* SIMA - CONTABILIDAD ACADEMICA PROFESORES
		     CONFIGURACION DEL A�O SISTEMA

***************************************************************************
*-------------------- DECLARACION DE LAS FUNCIONES -----------------------*
**************************************************************************/

/*************************************************************************
* TITULO..: CREACION DE LA ESTRUCTURA                                    *
**************************************************************************

AUTOR: Nelson Fern�ndez G�mez       FECHA DE CREACION: feb 13/2008 DIA A
       Colombia, Bucaramanga        INICIO:  07:57 AM  feb 13/2008 DIA

OBJETIVOS:

1- Crea la estructura del archivo

2- Retorna NIL

*------------------------------------------------------------------------*
*                              PROGRAMA                                  *
*------------------------------------------------------------------------*/

FUNCTION CreaSprAno(lShared,nModCry,PathArc,fArchvo,aStrDbf)

*>>>>DESCRIPCION DE PARAMETROS
/*     lShared                              // .T. Sistema Compartido
       nModCry                              // Modo de Protecci�n
       PathArc                              // Path del Archivo
       fArchvo                              // Nombre del Archivo
       aStrDbf                              // Estructura del Archivo */
*>>>>FIN DESCRIPCION DE PARAMETROS

*>>>>DECLARACION DE VARIABLES
       LOCAL PathAct := ''                  // Path Actual

       LOCAL aDbfStr := {}                  // Estructura del Archivo
*>>>>FIN DECLARACION DE VARIABLES

*>>>>DECLARACION DE VARIABLES
       AADD(aDbfStr,{'PathUnoAno','Character',70,0}) // Path de Integraci�n No. 1
       AADD(aDbfStr,{'PathDosAno','Character',70,0}) // Path de Integraci�n No. 2
       AADD(aDbfStr,{'PathTreAno','Character',70,0}) // Path de Integraci�n No. 3
       AADD(aDbfStr,{'PathCuaAno','Character',70,0}) // Path de Integraci�n No. 4
       AADD(aDbfStr,{'cMaeHabAno','Character',09,0}) // Maestros Habilitados para el A�o
       AADD(aDbfStr,{'cJorHabAno','Character',04,0}) // Jornadas Habilitadas
       AADD(aDbfStr,{'cDerechAno','Character',110,0}) // Activaci�n de Derechos Individuales

       AADD(aDbfStr,{'cSysHabAno','Character',02,0}) // SPACES(02) => A�o habilitado. Diferente a Espacios => A�o NO Habilitado
       AADD(aDbfStr,{'dFecHabAno','Date'     ,08,0}) // Fecha de Cierre del Sistema
       AADD(aDbfStr,{'cHorHabAno','Character',08,0}) // Hora de Cierre del Sistema

       AADD(aDbfStr,{'nModRegAno','Numeric'  ,01,0}) // 0=Modificar Registros. 1=No Modificar Registros
       AADD(aDbfStr,{'cGraNovAno','Character',03,0}) // Grabaci�n de la Novedad 1: Confirma la Novedad por Defecto 2: Menu de Grabaci�n
       AADD(aDbfStr,{'lValNovAno','Logical'  ,01,0}) // .T. Validar la Novedad de Grabaci�n.
       AADD(aDbfStr,{'lTruncaAno','Logical'  ,01,0}) // .T. Trunca .F. Aproxima
       AADD(aDbfStr,{'cTitul1Ano','Character',04,0}) // T�tulo No.1

       AADD(aDbfStr,{'nIdeCodAno','Numeric'  ,16,0}) // C�digo del Registro
       AADD(aDbfStr,{'cNomUsrAno','Character',10,0}) // Nombre del Usuario
       AADD(aDbfStr,{'dFecUsrAno','Date'     ,08,0}) // Fecha del Usuario
       AADD(aDbfStr,{'cHorUsrAno','Character',10,0}) // Hora del Usuario
*>>>>FIN DECLARACION DE VARIABLES

*>>>>RETORNO DE LA ESTRUCTURA
       IF !EMPTY(aStrDbf)
          aStrDbf := aDbfStr
          RETURN NIL
       ENDIF
*>>>>FIN RETORNO DE LA ESTRUCTURA

*>>>>CREACION DE LA ESTRUCTURA
       PathAct := cPathAct()
       DO CASE
       CASE nCd(PathArc) == 0
            DBCREATE(fArchvo,aDbfStr,'DBFNTX')

       CASE nCd(PathArc) == -3
	    WAIT 'NO EXISTE EL DIRECTORIO: '+PathArc

       CASE nCd(PathArc) == -5
	    WAIT 'NO TIENE DERECHOS EN: '+PathArc
       ENDCASE
       nCd(PathAct)
*>>>>FIN CREACION DE LA ESTRUCTURA

*>>>>SELECCION DE LAS AREAS DE TRABAJO
       IF !lUseDbf(.T.,PathArc+'\'+fArchvo,'ARC',NIL,lShared)
	  cError('ABRIENDO EL ARCHIVO DE CONFIGURACION DEL SISTEMA')
	  Closeall()
	  RETURN NIL
       ENDIF
*>>>>FIN SELECCION DE LAS AREAS DE TRABAJO

*>>>>GRABACION DE LOS PARAMETROS POR DEFECTO
       IF ARC->(lRegLock(lShared,.T.))

	  REPL ARC->PathUnoAno WITH '\Sima\SimaCont'
	  REPL ARC->PathDosAno WITH ''
	  REPL ARC->PathTreAno WITH ''
	  REPL ARC->PathCuaAno WITH ''

	  REPL ARC->nIdeCodAno WITH ARC->(RECNO())
	  REPL ARC->cNomUsrAno WITH 'system'
	  REPL ARC->dFecUsrAno WITH DATE()
	  REPL ARC->cHorUsrAno WITH TIME()

	  ARC->(DBCOMMIT())
       ELSE
	  cError('NO SE GRABA EL REGISTRO INICIAL DE LA CONFIGURACION')
       ENDIF
       IF lShared
	  ARC->(DBUNLOCK())
       ENDIF
       CloseDbf('ARC')
       RETURN NIL
*>>>>FIN GRABACION DE LOS PARAMETROS POR DEFECTO

/*************************************************************************
* TITULO..: RECUPERACION DE LA ESTRUCTURA                                *
**************************************************************************

AUTOR: Nelson Fern�ndez G�mez       FECHA DE CREACION: feb 13/2008 DIA A
       Colombia, Bucaramanga        INICIO:  07:57 AM  feb 13/2008 DIA

OBJETIVOS:

1- Recupera la estructura del archivo

2- Retorna NIL

*------------------------------------------------------------------------*
*                              PROGRAMA                                  *
*------------------------------------------------------------------------*/

FUNCTION RecuSprAno(lShared,nModCry,PathArc,fArchvo)

*>>>>DESCRIPCION DE PARAMETROS
/*     lShared                        // .T. Sistema Compartido
       nModCry                        // Modo de Protecci�n
       PathArc			      // Path del Archivo
       fArchvo			      // Nombre del Archivo */
*>>>>FIN DESCRIPCION DE PARAMETROS

*>>>>RECUPERA LA ESTRUCTURA DEL ARCHIVO
       SAVE SCREEN
       WAIT "SE BORRO EL ARCHIVO "+PathArc+'\'+fArchvo

       Termometro(0,"CREANDO LA ESTRUCTURA DE CONFIGURACION DEL SISTEMA")
       Termometro(100)

       CreaSprAno(lShared,nModCry,PathArc,fArchvo)
       RESTORE SCREEN
       RETURN NIL
*>>>>FIN RECUPERA LA ESTRUCTURA DEL ARCHIVO

/*************************************************************************
* TITULO..: ACTUALIZACION DE LA ESTRUCTURA                               *
**************************************************************************

AUTOR: Nelson Fern�ndez G�mez       FECHA DE CREACION: feb 13/2008 DIA A
       Colombia, Bucaramanga        INICIO:  07:57 AM  feb 13/2008 DIA

OBJETIVOS:

1- Actualiza la Estructura del Archivo

2- Retorna el N�mero de Error:
   0 => No hay Error
	Estructura Actualizada
	Estructura Sin problemas
   1 => Estructura No Actualizada

*------------------------------------------------------------------------*
*                              PROGRAMA                                  *
*------------------------------------------------------------------------*/

FUNCTION nActSprAno(lShared,nModCry,PathArc,cNomArc)

*>>>>DESCRIPCION DE PARAMETROS
/*     lShared                              // .T. Sistema Compartido
       nModCry                              // Modo de Protecci�n
       PathArc                              // Path del Archivo
       cNomArc                              // Nombre del Archivo */
*>>>>FIN DESCRIPCION DE PARAMETROS

*>>>>DECLARACION DE VARIABLES
       LOCAL cSavPan := ''                  // Salvar Pantalla
       LOCAL nNroErr := 0                   // N�mero del Error
       LOCAL       i := 0                   // Contador
       LOCAL aStrNew := {'1'}               // Estructura Nueva
       LOCAL aStrOld := {}                  // Estructura Vieja
       LOCAL lCambio := .F.                 // .T. Cambio la Estructura
       LOCAL FileTem := ''                  // Archivo temporal
*>>>>FIN DECLARACION DE VARIABLES

*>>>>SELECCION DE LAS AREAS DE TRABAJO
       IF !lUseDbf(.T.,PathArc+'\'+cNomArc,'ACT',NIL,lShared,nModCry)
	  WAIT 'ABRIENDO EL ARCHIVO'
	  CloseDbf('ACT')
	  RETURN nNroErr := 1
       ENDIF
       aStrOld := ACT->(DBSTRUCT())
       CloseDbf('ACT')
       CreaSprAno(lShared,nModCry,PathArc,cNomArc,@aStrNew)
*>>>>FIN SELECCION DE LAS AREAS DE TRABAJO

*>>>>VALIDACION DE LA ESTRUCTURA
       lCambio := .T.
       IF LEN(aStrNew) == LEN(aStrOld)
          lCambio := .F.
          nNroErr := 1
       ENDIF

       IF !lCambio
          FOR i := 1 TO LEN(aStrNew)
              IF UPPER(aStrNew[i,1]) # UPPER(aStrOld[i,1]) .OR.;
                 UPPER(SUBS(aStrNew[i,2],1,1)) # UPPER(SUBS(aStrOld[i,2],1,1)) .OR.;
                 aStrNew[i,3] # aStrOld[i,3] .OR.;
                 aStrNew[i,4] # aStrOld[i,4]
                 lCambio := .T.
                 EXIT
              ENDIF
          ENDFOR
       ENDIF
*>>>>FIN VALIDACION DE LA ESTRUCTURA

*>>>>CONFIRMACION DE LA ACTUALIZACION
       IF lCambio .AND. lShared
	  WAIT 'El Archivo CONFIGURACION DEL A�O su Estructura Esta DesActualizada.'
	  lCambio := lPregunta('DESEA ACTUALIZAR LA ESTRUCTURA? No Si')
	  IF !lCambio
	     nNroErr := 1
	  ENDIF
       ENDIF
*>>>>FIN CONFIRMACION DE LA ACTUALIZACION

*>>>>ACTUALIZACION DE LA ESTRUCTURA
       IF lCambio
          FileTem := SUBS(cNomArc,1,AT('.',cNomArc))+'STR'
          cSavPan := SAVESCREEN(0,0,24,79)
          IF !lStrucFile(lShared,;
                         PathArc+'\'+cNomArc,;
                         PathArc+'\'+FileTem,;
			 {||CreaSprAno(lShared,nModCry,PathArc,FileTem)})
	     WAIT 'NO SE ACTUALIZO LA ESTRUCTURA DEL ARCHIVO'
	     nNroErr := 1
	  ENDIF
	  RESTSCREEN(0,0,24,79,cSavPan)
       ENDIF
       RETURN nNroErr
*>>>>FIN ACTUALIZACION DE LA ESTRUCTURA

/*************************************************************************
* TITULO..: MANTENIMIENTO DEL ARCHIVO                                    *
**************************************************************************

AUTOR: Nelson Fern�ndez G�mez       FECHA DE CREACION: feb 13/2008 DIA A
       Colombia, Bucaramanga        INICIO:  07:57 AM  feb 13/2008 DIA

OBJETIVOS:

1- Permite el mantenimiento del archivo

*------------------------------------------------------------------------*
*                              PROGRAMA                                  *
*------------------------------------------------------------------------*/

FUNCTION ManSprAno(aP1,aP2,aP3,bMenSys)

*>>>>DESCRIPCION DE PARAMETROS
/*     aP1                                  // Parametros Generales
       aP2                                  // Parametros Generales
       aP3                                  // Parametros Generales
       bMenSys                              // Menus del Sistema */
*>>>>FIN DESCRIPCION DE PARAMETROS

*>>>>DECLARACION DE VARIABLES
       #INCLUDE "inkey.ch"                  // Declaraci�n de teclas
       #INCLUDE "CAMPOS\ARC-SPR.PRG"        // Archivos del Sistema

       LOCAL oBrowse := NIL                 // Browse del Archivo
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

       LOCAL nFilPal := xPrm(aP1,'nFilPal') // Fila Inferior Men� principal
       LOCAL nFilInf := xPrm(aP1,'nFilInf') // Fila Inferior del SubMen�
       LOCAL nColInf := xPrm(aP1,'nColInf') // Columna Inferior del SubMen�
     *�Detalles Tecnicos

       LOCAL cMaeAlu := xPrm(aP1,'cMaeAlu') // Maestros habilitados
       LOCAL cMaeAct := xPrm(aP1,'cMaeAct') // Maestro Activo
       LOCAL cJorTxt := xPrm(aP1,'cJorTxt') // Jornada escogida
     *�Detalles Acad�micos

       CloseAll()
*>>>>FIN DECLARACION PARAMETROS GENERALES

*>>>>SELECION DE LAS AREAS DE TRABAJO
       IF !lUseDbf(.T.,cPatSis+'\'+fSprAno+cAnoUsr+ExtFile,'ARC',NIL,lShared)
	  cError('ABRIENDO EL ARCHIVO DE CONFIGURACION DEL A�O')
	  CloseAll()
	  RETURN NIL
       ENDIF
*>>>>FIN SELECION DE LAS AREAS DE TRABAJO

*>>>>MANTENIMIENTO DEL ARCHIVO
       oBrowse := oBrwSprAno(lShared,cAnoUsr,nFilPal+1,00,22,79)
       SETKEY(K_F5,{||DerSprAno(lShared,bMenSys,cNomSis)})
       ARC->(CtrlBrw(lShared,oBrowse))
       SETKEY(K_F5,NIL)

       CloseAll()
       RETURN NIL
*>>>>FIN MANTENIMIENTO DEL ARCHIVO

/*************************************************************************
* TITULO..: DEFINICION DEL OBJETO BROWSE                                 *
**************************************************************************

AUTOR: Nelson Fern�ndez G�mez       FECHA DE CREACION: feb 13/2008 DIA A
       Colombia, Bucaramanga        INICIO:  07:57 AM  feb 13/2008 DIA

OBJETIVOS:

1- Define el objeto Browse del archivo

SINTAXIS:


*------------------------------------------------------------------------*
*                              PROGRAMA                                  *
*------------------------------------------------------------------------*/

FUNCTION oBrwSprAno(lShared,cAnoUsr,nFilSup,nColSup,nFilInf,nColInf)

*>>>>PARAMETROS DE LA FUNCION
/*     lShared                              // .T. Archivos Compartidos
       cAnoUsr                              // A�o del Usuario
       nFilSup	                            // Fila Superior
       nColSup	                            // Columna Superior
       nFilInf	                            // Fila Inferior
       nColInf	                            // Columna Inferior */
*>>>>FIN PARAMETROS DE LA FUNCION

*>>>>DECLARACION DE VARIABLES
       LOCAL oColumn := NIL                 // Objeto Columna
       LOCAL oBrowse := NIL	            // Browse de Clientes
*>>>>FIN DECLARACION DE VARIABLES

*>>>>DEFINICION DEL OBJETO BROWSE
       oBrowse := TBROWSEDB(nFilSup+1,nColSup+1,nFilInf-1,nColInf-1)
      *Definici�n de Objeto y asignaci�n de las coordenadas

       oBrowse:ColSep    := '�'
       oBrowse:HeadSep   := '�'
       oBrowse:Cargo     := {'<< CONFIGURACION DEL A�O >>',;
			    '<ENTER>Modificar  <ESC>Abandonar'}
     *�Definici�n de cabeceras y l�neas de cabeceras

       SELECT ARC
       oColumn	:= TBCOLUMNNEW('PATH DE SIMACONT',{||ARC->PathUnoAno})
       oColumn:Cargo := {{'MODI',.T.},{'ALIAS','ARC'},;
			 {'FIELD','PathUnoAno'},{'PICTURE','@X'}}
       oBrowse:ADDCOLUMN(oColumn)
     *�Definici�n Columna

/*
       oColumn	:= TBCOLUMNNEW('PATH DE INTEGRACION 2',{||ARC->PathDosAno})
       oColumn:Cargo := {{'MODI',.T.},{'ALIAS','ARC'},;
			 {'FIELD','PathDosAno'},{'PICTURE','@X'}}
       oBrowse:ADDCOLUMN(oColumn)
     *�Definici�n Columna

       oColumn	:= TBCOLUMNNEW('PATH DE INTEGRACION 3',{||ARC->PathTreAno})
       oColumn:Cargo := {{'MODI',.T.},{'ALIAS','ARC'},;
			 {'FIELD','PathTreAno'},{'PICTURE','@X'}}
       oBrowse:ADDCOLUMN(oColumn)
     *�Definici�n Columna

       oColumn	:= TBCOLUMNNEW('PATH DE INTEGRACION 4',{||ARC->PathCuaAno})
       oColumn:Cargo := {{'MODI',.T.},{'ALIAS','ARC'},;
			 {'FIELD','PathCuaAno'},{'PICTURE','@X'}}
       oBrowse:ADDCOLUMN(oColumn)
     *�Definici�n Columna
*/

       oColumn	:= TBCOLUMNNEW('MAESTROS;HABILITADOS',{||ARC->cMaeHabAno})
       oColumn:Cargo := {{'MODI',.T.},{'ALIAS','ARC'},;
			 {'FIELD','cMaeHabAno'}, {'PICTURE','@!'}}
       oBrowse:ADDCOLUMN(oColumn)
     *�Definici�n Columna

       oColumn	:= TBCOLUMNNEW('JORNADAS;HABILITADAS',{||ARC->cJorHabAno})
       oColumn:Cargo := {{'MODI',.T.},{'ALIAS','ARC'},;
			 {'FIELD','cJorHabAno'}, {'PICTURE','@!'}}
       oBrowse:ADDCOLUMN(oColumn)
     *�Definici�n Columna

       oColumn	:= TBCOLUMNNEW('INGRESO ACTIVADO;AL SISTEMA',{||ARC->cSysHabAno})
       oColumn:Cargo := {{'MODI',.T.},{'ALIAS','ARC'},;
			 {'FIELD','cSysHabAno'},{'PICTURE','99'}}
       oBrowse:ADDCOLUMN(oColumn)
     *�Definici�n Columna

       oColumn	:= TBCOLUMNNEW('FECHA CIERRE;DEL SISTEMA',{||ARC->dFecHabAno})
       oColumn:Cargo := {{'MODI',.T.},{'ALIAS','ARC'},;
			 {'FIELD','dFecHabAno'},{'PICTURE','@D'}}
       oBrowse:ADDCOLUMN(oColumn)
     *�Definici�n Columna

       oColumn	:= TBCOLUMNNEW('HORA CIERRE;DEL SISTEMA',{||ARC->cHorHabAno})
       oColumn:Cargo := {{'MODI',.T.},{'ALIAS','ARC'},;
			 {'FIELD','cHorHabAno'},{'PICTURE','XXXXXXXX'}}
       oBrowse:ADDCOLUMN(oColumn)
     *�Definici�n Columna

       oColumn	:= TBCOLUMNNEW('GRABAR NOVEDAD',{||ARC->cGraNovAno})
       oColumn:Cargo := {{'MODI',.T.},{'ALIAS','ARC'},;
			 {'FIELD','cGraNovAno'},{'PICTURE','999999'}}
       oBrowse:ADDCOLUMN(oColumn)
     *�Definici�n Columna

       oColumn	:= TBCOLUMNNEW('NO VALIDAR NOVEDAD',{||ARC->lValNovAno})
       oColumn:Cargo := {{'MODI',.T.},{'ALIAS','ARC'},;
			 {'FIELD','lValNovAno'},{'PICTURE','@!'}}
       oBrowse:ADDCOLUMN(oColumn)
     *�Definici�n Columna

       oColumn	:= TBCOLUMNNEW('MODIFICAR REGISTROS',{||ARC->nModRegAno})
       oColumn:Cargo := {{'MODI',.T.},{'ALIAS','ARC'},;
			 {'FIELD','nModRegAno'},{'PICTURE','9'}}
       oBrowse:ADDCOLUMN(oColumn)
     *�Definici�n Columna


       oColumn	:= TBCOLUMNNEW('TITULO No.1',{||ARC->cTitul1Ano})
       oColumn:Cargo := {{'MODI',.T.},{'ALIAS','ARC'},;
			 {'FIELD','cTitul1Ano'},{'PICTURE','@!'}}
       oBrowse:ADDCOLUMN(oColumn)
     *�Definici�n Columna



       oColumn	:= TBCOLUMNNEW('TRUNCAR DEFINITIVA',{||ARC->lTruncaAno})
       oColumn:Cargo := {{'MODI',.T.},{'ALIAS','ARC'},;
			 {'FIELD','lTruncaAno'},{'PICTURE','@!'}}
       oBrowse:ADDCOLUMN(oColumn)
     *�Definici�n Columna

       oColumn	     := TBCOLUMNNEW('NOMBRE;DEL USUARIO',{||ARC->cNomUsrAno})
       oColumn:Cargo := {{'MODI',.F.}}
       oBrowse:ADDCOLUMN(oColumn)
     *�Definici�n Columna

       oColumn	     := TBCOLUMNNEW('FECHA DE;PROCESO',;
				    {||cFecha(ARC->dFecUsrAno)})
       oColumn:Cargo := {{'MODI',.F.}}
       oBrowse:ADDCOLUMN(oColumn)
     *�Definici�n Columna

       oColumn	     := TBCOLUMNNEW('HORA DE;PROCESO',;
				    {||cHoraSys(ARC->cHorUsrAno)})
       oColumn:Cargo := {{'MODI',.F.}}
       oBrowse:ADDCOLUMN(oColumn)
     *�Definici�n Columna

       oColumn	     := TBCOLUMNNEW('CODIGO',{||ARC->nIdeCodAno})
       oColumn:Cargo := {{'MODI',.F.}}
       oBrowse:ADDCOLUMN(oColumn)
     *�Definici�n Columna

       oColumn	     := TBCOLUMNNEW('No.',{||ARC->(RECNO())})
       oColumn:Cargo := {{'MODI',.F.}}
       oBrowse:ADDCOLUMN(oColumn)
     *�Definici�n Columna

       RETURN oBrowse
*>>>>FIN DEFINICION DEL OBJETO BROWSE

/*************************************************************************
* TITULO..: DERECHOS DEL SISTEMA                                         *
**************************************************************************

AUTOR: Nelson Fern�ndez G�mez       FECHA DE CREACION: FEB 22/2008 VIE A
       Colombia, Bucaramanga        INICIO: 11:00 AM   FEB 22/2008 VIE

1- Permite asignar y grabar los derechos a los men�s del sistema

2- Retorna Nil

*------------------------------------------------------------------------*
*                              PROGRAMA                                  *
*------------------------------------------------------------------------*/

FUNCTION DerSprAno(lShared,bMenSys,cNomSis)

*>>>>DESCRIPCION DE PARAMETROS
/*     lShared                              // .T. Archivos Compartidos
       bMenSys				    // Menu del Sistema
       cNomSis                              // Nombre del Sistema */
*>>>>FIN DESCRIPCION DE PARAMETROS

*>>>>DECLARACION DE VARIABLES
       LOCAL cDerech := ''                  // Derechos del Sistema
*>>>>FIN DECLARACION DE VARIABLES

*>>>>DERECHOS DEL SISTEMA
       cDerech := ARC->cDerechAno
       cDerech := cOpcOnOf(lShared,bMenSys,cDerech,cNomSis)

       IF ARC->(lRegLock(lShared,.F.))
	  REPL ARC->cDerechAno WITH cDerech
	  ARC->(DBCOMMIT())
       ELSE
	  cError('NO SE GRABAN LOS DERECHOS')
       ENDIF
       IF lShared
	  ARC->(DBUNLOCK())
       ENDIF
       RETURN NIL
*>>>>FIN DERECHOS DEL SISTEMA

/*************************************************************************
* TITULO ..: DERECHOS DEL SISTEMA                                        *
**************************************************************************

AUTOR: Nelson Fern�ndez G�mez       FECHA DE CREACION: FEB 22/2008 VIE A
       Colombia, Bucaramanga        INICIO: 11:45 AM   FEB 22/2008 VIE

OBJETIVOS:

1- Activa los derechos del Sistema

2- Retorna los derechos del Sistema

SINTAXIS:

*------------------------------------------------------------------------*
*                              PROGRAMA                                  *
*------------------------------------------------------------------------*/

FUNCTION cDerSprAno(cDerUsr,cDerSys)

*>>>>DESCRIPCION DE PARAMETROS
/*     cDerUsr                              // Derechos del Usuario
       cDerSys                              // Derechos del Sistema */
*>>>>FIN DESCRIPCION DE PARAMETROS

*>>>>DECLARACION DE VARIABLES
       LOCAL i := 0                         // Contador
*>>>>FIN DECLARACION DE VARIABLES

*>>>>OPCION DE CLASIFICACION
      i := 47
      IF VAL(SUBS(cDerUsr,i,1)) == 1 .AND.;
	 VAL(SUBS(cDerSys,i,1)) == 0
	 cDerUsr := STUFF(cDerUsr,i,1,'0')
      ENDIF
      RETURN cDerUsr
*>>>>FIN OPCION DE CLASIFICACION

/*************************************************************************
* TITULO..: LECTURAS PATHS DE INTEGRACION                                *
**************************************************************************

AUTOR: Nelson Fern�ndez G�mez       FECHA DE CREACION: feb 13/2008 DIA A
       Colombia, Bucaramanga        INICIO:  07:57 AM  feb 13/2008 DIA

OBJETIVOS:

1- Lee del archivo los paths de integraci�n

2- Retorna por referencia los param�tros de la funci�n marcados con @

3- Retorna Nil

SINTAXIS:

*------------------------------------------------------------------------*
*                              PROGRAMA                                  *
*------------------------------------------------------------------------*/

FUNCTION PathSprAno(lShared,nModCry,cAnoUsr,cPatSis,;
		   PathUno,PathDos,PathTre,PathCua)

*>>>>DESCRIPCION DE PARAMETROS
/*     lShared                              // .T. Archivos Compartidos
       nModCry                              // Modo de Protecci�n
       cAnoUsr                              // A�o del Usuario
       cPatSis                              // Path del sistema
       PathUno			            // @Path de Integraci�n uno
       PathDos			            // @Path de Integraci�n dos
       PathTre			            // @Path de Integraci�n tres
       PathCua			            // @Path de Integraci�n cuatro */
*>>>>FIN DESCRIPCION DE PARAMETROS

*>>>>DECLARACION DE VARIABLES
       #INCLUDE "CAMPOS\ARC-SPR.PRG"       // Archivos del Sistema
*>>>>FIN DECLARACION DE VARIABLES

*>>>>DECLARACION DE VARIABLES
       LOCAL PathAct := ''                  // Path Actual
*>>>>FIN DECLARACION DE VARIABLES

*>>>>DIRECTORIO DEL A�O
       PathAct := cPathAct()

       Mdir(cPatSis,PathAct)
     *�Directorio del A�o.
*>>>>FIN DIRECTORIO DEL A�O

*>>>>VALIDACION DEL ARCHIVO
       IF !FILE(cPatSis+'\'+fSprAno+cAnoUsr+ExtFile)
	  RecuSprAno(lShared,nModCry,cPatSis,;
		     fSprAno+cAnoUsr+ExtFile)
       ELSE
	  nActSprAno(lShared,nModCry,cPatSis,;
		     fSprAno+cAnoUsr+ExtFile)
       ENDIF
*>>>>FIN VALIDACION DEL ARCHIVO

*>>>>SELECION DE LAS AREAS DE TRABAJO
       IF !lUseDbf(.T.,cPatSis+'\'+fSprAno+cAnoUsr+ExtFile,'ARC',NIL,lShared)
	  cError('ABRIENDO EL ARCHIVO DEL A�O','PATH DE INTEGRACION')
	  CloseAll()
	  RETURN .F.
       ENDIF
*>>>>FIN SELECION DE LAS AREAS DE TRABAJO

*>>>>LECTURA DE LOS PATHS DE INTEGRACION
       SELECT ARC
       ARC->(DBGOTOP())
       PathUno := ALLTRIM(ARC->PathUnoAno)
       PathDos := ALLTRIM(ARC->PathDosAno)
       PathTre := ALLTRIM(ARC->PathTreAno)
       PathCua := ALLTRIM(ARC->PathCuaAno)
       CloseDbf('ARC')
       RETURN .T.
*>>>>FIN LECTURA DE LOS PATHS DE INTEGRACION