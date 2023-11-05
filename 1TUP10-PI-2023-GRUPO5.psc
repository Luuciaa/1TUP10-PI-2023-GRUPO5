SubProceso PRECARGA(rutasAereas, capacidadPasajeros, asientosVendidos, costoBase)
		
		capacidadPasajeros[0] = 120
		capacidadPasajeros[1] = 120
		capacidadPasajeros[2] = 80
		capacidadPasajeros[3] = 80
		
		costoBase[0] = 150000
		costoBase[1] = 120000
		costoBase[2] = 70000
		costoBase[3] = 95000
		
		rutasAereas[0] = "Buenos Aires - Bariloche"
		rutasAereas[1] = "Buenos Aires - Salta    "
		rutasAereas[2] = "Rosario - Buenos Aires  "
		rutasAereas[3] = "Mar Del Plata - Mendoza "
		
		asientosVendidos[0]=0
		asientosVendidos[1]=0
		asientosVendidos[2]=0
		asientosVendidos[3]=0
FinsubProceso

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
Subproceso elegirOpcion <- MENU
	
    Definir op, elegirOpcion Como Entero  
	
	Borrar Pantalla
	
	Escribir "------------------------------"
	Escribir "BIENVENIDO A RUN AIRLINES "
	Escribir "------------------------------"
	Escribir " "
	Escribir "------------------------------"
	Escribir "Menu de opciones"
	Escribir "------------------------------"
	Escribir "1. Venta pasaje "
	Escribir "2. Buscar pasaje vendido "
	Escribir "3. Buscar pasajero"
	Escribir "4. Ordenar y mostrar lista pasajeros "
	Escribir "5. Listado/s "
	Escribir "------------------------------"
	Escribir " Elija una opción"
	Leer op
	
	elegirOpcion <- op
	
FinSubproceso

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
SubProceso ult <- VENTA(rutasAereas, capacidadPasajeros, asientosVendidos, costoBase,vRutasAereas, vNombreApellido,vDni,vAsiento,vNroRuta,ult)
	
	Definir dni,numPasajeroFrecuente,opcionRuta Como Entero
	Definir equipajeBodega Como Entero
	Definir i Como Entero
	Definir precio,recargo,CostoPasaje, recargoBodega Como Real
	Definir telefono, nombreApellido Como Caracter
	
	Escribir "Rutas Aéreas Disponibles:"
	
	Para i Desde 0 Hasta 3 Hacer
		Escribir i+1, ". ", rutasAereas[i]
	FinPara
	
	Escribir "------------------------------"
	Repetir
		Escribir "Elija una ruta aérea (1-4): "
		Leer opcionRuta
		
		Si opcionRuta < 1 | opcionRuta > 4 Entonces
			Escribir "Error! Debe ser un número entre 1 y 4"					
		Fin Si
	Hasta que (opcionRuta >= 1) y (opcionRuta <=4)   
	
	i <- opcionRuta-1
	
	Si asientosVendidos[i] < capacidadPasajeros[i]  Entonces
		
		asientosVendidos[i] <- asientosVendidos[i] + 1
		
		Escribir "Ingrese el Nombre y Apellido del pasajero: "
		Leer nombreApellido
		
		Mientras nombreApellido = "" Hacer
            Escribir "El nombre no puede estar vacío. Ingrese el nombre nuevamente"
            Leer nombreApellido
        FinMientras
		
		Escribir "Ingrese el DNI del pasajero: "
		Leer dni
		
		Mientras (dni <= 1111111 O dni >= 99999999)  Hacer
            Escribir "El dni es incorrecto, tiene que estar entre 11111111 y 99999999, ingrese nuevamente"
            Leer dni
        FinMientras
		
		Escribir "Ingrese el teléfono del pasajero: "
		Leer telefono
		
		Mientras telefono = "" Hacer
            Escribir "El telefono no puede estar vacío. Ingrese el teléfono nuevamente"
            Leer telefono
        FinMientras
		
		Escribir "¿Tiene equipaje en bodega? (1-Verdadero/2-Falso): "
		Leer equipajeBodega
		
		Mientras equipajeBodega <> 1 y equipajeBodega <> 2 Hacer
			escribir "El equipaje de bodega debe ser un 1 o 2"
			Leer equipajeBodega
		FinMientras
		
		Escribir "Ingrese el número de pasajero frecuente: "
		Leer numPasajeroFrecuente
		
		Mientras (numPasajeroFrecuente <= 0 O numPasajeroFrecuente >= 99999999)  Hacer
            Escribir "El numero de Pasajero Frecuente es incorrecto, tiene que estar entre 0 y 99999999, ingrese nuevamente"
            Leer numPasajeroFrecuente
        FinMientras
		
		Si opcionRuta = 1 o opcionRuta = 2 entonces
			
			Si asientosVendidos[i] <= 20 Entonces
				
				precio <- costoBase[i] 
			SiNo
				Si asientosVendidos[i] <= 60 Entonces
					recargo <- costoBase[i] * 10 / 100
					precio <- costoBase[i] + recargo
				SiNo
					recargo <- costoBase[i] * 10 / 100
					precio <- costoBase[i] + recargo
				FinSi
			FinSi
		SiNo
			Si asientosVendidos[i] <= 10 Entonces
				precio <- costoBase[i] 
			SiNo
				Si asientosVendidos[i] <= 40 Entonces
					recargo <- costoBase[i] * 15 / 100
					precio <- costoBase[i] + recargo
				SiNo
					recargo <- costoBase[i] * 10 / 100
					precio <- costoBase[i] + recargo
				FinSi
			FinSi
		FinSi
		
		Si equipajeBodega = 1 Entonces
			
			recargoBodega <- precio *5/100
		SiNo
			recargoBodega <- 0
		FinSi
		
		CostoPasaje <- precio + recargoBodega
		
		Escribir " "
		escribir "------ TICKET ----------"
		Escribir "Ruta                     : ", rutasAereas[i]
		Escribir "Nombre y Apellido        : " nombreApellido
		Escribir "DNI                      : " dni
		Escribir "Teléfono                 :  " telefono
		Escribir "------------------------"
		
		Si equipajeBodega = 1 Entonces
			
			Escribir "Equipaje en bodega       : Verdadero"		
		SiNo
			Escribir "Equipaje en bodega       : Falso"
		FinSi
		
		Escribir "-------------------------"
		Escribir "Número pasajero frecuente: " numPasajeroFrecuente
		Escribir "Asiento                  : " asientosVendidos[i]
		Escribir "Costo pasaje             $ " CostoPasaje
		Escribir "-------------------------"
		Escribir " "
		
		vRutasAereas[ult] <- rutasAereas[i]
		vNroRuta[ult] <- opcionRuta
		vNombreApellido[ult] <- nombreApellido
		vDni[ult] <- dni
		vAsiento[ult] <- asientosVendidos[i]
		
		ult <- ult + 1
		
	SiNo
		Escribir "No hay asientos disponibles para la ruta elegida"
	Fin Si
	
finSubProceso

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
SubProceso  BUSQUEDA_POR_NRO(rutasAereas, vRutasAereas, vNombreApellido,vDni,vAsiento,vNroRuta, ult)
	
	Definir asientoABuscar, i, opcionRuta, pos Como Entero
	
	Escribir "Ingrese el nro de asiento a buscar: "
	Leer asientoABuscar
	
	Mientras asientoABuscar <= 0 Hacer
		Escribir "Error! Debe ser un valor mayor a cero"
		Leer asientoABuscar
	FinMientras
	
	Escribir "--->Rutas Aéreas Disponibles<---:"
	
	Para i Desde 0 Hasta 3 Hacer
		Escribir i+1, ". ", rutasAereas[i]
	FinPara
	
	Escribir " "
	
	Repetir
		
		Escribir "Elija una ruta aérea (1-4): "
		Leer opcionRuta
		
		Si opcionRuta < 1 | opcionRuta > 4 Entonces
			Escribir "Error! Debe ser un número entre 1 y 4"					
		Fin Si
	Hasta que (opcionRuta >= 1) y (opcionRuta <=4)   
	
	pos <- -1
	i <- 0
	
	Mientras i < ult y pos = -1 Hacer
		
		Si asientoABuscar = vAsiento[i] y opcionRuta = vNroRuta[i] Entonces
			pos <- i
		SiNo
			i <- i + 1
		FinSi
	FinMientras
	
	Si pos = -1  Entonces
		Escribir "No existen los datos ingresados"
	SiNo
		Escribir " "
		Escribir "------------------------------------------"
		Escribir "Los datos del pasajero son "
	    Escribir "Nombre y Apellido: " vNombreApellido[pos]
	    Escribir "Ruta             : " vRutasAereas[pos]
	    Escribir "DNI              : " vDni[pos]
		Escribir "------------------------------------------"
	FinSi
	
FinSubProceso

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
SubProceso  BUSQUEDA_POR_NOMBRE(vRutasAereas, vNombreApellido,vDni,vAsiento,vNroRuta, ult)
	
	Definir nombreABuscar Como Caracter
	Definir pos, i Como Entero
	
	Escribir "Ingrese el nombre del pasajero a buscar: "
	Leer nombreABuscar
	
	Mientras nombreABuscar = "" Hacer
		Escribir "El nombre no puede estar vacío. Ingrese el nombre nuevamente"
		Leer nombreABuscar 
	FinMientras
	
	pos <- -1
	i <- 0
	Mientras i < ult y pos = -1 hacer
		
		Si nombreABuscar = vNombreApellido[i] Entonces
			pos <- i
		SiNo
			i <- i + 1
		FinSi
	FinMientras
	
	si pos = -1  Entonces
		
		Escribir "No existen los datos ingresados"
	SiNo
		
		Escribir " "
		Escribir "---------------------------"
		Escribir "Los datos del pasajero son "
	    Escribir "Nombre y Apellido: " vNombreApellido[pos]
	    Escribir "Ruta             : " vRutasAereas[pos]
	    Escribir "DNI              : " vDni[pos]
		Escribir "---------------------------"
	FinSi
	
FinSubProceso


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
SubProceso  LISTADO_PASAJEROS(rutasAereas, vRutasAereas, vNombreApellido,vDni,vAsiento,vNroRuta, ult)
	
	
	Definir opcionRuta, i, orden Como Entero
	
	
	Escribir "--->Rutas Aéreas Disponibles<---:"
	Para i Desde 0 Hasta 3 Hacer
		Escribir i+1, ". ", rutasAereas[i]
	FinPara
	
	Escribir " "
	Repetir
		
		Escribir "Elija una ruta aérea (1-4): "
		Leer opcionRuta
		
		Si opcionRuta < 1 | opcionRuta > 4 Entonces
			
			Escribir "Error! Debe ser un número entre 1 y 4"					
		Fin Si
	Hasta que (opcionRuta >= 1) y (opcionRuta <=4)   
	
	Escribir "Cómo desea ordenar (1- Ascendente ; 2- Descendente)"
	Leer orden
	
	Mientras orden <> 1 y orden <> 2 Hacer
		Escribir "Error! Debe ser 1 o 2"
		Leer orden
	FinMientras
	
	Si orden = 1 Entonces
		ordenamientoASC(vRutasAereas, vNombreApellido,vDni,vAsiento,vNroRuta, ult)
	SiNo
		ordenamientoDESC(vRutasAereas, vNombreApellido,vDni,vAsiento,vNroRuta, ult)
	FinSi
	
	Escribir " "
	Escribir "Listado de pasajeros"
	
	Para i desde 0 hasta ult-1 con paso 1 hacer
		
		Si opcionRuta = vNroRuta[i] Entonces
			
			Escribir ".............................................."
			Escribir "Nro de ruta      : ", vNroRuta[i]
			Escribir "Ruta             : ", vRutasAereas[i]
			Escribir "Nombre y Apellido: ", vNombreApellido[i]
			Escribir "DNI              : ", vDni[i]
			Escribir "Asiento          : ", vAsiento[i]
			Escribir ".............................................."
	    FinSi
	FinPara
FinSubProceso

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
SubProceso ordenamientoASC(vRutasAereas, vNombreApellido,vDni,vAsiento,vNroRuta, ult)
	
	Definir i, j, aux como Entero
	
	Para i desde 0 hasta ult-2 hacer
		
		Para j desde i+1 hasta ult-1 Hacer 
			
			Si vAsiento[i] > vAsiento[j] Entonces 
				aux <- vNroRuta[i]
				vNroRuta[i] <- vNroRuta[j]
				vNroRuta[j] <- aux
				
				aux2<- vNombreApellido[i]
				vNombreApellido[i] <- vNombreApellido[j]
				vNombreApellido[j] <- aux2
				
				aux3<- vDni[i]
				vDni[i] <- vDni[j]
				vDni[j] <- aux3
				
				aux4<- vRutasAereas[i]
				vRutasAereas[i] <- vRutasAereas[j]
				vRutasAereas[j] <- aux4
				
				aux5<- vAsiento[i]
				vAsiento[i] <- vAsiento[j]
				vAsiento[j] <- aux5
				
			FinSi
		FinPara
	FinPara
FinSubProceso

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
SubProceso ordenamientoDESC(vRutasAereas, vNombreApellido,vDni,vAsiento,vNroRuta, ult)
	
	Definir i, j, aux,aux3,aux5 como Entero
	Definir aux2,aux4 Como Caracter
	
	Para i desde 0 hasta ult-2 hacer
		
		Para j desde i+1 hasta ult-1 Hacer
			
			Si vAsiento[i] < vAsiento[j] Entonces  
				aux <- vNroRuta[i]
				vNroRuta[i] <- vNroRuta[j]
				vNroRuta[j] <- aux
				
				aux2<- vNombreApellido[i]
				vNombreApellido[i] <- vNombreApellido[j]
				vNombreApellido[j] <- aux2
				
				aux3<- vDni[i]
				vDni[i] <- vDni[j]
				vDni[j] <- aux3
				
				aux4<- vRutasAereas[i]
				vRutasAereas[i] <- vRutasAereas[j]
				vRutasAereas[j] <- aux4
				
				aux5<- vAsiento[i]
				vAsiento[i] <- vAsiento[j]
				vAsiento[j] <- aux5
				
			FinSi
		FinPara
	FinPara
	
FinSubProceso

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
SubProceso LISTADOS(asientosVendidos,rutasAereas)
	
	Definir op, suma, i, porcentaje Como Entero
	
	Escribir "--------------------------------------------"
	Escribir "Listados"
	Escribir "--------------------------------------------"
	Escribir "1. Cantidad de pasajes vendido por ruta aérea"
	Escribir "2. Porcentaje de ventas por ruta aérea"
	Escribir " "
	Escribir "Ingrese una opción: 	"
	leer op
	
	Segun op Hacer
		1:
			Escribir "Cantidad de pasajes vendido por ruta aérea"
			Escribir "Ruta                         Cantidad de pasajes "
			Para i<-0 Hasta 3 Con Paso 1 Hacer
				Escribir  rutasAereas[i], "      ", asientosVendidos[i]
			Fin Para
		2:
			suma <- 0
			
			Para i <- 0 Hasta 3 Con Paso 1 Hacer
				suma <- suma + asientosVendidos[i]
			Fin Para
			
			Escribir "Porcentaje de ventas por Ruta Aérea"
			Escribir "Ruta                           Porcentaje de pasajes vendidos sobre el total"  
			Para i<-0 Hasta 3 Con Paso 1 Hacer
				porcentaje <- (asientosVendidos[i] / suma) * 100
				Escribir rutasAereas[i], "        ", porcentaje
			FinPara
		De Otro Modo:
			Escribir "Error! Opcion incorrecta!"
	Fin Segun
	
FinSubProceso

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
Proceso VentaPasajes
	
	Dimension vRutasAereas[400], vNombreApellido[400], vDni[400], vAsiento[400], vNroRuta[400]
	
	Definir vRutasAereas Como Caracter
	Definir vDni, vAsiento,vNroRuta Como Entero
	Definir vNombreApellido Como Caracter
	
	Dimension rutasAereas[4], capacidadPasajeros[4], asientosVendidos[4], costoBase[4]
	Definir rutasAereas Como Caracter
	Definir capacidadPasajeros, asientosVendidos, costoBase Como Entero
	
	Definir respuesta como Caracter
	Definir ult, elegirOpcion como Entero
	
	PRECARGA(rutasAereas, capacidadPasajeros, asientosVendidos, costoBase)
	
	ult <- 0
	
	Repetir
		
		elegirOpcion <- MENU()
		
		Segun elegirOpcion Hacer
			
			1:
				ult <- VENTA(rutasAereas, capacidadPasajeros, asientosVendidos, costoBase,vRutasAereas, vNombreApellido,vDni,vAsiento,vNroRuta, ult)
				
		    2:
				BUSQUEDA_POR_NRO(rutasAereas, vRutasAereas, vNombreApellido,vDni,vAsiento,vNroRuta, ult)
				
			3:	
				BUSQUEDA_POR_NOMBRE(vRutasAereas, vNombreApellido,vDni,vAsiento,vNroRuta, ult)
				
			4:
				LISTADO_PASAJEROS(rutasAereas,vRutasAereas, vNombreApellido,vDni,vAsiento,vNroRuta, ult)
				
			5:
				LISTADOS(asientosVendidos,rutasAereas)
				
			De Otro Modo:
				Escribir "Opcion incorrecta! Debe ser un numero del 0 al 5"
				
		FinSegun
		
		Escribir " "
		Escribir "------------------------------------"
		Escribir "Presione una tecla para continuar"
		Escribir "------------------------------------"
		Esperar tecla
		Escribir " "
		
		Escribir " "
		Escribir "------------------------------------------------------------------------------"
		Escribir "Ingresar SALIR para salir del programa o CONTINUAR para volver al menú inicial"
		Escribir "------------------------------------------------------------------------------"
		Leer respuesta
		Escribir " "
		
		respuesta <- Mayusculas(respuesta)
		
	Hasta que respuesta = "SALIR"
	
	Escribir "GRACIAS POR ELEGIR RUN AIRLINES !!"

FinProceso
