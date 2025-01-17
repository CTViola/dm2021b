#Necesita para correr en Google Cloud
#64 GB de memoria RAM
#256 GB de espacio en el disco local
#8 vCPU


#Feature Engineering
#creo nuevas variables dentro del mismo mes
#Condimentar a gusto con nuevas variables

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")



#Establezco el Working Directory
setwd( "~/buckets/b1/" )


EnriquecerDataset <- function( dataset , arch_destino )
{
  columnas_originales <-  copy(colnames( dataset ))

  #INICIO de la seccion donde se deben hacer cambios con variables nuevas
  #se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
  #varias formas de combinar Visa_status y Master_status
  
  # Nuestras variables 
  dataset[ , mv_status01   := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
  dataset[ , mv_statustc   := ifelse 
           (pmax( ifelse( is.na(Master_status), 0, Master_status) , ifelse( is.na(Visa_status), 0, Visa_status) ) != 0, 1, 0) ]
  dataset[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos, mpagodeservicios ) , na.rm=TRUE ) ]
  dataset[ , mv_mtransacciones       := ifelse (rowSums 
                                                ( cbind (ctarjeta_debito_transacciones, ctarjeta_visa_transacciones, 
                                                         ctarjeta_master_transacciones, ccallcenter_transacciones, 
                                                         chomebanking_transacciones, ccajas_transacciones ), na.rm=TRUE ) != 0, 1, 0) ]
  dataset[ , mv_mtransacciones       := rowSums ( cbind ( ctarjeta_visa_transacciones, ctarjeta_master_transacciones ), na.rm=TRUE ) ]
  dataset[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
  dataset[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
  dataset[ , mv_usotc                := mv_mpagado / mv_mlimitecompra ]
  dataset[ , madelantopesos_tarjetas     := rowSums( cbind(  Master_madelantopesos,	Master_madelantodolares,	Visa_madelantopesos,	Visa_madelantodolares) , na.rm=TRUE ) ]
  dataset[ , flag_operaciones_caja       := ifelse (rowSums ( cbind ( ccajas_transacciones,	ccajas_consultas,
                                                                      ccajas_depositos,	ccajas_extracciones,	ccajas_otras)
                                                                      , na.rm=TRUE ) != 0, 0, 1) ]
  dataset[ , ratio_cheques_depo          := ccheques_depositados_rechazados  / ccheques_depositados ]
  dataset[ , monto_total_cajas_ahorro    := rowSums( cbind(  mcaja_ahorro,	mcaja_ahorro_adicional,mcaja_ahorro_dolares ) , na.rm=TRUE ) ]
  dataset[ , ctarjetas_deb_auto          := rowSums( cbind(  ctarjeta_visa_debitos_automaticos,	ctarjeta_master_debitos_automaticos) , na.rm=TRUE ) ]
  ##dataset[ , mtarjetas_deb_auto          := rowSums( cbind(  mtarjeta_visa_debitos_automaticos,	mttarjeta_master_debitos_automaticos) , na.rm=TRUE ) ]
  dataset[ , ctotal_debitos_automaticos  := rowSums( cbind(  ccuenta_debitos_automaticos,	ctarjeta_visa_debitos_automaticos, ctarjeta_master_debitos_automaticos ) , na.rm=TRUE ) ]
  ##dataset[ , dtotal_debitos_automaticos  := rowSums( cbind(  mcuenta_debitos_automaticos,	mtarjeta_visa_debitos_automaticos, mttarjeta_master_debitos_automaticos ) , na.rm=TRUE ) ]
  dataset[ , mtarjeta_descuentos         := rowSums( cbind(  mtarjeta_visa_descuentos,	mtarjeta_master_descuentos) , na.rm=TRUE ) ]
  dataset[ , ctarjeta_descuentos         := rowSums( cbind(  ctarjeta_visa_descuentos,	ctarjeta_master_descuentos) , na.rm=TRUE ) ]
  dataset[ , mtotal_descuentos           := rowSums( cbind(  mcajeros_propios_descuentos,	mtarjeta_visa_descuentos, mtarjeta_master_descuentos) , na.rm=TRUE ) ]
  dataset[ , ctotal_descuentos           := rowSums( cbind(  ccajeros_propios_descuentos,	ctarjeta_visa_descuentos, ctarjeta_master_descuentos ) , na.rm=TRUE ) ]
  dataset[ , ctotal_inversiones          := rowSums( cbind(  cinversion1,	cinversion2) , na.rm=TRUE ) ]
  dataset[ , mtotal_inversiones          := rowSums( cbind(  minversion1_pesos,	minversion1_dolares, minversion2 ) , na.rm=TRUE ) ]
  dataset[ , m_haberes                   := rowSums( cbind(  mpayroll,	mpayroll2) , na.rm=TRUE ) ]
  dataset[ , ratio_inversiones           := mtotal_inversiones  / m_haberes ]			
  dataset[ , c_haberes                   := rowSums( cbind(  cpayroll_trx,	cpayroll2_trx) , na.rm=TRUE ) ]
  dataset[ , ctotal_seguros              := rowSums( cbind(cseguro_vida,	cseguro_auto, cseguro_vivienda, cseguro_accidentes_personales) , na.rm=TRUE ) ]
  dataset[ , ctotal_comisiones           := rowSums( cbind(  ccomisiones_mantenimiento,	ccomisiones_otras) , na.rm=TRUE ) ]
  dataset[ , min_finiciomora             := rowSums( cbind(  Master_Finiciomora,	Visa_Finiciomora) , na.rm=TRUE ) ]
  
  
  
  # Variables de Gustavo
 
  dataset[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]
  dataset[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
  dataset[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
  dataset[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
  dataset[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
  dataset[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
  dataset[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
  dataset[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
  dataset[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
  dataset[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
  dataset[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
  dataset[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
  dataset[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
  dataset[ , mv_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
  dataset[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]
  dataset[ , mv_status02   := Master_status +  Visa_status ]
  dataset[ , mv_status05   := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ]
  dataset[ , mv_status06   := ifelse( is.na(Visa_status), ifelse( is.na(Master_status), 10, Master_status), Visa_status)  ]
  dataset[ , mv_status07   := ifelse( is.na(Master_status), ifelse( is.na(Visa_status), 10, Visa_status), Master_status)  ]
  dataset[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
  dataset[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
  dataset[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
  dataset[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
  dataset[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
  dataset[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
  dataset[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
  dataset[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
  dataset[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
  dataset[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
  dataset[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
  dataset[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
  dataset[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
  dataset[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
  dataset[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]

  #valvula de seguridad para evitar valores infinitos
  #paso los infinitos a NULOS
  infinitos      <- lapply(names(dataset),function(.name) dataset[ , sum(is.infinite(get(.name)))])
  infinitos_qty  <- sum( unlist( infinitos) )
  if( infinitos_qty > 0 )
  {
    cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
    dataset[mapply(is.infinite, dataset)] <- NA
  }


  #valvula de seguridad para evitar valores NaN  que es 0/0
  #paso los NaN a 0 , decision polemica si las hay
  #se invita a asignar un valor razonable segun la semantica del campo creado
  nans      <- lapply(names(dataset),function(.name) dataset[ , sum(is.nan(get(.name)))])
  nans_qty  <- sum( unlist( nans) )
  if( nans_qty > 0 )
  {
    cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
    cat( "Si no te gusta la decision, modifica a gusto el programa!\n\n")
    dataset[mapply(is.nan, dataset)] <- 0
  }

  #FIN de la seccion donde se deben hacer cambios con variables nuevas

  columnas_extendidas <-  copy( setdiff(  colnames(dataset), columnas_originales ) )

  #grabo con nombre extendido
  fwrite( dataset,
          file=arch_destino,
          sep= "," )
}
#------------------------------------------------------------------------------

dir.create( "./datasets/" )

#lectura rapida del dataset  usando fread  de la libreria  data.table
dataset1  <- fread("./datasetsOri/paquete_premium.csv.gz")

EnriquecerDataset( dataset1, "./datasets/paquete_premium_ext.csv.gz" )

quit( save="no")
