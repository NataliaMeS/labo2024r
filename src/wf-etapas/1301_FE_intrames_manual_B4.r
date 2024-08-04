#!/usr/bin/env Rscript

# Workflow  Feature Engineering intrames manual artesanal

# inputs
#  * dataset
# output  
#   un dataset algo mas grande:
#     misma cantidad de registros
#     nuevos atributos construidos en forma artesanal y con mucho sufrimiento
#     generados en codigo R,  especificos para este dataset y clase

# limpio la memoria
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE) # garbage collection

require("data.table")
require("yaml")


#cargo la libreria
# args <- c( "~/labo2024r" )
args <- commandArgs(trailingOnly=TRUE)
source( paste0( args[1] , "/src/lib/action_lib.r" ) )

#------------------------------------------------------------------------------
# Esta es la parte que los alumnos deben desplegar todo su ingenio
# Agregar aqui sus PROPIAS VARIABLES manuales

AgregarVariables_IntraMes <- function(dataset) {
  cat( "inicio AgregarVariables_IntraMes()\n")
  gc()
  # INICIO de la seccion donde se deben hacer cambios con variables nuevas

  # el mes 1,2, ..12
  dataset[, kmes := foto_mes %% 100]

  # creo un ctr_quarter que tenga en cuenta cuando
  # los clientes hace 3 menos de meses que estan
  # ya que seria injusto considerar las transacciones medidas en menor tiempo
  dataset[, ctrx_quarter_normalizado := as.numeric(ctrx_quarter) ]
  dataset[cliente_antiguedad == 1, ctrx_quarter_normalizado := ctrx_quarter * 5]
  dataset[cliente_antiguedad == 2, ctrx_quarter_normalizado := ctrx_quarter * 2]
  dataset[
    cliente_antiguedad == 3,
    ctrx_quarter_normalizado := ctrx_quarter * 1.2
  ]

  # variable extraida de una tesis de maestria de Irlanda
  dataset[, mpayroll_sobre_edad := mpayroll / cliente_edad]

  # se crean los nuevos campos para MasterCard  y Visa,
  #  teniendo en cuenta los NA's
  # varias formas de combinar Visa_status y Master_status
  dataset[, vm_status01 := pmax(Master_status, Visa_status, na.rm = TRUE)]
  dataset[, vm_status02 := Master_status + Visa_status]

  dataset[, vm_status03 := pmax(
    ifelse(is.na(Master_status), 10, Master_status),
    ifelse(is.na(Visa_status), 10, Visa_status)
  )]

  dataset[, vm_status04 := ifelse(is.na(Master_status), 10, Master_status)
    + ifelse(is.na(Visa_status), 10, Visa_status)]

  dataset[, vm_status05 := ifelse(is.na(Master_status), 10, Master_status)
    + 100 * ifelse(is.na(Visa_status), 10, Visa_status)]

  dataset[, vm_status06 := ifelse(is.na(Visa_status),
    ifelse(is.na(Master_status), 10, Master_status),
    Visa_status
  )]

  dataset[, mv_status07 := ifelse(is.na(Master_status),
    ifelse(is.na(Visa_status), 10, Visa_status),
    Master_status
  )]


  # combino MasterCard y Visa
  dataset[, vm_mfinanciacion_limite := rowSums(cbind(Master_mfinanciacion_limite, Visa_mfinanciacion_limite), na.rm = TRUE)]

  dataset[, vm_Fvencimiento := pmin(Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE)]
  dataset[, vm_Finiciomora := pmin(Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE)]
  dataset[, vm_msaldototal := rowSums(cbind(Master_msaldototal, Visa_msaldototal), na.rm = TRUE)]
  dataset[, vm_msaldopesos := rowSums(cbind(Master_msaldopesos, Visa_msaldopesos), na.rm = TRUE)]
  dataset[, vm_msaldodolares := rowSums(cbind(Master_msaldodolares, Visa_msaldodolares), na.rm = TRUE)]
  dataset[, vm_mconsumospesos := rowSums(cbind(Master_mconsumospesos, Visa_mconsumospesos), na.rm = TRUE)]
  dataset[, vm_mconsumosdolares := rowSums(cbind(Master_mconsumosdolares, Visa_mconsumosdolares), na.rm = TRUE)]
  dataset[, vm_mlimitecompra := rowSums(cbind(Master_mlimitecompra, Visa_mlimitecompra), na.rm = TRUE)]
  dataset[, vm_madelantopesos := rowSums(cbind(Master_madelantopesos, Visa_madelantopesos), na.rm = TRUE)]
  dataset[, vm_madelantodolares := rowSums(cbind(Master_madelantodolares, Visa_madelantodolares), na.rm = TRUE)]
  dataset[, vm_fultimo_cierre := pmax(Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE)]
  dataset[, vm_mpagado := rowSums(cbind(Master_mpagado, Visa_mpagado), na.rm = TRUE)]
  dataset[, vm_mpagospesos := rowSums(cbind(Master_mpagospesos, Visa_mpagospesos), na.rm = TRUE)]
  dataset[, vm_mpagosdolares := rowSums(cbind(Master_mpagosdolares, Visa_mpagosdolares), na.rm = TRUE)]
  dataset[, vm_fechaalta := pmax(Master_fechaalta, Visa_fechaalta, na.rm = TRUE)]
  dataset[, vm_mconsumototal := rowSums(cbind(Master_mconsumototal, Visa_mconsumototal), na.rm = TRUE)]
  dataset[, vm_cconsumos := rowSums(cbind(Master_cconsumos, Visa_cconsumos), na.rm = TRUE)]
  dataset[, vm_cadelantosefectivo := rowSums(cbind(Master_cadelantosefectivo, Visa_cadelantosefectivo), na.rm = TRUE)]
  dataset[, vm_mpagominimo := rowSums(cbind(Master_mpagominimo, Visa_mpagominimo), na.rm = TRUE)]

  # a partir de aqui juego con la suma de Mastercard y Visa
  dataset[, vmr_Master_mlimitecompra := Master_mlimitecompra / vm_mlimitecompra]
  dataset[, vmr_Visa_mlimitecompra := Visa_mlimitecompra / vm_mlimitecompra]
  dataset[, vmr_msaldototal := vm_msaldototal / vm_mlimitecompra]
  dataset[, vmr_msaldopesos := vm_msaldopesos / vm_mlimitecompra]
  dataset[, vmr_msaldopesos2 := vm_msaldopesos / vm_msaldototal]
  dataset[, vmr_msaldodolares := vm_msaldodolares / vm_mlimitecompra]
  dataset[, vmr_msaldodolares2 := vm_msaldodolares / vm_msaldototal]
  dataset[, vmr_mconsumospesos := vm_mconsumospesos / vm_mlimitecompra]
  dataset[, vmr_mconsumosdolares := vm_mconsumosdolares / vm_mlimitecompra]
  dataset[, vmr_madelantopesos := vm_madelantopesos / vm_mlimitecompra]
  dataset[, vmr_madelantodolares := vm_madelantodolares / vm_mlimitecompra]
  dataset[, vmr_mpagado := vm_mpagado / vm_mlimitecompra]
  dataset[, vmr_mpagospesos := vm_mpagospesos / vm_mlimitecompra]
  dataset[, vmr_mpagosdolares := vm_mpagosdolares / vm_mlimitecompra]
  dataset[, vmr_mconsumototal := vm_mconsumototal / vm_mlimitecompra]
  dataset[, vmr_mpagominimo := vm_mpagominimo / vm_mlimitecompra]

  # Aqui debe usted agregar sus propias nuevas variables

  dataset[, nvar_2 := mcuentas_saldo/cproductos]
  dataset[, nvar_3 := mrentabilidad/cliente_edad]
  dataset[, nvar_4 := mrentabilidad_annual/cliente_edad]
  dataset[, nvar_5 := mcomisiones/cliente_edad]
  dataset[, nvar_6 := mactivos_margen/cliente_edad]
  dataset[, nvar_7 := mpasivos_margen/cliente_edad]
  dataset[, nvar_8 := mcuenta_corriente_adicional/cliente_edad]
  dataset[, nvar_9 := mcuenta_corriente/cliente_edad]
  dataset[, nvar_10 := mcaja_ahorro/cliente_edad]
  dataset[, nvar_11 := mcaja_ahorro_adicional/cliente_edad]
  dataset[, nvar_12 := mcaja_ahorro_dolares/cliente_edad]
  dataset[, nvar_13 := mcuentas_saldo/cliente_edad]
  dataset[, nvar_14 := mautoservicio/cliente_edad]
  dataset[, nvar_15 := mtarjeta_visa_consumo/cliente_edad]
  dataset[, nvar_16 := mtarjeta_master_consumo/cliente_edad]
  dataset[, nvar_17 := mprestamos_personales/cliente_edad]
  dataset[, nvar_18 := mprestamos_prendarios/cliente_edad]
  dataset[, nvar_19 := mprestamos_hipotecarios/cliente_edad]
  dataset[, nvar_20 := mplazo_fijo_dolares/cliente_edad]
  dataset[, nvar_21 := mplazo_fijo_pesos/cliente_edad]
  dataset[, nvar_22 := minversion1_pesos/cliente_edad]
  dataset[, nvar_23 := minversion1_dolares/cliente_edad]
  dataset[, nvar_24 := minversion2/cliente_edad]
  dataset[, nvar_27 := mcuenta_debitos_automaticos/cliente_edad]
  #dataset[, nvar_28 := mtarjeta_visa_debitos_automaticos/cliente_edad]
  #dataset[, nvar_29 := mttarjeta_master_debitos_automaticos/cliente_edad]
  dataset[, nvar_30 := mpagodeservicios/cliente_edad]
  dataset[, nvar_31 := mpagomiscuentas/cliente_edad]
  dataset[, nvar_32 := mcajeros_propios_descuentos/cliente_edad]
  dataset[, nvar_33 := mtarjeta_visa_descuentos/cliente_edad]
  dataset[, nvar_34 := mtarjeta_master_descuentos/cliente_edad]
  dataset[, nvar_35 := mcomisiones_mantenimiento/cliente_edad]
  dataset[, nvar_36 := mcomisiones_otras/cliente_edad]
  dataset[, nvar_37 := mforex_buy/cliente_edad]
  dataset[, nvar_38 := mforex_sell/cliente_edad]
  dataset[, nvar_39 := mtransferencias_recibidas/cliente_edad]
  dataset[, nvar_40 := mtransferencias_emitidas/cliente_edad]
  dataset[, nvar_41 := mextraccion_autoservicio/cliente_edad]
  dataset[, nvar_42 := mcheques_depositados/cliente_edad]
  dataset[, nvar_43 := mcheques_emitidos/cliente_edad]
  dataset[, nvar_44 := mcheques_depositados_rechazados/cliente_edad]
  dataset[, nvar_45 := mcheques_emitidos_rechazados/cliente_edad]
  dataset[, nvar_46 := matm/cliente_edad]
  dataset[, nvar_47 := matm_other/cliente_edad]
  
  dataset[, nvar_48 := rowSums(cbind(mcaja_ahorro, mcaja_ahorro_adicional), na.rm = TRUE)]
  dataset[, nvar_49 := nvar_47/cliente_edad]
  dataset[, nvar_50 := mcuentas_saldo/mautoservicio]
  dataset[, nvar_51 := mautoservicio/cliente_antiguedad]
  dataset[, nvar_52 := mcuentas_saldo/cliente_antiguedad]
  
  dataset[, nvar_53 := mcuentas_saldo/cliente_antiguedad]
  dataset[, nvar_54 := mrentabilidad/cliente_antiguedad]
  dataset[, nvar_55 := mrentabilidad_annual/cliente_antiguedad]
  dataset[, nvar_56 := mcomisiones/cliente_antiguedad]
  dataset[, nvar_57 := mactivos_margen/cliente_antiguedad]
  dataset[, nvar_58 := mpasivos_margen/cliente_antiguedad]
  dataset[, nvar_59 := mcuenta_corriente_adicional/cliente_antiguedad]
  dataset[, nvar_60 := mcuenta_corriente/cliente_antiguedad]
  dataset[, nvar_61 := mcaja_ahorro/cliente_antiguedad]
  dataset[, nvar_62 := mcaja_ahorro_adicional/cliente_antiguedad]
  dataset[, nvar_63 := mcaja_ahorro_dolares/cliente_antiguedad]
  dataset[, nvar_64 := mcuentas_saldo/cliente_antiguedad]
  dataset[, nvar_65 := mautoservicio/cliente_antiguedad]
  
  dataset[, nvar_66 := mprestamos_personales/(cliente_edad^2)]
  dataset[, nvar_67 := mcuentas_saldo/(cproductos^2)]
  dataset[, nvar_68 := mcaja_ahorro/(cliente_edad^2)]
  dataset[, nvar_69 := mpasivos_margen/(cliente_antiguedad^2)]
  dataset[, nvar_70 := mcuentas_saldo/(cliente_antiguedad^2)]
  
  dataset[, nvar_71 := mrentabilidad/mrentabilidad_annual]
  dataset[, nvar_72 := mrentabilidad_annual/mrentabilidad]
  dataset[, nvar_73 := mrentabilidad_annual/(mrentabilidad^2)]
  dataset[, nvar_74 := mcomisiones/mactivos_margen]
  dataset[, nvar_75 := mactivos_margen/mpasivos_margen]
  dataset[, nvar_76 := mcaja_ahorro/mrentabilidad]
  dataset[, nvar_77 := mcaja_ahorro/(mrentabilidad^2)]
  dataset[, nvar_78 := mcuentas_saldo/mrentabilidad]
  dataset[, nvar_79 := mcuentas_saldo/(mrentabilidad^2)]
  
  dataset[, nvar_80 := mcaja_ahorro/mrentabilidad_annual]
  dataset[, nvar_81 := mcaja_ahorro/(mrentabilidad_annual^2)]
  dataset[, nvar_81 := mcuentas_saldo/mrentabilidad_annual]
  dataset[, nvar_83 := mcuentas_saldo/(mrentabilidad_annual^2)]
  dataset[, nvar_84 := mcuentas_saldo/(mrentabilidad_annual^2)]
  dataset[, nvar_80 := mtransferencias_recibidas/mrentabilidad_annual]
  dataset[, nvar_81 := mtransferencias_recibidas/(mrentabilidad_annual^2)]
  dataset[, nvar_82 := rowSums(cbind(mtransferencias_recibidas, mtransferencias_emitidas), na.rm = TRUE)]
  dataset[, nvar_83 := nvar_82/(mrentabilidad_annual^2)]
  dataset[, nvar_84 := nvar_82/mrentabilidad_annual]
  dataset[, nvar_85 := mcheques_depositados_rechazados/mrentabilidad_annual]
  dataset[, nvar_86 := mcheques_depositados_rechazados/(mrentabilidad_annual^2)]
  dataset[, nvar_87 := mcheques_depositados_rechazados/mrentabilidad]
  dataset[, nvar_88 := mcheques_depositados_rechazados/(mrentabilidad^2)]
  
  
  dataset[, nvar_89 := mactivos_margen/mrentabilidad_annual]
  dataset[, nvar_90 := mactivos_margen/(mrentabilidad_annual^2)]
  dataset[, nvar_91 := mcuenta_corriente_adicional/mrentabilidad_annual]
  dataset[, nvar_92 := mcuenta_corriente_adicional/(mrentabilidad_annual^2)]
  dataset[, nvar_93 := mcuenta_corriente/mrentabilidad_annual]
  dataset[, nvar_94 := mcuenta_corriente/(mrentabilidad_annual^2)]
  
  
  dataset[, nvar_95 := cproductos/mrentabilidad_annual]
  dataset[, nvar_96 := cproductos/(mrentabilidad_annual^2)]
  dataset[, nvar_97 := cproductos/cliente_antiguedad]
  dataset[, nvar_98 := cproductos/cliente_edad]
  
  dataset[, nvar_99 := ccaja_ahorro/mrentabilidad_annual]
  dataset[, nvar_100 := ccaja_ahorro/(mrentabilidad_annual^2)]
  dataset[, nvar_101 := ccaja_ahorro/cliente_antiguedad]
  dataset[, nvar_102 := ccaja_ahorro/cliente_edad]
  
  dataset[, nvar_103 := ccuenta_corriente/mrentabilidad_annual]
  dataset[, nvar_104 := ccuenta_corriente/(mrentabilidad_annual^2)]
  dataset[, nvar_105 := ccuenta_corriente/cliente_antiguedad]
  dataset[, nvar_106 := ccuenta_corriente/cliente_edad]
  
  dataset[, nvar_107 := cdescubierto_preacordado/mrentabilidad_annual]
  dataset[, nvar_108 := cdescubierto_preacordado/(mrentabilidad_annual^2)]
  dataset[, nvar_109 := cdescubierto_preacordado/cliente_antiguedad]
  dataset[, nvar_110 := cdescubierto_preacordado/cliente_edad]
  
  dataset[, nvar_111 := ctarjeta_debito_transacciones/mrentabilidad_annual]
  dataset[, nvar_112 := ctarjeta_debito_transacciones/(mrentabilidad_annual^2)]
  dataset[, nvar_113 := ctarjeta_debito_transacciones/cliente_antiguedad]
  dataset[, nvar_114 := ctarjeta_debito_transacciones/cliente_edad]
  
  dataset[, nvar_115 := cprestamos_personales/mrentabilidad_annual]
  dataset[, nvar_116 := cprestamos_personales/(mrentabilidad_annual^2)]
  dataset[, nvar_117 := cprestamos_personales/cliente_antiguedad]
  dataset[, nvar_118 := cprestamos_personales/cliente_edad]
  
  dataset[, nvar_119 := cprestamos_prendarios/mrentabilidad_annual]
  dataset[, nvar_120 := cprestamos_prendarios/(mrentabilidad_annual^2)]
  dataset[, nvar_121 := cprestamos_prendarios/cliente_antiguedad]
  dataset[, nvar_122 := cprestamos_prendarios/cliente_edad]
  
  dataset[, nvar_123 := cplazo_fijo/mrentabilidad_annual]
  dataset[, nvar_124 := cplazo_fijo/(mrentabilidad_annual^2)]
  dataset[, nvar_125 := cplazo_fijo/cliente_antiguedad]
  dataset[, nvar_126 := cplazo_fijo/cliente_edad]
  
  dataset[, nvar_127 := cprestamos_hipotecarios/mrentabilidad_annual]
  dataset[, nvar_128 := cprestamos_hipotecarios/(mrentabilidad_annual^2)]
  dataset[, nvar_129 := cprestamos_hipotecarios/cliente_antiguedad]
  dataset[, nvar_130 := cprestamos_hipotecarios/cliente_edad]
  
  dataset[, nvar_131 := cinversion1/mrentabilidad_annual]
  dataset[, nvar_132 := cinversion1/(mrentabilidad_annual^2)]
  dataset[, nvar_133 := cinversion1/cliente_antiguedad]
  dataset[, nvar_134 := cinversion1/cliente_edad]
  
  dataset[, nvar_135 := cinversion2/mrentabilidad_annual]
  dataset[, nvar_136 := cinversion2/(mrentabilidad_annual^2)]
  dataset[, nvar_137 := cinversion2/cliente_antiguedad]
  dataset[, nvar_138 := cinversion2/cliente_edad]
  
  dataset[, nvar_139 := cseguro_vida/mrentabilidad_annual]
  dataset[, nvar_140 := cseguro_vida/(mrentabilidad_annual^2)]
  dataset[, nvar_141 := cseguro_vida/cliente_antiguedad]
  dataset[, nvar_142 := cseguro_vida/cliente_edad]
  
  dataset[, nvar_143 := cseguro_auto/mrentabilidad_annual]
  dataset[, nvar_144 := cseguro_auto/(mrentabilidad_annual^2)]
  dataset[, nvar_145 := cseguro_auto/cliente_antiguedad]
  dataset[, nvar_146 := cseguro_auto/cliente_edad]
  
  dataset[, nvar_143 := cseguro_auto/mrentabilidad_annual]
  dataset[, nvar_144 := cseguro_auto/(mrentabilidad_annual^2)]
  dataset[, nvar_145 := cseguro_auto/cliente_antiguedad]
  dataset[, nvar_146 := cseguro_auto/cliente_edad]
  
  dataset[, nvar_147 := cseguro_vivienda/mrentabilidad_annual]
  dataset[, nvar_148 := cseguro_vivienda/(mrentabilidad_annual^2)]
  dataset[, nvar_149 := cseguro_vivienda/cliente_antiguedad]
  dataset[, nvar_150 := cseguro_vivienda/cliente_edad]
  
  dataset[, nvar_151 := cseguro_accidentes_personales/mrentabilidad_annual]
  dataset[, nvar_152 := cseguro_accidentes_personales/(mrentabilidad_annual^2)]
  dataset[, nvar_153 := cseguro_accidentes_personales/cliente_antiguedad]
  dataset[, nvar_154 := cseguro_accidentes_personales/cliente_edad]
  
  dataset[, nvar_155 := ccaja_seguridad/mrentabilidad_annual]
  dataset[, nvar_156 := ccaja_seguridad/(mrentabilidad_annual^2)]
  dataset[, nvar_157 := ccaja_seguridad/cliente_antiguedad]
  dataset[, nvar_158 := ccaja_seguridad/cliente_edad]
  
  dataset[, nvar_159 := cpayroll_trx/mrentabilidad_annual]
  dataset[, nvar_160 := cpayroll_trx/(mrentabilidad_annual^2)]
  dataset[, nvar_161 := cpayroll_trx/cliente_antiguedad]
  dataset[, nvar_162 := cpayroll_trx/cliente_edad]
  
  dataset[, nvar_163 := cpayroll2_trx/mrentabilidad_annual]
  dataset[, nvar_164 := cpayroll2_trx/(mrentabilidad_annual^2)]
  dataset[, nvar_165 := cpayroll2_trx/cliente_antiguedad]
  dataset[, nvar_166 := cpayroll2_trx/cliente_edad]
  
  dataset[, nvar_167 := ccuenta_debitos_automaticos/mrentabilidad_annual]
  dataset[, nvar_168 := ccuenta_debitos_automaticos/(mrentabilidad_annual^2)]
  dataset[, nvar_169 := ccuenta_debitos_automaticos/cliente_antiguedad]
  dataset[, nvar_170 := ccuenta_debitos_automaticos/cliente_edad]
  
  dataset[, nvar_171 := cforex/mrentabilidad_annual]
  dataset[, nvar_172 := cforex/(mrentabilidad_annual^2)]
  dataset[, nvar_173 := cforex/cliente_antiguedad]
  dataset[, nvar_174 := cforex/cliente_edad]
  
  dataset[, nvar_175 := cforex_buy/mrentabilidad_annual]
  dataset[, nvar_176 := cforex_buy/(mrentabilidad_annual^2)]
  dataset[, nvar_177 := cforex_buy/cliente_antiguedad]
  dataset[, nvar_178 := cforex_buy/cliente_edad]
  
  dataset[, nvar_179 := ctransferencias_recibidas/mrentabilidad_annual]
  dataset[, nvar_180 := ctransferencias_recibidas/(mrentabilidad_annual^2)]
  dataset[, nvar_181 := ctransferencias_recibidas/cliente_antiguedad]
  dataset[, nvar_182 := ctransferencias_recibidas/cliente_edad]
  
  dataset[, nvar_183 := ctransferencias_emitidas/mrentabilidad_annual]
  dataset[, nvar_184 := ctransferencias_emitidas/(mrentabilidad_annual^2)]
  dataset[, nvar_185 := ctransferencias_emitidas/cliente_antiguedad]
  dataset[, nvar_186 := ctransferencias_emitidas/cliente_edad]
  
  dataset[, nvar_187 := ccheques_depositados_rechazados/mrentabilidad_annual]
  dataset[, nvar_188 := ccheques_depositados_rechazados/(mrentabilidad_annual^2)]
  dataset[, nvar_189 := ccheques_depositados_rechazados/cliente_antiguedad]
  dataset[, nvar_190 := ccheques_depositados_rechazados/cliente_edad]
  
  dataset[, nvar_191 := ccheques_emitidos_rechazados/mrentabilidad_annual]
  dataset[, nvar_192 := ccheques_emitidos_rechazados/(mrentabilidad_annual^2)]
  dataset[, nvar_193 := ccheques_emitidos_rechazados/cliente_antiguedad]
  dataset[, nvar_194 := ccheques_emitidos_rechazados/cliente_edad]
  
  dataset[, nvar_195 := Visa_cconsumos/mrentabilidad_annual]
  dataset[, nvar_196 := Visa_cconsumos/(mrentabilidad_annual^2)]
  dataset[, nvar_197 := Visa_cconsumos/cliente_antiguedad]
  dataset[, nvar_198 := Visa_cconsumos/cliente_edad]
  
  dataset[, nvar_199 := Master_cconsumos/mrentabilidad_annual]
  dataset[, nvar_200 := Master_cconsumos/(mrentabilidad_annual^2)]
  dataset[, nvar_201 := Master_cconsumos/cliente_antiguedad]
  dataset[, nvar_202 := Master_cconsumos/cliente_edad]
  
  
  
  
  # valvula de seguridad para evitar valores infinitos
  # paso los infinitos a NULOS
  infinitos <- lapply(
    names(dataset),
    function(.name) dataset[, sum(is.infinite(get(.name)))]
  )

  infinitos_qty <- sum(unlist(infinitos))
  if (infinitos_qty > 0) {
    cat(
      "ATENCION, hay", infinitos_qty,
      "valores infinitos en tu dataset. Seran pasados a NA\n"
    )
    dataset[mapply(is.infinite, dataset)] <<- NA
  }


  # valvula de seguridad para evitar valores NaN  que es 0/0
  # paso los NaN a 0 , decision polemica si las hay
  # se invita a asignar un valor razonable segun la semantica del campo creado
  nans <- lapply(
    names(dataset),
    function(.name) dataset[, sum(is.nan(get(.name)))]
  )

  nans_qty <- sum(unlist(nans))
  if (nans_qty > 0) {
    cat(
      "ATENCION, hay", nans_qty,
      "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n"
    )

    cat("Si no te gusta la decision, modifica a gusto el programa!\n\n")
    dataset[mapply(is.nan, dataset)] <<- 0
  }

  cat( "fin AgregarVariables_IntraMes()\n")
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa
cat( "z1301_FE_intrames_manual.r  START\n")
action_inicializar() 


# cargo el dataset donde voy a entrenar
# esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
# cargo el dataset
envg$PARAM$dataset <- paste0( "./", envg$PARAM$input, "/dataset.csv.gz" )
envg$PARAM$dataset_metadata <- read_yaml( paste0( "./", envg$PARAM$input, "/dataset_metadata.yml" ) )

cat( "lectura del dataset\n")
action_verificar_archivo( envg$PARAM$dataset )
cat( "Iniciando lectura del dataset\n" )
dataset <- fread(envg$PARAM$dataset)
cat( "Finalizada lectura del dataset\n" )

GrabarOutput()

# Agrego las variables manuales
cat( "variables intra mest\n")
AgregarVariables_IntraMes(dataset)

#------------------------------------------------------------------------------
# grabo el dataset
cat( "grabado del dataset\n")
cat( "Iniciando grabado del dataset\n" )
fwrite(dataset,
  file = "dataset.csv.gz",
  logical01 = TRUE,
  sep = ","
)
cat( "Finalizado grabado del dataset\n" )


# copia la metadata sin modificar
cat( "grabado de metadata\n")
write_yaml( envg$PARAM$dataset_metadata, 
  file="dataset_metadata.yml" )

#------------------------------------------------------------------------------

# guardo los campos que tiene el dataset
tb_campos <- as.data.table(list(
  "pos" = 1:ncol(dataset),
  "campo" = names(sapply(dataset, class)),
  "tipo" = sapply(dataset, class),
  "nulos" = sapply(dataset, function(x) {
    sum(is.na(x))
  }),
  "ceros" = sapply(dataset, function(x) {
    sum(x == 0, na.rm = TRUE)
  })
))

fwrite(tb_campos,
  file = "dataset.campos.txt",
  sep = "\t"
)

#------------------------------------------------------------------------------
cat( "Fin del programa\n")

envg$OUTPUT$dataset$ncol <- ncol(dataset)
envg$OUTPUT$dataset$nrow <- nrow(dataset)
envg$OUTPUT$time$end <- format(Sys.time(), "%Y%m%d %H%M%S")
GrabarOutput()

#------------------------------------------------------------------------------
# finalizo la corrida
#  archivos tiene a los files que debo verificar existen para no abortar

action_finalizar( archivos = c("dataset.csv.gz","dataset_metadata.yml")) 
cat( "1301_FE_intrames_manual_B4.r  END\n")
