###############################################################################
# Esto son pruebas de agrupamiento con dplyr
df_train_mini_7semanas <- df_train %>%
  select(-Venta_hoy, -Dev_proxima, -Canal_ID, -Ruta_SAK, -Agencia_ID) %>% 
  group_by(Cliente_ID,Producto_ID) %>% 
  mutate(n=n()) %>% 
  filter(n>1)

df_train_mini_7semanas <- df_train %>%
  select(-Venta_hoy, -Dev_proxima, -Canal_ID, -Ruta_SAK, -Agencia_ID) %>% 
  group_by(Cliente_ID,Producto_ID) %>% 
  summarise(n=n()) %>% 
  filter(n>1)
################################################################################

df_train_7semanas_evolucion <- df_train_backup %>%
  select(-Venta_hoy, -Dev_proxima) %>% 
  group_by(Cliente_ID,Producto_ID) %>% 
  mutate(n=n()) %>% 
  filter(n==7) %>% 
  ungroup()

df_train_7semanas_evolucion$n <- NULL
