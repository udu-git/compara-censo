library(sf)
library(dplyr)


# carrega arquivo com base vetorial e variáveis dos setores de 2022
# setor_2022 <- read_sf("/home/joaquim/Documents/nikity_compara/bases/RJ_Malha_Preliminar_2022.gpkg")
setor_2022 <- read_sf(choose.files(caption = "setores de 2022", multi = FALSE)) |>
    st_cast("MULTIPOLYGON") |>
    st_make_valid() |>
    mutate(SETOR_2022 = substr(CD_SETOR, 1, 15)) |>
    filter(substr(SETOR_2022, 1, 7) == "3303302") |>
    group_by(SETOR_2022, CD_MUN, NM_MUN, v0001, v0002) |>
    summarise()

# seleciona arquivo com camadas de divisões administrativas
arq_adm <- choose.files(caption = "divisões administrativas", multi = FALSE)

# carrega base de regioes de niteroi
regioes <- read_sf(arq_adm, layer = select.list(st_layers(arq_adm)[[1]], title = "base de regiões administrativas")) |>
    st_make_valid() |>
    st_transform(crs = st_crs(setor_2022)) |>
    select(Layer) |>
    rename("Regiao" = "Layer")

# carrega base de bairros de niteroi
bairros <- st_read(choose.files(caption = "base de bairros", multi = FALSE)) |>
    # bairros <- st_read(arq_adm, multi = FALSE, layer = select.list(st_layers(arq_adm)[[1]], title = "base de bairros")) #|>
    st_make_valid() |>
    st_transform(crs = st_crs(setor_2022)) |>
    select(RefName) |>
    rename("Bairro" = "RefName")

# carrega arquivo com tabela de comparabilidade entre setores 2010 x 2022
depara <- read_sf(
    # "/home/joaquim/Documents/nikity_compara/bases/Comparabilidade 2022 - 2010 - RJ.xlsx",
    choose.files(caption = "tabela de:para 2010/2022", multi = FALSE),
    options = "HEADERS=FORCE"
) |>
    distinct()

# carrega variável de domicílios do censo 2010
tab_dom_2010 <- read_sf(
    choose.files(caption = "tabela Domicilio01 de 2010", multi = FALSE),
    options = "HEADERS=FORCE"
) |>
    select(Cod_setor, V001) |>
    rename(SETOR_2010 = Cod_setor, dom_2010 = V001)

# carrega variável de população do censo 2010
tab_pop_2010 <- read_sf(
    choose.files(caption = "tabela Domicilio02 de 2010", multi = FALSE),
    options = "HEADERS=FORCE"
) |>
    select(Cod_setor, V001) |>
    rename(SETOR_2010 = Cod_setor, pop_2010 = V001)

# junta as variáveis do censo de 2010 em uma única tabela e converte o geocodigo para texto
var_2010 <- tab_dom_2010 |>
    full_join(tab_pop_2010, by = "SETOR_2010") |>
    mutate(SETOR_2010 = as.character(SETOR_2010)) |>
    filter(substr(SETOR_2010, 1, 7) == "3303302")

# separa as variáveis e codigo dos setores de 2022
var_2022 <- setor_2022 |>
    as.data.frame() |>
    mutate(
        dom_2022 = v0002,
        pop_2022 = v0001
    ) |>
    select(SETOR_2022, dom_2022, pop_2022)



# quantifica o número de setores da outra faixa temporal análogos ao setor, para 2010 e 2022
set_cont <- depara |>
    # filter(substr(SETOR_2022, 1, 7) == "3303302") |>
    mutate(c_2010 = n(), .by = "SETOR_2010") |>
    mutate(c_2022 = n(), .by = "SETOR_2022")

# separa os casos de múltiplos setores de uma faixa temporal se relacionando a múltiplos setores
# de outra faixa temporal, que não poderão ser processados automaticamente
set_multi <- set_cont |>
    filter(c_2010 > 1 & c_2022 > 1)

# cria lista de setores de 2022 separados para resolução manual
set_2022_manual <- unique(set_multi$SETOR_2022)

# cria lista de setores de 2010 separados para resolução manual
set_2010_manual <- unique(set_multi$SETOR_2010)

# separa setores que poderão ser relacionados automaticamente e cria geocodigo da base compatibilizada
set_auto <- set_cont |>
    filter(!(SETOR_2022 %in% set_2022_manual) & !(SETOR_2010 %in% set_2010_manual)) |>
    mutate(COD_AGR = case_when(
        c_2010 == 1 & c_2022 == 1 ~ paste0(SETOR_2022, "_MANT"),
        c_2010 > 1 ~ paste0(SETOR_2010, "_DIVD"),
        c_2022 > 1 ~ paste0(SETOR_2022, "_UNID")
    ))

# filtra setores para resolução automatica de niteroi
set_auto_nikity <- set_auto |>
    filter(substr(SETOR_2022, 1, 7) == "3303302" | substr(SETOR_2010, 1, 7) == "3303302")

# separa setores que terão que ser relacionados manualmente
set_manual <- set_cont |>
    filter(SETOR_2022 %in% set_2022_manual | SETOR_2010 %in% set_2010_manual)

# filtra setores para resolução manual do município de niterói
set_manual_nikity <- set_manual |>
    filter(substr(SETOR_2022, 1, 7) == "3303302" | substr(SETOR_2010, 1, 7) == "3303302")

# cria lista de setores 2010 de niterói para resolução manual
aval_2010 <- unique(set_manual_nikity$SETOR_2010)

# cria lista de setores 2022 de niterói para resolução manual
aval_2022 <- unique(set_manual_nikity$SETOR_2022)

#######
#######
#######


# salva tabela de:para de niterói com os casos para resolução manual
# write_sf(set_manual_nikity, file.choose(new = TRUE))

# carrega tabela de:para de niterói resolvida manualmente
set_manual_nikity <- read_sf(
    choose.files(caption = "tabela de:para resolvida", multi = FALSE)
)

# junta as tabelas para agregação das variáveis
set_comp_nikity <- rbind(set_auto_nikity, set_manual_nikity)

# carrega as variáves de 2010 e 2022 na tabela e agrega para a base compatibilizada
set_comp_nikity <- set_comp_nikity |>
    left_join(var_2010, by = "SETOR_2010", relationship = "many-to-many") |>
    mutate(
        dom_2010 = case_when(is.na(dom_2010) ~ 0, .default = dom_2010),
        pop_2010 = case_when(is.na(pop_2010) ~ 0, .default = pop_2010)
    ) |>
    left_join(var_2022, by = "SETOR_2022", relationship = "many-to-many")

# agrega os setores de 2010 unificados em 2022
set_agg_2010 <- set_comp_nikity |>
    filter(substr(COD_AGR, 17, 20) == "UNID") |>
    group_by(COD_AGR, SETOR_2022) |>
    summarise(
        dom_2022 = first(dom_2022),
        pop_2022 = first(pop_2022),
        dom_2010 = sum(dom_2010),
        pop_2010 = sum(pop_2010)
    )

# agrega os valores de 2010 dos setores relacionados manualmente
set_agg_man_2010 <- set_comp_nikity |>
    filter(substr(COD_AGR, 17, 18) == "AG") |>
    select(COD_AGR, SETOR_2010, dom_2010, pop_2010) |>
    distinct() |>
    group_by(COD_AGR) |>
    summarise(dom_2010 = sum(dom_2010), pop_2010 = sum(pop_2010))

# retorna os valores agregados de 2010 aos setores relacionados manualmente
set_agg_man <- set_comp_nikity |>
    filter(substr(COD_AGR, 17, 18) == "AG") |>
    select(COD_AGR, SETOR_2022, dom_2022, pop_2022) |>
    left_join(set_agg_man_2010, by = "COD_AGR") |>
    distinct()

# separa os setores cujos valores de 2022 serão agregados ou mantidos
set_agg_2022 <- set_comp_nikity |>
    filter(substr(COD_AGR, 17, 20) %in% c("MANT", "DIVD")) |>
    select(COD_AGR, SETOR_2022, dom_2022, pop_2022, dom_2010, pop_2010)

# junta as tabelas de agregação
agregados <- rbind(set_agg_2010, set_agg_man, set_agg_2022)

# remove colunas desnecessarias na base de 2022 com geometria
setor_2022_final <- setor_2022 |>
    select(SETOR_2022) |>
    left_join(agregados, by = "SETOR_2022")

# agrega os dados de 2022 e associa com regioes e bairros de niteroi
SETOR_FINAL <- setor_2022_final |>
    group_by(COD_AGR) |>
    summarise(
        dom_2010 = first(dom_2010),
        pop_2010 = first(pop_2010),
        dom_2022 = sum(as.numeric(dom_2022)),
        pop_2022 = sum(as.numeric(pop_2022))
    ) |>
    mutate(
        delta_dom = dom_2022 - dom_2010,
        delta_pop = pop_2022 - pop_2010
    ) |>
    st_join(regioes, join = st_intersects, left = TRUE, largest = TRUE) |>
    st_join(bairros, join = st_intersects, left = TRUE, largest = TRUE)

SETOR_FINAL |>
    filter(Bairro == "Icaraí") |>
    View()
# st_write(SETOR_FINAL, file.choose(), layer = "SETOR_FINAL_COMPARACAO")

tabela_bairros <- SETOR_FINAL |>
    as.data.frame() |>
    summarise(
        dom_2010 = sum(dom_2010),
        pop_2010 = sum(pop_2010),
        dom_2022 = sum(dom_2022),
        pop_2022 = sum(pop_2022),
        .by = "Bairro"
    ) |>
    mutate(
        delta_dom = dom_2022 - dom_2010,
        delta_pop = pop_2022 - pop_2010
    )

tabela_regioes <- SETOR_FINAL |>
    as.data.frame() |>
    summarise(
        dom_2010 = sum(dom_2010),
        pop_2010 = sum(pop_2010),
        dom_2022 = sum(dom_2022),
        pop_2022 = sum(pop_2022),
        .by = "Regiao"
    ) |>
    mutate(
        delta_dom = dom_2022 - dom_2010,
        delta_pop = pop_2022 - pop_2010
    )
