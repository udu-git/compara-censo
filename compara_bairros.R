library(sf)
library(dplyr)
library(tidyr)


# carrega arquivo com base vetorial e variáveis dos setores de 2022 e filtra por 3303302 (niteroi)
setor_2022 <-
    read_sf(choose.files(caption = "setores de 2022", multi = FALSE)) |>
    st_make_valid() |>
    mutate(SETOR_2022 = substr(CD_SETOR, 1, 15), dom_2022 = as.numeric(v0002), pop_2022 = as.numeric(v0001)) |>
    filter(substr(SETOR_2022, 1, 7) == "3303302") |>
    st_transform(st_crs(31983))


# carrega arquivo com base vetorial e variáveis dos setores de 2010 e filtra por 3303302 (niteroi)
setor_2010 <-
    read_sf(choose.files(caption = "setores de 2010", multi = FALSE), options = "ENCODING=latin1") |>
    st_cast("MULTIPOLYGON") |>
    st_make_valid() |>
    mutate(SETOR_2010 = CD_GEOCODI) |>
    filter(substr(SETOR_2010, 1, 7) == "3303302") |>
    group_by(SETOR_2010, CD_GEOCODM, NM_MUNICIP) |>
    summarise() |>
    st_transform(st_crs(31983)) |>
    ungroup()


# # seleciona arquivo com camadas de divisões administrativas
# arq_adm <- choose.files(caption = "divisões administrativas", multi = FALSE)


# carrega base de bairros de niteroi
bairros <- st_read(choose.files(caption = "base de bairros", multi = FALSE)) |>
    # bairros <- st_read(arq_adm, multi = FALSE, layer = select.list(st_layers(arq_adm)[[1]], title = "base de bairros")) #|>
    st_make_valid() |>
    st_transform(crs = st_crs(setor_2022)) |>
    mutate(Regiao = case_when(
        str_detect(Layer, "RPB -") ~ "Praias da Baía",
        str_detect(Layer, "RN -") ~ "Norte",
        str_detect(Layer, "RP -") ~ "Pendotiba",
        str_detect(Layer, "RO -") ~ "Oceânica",
        str_detect(Layer, "RL -") ~ "Leste",
    )) |>
    select(RefName, Regiao) |>
    rename("Bairro" = "RefName")


# # carrega base de regioes de niteroi
# regioes <- read_sf(arq_adm, layer = select.list(st_layers(arq_adm)[[1]], title = "base de regiões administrativas")) |>
#     st_make_valid() |>
#     st_transform(crs = st_crs(setor_2022)) |>
#     select(Layer) |>
#     rename("Regiao" = "Layer")


# carrega a base de áreas urbanizadas 2015
area_urb_2015 <-
    read_sf(choose.files(caption = "areas urbanizadas 2015", multi = FALSE)) |>
    st_make_valid() |>
    st_transform(st_crs(31983)) |>
    st_filter(bairros, .predicate = st_intersects) |>
    filter(Tipo == "Área urbanizada")


# carrega a base de áreas urbanizadas 2015
area_urb_2019 <-
    read_sf(choose.files(caption = "areas urbanizadas 2019", multi = FALSE)) |>
    st_make_valid() |>
    st_transform(st_crs(31983)) |>
    st_filter(bairros, .predicate = st_intersects) |>
    filter(Tipo == "Área urbanizada")


# carrega variável de domicílios do censo 2010
tab_dom_2010 <- read_sf(
    choose.files(caption = "tabela Domicilio01 de 2010", multi = FALSE),
    options = "HEADERS=FORCE"
) |>
    select(Cod_setor, V001) |>
    rename(SETOR_2010 = Cod_setor, dom_2010 = V001) |>
    filter(substr(SETOR_2010, 1, 7) == "3303302")


# carrega variável de população do censo 2010
tab_pop_2010 <- read_sf(
    choose.files(caption = "tabela Domicilio02 de 2010", multi = FALSE),
    options = "HEADERS=FORCE"
) |>
    select(Cod_setor, V001) |>
    rename(SETOR_2010 = Cod_setor, pop_2010 = V001) |>
    filter(substr(SETOR_2010, 1, 7) == "3303302")


# junta as variáveis do censo de 2010 em uma única tabela e converte o geocodigo para texto
var_2010 <- tab_dom_2010 |>
    full_join(tab_pop_2010, by = "SETOR_2010") |>
    mutate(SETOR_2010 = as.character(SETOR_2010))


# junta as variáveis à camada de setores 2010
setor_2010 <- setor_2010 |>
    left_join(var_2010, by = "SETOR_2010")

#########


# separa os setores 2010 que iterseccionam areas urbanizadas 2015
setor_2010_urb <-
    st_filter(setor_2010, area_urb_2015, .predicate = st_intersects)


# separa os setores 2010 que não iterseccionam areas urbanizadas 2015
setor_2010_naourb <- setor_2010 |>
    setdiff(setor_2010_urb) |>
    mutate(area_tot = st_area(geometry))


# # separa os setores 2022 que iterseccionam areas urbanizadas 2019
# setor_2022_urb <-
#     st_filter(setor_2022, area_urb_2019, .predicate = st_intersects)


# # separa os setores 2010 que não iterseccionam areas urbanizadas 2015
# setor_2022_naourb <- setor_2022 |>
#     setdiff(setor_2022_urb) |>
#     mutate(area_tot = st_area(geom))


# cira lista de setores 2022 que iterseccionam areas urbanizadas 2019
lista_setor_2022_urb <-
    st_filter(setor_2022, area_urb_2019, .predicate = st_intersects) |>
    pull(unique(SETOR_2022))


# separa os setores 2022 que iterseccionam areas urbanizadas 2019
setor_2022_urb <- setor_2022 |>
    filter(SETOR_2022 %in% lista_setor_2022_urb)


# separa os setores 2022 que não iterseccionam areas urbanizadas 2019
setor_2022_naourb <- setor_2022 |>
    filter(!(SETOR_2022 %in% lista_setor_2022_urb)) |>
    st_cast("MULTIPOLYGON") |>
    group_by(SETOR_2022, CD_MUN, NM_MUN, dom_2022, pop_2022) |>
    summarise() |>
    mutate(area_tot = st_area(geom))


# recorta os setores 2010 com as áreas urbanizadas 2015
setor_2010_rec <- setor_2010_urb |>
    st_intersection(area_urb_2015) |>
    group_by(SETOR_2010, CD_GEOCODM, NM_MUNICIP, dom_2010, pop_2010) |>
    summarise() |>
    mutate(area_tot = st_area(geometry))


# recorta os setores 2022 com as áreas urbanizadas 2019
setor_2022_rec_t <- setor_2022_urb |>
    st_intersection(area_urb_2019) #|>


# extrai os polígonos das feições que geraram geometrycollections
setor_2022_rec_coll <- setor_2022_rec_t %>%
    filter(st_is(., c("GEOMETRYCOLLECTION"))) |>
    st_collection_extract(type = "POLYGON")


# filtra as feições que são polígonos ou multipolígonos, junta com as feições que eram geometrycollections
# e converte em multipolígonos por setor
setor_2022_rec <- setor_2022_rec_t %>%
    filter(st_is(., c("POLYGON", "MULTIPOLYGON"))) |>
    bind_rows(setor_2022_rec_coll) |>
    st_cast("MULTIPOLYGON") |>
    group_by(SETOR_2022, CD_MUN, NM_MUN, dom_2022, pop_2022) |>
    summarise() |>
    mutate(area_tot = st_area(geom))


# reune os setores em area urbana e os fora de area urbana de 2010
setor_2010_full <- setor_2010_rec |>
    bind_rows(setor_2010_naourb)


# intersecciona os setores 2010 com os bairros de niteroi
setor_2010_bairros <- setor_2010_full |>
    st_intersection(bairros) |>
    group_by(SETOR_2010, CD_GEOCODM, NM_MUNICIP, dom_2010, pop_2010, area_tot, Bairro, Regiao) |>
    summarise() |>
    mutate(area_parc = st_area(geometry)) |>
    mutate(dom_2010_parc = dom_2010 * area_parc / area_tot) |>
    mutate(pop_2010_parc = pop_2010 * area_parc / area_tot)


# reune os setores em area urbana e os fora de area urbana de 2022
setor_2022_full <- setor_2022_rec |>
    bind_rows(setor_2022_naourb)


# intersecciona os setores 2022 com os bairros de niteroi
setor_2022_bairros <- setor_2022_full |>
    st_intersection(bairros) |>
    group_by(SETOR_2022, CD_MUN, NM_MUN, dom_2022, pop_2022, area_tot, Bairro) |>
    summarise() |>
    mutate(area_parc = st_area(geom)) |>
    mutate(dom_2022_parc = dom_2022 * area_parc / area_tot) |>
    mutate(pop_2022_parc = pop_2022 * area_parc / area_tot)


# agrega por bairros os dados de 2010
bairros_2010 <- setor_2010_bairros |>
    drop_na() |>
    group_by(Bairro, Regiao) |>
    summarise(
        dom_2010 = round(sum(dom_2010_parc), 0),
        pop_2010 = round(sum(pop_2010_parc), 0)
    ) |>
    as.data.frame()


# agrega por bairros os dados de 2022
bairros_2022 <- setor_2022_bairros |>
    drop_na() |>
    group_by(Bairro) |>
    summarise(
        dom_2022 = round(sum(dom_2022_parc), 0),
        pop_2022 = round(sum(pop_2022_parc), 0)
    ) |>
    as.data.frame()


# junta as bases de 2010 e 2022
bairros_comp <- bairros_2010 |>
    left_join(bairros_2022, by = "Bairro") |>
    mutate(
        delta_dom = dom_2022 - dom_2010,
        delta_pop = pop_2022 - pop_2010
    )


# exporta as tabelas
st_write(bairros_comp, "bairros_comp.ods")
