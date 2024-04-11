library(sf)
library(dplyr)



# setor_2022 <- read_sf("/home/joaquim/Documents/nikity_compara/bases/RJ_Malha_Preliminar_2022.gpkg")
setor_2022 <- choose.files(multi = FALSE)

depara <- read_sf(
    # "/home/joaquim/Documents/nikity_compara/bases/Comparabilidade 2022 - 2010 - RJ.xlsx",
    choose.files(multi = FALSE),
    options = "HEADERS=FORCE"
)

dom_2010 <- read_sf(
    # "/home/joaquim/Documents/nikity_compara/bases/Base informaçoes setores2010 universo RJ/EXCEL/Domicilio01_RJ.xls",
    choose.files(multi = FALSE),
    options = "HEADERS=FORCE"
) |>
    select(Cod_setor, V001) |>
    rename(SETOR_2010 = Cod_setor, dom_2010 = V001)

pop_2010 <- read_sf(
    # "/home/joaquim/Documents/nikity_compara/bases/Base informaçoes setores2010 universo RJ/EXCEL/Domicilio02_RJ.xls",
    choose.files(multi = FALSE),
    options = "HEADERS=FORCE"
) |>
    select(Cod_setor, V001) |>
    rename(SETOR_2010 = Cod_setor, pop_2010 = V001)

var_2010 <- dom_2010 |>
    full_join(pop_2010, by = "SETOR_2010")

set_cont <- depara |>
    # filter(substr(SETOR_2022, 1, 7) == "3303302") |>
    mutate(c_2010 = n(), .by = "SETOR_2010") |>
    mutate(c_2022 = n(), .by = "SETOR_2022")

set_2022_manual <- set_cont |>
    filter(c_2010 > 1 & c_2022 > 1)

set_2022_manual <- unique(set_2022_manual$SETOR_2022)

set_2010_manual <- set_cont |>
    filter(c_2010 > 1 & c_2022 > 1)

set_2010_manual <- unique(set_2010_manual$SETOR_2010)

set_auto <- set_cont |>
    filter(!(SETOR_2022 %in% set_2022_manual) & !(SETOR_2010 %in% set_2010_manual))

# depara_manual <- depara |>
# filter(SETOR_2022 %in% set_2022_manual | SETOR_2010 %in% set_2010_manual)



# agg_2010 <- set_auto |>
#     filter(c_2010 > 1) |>
#     summarise()
