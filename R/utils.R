processing_strava = function(metadata, 
                             shape,
                             dateVar
                             ) {
    rides = copy(metadata)
    ruas = copy(shape)
    #

    # 1. somando forward_* e backward_*:
    rides = rides[, .(
            edge_uid,
            osm_reference_id,
            dateVar = get(dateVar),
            trip_count = forward_trip_count + reverse_trip_count,
            people_count = forward_people_count + forward_people_count,
            commute_trip_count = forward_commute_trip_count + reverse_commute_trip_count,
            leisure_trip_count = forward_leisure_trip_count + reverse_leisure_trip_count,
            morning_trip_count = forward_morning_trip_count + reverse_morning_trip_count,
            evening_trip_count = forward_evening_trip_count + reverse_evening_trip_count,
            male_people_count = forward_male_people_count + reverse_male_people_count,
            female_people_count = forward_female_people_count + reverse_female_people_count,
            unspecified_people_count = forward_unspecified_people_count + reverse_unspecified_people_count,
            age_13_19_people_count = forward_13_19_people_count + reverse_13_19_people_count,
            age_20_34_people_count = forward_20_34_people_count + reverse_20_34_people_count,
            age_35_54_people_count = forward_35_54_people_count + reverse_35_54_people_count,
            age_55_64_people_count = forward_55_64_people_count + reverse_55_64_people_count,
            age_65_plus_people_count = forward_65_plus_people_count + reverse_65_plus_people_count,
            # take the avg of speed:
            average_speed = (forward_average_speed + reverse_average_speed)/2
        )
    ]
    setorderv(rides, c("edge_uid", "osm_reference_id", "dateVar"))
    rides
    
    # tirando a média mensal:
    rides = rides[, map(.SD, mean), 
                  by = .(edge_uid, osm_reference_id), 
                  .SDcols = setdiff(names(rides), c("dateVar", "edge_uid", "osm_reference_id"))]
    
    # Categorizando variáveis de tráfego:
    
    # trip count
    q1 = quantile(rides[trip_count>0]$trip_count, c(.33))
    q2 = quantile(rides[trip_count>0]$trip_count, c(.66))
    rides[, trip_count_cat := case_when(trip_count < q1 ~ "baixo",
                                        trip_count < q2 ~ "médio",
                                        TRUE  ~ "alto")]
    rides[trip_count == 0, trip_count_cat := NA]
    rides[, trip_count_cat := factor(trip_count_cat, levels = c("baixo", "médio", "alto"))]
    
    # people count:
    q1 = quantile(rides[people_count>0]$people_count, c(.33))
    q2 = quantile(rides[people_count>0]$people_count, c(.66))
    rides[, people_count_cat := case_when(people_count < q1 ~ "baixo",
                                          people_count < q2 ~ "médio",
                                          TRUE  ~ "alto")]
    rides[people_count == 0, people_count_cat := NA]
    rides[, people_count_cat := factor(people_count_cat, levels = c("baixo", "médio", "alto"))]
    
    # commute trip count
    q1 = quantile(rides[commute_trip_count > 0]$commute_trip_count, c(.33))
    q2 = quantile(rides[commute_trip_count > 0]$commute_trip_count, c(.66))
    rides[, commute_trip_count_cat := case_when(commute_trip_count < q1 ~ "baixo",
                                                commute_trip_count < q2 ~ "médio",
                                                TRUE  ~ "alto")]
    rides[commute_trip_count == 0, commute_trip_count_cat := NA]
    rides[, commute_trip_count_cat := factor(commute_trip_count_cat, levels = c("baixo", "médio", "alto"))]
    
    # leisure trip count
    q1 = quantile(rides[leisure_trip_count>0]$leisure_trip_count, c(.33))
    q2 = quantile(rides[leisure_trip_count>0]$leisure_trip_count, c(.66))
    rides[, leisure_trip_count_cat := case_when(leisure_trip_count < q1 ~ "baixo",
                                                leisure_trip_count < q2 ~ "médio",
                                                TRUE  ~ "alto")]
    rides[leisure_trip_count == 0, leisure_trip_count_cat := NA]
    rides[, leisure_trip_count_cat := factor(leisure_trip_count_cat, levels = c("baixo", "médio", "alto"))]
    
    # morning trip count
    q1 = quantile(rides[morning_trip_count>0]$morning_trip_count, c(.33))
    q2 = quantile(rides[morning_trip_count>0]$morning_trip_count, c(.66))
    rides[, morning_trip_count_cat := case_when(morning_trip_count < q1 ~ "baixo",
                                                morning_trip_count < q2 ~ "médio",
                                                TRUE ~ "alto")]
    rides[morning_trip_count == 0, morning_trip_count_cat := NA]
    rides[, morning_trip_count_cat := factor(morning_trip_count_cat, levels = c("baixo", "médio", "alto"))]
    
    # evening trip count:
    q1 = quantile(rides[evening_trip_count>0]$evening_trip_count, c(.33))
    q2 = quantile(rides[evening_trip_count>0]$evening_trip_count, c(.66))
    rides[, evening_trip_count_cat := case_when(evening_trip_count < q1 ~ "baixo",
                                                evening_trip_count < q2 ~ "médio",
                                                TRUE ~ "alto")]
    rides[evening_trip_count == 0, evening_trip_count_cat := NA]
    rides[, evening_trip_count_cat := factor(evening_trip_count_cat, levels = c("baixo", "médio", "alto"))]
    
    # male people count
    q1 = quantile(rides[male_people_count>0]$male_people_count, c(.33))
    q2 = quantile(rides[male_people_count>0]$male_people_count, c(.66))
    rides[, male_people_count_cat := case_when(male_people_count < q1 ~ "baixo",
                                               male_people_count < q2 ~ "médio",
                                               TRUE ~ "alto")]
    rides[male_people_count == 0, male_people_count_cat := NA]
    rides[, male_people_count_cat := factor(male_people_count_cat, levels = c("baixo", "médio", "alto"))]
    
    # female people count
    q1 = quantile(rides[female_people_count>0]$female_people_count, c(.33))
    q2 = quantile(rides[female_people_count>0]$female_people_count, c(.66))
    rides[, female_people_count_cat := case_when(female_people_count < q1 ~ "baixo",
                                                 female_people_count < q2 ~ "médio",
                                                 TRUE ~ "alto")]
    rides[female_people_count == 0, female_people_count_cat := NA]
    rides[, female_people_count_cat := factor(female_people_count_cat, levels = c("baixo", "médio", "alto"))]
    
    # age 13-19 
    q1 = quantile(rides[age_13_19_people_count>0]$age_13_19_people_count, c(.33))
    q2 = quantile(rides[age_13_19_people_count>0]$age_13_19_people_count, c(.66))
    rides[, age_13_19_people_count_cat := case_when(age_13_19_people_count < q1 ~ "baixo",
                                                    age_13_19_people_count < q2 ~ "médio",
                                                    TRUE ~ "alto")]
    rides[age_13_19_people_count == 0, age_13_19_people_count_cat := NA]
    rides[, age_13_19_people_count_cat := factor(age_13_19_people_count_cat, levels = c("baixo", "médio", "alto"))]
    
    # age 20-34
    q1 = quantile(rides[age_20_34_people_count>0]$age_20_34_people_count, c(.33))
    q2 = quantile(rides[age_20_34_people_count>0]$age_20_34_people_count, c(.66))
    rides[, age_20_34_people_count_cat := case_when(age_20_34_people_count < q1 ~ "baixo",
                                                    age_20_34_people_count < q2 ~ "médio",
                                                    TRUE ~ "alto")]
    rides[age_20_34_people_count == 0, age_20_34_people_count_cat := NA]
    rides[, age_20_34_people_count_cat := factor(age_20_34_people_count_cat, levels = c("baixo", "médio", "alto"))]
    
    # age 35-54
    q1 = quantile(rides[age_35_54_people_count>0]$age_35_54_people_count, c(.33))
    q2 = quantile(rides[age_35_54_people_count>0]$age_35_54_people_count, c(.66))
    rides[, age_35_54_people_count_cat := case_when(age_35_54_people_count < q1 ~ "baixo",
                                                    age_35_54_people_count < q2 ~ "médio",
                                                    TRUE ~ "alto")]
    rides[age_35_54_people_count == 0, age_35_54_people_count_cat := NA]
    rides[, age_35_54_people_count_cat := factor(age_35_54_people_count_cat, levels = c("baixo", "médio", "alto"))]
    
    # age 55-64
    q1 = quantile(rides[age_55_64_people_count>0]$age_55_64_people_count, c(.33))
    q2 = quantile(rides[age_55_64_people_count>0]$age_55_64_people_count, c(.66))
    rides[, age_55_64_people_count_cat := case_when(age_55_64_people_count < q1 ~ "baixo",
                                                    age_55_64_people_count < q2 ~ "médio",
                                                    TRUE  ~ "alto")]
    rides[age_55_64_people_count==0, age_55_64_people_count_cat := NA]
    rides[, age_55_64_people_count_cat := factor(age_55_64_people_count_cat, levels = c("baixo", "médio", "alto"))]
    
    # age 65+
    q1 = quantile(rides[age_65_plus_people_count>0]$age_65_plus_people_count, c(.33))
    q2 = quantile(rides[age_65_plus_people_count>0]$age_65_plus_people_count, c(.66))
    rides[, age_65_plus_people_count_cat := case_when(age_65_plus_people_count < q1 ~ "baixo",
                                                      age_65_plus_people_count < q2 ~ "médio",
                                                      TRUE ~ "alto")]
    rides[age_65_plus_people_count==0, age_65_plus_people_count_cat := NA]
    rides[, age_65_plus_people_count_cat := factor(age_65_plus_people_count_cat, levels = c("baixo", "médio", "alto"))]
    rides
    
    # convert to DT:
    setDT(ruas)
    
    # 2. merge to associate edge-osm ID to its geometry
    rides = merge(
        rides, 
        ruas, 
        by.x = c("edge_uid", "osm_reference_id"), 
        by.y = c("edgeUID", "osmId")
    )
    
    # 3. merge with rides by osm_id to associate the street name:
    recife_lines = readRDS("data/recife_lines.rds")
    rides = merge(
        rides,
        recife_lines[, .(osm_id, name)],
        by.x = "osm_reference_id",
        by.y = "osm_id",
        all.x = T
    )
    setcolorder(rides, c("name", "osm_reference_id", "edge_uid"))
    rides
}



spatialMerge_stravaMalhaPermanente <- function(strava,
                                               malhaPermanente) {
    ## Identificar trechos do strava com cobertura de malha cicloviária
    
    ## dois argumentos: 
    ##  1. malhaPermanente
    ##  2. strava formatado
    rides = copy(strava)
    
    # retorno:
    # base do strava com flag de cobertura.
    
    # spatial merge to associate edgeUID and osmId to malha permanente
    malhaPermanente2 = st_join(
        st_as_sf(rides[, .(osm_reference_id, edge_uid, name, geometry)]),
        st_buffer(st_as_sf(malhaPermanente), dist = 15),
        join = st_is_within_distance,
        left= F,
        dist = 5
    )
    setDT(malhaPermanente2)
    malhaPermanente2
    
    # merge to identify paths covered by cycleway
    foo = merge(
        rides |> dplyr::select(edge_uid, osm_reference_id),
        malhaPermanente2 |> dplyr::select(-geometry, -name),
        all.x = T,
        by= c("edge_uid", "osm_reference_id")
    )
    
    # flag to identify paths covered by cycleway:
    foo[, flag := ifelse(is.na(Nome), "Não Cobertura", "Cobertura")]
    foo[, c("Nome", "Tipo", "Sentido", "Bairro", "Logradouro") := NULL]
    foo = unique(foo)
    foo # flag de cobertura para cada trecho
    
    rides = merge(rides, 
                  foo, 
                  by = c("edge_uid", "osm_reference_id"))
    rides
}


spatialMerge_stravaMalhaPDC <- function(strava,
                                        malhaPDC) {
    ## Identificar trechos do strava com cobertura de malha cicloviária
    
    ## dois argumentos: 
    ##  1. malhaPermanente
    ##  2. strava formatado
    rides = copy(strava)
    
    # retorno:
    # base do strava com flag de cobertura.
    
    # spatial merge to associate edgeUID and osmId to malha permanente
    malhaBuffered = st_join(
        st_as_sf(rides[, .(osm_reference_id, edge_uid, name, geometry)]),
        st_buffer(st_as_sf(malhaPDC), dist = 15),
        join = st_is_within_distance,
        left= F,
        dist = 5
    )
    setDT(malhaBuffered)
    malhaBuffered
    
    # merge to identify paths covered by cycleway
    foo = merge(
        rides |> dplyr::select(edge_uid, osm_reference_id),
        malhaBuffered |> dplyr::select(-geometry, -name),
        all.x = T,
        by= c("edge_uid", "osm_reference_id")
    )
    
    # flag to identify paths covered by cycleway:
    foo[, flag_PDC := ifelse(is.na(Name), "Não Cobertura", "Cobertura")]
    foo[, c("Name", "Description") := NULL]
    foo = unique(foo)
    foo # flag de cobertura para cada trecho
    
    rides = merge(rides, 
                  foo, 
                  by = c("edge_uid", "osm_reference_id"))
    rides
}


# Função para calcular a interseção entre a malha Permanente e Operacional:
calcIntersectionMalhas <- function(malhaPermanente, 
                                   malhaOperacional
                                   ) {
    res = st_intersection(
        malhaPermanente,
        st_buffer(malhaOperacional, dist = 5)
    )
    
    res = res %>% mutate(length = as.numeric(st_length(geometry)))
    res
}


computeStatsTrafego = function(rides, nome_variavel) {
    # ARGUMENTOS:
    # rides
    # nome_variavel = "trip_count_cat"
    rides = copy(rides)
    
    # Nome das ruas sem cobertura de ciclofaixa:
    # tráfego alto
    nomeRuas_trafegoAlto = rides[flag == "Não Cobertura" & get(nome_variavel) == "alto", .N, name][order(-N)] %>% 
        drop_na(name) %>% 
        pull(name)
    
    # tráfego médio
    nomeRuas_trafegoMedio = rides[flag == "Não Cobertura" & get(nome_variavel) == "médio", .N, name][order(-N)] %>% 
        drop_na(name) %>% 
        filter(!name %in% nomeRuas_trafegoAlto) %>% 
        pull(name)
    
    # tráfego baixo
    nomeRuas_trafegoBaixo = rides[flag == "Não Cobertura" & get(nome_variavel) == "baixo", .N, name][order(-N)] %>% 
        drop_na(name) %>% 
        filter(!name %in% c(nomeRuas_trafegoAlto, nomeRuas_trafegoMedio)) %>% 
        pull(name)
    
    # Percentural de ruas sem cobertura por nível tráfego:
    # baixo
    percTrafegoBaixo <- rides[get(nome_variavel) == "baixo", 
                              .N, 
                              c(nome_variavel, "flag")][, prop := N/sum(N)][flag == "Não Cobertura", prop]
    percTrafegoBaixo <- round(percTrafegoBaixo, 4)
    
    # médio
    percTrafegoMedio <- rides[get(nome_variavel) == "médio", 
                              .N, 
                              c(nome_variavel, "flag")][, prop := N/sum(N)][flag == "Não Cobertura", prop]
    percTrafegoMedio <- round(percTrafegoMedio, 4)
    
    # alto
    percTrafegoAlto <- rides[get(nome_variavel) == "alto", 
                             .N, 
                             c(nome_variavel, "flag")][, prop := N/sum(N)][flag == "Não Cobertura", prop]
    percTrafegoAlto <- round(percTrafegoAlto, 4)
    
    
    # COM PREVISAO DO PDC:
    # Nome das ruas sem cobertura de ciclofaixa:
    # tráfego alto
    nomeRuas_trafegoAlto_CPDC = rides[flag == "Não Cobertura" & flag_PDC == "Cobertura" & get(nome_variavel) == "alto", .N, name][order(-N)] %>% 
        drop_na(name) %>% 
        pull(name)
    
    # tráfego médio
    nomeRuas_trafegoMedio_CPDC = rides[flag == "Não Cobertura" & flag_PDC == "Cobertura" & get(nome_variavel) == "médio", .N, name][order(-N)] %>% 
        drop_na(name) %>% 
        filter(!name %in% nomeRuas_trafegoAlto_CPDC) %>% 
        pull(name)
    
    # tráfego baixo
    nomeRuas_trafegoBaixo_CPDC = rides[flag == "Não Cobertura" & flag_PDC == "Cobertura" & get(nome_variavel) == "baixo", .N, name][order(-N)] %>% 
        drop_na(name) %>% 
        filter(!name %in% c(nomeRuas_trafegoAlto_CPDC, nomeRuas_trafegoMedio_CPDC)) %>% 
        pull(name)
    
    # Percentural de ruas sem cobertura por nível tráfego:
    # baixo
    percTrafegoBaixo_CPDC <- rides[get(nome_variavel) == "baixo", 
                              .N, 
                              c(nome_variavel, "flag", "flag_PDC")][, prop := N/sum(N)][flag == "Não Cobertura" & flag_PDC == "Cobertura", prop]
    percTrafegoBaixo_CPDC <- round(percTrafegoBaixo_CPDC, 4)
    
    # médio
    percTrafegoMedio_CPDC <- rides[get(nome_variavel) == "médio", 
                              .N, 
                              c(nome_variavel, "flag", "flag_PDC")][, prop := N/sum(N)][flag == "Não Cobertura" & flag_PDC == "Cobertura", prop]
    percTrafegoMedio_CPDC <- round(percTrafegoMedio_CPDC, 4)
    
    # alto
    percTrafegoAlto_CPDC <- rides[get(nome_variavel) == "alto", 
                             .N, 
                             c(nome_variavel, "flag", "flag_PDC")][, prop := N/sum(N)][flag == "Não Cobertura" & flag_PDC == "Cobertura", prop]
    percTrafegoAlto_CPDC <- round(percTrafegoAlto_CPDC, 4)
    
    
    # SEM PREVISAO DO PDC:
    # Nome das ruas sem cobertura de ciclofaixa:
    # tráfego alto
    nomeRuas_trafegoAlto_SPDC = rides[flag == "Não Cobertura" & flag_PDC == "Não Cobertura" & get(nome_variavel) == "alto", .N, name][order(-N)] %>% 
        drop_na(name) %>% 
        pull(name)
    
    # tráfego médio
    nomeRuas_trafegoMedio_SPDC = rides[flag == "Não Cobertura" & flag_PDC == "Não Cobertura" & get(nome_variavel) == "médio", .N, name][order(-N)] %>% 
        drop_na(name) %>% 
        filter(!name %in% nomeRuas_trafegoAlto_SPDC) %>% 
        pull(name)
    
    # tráfego baixo
    nomeRuas_trafegoBaixo_SPDC = rides[flag == "Não Cobertura" & flag_PDC == "Não Cobertura" & get(nome_variavel) == "baixo", .N, name][order(-N)] %>% 
        drop_na(name) %>% 
        filter(!name %in% c(nomeRuas_trafegoAlto_SPDC, nomeRuas_trafegoMedio_SPDC)) %>% 
        pull(name)
    
    # Percentural de ruas sem cobertura por nível tráfego:
    # baixo
    percTrafegoBaixo_SPDC <- rides[get(nome_variavel) == "baixo", 
                              .N, 
                              c(nome_variavel, "flag", "flag_PDC")][, prop := N/sum(N)][flag == "Não Cobertura" & flag_PDC == "Não Cobertura", prop]
    percTrafegoBaixo_SPDC <- round(percTrafegoBaixo_SPDC, 4)
    
    # médio
    percTrafegoMedio_SPDC <- rides[get(nome_variavel) == "médio", 
                              .N, 
                              c(nome_variavel, "flag", "flag_PDC")][, prop := N/sum(N)][flag == "Não Cobertura" & flag_PDC == "Não Cobertura", prop]
    percTrafegoMedio_SPDC <- round(percTrafegoMedio_SPDC, 4)
    
    # alto
    percTrafegoAlto_SPDC <- rides[get(nome_variavel) == "alto", 
                             .N, 
                             c(nome_variavel, "flag", "flag_PDC")][, prop := N/sum(N)][flag == "Não Cobertura" & flag_PDC == "Não Cobertura", prop]
    percTrafegoAlto_SPDC <- round(percTrafegoAlto_SPDC, 4)
    
    
    # checking
    percTrafegoAlto 
    percTrafegoAlto_CPDC + percTrafegoAlto_SPDC
    
    percTrafegoMedio 
    percTrafegoMedio_CPDC + percTrafegoMedio_SPDC
    
    percTrafegoBaixo
    percTrafegoBaixo_CPDC + percTrafegoBaixo_SPDC
    
    setdiff(nomeRuas_trafegoAlto, c(nomeRuas_trafegoAlto_CPDC, nomeRuas_trafegoAlto_SPDC))
    setdiff(nomeRuas_trafegoMedio, c(nomeRuas_trafegoMedio_CPDC, nomeRuas_trafegoMedio_SPDC))
    setdiff(nomeRuas_trafegoBaixo, c(nomeRuas_trafegoBaixo_CPDC, nomeRuas_trafegoBaixo_SPDC))
    
    # output:
    out = list(
        trafegoBaixo = list(
            ruas = list(
                all = nomeRuas_trafegoBaixo,
                CPDC = nomeRuas_trafegoBaixo_CPDC,
                SPDC = nomeRuas_trafegoBaixo_SPDC
            ),
            percentual = list(
                all = percTrafegoBaixo,
                CPDC = percTrafegoBaixo_CPDC,
                SPDC = percTrafegoBaixo_SPDC
            )
        ),
        trafegoMedio = list(
            ruas = list(
                all = nomeRuas_trafegoMedio,
                CPDC = nomeRuas_trafegoMedio_CPDC,
                SPDC = nomeRuas_trafegoMedio_SPDC
            ),
            percentual = list(
                all = percTrafegoMedio,
                CPDC = percTrafegoMedio_CPDC,
                SPDC = percTrafegoMedio_SPDC
            )
        ),
        trafegoAlto = list(
            ruas = list(
                all = nomeRuas_trafegoAlto,
                CPDC = nomeRuas_trafegoAlto_CPDC,
                SPDC = nomeRuas_trafegoAlto_SPDC
            ),
            percentual = list(
                all = percTrafegoAlto,
                CPDC = percTrafegoAlto_CPDC,
                SPDC = percTrafegoAlto_SPDC
            )
        )
    )
    
    return(out)

}