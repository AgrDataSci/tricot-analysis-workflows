######################################################
#David Brown WUR
#
library(dplyr)

#'references
#'
#'BITA-2 = TMBx 5295-1
#'Ortiz, R., & Vuylsteke, D. (1998). `BITA-3': 
#'A Starchy Banana with Partial Resistance to Black Sigatoka and 
#'Tolerance to Streak Virus. HortScience HortSci, 33(2), 358-359. https://doi.org/10.21273/hortsci.33.2.358 
#'
#'PITA-16 = TMB3x 15108-6
#'https://www.promusa.org/PITA-16
#'
#'PV 03-44 = EMB 402
#'https://www.musalit.org/viewPdf.php?file=IN050615_eng.pdf&id=9041



#standardize genotype names
correct_names <- function(input_df){
  corrected_names <- input_df %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "Calcutta4", "Calcutta 4")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "Agbagya", "Agbagba")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "Agbagba (control)", "Agbagba")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "Cardava", "Cardaba")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "SH3640", "SH-3640")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "SH34369", "SH-3436-9")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "GrandNaine", "Grande Naine")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "Local cv. (Grande Naine)", "Grande Naine")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "GrandeNaine", "Grande Naine")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "Grand Nain", "Grande Naine")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "YangambiKm5", "Yangambi Km5")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "Yangambi Km 5", "Yangambi Km5")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "Yangambi km 5", "Yangambi Km5")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "WilliamsBell", "Williams")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "Williams (Bell, South Johnstone)", "Williams")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "TieuLun", "Chuoi Tieu Lun")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "GrosMichel", "Gros Michel")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "Gros michel", "Gros Michel")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "CurrareEnano", "Currare Enano")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "FalsoCuerno", "Falso Cuerno")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "CRBP 39", "CRBP-39")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "CRBP39", "CRBP-39")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "PisangJariBuaya", "Pisang Jari Buaya")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "Pisang mas", "Pisang Mas")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "PisangLemakManis", "Pisang Lemak Manis")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "FHIA25", "FHIA-25")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "FHIA23", "FHIA-23")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "FHIA21", "FHIA-21")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "FHIA-21 (#68)", "FHIA-21")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "FHIA21no68", "FHIA-21")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "FHIA20", "FHIA-20")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "FHIA18", "FHIA-18")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "Local cv. (FHIA-18)", "FHIA-18")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "FHIA17", "FHIA-17")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "FHIA01", "FHIA-01")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "FHIA 1", "FHIA-01")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "FHIA02", "FHIA-02")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "FHIA 2", "FHIA-02")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "FHIA03", "FHIA-03")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "FHIA 3", "FHIA-03")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "FHIA 4", "FHIA-04")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "FHIA 5", "FHIA-05")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "FHIA 6", "FHIA-06")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "FHIA 7", "FHIA-07")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "PA 03-22 ", "PA 03-22")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "PisangCeylan", "Pisang Ceylan")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "PisangBerlin", "Pisang Berlin")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "Pisang berlin", "Pisang Berlin")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "Pisang lilin", "Pisang Lilin")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "CultivarRose", "Rose")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "SF 215/ NBA 14", "SF 215 / NBA 14")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "CvRose", "Rose")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "Cv. Rose", "Rose")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "Local cv. (Latundan)", "Latundan")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "Local cv. (Barraganete)", "Barraganete")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "BluggoeCachaco", "Cachaco")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "BurroCemsa", "Burro Cemsa")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "FrenchSombre", "French Sombre")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "PisangMas", "Pisang Mas")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "GCTCV119", "GCTCV-119")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "GCTCV 119", "GCTCV-119")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "GCTCV215", "GCTCV-215")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "GCTCV 215", "GCTCV-215")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "PV42320", "PV 42-320")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "PV-03.44", "PV 03-44")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "EMB 402", "PV 03-44")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "PA-03.22", "PA 03-22")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "SH 3640", "SH-3640")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "TMBx52951", "BITA-3")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "TMBx 5295-1", "BITA-3")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "TMBx91283", "TMB2x 9128-3")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "BITA3", "BITA-3")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "BITA2", "BITA-2")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "PITA16", "PITA-16")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "PITA 16", "PITA-16")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "TMB3x 15108-6", "PITA-16")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "Nyarma Yik", "Niyarma Yik")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "Niyarma-Yik", "Niyarma Yik")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "NyarmaYik", "Niyarma Yik")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "Niyarma yik", "Niyarma Yik")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "Tuu gia", "Tuu Gia")) %>%
    dplyr::mutate(genotypes = replace(genotypes, genotypes == "Valery (control)", "Valery"))
}

