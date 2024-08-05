# Define the function to calculate traveling allowance for air travel
calculate_travel_allowance_air <- function(name_air, rank, num_days, air_ticket_value, airport_taxi_value, travel_from, travel_to) {
        
        # Check if travel_from is the same as travel_to
        if (travel_from == travel_to) {
                stop("Travel from and travel to locations cannot be the same")
        }
        
        # Define daily travel allowance (DTA) based on rank
        rank_dta <- c("Grade Level 1" = 10000, "Grade Level 2" = 10000, "Grade Level 3" = 10000, 
                      "Grade Level 4" = 10000, "Grade Level 5" = 15000, "Grade Level 6" = 15000, 
                      "Grade Level 7" = 17500, "Grade Level 8(Officer II)" = 17500, "Grade Level 9(Officer I)" = 17500, "Grade Level 10(Senior)" = 17500, 
                      "Grade Level 12(Principal)" = 20000, "Grade Level 13(Assistant Chief)" = 20000, 
                      "Grade Level 14(Chief)" = 25000, "Grade Level 15(Assistant Director)" = 25000, 
                      "Grade Level 16(Deputy Director)" = 37500, "Grade Level 17(Director)" = 37500, 
                      "Permanent Secretary/Chief Executive/Board Members" =70000, "Minister/SGF/HOS"=80000)
        
        # Check if rank is valid
        if (!rank %in% names(rank_dta)) {
                stop("This function is not set up for the selected rank")
        }
        
        dta_per_day <- rank_dta[rank]
        
        dta_days <- dta_per_day * num_days
        
        local_run_days <- 0.30 * dta_per_day * num_days
        
        total_allowance_days <- dta_days + local_run_days
        
        # Calculate the total travel allowance
        total_travel_allowance <- total_allowance_days + air_ticket_value + airport_taxi_value
        
        # Create a dataframe with all variables and the total value
        result <- data.frame(
                Variable = c(
                        "Name of Staff",
                        "Rank", 
                        "Travel From",
                        "Travel To",
                        "Number of Days", 
                        "DTA Days", 
                        "Local Run Days", 
                        "Air Ticket Value", 
                        "Airport Taxi Value", 
                        "Total Travel Allowance"
                ),
                Value = c(
                        name_air,
                        rank,
                        travel_from,
                        travel_to,
                        num_days,
                        dta_days,
                        local_run_days,
                        air_ticket_value,
                        airport_taxi_value,
                        total_travel_allowance
                )
        )
        
        # Format the numerical values with commas and two decimal places
        result$Value <- sapply(seq_along(result$Value), function(i) {
                x <- result$Value[i]
                num_value <- suppressWarnings(as.numeric(x))
                if (!is.na(num_value)) {
                        if (result$Variable[i] == "Number of Days") {
                                return(formatC(num_value, format = "f", big.mark = ",", digits = 0))
                        } else {
                                return(formatC(num_value, format = "f", big.mark = ",", digits = 2))
                        }
                } else {
                        return(as.character(x))
                }
        })
        
        return(result)
}


# Define the function to calculate traveling allowance for road travel
calculate_travel_allowance_road <- function(name_road, rank, num_days, rate_per_km, distance_km, travel_from, travel_to) {
        
        # Check if travel_from is the same as travel_to
        if (travel_from == travel_to) {
                stop("Travel from and travel to locations cannot be the same")
        }
        
        # Define daily travel allowance based on rank
        dta_per_day <- switch(rank,
                              "Grade Level 1" = 10000,
                              "Grade Level 2" = 10000,
                              "Grade Level 3" = 10000,
                              "Grade Level 4" = 10000,
                              "Grade Level 5" = 15000,
                              "Grade Level 6" = 15000,
                              "Grade Level 7" = 17500,
                              "Grade Level 8(Officer II)" = 17500,
                              "Grade Level 9(Officer I)" = 17500,
                              "Grade Level 10(Senior)" = 17500,
                              "Grade Level 12(Principal)" = 20000,
                              "Grade Level 13(Assistant Chief)" = 20000,
                              "Grade Level 14(Chief)" = 25000,
                              "Grade Level 15(Assistant Director)" = 25000,
                              "Grade Level 16(Depurty Director)" = 37500,
                              "Grade Level 17(Director)" = 37500,
                              "Permanent Secretary/Chief Executive/Borad Members" =70000, 
                              "Minister/SGF/HOS"=80000,
                              stop("This function is not set up for the selected rank")
        )
        
        # Define rate per kilometer based on rank
        rate_per_km <- switch(rank,
                              "Grade Level 1" = 35,
                              "Grade Level 2" = 35,
                              "Grade Level 3" = 35,
                              "Grade Level 4" = 35,
                              "Grade Level 5" = 35,
                              "Grade Level 6" = 35,
                              "Grade Level 7" = 55,
                              "Grade Level 8(Officer II)" = 55,
                              "Grade Level 9(Officer I)" = 55,
                              "Grade Level 10(Senior)" = 55,
                              "Grade Level 12(Principal)" = 55,
                              "Grade Level 13(Assistant Chief)" = 55,
                              "Grade Level 14(Chief)" = 55,
                              "Grade Level 15(Assistant Director)" = 75,
                              "Grade Level 16(Deputy Director)" = 75,
                              "Grade Level 17(Director)" = 75,
                              "Permanent Secretary/Chief Executive/Borad Members" =150,
                              "Minister/SGF/HOS"=150,
                              stop("This function is not set up for the selected rank")
        )
        
        # Define distances between state capitals
        state_distance <- list(
                Abakaliki_Abeokuta=656,Abakaliki_Abuja=477,Abakaliki_AdoEkiti=566,Abakaliki_Akure=506,
                Abakaliki_Asaba=193,Abakaliki_Awka=145,Abakaliki_Bauchi=824,Abakaliki_BeninCity=332,
                Abakaliki_BirninKebbi=1204,Abakaliki_Calabar=350,Abakaliki_Damaturu=1183,Abakaliki_Dutse=994,
                Abakaliki_Enugu=84,Abakaliki_Gombe=974,Abakaliki_Gusau=1116,Abakaliki_Ibadan=642,
                Abakaliki_Ikeja=660,Abakaliki_Ilorin=737,Abakaliki_Jalingo=626,Abakaliki_Jos=692,
                Abakaliki_Kaduna=856,Abakaliki_Kano=1167,Abakaliki_Katsina=1286,Abakaliki_Lafia=388,
                Abakaliki_Lokoja=476,Abakaliki_Maiduguri=1285,Abakaliki_Makurdi=258,Abakaliki_Minna=682,
                Abakaliki_Osogbo=546,Abakaliki_Owerri=230,Abakaliki_PortHarcourt=313,Abakaliki_Sokoto=1356,
                Abakaliki_Umuahia=202,Abakaliki_Uyo=273,Abakaliki_Yenagoa=345,Abakaliki_Yola=954,Abeokuta_Abuja=722,
                Abeokuta_AdoEkiti=340,Abeokuta_Akure=280,Abeokuta_Asaba=458,Abeokuta_Awka=444,Abeokuta_Bauchi=1143,
                Abeokuta_BeninCity=314,Abeokuta_BirninKebbi=756,Abeokuta_Calabar=764,Abeokuta_Damaturu=1384,
                Abeokuta_Dutse=1140,Abeokuta_Enugu=572,Abeokuta_Gombe=1304,Abeokuta_Gusau=1110,Abeokuta_Ibadan=75,
                Abeokuta_Ikeja=82,Abeokuta_Ilorin=238,Abeokuta_Jalingo=1312,Abeokuta_Jos=1015,Abeokuta_Kaduna=835,
                Abeokuta_Kano=1088,Abeokuta_Katsina=1181,Abeokuta_Lafia=902,Abeokuta_Lokoja=520,Abeokuta_Maiduguri=1596,
                Abeokuta_Makurdi=991,Abeokuta_Minna=663,Abeokuta_Osogbo=157,Abeokuta_Owerri=559,Abeokuta_PortHarcourt=671,
                Abeokuta_Sokoto=970,Abeokuta_Umuahia=595,Abeokuta_Uyo=671,Abeokuta_Yenagoa=524,Abeokuta_Yola=1580,
                Abuja_AdoEkiti=450,Abuja_Akure=420,Abuja_Asaba=404,Abuja_Awka=440,Abuja_Bauchi=419,Abuja_BeninCity=493,
                Abuja_BirninKebbi=573,Abuja_Calabar=729,Abuja_Damaturu=957,Abuja_Dutse=512,Abuja_Enugu=393,
                Abuja_Gombe=506,Abuja_Gusau=490,Abuja_Ibadan=645,Abuja_Ikeja=761,Abuja_Ilorin=482,Abuja_Jalingo=691,
                Abuja_Jos=297,Abuja_Kaduna=186,Abuja_Kano=396,Abuja_Katsina=570,Abuja_Lafia=180,Abuja_Lokoja=183,
                Abuja_Maiduguri=875,Abuja_Makurdi=280,Abuja_Minna=156,Abuja_Osogbo=428,Abuja_Owerri=533,
                Abuja_PortHarcourt=616,Abuja_Sokoto=748,Abuja_Umuahia=513,Abuja_Uyo=593,Abuja_Yenagoa=647,
                Abuja_Yola=704,AdoEkiti_Akure=50,AdoEkiti_Asaba=368,AdoEkiti_Awka=412,AdoEkiti_Bauchi=945,
                AdoEkiti_BeninCity=228,AdoEkiti_BirninKebbi=634,AdoEkiti_Calabar=579,AdoEkiti_Damaturu=1207,
                AdoEkiti_Dutse=1130,AdoEkiti_Enugu=482,AdoEkiti_Gombe=1124,AdoEkiti_Gusau=1021,AdoEkiti_Ibadan=254,
                AdoEkiti_Ikeja=401,AdoEkiti_Ilorin=164,AdoEkiti_Jalingo=940,AdoEkiti_Jos=940,AdoEkiti_Kaduna=761,
                AdoEkiti_Kano=1010,AdoEkiti_Katsina=1071,AdoEkiti_Lafia=640,AdoEkiti_Lokoja=296,AdoEkiti_Maiduguri=536,
                AdoEkiti_Makurdi=640,AdoEkiti_Minna=596,AdoEkiti_Osogbo=178,AdoEkiti_Owerri=473,AdoEkiti_PortHarcourt=584,
                AdoEkiti_Sokoto=894,AdoEkiti_Umuahia=510,AdoEkiti_Uyo=605,AdoEkiti_Yenagoa=438,AdoEkiti_Yola=1236,
                Akure_Asaba=310,Akure_Awka=342,Akure_Bauchi=885,Akure_BeninCity=165,Akure_BirninKebbi=711,
                Akure_Calabar=619,Akure_Damaturu=1147,Akure_Dutse=1070,Akure_Enugu=422,Akure_Gombe=1255,Akure_Gusau=1057,
                Akure_Ibadan=204,Akure_Ikeja=341,Akure_Ilorin=199,Akure_Jalingo=880,Akure_Jos=682,Akure_Kaduna=749,
                Akure_Kano=1048,Akure_Katsina=1108,Akure_Lafia=600,Akure_Lokoja=236,Akure_Maiduguri=1279,Akure_Makurdi=580,
                Akure_Minna=632,Akure_Osogbo=118,Akure_Owerri=413,Akure_PortHarcourt=524,Akure_Sokoto=930,Akure_Umuahia=430,
                Akure_Uyo=545,Akure_Yenagoa=378,Akure_Yola=1165,Asaba_Awka=44,Asaba_Bauchi=624,Asaba_BeninCity=133,
                Asaba_BirninKebbi=1027,Asaba_Calabar=297,Asaba_Damaturu=1135,Asaba_Dutse=1080,Asaba_Enugu=109,
                Asaba_Gombe=979,Asaba_Gusau=1005,Asaba_Ibadan=430,Asaba_Ikeja=467,Asaba_Ilorin=507,Asaba_Jalingo=859,
                Asaba_Jos=692,Asaba_Kaduna=745,Asaba_Kano=1007,Asaba_Katsina=1098,Asaba_Lafia=435,Asaba_Lokoja=362,
                Asaba_Maiduguri=1288,Asaba_Makurdi=353,Asaba_Minna=673,Asaba_Osogbo=396,Asaba_Owerri=97,Asaba_PortHarcourt=225,
                Asaba_Sokoto=1232,Asaba_Umuahia=166,Asaba_Uyo=247,Asaba_Yenagoa=213,Asaba_Yola=960,Awka_Bauchi=803,
                Awka_BeninCity=166,Awka_BirninKebbi=1245,Awka_Calabar=259,Awka_Damaturu=1115,Awka_Dutse=976,
                Awka_Enugu=62,Awka_Gombe=958,Awka_Gusau=984,Awka_Ibadan=457,Awka_Ikeja=494,Awka_Ilorin=528,
                Awka_Jalingo=815,Awka_Jos=671,Awka_Kaduna=724,Awka_Kano=988,Awka_Katsina=1077,Awka_Lafia=416,
                Awka_Lokoja=456,Awka_Maiduguri=1245,Awka_Makurdi=332,Awka_Minna=926,Awka_Osogbo=425,Awka_Owerri=141,
                Awka_PortHarcourt=255,Awka_Sokoto=1211,Awka_Umuahia=126,Awka_Uyo=207,Awka_Yenagoa=257,Awka_Yola=746,
                Bauchi_BeninCity=850,Bauchi_BirninKebbi=962,Bauchi_Calabar=1002,Bauchi_Damaturu=312,Bauchi_Dutse=313,
                Bauchi_Enugu=740,Bauchi_Gombe=155,Bauchi_Gusau=670,Bauchi_Ibadan=1053,Bauchi_Ikeja=1202,Bauchi_Ilorin=909,
                Bauchi_Jalingo=610,Bauchi_Jos=180,Bauchi_Kaduna=410,Bauchi_Kano=301,Bauchi_Katsina=518,Bauchi_Lafia=326,
                Bauchi_Lokoja=601,Bauchi_Maiduguri=462,Bauchi_Makurdi=469,Bauchi_Minna=590,Bauchi_Osogbo=908,
                Bauchi_Owerri=879,Bauchi_PortHarcourt=970,Bauchi_Sokoto=777,Bauchi_Umuahia=660,Bauchi_Uyo=943,
                Bauchi_Yenagoa=995,Bauchi_Yola=416,BeninCity_BirninKebbi=862,BeninCity_Calabar=490,
                BeninCity_Damaturu=1274,BeninCity_Dutse=880,BeninCity_Enugu=254,BeninCity_Gombe=1044,
                BeninCity_Gusau=1046,BeninCity_Ibadan=300,BeninCity_Ikeja=322,BeninCity_Ilorin=399,
                BeninCity_Jalingo=998,BeninCity_Jos=758,BeninCity_Kaduna=785,BeninCity_Kano=1049,
                BeninCity_Katsina=1252,BeninCity_Lafia=673,BeninCity_Lokoja=287,BeninCity_Maiduguri=1353,
                BeninCity_Makurdi=506,BeninCity_Minna=608,BeninCity_Osogbo=259,BeninCity_Owerri=240,
                BeninCity_PortHarcourt=296,BeninCity_Sokoto=1020,BeninCity_Umuahia=279,BeninCity_Uyo=375,
                BeninCity_Yenagoa=210,BeninCity_Yola=1107,BirninKebbi_Calabar=1318,BirninKebbi_Damaturu=1181,
                BirninKebbi_Dutse=660,BirninKebbi_Enugu=1120,BirninKebbi_Gombe=1117,BirninKebbi_Gusau=322,
                BirninKebbi_Ibadan=679,BirninKebbi_Ikeja=826,BirninKebbi_Ilorin=520,BirninKebbi_Jalingo=1266,
                BirninKebbi_Jos=830,BirninKebbi_Kaduna=550,BirninKebbi_Kano=651,BirninKebbi_Katsina=596,
                BirninKebbi_Lafia=753,BirninKebbi_Lokoja=832,BirninKebbi_Maiduguri=1404,BirninKebbi_Makurdi=898,
                BirninKebbi_Minna=456,BirninKebbi_Osogbo=635,BirninKebbi_Owerri=1124,BirninKebbi_PortHarcourt=1282,
                BirninKebbi_Sokoto=168,BirninKebbi_Umuahia=1272,BirninKebbi_Uyo=1353,BirninKebbi_Yenagoa=1092,
                BirninKebbi_Yola=1408,Calabar_Damaturu=988,Calabar_Dutse=1050,Calabar_Enugu=276,Calabar_Gombe=1152,
                Calabar_Gusau=1309,Calabar_Ibadan=518,Calabar_Ikeja=772,Calabar_Ilorin=849,Calabar_Jalingo=624,
                Calabar_Jos=370,Calabar_Kaduna=1034,Calabar_Kano=1296,Calabar_Katsina=1464,Calabar_Lafia=514,
                Calabar_Lokoja=579,Calabar_Maiduguri=1465,Calabar_Makurdi=532,Calabar_Minna=860,Calabar_Osogbo=695,
                Calabar_Owerri=200,Calabar_PortHarcourt=196,Calabar_Sokoto=1535,Calabar_Umuahia=151,Calabar_Uyo=67,
                Calabar_Yenagoa=300,Calabar_Yola=1132,Damaturu_Dutse=288,Damaturu_Enugu=1099,Damaturu_Gombe=256,
                Damaturu_Gusau=710,Damaturu_Ibadan=1274,Damaturu_Ikeja=1421,Damaturu_Ilorin=1292,Damaturu_Jalingo=364,
                Damaturu_Jos=444,Damaturu_Kaduna=724,Damaturu_Kano=430,Damaturu_Katsina=603,Damaturu_Lafia=540,
                Damaturu_Lokoja=895,Damaturu_Maiduguri=130,Damaturu_Makurdi=783,Damaturu_Minna=874,Damaturu_Osogbo=1407,
                Damaturu_Owerri=1273,Damaturu_PortHarcourt=1172,Damaturu_Sokoto=1013,Damaturu_Umuahia=1251,
                Damaturu_Uyo=1038,Damaturu_Yenagoa=1523,Damaturu_Yola=322,Dutse_Enugu=910,Dutse_Gombe=895,
                Dutse_Gusau=416,Dutse_Ibadan=963,Dutse_Ikeja=1110,Dutse_Ilorin=846,Dutse_Jalingo=542,Dutse_Jos=428,
                Dutse_Kaduna=338,Dutse_Kano=136,Dutse_Katsina=296,Dutse_Lafia=634,Dutse_Lokoja=650,Dutse_Maiduguri=418,
                Dutse_Makurdi=708,Dutse_Minna=558,Dutse_Osogbo=958,Dutse_Owerri=1076,Dutse_PortHarcourt=1150,
                Dutse_Sokoto=520,Dutse_Umuahia=1030,Dutse_Uyo=1150,Dutse_Yenagoa=1192,Dutse_Yola=558,Enugu_Gombe=890,
                Enugu_Gusau=1046,Enugu_Ibadan=558,Enugu_Ikeja=376,Enugu_Ilorin=653,Enugu_Jalingo=735,Enugu_Jos=608,
                Enugu_Kaduna=772,Enugu_Kano=1083,Enugu_Katsina=1202,Enugu_Lafia=705,Enugu_Lokoja=392,Enugu_Maiduguri=1201,
                Enugu_Makurdi=270,Enugu_Minna=598,Enugu_Osogbo=462,Enugu_Owerri=145,Enugu_PortHarcourt=229,
                Enugu_Sokoto=1272,Enugu_Umuahia=118,Enugu_Uyo=189,Enugu_Yenagoa=265,Enugu_Yola=870,Gombe_Gusau=718,
                Gombe_Ibadan=1223,Gombe_Ikeja=1358,Gombe_Ilorin=1064,Gombe_Jalingo=279,Gombe_Jos=286,Gombe_Kaduna=566,
                Gombe_Kano=457,Gombe_Katsina=675,Gombe_Lafia=482,Gombe_Lokoja=756,Gombe_Maiduguri=400,Gombe_Makurdi=623,
                Gombe_Minna=745,Gombe_Osogbo=1383,Gombe_Owerri=1030,Gombe_PortHarcourt=1122,Gombe_Sokoto=931,
                Gombe_Umuahia=1010,Gombe_Uyo=1094,Gombe_Yenagoa=1146,Gombe_Yola=262,Gusau_Ibadan=1017,Gusau_Ikeja=1150,
                Gusau_Ilorin=857,Gusau_Jalingo=1062,Gusau_Jos=330,Gusau_Kaduna=260,Gusau_Kano=260,Gusau_Katsina=463,
                Gusau_Lafia=650,Gusau_Lokoja=692,Gusau_Maiduguri=914,Gusau_Makurdi=774,Gusau_Minna=558,Gusau_Osogbo=972,
                Gusau_Owerri=1180,Gusau_PortHarcourt=1280,Gusau_Sokoto=274,Gusau_Umuahia=1150,Gusau_Uyo=1240,
                Gusau_Yenagoa=1270,Gusau_Yola=971,Ibadan_Ikeja=132,Ibadan_Ilorin=157,Ibadan_Jalingo=1117,Ibadan_Jos=928,
                Ibadan_Kaduna=756,Ibadan_Kano=1005,Ibadan_Katsina=1065,Ibadan_Lafia=825,Ibadan_Lokoja=584,
                Ibadan_Maiduguri=1532,Ibadan_Makurdi=920,Ibadan_Minna=592,Ibadan_Osogbo=90,Ibadan_Owerri=450,
                Ibadan_PortHarcourt=656,Ibadan_Sokoto=890,Ibadan_Umuahia=580,Ibadan_Uyo=676,Ibadan_Yenagoa=510,
                Ibadan_Yola=1543,Ikeja_Ilorin=291,Ikeja_Jalingo=1264,Ikeja_Jos=1168,Ikeja_Kaduna=889,Ikeja_Kano=1139,
                Ikeja_Katsina=1211,Ikeja_Lafia=941,Ikeja_Lokoja=531,Ikeja_Maiduguri=1665,Ikeja_Makurdi=820,
                Ikeja_Minna=725,Ikeja_Osogbo=237,Ikeja_Owerri=564,Ikeja_PortHarcourt=620,Ikeja_Sokoto=1023,
                Ikeja_Umuahia=600,Ikeja_Uyo=689,Ikeja_Yenagoa=532,Ikeja_Yola=1612,Ilorin_Jalingo=958,Ilorin_Jos=776,
                Ilorin_Kaduna=597,Ilorin_Kano=846,Ilorin_Katsina=907,Ilorin_Lafia=660,Ilorin_Lokoja=319,
                Ilorin_Maiduguri=1372,Ilorin_Makurdi=863,Ilorin_Minna=432,Ilorin_Osogbo=115,Ilorin_Owerri=667,
                Ilorin_PortHarcourt=843,Ilorin_Sokoto=730,Ilorin_Umuahia=770,Ilorin_Uyo=865,Ilorin_Yenagoa=609,
                Ilorin_Yola=319,Jalingo_Jos=542,Jalingo_Kaduna=822,Jalingo_Kano=731,Jalingo_Katsina=1084,
                Jalingo_Lafia=450,Jalingo_Lokoja=648,Jalingo_Maiduguri=416,Jalingo_Makurdi=368,Jalingo_Minna=808,
                Jalingo_Osogbo=1073,Jalingo_Owerri=808,Jalingo_PortHarcourt=808,Jalingo_Sokoto=1188,Jalingo_Umuahia=756,
                Jalingo_Uyo=674,Jalingo_Yenagoa=924,Jalingo_Yola=142,Jos_Kaduna=278,Jos_Kano=418,Jos_Katsina=592,
                Jos_Lafia=196,Jos_Lokoja=469,Jos_Maiduguri=592,Jos_Makurdi=336,Jos_Minna=457,Jos_Osogbo=776,
                Jos_Owerri=745,Jos_PortHarcourt=840,Jos_Sokoto=644,Jos_Umuahia=727,Jos_Uyo=811,Jos_Yenagoa=861,
                Jos_Yola=540,Kaduna_Kano=220,Kaduna_Katsina=428,Kaduna_Lafia=474,Kaduna_Lokoja=502,Kaduna_Maiduguri=874,
                Kaduna_Makurdi=500,Kaduna_Minna=297,Kaduna_Osogbo=715,Kaduna_Owerri=910,Kaduna_PortHarcourt=1003,
                Kaduna_Sokoto=480,Kaduna_Umuahia=890,Kaduna_Uyo=975,Kaduna_Yenagoa=1026,Kaduna_Yola=820,
                Kano_Katsina=172,Kano_Lafia=618,Kano_Lokoja=766,Kano_Maiduguri=574,Kano_Makurdi=756,Kano_Minna=503,
                Kano_Osogbo=915,Kano_Owerri=1173,Kano_PortHarcourt=1270,Kano_Sokoto=645,Kano_Umuahia=1153,
                Kano_Uyo=1237,Kano_Yenagoa=1289,Kano_Yola=714,Katsina_Lafia=788,Katsina_Lokoja=918,
                Katsina_Maiduguri=794,Katsina_Makurdi=850,Katsina_Minna=606,Katsina_Osogbo=1008,Katsina_Owerri=1142,
                Katsina_PortHarcourt=1438,Katsina_Sokoto=380,Katsina_Umuahia=1322,Katsina_Uyo=1406,Katsina_Yenagoa=1258,
                Katsina_Yola=875,Lafia_Lokoja=384,Lafia_Maiduguri=728,Lafia_Makurdi=82,Lafia_Minna=297,Lafia_Osogbo=387,
                Lafia_Owerri=490,Lafia_PortHarcourt=587,Lafia_Sokoto=840,Lafia_Umuahia=456,Lafia_Uyo=558,
                Lafia_Yenagoa=606,Lafia_Yola=736,Lokoja_Maiduguri=1063,Lokoja_Makurdi=342,Lokoja_Minna=309,
                Lokoja_Osogbo=305,Lokoja_Owerri=424,Lokoja_PortHarcourt=521,Lokoja_Sokoto=985,Lokoja_Umuahia=405,
                Lokoja_Uyo=490,Lokoja_Yenagoa=540,Lokoja_Yola=945,Maiduguri_Makurdi=930,Maiduguri_Minna=1052,
                Maiduguri_Osogbo=1372,Maiduguri_Owerri=1142,Maiduguri_PortHarcourt=1419,Maiduguri_Sokoto=1123,
                Maiduguri_Umuahia=1323,Maiduguri_Uyo=1406,Maiduguri_Yenagoa=1258,Maiduguri_Yola=436,Makurdi_Minna=440,
                Makurdi_Osogbo=618,Makurdi_Owerri=408,Makurdi_PortHarcourt=505,Makurdi_Sokoto=1116,Makurdi_Umuahia=374,
                Makurdi_Uyo=473,Makurdi_Yenagoa=524,Makurdi_Yola=674,Minna_Osogbo=552,Minna_Owerri=735,
                Minna_PortHarcourt=830,Minna_Sokoto=674,Minna_Umuahia=716,Minna_Uyo=800,Minna_Yenagoa=851,
                Minna_Yola=1124,Osogbo_Owerri=495,Osogbo_PortHarcourt=623,Osogbo_Sokoto=348,Osogbo_Umuahia=564,
                Osogbo_Uyo=645,Osogbo_Yenagoa=469,Osogbo_Yola=225,Owerri_PortHarcourt=112,Owerri_Sokoto=1411,
                Owerri_Umuahia=165,Owerri_Uyo=250,Owerri_Yenagoa=116,Owerri_Yola=1008,PortHarcourt_Sokoto=1263,
                PortHarcourt_Umuahia=114,PortHarcourt_Uyo=120,PortHarcourt_Yenagoa=104,PortHarcourt_Yola=1105,
                Sokoto_Umuahia=1376,Sokoto_Uyo=1475,Sokoto_Yenagoa=1527,Sokoto_Yola=1187,Umuahia_Uyo=88,
                Umuahia_Yenagoa=281,Umuahia_Yola=917,Uyo_Yenagoa=224,Uyo_Yola=1073,Yenagoa_Yola=1124
        )
        
        # Check if travel_from and travel_to are valid locations
        valid_trip <- paste(travel_from, travel_to, sep = "_") %in% names(state_distance) ||
                paste(travel_to, travel_from, sep = "_") %in% names(state_distance)
        if (!valid_trip) {
                stop("Distance between the specified locations is not available.")
        }
        
        # Get distance
        if (paste(travel_from, travel_to, sep = "_") %in% names(state_distance)) {
                distance_km <- state_distance[[paste(travel_from, travel_to, sep = "_")]]
        } else {
                distance_km <- state_distance[[paste(travel_to, travel_from, sep = "_")]]
        }
        # Calculate local run allowance for number of days
        local_run_days <- 0.30 * dta_per_day * num_days
        
        # Calculate DTA for number of days
        dta_days <- dta_per_day * num_days
        
        
        # Calculate total allowance for the given number of days
        total_allowance_days <- dta_days + local_run_days
        
        # Calculate the total travel allowance
        total_travel_allowance <- total_allowance_days + (rate_per_km * distance_km * 2)
        
        # Create a dataframe with all variables and the total value
        result <- data.frame(
                Variable = c(
                        "Name of Staff",
                        "Rank",
                        "Travel From",
                        "Travel To",
                        "Number of Days", 
                        "DTA Days", 
                        "Local Run Days", 
                        "Rate Per Km", 
                        "Distance in Km (one way)", 
                        "Total Travel Allowance"
                ),
                Value = c(
                        name_road,
                        rank,
                        travel_from,
                        travel_to,
                        num_days,
                        dta_days,
                        local_run_days,
                        rate_per_km,
                        distance_km,
                        total_travel_allowance
                )
        )
        
        # Format the numerical values with commas and two decimal places
        result$Value <- sapply(seq_along(result$Value), function(i) {
                x <- result$Value[i]
                num_value <- suppressWarnings(as.numeric(x))
                if (!is.na(num_value)) {
                        if (result$Variable[i] == "Number of Days" || result$Variable[i] == "Distance in Km (one way)") {
                                return(formatC(num_value, format = "f", big.mark = ",", digits = 0))
                        } else {
                                return(formatC(num_value, format = "f", big.mark = ",", digits = 2))
                        }
                } else {
                        return(as.character(x))
                }
        })
        
        return(result)
}

# Define the function to calculate estacode allowance
calculate_travel_allowance_estacode <- function(name_estacode, rank, num_days, exchange_rate, travel_from, travel_to) {
        
        # Check if travel_from is the same as travel_to
        if (travel_from == travel_to) {
                stop("Travel from and travel to locations cannot be the same")
        }
        
        # Define daily estacode allowance based on rank
        rank_estacode <- c("Grade Level 1" = 206, "Grade Level 2" = 206, 
                           "Grade Level 3" = 206, "Grade Level 4" = 206, 
                           "Grade Level 5" = 206, "Grade Level 6" = 206,
                           "Grade Level 7" = 381, "Grade Level 8(Officer II)" = 381, 
                           "Grade Level 9(Officer I)" = 381, "Grade Level 10(Senior)" = 381, 
                           "Grade Level 12(Principal)" = 381, "Grade Level 13(Assistant Chief)" = 381, 
                           "Grade Level 14(Chief)" = 381, "Grade Level 15(Assistant Director)" = 425, 
                           "Grade Level 16(Deputy Director)" = 425, "Grade Level 17(Director)" = 425, 
                           "Permanent Secretary/Chief Executive/Board Members" = 600, "Minister/SGF/HOS"=900)
        
        # Check if rank is valid
        if (!rank %in% names(rank_estacode)) {
                stop("This function is not set up for the selected rank")
        }
        
        estacode_per_day <- rank_estacode[rank]
        
        # Calculate the total travel allowance
        total_travel_allowance <- estacode_per_day * num_days * exchange_rate
        
        # Create a dataframe with all variables and the total value
        result <- data.frame(
                Variable = c(
                        "Name of Staff",
                        "Rank", 
                        "Travel From",
                        "Travel To",
                        "Number of Days", 
                        "Estacode per day($)", 
                        "Exchange rate(₦/$)", 
                        "Total Travel Allowance"
                ),
                Value = c(
                        name_estacode,
                        rank,
                        travel_from,
                        travel_to,
                        num_days,
                        estacode_per_day,
                        exchange_rate,
                        total_travel_allowance
                )
        )
        
        # Format the numerical values with commas and two decimal places
        result$Value <- sapply(seq_along(result$Value), function(i) {
                x <- result$Value[i]
                num_value <- suppressWarnings(as.numeric(x))
                if (!is.na(num_value)) {
                        if (result$Variable[i] == "Number of Days") {
                                return(formatC(num_value, format = "f", big.mark = ",", digits = 0))
                        } else {
                                return(formatC(num_value, format = "f", big.mark = ",", digits = 2))
                        }
                } else {
                        return(as.character(x))
                }
        })
        
        return(result)
}

# Define the function to calculate traveling allowance for air travel
calculate_travel_allowance_estacode_supp <- function(name_estacode_supp, rank, num_days, exchange_rate, estacode_supplement_category, cash_received, travel_from, travel_to) {
        
        # Check if travel_from is the same as travel_to
        if (travel_from == travel_to) {
                stop("Travel from and travel to locations cannot be the same")
        }
        
        # Define daily estacode allowance based on rank
        rank_estacode <- c("Grade Level 1" = 206, "Grade Level 2" = 206, "Grade Level 3" = 206, 
                           "Grade Level 4" = 206, "Grade Level 5" = 206, "Grade Level 6" = 206,
                           "Grade Level 7" = 381, "Grade Level 8(Officer II)" = 381, 
                           "Grade Level 9(Officer I)" = 381, "Grade Level 10(Senior)" = 381, 
                           "Grade Level 12(Principal)" = 381, "Grade Level 13(Assistant Chief)" = 381, 
                           "Grade Level 14(Chief)" = 381, "Grade Level 15(Assistant Director)" = 425, 
                           "Grade Level 16(Deputy Director)" = 425, "Grade Level 17(Director)" = 425, 
                           "Permanent Secretary/Chief Executive/Board Members" =600, "Minister/SGF/HOS"=900)
        
        # Check if rank is valid
        if (!rank %in% names(rank_estacode)) {
                stop("This function is not set up for the selected rank")
        }
        
        estacode_per_day <- rank_estacode[rank]
        
        # Calculate the total travel allowance in dollars
        total_estacode_allowance <- estacode_per_day * num_days
        
        
        if(estacode_supplement_category == "boarding & lodging") {
                total_supplementary_allowance = total_estacode_allowance * 0.1
        } else if (estacode_supplement_category == "lodging and cash") {
                total_supplementary_allowance = (total_estacode_allowance * 0.3) - cash_received
        } else if (estacode_supplement_category == "lodging only") {
                total_supplementary_allowance = total_estacode_allowance * 0.4 
        } else if (estacode_supplement_category == "cash only" && num_days <= 28) {
                total_supplementary_allowance = total_estacode_allowance - cash_received
        } else if (estacode_supplement_category == "cash only" && num_days > 28) {
                total_supplementary_allowance = (total_estacode_allowance * 0.3) - cash_received
        } else {
                stop("Estacode supplemtation category selected is not valid")
        } 
        
        # Check if total supplementary allowance is valif
        if (total_supplementary_allowance <= 0) {
                stop("Estacode supplemtation allowance is not valid for payment")
        }
        
        total_travel_allowance <- total_supplementary_allowance  * exchange_rate
        
        # Create a dataframe with all variables and the total value
        result <- data.frame(
                Variable = c(
                        "Name of Staff",
                        "Rank", 
                        "Travel From",
                        "Travel To",
                        "Estacode Supplement Category",
                        "Number of Days", 
                        "Estacode per day($)", 
                        "Total estacode allowance($)",
                        "Cash Recieved($)",
                        "Total Supplementary allowance($)",
                        "Exchange rate(₦/$)",
                        "Total Supplemetary Allowance(₦)"
                ),
                Value = c(
                        name_estacode_supp,
                        rank,
                        travel_from,
                        travel_to,
                        estacode_supplement_category,
                        num_days,
                        estacode_per_day,
                        total_estacode_allowance,
                        cash_received,
                        total_supplementary_allowance,
                        exchange_rate,
                        total_travel_allowance
                )
        )
        
        # Format the numerical values with commas and two decimal places
        result$Value <- sapply(seq_along(result$Value), function(i) {
                x <- result$Value[i]
                num_value <- suppressWarnings(as.numeric(x))
                if (!is.na(num_value)) {
                        if (result$Variable[i] == "Number of Days") {
                                return(formatC(num_value, format = "f", big.mark = ",", digits = 0))
                        } else {
                                return(formatC(num_value, format = "f", big.mark = ",", digits = 2))
                        }
                } else {
                        return(as.character(x))
                }
        })
        
        return(result)
}

calculate_warm_clothing__allowance <- function(name, rank, exchange_rate, warm_clothing_rate, travel_from, travel_to) {
        
        # Check if travel_from is the same as travel_to
        if (travel_from == travel_to) {
                stop("Travel from and travel to locations cannot be the same")
        }
        
        # Define daily estacode allowance based on rank
        warm_clothing_rate <- 720
        
        # Calculate the warm clothing allowance
        warm_clothing_allowance <- warm_clothing_rate * exchange_rate
        
        
        # Create a dataframe with all variables and the Warm Clothing Allowance
        result <- data.frame(
                Variable = c(
                        "Name of Staff",
                        "Rank", 
                        "Travel From",
                        "Travel To",
                        "Warm Clothing Rate($)", 
                        "Exchange rate(₦/$)",
                        "Warm Clothing Allowance(₦)"
                ),
                Value = c(
                        name,
                        rank,
                        travel_from,
                        travel_to,
                        warm_clothing_rate,
                        exchange_rate,
                        warm_clothing_allowance
                )
        )
        
        # Format the numerical values with commas and two decimal places
        result$Value <- sapply(seq_along(result$Value), function(i) {
                x <- result$Value[i]
                num_value <- suppressWarnings(as.numeric(x))
                if (!is.na(num_value)) {
                        return(formatC(num_value, format = "f", big.mark = ",", digits = 2))
                } else {
                        return(as.character(x))
                }
        })
        
        return(result)
}


# Define the server logic required to calculate the allowance and display the results
server <- function(input, output, session) {
        result_air <- reactiveVal(NULL)
        result_road <- reactiveVal(NULL)
        result_estacode <- reactiveVal(NULL)
        result_estacode_supp <- reactiveVal(NULL)
        result_warm_clothing <- reactiveVal(NULL)
        
        observeEvent(input$calculate_air, {
                name <- input$name_air
                rank <- input$rank_air
                num_days <- input$num_days_air
                air_ticket_value <- input$air_ticket_value
                airport_taxi_value <- input$airport_taxi_value
                travel_from <- input$travel_from_air
                travel_to <- input$travel_to_air
                
                result_air(calculate_travel_allowance_air(name, rank, num_days, air_ticket_value, airport_taxi_value, travel_from, travel_to))
        })
        
        observeEvent(input$calculate_road, {
                name <- input$name_road
                rank <- input$rank_road
                num_days <- input$num_days_road
                rate_per_km <- input$rate_per_km
                distance_km <- input$distance_km
                travel_from <- input$travel_from_road
                travel_to <- input$travel_to_road
                
                result_road(calculate_travel_allowance_road(name, rank, num_days, rate_per_km, distance_km, travel_from, travel_to))
        })
        
        observeEvent(input$calculate_estacode, {
                name <- input$name_estacode
                rank <- input$rank_estacode
                num_days <- input$num_days_estacode
                exchange_rate <- input$exchange_rate_estacode
                travel_from <- input$travel_from_estacode
                travel_to <- input$travel_to_estacode
                
                result_estacode(calculate_travel_allowance_estacode(name, rank, num_days, exchange_rate, travel_from, travel_to))
        })
        
        observeEvent(input$calculate__estacode_supp, {
                name <- input$name_estacode_supp
                rank <- input$rank_estacode_supp
                num_days <- input$num_days_estacode_supp
                estacode_supplement_category <- input$estacode_supplement_category
                cash_received <- input$cash_received
                exchange_rate <- input$exchange_rate_estacode_supp
                travel_from <- input$travel_from_estacode_supp
                travel_to <- input$travel_to_estacode_supp
                
                result_estacode_supp(calculate_travel_allowance_estacode_supp(name_estacode_supp, rank, num_days, exchange_rate, estacode_supplement_category, cash_received, travel_from, travel_to))
        })
        
        observeEvent(input$calculate_warm_clothing, {
                name <- input$name_warm_clothing
                rank <- input$rank_warm_clothing
                exchange_rate <- input$exchange_rate_warm_clothing
                travel_from <- input$travel_from_warm_clothing
                travel_to <- input$travel_to_warm_clothing
                
                result_warm_clothing(calculate_warm_clothing__allowance(name, rank, exchange_rate, warm_clothing_rate, travel_from, travel_to))
        })
        
        
        output$air_allowance_table <- renderTable({
                result_air()
        })
        
        output$road_allowance_table <- renderTable({
                result_road()
        })
        
        output$estacode_allowance_table <- renderTable({
                result_estacode()
        })
        
        output$estacode_supp_allowance_table <- renderTable({
                result_estacode_supp()
        })
        
        output$warm_clothing_allowance_table <- renderTable({
                result_warm_clothing()
        })
        
        output$downloadWord_air <- downloadHandler(
                filename = function() {
                        paste("Air_Travel_Allowance_", Sys.Date(), ".docx", sep = "")
                },
                content = function(file) {
                        df <- result_air()
                        doc <- read_docx() %>%
                                body_add_table(df, style = "table_template")
                        print(doc, target = file)
                },
                contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
        )
        
        output$downloadCSV_air <- downloadHandler(
                filename = function() {
                        paste("Air_Travel_Allowance_", Sys.Date(), ".csv", sep = "")
                },
                content = function(file) {
                        df <- result_air()
                        write.csv(df, file, row.names = FALSE)
                },
                contentType = "text/csv"
        )
        
        output$downloadWord_road <- downloadHandler(
                filename = function() {
                        paste("Road_Travel_Allowance_", Sys.Date(), ".docx", sep = "")
                },
                content = function(file) {
                        df <- result_road()
                        doc <- read_docx() %>%
                                body_add_table(df, style = "table_template")
                        print(doc, target = file)
                },
                contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
        )
        
        output$downloadCSV_road <- downloadHandler(
                filename = function() {
                        paste("Road_Travel_Allowance_", Sys.Date(), ".csv", sep = "")
                },
                content = function(file) {
                        df <- result_road()
                        write.csv(df, file, row.names = FALSE)
                },
                contentType = "text/csv"
        )
        
        output$downloadWord_estacode <- downloadHandler(
                filename = function() {
                        paste("Estacode_Allowance_", Sys.Date(), ".docx", sep = "")
                },
                content = function(file) {
                        df <- result_estacode()
                        doc <- read_docx() %>%
                                body_add_table(df, style = "table_template")
                        print(doc, target = file)
                },
                contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
        )
        
        output$downloadCSV_estacode <- downloadHandler(
                filename = function() {
                        paste("Estacode_Allowance_", Sys.Date(), ".csv", sep = "")
                },
                content = function(file) {
                        df <- result_estacode()
                        write.csv(df, file, row.names = FALSE)
                },
                contentType = "text/csv"
        )
        
        output$downloadWord__estacode_supp <- downloadHandler(
                filename = function() {
                        paste("Estacode_Supp_Allowance_", Sys.Date(), ".docx", sep = "")
                },
                content = function(file) {
                        df <- result_estacode_supp()
                        doc <- read_docx() %>%
                                body_add_table(df, style = "table_template")
                        print(doc, target = file)
                },
                contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
        )
        
        output$downloadCSV__estacode_supp <- downloadHandler(
                filename = function() {
                        paste("Estacode_Supp_Allowance_", Sys.Date(), ".csv", sep = "")
                },
                content = function(file) {
                        df <- result_estacode_supp()
                        write.csv(df, file, row.names = FALSE)
                },
                contentType = "text/csv"
        )
        
        output$downloadWord_warm_clothing <- downloadHandler(
                filename = function() {
                        paste("Warm_Clothing_Allowance_", Sys.Date(), ".docx", sep = "")
                },
                content = function(file) {
                        df <- result_warm_clothing()
                        doc <- read_docx() %>%
                                body_add_table(df, style = "table_template")
                        print(doc, target = file)
                },
                contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
        )
        
        output$downloadCSV_warm_clothing <- downloadHandler(
                filename = function() {
                        paste("Estacode_Supp_Allowance_", Sys.Date(), ".csv", sep = "")
                },
                content = function(file) {
                        df <- result_warm_clothing()
                        write.csv(df, file, row.names = FALSE)
                },
                contentType = "text/csv"
        )
        
        
        
        observeEvent(input$reset_air, {
                updateTextInput(session, "name_air", value = "")
                updateTextInput(session, "travel_from_air", value = "")
                updateTextInput(session, "travel_to_air", value = "")
                updateTextInput(session, "rank_air", value = "")
                updateNumericInput(session, "num_days_air", value = "")
                updateNumericInput(session, "air_ticket_value", value = "")
                updateNumericInput(session, "airport_taxi_value", value = "")
                output$air_allowance_table <- renderTable(NULL)
        })
        
        observeEvent(input$reset_road, {
                updateTextInput(session, "name_road", value = "")
                updateTextInput(session, "travel_from_road", value = "")
                updateTextInput(session, "travel_to_road", value = "")
                updateTextInput(session, "rank_road", value = "")
                updateNumericInput(session, "num_days_road", value = "")
                updateNumericInput(session, "rate_per_km", value = "")
                updateNumericInput(session, "distance_km", value = "")
                output$road_allowance_table <- renderTable(NULL)
        })
        
        observeEvent(input$reset_estacode, {
                updateTextInput(session, "name_estacode", value = "")
                updateTextInput(session, "travel_from_estacode", value = "")
                updateTextInput(session, "travel_to_estacode", value = "")
                updateTextInput(session, "rank_estacode", value = "")
                updateNumericInput(session, "num_days_estacode", value = "")
                updateNumericInput(session, "exchange_rate_estacode", value = "")
                output$road_allowance_table <- renderTable(NULL)
        })
        
        observeEvent(input$reset__estacode_supp, {
                updateTextInput(session, "name_estacode_supp", value = "")
                updateTextInput(session, "travel_from_estacode_supp", value = "")
                updateTextInput(session, "travel_to_estacode_supp", value = "")
                updateTextInput(session, "rank_estacode_supp", value = "")
                updateTextInput(session, "estacode_supplement_category", value = "")
                updateNumericInput(session, "num_days_estacode_supp", value = "")
                updateNumericInput(session, "exchange_rate_estacode_supp", value = "")
                updateNumericInput(session, "cash_received", value = "")
                output$road_allowance_table <- renderTable(NULL)
        })
        
        observeEvent(input$reset_warm_clothing, {
                updateTextInput(session, "name_warm_clothing", value = "")
                updateTextInput(session, "travel_from_warm_clothing", value = "")
                updateTextInput(session, "travel_to_warm_clothing", value = "")
                updateTextInput(session, "rank_warm_clothing", value = "")
                updateNumericInput(session, "exchange_rate_warm_clothing", value = "")
                output$road_allowance_table <- renderTable(NULL)
        })
}