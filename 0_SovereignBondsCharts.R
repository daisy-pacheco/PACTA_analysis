# Charts for Sovereign Bonds Analysis


SovereignBondFigures <- function(){
  SB_CountryRating("91")
  SB_Suscept("92")
  SB_GDP("93")
  SB_TechExposure("94")
  SB_PolicyAmbition("95")
}

GetSovBondCoverage <- function(){
  
  HoldingSummary <- readRDS(paste0(proc_input_path, "/", project_name, "_total_portfolio.rda"))
  
  
  HoldingSummary$IsSB <- ifelse(HoldingSummary$security_type %in%  c("Sovereign Debt","Sovereign Agency Debt", "Government inflation linked Bonds"),1,0)
  HoldingSummary$IsSB <- ifelse(HoldingSummary$security_bics_subgroup %in% c("Sovereign","Sovereign Agency", "Sovereigns"),1,HoldingSummary$IsSB)
  
  
  SB.Holdings <- HoldingSummary %>% 
    group_by(investor_name,portfolio_name, IsSB) %>% 
    summarise(SBValue = sum(value_usd,na.rm = T)) %>% 
    group_by(investor_name,portfolio_name) %>%
    mutate(TotPortfolio = sum(SBValue),
           SBPerc = SBValue/TotPortfolio) %>% 
    filter(IsSB == 1)
  
  SB.Holdings
  
}

SB_CountryRating <- function(plotnumber){
  #### Figure 1_2 #### 
  ## Breakdown of portfolio by country and rating (S&P)
  
  # Portfolio Countries
  
  SBCountries <- SB.Summary %>% 
    filter(investor_name == investor_name_select,
           portfolio_name == portfolio_name_select) %>% 
    group_by(investor_name, portfolio_name) %>% 
    mutate(total_value = sum(value_usd)) %>% 
    group_by(investor_name, portfolio_name, country_of_domicile) %>% 
    mutate(Perc = value_usd/total_value)
  
  
  if(data_check(SBCountries)){
    
    # data
    sb1cc <- read_csv(paste0(sb_data_path, "Fig.1_2 Credit ratingbycountry_Rev1_5.csv")) %>%
      mutate(
        RegionGranular = case_when(
          Region == "Africa" ~ "AF(Others)"
        ),
        RegionGranular = case_when(
          Country %in% c(
            "Morocco",
            "Tunisia",
            "Egypt"
          ) ~ "AF(North)",
          Country %in% c(
            "Nigeria",
            "Senegal",
            "Ivory Coast",
            "Ghana",
            "Benin",
            "Togo"
          ) ~ "AF(West)",
          Country %in% c(
            "Cameroon",
            "Gabon",
            "Congo",
            "Angola",
            "Republic of the Congo"
          ) ~ "AF(Central)",
          Country %in% c(
            "Ethiopia",
            "Uganda",
            "Kenya",
            "Tanzania",
            "Mozambique",
            "Zambia"
          ) ~ "AF(East)",
          Country %in% c(
            "Namibia",
            "Botswana",
            "South Africa"
          ) ~ "AF(South)",
          Country %in% c(
            "Spain",
            "Italy",
            "Portugal",
            "Greece"
          ) ~ "EU(South)",
          Country %in% c(
            "France",
            "Switzerland",
            "Luxembourg",
            "Belgium",
            "Netherlands",
            "Germany",
            "Austria"
          ) ~ "EU(West)",
          Country %in% c(
            "Iceland",
            "Norway",
            "Sweden",
            "Finland"
          ) ~ "EU(North)",
          Country %in% c(
            "Slovakia",
            "Hungary",
            "Israel",
            "Poland",
            "Romania",
            "Ukraine",
            "Estonia",
            "Latvia",
            "Lithuania",
            "Belarus",
            "Czech Republic",
            "Slovakia",
            "Moldova",
            "Slovenia",
            "Croatia",
            "Bosnia and Herzegovina",
            "Serbia",
            "Bulgaria",
            "Montenegro",
            "Macedonia",
            "Albania"
          ) ~ "EU(East)",
          Country %in% c(
            "Turkey",
            "Georgia",
            "Azerbaijan",
            "Armenia",
            "Israel",
            "Bahrain",
            "Iraq",
            "Jordan",
            "Kuwait",
            "Lebanon",
            "Oman",
            "Qatar",
            "Saudi Arabia",
            "United Arab Emirates"
          ) ~ "AS(West)",
          Country %in% c(
            "Uzbekistan",
            "Kazakhstan",
            "Kyrgyzstan",
            "Pakistan",
            "India",
            "Bangladesh",
            "Sri Lanka",
            "Maldives"
          ) ~ "AS(South-central)",
          Country %in% c(
            "China",
            "Taiwan",
            "Hong Kong",
            "Mongolia",
            "South Korea",
            "Japan"
          ) ~ "AS(East)",
          Country %in% c(
            "Thailand",
            "Vietnam",
            "Cambodia",
            "Malaysia",
            "Singapore",
            "Indonesia",
            "Philippines"
          ) ~ "AS(South-east)",
          Country %in% c(
            "Australia",
            "New Zealand"
          ) ~ "OC.",
          Country %in% c(
            "Mexico",
            "Costa Rica",
            "El Salvador",
            "Jamaica",
            "Panama",
            "Dominican Republic"
          ) ~ "Latam",
          Country %in% c(
            "Argentina",
            "Chile",
            "Uruguay",
            "Colombia",
            "Bolivia",
            "Ecuador",
            "Paraguay",
            "Peru",
            "Brazil"
          ) ~ "Latam",
          TRUE ~ iso2c
        )
      )
    
    regions_countries_combination <- unique(c(unique(sb1cc$RegionGranular), unique(sb1cc$iso2c)))
    regions_countries_combination <- regions_countries_combination[!is.na(regions_countries_combination)]
    
    colors_palettes <- c(
      "Set3",
      "Set2",
      "Set1",
      "Pastel2",
      "Paired",
      "Dark2",
      "Accent",
      "BuPu",
      "GnBu",
      "OrRd",
      "PuBu",
      "PuBuGn",
      "PuRd",
      "RdPu",
      "YlGn",
      "YlGnBu",
      "YlOrBr",
      "YlOrRd"
    )
    vector_of_colors <- NULL
    for(pallete in colors_palettes) {
      vector_of_colors <- c(vector_of_colors, brewer.pal(n = 8, name = pallete))
    }
    
    region_country_colors <- vector_of_colors[1:length(regions_countries_combination)]
    names(region_country_colors) <- regions_countries_combination
    
    Bond.Rating <- sb1cc %>% mutate(num = 1) %>% 
      merge(SBCountries, by.x = "iso2c", by.y = "country_of_domicile") %>%
      mutate(S.P = gsub("[[:space:]]","",S.P)) %>% 
      arrange(Region, Country_ES)
    
    
    Bond.Rating$Country_ES <- factor(Bond.Rating$Country_ES, levels = Bond.Rating$Country_ES)
    
    Bond.Rating$Label <- Bond.Rating$S.P
    
    Bond.Rating$S.P <- gsub("[+]","",Bond.Rating$S.P)
    Bond.Rating$S.P <- gsub("[-]","",Bond.Rating$S.P)
    
    Bond.Rating$S.P <- factor(Bond.Rating$S.P, levels = c("AAA","AA","A",
                                                          "BBB","BB","B",
                                                          "CCC", "CC", "C", "D"), ordered = T)
    
    
    Bond.Rating <- Bond.Rating[!is.na(Bond.Rating$S.P),]
    
    Bond.Rating$Label <- Bond.Rating$S.P.2
    
    Bond.Rating$S.P.2 <- gsub("[+]","",Bond.Rating$S.P.2)
    Bond.Rating$S.P.2 <- gsub("[-]","",Bond.Rating$S.P.2)
    
    Bond.Rating$S.P.2 <- factor(Bond.Rating$S.P.2, levels = c("AAA","AA","A",
                                                          "BBB","BB","B",
                                                          "CCC", "CC", "C", "SD", "D"), ordered = T)
    
    
    Bond.Rating <- Bond.Rating[!is.na(Bond.Rating$S.P.2),]
    
    # chart values
    
    # opt.order <- c(3, 6, 9, 1, 4, 7, 2, 5, 8)
    # 
    # # However the colours are repeated. Need a solution for more colours
    # Europe.colors <- brewer.pal(n =  9, name = "Blues")[opt.order]
    # N.America.colors <- brewer.pal(n = 9, name = "Greens")[opt.order]
    # S.America.colors <- brewer.pal(n = 9, name = "Reds")[opt.order]
    # AsiaPacific.colors <- brewer.pal(n = 9, name = "Purples")[opt.order]
    # M.EastAfrica <- brewer.pal(n = 9, name = "Oranges")[opt.order] 
    # Australia.colors <- brewer.pal(n = 9, name = "RdPu")[opt.order]
    # Africa.colors <- brewer.pal(n= 9, name = "Greys")[opt.order]
    
    # Bond.Rating$colors <- "grey"
    # Bond.Rating <- Bond.Rating %>% mutate(colors = ifelse(Bond.Rating$Region == "Europe", Europe.colors,
    #                                                       ifelse(Bond.Rating$Region == "North America", N.America.colors,
    #                                                              ifelse(Bond.Rating$Region == "South and Central America", S.America.colors, 
    #                                                                     ifelse(Bond.Rating$Region == "Asia",AsiaPacific.colors,
    #                                                                            ifelse(Bond.Rating$Region == "MiddleEast", M.EastAfrica, 
    #                                                                                   ifelse(Bond.Rating$Region == "Africa", Africa.colors, "grey")))))))
    
    legend.rows = ceiling(nrow(Bond.Rating)/4)
    
    # write_csv(Bond.Rating, paste0(raw_input_path,"/BondRating.csv"))
    
    Bond.Rating.Group <- Bond.Rating %>% 
      mutate(
        Region = RegionGranular,
        Region = ifelse(Country == "Colombia", iso2c, Region),
        S.P = S.P.2
      ) %>% 
      group_by(Region, S.P) %>% 
      mutate(
        count_by_sp = n(),
        Region = case_when(
          count_by_sp < 3 & !(S.P %in% c("BBB", "BB", "B")) ~ iso2c,
          count_by_sp > 1 & (S.P %in% c("BBB", "BB", "B")) ~ Region,
          TRUE ~ Region
        )
      ) %>% 
      group_by(Region, S.P) %>% 
      summarise(Perc = sum(Perc)) %>%
      ungroup() %>% 
      mutate(
        n_country = n_distinct(Region),
        max_sup_90 = ifelse(Perc > 0.9 & max(Perc) == Perc, TRUE, FALSE)
      ) %>% 
      filter(!(max_sup_90) | n_country == 1)

    # vector_of_colors <- c(
    #   Europe.colors,
    #   N.America.colors,
    #   S.America.colors,
    #   AsiaPacific.colors,
    #   M.EastAfrica,
    #   Australia.colors,
    #   Africa.colors
    # )
    
    # chart
    Fig1_2 <- ggplot(Bond.Rating.Group, aes(x = S.P, y = Perc, fill = Region)) +
      geom_bar(stat = "identity", width = 0.3) + 
      scale_fill_manual(values = region_country_colors) +
      scale_y_continuous(labels = percent_format(accuracy = 0.001, trim = FALSE)) +
      scale_x_discrete(expand = expand_scale(add = c(0,1))) +
      theme_barcharts_sb() +
      theme(axis.line.x = element_line(size = 0.5, linetype = "solid", color = textcolor),
            axis.line.y = element_blank(),
            axis.text.x = element_text(colour=textcolor, size = textsize+2, face = "bold"),
            legend.position = "bottom",
            legend.justification = 0.5,
            legend.key.size = unit(0.4, "cm"),
            legend.box.margin = unit(c(-3, 0, 0, 0), "pt"),
            legend.text = element_text(margin = margin(r = 0, l = 1, unit = "pt"), size = textsize/2),
            legend.title = element_blank(),
            panel.border = element_blank(),
            panel.grid.major.y = element_line(colour = "#989898", size = 0.2),
            plot.margin = unit(c(0., 0.8, 0., 0.), "cm"),
            plot.title = element_text(size = textsize+1, colour = textcolor, 
                                      face = "bold", hjust = 0)) +
      labs(y = ShareByRating) +
      guides(fill = guide_legend(ncol = 8, override.aes = list(color = "white")))+ #nrow = 4, ncol = 9, 
      geom_text_repel( 
        aes(label = paste0(sprintf("%.4f", Perc*100),"%")), # Region,": ","\n", 
        segment.size  = 0.05, 
        segment.color = "grey70",
        point.padding = NA,
        position = position_stack(vjust = 0.99),
        direction = "y", 
        hjust = -0.6,
        size = textsize/6, family = textfont, color = textcolor)
    
  }else{
    
    Fig1_2 = no_chart(NoSovBonds)
  }
  # Expanding limits using D resets the order of the columns
  
  # save as png
  ggsave(Fig1_2, 
         filename=graph_name(plotnumber,ParameterFile), 
         width = 4, height = 3,
         bg = "white", dpi = 300)
  
}

#### Figure 2 #### 
## Sovereign bonds susceptibility class

SB_Suscept <- function(plotnumber){
  
  sb2 <- read_csv(paste0(sb_data_path, "Fig.1_2 Credit ratingbycountry_Rev1_5.csv"))
  
  SBCountries <- SB.Summary %>% 
    filter(investor_name == investor_name_select,
           portfolio_name == portfolio_name_select) %>% 
    group_by(investor_name, portfolio_name) %>% 
    mutate(total_value = sum(value_usd)) %>% 
    group_by(investor_name, portfolio_name, country_of_domicile) %>% 
    mutate(Perc = value_usd/total_value)
  
  
  if(data_check(SBCountries)){
    
    # sb2 <- sb2 %>% filter(iso2c %in% SBCountries$country_of_domicile)
    
    sb2 <- base::merge(sb2, SBCountries, by.x = "iso2c", by.y = "country_of_domicile")
    
    
    Moodys.Rating <- sb2 %>% 
      group_by(MoodyCCrating) %>% 
      summarize(rate_count = n(),
                sb.weight = sum(Perc)) %>% 
      ungroup() %>%
      mutate(rate_percent = sb.weight/sum(sb.weight),
             rate_count = sum(rate_count))
    
    # chart values
    # Rate.colors <- c("Not Rated" = "#E0E1E0" ,   #'#feebe2',
    #                  "Least Susceptible" = "#E8924D",    #'#fbb4b9',
    #                  "Less Susceptible" = "#F6CB54",   #'#f768a1',
    #                  "Susceptible" = "#53B38E",  #'#c51b8a',
    #                  "Most Susceptible" = "#C2344C")   #'#7a0177')  
    
    Rate.colors <- data.frame(MoodyCCrating = c("Not Rated","Least Susceptible","Less Susceptible","Susceptible","Most Susceptible"),
                              Color.no = 1:5,
                              Colours = c("#E0E1E0","#E8924D","#F6CB54","#53B38E","#C2344C"))
    
    Rate.colors$MoodyCCrating <- dplyr::recode(Rate.colors$MoodyCCrating, 
                                               "Most Susceptible" = x_MostSusceptible, 
                                               "Susceptible" = x_Susceptible, 
                                               "Less Susceptible" = x_LessSusceptible, 
                                               "Least Susceptible" = x_LeastSusceptible, 
                                               "Not Rated" = x_NotRated)
    
    Moodys.Rating$MoodyCCrating <- factor(Moodys.Rating$MoodyCCrating, 
                                          levels = c("Most Susceptible", 
                                                     "Susceptible", 
                                                     "Less Susceptible", 
                                                     "Least Susceptible", 
                                                     "Not Rated"))
    
    Moodys.Rating$MoodyCCrating <- dplyr::recode(Moodys.Rating$MoodyCCrating, 
                                                 "Most Susceptible" = x_MostSusceptible, 
                                                 "Susceptible" = x_Susceptible, 
                                                 "Less Susceptible" = x_LessSusceptible, 
                                                 "Least Susceptible" = x_LeastSusceptible, 
                                                 "Not Rated" = x_NotRated)
    
    Colours <- Rate.colors$Colours
    names(Colours) <- Rate.colors$MoodyCCrating
    
    
    # chart
    Fig2 <- ggplot(Moodys.Rating, aes(x = "", y = rate_percent, fill = MoodyCCrating)) + 
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = Colours) +
      scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1), expand = c(0, 0)) +
      #scale_x_discrete(expand = c(0, 0)) + 
      theme_void() +
      theme(axis.text.x = element_text(family = textfont, size = textsize+3, colour = textcolor),
            axis.title = element_blank(), 
            axis.line.y = element_line(size = 0.8, colour = textcolor),
            legend.box.margin = unit(c(20, 0, 0, 0), "pt"),
            legend.key.height = unit(0.45, "cm"),
            legend.key.width = unit(0.45, "cm"),
            legend.spacing.y = unit(1.0, "cm"),
            legend.position = "bottom",
            legend.text = element_text(family = textfont, size = textsize+3, 
                                       colour = textcolor,
                                       margin = margin(l = 3, r = 10, unit = "pt")),
            legend.title = element_blank(),
            panel.grid.major = element_line(colour = "#989898", size = 0.2),
            panel.grid.minor = element_blank(),
            plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"),
            plot.title = element_text(size = textsize+2, colour = textcolor, 
                                      face = "bold", hjust = 0, 
                                      margin = margin(b = 30, unit = "pt"))) +
      #labs(title = "Susceptibility to being impacted by climate change \nof sovereign bonds portfolio") +
      guides(fill = guide_legend(reverse = TRUE, nrow = 3, ncol = 2, override.aes = list(color = "white"))) 
  }else{
    
    Fig2 <- no_chart(NoSovBonds)
  }
  
  # save as png
  ggsave(Fig2, 
         filename = graph_name(plotnumber, ParameterFile), 
         width = 5, height = 2.5,
         bg = "white", dpi = 300)
  
}

#### Figure 3 #### 
# GDP exposure to high-carbon sectors 

SB_GDP <- function(plotnumber){
  # data
  sb3 <- read_csv(paste0(sb_data_path, "Fig.3 SectorialGDP.csv"))
  
  portfolio.countries <- SB.Summary %>% filter(investor_name == investor_name_select, portfolio_name == portfolio_name_select) %>% select(country_of_domicile) 
  
  sb3 <- sb3 %>% filter(iso2 %in% portfolio.countries$country_of_domicile)
  
  if(data_check(sb3)){
    sb3$Sector <- factor(sb3$Sector, levels = c("Fossil fuels", "Power", "Transport", "Cement & Steel & Iron", "Others"))
    
    Sector.colors <- c("Cement & Steel & Iron" = material, "Fossil fuels" = energy, 
                       "Power" = pow, "Transport" = trans, "Others" = notrelevant)
    
    max.GDP <- sb3 %>% filter(Sector != "Others") %>%
      group_by(Country) %>%
      mutate(sum.GDP = sum(GDP)) %>% ungroup() #%>%
    #summarize(max.GDP = max(sum.GDP))
    
    limit.GDP.axis <- (1-max(max.GDP$sum.GDP))*0.95
    
    sb3.ToPlot <- sb3 %>% filter(Sector != "Others")
    caption.iso <- unique(paste0(sb3.ToPlot$iso2, "-", sb3.ToPlot$Country))
    
    sb3.ToPlot <- sb3 %>% filter(Sector != "Others")
    caption.iso <- as.vector(unique(paste0(sb3.ToPlot$iso2, "-", sb3.ToPlot$Country)))
    caption.iso.string <- str_c(caption.iso, collapse = ", ")  #### - continue
    
    sb3.ToPlot$Sector <- dplyr::recode(sb3.ToPlot$Sector,
                                       "Fossil fuels" = S_FossilFuels,
                                       "Power" = S_Power,
                                       "Transport" = S_Transport,
                                       "Cement & Steel & Iron" = S_CementSteel)
    
    Sector.colors <- rev(c(energy,pow,trans,material))
    
    uni.countries <- sb3 %>% select(Country_ES,iso2) %>% distinct() %>% arrange(iso2)
    uni.countries$label <- paste0(uni.countries$iso2,"-",uni.countries$Country_ES) 
    capt <- paste0(uni.countries$label, collapse = ", ")
    
    # chart
    Fig3 <- ggplot(sb3.ToPlot, aes(x = iso2, y =  GDP, fill = fct_rev(Sector))) +
      geom_col(width = 0.6) + 
      #scale_fill_brewer(palette = "Set2") +
      scale_fill_manual(values = Sector.colors) +
      scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0, 0, 0, -limit.GDP.axis),
                         limits = c(0, 1)) +
      theme_barcharts() +
      theme(axis.line.x = element_line(size = 0.5, linetype = "solid", color = textcolor),
            axis.line.y = element_blank(),
            legend.position = "bottom",
            legend.justification = 0.5,
            legend.key.size = unit(0.4, "cm"),
            legend.box.margin = unit(c(-10, 0, 0, 0), "pt"),
            legend.text = element_text(margin = margin(r = 3, l = 3, unit = "pt")),
            legend.title = element_blank(),
            panel.border = element_blank(),
            panel.grid.major.y = element_line(colour = "#989898", size = 0.2),
            plot.margin = unit(c(0, 0.5, 0.1, 0.1), "cm"),
            plot.title = element_text(size = textsize+2, colour = textcolor, 
                                      face = "bold", hjust = -0.5, 
                                      margin = margin(b = 10, unit = "pt"))) +
      guides(fill = guide_legend(nrow = 2, reverse = TRUE, override.aes = list(color = "white"))) +
      labs(#title = "GDP exposure to high-carbon sectors", 
        y = x_ShareGDP, caption = wrap_labels(capt, 80))
  }else{
    
    Fig3 <- no_chart(NoSovBonds)
  }
  # save as png
  ggsave(Fig3, 
         filename = graph_name(plotnumber, ParameterFile), 
         width = 4, height = 3.2,
         bg = "white", dpi = 300)
  
}

#### Figure 4 ####
## Technology exposure 
SB_TechExposure <- function(plotnumber){
  
  e.Y = Startyear +5
  
  SBCountries <- SB.Summary %>% 
    filter(investor_name == investor_name_select,
           portfolio_name == portfolio_name_select) %>% 
    group_by(investor_name, portfolio_name) %>% 
    mutate(total_value = sum(value_usd)) %>% 
    group_by(investor_name, portfolio_name, country_of_domicile) %>% 
    mutate(Perc = value_usd/total_value)
  
  
  if(data_check(SBCountries)){
    
    ## Scenario data
    sb4.Scen <- read_csv(paste0(sb_data_path, "Fig.4_6_Scenario_data_techshare.csv")) %>% 
      rename(Production = AnnualvalIEAtech)
    
    sb4.ALD.19 <- read_csv(paste0(analysis_inputs_path, "sector_production_mapped_per_technology_country_and_year_2019.csv")) 
    sb4.ALD.24 <- read_csv(paste0(analysis_inputs_path, "sector_production_mapped_per_technology_country_and_year_2024.csv"))  
    
    
    
    BENCH.REGIONS <- readRDS("data/bench_regions.rda") %>% 
      mutate(country_iso = ifelse(is.na(country_iso),"NA",country_iso)) %>% 
      rename(ScenarioGeography = scenario_geography) %>% 
      rename(CountryISO = country_iso) %>% 
      rename(reg.count = reg_count)
    
    Source.PowerFF <- "WEO2018"
    Source.Auto <- "ETP2017"
    e.Y <- Startyear + 5
    Sector.levels <-  c("Fossil Fuels", "Power", "Automotive")
    Technology.levels = c("Coal", "Oil", "Gas", 
                          "CoalCap", "GasCap", "NuclearCap", "HydroCap", "RenewablesCap", 
                          "ICE", "Hybrid", "Electric")
    
    
    
    ### Scenario data
    sb4.Scen <- sb4.Scen %>% 
      filter(#ScenarioGeography == gsub("Aggregate","",BenchmarkRegionchoose), 
        Scenario == Scenariochoose,
        Technology != "OilCap",
        Year == e.Y)
    
    # Power and Fossil Fuel from Source == "WEO2018"
    sb4.Scen.FF <- sb4.Scen %>% 
      filter(Sector %in% c("Fossil Fuels") & Source == Source.PowerFF & ScenarioGeography == "Global")
    
    sb4.Scen.Power <- sb4.Scen %>% 
      filter(Sector %in% c("Power") & Source == Source.PowerFF ) %>% 
      left_join(BENCH.REGIONS, by = "ScenarioGeography" ) %>% 
      filter(CountryISO %in% SBCountries$country_of_domicile) %>% 
      group_by(Technology, CountryISO) %>% 
      mutate(min.region = min(reg.count)) %>% 
      filter(min.region == reg.count) %>% 
      select(-min.region, -reg.count)
    
    SBCountries <- SBCountries %>% mutate(CountryISO = country_of_domicile) 
    
    sb4.Scen.Power <- sb4.Scen.Power %>%
      left_join(SBCountries, by = "CountryISO") %>%
      ungroup() %>% 
      select(Sector, Technology, Year,  TechShare, Perc) %>% 
      group_by(Sector, Technology, Year) %>% 
      summarise(Sector.Share = weighted.mean(TechShare, Perc)) %>% 
      select(Sector, Technology, Year, Sector.Share) %>%
      mutate(Type = "Scenario",
             SumProd = Sector.Share) 
    
    # Automotive from Source == "ETP2017"
    sb4.Scen.A <- sb4.Scen %>% 
      filter(Sector == "Automotive" & Source == Source.Auto & ScenarioGeography == "Global")
    
    sb4.Scen.ToPlot <- bind_rows(sb4.Scen.FF, sb4.Scen.A) %>% 
      select(Sector, Technology, Year, Production) %>% #mutate(Year = paste0(e.Y, "\n", Scenariochoose, "\n", "Scenario")) %>% 
      mutate(Production = if_else(Technology == "Coal",Production * 29.3076, Production ),
             Production = if_else(Technology == "Oil",Production * 6.12, Production ),
             Production = if_else(Technology == "Gas",Production * 0.037681200, Production )) %>%
      group_by(Sector, Year, Technology) %>% 
      summarize(SumProd = sum(Production, na.rm = T)) %>% 
      mutate(Sector.Share = SumProd/sum(SumProd),
             Type = "Scenario") 
    
    sb4.Scen.ToPlot <- bind_rows(sb4.Scen.ToPlot,sb4.Scen.Power) 
    
    sb4.Scen.ToPlot$Sector <- factor(sb4.Scen.ToPlot$Sector, levels = Sector.levels)
    sb4.Scen.ToPlot$Technology <- factor(sb4.Scen.ToPlot$Technology, levels = Technology.levels)
    
    
    ## ** Units Fossil Fuels ** ## 
    # Caol: 1 tce = 29307600000 J = 29.3076 GJ
    # Oil: 1 boe = 6120000000 J = 6.12 GJ
    # Gas: 1 cm3 = 1e-6 m3, 1m3 = 3.76812E+16 J = 0.037681200 GJ
    
    
    ### ALD data
    sb4.ALD <- sb4.ALD.19 %>% bind_rows(sb4.ALD.24) %>% 
      rename(Sector = sector, Technology = technology, Year = production_year, Production = total_production, Country = country_name) %>%
      mutate(Production = if_else(Production == "NULL"|is.na(Production), total_capacity,Production),
             Technology = gsub("Cok","", Technology),
             Type = "ALD") %>% 
      mutate(Technology = case_when(
        Technology == "Thermal" ~ "Coal",
        Technology == "Metallurgical" ~ "Coal",
        Technology == "Thermal & Metallurgical" ~ "Coal",
        TRUE ~ Technology))
    
    sb4.ALD$Production <- as.numeric(as.character(sb4.ALD$Production))
    
    sb4.ALD$Sector <- dplyr::recode(sb4.ALD$Sector, "Coal" = "Fossil Fuels", "Oil&Gas" = "Fossil Fuels")
    
    
    sb4.ALD <- sb4.ALD %>% filter(iso2 %in% SBCountries$country_of_domicile)
    
    # sb4.ALD <- base::merge(sb4.ALD,SBCountries, by.x = "iso2", by.y = "country_of_domicile")
    
    sb4.ALD.ToPlot <- sb4.ALD %>% 
      select(Type, Sector, Technology, Year, Country,iso2, region_name, Production) %>% 
      filter(Sector %in% c("Automotive", "Fossil Fuels", "Power"), Year %in% c(Startyear, e.Y),
             !Technology %in% c("FuelCell", "OilCap")) %>% 
      mutate(Production = if_else(Technology == "Coal", Production * 29.3076, Production)) %>%
      group_by(Type,Country,iso2, Sector, Year, Technology) %>% 
      summarize(SumProd = sum(Production, na.rm = T)) 
    
    sb4.ALD.ToPlot <- base::merge(sb4.ALD.ToPlot, SBCountries, by.x = "iso2", by.y = "country_of_domicile")
    
    sb4.ALD.ToPlot <- sb4.ALD.ToPlot %>% 
      mutate(CtryProd = SumProd * Perc) %>% 
      group_by(Type, Sector, Year, Technology) %>% 
      summarize(SumProd = sum(CtryProd)) %>% 
      mutate(Sector.Share = SumProd/sum(SumProd))
    
    
    ### data frame to plot
    Technology.Exposure_sb <- bind_rows(sb4.Scen.ToPlot, sb4.ALD.ToPlot)
    Technology.Exposure_sb <- Technology.Exposure_sb %>% 
      group_by(Type, Year, Sector) %>% 
      mutate(Sector.Tot = sum(SumProd,na.rm = T),
             Technology = if_else(Technology == "Oil and Condensate", "Oil", Technology))
    
    # Normalise the scenario by the sector total
    ALD.EY.Sub <- Technology.Exposure_sb %>% 
      ungroup() %>%
      filter(Year == e.Y &
               Type == "ALD") %>% 
      rename(Sector.Tot.EY = Sector.Tot) %>%
      select(Sector,Sector.Tot.EY) %>% distinct()
    
    Technology.Exposure_sb <- merge(Technology.Exposure_sb, ALD.EY.Sub, by = "Sector")
    Technology.Exposure_sb$SumProd[Technology.Exposure_sb$Type == "Scenario"] <- Technology.Exposure_sb$SumProd[Technology.Exposure_sb$Type == "Scenario"] * Technology.Exposure_sb$Sector.Tot.EY[Technology.Exposure_sb$Type == "Scenario"] /Technology.Exposure_sb$Sector.Tot[Technology.Exposure_sb$Type == "Scenario"]
    
    Technology.Exposure_sb$Technology <- factor(Technology.Exposure_sb$Technology, levels = Technology.levels, ordered = T)
    Technology.Exposure_sb$Sector <- factor(Technology.Exposure_sb$Sector, levels = Sector.levels, ordered = T)
    
    Technology.Exposure_sb <- Technology.Exposure_sb %>% arrange(Sector,Technology)
    
    
    Exposure.colors <- c(Technology = c("RenewableCap","HydroCap", "NuclearCap","GasCap", "CoalCap","Electric","Hybrid","ICE","Gas","Oil","Coal"),
                         Colors = c(RenewablesColour,HydroColour,NuclearColour,GasCapColour,CoalCapColour,ElectricColour,HybridColour,ICEColour,GasProdColour,OilProdColour,CoalProdColour))
    
    
    ## prepared for translation:
    
    Technology.Exposure_sb$Sector <- dplyr::recode(Technology.Exposure_sb$Sector, 
                                                   "Fossil Fuels" = paste0(wrap_labels(FossilFuelsProduction, 30),"\n (GJ)"), 
                                                   "Power" = paste0(C_Power, "\n(MW)"), 
                                                   "Automotive" = paste0(AutomotiveProduction,"\n(",Vehicles,")"))
    
    Technology.Exposure_sb$Technology <-  dplyr::recode(Technology.Exposure_sb$Technology, 
                                                        "RenewablesCap" = wrap_labels(T_RenewablesCap,15),
                                                        "HydroCap" = wrap_labels(T_HydroCap,15),
                                                        "NuclearCap" = wrap_labels(T_NuclearCap,15),
                                                        "GasCap" = wrap_labels(T_GasCap,15), 
                                                        "CoalCap" = wrap_labels(T_CoalCap,15),
                                                        "Electric" = wrap_labels(x_Electric,15), 
                                                        "Hybrid" = wrap_labels(x_Hybrid,15),
                                                        "ICE" = wrap_labels(x_ICE,15),
                                                        "Gas" = wrap_labels(T_GasProd,15),
                                                        "Oil" = wrap_labels(T_OilProd,15), 
                                                        "Coal" = wrap_labels(T_CoalProd,15))
    
    Technology.Exposure_sb$Year[Technology.Exposure_sb$Type == "Scenario"] <- paste0(paste0(e.Y, "\n", Scenariochoose, "\n", x_Scenario))
    
    cols <-  c(ElectricColour,HybridColour,ICEColour, RenewablesColour,HydroColour,NuclearColour,GasCapColour,CoalCapColour,GasProdColour,OilProdColour,CoalProdColour)
    
    # chart
    Fig4 <- ggplot(Technology.Exposure_sb, aes(x = Year, y = SumProd, fill = fct_rev(Technology))) +
      geom_col(width = 0.4) + 
      facet_wrap(~Sector,  scales = "free") +
      scale_fill_manual(values=cols) +
      scale_y_continuous(labels=format_si(), 
                         #limits = c(0, 1), 
                         expand = c(0, 0, 0, 0)) +
      scale_x_discrete(expand = c(0, 0.3, 0.1, 0.5)) +
      theme_barcharts_sb() +
      theme(axis.line.x = element_line(size = 0.5, linetype = "solid", color = textcolor),
            axis.line.y = element_line(size = 0.5, linetype = "solid", color = textcolor),
            axis.ticks.y.left = element_line(size = 0.2, linetype = "solid", color = textcolor),
            legend.position = "bottom",
            legend.justification = 0.5,
            legend.key.size = unit(0.2, "cm"),
            legend.text = element_text(margin = margin(r = 0, l = 1, unit = "pt")),
            legend.title = element_blank(),
            legend.box.margin = margin(0, 0, 0, 0),
            panel.border = element_blank(),
            panel.spacing = unit(0.5, "cm"),
            plot.margin = unit(c(0.1, 1, 0.1, 0.5), "cm"),
            strip.background  = element_blank(),
            strip.text = element_text(family = textfont, face = "bold",
                                      color = textcolor, size = textsize+1.2,
                                      margin = margin(0.5, 0, 0.5, 0, "cm"))) +
      guides(fill = guide_legend(nrow = 1, reverse = TRUE)) +
      labs(y = SectorProduction) +
      geom_text_repel(
        aes(label = paste0(sprintf("%.1f", Sector.Share*100),"%")),
        segment.size  = 0.08, segment.color = "grey60",
        position = position_stack(vjust = 0.5), direction = "y", hjust = -0.85, 
        size = textsize/3.5, family = textfont, color = textcolor)
    
  }else{
    
    Fig4 <- no_chart(NoSovBonds)
    
  }
  # save as png
  ggsave(Fig4 , 
         filename = graph_name(plotnumber, ParameterFile), 
         width = 8, height = 3.5,
         bg = "white", dpi = 300)
  
}


#### Figure 5 ####
## Policy Ambition 
SB_PolicyAmbition <- function(plotnumber){
  
  sb2 <- read_csv(paste0(sb_data_path, "Fig.1_2 Credit ratingbycountry_Rev1_5.csv"))
  
  SBCountries <- SB.Summary %>% 
    filter(investor_name == investor_name_select,
           portfolio_name == portfolio_name_select) %>% 
    group_by(investor_name, portfolio_name) %>% 
    mutate(total_value = sum(value_usd)) %>% 
    group_by(investor_name, portfolio_name, country_of_domicile) %>% 
    mutate(Perc = value_usd/total_value)
  
  
  if(data_check(SBCountries)){
    
    # sb2 <- sb2 %>% filter(iso2c %in% SBCountries$country_of_domicile)
    
    sb2_country <- base::merge(sb2, SBCountries, by.x = "iso2c", by.y = "country_of_domicile")
    
    
    Country.Rating <- sb2_country %>% 
      group_by(CAT_rating) %>% 
      summarize(rate_count = n(),
                sb.weight = sum(Perc)) %>% 
      ungroup() %>%
      mutate(rate_percent = sb.weight/sum(sb.weight),
             rate_count = sum(rate_count))
    
    
    Rate.colors <- data.frame(CAT_rating = c("Critically Insufficient","Highly Insufficient","Insufficient", "2°C Compatible", '1.5°C Paris Agreement compatible',"Not Rated"),
                              Color.no = 1:6,
                              Colours = c("#6A6464","#EF6949","#F2AD49","#F2E464","#B1CC68","#A9A9A9"))
    
    
    Country.Rating <- base::merge(Country.Rating, Rate.colors, by = "CAT_rating")
    
    Country.Rating$CAT_rating <- factor(Country.Rating$CAT_rating, 
                                        levels = c("Critically Insufficient",
                                                   "Highly Insufficient", 
                                                   "Insufficient", 
                                                   "2°C Compatible", 
                                                   "1.5°C Paris Agreement compatible",
                                                   "Not Rated"))
    # Country.Rating$Colours <- factor(Country.Rating$Colours,
    #                                  levels = c("#A9A9A9","#6A6464","#EF6949","#F2AD49","#F2E464","#B1CC68"))
    
    
    Country.Rating$CAT_rating <- dplyr::recode(Country.Rating$CAT_rating, 
                                               "1.5°C Paris Agreement compatible" = Compatible1.5, 
                                               "2°C Compatible" = Compatible2, 
                                               "Insufficient" = Insufficient, 
                                               "Highly Insufficient" = HighlyInsufficient,
                                               "Critically Insufficient" = CriticallyInsufficient,
                                               "Not Rated" = x_NotRated)
    
    # Colours =  rev(c("#6A6464","#EF6949","#F2AD49","#F2E464","#B1CC68","#A9A9A9"))
    
    # "#B1CC68" "#F2E464" "#F2AD49" "#EF6949" "#6A6464" "#A9A9A9"
    
    
    # col = c("#EF6949", )
    
    # chart
    Fig5 <- ggplot(Country.Rating, aes(x = "", y = rate_percent, fill = (CAT_rating))) + 
      geom_col() +
      coord_flip() +
      scale_fill_manual(values =  as.character(Country.Rating$Colours))+
      scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1), expand = c(0, 0)) +
      #scale_x_discrete(expand = c(0, 0)) + 
      theme_void() +
      theme(axis.text.x = element_text(family = textfont, size = textsize+3, colour = textcolor),
            axis.title = element_blank(), 
            axis.line.y = element_line(size = 0.8, colour = textcolor),
            legend.box.margin = unit(c(20, 0, 0, 0), "pt"),
            legend.key.height = unit(0.45, "cm"),
            legend.key.width = unit(0.45, "cm"),
            legend.spacing.y = unit(1.0, "cm"),
            legend.position = "bottom",
            legend.text = element_text(family = textfont, size = textsize+3, 
                                       colour = textcolor,
                                       margin = margin(l = 3, r = 10, unit = "pt")),
            legend.title = element_blank(),
            panel.grid.major = element_line(colour = "#989898", size = 0.2),
            panel.grid.minor = element_blank(),
            plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"),
            plot.title = element_text(size = textsize+2, colour = textcolor, 
                                      face = "bold", hjust = 0, 
                                      margin = margin(b = 30, unit = "pt"))) +
      #labs(title = "Susceptibility to being impacted by climate change \nof sovereign bonds portfolio") +
      guides(fill = guide_legend(reverse = TRUE, nrow = 2, override.aes = list(color = "white"))) 
    
  }else{
    
    Fig5 <- no_chart(NoSovBonds)
  }
  
  # save as png
  ggsave(Fig5, 
         filename = graph_name(plotnumber, ParameterFile), 
         width = 5, height = 2.5,
         bg = "white", dpi = 300)
  
  
  
}

theme_barcharts_sb <-function(base_size = textsize, base_family = "") {
  
  theme(axis.ticks = element_blank(),
        axis.text.x = element_text(colour=textcolor, size = textsize),
        axis.text.y = element_text(colour=textcolor, size = textsize),
        axis.title.x = element_blank(),
        axis.title.y = element_text(colour=textcolor, size = textsize),
        axis.line.x = element_line(colour = textcolor, size=0.5),
        axis.line.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        # legend.position=c(0.5,0),#legend.position = "none",
        legend.position = "none",
        legend.direction ="horizontal",
        legend.text = element_text(size=textsize, colour = textcolor),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.key.size = unit(0.4,"cm"),
        #legend.title = element_blank(),
        legend.title = element_text(colour = textcolor, size = textsize),
        legend.key = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA),
        plot.margin = unit(c(1,1, 0, 0), "lines"),
        plot.title = element_text(size = textsize+2, colour = textcolor, face = "bold"),
        text = element_text(family = "Calibri",size = textsize)
        # plot.margin = unit(c(1,1, 5, 2), "lines")
  )
}

# Format long scale numbers
format_si <- function(...) {
  # Based on code by Ben Tupper
  # https://stat.ethz.ch/pipermail/r-help/2012-January/299804.html
  
  function(x) {
    limits <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12,
                1e-9,  1e-6,  1e-3,  1e0,   1e3,
                1e6,   1e9,   1e12,  1e15,  1e18,
                1e21,  1e24)
    prefix <- c("y",   "z",   "a",   "f",   "p",
                "n",   "µ",   "m",   " ",   "k",
                "MW",   "GJ",   "T",   "P",   "E",
                "Z",   "Y")
    
    # Vector with array indices according to position in intervals
    i <- findInterval(abs(x), limits)
    
    # Set prefix to " " for very small values < 1e-24
    i <- ifelse(i==0, which(limits == 1e0), i)
    
    paste(format(round(x/limits[i], 1),
                 trim=TRUE, scientific=FALSE, ...),
          prefix[i])
  }
}

GraphName <- function(plotnumber, ParameterFile){
  
  namelist <-ParameterFile[,!names(ParameterFile) %in% c("PortfolioName","InvestorName")]
  namelist[is.na(namelist)] <- ""
  namelist <- lapply(1:length(namelist), function(x) paste0(toupper(substr(as.character(namelist[[x]]),1,3)),"_"))
  namelist <- namelist[1:6]
  namelist <- c(namelist, Startyear)
  
  graphname <- do.call(paste0,namelist)  
  graphname <- paste0(graphname, ".png")
  
  if (plotnumber == "00"){
    graphname <- gsub(".png","",graphname)
  } else if (plotnumber != "99"){
    graphname <- paste0(report_path,plotnumber,"_",graphname)
  }else{
    graphname <- paste0(GRAPH.PATH,graphname)    
  }
  
  
  
  return(graphname)
}
