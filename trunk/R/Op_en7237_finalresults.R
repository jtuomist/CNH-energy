library(OpasnetUtils)

ova <- Ovariable(
  "ova",
  dependencies=data.frame(Name="a"),
  formula=function(...) {
    return(a^2)
  }
)

a <- Ovariable(
  "a",# This code is Op_en7237/finalresults on page [[Helsinki energy decision 2015]]
  library(OpasnetUtils)
  library(ggplot2)
  
  openv.setN(0) # use medians instead of whole sampled distributions
  
  # Download all pre-calculated inputs, e.g. building stock.
  objects.latest("Op_en7237", code_name = "intermediates") # [[Helsinki energy decision 2015]]
  objects.latest("Op_en6007", code_name = "answer") # [[OpasnetUtils/Drafts]] oggplot 
  objects.latest("Op_en7392", code_name = "translate") # [[OpasnetUtils/Translate]] translate
  objects.latest("Op_en7392", code_name = "dictionary") # [[OpasnetUtils/Translate]] dictionary
  BSbase <- 24 # base_size for graph fonts. Use 12 if you use savefig to sav svg fils.
  BS <- BSbase
  saveobjects <- FALSE
  savefigs <- FALSE
  language <- "Finnish"
  fi <- language == "Finnish"
  
  savefig <- function(
    fil, 
    path = "N:/YMAL/Publications/2015/Helsingin energiapäätös/Kuvat/",
    sav = if(exists("savefigs")) savefigs else FALSE,
    type = "svg",
    height = 18,
    width = 24,
    units = "cm"
  ) {
    if(sav) {
      ggsave(paste(path, fil, ".", type, sep = ""), height = height, width = width, units = units)
    }
  }
  
  allplants <- c(
    'Biofuel heat plants',
    'CHP diesel generators',
    'Data center heat',
    'Deep-drill heat',
    'Hanasaari',
    'Household air conditioning',
    'Household air heat pumps',
    'Household geothermal heat',
    'Household solar',
    'Katri Vala cooling',
    'Katri Vala heat',
    'Kellosaari back-up plant',
    'Loviisa nuclear heat',
    'Neste oil refinery heat',
    'Salmisaari A&B',
    'Salmisaari biofuel renovation',
    'Sea heat pump',
    'Sea heat pump for cooling',
    'Small fuel oil heat plants',
    'Small gas heat plants',
    'Small-scale wood burning',
    'Suvilahti power storage',
    'Suvilahti solar',
    'Vanhakaupunki museum',
    'Vuosaari A',
    'Vuosaari B',
    'Vuosaari C biofuel',
    'Wind mills'
  )
  shutdown <- allplants[!allplants %in% noshutdown]
  shutdown <- c(shutdown, "None")
  
  customdecisions <- data.frame(
    Obs = NA, 
    Decision_maker = "Helen", 
    Decision = "PlantPolicy", 
    Option = "Custom", 
    Variable = c("plantParameters", "energyProcess"),
    Cell = c(
      paste("Plant:", paste(shutdown, collapse = ","), ";Parameter:Max,Min,Investment cost;Time:>2015", sep = ""),
      ""
    ),
    Change = c("Replace", "Identity"),
    Unit = NA,
    Result = 0
  )
  
  decisions <- rbind(decisions, customdecisions)
  
  if ("Hanasaari" %in% renovation) {
    customdecisions <- data.frame(
      Obs = NA, 
      Decision_maker = "Helen", 
      Decision = "PlantPolicy", 
      Option = "Custom", 
      Variable = rep(c("plantParameters", "energyProcess"), each = 2),
      Cell = c(
        "Plant:Hanasaari;Time:>=2018;Time:<=2060;Parameter:Max", 
        "Plant:Hanasaari;Time:>=2018;Parameter:Investment cost",
        "Plant:Hanasaari;Fuel:Biofuel;Time:>=2018", 
        "Plant:Hanasaari;Fuel:Coal;Time:>=2018"
      ),
      Change = rep(c("Replace", "Add"), each = 2),
      Unit = NA,
      Result = c(640, 100, -0.40, 0.40)
    )
    decisions <- rbind(decisions, customdecisions)
  }
  
  DecisionTableParser(decisions)
  
  oprint(data.frame(
    Running = c(
      "These plants will be running in the custom plant policy:",
      paste(allplants[!allplants %in% shutdown], collapse = ", ")
    ),
    Shutdown = c(
      "Plants that will be shut down in the custom plant policy:",
      paste(shutdown, collapse = ", ")
    )
  ))
  
  EnergyNetworkDemand <- EvalOutput(EnergyNetworkDemand)
  
  EnergyNetworkDemand@output <- rbind(
    EnergyNetworkDemand@output,
    data.frame(
      Time = rep(c(2025, 2035, 2045, 2055, 2065), each = 4),
      EnergySavingPolicy = rep(c("BAU", "Energy saving moderate", "Energy saving total", "WWF energy saving"), times = 5),
      Temperature = "(-18,-15]",
      EnergyConsumerDemandSource = "Formula",
      EnergyConsumerDemandTotalSource = "Formula",
      Fuel = "Cooling",
      fuelSharesSource = "Formula",
      EnergyNetworkDemandResult = 0,
      EnergyNetworkDemandSource = "Formula"
    )
  )
  
  #cat("All energy taxes are assumed zero.\n")
  #objects.latest("Op_en4151", code_name = "fuelTax")
  #fuelTax <- EvalOutput(fuelTax)
  #result(fuelTax) <- 0
  
  fuelUse <- EvalOutput(fuelUse)
  
  fuelUse$Fuel <- factor(
    fuelUse$Fuel, levels = c(
      "Biofuel",
      "Coal",
      "Fuel oil",
      "Gas",
      "Light oil",
      "Wood",
      "Electricity",
      "Electricity_taxed",
      "Heat",
      "Cooling"
    ), ordered = TRUE
  )
  
  DALY <- EvalOutput(DALYs)
  DALYs <- unkeep(DALY, cols = c("Age", "Sex", "Population"))
  DALYs <- oapply(DALYs, cols = c("Emission_site", "Emission_height", "Area"), FUN = sum)
  DALYs <- DALYs[DALYs$Response == "Total mortality" , ]
  
  EnergyNetworkCost <- EvalOutput(EnergyNetworkCost)
  
  EnergyNetworkCost$Time <- as.numeric(as.character(EnergyNetworkCost$Time))
  
  totalCost <- EvalOutput(totalCost)
  totalCost@output$Time <- as.numeric(as.character(totalCost@output$Time))
  totalCost <- unkeep(totalCost[totalCost$Time >= 2015 & totalCost$Time <=2065 , ], sources = TRUE)
  
  # Net present value and effective annual cost
  
  discount <- 0.03
  times <- c(2015, 2065)
  EAC <- EvalOutput(EAC)
  
  if(saveobjects) {
    objects.store(list = ls())
    cat("All objects stored for later use:\n", paste(ls(), collapse = ", "), "\n")
  }
  
  ############## POST_PROCESSING AND GRAPHS, VERSION FROM PERFERENCE ANALYSIS
  
  cat(translate("NOTE! In all graphs and tables, the Total energy saving policy is assumed unless otherwise noted\n"))
  cat(translate("Total DALYs/a by different combinations of policy options.\n"))
  
  temp <- DALYs[as.character(DALYs$Time) %in% c("2015", "2035") & DALYs$Response == "Total mortality" , ]
  
  oprint(
    translate(oapply(temp, INDEX = c("Time", "EnergySavingPolicy", "PlantPolicy"), FUN = sum)),
    caption = translate("Table 1: Total DALYs/a by different combinations of policy options."),
    caption.placement = "top",
    include.rownames = FALSE
  )
  
  bui <- oapply(buildings * 1E-6, cols = c("City_area", "buildingsSource"), FUN = sum)
  bui <- truncateIndex(bui, cols = "Heating", bins = 4)
  
  oggplot(bui[bui$EnergySavingPolicy == "BAU" , ], x = "Time", fill = "Heating", binwidth = 5) + 
    labs(
      title = translate("Building stock in Helsinki by heating"),
      y = translate("Floor area (M m2)")
    )
  
  savefig("Rakennuskannan koko Helsingissä")
  
  oggplot(bui, x = "Time", fill = "Efficiency", binwidth = 5) + 
  {if(fi) facet_wrap(~ Energiansäästöpolitiikka) else facet_wrap(~ EnergySavingPolicy)} + 
    labs(
      title = translate("Building stock in Helsinki by efficiency policy"),
      y = translate("Floor area (M m2)")
    )
  
  oggplot(bui, x = "Time", fill = "Renovation", binwidth = 5) + 
  {if(fi) facet_wrap(~ Energiansäästöpolitiikka) else facet_wrap(~ EnergySavingPolicy)} +
    labs(
      title = translate("Building stock in Helsinki by renovation policy"),
      y = translate("Floor area (M m2)")
    )
  
  oggplot(bui[bui$EnergySavingPolicy == "BAU" , ], x = "Time", fill = "Building", binwidth = 5) + 
    labs(
      title = translate("Building stock in Helsinki"),
      y = translate("Floor area (M m2)")
    )
  
  oggplot(buildings, x = "Time", fill = "Efficiency", binwidth = 5)+
  {if(fi) facet_grid(Energiansäästöpolitiikka ~ Korjaukset) else facet_grid(EnergySavingPolicy ~ Renovation)} + 
    labs(
      title = translate("Renovation of buildings by policy and efficiency"),
      y = translate("Floor area (M m2)")
    )
  
  # Contains also other buildings than district heating and other energy than heating
  hea <- EnergyConsumerDemandTotal * temperdays * 24 * 1E-3 # MW -> GWh
  hea$Time <- as.numeric(as.character(hea$Time))
  
  temp <- hea[hea$EnergySavingPolicy == "Energy saving total" & !hea$Consumable %in% c("District cooling", "Electric cooling") , ]
  oggplot(truncateIndex(temp, cols = "Temperature", bins = 7), x = "Time", fill = "Temperature", binwidth = 5) + 
  {if(fi) facet_wrap(~ Hyödyke) else facet_wrap(~ Consumable)} +
    labs(
      title = translate("Energy consumption in all buildings"),
      y = translate("Total energy (GWh /a)")
    )
  
  temp <- hea[!hea$Consumable %in% c("District cooling", "Electric cooling") , ]
  oggplot(temp, x = "Time", fill = "Consumable", binwidth = 5) + 
  {if(fi) facet_wrap(~ Energiansäästöpolitiikka) else facet_wrap(~ EnergySavingPolicy)} +
    labs(
      title = translate("Energy consumption in all buildings"),
      y = translate("Total energy (GWh /a)")
    )
  
  savefig("Helsingin vuotuinen energiantarve")
  
  oggplot(hea, x = "Time", fill = "Consumable", binwidth = 5) + 
  {if(fi) facet_wrap(~ Energiansäästöpolitiikka) else facet_wrap(~ EnergySavingPolicy)} +
    labs(
      title = translate("Energy consumption in all buildings"),
      y = translate("Total energy (GWh /a)")
    )
  
  hea2 <- EnergyNetworkDemand * temperdays * 24 / 1000 # MW -> GWh
  hea2$Time <- as.numeric(as.character(hea2$Time))
  
  oggplot(hea2, x = "Time", fill = "Fuel", binwidth = 5) + 
  {if(fi) facet_wrap(~ Energiansäästöpolitiikka) else facet_wrap(~ EnergySavingPolicy)} +
    labs(
      title = translate("Energy demand in the network"),
      fill = translate("Consumable"),
      y = translate("Total energy (GWh /a)")
    )
  
  savefig("Energiankulutus verkossa Helsingissä")
  
  eb <-EnergyNetworkOptim[EnergyNetworkOptim$Process_variable_type == "Activity",]
  eb <- eb[eb$EnergySavingPolicy == "Energy saving total" , ]
  colnames(eb@output)[colnames(eb@output) == "Process_variable_name"] <- "Plant"
  eb$Process_variable_type <- NULL
  
  ebtemp <- eb[eb$Time %in% c("2035") & eb$PlantPolicy == "BAU" & eb$Temperature != "(-18,-15]" , ]
  ebtemp <- truncateIndex(ebtemp, cols = "Plant", bins = 7)
  
  oggplot(ebtemp, x = "Temperature", fill = "Plant", turnx = TRUE) +
    labs(
      title = translate("Power plant activity by temperature daily optim \nPlant policy = BAU, Year = 2035"),
      x = translate("Temperature of the day"),
      y = translate("Average daily activity (MW)")
    )
  
  ebtemp <- eb[eb$Time %in% c("2035") & eb$Temperature != "(-18,-15]" , ]
  ebtemp <- truncateIndex(ebtemp, cols = "Plant", bins = 10)
  oggplot(ebtemp, x = "Temperature", fill = "Plant", turnx = TRUE) + 
  {if(fi) facet_wrap(~ Voimalapolitiikka) else facet_wrap(~ PlantPolicy)} +
    labs(
      title = translate("Power plant activity by temperature daily optim in 2035"),
      x = translate("Temperature of the day"),
      y = translate("Average daily activity (MW)")
    )
  
  savefig("Helsingin päivittäinen kaukolämpötase")
  
  ebtemp <- eb[eb$Time %in% c("2005") & eb$PlantPolicy == "BAU" & eb$Temperature == "(0,3]" , ]
  ebtemp <- truncateIndex(ebtemp, cols = "Plant", bins = 10)
  
  oggplot(ebtemp, x = "Plant", fill = "Plant", turnx = TRUE) + 
  {if(fi) facet_wrap(~ Lämpötila) else facet_wrap( ~ Temperature)} +
    theme(axis.text.x = element_blank()) + # Turn text and adjust to right
    labs(
      title = translate("Power plant activity by temperature daily optim \nPlant policy = BAU, Year = 2005"),
      y = translate("Average daily activity (MW)")
    )
  
  fu <- fuelUse / 3.6E+6 # From MJ/a -> GWh/a
  fu <- fu[fu$EnergySavingPolicy == "Energy saving total" , ]
  fu$Burner <- NULL
  fu$Time <- as.numeric(as.character(fu$Time))
  
  futemp <- fu[fu$Time %in% c("2015", "2035", "2065") & fu$PlantPolicy == "BAU" , ]
  futemp <- truncateIndex(futemp, cols = "Plant", bins = 7) * -1
  
  oggplot(futemp, x = "Fuel", fill = "Plant", turnx = TRUE) + 
  {if(fi) facet_grid(Aika ~ Energiansäästöpolitiikka) else facet_grid(Time ~ EnergySavingPolicy)} +
    labs(
      title = translate("Energy commodity flows \n Plant policy = BAU"),
      y = translate("Total annual energy (GWh/a)")
    )
  
  futemp <- fu[fu$Time %in% c("2005") & fu$PlantPolicy == "BAU" , ]
  futemp <- truncateIndex(futemp, cols = "Plant", bins = 7) * -1
  
  oggplot(futemp, x = "Fuel", fill = "Plant", turnx = TRUE) +
    labs(
      title = translate("Energy commodity flows in 2005 \n Plant policy = BAU"),
      y = translate("Total annual energy (GWh/a)")
    )
  
  futemp <- fu[fu$Fuel %in% c("Heat") , ]
  futemp <- truncateIndex(futemp, cols = "Plant", bins = 10) * -1
  
  oggplot(futemp,
          x = "Time", fill = "Plant", binwidth = 5) + 
          {if(fi) facet_wrap(~ Voimalapolitiikka) else facet_wrap(~ PlantPolicy)} +
    labs(
      title = translate("District heat flow"),
      y = translate("Total annual energy (GWh/a)")
    )
  
  savefig("Helsingin vuotuinen kaukolämpötase")
  
  futemp <- fu[fu$Fuel %in% c("Electricity") & fu$Plant != "Kymijoki River's plants", ]
  futemp <- truncateIndex(futemp, cols = "Plant", bins = 7) * -1
  
  # Does not contain plants outside Helsinki: Kymijoki River's plants, a share of Olkiluoto nuclear plant.
  oggplot(futemp, x = "Time", fill = "Plant", binwidth = 5) + 
  {if(fi) facet_wrap(~ Voimalapolitiikka) else facet_wrap(~ PlantPolicy)} +
    labs(
      title = translate("Electricity flow"),
      y = translate("Total annual energy (GWh/a)")
    )
  
  savefig("Helsingin vuotuinen sähkötase")
  
  emis <- truncateIndex(emissions, cols = "Emission_site", bins = 5)
  emis <- emis[emis$EnergySavingPolicy == "Energy saving total" & emis$Fuel != "Electricity" , ]
  levels(emis$Fuel)[levels(emis$Fuel) == "Electricity_taxed"] <- "Electricity bought"
  emis$Time <- as.numeric(as.character(emis$Time))
  oggplot(emis, x = "Time", fill = "Fuel", binwidth = 5) + 
  {if(fi) facet_grid(Saaste ~ Voimalapolitiikka, scale = "free_y") else facet_grid(Pollutant ~ PlantPolicy, scale = "free_y")} +
    labs(
      title = translate("Emissions from heating in Helsinki"),
      y = translate("Emissions (ton /a)")
    ) + 
    scale_x_continuous(breaks = c(2000, 2050))
  
  savefig("Helsingin energiantuotannon päästöt")
  
  da <- DALYs[DALYs$EnergySavingPolicy == "Energy saving total" & DALYs$Fuel != "Electricity" , ]
  levels(da$Fuel)[levels(da$Fuel) == "Electricity_taxed"] <- "Electricity bought"
  da$Time <- as.numeric(as.character(da$Time))
  oggplot(da, x = "Time", fill = "Fuel", binwidth = 5) + 
  {if(fi) facet_wrap(~ Voimalapolitiikka) else facet_wrap(~ PlantPolicy)} +
    labs(
      title = translate("Health effects of PM2.5 from heating in Helsinki"),
      y = translate("Health effects (DALY /a)")
    )
  
  savefig("Helsingin energiantuotannon terveysvaikutukset")
  
  fp <- fuelPrice[fuelPrice$Fuel %in% c(
    "Biofuel",
    "Coal",
    "Electricity_taxed",
    "Fuel oil",
    "Heat",
    "Light oil",
    "Natural gas",
    "Peat"
  ) , ]
  fp$Time <- as.numeric(as.character(fp$Time))
  levels(fp$Fuel)[levels(fp$Fuel) == "Electricity_taxed"] <- "Electricity"
  
  ggplot(translate(fp@output), if(fi) {
    aes(x = Aika, y = fuelPriceResult, colour = Polttoaine, group = Polttoaine)
  } else {
    aes(x = Time, y = fuelPriceResult, colour = Fuel, group = Fuel)
  }) +
    geom_line(size = 2)+theme_gray(base_size = BS) +
    labs(
      title = translate("Fuel prices (with tax)"),
      y = translate("Price (Eur/MWh)")
    )
  
  savefig("Polttoaineiden verolliset hinnat")
  
  tc <- truncateIndex(totalCost, cols = "Plant", bins = 11) / 10 * -1 # Yearly benefits (costs are negative)
  tc <- tc[tc$EnergySavingPolicy == "Energy saving total" , ]
  
  oggplot(tc, x = "Time", fill = "Cost", binwidth = 10) + 
  {if(fi) facet_wrap(~ Voimalapolitiikka) else facet_wrap( ~ PlantPolicy)} +
    labs(
      y = translate("Yearly cash flow (Meur)"),
      title = translate("Total benefits and costs of energy production")
    )+
    scale_x_continuous(breaks = c(2000, 2020, 2040, 2060))
  
  savefig("Energiantuotannon kokonaiskustannus Helsingissä kustannuksittain")
  
  oggplot(tc, x = "Time", fill = "Plant", binwidth = 10) + 
  {if(fi) facet_wrap(~ Voimalapolitiikka) else facet_wrap(~ PlantPolicy)} +
    labs(
      y = translate("Yearly cash flow (Meur)"),
      title = translate("Total benefits and costs of energy production")
    )+
    scale_x_continuous(breaks = c(2000, 2020, 2040, 2060))
  
  savefig("Energiantuotannon kokonaiskustannus Helsingissä voimaloittain")
  
  eac <- EAC[EAC$EnergySavingPolicy == "Energy saving total" , ] * -1
  
  BS <- BSbase * 0.7 # Plot the next two graphs with smaller font because they are busy graphs.
  
  eac2 <- eac[!eac$Plant %in% c(
    'Household air conditioning',
    'Household solar',
    'Katri Vala cooling',
    'Kellosaari back-up plant',
    'Sea heat pump for cooling',
    'Small-scale wood burning',
    'Suvilahti power storage',
    'Suvilahti solar',
    'Vanhakaupunki museum',
    'Wind mills'
  ) , ]
  
  oggplot(eac2, x = "PlantPolicy", fill = "Cost", turnx = TRUE) +
  {if(fi) facet_wrap(~ Voimala, scale = "free_y") else facet_wrap(~ Plant, scale = "free_y")} +
    labs(
      title = translate("Incomes and costs by plant"), 
      y = translate("Effective annual cash flow (Meur/a)")
    )
  
  savefig("Helsingin voimalaitosten kustannustehokkuus")
  
  oggplot(eac2, x = "PlantPolicy", fill = "Cost", turnx = TRUE)+
  {if(fi) facet_wrap(~ Voimala) else facet_wrap(~ Plant)} +
    labs(
      title = translate("Incomes and costs by plant"), 
      y = translate("Effective annual cash flow (Meur/a)")
    )
  
  savefig("Helsingin voimalaitosten kustannustehokkuus yhtenäisasteikolla")
  
  BS <- BSbase
  eac <- truncateIndex(eac, cols = "Plant", bins = 11)
  
  oggplot(eac, x = "PlantPolicy", fill = "Plant", turnx = TRUE)+
    labs(
      title = translate("Incomes and costs by plant policy"), 
      y = translate("Effective annual cash flow (Meur/a)")
    )
  
  oggplot(eac, x = "PlantPolicy", fill = "Cost", turnx = TRUE)+
    labs(
      title = translate("Incomes and costs by plant policy"), 
      y = translate("Effective annual cash flow (Meur/a)")
    )
  
  savefig("Teholliset tulot ja menot energiantuotannosta Helsingissä kustannuksittain")
  
  temp <- truncateIndex(plantParameters[plantParameters$Parameter == "Max" , ], cols = "Plant", bins = 11)
  temp <- temp[temp$Time >= 2000 & temp$Time <=2070 , ]
  oggplot(temp, x = "Time", fill = "Plant", binwidth = 1) + 
  {if(fi) facet_wrap(~ Voimalapolitiikka) else facet_wrap(~ PlantPolicy)} +
    labs(
      title = translate("Energy production capacity by plant policy"), 
      y = translate("Maximum capacity (MW)")
    )
  
  savefig("Energiantuotantokapasiteetin kehitys Helsingissä")
  
  # odag() #Plots a directed acyclic graph of ovariables used in the model.
  # This causes an internal error, so it must be the last row of the model.
  
  data=data.frame(A=1:10,Result=101:110)
)

EvalOutput(ova)@output

ova@formula <- function(...) {
  out <- ova@formula(...)
  out <- out - 10000
  return(out)
}
ova@formula
