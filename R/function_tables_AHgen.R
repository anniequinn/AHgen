tables_AHgen <- function(vSummary, 
                         eSummary, 
                         vExcluded_benchmark, 
                         vExcluded_input, 
                         results,
                         singleScenario = TRUE, 
                         compareLocations = FALSE, 
                         compareScenarios = FALSE) {
  
  tbl_vertices = 
    table_vertices(
      vSummary = vSummary, 
      singleScenario = singleScenario, 
      compareLocations = compareLocations, 
      compareScenarios = compareScenarios)

  tbl_edges = 
    table_edges(
      eSummary = eSummary, 
      singleScenario = singleScenario, 
      compareLocations = compareLocations, 
      compareScenarios = compareScenarios)
  
  tbl_vExcluded = 
    table_vExcluded(
      vExcluded_benchmark = vExcluded_benchmark, 
      vExcluded_input = vExcluded_input, 
      singleScenario = singleScenario, 
      compareLocations = compareLocations, 
      compareScenarios = compareScenarios)
  
  tbl_degree_all = 
    table_rankDegree(
      results = results, 
      levels = "all", 
      singleScenario = singleScenario, 
      compareLocations = compareLocations, 
      compareScenarios = compareScenarios)
  
  tbl_degrees_purposes = 
    table_rankDegree(
      results = results, 
      levels = "Purposes", 
      singleScenario = singleScenario, 
      compareLocations = compareLocations, 
      compareScenarios = compareScenarios)
  
  tbl_degrees_outcomes = 
    table_rankDegree(
      results = results, 
      levels = "Outcomes", 
      singleScenario = singleScenario, 
      compareLocations = compareLocations, 
      compareScenarios = compareScenarios)
  
  tbl_degrees_tasks = 
    table_rankDegree(
      results = results, 
      levels = "Tasks", 
      singleScenario = singleScenario, 
      compareLocations = compareLocations, 
      compareScenarios = compareScenarios)
  
  tbl_degrees_processes = 
    table_rankDegree(
      results = results, 
      levels = "Processes", 
      singleScenario = singleScenario, 
      compareLocations = compareLocations, 
      compareScenarios = compareScenarios)
  
  tbl_degrees_resources = 
    table_rankDegree(
      results = results, 
      levels = "Resources", 
      singleScenario = singleScenario, 
      compareLocations = compareLocations, 
      compareScenarios = compareScenarios)
  
  tbl_EC = 
    table_rankEC(
      results = results,
      singleScenario = singleScenario, 
      compareLocations = compareLocations, 
      compareScenarios = compareScenarios)
  
  tbl_SBC = 
    table_rankSBC(
      results = results,
      singleScenario = singleScenario, 
      compareLocations = compareLocations, 
      compareScenarios = compareScenarios)
  
  output <- 
    list(tbl_vertices,
         tbl_edges,
         tbl_vExcluded,
         tbl_degree_all,
         tbl_degrees_purposes,
         tbl_degrees_outcomes,
         tbl_degrees_tasks,
         tbl_degrees_processes,
         tbl_degrees_resources, 
         tbl_EC, 
         tbl_SBC)
  
  return(output)
  
}